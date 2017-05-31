
(require 'cl)
(require 'subr-x)
(require 'simple-httpd)

(setq httpd-port 80
      httpd-listings nil
      valid-exercises (mapcar 'symbol-name '(BCD NCO PDQ NTC
                                                 PTW NTM BMI NES NRC BRE PQR
                                                 NGP PIQ
                                                 PRK NIO PFA BGT NGI NLR BSP NBO PBI))
      verify-key-template
"<!DOCTYPE html>
<html>
   <head>
   <meta charset='utf-8' />
   <title>Verify Answer Key</title>
   </head>
   <body>
   <form id='key-form' action='' method='POST'>
      Enter Answer Key <br/>
    <input type='text' name='key' id='key' />
    <br/>
    <button type='submit' id='submit-button'>Verify Key</button>
    <div id='messages'>
    </div>
   </form>
   <script type='text/javascript'>
     var form = document.getElementById('key-form');
     var messages = document.getElementById('messages');
     var messagesDefaultText = '';
     messages.innerHTML = messagesDefaultText;
     function resetMessages()
     {
        messages.innerHTML = messagesDefaultText;
     }
     form.onsubmit = function(event) {
       event.preventDefault();
       var formData = new FormData();
       var key = form['key'];
       formData.append(key.name, key.value);
       var xhr = new XMLHttpRequest();
       xhr.open('POST', '/verify-key/%s', true);
       xhr.onload = function () {
         if (xhr.status === 200) {
           messages.innerHTML = '<br />' + xhr.responseText;
         } else {
           messages.innerHTML = '<br />' + 'An error occurred!';
         }
         setTimeout('resetMessages()', 5000);
       }
       xhr.send(formData);
       return false;
     }
   </script>
 </body>
</html>")

(defservlet* answer/:tla text/html ()
  (cond ((member-ignore-case tla valid-exercises)
         (insert (format verify-key-template tla)))
        (t (httpd-error t 500 "No such exercise."))))

(defun httpd/verify-key (proc path &rest args)
  (let* ((content (parse-content args))
         (form-data (string-trim (cadr (assoc "file-data" content))))
         (key-present (and (= 3 (length content))
                           (string= "\"key\"" (cadr (assoc "name" content)))
                           (not (zerop (length form-data)))))
         (key (if key-present form-data "missing")))
    (with-httpd-buffer proc "text/html"
                       (insert (format "Key entered is %s.<br />No answer for that key.<br />(Hi, Ross!)" key)))))

;; (("Content-Disposition" "form-data") ("name" "\"key\"") ("file-data" "asdfasdf "))

(defservlet* birthday/:which/:who text/html (verbose)
  (cond ((equal which "70")
         (cond ((equal who "dds")
                (httpd-redirect t "https://firstthreeodds.org/17657741833134731255/three.js/examples/webgl_effects_anaglyph.html"))
               (t (httpd-error t 500 "No such an one.."))))
        ((equal which "60")
         (cond ((equal who "ken")
                (httpd-redirect t "https://firstthreeodds.org/17657741833134731255/three.js/examples/webgl-effects-anaglyph.html"))
               (t (httpd-error t 500 "No such an one.."))))
        (t (httpd-error t 500 "No such birthday."))))

(defun httpd/hello-world (proc path &rest args)
  (with-httpd-buffer proc "text/plain"
                     (insert "hello, " (file-name-nondirectory path))))

(defun httpd/ask-question (proc path &rest args)
  (let* ((content (parse-content args)))
    (cond ((null (assoc "file-data" content))
           (http-error proc 400 "No file uploaded"))
          ((file-too-large content)
           (httpd-error proc 400 "File too large"))
          (t 
           (save-file content)
           (with-httpd-buffer proc "text/plain"
                              (insert (format "Thanks for asking!\n\n")))))))

(defun httpd/ask-question-of-bnliodlhp (proc path &rest args)
  (let* ((content (parse-content args)))
    (cond ((null (assoc "file-data" content))
           (http-error proc 400 "No file uploaded"))
          ((file-too-large content)
           (httpd-error proc 400 "File too large"))
          (t 
           (save-file content)
           (with-httpd-buffer proc "text/plain"
                              (insert "I'm sorry. My responses are limited. You must ask the right questions.\n"))))))

;; lcdq = letter counts distance query

(defvar count-letters-target
  [8 2 0 2 7 2 1 2 4 1 1 1 2 8 8 3 0 8 4 10 2 0 0 0 1 0])

(defun distance-from-target (vec)
  (apply #'+ (map 'list #'abs (map 'list #'- vec count-letters-target))))

(defun convert-to-vector (counts-as-string)
  (map 'vector (lambda (x) (string-to-number (string x) 16)) counts-as-string))

(defun tryit ()
  (distance-from-target (convert-to-vector "8134D247F0122B7408AE223110")))

(defun httpd/lcdq (proc path &rest args)
  (let* ((counts (assoc "counts" (car args)))
         (counts-as-string (if (null counts) "" (nth 1 counts)))
         (ok (= (length counts-as-string) 26)))
    (with-httpd-buffer proc "text/plain"
      (insert (if ok
                  (number-to-string
                   (distance-from-target (convert-to-vector counts-as-string)))
                "bad query")))))

;; permdq = permutation distance query

(defvar correct-perm "bnhmpgsqekrlafjictdo")

(defun string-to-list (str)
  (map 'list (lambda (x) (- x ?a)) str))

(defvar correct-perm-as-list (string-to-list correct-perm))

(defvar correct-count (apply '+ correct-perm-as-list))

(defun correct (perm)
  (equal perm correct-perm))

(defun factorial (n)
  (if (< n 2) 1 (* n (factorial (1- n)))))

(defun rank (y)
  (if (zerop (length y)) 0
    (let* ((x0 (car y))
           (xs (cdr y)))
      (+ (rank xs)
         (* (loop for x in xs when (< x x0) sum 1)
            (factorial (length xs)))))))

(defvar rank-of-correct-perm-as-list (rank correct-perm-as-list))

(defun distance (perm-as-list)
  (abs (- (rank perm-as-list) rank-of-correct-perm-as-list)))

(defun perm-is-valid (perm perm-as-list)
  (and (stringp perm) (= 20 (length perm)) (= correct-count (apply '+ perm-as-list))))

(defun httpd/pdq (proc path &rest args)
  (let* ((perm (assoc "perm" (car args)))
         (perm (if (null perm) "" (nth 1 perm)))
         (perm-as-list (string-to-list perm)))
    (with-httpd-buffer proc "text/plain"
      (insert (if (perm-is-valid perm perm-as-list)
                  (number-to-string (distance perm-as-list))
                "invalid perm")))))
