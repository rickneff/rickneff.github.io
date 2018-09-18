(require 'simple-httpd)

(setq httpd-port 8080
      httpd-root "~/rickneff.github.io/home-stretch"
      httpd-listings nil)

(defun parse-file (content)
  (let* ((boundary (substring (car (split-string content "\n" 1)) 0 -1))
         (sep (split-string (second (split-string content boundary)) "\r\n\r\n"))
         (header (mapcar (lambda (h) (split-string h ": ")) (split-string (car sep) "\r\n")))
         (file-data (cadr sep))
         (disp (split-string (cadr (assoc "Content-Disposition" header)) "; " 1))
         (format (car disp))
         (meta (mapcar (lambda (l) (split-string l "=")) (cdr disp)))
         (content-type (assoc "Content-Type" header))
         )
    (append (list (list "Content-Disposition" format))
            meta
            (list content-type)
            (list (list "file-data" file-data))
            (list (list "boundary" boundary))
            )
    )
  )

(defun httpd/file (proc path &rest args)
  (shell-command "touch foo.foo.bar")
  (let* ((content (parse-content args)))
    (cond ((null (assoc "file-data" content))
           (http-error proc 400 "No file uploaded"))
          (t ;; (save-file content)
           (write-region (format "%S" (cadr (assoc "boundary" content))) 0 "~/rickneff.github.io/home-stretch/tmp.txt")
           (with-httpd-buffer proc "text/plain"
                              (insert (format "File accepted.\n\n")))))))
