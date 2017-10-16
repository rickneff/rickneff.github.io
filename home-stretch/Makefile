EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
#EMACS=emacs

all: app.el html.el

app.el: app.org
	emacs app.org --batch -f org-babel-tangle

html.el: html.org
	emacs html.org --batch -f org-babel-tangle


clean:
	rm app.el html.el

run:
#	$(EMACS) -Q -batch -L ~/emacs-web-server -L ~/inferno-lead -L ~/mebewiyo/W17/0dm -l simple-httpd.el -l servlets.el -l app.el -f httpd-batch-start
	$(EMACS) -Q -batch -L ~/emacs-web-server -L ~/rickneff.github.io/home-stretch -L ~/inferno-lead -l simple-httpd.el -l app.el -l servlets.el -f httpd-batch-start
