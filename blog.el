;;; blog.el --- A simple org-mode & elnode blog software.
;;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'elnode)
(require 'f)
(require 'ht)

;; Definition of customization options

(defgroup elblog nil
  "Configuration for the Emacs Lisp blog software"
  :link '(url-link "https://github.com/tazjin/elblog"))

(defcustom elblog-port 8010
  "Port to run elblog's HTTP server on"
  :group 'elblog
  :type 'integer)

(defcustom elblog-host "localhost"
  "Host for elblog's HTTP server to listen on"
  :group 'elblog
  :type 'string)

(defcustom elblog-title "Elblog"
  "Title text for this elblog instance"
  :group 'elblog
  :type 'string)

(defcustom elblog-article-directory nil
  "Directory in which elblog articles are stored"
  :group 'elblog
  :type 'string)

;; Declare user-configurable variables needed at runtime.

(defvar elblog-articles (ht-create)
  "A hash-table of blog articles. This is used for looking up articles from
   URL fragments as well as for rendering the index.")

;; HTML templating setup

(defun template-preamble ()
  "Templates the preamble snippet with the correct blog title."
  (format (f-read-text "preamble.html") elblog-title))

(defun configure-org-html-export ()
  "Configure org-mode settings for elblog's HTML templating to work correctly."
  (setq org-html-postamble t)
  (setq org-html-doctype "html5")
  (setq org-html-head-include-scripts nil)
  (setq org-html-style-default (f-read-text "blog.css"))
  (setq org-html-preamble-format `(("en" ,(template-preamble))))
  (setq org-html-postamble-format `(("en" ,(f-read-text "postamble.html")))))

;; Article fetching & rendering functions

(defun render-org-buffer (input-buffer &optional force)
  "Renders an org-mode buffer as HTML and returns the name of the output buffer."
  (letrec ((output-buffer (concat (buffer-name input-buffer) "-rendered"))
           ;; Don't re-render articles unless forced.
           (must-render (or force
                            (not (get-buffer output-buffer)))))
    (if (and input-buffer must-render)
        (with-current-buffer input-buffer
          (org-export-to-buffer 'html output-buffer nil nil t)))
    (if input-buffer output-buffer nil)))

(defun get-buffer-string (buffer)
  "Returns the contents of the specified buffer as a string."
  (with-current-buffer (get-buffer buffer)
    (buffer-string)))

(defvar-local article-not-found
  '(404 . "<html><body><p>Oh no, the article was not found.</p></body></html>"))

(defvar-local text-html '("Content-Type" . "text/html"))

(defun render-article (article)
  "Renders an article, if it exists."
  (letrec ((rendered (-some->>
                      (ht-get elblog-articles article)
                      (concat elblog-article-directory)
                      (find-file)
                      (render-org-buffer))))
    (if rendered `(200 . ,(get-buffer-string rendered))
      article-not-found)))

(defun blog-post-handler (httpcon)
  "This handler servers a blog post from the configured blog post directory."
  (let ((response (render-article (elnode-http-mapping httpcon 1))))
    (elnode-http-start httpcon  (car response) text-html)
    (elnode-http-return httpcon (cdr response))))

;; Web server implementation

(defvar-local elblog-routes
  '(("^.*//en/\\(.*\\)" . blog-post-handler)))

(defun elblog-handler (httpcon)
  (elnode-hostpath-dispatcher httpcon elblog-routes))

(defun start-elblog ()
  (interactive)
  (configure-org-html-export)
  (elnode-start 'elblog-handler
              :port elblog-port
              :host elblog-host))

(defun stop-elblog ()
  (interactive)
  (elnode-stop elblog-port))

(provide 'elblog)
