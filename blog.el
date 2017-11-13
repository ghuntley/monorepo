;;; blog.el --- A simple org-mode & elnode blog software.
;;; -*- lexical-binding: t; -*-

(require 'elnode)
(require 'f)

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

(defun configure-org-html-export ()
  "Configure org-mode settings for elblog's HTML templating to work correctly."
  (setq org-html-postamble t)
  (setq org-html-doctype "html5")
  (setq org-html-head-include-scripts nil)
  (setq org-html-style-default (f-read-text "blog.css"))
  (setq org-html-preamble-format `(("en" ,(f-read-text "preamble.html"))))
  (setq org-html-postamble-format `(("en" ,(f-read-text "postamble.html")))))

;; Article fetching & rendering functions

(defun render-org-buffer (buffer &optional force)
  "Renders an org-mode buffer as HTML and returns the name of the output buffer."
  (letrec ((input-buffer (get-buffer buffer))
           (output-buffer (concat buffer "-rendered"))
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
  (let ((output-buffer (render-org-buffer (concat article ".org") t)))
    (if output-buffer `(200 . ,(get-buffer-string output-buffer))
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
