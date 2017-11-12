;;; blog.el --- A simple org-mode & elnode blog software.
;;; -*- lexical-binding: t; -*-

(require 'elnode)
(require 'f)

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
  (let ((output-buffer (render-org-buffer (concat article ".org"))))
    (if output-buffer `(200 . ,(get-buffer-string output-buffer))
      article-not-found)))

(defun blog-post-handler (httpcon)
  "This handler servers a blog post from the configured blog post directory."
  (let ((response (render-article (elnode-http-mapping httpcon 1))))
    (elnode-http-start httpcon  (car response) text-html)
    (elnode-http-return httpcon (cdr response))))

(defvar-local elblog-routes
  '(("^.*//en/\\(.*\\)" . blog-post-handler)))

(defun elblog-handler (httpcon)
  (elnode-hostpath-dispatcher httpcon elblog-routes))

(defun start-elblog ()
  (interactive)
  (elnode-start 'elblog-handler
              :port 8010
              :host "localhost"))

(defun stop-elblog ()
  (interactive)
  (elnode-stop 8010))
