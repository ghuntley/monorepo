;;; dottime.el --- use dottime in the modeline
;;
;; Copyright (C) 2019 Google Inc.
;;
;; Author: Vincent Ambo <tazjin@google.com>
;; Version: 1.0
;; Package-Requires: (cl-lib)
;;
;;; Commentary:
;;
;; This package changes the display of time in the modeline to use
;; dottime (see https://dotti.me/) instead of the standard time
;; display.
;;
;; Modeline dottime display is enabled by calling
;; `dottime-display-mode' and dottime can be used in Lisp code via
;; `dottime-format'.

(require 'cl-lib)
(require 'time)

(defun dottime--format-string (&optional offset prefix)
  "Creates the dottime format string for `format-time-string'
  based on the local timezone."

  (let* ((offset-sec (or offset (car (current-time-zone))))
         (offset-hours (/ offset-sec 60 60))
         (base (concat prefix "%m-%dT%H·%M")))
    (if (/= offset-hours 0)
        (concat base (format "%0+3d" offset-hours))
      base)))

(defun dottime--display-time-update-advice (orig)
  "Function used as advice to `display-time-update' with a
  rebound definition of `format-time-string' that renders all
  timestamps as dottime."

  (cl-letf* ((format-orig (symbol-function 'format-time-string))
             ((symbol-function 'format-time-string)
              (lambda (&rest _)
                (funcall format-orig (dottime--format-string) nil t))))
    (funcall orig)))

(defun dottime-format (&optional time offset prefix)
  "Format the given TIME in dottime at OFFSET. If TIME is nil,
  the current time will be used. PREFIX is prefixed to the format
  string verbatim.

  OFFSET can be an integer representing an offset in seconds, or
  the argument can be elided in which case the system time zone
  is used."

  (format-time-string (dottime--format-string offset prefix) time t))

(defun dottime-display-mode (arg)
  "Enable time display as dottime. Disables dottime if called
  with prefix 0 or nil."

  (interactive "p")
  (if (or (eq arg 0) (eq arg nil))
      (advice-remove 'display-time-update #'dottime--display-time-update-advice)
    (advice-add 'display-time-update :around #'dottime--display-time-update-advice))
  (display-time-update))

;; Amend the time display in telega.el to use dottime.
;;
;; This will never display offsets in the chat window, as those are
;; always visible in the modeline anyways.
(when (featurep 'telega)
  (defun telega-ins--dottime-advice (orig timestamp)
    (let* ((dtime (decode-time timestamp t))
           (current-ts (time-to-seconds (current-time)))
           (ctime (decode-time current-ts))
           (today00 (telega--time-at00 current-ts ctime)))
      (if (> timestamp today00)
          (telega-ins-fmt "%02d·%02d" (nth 2 dtime) (nth 1 dtime))
        (funcall orig timestamp))))

  (advice-add 'telega-ins--date :around #'telega-ins--dottime-advice))

;; Amend the time display in notmuch to use dottime.
(when (featurep 'notmuch)
  (defun notmuch-show--dottime-date-advice (orig header header-value)
    (if (equal "Date" header)
        ;; Unfortunately the header insertion functions do not have access
        ;; to the message object, which means that the only information we
        ;; have about the timestamp is its string rendering.
        (-let* (((sec min hour day mon year dow dst tz)
                 (parse-time-string header-value)))
          (insert header ": "
                  (dottime-format (encode-time sec min hour day mon year tz)
                                  tz "%a, %Y-")
                  "\n"))

      (funcall orig header header-value)))

  (advice-add 'notmuch-show-insert-header :around #'notmuch-show--dottime-date-advice))

(provide 'dottime)
