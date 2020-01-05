;; Scraping funda.nl (this file is just notes and snippets, not full code)
;;
;; Begin by copying whole page into buffer (out of inspect element
;; because encoding is difficult)

(beginning-of-buffer)

;; zap everything that isn't a relevant result
(keep-lines "data-object-url-tracking\\|img alt")

;; mark all spans, move them to the end of the buffer
(cl-letf (((symbol-function 'read-regexp)
           (lambda (&rest _) "</span>")))
  (mc/mark-all-in-region-regexp (point-min) (point-max)))

;; mark all images lines (these contain street addresses for things
;; with images), clear up and join with previous
;;
;; mark all: data-image-error-fallback

;; delete all lines that don't either contain a span or an img tag
;; (there are duplicates)
(keep-lines "span class\\|img alt")

;; do some manual cleanup from the hrefs and done
