(defpackage wc
  (:use #:cl #:iterate)
  (:export :main))
(in-package :wc)
(declaim (optimize (speed 3) (safety 0)))

(defun main ()
  (let ((filename (cadr (opts:argv)))
        (space (char-code #\Space))
        (newline (char-code #\Newline)))
    (with-open-file (file-stream filename :element-type '(unsigned-byte 8))
      (iter
        (for byte in-stream file-stream using #'read-byte)
        (for previous-byte previous byte)
        (for is-newline = (eql newline byte))

        ;; Count each byte
        (sum 1 into bytes)

        ;; Count every newline
        (counting is-newline into newlines)

        ;; Count every "word", unless the preceding character already
        ;; was a space or we are at the beginning of the file.
        (when (or (eql space previous-byte)
                  (eql newline previous-byte)
                  (not previous-byte))
          (next-iteration))

        (counting (or is-newline (eql space byte))
                  into words)

        (declare (fixnum bytes newlines words))
        (finally (format t "  ~A ~A ~A ~A~%" newlines words bytes filename))))))
