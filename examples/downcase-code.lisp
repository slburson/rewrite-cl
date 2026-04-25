;;;; downcase-code.lisp
;;;; Example: Downcase all symbols and keywords in a CL source file
;;;; Contributed by Scott L. Burson (https://github.com/slburson)

(require :asdf)
(push (truename ".") asdf:*central-registry*)
(asdf:load-system "rewrite-cl")

(defpackage #:downcase-code
  (:use #:cl #:rewrite-cl))

(in-package #:downcase-code)

(defun downcase-code-in-file (filename)
  "Downcase all symbol and keyword tokens in FILENAME, preserving formatting."
  (let ((trees (parse-file-all filename)))
    (with-open-file (s filename :direction :output :if-exists :rename)
      (dolist (tree trees)
        (write (zip-root-string
                 (zip-prewalk (of-node tree)
                              (lambda (z)
                                (if (member (zip-tag z) '(:symbol :keyword))
                                    (let ((new (make-token-node
                                                 (zip-sexpr z)
                                                 (string-downcase (zip-string z)))))
                                      (zip-replace z new))
                                  z))))
               :stream s :escape nil)))))

;;; Demo
(defun demo ()
  (format t "~%=== Downcase Code Demo ===~%~%")

  (let ((examples
          '(;; Mixed-case symbols
            "(DEFUN FOO (X) (+ X 1))"

            ;; Keywords
            "(make-instance 'MY-CLASS :SLOT-A 1 :SLOT-B 2)"

            ;; Preserves strings and comments
            "(FORMAT T \"HELLO WORLD\") ; THIS IS A COMMENT")))

    (dolist (source examples)
      (let* ((tree (parse-string source))
             (result (zip-root-string
                       (zip-prewalk (of-node tree)
                                    (lambda (z)
                                      (if (member (zip-tag z) '(:symbol :keyword))
                                          (let ((new (make-token-node
                                                       (zip-sexpr z)
                                                       (string-downcase (zip-string z)))))
                                            (zip-replace z new))
                                        z))))))
        (format t "BEFORE:~%~A~%~%" source)
        (format t "AFTER:~%~A~%~%" result)
        (format t "~A~%" (make-string 50 :initial-element #\-))))))

;; Run demo
(demo)
