;;; all rests loader

;;; in this folder, run with:
;; cd Programming/Lisp/basic-editor/tests/
;; sbcl --load ./all-tests-loader.lisp


(ql:quickload :basic-editor/tests)
(in-package #:basic-editor-test)
(test-all)

(defun in-replp ()
  (equalp
   (package-name (symbol-package
                  (slot-value *standard-output* 'symbol)))
   "SWANK"))

(if (in-replp)
    (progn
      (warn "finished running the tests in REPL"))
    (progn
      (warn "finished running the test in the terminal, quitting")
      (sb-ext:quit)))
