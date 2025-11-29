;;; all rests loader

;;; in this folder, run with:
;; sbcl --load ./all-tests-loader.lisp

(progn
  (ql:quickload :basic-editor/tests)
  (in-package #:basic-editor-test)
  (test-all))

(sb-ext:quit)
