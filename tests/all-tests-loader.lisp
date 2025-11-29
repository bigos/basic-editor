;;; all rests loader

;;; in this folder, run with:
;; cd Programming/Lisp/basic-editor/tests/
;; sbcl --load ./all-tests-loader.lisp


(ql:quickload :basic-editor/tests)
(in-package #:basic-editor-test)
(test-all)

(sb-ext:quit)
