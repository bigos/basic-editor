(defpackage basic-editor-system
  (:use :common-lisp :asdf))
;;; some say it is a good practice to use a separate package for system declaration
(in-package :basic-editor-system)

(defsystem "basic-editor"
  :version "0.0.1"
  :author "https://github.com/bigos"
  :license "PUBLIC DOMAIN"
  :depends-on (#:clops-gui
               #:sycamore
               #:serapeum
               #:cl-gtk4
               #:cl-gdk4 #:cl-glib #:cl-cairo2
               #:defclass-std
               )
  :pathname "src/"
  :components ((:file "packages")
               (:file "classes" :depends-on ("packages"))
               (:file "cursor")
               (:file "file-selectors" :depends-on ("packages"))
               (:file "basic-editor" :depends-on ("packages")))
  :description "basic editor for GUI for clops in separate system")


(defsystem "basic-editor/tests"
  :depends-on ("basic-editor" "fiveam")
  :pathname "tests/"
  :serial T
  :components ((:file "packages")
               (:file "basic-editor-tests"))
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run-all-tests)))
;; (ql:quickload :basic-editor/tests)
;; (asdf:test-system :basic-editor/tests)
