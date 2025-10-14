(defsystem "basic-editor"
  :version "0.0.1"
  :author "https://github.com/bigos"
  :license "PUBLIC DOMAIN"
  :depends-on (#:clops-gui
               #:sycamore
               #:uiop
               #:cl-gtk4
               #:cl-gdk4 #:cl-glib #:cl-cairo2
               #:serapeum
               #:defclass-std
               )
  :pathname "src/"
  :components ((:file "packages")
               (:file "basic-editor"))
  :description "basic editor for GUI for clops in separate system")


(defsystem "basic-editor/tests"
  :depends-on ("basic-editor" "fiveam")
  :pathname "tests/"
  :components ((:file "packages")
               (:file "basic-editor-tests"))
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run-all-tests)))
;; (ql:quickload :basic-editor/tests)
;; (asdf:test-system :basic-editor/tests)
