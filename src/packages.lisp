(defpackage #:basic-editor
  (:use #:cl)
  (:import-from :serapeum
                ~>
                ->)
  (:import-from :defclass-std
                defclass/std)
  (:import-from :boxes
                model
                make-node
                make-coordinates-relative
                node-text
                boxes-window
                width
                height
                text
                render
                bchar
                add-children
                mouse-position
                mouse-over-p
                )
  (:local-nicknames (#:sy #:sycamore))
  (:export
   main))
