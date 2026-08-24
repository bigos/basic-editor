(defpackage #:file-selectors
  (:use #:cl)
  (:export
   open-file
   cancel-open-file
   save-file
   cancel-save-file
   file-save-selector))

(defpackage #:basic-editor
  (:use #:cl #:file-selectors)
  (:import-from :serapeum
                ~>
                ->)
  (:import-from :file-selectors
                open-file
                cancel-open-file
                save-file
                cancel-save-file
                file-save-selector)
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
