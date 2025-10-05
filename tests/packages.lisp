(defpackage #:basic-editor-test
  (:use #:cl
        #:basic-editor
        #:fiveam)
  (:local-nicknames (#:be #:basic-editor))
  (:export #:run!
           #:basic-editor-suite)
  (:import-from #:basic-editor
                #:main
                #:*basic-editor-model*
                #:width
                #:height
                #:view-port-first-line
                #:view-port-first-column
                #:process-event))
