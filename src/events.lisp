(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:basic-editor)

;;; events =====================================================================
(defmethod process-event ((lisp-window basic-editor-window) event &rest args)
  (unless (member event '(:timeout :motion))
    ;; (unless (eq *environment* :testing) (warn "event ~S ~S" event args))
    )
  (case event
    (:timeout
     ;; do nothing yet
     )
    ((:motion :motion-enter)
     ;; we use simple case with one window so we ignore the window argument
     (destructuring-bind ((x y)) args
       (setf (mouse-position *basic-editor-model*) (cons x y))
       (gui-app:mouse-motion-enter lisp-window x y)))
    (:motion-leave
     (gui-app:mouse-motion-leave))
    (:focus-enter)
    (:focus-leave)
    (:pressed
          (destructuring-bind ((button x y)) args
            (gui-app:mouse-button-pressed button)
            (warn "mouse state ~S ~S" (gui-app:mouse-button gui-app:*lisp-app*) (list button x y))
            (let*
                ((children (~> *basic-editor-model*
                               world
                               boxes:children
                               (nth 1 _)
                               boxes:children))
                 (first-child-found ;; TODO because i added structure this no longer works
                   (car (loop for c in children
                              when (boxes:mouse-over-p c)
                                collect c))))
              ;; (warn "model world children under mouse ~S"
              ;;       first-child-found)
              (when (and first-child-found
                         (typep first-child-found 'basic-editor-character))
                (warn "clicked ready to move cursor ~S" first-child-found)
                (move-cursor-to *basic-editor-model* (row first-child-found) (col first-child-found)))
              )))
    (:released
          (destructuring-bind ((button x y)) args
            (gui-app:mouse-button-released button)
            (warn "mouse state released ~S ~S" (gui-app:mouse-button gui-app:*lisp-app*) (list button x y))))
    (:scroll)
    (:resize
     ;; on resize move cursor to corresponding file position
     (destructuring-bind ((w h)) args
       (gui-window:window-resize w h lisp-window)
       (setf (width lisp-window) w
             (height lisp-window) h)
       (let ((model *basic-editor-model*))
         (when (world model)
           (let ((bwidth (calculate-bwidth model)))
             (when (> bwidth 0)
               (setf (wrap-at-column model)
                     (floor
                      (/
                       (width (the-container model))
                       bwidth)))
               (reload-text-structure model)))))))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       ;; example of accessing gtk window object
       ;; (gtk4:widget-grab-focus (gui-window:gir-window lisp-window))
       ;; (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))
       (handle-key-pressed entered key-name key-code mods lisp-window)))
    (:menu-simple
     (destructuring-bind ((action)) args
       (cond
         ;; File
         ((equalp action "new")
          (format T "menu selected new~%")
          (new-file))
         ((equalp action "open")
          (format T "menu selected open~%")
          (gui-window-gtk:present-file-open-dialog))
         ((equalp action "save-as")
          (format T "menu selected save-as~%")
          (file-save-selector))
         ((equalp action "quit")
          (format T "menu selected quit~%")
          (gui-window-gtk:close-all-windows-and-quit))
         ;; View
         ((equalp action "toggle-line-numbers")
          (format T "menu selected toggle line numbers~%")
          (setf (show-line-numbers *basic-editor-model*) (if (show-line-numbers *basic-editor-model*)
                                                             nil
                                                             T))
          (warn "toggled showing lines to ~S" (show-line-numbers *basic-editor-model*)))
         ;; Help
         ((equalp action "about")
          (format T "menu selected about~%")
          (gui-window-gtk:present-about-dialog (about-dialog)))
         (T
          (format T "unhandled menu action ~S~%" action)))

       (gtk4:widget-grab-focus gui-window-gtk:*canvas-widget*)
       ;; possibly steal menu focus
       ))
    (otherwise
     (unless (eq event  :key-released)
       (warn "not handled event ~S ~S" event args))))

  ;; moving widgets -------------------------
  ;; (warn "may implement moving widgets in response to actions)
  ;; redrawing ------------------------------
  (gui-window:redraw-canvas lisp-window (format  nil "EVENT_~A" event)))
