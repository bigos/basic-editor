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
           ((children (~> (the-container *basic-editor-model*)
                          boxes:children))
            (first-child-found
              (car (loop for c in children
                         when (boxes:mouse-over-p c)
                           collect c)))
            (char-child-found nil))
         (warn "model world children under mouse ~S"
               first-child-found)
         ;; TODO we need overhaul of finding widgets

         (if (and children
                  (null first-child-found))
                                        ;then
          (let* ((grandchildren (children (first children)))
                 (first-grandchild-found
                   (car (loop for c in grandchildren
                              when (boxes:mouse-over-p c)
                                collect c))))
            (setf char-child-found first-grandchild-found))
                                        ;else
          (progn
            (setf char-child-found first-child-found)))

         (when (and char-child-found
                    (typep char-child-found 'basic-editor-character))
           (warn "clicked ready to move cursor ~S" char-child-found)
           (move-cursor-to *basic-editor-model*
                           (row char-child-found)
                           (col char-child-found))))))
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
            ;; (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))
            (handle-key-pressed entered key-name key-code mods lisp-window)))
    (:menu-simple
          (destructuring-bind ((action)) args
            (cond
              ;; File
              ((equalp action "new")
               (format T "menu selected new~%")
               (new-file *basic-editor-model*))
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
               (line-numbers-toggle *basic-editor-model*))
              ;; Help
              ((equalp action "about")
               (format T "menu selected about~%")
               (gui-window-gtk:present-about-dialog (about-dialog)))
              (T
               (format T "unhandled menu action ~S~%" action)))

            ;; remember to steal menu focus
            (gui-window:steal-focus lisp-window)))
    (otherwise
     (unless (eq event  :key-released)
       (warn "not handled event ~S ~S" event args))))

  ;; moving widgets -------------------------
  ;; (warn "may implement moving widgets in response to actions)
  ;; redrawing ------------------------------
  (gui-window:redraw-canvas lisp-window (format  nil "EVENT_~A" event)))
