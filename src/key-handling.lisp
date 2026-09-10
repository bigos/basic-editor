(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:basic-editor)

;;; key handling ===============================================================
(defun key-handling-f1-help ()
  (warn "------------ F1 Help --------------------")
  (warn "F1 = help")
  (warn "Ctrl Shift D = interactive debugger")   ; calls it directly, bypassing handle-key-pressed
  (warn "F7 = stats")
  (warn "F8 = debug")
  (warn "F9 = examine model")
  (warn "Alt-n = new file")
  (warn "Alt-f = open file")
  (warn "Alt-s = save file")
  (warn "Alt-a = about")
  (warn "Alt-w = toggle wrap/trim")
  (warn "Alt-Home = move cursor to first row Home")
  (warn "Alt-End =  move cursor to last  row End")
  (warn "Ctrl-p = previous line")
  (warn "Ctrl-n = next line")
  (warn "Ctrl-b = backwards character")
  (warn "Ctrl-f = forwards character")
  (warn "Ctrl-M = toggle menu sensitive")
  (warn "-----------------------------------------"))

(defun handle-key-pressed (entered key-name key-code mods lisp-window)
  (alexandria:write-string-into-file
   (format T "~S~%" (list entered key-name key-code mods))
   "/tmp/basic-editor-log-key-presses.txt" :if-exists :append
                                           :if-does-not-exist :create)
  (labels ((match-key (name &optional mod-keys)
             (and (equal key-name name)
                  (equal mods mod-keys))))
    (let ((model *basic-editor-model*))
      (cond
        ((match-key "F1")
         (key-handling-f1-help))

        ((match-key "F7")
         ;; (warn "model stats ------------------------------------------")
         ;; (warn "TODO - something will go here")
         )

        ((match-key "F8")
         (break "examine the models ~S" (list lisp-window *basic-editor-model*) ))

        ((match-key "F9")
         (progn
           (warn "working on F9")
           (if (slot-boundp model 'text-structure)
               (progn
                 (warn "examine model ------------------------------")
                 (warn "cursor ~S" (~> model cursor))
                 (warn "type of text ~S" (type-of (text model)))
                 (warn "file position ~S" (find-cursor-position model))
                 ;; (warn "text ~S" (sycamore:rope-string (text model)))
                 (warn "model text structure ~S" (print-text-stats model))
                 (warn "view port ~S" (list
                                       :view-port-size
                                       (size (view-port model))
                                       :view-port-lines
                                       (lines (view-port model))
                                       :view-port-max-column
                                       (max-column (view-port model))
                                       :view-port-first-line
                                       (first-line (view-port model))
                                       :view-port-first-column
                                       (first-column (view-port model))
                                       :container-width-pixels
                                       (boxes:width (the-container model))
                                       :container-height-pixels
                                       (boxes:height (the-container model))
                                       :wrap-at-column
                                       (wrap-at-column model)
                                       :current-file
                                       (current-file model)))
                 (warn "--------------------------------------------"))
               (progn
                 (warn "no text loaded")))))
        ;; (:SHIFT :CTRL :ALT :WIN)

        ((match-key "j" '(:CTRL))
         ;; simulate Enter due to the menu focus problem
         (progn
           (insert-character-at-cursor model (format nil "~%") nil)
           (move-cursor-down model :ignored)
           (move-cursor-home model)))

        ((match-key "n" '(:ALT))
         (format T "keyboard selected new~%")
         (new-file *basic-editor-model*))

        ((match-key "f" '(:ALT))
         (format T "keyboard selected open~%")
         (gui-window-gtk:present-file-open-dialog))

        ((match-key "s" '(:ALT))
         (format T "keyboard selected save~%")
         (file-save-selector))

        ((match-key "a" '(:ALT))
         (format T "keyboard selected about~%")
         (gui-window-gtk:present-about-dialog (about-dialog)))

        ((match-key "w" '(:ALT))
         (format T "keyboard selected wrap toggle~%")
         (wrap-toggle model))

        ((match-key "Home" '(:ALT))
         (format T "keyboard selected Alt Home~%")
         (move-cursor-first-line-home model))

        ((match-key "End" '(:ALT))
         (format T "keyboard selected Alt End~%")
         (move-cursor-last-line-end model))

        ((match-key "p" '(:CTRL))
         (setf (first-line (view-port model)) (1- (first-line (view-port model))) ))

        ((match-key "n" '(:CTRL))
         (setf (first-line (view-port model)) (1+ (first-line (view-port model))) ))

        ((match-key "Page Up")
         (let ((fl (- (first-line (view-port model))
                      (find-page-rows model))))
           (setf (first-line (view-port model)) fl)
           (move-cursor-to model fl 0)))

        ((match-key "Page_Down")
         (let ((fl (+ (first-line (view-port model))
                      (find-page-rows model))))
           (setf (first-line (view-port model)) fl)
           (move-cursor-to model fl 0)))

        ((match-key "b" '(:CTRL))
         (setf (first-column (view-port model)) (1- (first-column (view-port model)))))

        ((match-key "f" '(:CTRL))
         (setf (first-column (view-port model)) (1+ (first-column (view-port model)))))

        ((match-key "Left")
         ;; handle menu bar focus problem
         (move-cursor-left model))

        ((match-key "Right")
         ;; handle menu bar focus problem
         (move-cursor-right model))

        ((match-key "Up")
         (move-cursor-up model)
         (when (< (~> model cursor row)
                  (first-line (view-port model)))
           (setf (first-line (view-port model)) (~> model cursor row))))

        ((match-key "Down")
         (move-cursor-down model :ignored)
         (let ((pr (find-page-rows model)))
           ;; (warn "cursor on last line zzzz 1-- row ~S --first line ~S"
           ;;       (~> model cursor row)
           ;;       (~> model view-port first-line))
           (when (> (~> model cursor row)
                    (+ (first-line (view-port model))
                       pr))
             (setf (first-line (view-port model))
                   (-
                    (~> model cursor row)
                    pr)))))

        ((match-key "Home")
         (move-cursor-home model))

        ((match-key "End")
         (move-cursor-end model :ignored))

        ((match-key "Delete")
         (delete-character-at-cursor model))

        ((match-key "BackSpace")
         (move-cursor-left model)
         (delete-character-at-cursor model))

        ((match-key "M" '(:SHIFT :CTRL))
         (progn
           (warn "implement toggling menu sensitivity or hiding")))
        (T
         (if (equal entered "")
             (format t "unhandled key ~S~%" (list entered key-name key-code mods))
             (progn
               (if (equal key-name "Return")
                   (progn
                     ;; (warn "going to insert Return character for ~S ~S" entered key-name)
                     (insert-character-at-cursor model entered key-name))
                   (progn
                     ;; (warn "going to insert character for ~S ~S" entered key-name)
                     (insert-character-at-cursor model entered key-name))))))))))
