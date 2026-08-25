(declaim (optimize (speed 0) (safety 3) (debug 3)))
;;; basic-editor

;; (ql:quickload :basic-editor)
;; (basic-editor:main)
;; (warn "hello")

(in-package #:basic-editor)
;; (setf *break-on-signals* T)
;; (setf *print-circle* T)

(defparameter *environment* nil)

(defparameter *boundary-kilobyte* (expt 2 10))
(defparameter *boundary-megabyte* (expt 2 20))
(defparameter *boundary-gigabyte* (expt 2 30))

(defun pseudo (default &rest rest-args )
  (warn "running pseudo ~S" (list default rest-args ))
  default)

;;; minimal window -------------------------------------------------------------
(defparameter *basic-editor-model* nil)

(defun print-object-inner (obj  stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a"
            (loop for sl in (sb-mop:class-slots (class-of obj))
                  for slot-name = (sb-mop:slot-definition-name sl)
                  collect (cons slot-name
                                (if (slot-boundp obj slot-name)
                                    (format nil "~S" (slot-value obj slot-name))
                                    (format nil "---unbound---" )))))))

;;; replace T with concrete classes
;; (defmethod print-object ((obj standard-object) stream)
;;   (print-object-inner obj stream))

;; (defmethod print-object ((obj basic-editor-character) stream)
;;   (print-object-inner obj stream))

(defmethod print-object ((obj basic-editor-character) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~s" (list
                         :rel
                         (boxes:x (boxes:coordinates-relative obj))
                         (boxes:y (boxes:coordinates-relative obj))
                         :wh
                         (boxes:width obj)
                         (boxes:height obj)
                         ;; (color obj)
                         ;; (bchar obj)
                         :row/col
                         (row obj)
                         (col obj)
                         :out
                         (outside obj)
                         ))))

(defmethod print-object ((obj text-row) stream)
  (print-object-inner obj stream))

;;; ============================================================================
(defmethod the-container ((model basic-editor-model))
  (~> model world boxes:children (nth 1 _)))

(defun sample-text (n)
  (case n
    (:last-nl-yes
     (format nil "~A~%~A~%~A~%"
             "Ala ma kota"
             "Ola ma psa"
             "A, ja mam Lisp."))
    (:last-nl-no
     (format nil "~A~%~A~%~A"
             "Ala ma kota"
             "Ola ma psa"
             "A, ja mam Lisp."))
    (:first-nl-yes
     (format nil "~%~A~%~%~%~A"
             "Ala ma kota"
             "A ja Lisp."))
    (T
     (format nil "one line no NL"))))

;; (print-text-stats (sample-text :first-nl-yes))
(defun print-text-stats (model)
  (let ((rx (sample-text-stats model)))
    ;; (format t "we have ~s lines ================= ~S~%" (hash-table-count lf) txt)

    (loop for r being the hash-value of rx
          do (let ((rtext (row-text r (text model)) ))
               (format t "row ~S - ~S  ~%"
                       (row r)
                       rtext
                       )))))

(defun print-hash-text-stats (model hash)
  (warn "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")
  (let ((rx hash))
    ;; (format t "we have ~s lines ================= ~S~%" (hash-table-count lf) txt)

    (loop for r being the hash-value of rx
          do (let ((rtext (row-text r (text model)) ))
               (format t "row ~S - ~S  ~%"
                       (row r)
                       rtext
                       )))))

(defmethod row-text ((row text-row) text)
  (subseq text
          (home row)
          (end row)))

(defun model-characters (model)
  (~> model world
      boxes:children (elt _ 1)
      boxes:children (elt _ (if (show-line-numbers model)
                                1
                                0))          ; take into consderation the area for line numbers
      boxes:children))

;;; ----------------------------------------------------------------------------
(defmethod first-row ((model basic-editor-model))
  (let ((the-data (data (text-structure model))))
    (gethash 0 the-data)))

(defmethod previous-row ((model basic-editor-model))
  (let ((the-data (data (text-structure model)))
        (row (~> model cursor row)))
    (gethash (1- row) the-data)))

(defmethod current-row ((model basic-editor-model))
  (let ((the-data (data (text-structure model)))
        (row (~> model cursor row)))
    (gethash row the-data)))

(defmethod next-row ((model basic-editor-model))
  (let ((the-data (data (text-structure model)))
        (row (~> model cursor row)))
    (gethash (1+ row) the-data)))

(defmethod last-row ((model basic-editor-model))
  (let ((the-data (data (text-structure model))))
    (gethash (1- (hash-table-count the-data)) the-data)))

(defmethod nth-row ((model basic-editor-model) nth)
  (let ((the-data (data (text-structure model))))
    (gethash nth the-data)))
;;; ----------------------------------------------------------------------------

(defmethod wrap-toggle ((model basic-editor-model))
  (setf (text-wrap model) (ecase (text-wrap model)
                            (:trim
                             :wrap)
                            (:wrap
                             :trim)
                            (:word-wrap
                             :trim))))

(defun sample-text-stats (model)
  (assert (typep (text model) 'simple-array))
  (let* (
         ;; (text-container-width (~> model world width (- _ 20 20)))
        (text (text model))
        (lines-hash-table (make-hash-table)))

    (labels
        ((set-new-line (row home i)
           ;; (warn "adding row ~S ~S ~S" row home i)
           (setf (gethash row lines-hash-table)
                 (make-instance 'text-row
                                :row row
                                :home home
                                :end i))))
      (loop
        for prevc = nil then c
        for c across text
        for i = 0 then (1+ i)
        for home = 0 then (if (or (eq prevc #\Newline)
                                  (and cur-col
                                       (>= cur-col (wrap-at-column model))))
                              i
                              home)
        for cur-col = (- i home)
        for row =  (if (and (zerop i) (eq c #\Newline)) 0 -1) then (if (or
                                                                        (eq c #\Newline)
                                                                        (>= cur-col (wrap-at-column model)))
                                                                       (1+ row)
                                                                       row)
        do (progn
             (when (or
                    (eq c #\Newline)
                    (>= cur-col (wrap-at-column model)))
               (set-new-line row home (1+ i))))
        finally
           (when i
             (unless (eq c #\Newline)
               (set-new-line (1+ row) home (1+ i))))))
    lines-hash-table))

(defmethod reload-text-structure ((model basic-editor-model))
  ;; test-structure is a class holding a hash where
  ;; keys are integer row numbers starting with 0 and
  ;; values are instances of text-row
  (setf (text-structure model) (make-instance 'text-structure
                                              :data (sample-text-stats model))))

;;; ----------------------------------------------------------------------------
(defmethod find-cursor-position ((model basic-editor-model))
  (let ((cur-row (current-row model)))
    (when cur-row
      (+ (~> model cursor col)
         (home cur-row)))))

(defmethod find-first-visible-row ((model basic-editor-model))
  (loop for c in (seen-chars model)
        minimize (~> c row)))
(defmethod find-last-visible-row ((model basic-editor-model))
  (loop for c in (seen-chars model)
        maximize (~> c row)))
(defmethod find-first-visible-col ((model basic-editor-model))
  (loop for c in (seen-chars model)
        minimize (~> c col)))
(defmethod find-last-visible-col ((model basic-editor-model))
  (loop for c in (seen-chars model)
        maximize (~> c col)))

(defmethod find-page-rows ((model basic-editor-model))
  (- (find-last-visible-row model)
     (find-first-visible-row model)))

;;; ----------------------------------------------------------------------------
(defmethod delete-character-at-cursor ((model basic-editor-model))
  (let ((cur-pos (~> model cursor text-position)))

    ;; (warn "will delete at row ~S col ~S pos ~S"
    ;;       (~> model cursor row)
    ;;       (~> model cursor col)
    ;;       cur-pos)
    (unless (equal (text model) "")
      (if (and cur-pos
               (>= cur-pos 0)
               (< cur-pos (length (text  model))))
          (progn
            (setf (text model) (format nil "~A~A"
                                       (subseq (text model) 0
                                               cur-pos)
                                       (subseq (text model) (+ 1 cur-pos)
                                               (length (text model)))))
            (reload-text-structure model))
          ;; (warn "No cursor position found, possibly no text")
          ))))

(defmethod insert-character-at-cursor ((model basic-editor-model) entered key-name)
  ;; (warn "before insert key ~S ~S" entered key-name)
  ;; (warn "~S"
  ;;       (text model))

  (let ((cur-pos (or (~> model cursor text-position) 0))
        (entered-key (if (equal key-name "Return")
                         (format nil "~%")
                         entered)))
    (setf (text model) (format nil "~A~A~A"
                               (subseq (text model) 0 cur-pos )
                               entered-key
                               (subseq (text model) cur-pos)))
    (reload-text-structure model)
    (move-cursor-to-position model (1+ (~> model cursor text-position)))))

;;; key handling ===============================================================
(defun key-handling-f1-help ()
  (warn "------------ F1 Help --------------------")
  (warn "F1 = help")
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
  (warn "-----------------------------------------"))

(defun handle-key-pressed (entered key-name key-code mods lisp-window)
  (alexandria:write-string-into-file
   (format nil "~S~%" (list entered key-name key-code mods))
   "/tmp/basic-editor-log-key-presses.txt" :if-exists :append
                                           :if-does-not-exist :create)

  (let ((model *basic-editor-model*))
    (cond
      ((and (equal key-name "F1")
            (null mods))
       (key-handling-f1-help))

      ((and (equal key-name "F7")
            (null mods))
       ;; (warn "model stats ------------------------------------------")
       ;; (warn "TODO - something will go here")
       )

      ((and (equal key-name "F8")
            (null mods))
       (break "examine the models ~S" (list lisp-window *basic-editor-model*) ))

      ((and (equal key-name "F9")
            (null mods))
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
      ((and (equal key-name "j")
            (equal mods '(:CTRL)))
       ;; simulate Enter due to the menu focus problem
       (progn
         (insert-character-at-cursor model (format nil "~%") nil)
         (move-cursor-down model :ignored)
         (move-cursor-home model)))
      ((and (equal key-name "n")
            (equal mods '(:Alt)))
       (format T "keyboard selected new~%")
       (new-file))
      ((and (equal key-name "f")
            (equal mods '(:Alt)))
       (format T "keyboard selected open~%")
       (gui-window-gtk:present-file-open-dialog))

      ((and (equal key-name "s")
            (equal mods '(:Alt)))
       (format T "keyboard selected save~%")
       (file-save-selector))

      ((and (equal key-name "a")
            (equal mods '(:Alt)))
       (format T "keyboard selected about~%")
       (gui-window-gtk:present-about-dialog (about-dialog)))

      ((and (equal key-name "w")
            (equal mods '(:Alt)))
       (format T "keyboard selected wrap toggle~%")
       (wrap-toggle model))

      ((and (equal key-name "Home")
            (equal mods '(:Alt)))
       (format T "keyboard selected Alt Home~%")
       (move-cursor-first-line-home model))
      ((and (equal key-name "End")
            (equal mods '(:Alt)))
       (format T "keyboard selected Alt End~%")
       (move-cursor-last-line-end model))

      ((and (equal key-name "p")
            (equal mods '(:CTRL)))
       (setf (first-line (view-port model)) (1- (first-line (view-port model))) ))
      ((and (equal key-name "n")
            (equal mods '(:CTRL)))
       (setf (first-line (view-port model)) (1+ (first-line (view-port model))) ))
      ((equal key-name "Page_Up")
       (let ((fl (- (first-line (view-port model))
                    (find-page-rows model))))
         (setf (first-line (view-port model)) fl)
         (move-cursor-to model fl 0)))
      ((equal key-name "Page_Down")
       (let ((fl (+ (first-line (view-port model))
                    (find-page-rows model))))
         (setf (first-line (view-port model)) fl)
         (move-cursor-to model fl 0)))

      ((and (equal key-name "b")
            (equal mods '(:CTRL)))
       (setf (first-column (view-port model)) (1- (first-column (view-port model)))))
      ((and (equal key-name "f")
            (equal mods '(:CTRL)))
       (setf (first-column (view-port model)) (1+ (first-column (view-port model)))))
      ((equal key-name "Left")
       ;; handle menu bar focus problem
       (move-cursor-left model))
      ((equal key-name "Right")
       ;; handle menu bar focus problem
       (move-cursor-right model))
      ((equal key-name "Up")
       (move-cursor-up model)
       ;; (warn "cursor on last line zzzz 1-- row ~S --first line ~S"
       ;;       (~> model cursor row)
       ;;       (~> model view-port first-line))
       (when (< (~> model cursor row)
                (first-line (view-port model)))
         (setf (first-line (view-port model)) (~> model cursor row))))
      ((equal key-name "Down")
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
      ((equal key-name "Home")
       (move-cursor-home model))
      ((equal key-name "End")
       (move-cursor-end model :ignored))
      ((and (equal key-name "Delete")
            (equal mods nil))
       (delete-character-at-cursor model))
      ((and (equal key-name "BackSpace")
            (equal mods nil)
            (move-cursor-left model))
       (delete-character-at-cursor model))
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
                   (insert-character-at-cursor model entered key-name)))))))))

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

;;; main =======================================================================
(defun menu-bar (app lisp-window)
  (let ((menu (gio:make-menu)))

    (gui-menu:build-menu
     menu
     (gui-menu:prepare-submenu
      "File"
      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple lisp-window app menu "New" "new")
        (gui-menu:prepare-item-simple lisp-window app menu "Open" "open")
        (gui-menu:prepare-item-simple lisp-window app menu "Save As" "save-as")
        ))
      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple lisp-window app menu "Quit" "quit"))))
     (gui-menu:prepare-submenu
      "View"
      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple lisp-window app menu "Toggle Line Numbers" "toggle-line-numbers"))))
     (gui-menu:prepare-submenu
      "Help"
      ;; for now I plan to have only the About menu item
      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple lisp-window app menu "About" "about")))))

    (values menu)))

(defun about-dialog ()
  (list :authors (list "Jacek Podkanski")
        :website      "https://github.com/bigos/basic-editor"
        :program-name "Basic Editor"
        :comments     (format nil "~A~%~A"
                       "Basic Editor"
                       "A sample editor experiment written in SBCL Common Lisp")
        :license      "Public Domain"
        :system-information (format nil "~A~%~A~%~A~%~A~%~A~%"
                                    (lisp-implementation-type)
                                    (lisp-implementation-version)
                                    (uiop/os:detect-os)
                                    (uiop/os:architecture)
                                    (uiop/os:implementation-identifier))
        ;; icon names to try
        ;; https://specifications.freedesktop.org/icon-naming-spec/latest/#names
        :logo-icon-name  "applications-development"))

(defun main (&key (testing nil))
  (setf
   gui-drawing:*client-fn-draw-objects*  'basic-editor::draw-window
   gui-window-gtk:*client-fn-menu-bar*      nil
   gui-events:*client-fn-process-event* 'basic-editor::process-event
   gui-window-gtk:*initial-window-width*    600
   gui-window-gtk:*initial-window-height*   400
   gui-window-gtk:*initial-title*           "Basic-Editor"
   gui-window-gtk:*client-fn-menu-bar* 'basic-editor::menu-bar
   gui-window-gtk:*client-fn-open-file* 'basic-editor::open-file
   gui-window-gtk:*client-fn-cancel-open-file* 'basic-editor::cancel-open-file
   gui-window-gtk:*client-fn-save-file* 'basic-editor::save-file
   gui-window-gtk:*client-fn-cancel-save-file* 'basic-editor::cancel-save-file

   *basic-editor-model* (make-instance 'basic-editor-model)
   boxes:*model* *basic-editor-model*
   )

  (if testing
      ;; then
      (let ((experimental-window (make-instance 'basic-editor-window)))
        (setf *environment* :testing)
        (setf (gui-window::gir-window experimental-window) :testing)
        (setf gui-app:*lisp-app* (gui-app:make-lisp-app nil))
        (gui-window-gtk:window-creation-from-simulation :testing experimental-window)
        experimental-window)
      ;; else
      (progn
        (setf *environment* :development)
        (gui-window-gtk:window-main (make-instance 'basic-editor-window)))))

;; (main)
;;; type annotations
(-> experiment () null)
(defun experiment ()
  (let ((ew (main :testing T)))
    (process-event ew :resize '(400 500))
    (process-event ew :motion-enter '(0 0))
    (process-event ew :motion '(10 10))
    nil))
