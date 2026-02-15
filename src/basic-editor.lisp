(declaim (optimize (speed 0) (safety 3) (debug 3)))
;;; basic-editor

;; (ql:quickload :basic-editor)
;; (basic-editor:main)
;; (warn "hello")

(in-package #:basic-editor)

(defparameter *environment* nil)

(defun pseudo (default &rest rest-args )
  (warn "running pseudo ~S" (list default rest-args ))
  default)

;;; minimal window -------------------------------------------------------------
(defparameter *basic-editor-model* nil)

(defclass/std basic-editor-model (boxes:model)
  ((text :std ""
         ;; (sycamore:rope
         ;;  (alexandria:read-file-into-string "~/.bashrc"))
         )
   (cursor :std (make-instance 'cursor :row 0 :col 0))
   (view-port-size :std (cons nil nil))
   (view-port-lines :std 0)
   (view-port-columns :std 0)
   (view-port-first-line :std 0)
   (view-port-first-column :std 0)
   (all-lines-count :std 0)
   (text-wrap :std :wrap)               ; trim, wrap, word-wrap
   (world)
   (seen-chars)
   (current-file)
   ;; debug
   ))

(defclass/std basic-editor-window (boxes:boxes-window) (()))

(defclass/std basic-editor-character (boxes:node-character)
  ((row)
   (col)
   (pos)
   (outside)))

(defclass/std cursor ()
  ((row)
   (col)))

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

(defmethod print-object ((obj basic-editor-character) stream)
  (print-object-inner obj stream))

;; (defmethod print-object ((obj basic-editor-character) stream)
;;   (print-unreadable-object (obj stream :type t :identity t)
;;     (format stream "~a - ~S ~S"
;;             (bchar obj)
;;             (row obj)
;;             (col obj))))

(defmethod print-object ((obj cursor) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~Sx~S"
            (row obj)
            (col obj))))

;;; ============================================================================
;;; review that

(defmethod move-cursor-to ((cursor cursor) row col)
  (setf
   (row cursor) row
   (col cursor) col))
(defmethod move-cursor-left ((cursor cursor))
  (setf
   (row cursor) (row cursor)
   (col cursor) (1- (col cursor))))
(defmethod move-cursor-right ((cursor cursor))
  (setf
   (row cursor) (row cursor)
   (col cursor) (1+ (col cursor))))
(defmethod move-cursor-up ((cursor cursor))
  (setf
   (row cursor) (if (> (row cursor) 0)
                    (1- (row cursor))
                    0)
   (col cursor) (col cursor)))
(defmethod move-cursor-down ((cursor cursor) last-row)
  (setf
   (row cursor) (if (<= (row cursor) last-row)
                    (1+ (row cursor))
                    (row cursor))
   (col cursor) (col cursor)))
(defmethod move-cursor-home ((cursor cursor))
  (setf
   (row cursor) (row cursor)
   (col cursor) 0))
(defmethod move-cursor-end ((cursor cursor) last-col)
  (setf
   (row cursor) (row cursor)
   (col cursor) last-col))
;;; ============================================================================
(defun is-first-line (model)
  (zerop  (~> model cursor row)))

(defun is-last-line (model)
  (let ((found-last-row (all-lines-count model)))
    (when found-last-row
      (>=
       (~> model cursor row)
       found-last-row))))


(defmethod move-cursor-to ((model basic-editor-model) row col)
  (move-cursor-to (cursor model) row col))
(defmethod move-cursor-left ((model basic-editor-model))
  (if (> (~> model cursor col) 0)
      (move-cursor-left (cursor model))
      (unless (is-first-line model)
        (move-cursor-up model)
        (move-cursor-end model :ignored))))

(defmethod move-cursor-right ((model basic-editor-model))
  (let ((last-row (car (find-last-row model)))
        (last-col (cdr (find-last-row model))))
    (warn "last row ~S - ~S" last-row last-col)
    (warn "current position ~S = ~S"
          (~> model cursor row)
          (~> model cursor col))
    (progn
      (warn "test passed ~S" (is-last-line model))
      (if (>=
           (~> model cursor col)
           (find-cursor-end model))
          (progn
            (insert-character-at-cursor model "" "Return")
            ;; why inserting return moves cursor down?
           ;; (move-cursor-down model (all-lines-count model))
            (move-cursor-home model))

          (move-cursor-right (cursor model)))))
  )
(defmethod move-cursor-up ((model basic-editor-model))
  (move-cursor-up (cursor model)))
(defmethod move-cursor-down ((model basic-editor-model) ignored)
  (let ((last-row (car (find-last-row model))))

    (move-cursor-down (cursor model) last-row)))
(defmethod move-cursor-home ((model basic-editor-model))
  (move-cursor-home (cursor model)))
(defmethod move-cursor-end ((model basic-editor-model) ignored)
  (move-cursor-end (cursor model) (find-cursor-end model)))

(defmethod move-cursor-first-line-home ((model basic-editor-model))
  (move-cursor-to model 0 0))
(defmethod move-cursor-last-line-end ((model basic-editor-model))
  (let ((last-row-cons (find-last-row model)))
    (move-cursor-to model (car last-row-cons) (cdr last-row-cons))))
;;; ----------------------------------------------------------------------------

(defmethod find-cursor-end ((model basic-editor-model))
  ;; (warn "looping seen-chars ~S" (loop for c in (seen-chars model) collecting (bchar c)))

  (let* ((row-chars (loop
                      for c in (seen-chars model)
                      for found = (and (equal
                                        (~> c row)
                                        (~> model cursor row)))
                      when found
                        collect c))
         (row-chars-length (length row-chars)))
    ;; (warn "model cursor ~S and row chars ~S and row length ~S"
    ;;       (~> model cursor)
    ;;       (mapcar (lambda (c) (bchar c)) row-chars)
    ;;       row-chars-length)

    (cond ((equal row-chars-length 0)
           ;; (warn "doing length 0")
           0)
          ((equal row-chars-length 1)
           ;; (warn "doing length 0")
           0)
          (T
           ;; (warn "doing length ~S and last ~S" row-chars-length (last row-chars))
           (let* ((last-row-chars (car (last row-chars)))
                  (dv
                    (if (equal (bchar last-row-chars) #\Newline)
                        (- row-chars-length 2)
                        (- row-chars-length 1))))
             ;; (warn "calculated ~S" dv)
             dv)))))

(defmethod looping-seen-chars ((model basic-editor-model) msg)
  (warn "looping seen-chars ~S ~S"
        msg
        (loop for c in (seen-chars model)
              collecting (list (bchar c) :R (row c) :C (col c)))))

(defmethod find-cursor-position ((model basic-editor-model))
  (warn "model cursor ~S" (cursor model))
  (looping-seen-chars model "")


  (loop for c in (seen-chars model)
        for found = (and (equal
                          (~> c row)
                          (~> model cursor row))
                         (equal
                          (~> c col)
                          (~> model cursor col)))
        until found
        finally (return (if found
                            (~> c pos)
                            nil)))
  )
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
(defmethod find-last-row ((model basic-editor-model))
  (loop for last-char = nil then c
        for c across (~> model text sycamore:rope-string)
        for row = 0 then (if (equal last-char #\Newline)
                             (1+ row) row)
        for col = 0 then (if (equal last-char #\Newline)
                             0 (1+ col))
        for pos = 0  then (1+ pos)
        finally
           (return (cons row col))))

(defmethod delete-character-at-cursor ((model basic-editor-model))
  (let ((cur-pos (find-cursor-position model)))
    (warn "will delete at row ~S col ~S pos ~S"
          (~> model cursor row)
          (~> model cursor col)
          cur-pos)
    (if cur-pos
        (setf (text model) (sycamore:rope
                            (sycamore:subrope (text model) :start 0
                                                           :end cur-pos)
                            (sycamore:subrope (text model) :start (1+ cur-pos)
                                                           :end (sycamore:rope-length (text model)))))
        (warn "No cursor position found, possibly no text"))))

(defun for-enter ()
  (format nil "~%"))

(defmethod insert-character-at-cursor ((model basic-editor-model) entered key-name)
  ;; TODO this desperately needs improving and testing
  (warn "before insert")
  (warn "~S"
        (sycamore:subrope (text model)))

  (let ((cur-pos (find-cursor-position model)))
    (if cur-pos
        (progn                          ; then
          (warn "cursor pos is present")
          (setf (text model)


                (if (equal key-name "Return")
                    (progn              ;then
                      (warn "doint Return")
                      (sycamore:rope
                       ;;  pre insert
                       ;; TODO it fails here, we need to test extensively this part in different cases
                       (warn "pre insert")
                       ;; (break "before subrope ~S ~S" (length (text model)) (text model))
                       (if (> (1+ cur-pos)
                               (length (text model)))
                           (sycamore:subrope (text model) :start 0
                                                          :end (+ 2  cur-pos))
                           (sycamore:subrope (text model) :start 0
                                                          :end (+ 0  cur-pos)))
                       ;; the insert
                       (warn "the insert")
                       (for-enter)

                       ;; post insert
                       (warn "post insert")
                       (sycamore:subrope (text model) :start (+ 2 cur-pos)
                                                      :end (sycamore:rope-length (text model)))
                       (warn "after post insert")
                       ))
                    ;; ---------------------------------------------------------------
                    (progn
                      (warn "doint NON Return")
                      (sycamore:rope
                       ;;  pre insert
                       (sycamore:subrope (text model) :start 0
                                                      :end cur-pos)
                       ;; the insert
                       (cond
                         ((equal key-name "Return")
                          (for-enter))
                         (T entered))
                       ;; post insert
                       (sycamore:subrope (text model) :start (+ 0 cur-pos)
                                                      :end (sycamore:rope-length (text model)))))))

          ;; ------------------------------------------------------
          (cond
            ((equal key-name "Return")
             (warn "move cursor return 1")
             (move-cursor-down model :ignored)
             (move-cursor-home model))
            (T
             (warn "move cursor normal 1")
             (move-cursor-right model))))

        (progn                          ; else
          ;; TODO start adding tests
          (warn "cursor pos is NIL")
          (setf (text model) (sycamore:rope
                              (cond
                                ((equal key-name "Return")
                                 (for-enter))
                                (T entered))))
          (cond
            ((equal key-name "Return")
             (warn "move cursor return 2")
             (move-cursor-down model :ignored)
             (move-cursor-home model))
            (T
             (warn "move cursor normal 2")
             (move-cursor-right model))))))
  (progn
    (warn "---------- done insert --------------")
    (warn "cursor ~S ~S" (~> model cursor row) (~> model cursor col))
    (warn "cursor text  ~S" (sycamore:rope-string (~> model text)))
    (warn "---------- finished insert --------------")
    (looping-seen-chars model "after finished insert")
    ))

(defun new-file ()
  (let ((model *basic-editor-model*))
    (setf
     (text model) (sycamore:rope "edit something")
     (current-file model) nil)))

;; (funcall *client-fn-open-file* (cancelled-value))
(defun open-file (filepath)
   (case (car  filepath)
     (:cancelled
      nil)
     (:selected
      (let ((model *basic-editor-model*)
            (clean-filepath (subseq (cdr  filepath) 7)))
        (warn "going to load ~S" clean-filepath)
        (setf
         (text model) (sycamore:rope
                       (alexandria:read-file-into-string clean-filepath))
         (current-file model) clean-filepath)))))

;; (funcall *client-fn-save-file* (cancelled-value))
(defun save-file (filepath)
  (case (car filepath)
    (:cancelled
     nil)
    (:selected
     (let ((model *basic-editor-model*)
           (clean-filepath (subseq (cdr filepath) 7)))
       (if (equal clean-filepath (current-file model))
           (warn "going to save ~S" clean-filepath)
           (warn "going to save AS ~S" clean-filepath))
       (setf (current-file model) clean-filepath)
       ;; TODO if we edit the file in the selector the program still does not see it
       (alexandria:write-string-into-file
        (sycamore:rope-string (text model))
        clean-filepath
        :if-exists :supersede
        :if-does-not-exist :create)))))

;;; drawing ====================================================================
(defun calculate-chars (text-container model)
  (let* ((the-chars
           (let*
               ((font-size 18)
                (margin-horizontal 0)
                (margin-vertical 0)
                (text-for-size  "pOly()/_")
                (text-data (text-size text-for-size font-size ))
                (twidth (floor (/ (getf text-data :width) (length text-for-size))))
                (theight          (getf text-data :height))

                (bwidth  (+ twidth  0))
                (bheight (+ theight 0))
                (wrap-mode :wrap)
                (wrap-column
                  (ecase wrap-mode
                    (:trim 10000000) ;; trim wraps on ridiculously high column
                    (:wrap  (-
                             (floor (/ (width text-container)
                                       (1+ bwidth)))
                             2)))))
             ;; (break "examine model in calculate chars ~S" model)

             (loop for last-char = nil then c
                   for c across
                         (sycamore:rope-string
                          (text model))
                   for row = 0 then (if (or (equal last-char #\Newline)
                                            (>= col wrap-column))
                                        (1+ row) row)
                   for col = 0 then (if (or (equal last-char #\Newline)
                                            (>= col wrap-column))
                                        0 (1+ col))
                   for pos = 0  then (1+ pos)
                   for maxcol = 0 then (max maxcol col)
                   for relx = (+ margin-horizontal
                                 (ceiling
                                  (* (- col (view-port-first-column model))
                                     (1+ bwidth) )))
                   for rely = (+ margin-vertical
                                 (ceiling
                                  (* (- row (view-port-first-line model))
                                     (1+ bheight))))
                   for min-rely = 0 then (min rely min-rely)
                   for outside = (let ((max-x-coord (+ relx bwidth))
                                       (max-y-coord (+ rely bheight)))
                                   (or
                                    (>= max-x-coord (- (width text-container) 10))
                                    (< relx 0)
                                    (>= max-y-coord (height text-container))
                                    (< rely 0)))
                   for max-seen-row = 0 then (if outside
                                                 max-seen-row
                                                 (max row max-seen-row))
                   for max-seen-col = 0 then (if outside
                                                 max-seen-col
                                                 (max col max-seen-col))
                   unless outside
                     collect (make-instance 'basic-editor-character
                                            :bchar c
                                            :font-size font-size
                                            :coordinates-relative
                                            (make-coordinates-relative
                                             relx
                                             rely)
                                            :width bwidth
                                            :height bheight
                                            :color (if (and (= (~> model cursor row)
                                                               row)
                                                            (= (~> model cursor col)
                                                               col))
                                                       "red"
                                                       "pink")
                                            :row row
                                            :col col
                                            :pos pos
                                            :outside outside
                                            )
                   finally
                      (setf (all-lines-count model) row)
                      (setf (view-port-lines model) max-seen-row)
                      (setf (view-port-columns model) max-seen-col)))))
    (setf (seen-chars model) the-chars)
    the-chars))

(defun text-size (text text-size)
  (cairo:select-font-face
   "Ubuntu Mono"
   ;;"Advaita Mono"
   ;; "Liberation Mono"
                          :normal :normal)
  (cairo:set-font-size text-size)

  (multiple-value-bind (xb yb width height)
      (handler-bind
          ((alexandria:simple-style-warning
             (lambda (warning)
               (when (alexandria:starts-with-subseq
                      "bare references to struct types are deprecated."
                      (simple-condition-format-control warning))
                 (muffle-warning warning)))))

        (cairo:text-extents (format nil "~A" text)))
    (list :xb xb :yb yb :width width :height height)))

(defun adding-children (world)
  (add-children world
                (list
                 (make-instance 'node-text
                                :coordinates-relative (make-coordinates-relative 10 50)
                                :width (- (width world) 40)
                                :height  30
                                :color "white"
                                :wrap 'truncate
                                :text (format nil "Heading will go here. ~S - ~S"
                                              (gui-app:mouse-button gui-app:*lisp-app*)
                                              (cursor *basic-editor-model*)
                                              ))
                 (let ((text-container (make-node 20
                                                  340
                                                  (- (width world) 20 20)
                                                  (- (height world) 60) "yellow")))
                   (add-children text-container
                                 (calculate-chars text-container *basic-editor-model*)))
                 (make-instance 'node-text
                                :coordinates-relative (make-coordinates-relative 10 50)
                                :width (- (width world) 40)
                                :height 30
                                :color "white"
                                :wrap 'truncate
                                :text (format nil
                                              "rowcols ~S, fl ~S, fc ~S"
                                              (cons
                                               (view-port-lines
                                                *basic-editor-model*)
                                               (view-port-columns
                                                *basic-editor-model*))
                                              (view-port-first-line   *basic-editor-model*)
                                              (view-port-first-column *basic-editor-model*))))))

(defmethod draw-window ((window basic-editor-window))
  ;; paint background
  (let ((cv 0.95)) (cairo:set-source-rgb  cv cv cv))
  (cairo:paint)
  (when nil
    ;; (cairo:select-font-face "Ubuntu Mono" :italic :bold)
    ;; (cairo:set-font-size 10)
    ;; (cairo:move-to 10 10)
    ;; (gui-window:set-rgba "black")
    ;; (cairo:show-text (format nil "try moving the mouse over the window and outside of it"))


    ;; (cairo:select-font-face "Ubuntu Mono" :italic :bold)
    ;; (cairo:set-font-size 15)
    ;; (cairo:move-to 10 100)
    ;; (let ((cmotion    (gui-app:current-motion-window-p gui-app:*lisp-app* window)))
    ;;   (if cmotion
    ;;       (gui-window:set-rgba "green")
    ;;       (gui-window:set-rgba "red"))
    ;;   (cairo:show-text (format nil "motion ~A" cmotion)))
    )

  ;; ==================================================================

  (let ((world (boxes::make-node-down
                0 0 (width window) (height window) "#cccccc88")))
    (setf (world *basic-editor-model*) world) ; zzzzzzzzzzzzzzzzzzz
    (boxes:absolute-coordinates world)

    ;; =========================================================================
    (adding-children world)

    ;; (warn "adding absolute coordinates -----------------------------------")
    (boxes:absolute-coordinates world)

    ;; (warn "rendering -----------------------------------------------")
    (render world)


    ;; pink square follows the mouse ------------------------------------------
    (let ((app gui-app:*lisp-app*))
      (when (and (eq (gui-app:current-motion app)
                     window)
                 (gui-app:mouse-coordinates app))
        (gui-window:set-rgba "blue")
        (cairo:rectangle
         (car (gui-app:mouse-coordinates app))
         (cdr (gui-app:mouse-coordinates app))
         25
         25)
        (cairo:fill-path)))))

;;; events =====================================================================
(defun handle-key-pressed (entered key-name key-code mods lisp-window)
  (let ((model *basic-editor-model*))
    (cond

      ((and (equal key-name "F1")
            (null mods))
       (warn "------------ F1 Help --------------------")
       (warn "F1 = help")
       (warn "F8 = debug")
       (warn "Alt-n = new file")
       (warn "Alt-f = open file")
       (warn "Alt-s = save file")
       (warn "Alt-a = about")
       (warn "Alt-Home = move cursor to first row Home")
       (warn "Alt-End =  move cursor to last  row End")
       (warn "Ctrl-p = previous line")
       (warn "Ctrl-n = next line")
       (warn "Ctrl-b = backwards character")
       (warn "Ctrl-f = forwards character")
       (warn "-----------------------------------------")
       )
      ((and (equal key-name "F8")
            (null mods))
       (break "examine the models ~S" (list lisp-window *basic-editor-model*) ))

      ((and (equal key-name "F9")
            (null mods))
       (progn
         (warn "examine model ------------------------------")
         (warn "cursor ~S ~S" (~> model cursor row) (~> model cursor col))
         (warn "text ~S" (sycamore:rope-string (text model)))))
      ;; (:SHIFT :CTRL :ALT :WIN)
      ((and (equal key-name "j")
            (equal mods '(:CTRL)))
       ;; simulate Enter due to the menu focus problem
       (progn
         (insert-character-at-cursor model (for-enter) nil)
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
       (error "finish me TODO"))

      ((and (equal key-name "a")
            (equal mods '(:Alt)))
       (format T "keyboard selected about~%")
       (gui-window-gtk:present-about-dialog (about-dialog)))

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
       (setf (view-port-first-line model) (1- (view-port-first-line model)) ))
      ((and (equal key-name "n")
            (equal mods '(:CTRL)))
       (setf (view-port-first-line model) (1+ (view-port-first-line model)) ))
      ((equal key-name "Page_Up")
       (let ((fl (- (view-port-first-line model)
                    (find-page-rows model))))
         (setf (view-port-first-line model) fl)
         (move-cursor-to model fl 0)))
      ((equal key-name "Page_Down")
       (let ((fl (+ (view-port-first-line model)
                    (find-page-rows model))))
         (setf (view-port-first-line model) fl)
         (move-cursor-to model fl 0)))

      ((and (equal key-name "b")
            (equal mods '(:CTRL)))
       (setf (view-port-first-column model) (1- (view-port-first-column model))))
      ((and (equal key-name "f")
            (equal mods '(:CTRL)))
       (setf (view-port-first-column model) (1+ (view-port-first-column model))))
      ((equal key-name "Left")
       ;; handle menu bar focus problem
       (move-cursor-left model))
      ((equal key-name "Right")
       ;; handle menu bar focus problem
       (move-cursor-right model))
      ((equal key-name "Up")
       (move-cursor-up model)
       (warn "cursor on last line zzzz 1-- row ~S --first line ~S"
             (~> model cursor row)
             (~> model view-port-first-line))
       (when (< (~> model cursor row)
                (view-port-first-line model))
         (setf (view-port-first-line model) (~> model cursor row))))
      ((equal key-name "Down")
       (move-cursor-down model :ignored)
       (let ((pr (find-page-rows model)))
         (warn "cursor on last line zzzz 1-- row ~S --first line ~S"
               (~> model cursor row)
               (~> model view-port-first-line))
         (when (> (~> model cursor row)
                  (+
                   (view-port-first-line model)
                   pr))
           (setf (view-port-first-line model) (-
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
                   (warn "going to insert Return character for ~S ~S" entered key-name)
                   (insert-character-at-cursor model entered key-name))
                 (progn
                   (warn "going to insert character for ~S ~S" entered key-name)
                   (insert-character-at-cursor model entered key-name)))))))))

(defmethod process-event ((lisp-window basic-editor-window) event &rest args)
  (unless (member event '(:timeout :motion))
    (unless (eq *environment* :testing) (warn "event ~S ~S" event args)))
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
            (warn "mouse state ~S" (gui-app:mouse-button gui-app:*lisp-app*))
            (let*
                ((children (~> *basic-editor-model*
                               world
                               boxes:children
                               (nth 1 _)
                               boxes:children))
                 (first-child-found
                   (car (loop for c in children
                              when (boxes:mouse-over-p c)
                                collect c))))
              (warn "model world children under mouse ~S"
                    first-child-found)
              (when first-child-found
                (move-cursor-to *basic-editor-model* (row first-child-found) (col first-child-found)))
              )))
    (:released
          (destructuring-bind ((button x y)) args
            (gui-app:mouse-button-released button)
            (warn "mouse state released ~S" (gui-app:mouse-button gui-app:*lisp-app*))))
    (:scroll)
    (:resize
     (destructuring-bind ((w h)) args
       (gui-window:window-resize w h lisp-window)
       (setf (width lisp-window) w
             (height lisp-window) h)))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
       ;; example of accessing gtk window object
       ;; (gtk4:widget-grab-focus (gui-window:gir-window lisp-window))

       (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))
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
          (if (current-file *basic-editor-model*)
              ;; then
              (gui-window-gtk:present-file-save-dialog
               :title "Save me As"
               :initial-folder (format nil "~A"
                                       (uiop/pathname:pathname-directory-pathname
                                        (current-file *basic-editor-model*)))

               :initial-file (current-file *basic-editor-model*))
              ;; else
              (gui-window-gtk:present-file-save-dialog
               :title "Save me As")))
         ((equalp action "quit")
          (format T "menu selected quit~%")
          (gui-window-gtk:close-all-windows-and-quit))
         ;; Help
         ((equalp action "about")
          (format T "menu selected about~%")
          (gui-window-gtk:present-about-dialog (about-dialog)))
         (T
          (format T "unhandled menu action ~S~%" action)))
       ;; possibly steal menu focus
       ))
    (otherwise
     (warn "not handled event ~S ~S" event args)))

  ;; moving widgets -------------------------
  ;; (warn "may implement moving widgets in response to actions)
  ;; redrawing ------------------------------
  (gui-window:redraw-canvas lisp-window (format  nil "EVENT_~A" event)))

;;; main =======================================================================
(defun menu-bar (app lisp-window)
  (let ((menu (gio:make-menu)))
    ;; stop the annoying menu selection when left and right arrows are pressed
    ;; i could not make it work
    ;; (gir:invoke (menu 'gtk_widget_set_focusable) nil)


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
   ;; unless i cen fix the problem of unwanted menu focus
   ;; I will not use Gtk4 menu
   ;; gui-window-gtk:*client-fn-menu-bar* 'basic-editor::menu-bar
   gui-window-gtk:*client-fn-open-file* 'basic-editor::open-file
   gui-window-gtk:*client-fn-cancel-open-file* 'basic-editor::cancel-open-file
   gui-window-gtk:*client-fn-save-file* 'basic-editor::save-file
   gui-window-gtk:*client-fn-cancel-save-file* 'basic-editor::cancel-save-file

   *basic-editor-model* (make-instance 'basic-editor-model)
   boxes::*model* *basic-editor-model*
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
