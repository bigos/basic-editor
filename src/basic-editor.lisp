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

(defclass/std basic-editor-model (boxes:model)
  ((text :std "" :type string)
   (text-structure :type text-structure)
   (cursor :std (make-instance 'cursor :row 0 :col 0 :text-position 0))
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
   ;; TODO
   (wrap-at-column :std 80) ; when in wrap mode column we wrap at
   ;; debug
   ))

(defclass/std text-row ()
  ((row  :type integer)
   (home :type integer)
   (end  :type integer)))

(defclass/std text-structure ()
  ((data :type hash-table)))

(defclass/std basic-editor-window (boxes:boxes-window) (()))

(defclass/std basic-editor-character (boxes:node-character)
  ((row)
   (col)
   (pos)
   (outside)))

(defclass/std basic-editor-cursor (boxes:node-character)
  ((row)
   (col)
   (pos)
   (outside)))

(defclass/std cursor (boxes:node-character)
  ((row)
   (col)
   (text-position)))

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
    (format stream "~Sx~S - ~S"
            (row obj)
            (col obj)
            (text-position obj))))
(defmethod print-object ((obj text-row) stream)
  (print-object-inner obj stream))

;;; ============================================================================
;;; review that

(defmethod cursor-position ((cursor cursor))
  (cons (~> cursor row)
        (~> cursor col)))

(defmethod move-cursor-to-position ((model basic-editor-model) position)
  (let* ((cursor (~> model cursor))
         (the-data (data (text-structure model)))
         (the-row (loop for hk being the hash-key in the-data
                        for r = (gethash hk the-data)
                        until (and (>= position (home r))
                                   (<= position (end r)))
                        finally (return r))))
    (setf
     (text-position cursor) position
     (row cursor) (row the-row)
     (col cursor) (- position (home the-row)))))

;;; ============================================================================
(defmethod cursor-stats ((model basic-editor-model))
  (sample-text-stats model))

(defmethod the-container ((model basic-editor-model))
  (~> model world boxes::children (nth 1 _)))

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
  (let ((rx (text-stats model)))
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

(defun max-col (row)
  (if row
      (max-col2 row)
      0))

(defmethod max-col2 ((row text-row))
  (1- (- (end row)
         (home row))))

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


(defun sample-text-stats (model)
  (assert (typep (text model) 'simple-array))
  (let* (
         ;; (text-container-width (~> model world width (- _ 20 20)))
        (text (text model))
        ;; (font-size 18)
        ;; (margin-horizontal 0)
        ;; (margin-vertical 0)
        ;; (bwidth  (calculate-bwidth model))
        ;; (bheight (calculate-bheight model ))
        ;; (wrap-column (wrap-at-column model))
        ;; (wrap-col (view-port-columns model))
        (lines-hash-table (make-hash-table)))
    ;; (warn "zzzzz cols zzzzzzz ~S ~S"
    ;;       wrap-column
    ;;       wrap-col)

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

;;; ghex is my hex editor
(defun text-stats (model)
  (sample-text-stats model))

(defmethod reload-text-structure ((model basic-editor-model))
  ;; (warn "=========== going to load string ================ ~S" (text model))
  (let ((stats (sample-text-stats model)))
    (warn "got stats ~S" stats)
    (setf (text-structure model) (make-instance 'text-structure :data stats))
    ))

(defmethod reload-text-structure :after ((model basic-editor-model))
  ;; (warn "after reloading text structure")
  ;; after removing the last line cursor need to move to last position of the
  ;; previous line
  ;; after removing
  ;; validate cursor
  ;; if cursor is
  ;; beyond the last row, move it to the last character of the last row
  ;; beyond the last character on the line, move it to last character
  ;; before the first column, move it to the first column
  ;;
  ;; zzzzzzzzzzzzz
  )

(defun is-first-line (model)
  (zerop  (~> model cursor row)))

(defun is-last-line (model)
  (let ((found-last-row (hash-table-count (~> model text-structure data))))
    (when found-last-row
      (>=
       (~> model cursor row)
       found-last-row))))

(defmethod move-cursor-to-text-position ((model basic-editor-model) text-position)
  (warn "finish me"))

(defmethod valid-cursor-position ((model basic-editor-model) row col)
  (reload-text-structure model)

  (let ((last-row (last-row model))
        (current-row (current-row model)))

    (let ((valid-row (and (>= row 0)
                          (<= row (row  last-row))))
          (valid-col (and current-row
                          (<= 0 col (max-col current-row)))))
      (warn "early validation ~S ~S" valid-row valid-col)
      (warn "current row ~S" (current-row model))
      (warn "row text: ~S" (when (current-row model)
                             (row-text (current-row model) (text model))))
      (let ((validated (and valid-row
                            valid-col)))
        (if validated
            (progn
              ;; (warn "info: cursor position ~S ~S is valid" row col)
              T)
            (progn
              ;; (warn "invalid cursor position ~S ~S" row col)
              nil))))))

(defmethod move-cursor-to ((model basic-editor-model) row col)
    (let ((nth-row (nth-row model row)))
      (move-cursor-to-position model (+ col (home nth-row)))))

(defmethod move-cursor-left ((model basic-editor-model))
  (when (> (~> model cursor text-position) 0)
    (move-cursor-to-position model (1- (~> model cursor text-position)))))

(defmethod move-cursor-right ((model basic-editor-model))
  (move-cursor-to-position model (1+ (~> model cursor text-position))))

(defmethod move-cursor-up ((model basic-editor-model))
  (let ((column (~> model cursor col))
        (previous-row (previous-row model)))
    (when previous-row
      (move-cursor-to-position model (min
                                      (1- (end previous-row))
                                      (+ column (home previous-row)))))))

(defmethod move-cursor-down ((model basic-editor-model) ignored)
  (let ((column (~> model cursor col))
        (next-row (next-row model)))
    (when next-row
      (move-cursor-to-position model (min
                                      (1- (end next-row))
                                      (+ column (home next-row)))))))

(defmethod move-cursor-home ((model basic-editor-model))
  (let ((cur-row (current-row model)))
    (when cur-row
      (move-cursor-to-position model (home cur-row)))))

(defmethod move-cursor-end ((model basic-editor-model) ignored)
  (let ((cur-row (current-row model)))
    (when cur-row
      (move-cursor-to-position model (1- (end cur-row))))))

(defmethod move-cursor-first-line-home ((model basic-editor-model))
  (move-cursor-to-position model 0))

(defmethod move-cursor-last-line-end ((model basic-editor-model))
  (let ((last-row (last-row model)))
    (move-cursor-to-position model  (1- (end last-row)))))

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

(defun for-enter ()
  (format nil "~%"))

(defmethod delete-character-at-cursor ((model basic-editor-model))
  (let ((cur-pos (find-cursor-position model)))

    ;; (warn "will delete at row ~S col ~S pos ~S"
    ;;       (~> model cursor row)
    ;;       (~> model cursor col)
    ;;       cur-pos)
    (if cur-pos
        (progn
          (setf (text model) (format nil "~A~A"
                                     (subseq (text model) 0
                                             cur-pos)
                                     (subseq (text model) (+ 1 cur-pos)
                                             (length (text model)))))
          (reload-text-structure model))
        ;; (warn "No cursor position found, possibly no text")
        )))

(defmethod insert-character-at-cursor ((model basic-editor-model) entered key-name)
  ;; TODO this desperately needs improving and testing
  ;; (warn "before insert")
  ;; (warn "~S"
  ;;       (text model))

  (let ((cur-pos (find-cursor-position model)))
    (if cur-pos
        (progn                          ; then
          ;; (warn "cursor pos is present")
          (setf (text model)


                (if (equal key-name "Return")
                    (progn              ;then
                      ;; (warn "doing Return")
                      (format nil "~A~A~A"
                              ;;  pre insert
                              ;; TODO it fails here, we need to test extensively this part in different cases
                              ;; (warn "pre insert")
                              ;; (break "before subrope ~S ~S" (length (text model)) (text model))

                              (subseq (text model) 0
                                                   (+ 0  cur-pos))
                              ;; the insert
                              ;; (warn "the insert")
                              (for-enter)

                              ;; post insert
                              ;; (warn "post insert")
                              (subseq (text model) (+ 0 cur-pos)
                                                   (length (text model)))
                              ;; (warn "after post insert")
                              ))
                    ;; ---------------------------------------------------------------
                    (progn
                      ;; (warn "doint NON Return")
                      (format nil "~A~A~A"
                              ;;  pre insert
                              (subseq (text model) 0
                                                   cur-pos)
                              ;; the insert
                              (cond
                                ((equal key-name "Return")
                                 (for-enter))
                                (T entered))
                              ;; post insert
                              (subseq (text model) (+ 0 cur-pos)
                                      (sycamore:rope-length (text model)))))))
          (reload-text-structure model)
          ;; ------------------------------------------------------
          (cond
            ((equal key-name "Return")
             ;; (warn "move cursor return 1")
             (move-cursor-down model :ignored)
             (move-cursor-home model))
            (T
             ;; (warn "move cursor normal 1")
             (move-cursor-right model))))

        (progn                          ; else
          ;; TODO start adding tests
          ;; (warn "cursor pos is NIL")
          (setf (text model) (cond
                               ((equal key-name "Return")
                                (for-enter))
                               (T entered)))
          (reload-text-structure model)
          (cond
            ((equal key-name "Return")
             ;; (warn "move cursor return 2")
             (move-cursor-down model :ignored)
             (move-cursor-home model))
            (T
             ;; (warn "move cursor normal 2")
             (move-cursor-right model))))))
  ;; (progn
  ;;   (warn "---------- done insert --------------")
  ;;   (warn "cursor ~S ~S" (~> model cursor row) (~> model cursor col))
  ;;   (warn "cursor text  ~S" (~> model text))
  ;;   (warn "---------- finished insert --------------"))

  )

;;; ----------------------------------------------------------------------------
(defun new-file ()
  (let ((model *basic-editor-model*))
    (setf
     (text model) "edit something"
     (current-file model) nil)))

;; (funcall *client-fn-open-file* (cancelled-value))
(defun open-file (filepath)
   (case (car  filepath)
     (:cancelled
      nil)
     (:selected
      (let* ((model *basic-editor-model*)
            (clean-filepath (subseq (cdr  filepath) 7))
            (text-content (alexandria:read-file-into-string clean-filepath)))
        ;; (warn "going to load ~S" clean-filepath)
        (setf (current-file model) clean-filepath)
        (setf (text model) text-content)
        (reload-text-structure model)))))

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
        (text model)
        clean-filepath
        :if-exists :supersede
        :if-does-not-exist :create)))))

;;; we need text-container, wrap,
(defmethod wrap-column ((model basic-editor-model) text-container-width bwidth)
  (let* ((world (world model)))
    (ecase (text-wrap model)
      (:trim *boundary-gigabyte*) ;; trim wraps on ridiculously high column
      (:wrap  (- (floor (/ text-container-width
                           (1+ bwidth)))
                 2)))))

;;; drawing ====================================================================
(defun calculate-bwidth (model)
  (let* ((font-size 18)
         (margin-horizontal 0)
         (margin-vertical 0)
         (text-for-size  "pOly()/_")
         (text-data (text-size text-for-size font-size ))
         (twidth (floor (/ (getf text-data :width)
                           (length text-for-size)))))

    (+ twidth  0)))

(defun calculate-bheight (model)
  (let* ((font-size 18)
         (margin-horizontal 0)
         (margin-vertical 0)
         (text-for-size  "pOly()/_")
         (text-data (text-size text-for-size font-size ))
         (theight          (getf text-data :height)))

    (+ theight 0)))

(defun calculate-chars (model)
  (let*
      ((world (world model))
       (text-container
         (make-node 20
                    340
                    (- (width world) 20 20)
                    (- (height world) 60) "yellow"))
       (font-size 18)
       (margin-horizontal 0)
       (margin-vertical 0)
       (bwidth  (calculate-bwidth model))
       (bheight (calculate-bheight model ))
       (wrap-column
         (if (and text-container (> bwidth 0))
             (floor (/ (width text-container )
                       (+ bwidth 3)))
             80)))

    (setf (wrap-at-column model) wrap-column)

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
                                   :outside outside)
              into the-chars
          when (eq (~> model cursor text-position) pos)
            do (setf
                (~> model cursor row) row
                (~> model cursor col) col)
          when (eq (~> model cursor text-position) pos)
            collect (make-instance 'basic-editor-cursor
                                   :bchar #\_
                                   :font-size font-size
                                   :coordinates-relative
                                   (make-coordinates-relative
                                    relx
                                    rely)
                                   :width bwidth
                                   :height bheight
                                   :color "#FFFF8844"
                                   :row row
                                   :col col
                                   :pos pos)
              into cursors
          finally
             ;; (warn "CURSORS ~S" cursors)
             (setf (all-lines-count model) row)
             (setf (view-port-lines model) (when max-seen-row (1+ max-seen-row)))
             (setf (view-port-columns model) max-seen-col)
             (setf (seen-chars model) the-chars)
             (return (list
                      :chars the-chars
                      :cursor cursors)))))

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

(defun adding-children (model)
  (let ((world (world model)))
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
                                                (cursor model)
                                                ))
                   (let ((text-container (make-node 20
                                                    340
                                                    (- (width world) 20 20)
                                                    (- (height world) 60) "yellow"))
                         (zzz (calculate-chars model)))
                     (add-children text-container
                                   (getf zzz :chars))
                     (add-children text-container
                                   (getf zzz :cursor)))

                   (make-instance 'node-text
                                  :coordinates-relative (make-coordinates-relative 10 50)
                                  :width (- (width world) 40)
                                  :height 30
                                  :color "white"
                                  :wrap 'truncate
                                  :text (format nil
                                                "rowcols ~S ~S, fl ~S, fc ~S ~S"
                                                (let ((cursor-cons (cursor-position (cursor model))))
                                                  (format nil "[~S ~S]"
                                                          (car cursor-cons)
                                                          (cdr cursor-cons)))
                                                (cons
                                                 (view-port-lines
                                                  model)
                                                 (view-port-columns
                                                  model))
                                                (view-port-first-line   model)
                                                (view-port-first-column model)
                                                (sycamore:rope-string (text model))
                                                ))))))

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

  (let ((model *basic-editor-model*)
        (world (boxes::make-node-down
                0 0 (width window) (height window) "#cccccc88")))
    (setf (world model) world) ; zzzzzzzzzzzzzzzzzzz
    (boxes:absolute-coordinates world)

    ;; =========================================================================
    (adding-children model)

    ;; (warn "adding absolute coordinates -----------------------------------")
    (boxes:absolute-coordinates world)

    ;; (warn "rendering -----------------------------------------------")
    (render world)

    ;; blue square follows the mouse ------------------------------------------
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
                                     (view-port-size model)
                                     :view-port-lines
                                     (view-port-lines model)
                                     :view-port-columns
                                     (view-port-columns model)
                                     :view-port-first-line
                                     (view-port-first-line model)
                                     :view-port-first-column
                                     (view-port-first-column model)
                                     :container-width-pixels
                                     (boxes::width (the-container model))
                                     :container-height-pixels
                                     (boxes::height (the-container model))
                                     :wrap-at-column
                                     (wrap-at-column model)))
               (warn "--------------------------------------------"))
             (progn
               (warn "no text loaded")))))
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
       ;; (warn "cursor on last line zzzz 1-- row ~S --first line ~S"
       ;;       (~> model cursor row)
       ;;       (~> model view-port-first-line))
       (when (< (~> model cursor row)
                (view-port-first-line model))
         (setf (view-port-first-line model) (~> model cursor row))))
      ((equal key-name "Down")
       (move-cursor-down model :ignored)
       (let ((pr (find-page-rows model)))
         ;; (warn "cursor on last line zzzz 1-- row ~S --first line ~S"
         ;;       (~> model cursor row)
         ;;       (~> model view-port-first-line))
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
            ;; (warn "mouse state ~S" (gui-app:mouse-button gui-app:*lisp-app*))
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
              ;; (warn "model world children under mouse ~S"
              ;;       first-child-found)
              (when first-child-found
                (move-cursor-to *basic-editor-model* (row first-child-found) (col first-child-found)))
              )))
    (:released
          (destructuring-bind ((button x y)) args
            (gui-app:mouse-button-released button)
            (warn "mouse state released ~S" (gui-app:mouse-button gui-app:*lisp-app*))))
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
     (unless (eq event  :key-released)
       (warn "not handled event ~S ~S" event args))))

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
