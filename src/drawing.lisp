(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:basic-editor)

;;; drawing ====================================================================
(defun calculate-bwidth (model)
  (let* ((font-size 18)
         (text-for-size  "pOly()/_")
         (text-data (text-size text-for-size font-size ))
         (twidth (floor (/ (getf text-data :width)
                           (length text-for-size)))))
    (+ twidth 0)))

(defun calculate-chars (model)
  (let*
      ((world (world model))
       (text-container
         (make-node 20
                    340
                    (- (width world) 20 20)
                    (- (height world) 60) "yellow"))
       (margin-horizontal 0)
       (margin-vertical 0)
       (font-size 18)
       (text-for-size  "pOly()/_")
       (text-data (text-size text-for-size font-size ))
       (twidth (calculate-bwidth model))
       (theight          (getf text-data :height))

       (bwidth  (+ twidth 0))
       (bheight (+ theight 0))
       (wrap-column
         (if (and text-container (> bwidth 0))
             (floor (/ (width text-container )
                       (+ bwidth 3)))
             80))
       (last-relx nil)
       (last-rely nil))

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
                         (* (- col (first-column (view-port model)))
                            (1+ bwidth) )))
          for rely = (+ margin-vertical
                        (ceiling
                         (* (- row (first-line (view-port model)))
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
          do (setf
              last-relx relx
              last-rely rely)
          finally
             ;; (warn "CURSORS ~S" cursors)
             (setf (all-lines-count model) row)
             (setf (lines (view-port model)) (when max-seen-row (1+ max-seen-row)))
             (setf (max-column (view-port model)) max-seen-col)
             (setf (seen-chars model) the-chars)
             (return (list
                      :chars the-chars
                      :cursor (if cursors
                                  cursors
                                  (when (and last-relx last-rely)
                                    (list
                                     (make-instance 'basic-editor-cursor
                                                    :bchar #\_
                                                    :font-size font-size
                                                    :coordinates-relative
                                                    (make-coordinates-relative
                                                     (+ last-relx bwidth)
                                                     last-rely)
                                                    :width bwidth
                                                    :height bheight
                                                    :color "#FFFF8844"
                                                    :row nil
                                                    :col nil
                                                    :pos nil)))))))))

(defun text-size (text text-size)
  (handler-bind
      ((simple-warning
         (lambda (w)
           (when  (alexandria:starts-with-subseq
                   "function returned with status ~a"
                   (simple-condition-format-control w))
             (muffle-warning w)))))
    (cairo:select-font-face
     "Ubuntu Mono"
     ;;"Advaita Mono"
     ;; "Liberation Mono"
     :normal :normal))

  (handler-bind
      ((simple-warning
         (lambda (w)
           (when  (alexandria:starts-with-subseq
                   "function returned with status ~a"
                   (simple-condition-format-control w))
             (muffle-warning w)))))
    (cairo:set-font-size text-size))

  (multiple-value-bind (xb yb width height)
      (handler-bind
          ((simple-warning
             (lambda (warning)
               ;; i found it with debugger
               ;; (break "examine the warning ~S" warning)
               (when  (alexandria:starts-with-subseq
                      "function returned with status ~a"
                      (simple-condition-format-control warning))
                 (muffle-warning warning)))))
        (cairo:text-extents (format nil "~A" text)))

    (list :xb xb :yb yb :width width :height height)))

(defun adding-children-viewport (model world)
  (let ((outer-container (boxes:make-node-right 20
                                                 340
                                                 (- (width world) 20 20)
                                                 (- (height world) 60) "black")))

    (let ((linenum-container (make-node 20
                                        120
                                        120
                                        (- (height world) 60) "red"))
          (text-container (make-node 20
                                     340
                                     (- (width world) 20 20)
                                     (- (height world) 60) "yellow"))
          (calculated-characters (calculate-chars model)))

      (add-children text-container
                    (getf calculated-characters :chars))
      (add-children linenum-container
                    (loop for lc in (~> text-container boxes:children)
                          when (and (typep lc 'basic-editor-character)
                                    (zerop (col lc)))
                            collect
                            (progn
                              ;; (warn "zaq ~s" (row lc))
                              (make-instance 'node-text
                                             :coordinates-relative (make-coordinates-relative 10
                                                                                              (~> lc boxes:coordinates-relative boxes:y))
                                             :width 80
                                             :height 15
                                             :color "white"
                                             :wrap 'truncate
                                             :text (format nil "~S" (~> lc row ))))))
      (add-children text-container
                    (getf calculated-characters :cursor))


      (add-children outer-container
                    (if (show-line-numbers model)
                        (list linenum-container
                              text-container)
                        (list text-container))))))

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
                                  :text (format nil "Heading , mouse button ~S, wrap ~S"
                                                (gui-app:mouse-button gui-app:*lisp-app*)
                                                (text-wrap model)))

                   (adding-children-viewport model world)

                   (make-instance 'node-text
                                  :coordinates-relative (make-coordinates-relative 10 50)
                                  :width (- (width world) 40)
                                  :height 30
                                  :color "white"
                                  :wrap 'truncate
                                  :text (format nil
                                                "rowcols ~S ~S, fl ~S, fc ~S"
                                                (let ((cursor-cons (cursor-position (cursor model))))
                                                  (format nil "[~S ~S]"
                                                          (car cursor-cons)
                                                          (cdr cursor-cons)))
                                                (list
                                                 :lines
                                                 (lines (view-port      model))
                                                 :max-column
                                                 (max-column (view-port model)))
                                                (first-line (view-port   model))
                                                (first-column (view-port model))
                                                ))
                   ))))

(defmethod draw-window ((window basic-editor-window))
  ;; paint background
  (let ((cv 0.95)) (cairo:set-source-rgb  cv cv cv))
  (cairo:paint)

  ;; ==================================================================
  (let ((model *basic-editor-model*)
        (world (boxes:make-node-down
                0 0 (width window) (height window) "#cccccc88")))
    (setf (world model) world)

    ;; =========================================================================
    (adding-children model)
    (render world)

    ;; blue square follows the mouse ------------------------------------------
    (let ((app gui-app:*lisp-app*))
      (when (and (eq (gui-app:current-motion app)
                     window)
                 (gui-app:mouse-coordinates app))
        ;; (gui-window:set-rgba "blue")
        ;; (cairo:rectangle
        ;;  (car (gui-app:mouse-coordinates app))
        ;;  (cdr (gui-app:mouse-coordinates app))
        ;;  25
        ;;  25)
        ;; (cairo:fill-path)
    (render-mouse (gui-app:mouse-coordinates app ))))))

(defun render-mouse (mouse-position)
  (when mouse-position
    (let* (
           (mx (car mouse-position))
           (my (cdr mouse-position))
           (px (+ mx 20))
           (py (+ my 20))
           (po 6)
           (ao 15))
      (when mouse-position
        (labels ((drrr ()
                   (cairo:line-to (+ 50 mx) (+ po 50 my)) ; end
                   (cairo:line-to px (+ po py))
                   (cairo:line-to (+ ao mx) (+ 50 my))
                   (cairo:line-to mx my)  ;mp
                   (cairo:line-to (+ 50 mx) (+ ao my))
                   (cairo:line-to (+ po px) py)
                   (cairo:line-to (+ po 50 mx) (+ 50 my)) ; end
                   (cairo:line-to (+ 50 mx) (+ po 50 my)) ; end
                   ))
          (drrr)
          (cairo:set-source-rgba 0.2 1.0 0.3 0.3)
          (cairo:fill-path)

          (cairo:set-line-width 1.0)
          (drrr)
          (cairo:set-source-rgb 0.0 0.0 0.0) ; http://davidbau.com/colors/
          (cairo:stroke))))))
