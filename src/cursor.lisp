(declaim (optimize (speed 0) (safety 3) (debug 3)))
;;; basic-editor

;; (ql:quickload :basic-editor)
;; (basic-editor:main)
;; (warn "hello")

(in-package #:basic-editor)
;; (setf *break-on-signals* T)
;; (setf *print-circle* T)

(defclass/std cursor (boxes:node-character)
  ((row)
   (col)
   (text-position)))

;;; review that

(defmethod cursor-position ((cursor cursor))
  (cons (~> cursor row)
        (~> cursor col)))

(defmethod move-cursor-to-position ((model basic-editor-model) position)
  (reload-text-structure model)
  (let* ((cursor (~> model cursor))
         (the-data (data (text-structure model)))
         (the-row (loop for hk being the hash-key in the-data
                        for r = (gethash hk the-data)
                        until (and (>= position (home r))
                                   (<= position (end r)))
                        finally (return r))))
    (if (and (>= position (home the-row))
             (<= position (end the-row)))
        (setf
         (text-position cursor) position
         (row cursor) (row the-row)
         (col cursor) (- position (home the-row)))
        (progn
          (warn "moving cursor outside normal range ~S" (list :position position :zzz the-row))
          (setf
           (text-position cursor) (1- (end the-row))
           (row cursor) (row the-row)
           (col cursor) (- (end  the-row) (home the-row)))))))

(defmethod print-object ((obj cursor) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~Sx~S - ~S"
            (row obj)
            (col obj)
            (text-position obj))))

(defmethod valid-cursor-position ((model basic-editor-model) row col)
  (reload-text-structure model)

  (let ((last-row (last-row model))
        (current-row (current-row model)))

    (let ((valid-row (and (>= row 0)
                          (<= row (row  last-row))))
          (valid-col (and current-row
                          (<= 0 col (max-col current-row)))))
      (and valid-row
           valid-col))))

(defmethod move-cursor-to ((model basic-editor-model) row col)
    (let ((nth-row (nth-row model row)))
      (move-cursor-to-position model (+ col (home nth-row)))))

(defmethod move-cursor-left ((model basic-editor-model))
  (when (> (~> model cursor text-position) 0)
    (move-cursor-to-position model (1- (~> model cursor text-position)))))

(defmethod move-cursor-right ((model basic-editor-model))
  (let ((last-row (last-row model)))
    (move-cursor-to-position model (min
                                    (1+ (~> model cursor text-position))
                                    (1- (end last-row))))))

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
