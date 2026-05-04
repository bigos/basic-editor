(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:basic-editor)

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
