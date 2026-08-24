(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:file-selectors)

;;; file selectors -------------------------------------------------------------

(defun extract-filepath (current-file-pair)
  (assert (eql (car current-file-pair) :selected))
  (assert (stringp (cdr current-file-pair)))
  (subseq (cdr current-file-pair) 7))

(defun open-file (current-file-pair)
  (warn "opening file with ~S" current-file-pair)
   (ecase (car  current-file-pair)
     (:cancelled
      nil)
     (:selected
      (let* ((model *basic-editor-model*)
             (clean-filepath (extract-filepath current-file-pair))
            (text-content (alexandria:read-file-into-string clean-filepath)))
        (warn "going to load ~S" clean-filepath)
        (setf (current-file model) current-file-pair)
        (setf (text model) text-content)
        (reload-text-structure model)))))

(defun cancel-open-file (ddd)
  (warn "Closed open file ~s" ddd))

(defun save-file (current-file-pair)
  (ecase (car current-file-pair)
    (:cancelled
     nil)
    (:selected
     (let ((model *basic-editor-model*)
           (clean-filepath (extract-filepath current-file-pair)))
       (if (equal clean-filepath (current-file model))
           (warn "going to save ~S" clean-filepath)
           (warn "going to save AS ~S" clean-filepath))
       (setf (current-file model) clean-filepath)
       (alexandria:write-string-into-file
        (text model)
        clean-filepath
        :if-exists :supersede
        :if-does-not-exist :create)))))

(defun cancel-save-file (ddd)
  (warn "Closed save file ~s" ddd))

(defun file-save-selector ()
  (let ((current-file-pair (current-file *basic-editor-model*)))
    (if current-file-pair
        ;; then
        (let ((current-file (extract-filepath current-file-pair)))
          (gui-window-gtk:present-file-save-dialog
           :title (format nil "Save me AS")
           :initial-folder (format nil "~A"
                                   (uiop/pathname:pathname-directory-pathname
                                    (uiop/pathname:absolute-pathname-p
                                     current-file)))
           :initial-file current-file))
        ;; else
        (gui-window-gtk:present-file-save-dialog
         :title "Save me As"))))
