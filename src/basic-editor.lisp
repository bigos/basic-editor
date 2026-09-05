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
