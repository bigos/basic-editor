(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:basic-editor-test)

;; (ql:quickload :basic-editor/tests)
;; (in-package #:basic-editor-test)
;; (run! 'basic-editor-suite)
;; (run! 'basic-editor-resizing)

(defun test-all ()
  "Compile and run all test in one command."
  (ql:quickload :basic-editor/tests)
  (fiveam:run-all-tests))

(defun snapshot (experimental-window &optional log)
  (gui-drawing:simulate-draw-func experimental-window log))

(defun char-kids (model)
  (serapeum:~>  model
                basic-editor::world
                boxes::children
                (nth 1 _)
                boxes::children))

(defun file-single-line-fname ()
  (merge-pathnames
   "tests/example_texts/single_line.txt"
   (asdf:system-source-directory :basic-editor/tests)))

(defun file-single-line-content ()
  (alexandria:read-file-into-string (file-single-line-fname)))

(defun file-three-lines-fname ()
  (merge-pathnames
   "tests/example_texts/three_lines.txt"
   (asdf:system-source-directory :basic-editor/tests)))

(defun file-three-lines-content ()
  (alexandria:read-file-into-string (file-three-lines-fname)))

;;; ============= suites ================================================
(progn                                  ; suites
  (def-suite equality
      :description "Suite to test if 2 and 2 is 4")

  (def-suite basic-editor-suite
      :description "Suite to hold other suites and tests")

  (def-suite basic-editor-resizing
      :description "Suite for resizing"
      :in basic-editor-suite)

  (def-suite basic-editor-text
      :description "Suite for text"
      :in basic-editor-suite)

  (def-suite basic-editor-text-scrolling
      :description "Suite for text scrolling"
      :in basic-editor-text)

  (def-suite basic-editor-text-first-line-left
      :description "Suite for text scrolling of first line"
      :in basic-editor-text)

  (def-suite basic-editor-text-last-line-right
      :description "Suite for text scrolling of last line"
      :in basic-editor-text)

  (def-suite basic-editor-text-pressing-enter
      :description "Suite for testing Enter"
      :in basic-editor-text))

(in-suite equality)        ; ==================================

(test test-equality
  "test some equalities"
  (is (= 2 2))
  (is (= 4 (* 2 2)))
  (let ((expected 3)
        (got 3))
    (is (= expected got ))))

(in-suite basic-editor-resizing)        ; ==================================

(test first-resizing
  "test resizing"
  (let ((experimental-window (main :testing T))
        (model *basic-editor-model*))
    (is (equal nil (width experimental-window)))
    (is (equal nil (height experimental-window)))
    (process-event experimental-window :resize '(110 120))
    (is (= 110 (width experimental-window)))
    (is (= 120 (height experimental-window)))
    (process-event experimental-window :resize '(370 750))
    (is (= 370 (width experimental-window)))
    (is (= 750 (height experimental-window)))
    (process-event experimental-window :resize '(700 500))
    (is (= 700 (width experimental-window)))
    (is (= 500 (height experimental-window)))))

(in-suite basic-editor-suite)           ; ==================================

(in-suite basic-editor-text-scrolling)           ; ==================================

(test first-scrolling
  "test resizing"
  (let ((experimental-window (main :testing T))
        (model *basic-editor-model*)
        (world (boxes::make-node-down 0 0 600 400 "#cccccc88")))
    (be::new-file)
    (be::open-file (cons :selected "file:///home/jacek/.bashrc"))

    (setf (be::world model) world)
    (basic-editor::adding-children world)


    (process-event experimental-window :resize '(710 420))
    (is (= 710 (width experimental-window)))
    (is (= 420 (height experimental-window)))
    (is (= 0 (view-port-first-line model)))
    (is (= 0 (view-port-first-column model)))
    (is (eq 'boxes::node-down  (type-of (be::world model))))

    (is (eq #\b (basic-editor::bchar (nth 5 (char-kids model)))))
    (is (= 5 (be::col (nth 5 (char-kids model)))))
    (is (= 0 (be::row (nth 5 (char-kids model)))))

    (snapshot experimental-window "resize 710 420")

    (process-event experimental-window  :key-pressed '("n" "n" 57 (:ctrl)))
    (is (= 1 (view-port-first-line model)))
    (is (= 0 (view-port-first-column model)))
    (is (= 1 (be::row (nth 5 (char-kids model)))))

    (process-event experimental-window  :key-pressed '("n" "n" 57 (:ctrl)))
    (is (= 2 (view-port-first-line model)))
    (is (= 0 (view-port-first-column model)))
    (is (= 2 (be::row (nth 5 (char-kids model)))))

    (process-event experimental-window  :key-pressed '("p" "p" 33 (:ctrl)))
    (is (= 1 (view-port-first-line model)))
    (is (= 0 (view-port-first-column model)))
    (is (= 1 (be::row (nth 5 (char-kids model)))))

    (process-event experimental-window  :key-pressed '("f" "f" 41 (:ctrl)))
    (is (= 1 (view-port-first-line model)))
    (is (= 1 (view-port-first-column model)))
    (is (= 6 (be::col (nth 5 (char-kids model)))))
    (is (= 1 (be::row (nth 5 (char-kids model)))))

    (process-event experimental-window  :key-pressed '("f" "f" 41 (:ctrl)))
    (is (= 1 (view-port-first-line model)))
    (is (= 2 (view-port-first-column model)))
    (process-event experimental-window  :key-pressed '("b" "b" 56 (:ctrl)))
    (is (= 1 (view-port-first-line model)))
    (is (= 1 (view-port-first-column model)))

    (process-event experimental-window  :key-pressed '("p" "p" 33 (:ctrl)))
    (process-event experimental-window  :key-pressed '("b" "b" 56 (:ctrl)))
    (is (= 5 (be::col (nth 5 (char-kids model)))))
    (is (= 0 (be::row (nth 5 (char-kids model)))))

    ;;(break "check the model ~S" *basic-editor-model*)
    ))

(in-suite basic-editor-suite)           ; ==================================

(test test-single-line-fname
  "test single-line.txt file name"
  (is (equal (namestring (file-single-line-fname))
             "/home/jacek/Programming/Lisp/basic-editor/tests/example_texts/single_line.txt")))

(test test-single-line-content
  "test single-line.txt file content"
  (is (equal (file-single-line-content)
             (format nil
                     "Ala ma kota.~%"))))

(test test-three-lines-fname
      "test three-lines.txt file name"
      (is (equal (namestring (file-three-lines-fname))
                 "/home/jacek/Programming/Lisp/basic-editor/tests/example_texts/three_lines.txt")))

(test test-three-lines-content
      "test three-lines.txt file content"
      (is (equal (file-three-lines-content)
                 "I need to make sure
three lines movements
works as expected.
")))

(test char-children
      "children for the characters"
      (let ((experimental-window (main :testing T))
            (world (boxes::make-node-down
                    0 0 600 400 "#cccccc88"))
            (model *basic-editor-model*))

        (process-event experimental-window :resize '(710 420))
        ;; (snapshot experimental-window "what-is-the-size")
        (is (= 710 (width experimental-window)))
        (is (= 420 (height experimental-window)))

        (be::new-file)
        (be::open-file (cons :selected
                             (format nil "file://~A"
                                     (file-three-lines-fname))))

        (setf (be::world model) world)
        (basic-editor::adding-children world)
        (let* ((children ;;(~> world boxes::children (nth 1 _) boxes::children)
                 (char-kids model))
               (loaded-text (sycamore:rope-string (be::text model))))

          (is (equal (type-of model) 'BE::BASIC-EDITOR-MODEL))
          (is (equal (subseq loaded-text 0 14) "I need to make"))

          (is (= 61 (length children)))
          (is (equal #\d (be::bchar (nth 5 children))))
          (is (equal #\Space (be::bchar (nth 6 children))))
          (is (equal #\t (be::bchar (nth 7 children))))
          (is (equal #\o (be::bchar (nth 8 children)))))))

(in-suite basic-editor-text)           ; ==================================

(in-suite basic-editor-text-first-line-left)           ; ==================================

(test single-line-moving-left
      "single line moving left"
      (let ((experimental-window (main :testing T))
            (world (boxes::make-node-down
                    0 0 600 400 "#cccccc88"))
            (model *basic-editor-model*))

        (process-event experimental-window :resize '(710 250))
        ;; (snapshot experimental-window "what-is-the-size")
        (is (= 710 (width experimental-window)))
        (is (= 250 (height experimental-window)))

        (be::new-file)
        (be::open-file (cons :selected
                             (format nil "file://~A"
                                     (file-single-line-fname))))

        (setf (be::world model) world)
        (basic-editor::adding-children world)
        (let* ((children ;;(~> world boxes::children (nth 1 _) boxes::children)
                 (char-kids model))
               (loaded-text (sycamore:rope-string (be::text model))))

          (is (equal (type-of model) 'BE::BASIC-EDITOR-MODEL))
          (is (equal (subseq loaded-text 0 13) (format nil "Ala ma kota.~%")))
          (is (= 13 (length children)))

          ;; TODO finish the tests and response to moving cursor

          (snapshot experimental-window "loaded")
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 0 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 1 (~> model be::cursor be::col)))


          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 2 (~> model be::cursor be::col)))


          (process-event experimental-window :key-pressed '("" "Left" 113 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 1 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Left" 113 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 0 (~> model be::cursor be::col)))

          ;; fis those failing tests
          (process-event experimental-window :key-pressed '("" "Left" 113 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 0 (~> model be::cursor be::col)))


          (process-event experimental-window :key-pressed '("" "Left" 113 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 0 (~> model be::cursor be::col))))))

(in-suite basic-editor-text-last-line-right)           ; ==================================

(test three-lines-moving-right
      "three lines moving lines"
      (let ((experimental-window (main :testing T))
            (world (boxes::make-node-down
                    0 0 600 400 "#cccccc88"))
            (model *basic-editor-model*))

        (process-event experimental-window :resize '(710 250))
        ;; (snapshot experimental-window "what-is-the-size")
        (is (= 710 (width experimental-window)))
        (is (= 250 (height experimental-window)))

        (be::new-file)
        (be::open-file (cons :selected
                             (format nil "file://~A"
                                     (file-three-lines-fname))))
        (setf (be::world model) world)
        (basic-editor::adding-children world)
        (let* ((children ;;(~> world boxes::children (nth 1 _) boxes::children)
                 (char-kids model))
               (loaded-text (sycamore:rope-string (be::text model))))

          (is (equal (type-of model) 'BE::BASIC-EDITOR-MODEL))
          (is (equal (subseq loaded-text 0 20) (format nil
                                                       "I need to make sure~%")))
          (is (= 61 (length children)))

          ;; TODO finish the tests and response to moving cursor

          (snapshot experimental-window "loaded")
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 0 (~> model be::cursor be::col)))

          ;; move 2 rows down

          (process-event experimental-window :key-pressed '("" "Down" 116 NIL))
          (is (eq 1 (~> model be::cursor be::row)))
          (is (eq 0 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Down" 116 NIL))
          (is (eq 2 (~> model be::cursor be::row)))
          (is (eq 0 (~> model be::cursor be::col)))

          ;; move to the right
          (loop for x from 1 to 16
                do (process-event experimental-window :key-pressed '("" "Right" 114 NIL)))
          (is (eq 2 (~> model be::cursor be::row)))
          (is (eq 16 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 2 (~> model be::cursor be::row)))
          (is (eq 17 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 2 (~> model be::cursor be::row)))
          (is (eq 18 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 2 (~> model be::cursor be::row)))
          (is (eq 18 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 2 (~> model be::cursor be::row)))
          (is (eq 18 (~> model be::cursor be::col)))



         )
        )
      )

(test single-line-moving-right
      "single line moving right"
      (let ((experimental-window (main :testing T))
            (world (boxes::make-node-down
                    0 0 600 400 "#cccccc88"))
            (model *basic-editor-model*))

        (process-event experimental-window :resize '(710 250))
        ;; (snapshot experimental-window "what-is-the-size")
        (is (= 710 (width experimental-window)))
        (is (= 250 (height experimental-window)))

        (be::new-file)
        (be::open-file (cons :selected
                             (format nil "file://~A"
                                     (file-single-line-fname))))

        (setf (be::world model) world)
        (basic-editor::adding-children world)
        (let* ((children ;;(~> world boxes::children (nth 1 _) boxes::children)
                 (char-kids model))
               (loaded-text (sycamore:rope-string (be::text model))))

          (is (equal (type-of model) 'BE::BASIC-EDITOR-MODEL))
          (is (equal (subseq loaded-text 0 13) (format nil "Ala ma kota.~%")))
          (is (= 13 (length children)))

          ;; TODO finish the tests and response to moving cursor

          (snapshot experimental-window "loaded")
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 0 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 1 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 2 (~> model be::cursor be::col)))


          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 3 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 4 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 5 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 6 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 7 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 8 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 9 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 10 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 11 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 12 (~> model be::cursor be::col)))

          ;; ;; on last row, do not go to the next row
          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 12 (~> model be::cursor be::col)))

          ;; ;; but stay on last position
          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 12 (~> model be::cursor be::col)))

          ;; ;; and stay on last position
          (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 12 (~> model be::cursor be::col)))

          )))

(in-suite basic-editor-text-pressing-enter)           ; ==================================

(test single-line-moving-left
      "single line moving left"
      (let ((experimental-window (main :testing T))
            (world (boxes::make-node-down
                    0 0 600 400 "#cccccc88"))
            (model *basic-editor-model*))

        (process-event experimental-window :resize '(710 250))
        ;; (snapshot experimental-window "what-is-the-size")
        (is (= 710 (width experimental-window)))
        (is (= 250 (height experimental-window)))

        (be::new-file)
        (be::open-file (cons :selected
                             (format nil "file://~A"
                                     (file-single-line-fname))))

        (setf (be::world model) world)
        (basic-editor::adding-children world)
        (let* ((children ;;(~> world boxes::children (nth 1 _) boxes::children)
                 (char-kids model))
               (loaded-text (sycamore:rope-string (be::text model))))

          (is (equal (type-of model) 'BE::BASIC-EDITOR-MODEL))
          (is (equal (subseq loaded-text 0 13) (format nil "Ala ma kota.~%")))
          (is (= 13 (length children)))

          ;; TODO finish the tests and response to moving cursor

          (snapshot experimental-window "loaded")
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 0 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "End" 115 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 12 (~> model be::cursor be::col)))

          (process-event experimental-window :key-pressed '("" "Home" 110 NIL))
          (is (eq 0 (~> model be::cursor be::row)))
          (is (eq 0 (~> model be::cursor be::col)))

          )))
