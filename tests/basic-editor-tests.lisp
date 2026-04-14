(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:basic-editor-test)

;; (setf 5am:*on-error* :debug)

;; (ql:quickload :basic-editor/tests)
;; (in-package #:basic-editor-test)
;; run suites
;; (run! 'basic-editor-suite)
;; (run! 'basic-editor-resizing)
;; run single test
;; (run! 'single-line-moving-right )

(defun test-all ()
  "Compile and run all test in one command."
  (ql:quickload :basic-editor/tests)
  (fiveam:run-all-tests))

(defun snapshot (experimental-window &optional log)
  (gui-drawing:simulate-draw-func experimental-window log))

(defun load-file-and-model (fname)
  (let ((experimental-window (main :testing T))
        (world (boxes::make-node-down
                0 0 600 400 "#cccccc88"))
        (model *basic-editor-model*))
    (process-event experimental-window :resize '(710 250))

    (be::new-file)
    (be::open-file (cons :selected
                         (format nil "file://~A" fname)))
    (setf (be::world model) world)
    (be::adding-children world)

    (list :model model :experimental-window experimental-window)))

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

(defun file-single-line-empty-fname ()
  (merge-pathnames
   "tests/example_texts/single_line_empty.txt"
   (asdf:system-source-directory :basic-editor/tests)))

(defun file-single-line-empty-content ()
  (alexandria:read-file-into-string (file-single-line-empty-fname)))

(defun file-single-line-one-character-no-newline-fname ()
  (merge-pathnames
   "tests/example_texts/single_line_one_character_no_newline.txt"
   (asdf:system-source-directory :basic-editor/tests)))

(defun file-single-line-empty-one-character-no-newline-content ()
  (alexandria:read-file-into-string (file-single-line-empty-fname)))

(defun file-single-line-one-character-with-newline-fname ()
  (merge-pathnames
   "tests/example_texts/single_line_one_character_with_newline.txt"
   (asdf:system-source-directory :basic-editor/tests)))

(defun file-single-line-empty-one-character-with-newline-content ()
  (alexandria:read-file-into-string (file-single-line-one-character-with-newline-fname)))

;;; ================= fixtures =================================================
(def-fixture prepare-text (fpath)
  ;; setup code
  (let* (
         (d (load-file-and-model fpath))
         (model (getf d :model))
         (experimental-window (getf d :experimental-window))
         (loaded-text (sycamore:rope-string (be::text model))))
    ;; body
    (&body)
    ;; teardown code
    ))

(def-fixture prepare-text-no-window (text)
  ;; setup code
  (let* ((model (make-instance 'basic-editor-model))
         (text-content text))
    (setf (be::text model) text-content)
    (be::reload-text-structure model)

    ;; body
    (&body)
    ;; teardown code
    ))

;;; ============= suites ================================================
(progn                                  ; suites
  (def-suite equality
      :description "Suite to test if 2 and 2 is 4")

  (def-suite basic-editor-suite
      :description "Suite to hold other suites and tests")

  (def-suite basic-editor-resizing
      :description "Suite for resizing"
      :in basic-editor-suite)

  (def-suite basic-editor-cursor-validation
      :description "Suite for cursor validation"
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
  (let ((experimental-window (main :testing T)))
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
  (with-fixture prepare-text ((file-three-lines-fname))
    (let ((children (char-kids model)))

      (is (equal (type-of model) 'BE::BASIC-EDITOR-MODEL))
      (is (equal (subseq loaded-text 0 14) "I need to make"))

      (is (= 61 (length children)))
      (is (equal #\d (be::bchar (nth 5 children))))
      (is (equal #\Space (be::bchar (nth 6 children))))
      (is (equal #\t (be::bchar (nth 7 children))))
      (is (equal #\o (be::bchar (nth 8 children))))
      )))

(in-suite basic-editor-text)           ; ==================================

(in-suite basic-editor-text-first-line-left)           ; ==================================

(test single-line-moving-left
  "single line moving left"
  (with-fixture prepare-text ((file-single-line-fname))

    (let ((children (char-kids model)))
      (is (= 13 (length children))))

    (is (equal (type-of model) 'BE::BASIC-EDITOR-MODEL))
    (is (equal (subseq loaded-text 0 13) (format nil "Ala ma kota.~%")))

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

    (process-event experimental-window :key-pressed '("" "Left" 113 NIL))
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 0 (~> model be::cursor be::col)))


    (process-event experimental-window :key-pressed '("" "Left" 113 NIL))
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 0 (~> model be::cursor be::col)))
    ))

(test single-line-moving-left-pressing-enter
  "single line moving left"
  (with-fixture prepare-text ((file-single-line-fname))

    (let ((children (char-kids model)))
      (is (= 13 (length children))))

    (is (equal (type-of model) 'BE::BASIC-EDITOR-MODEL))
    (is (equal (subseq loaded-text 0 13) (format nil "Ala ma kota.~%")))

      ;; TODO finish the tests and response to moving cursor


    (snapshot experimental-window "loaded")
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 0 (~> model be::cursor be::col)))

    (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 1 (~> model be::cursor be::col)))

    (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
    (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
    (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
    (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 5 (~> model be::cursor be::col)))

    (process-event experimental-window :key-pressed '("" "Left" 113 NIL))
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 4 (~> model be::cursor be::col)))
    (is (equal (sycamore:rope-string (be::text model)) (format nil "Ala ma kota.~%")))

    (process-event experimental-window :key-pressed '("" "Return" 36 NIL))
    (is (eq 1 (~> model be::cursor be::row)))
    (is (eq 0 (~> model be::cursor be::col)))
    (is (equal
         (format nil "Ala ~%ma kota.~%")
         (sycamore:rope-string (be::text model))
         ))
      ))

(in-suite basic-editor-text-last-line-right)           ; ==================================

(test three-lines-moving-right
  "three lines moving lines"
  (with-fixture prepare-text ((file-three-lines-fname))
    (let ((children (char-kids model)))
      (is (= 61 (length children))))

    (is (equal (type-of model) 'BE::BASIC-EDITOR-MODEL))
    (is (equal (subseq loaded-text 0 20) (format nil
                                                 "I need to make sure~%")))

    ;; TODO finish the tests and response to moving cursor

    (snapshot experimental-window "loaded")
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 0 (~> model be::cursor be::col)))

    ;; move 2 rows down

    ;; (process-event experimental-window :key-pressed '("" "Down" 116 NIL))
    ;; (is (eq 1 (~> model be::cursor be::row)))
    ;; (is (eq 0 (~> model be::cursor be::col)))

    ;; (process-event experimental-window :key-pressed '("" "Down" 116 NIL))
    ;; (is (eq 2 (~> model be::cursor be::row)))
    ;; (is (eq 0 (~> model be::cursor be::col)))

    ;; ;; move to the right
    ;; (loop for x from 1 to 16
    ;;       do (process-event experimental-window :key-pressed '("" "Right" 114 NIL)))
    ;; (is (eq 2 (~> model be::cursor be::row)))
    ;; (is (eq 16 (~> model be::cursor be::col)))
    ;; (is (equal loaded-text  (format nil
    ;;                                 "I need to make sure~%three lines movements~%works as expected.~%")))

    ;; (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
    ;; (is (eq 2 (~> model be::cursor be::row)))
    ;; (is (eq 17 (~> model be::cursor be::col)))
    ;; (is (equal loaded-text  (format nil
    ;;                                 "I need to make sure~%three lines movements~%works as expected.~%")))

    ;; (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
    ;; (is (eq 3 (~> model be::cursor be::row)))
    ;; (is (eq 0 (~> model be::cursor be::col)))
    ;; (is (equal loaded-text  (format nil
    ;;                                 "I need to make sure~%three lines movements~%works as expected.~%")))

    ;; (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
    ;; (is (eq 3 (~> model be::cursor be::row)))
    ;; (is (eq 0 (~> model be::cursor be::col)))
    ;; (is (equal loaded-text  (format nil
    ;;                                 "I need to make sure~%three lines movements~%works as expected.~%")))

    ;; (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
    ;; (is (eq 3 (~> model be::cursor be::row)))
    ;; (is (eq 0 (~> model be::cursor be::col)))
    ;; (is (equal loaded-text  (format nil
    ;;                                 "I need to make sure~%three lines movements~%works as expected.~%")))

    ;; (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
    ;; (is (eq 3 (~> model be::cursor be::row)))
    ;; (is (eq 0 (~> model be::cursor be::col)))
    ;; (is (equal loaded-text  (format nil
    ;;                                 "I need to make sure~%three lines movements~%works as expected.~%")))

    ))

(test single-line-moving-right
  "single line moving right"
  (with-fixture prepare-text ((file-single-line-fname))
    (let ((children ;;(~> world boxes::children (nth 1 _) boxes::children)
            (char-kids model)))
      (is (= 13 (length children))))

    (is (equal (type-of model) 'BE::BASIC-EDITOR-MODEL))
    (is (equal loaded-text  (format nil "Ala ma kota.~%")))

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
    (is (equal (sycamore:rope-string (be::text model)) (format nil "Ala ma kota.~%")))

    (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 12 (~> model be::cursor be::col)))
    (is (equal (sycamore:rope-string (be::text model)) (format nil "Ala ma kota.~%")))

    (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
    (is (eq 1 (~> model be::cursor be::row)))
    (is (eq 0 (~> model be::cursor be::col)))
    (is (equal (sycamore:rope-string (be::text model)) (format nil "Ala ma kota.~%~%")))

    (process-event experimental-window :key-pressed '("" "Right" 114 NIL))
    (is (eq 2 (~> model be::cursor be::row)))
    (is (eq 0 (~> model be::cursor be::col)))
    (is (equal (sycamore:rope-string (be::text model)) (format nil "Ala ma kota.~%~%~%")))
    ))

(in-suite basic-editor-text-pressing-enter)           ; ==================================

(test single-line-moving-left2
  "single line moving left2"
  (with-fixture prepare-text ((file-single-line-fname))
    (let ((children (char-kids model)))
      (is (= 13 (length children))))

    (is (equal (type-of model) 'BE::BASIC-EDITOR-MODEL))
    (is (equal (subseq loaded-text 0 13) (format nil "Ala ma kota.~%")))

    ;; TODO finish the tests and response to moving cursor

    (snapshot experimental-window "loaded")
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 0 (~> model be::cursor be::col)))

    (process-event experimental-window :key-pressed '("" "End" 115 NIL))
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 12 (~> model be::cursor be::col)))

    (process-event experimental-window :key-pressed '("" "Home" 110 NIL))
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 0 (~> model be::cursor be::col)))))

(test single-line-moving-left3
  "single line moving left 3"
  (with-fixture prepare-text ((file-single-line-fname))
    (let ((children (char-kids model)))
      (is (= 13 (length children))))

    (is (equal (type-of model) 'BE::BASIC-EDITOR-MODEL))
    (is (equal loaded-text (format nil "Ala ma kota.~%")))

    ;; TODO finish the tests and response to moving cursor

    (snapshot experimental-window "loaded")
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 0 (~> model be::cursor be::col)))
    (is (eq 0 (be::find-cursor-position model)))

    (process-event experimental-window :key-pressed '("" "End" 115 NIL))
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 12 (~> model be::cursor be::col)))
    (is (eq 12 (be::find-cursor-position model)))

    (process-event experimental-window :key-pressed '("" "Left" 113 NIL))
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 11 (~> model be::cursor be::col)))

    (is (equal (sycamore:rope-string (be::text model)) (format nil "Ala ma kota.~%")))
    (loop for x from 1 to 4 do
      (process-event experimental-window :key-pressed '("" "Left" 113 NIL)))
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 7 (~> model be::cursor be::col)))
    (is (eq 7 (be::find-cursor-position model)))

    ;; ;; add test for character under cursor
    (process-event experimental-window :key-pressed '("" "Return" 36 NIL))
    (is (eq 1 (~> model be::cursor be::row)))
    (is (eq 0 (~> model be::cursor be::col)))
    (is (equal (sycamore:rope-string (be::text model)) (format nil "Ala ma ~%kota.~%")))
    (is (eq 8 (be::find-cursor-position model)))

    (process-event experimental-window :key-pressed '("" "Home" 110 NIL))
    (is (eq 1 (~> model be::cursor be::row)))
    (is (eq 0 (~> model be::cursor be::col)))
    ))


(in-suite basic-editor-text)           ; ==================================

(test single-line-empty
  "one character file with empty content"
  (with-fixture prepare-text ( (file-single-line-empty-fname))
    (is (equal loaded-text (format nil "")))

    (process-event experimental-window :key-pressed '("" "End" 115 NIL))
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 0 (~> model be::cursor be::col)))

    (process-event experimental-window :key-pressed '("" "Home" 110 NIL))
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 0 (~> model be::cursor be::col)))

    (process-event experimental-window :key-pressed '("a" "a" 38 NIL))
    ;; (is (eq 0 (~> model be::cursor be::row))) ; TODO fix me
    (is (eq 0 (~> model be::cursor be::col)))

    ;; (process-event experimental-window :key-pressed '("l" "l" 46 NIL))
    ;; (is (eq 0 (~> model be::cursor be::row)))
    ;; (is (eq 1 (~> model be::cursor be::col)))

    ;; (process-event experimental-window :key-pressed '("a" "a" 38 NIL))
    ;; (is (eq 0 (~> model be::cursor be::row)))
    ;; (is (eq 3 (~> model be::cursor be::col)))
                                        ;
    ;; (loop for k in '((" " "space" 65 NIL)
    ;;                  ("m" "m" 58 NIL)
    ;;                  ("a" "a" 38 NIL)
    ;;                  (" " "space" 65 NIL)
    ;;                  ("k" "k" 45 NIL)
    ;;                  ("o" "o" 32 NIL)
    ;;                  ("t" "t" 28 NIL)
    ;;                  ("a" "a" 38 NIL)
    ;;                  ("." "period" 60 NIL))
    ;;       do (process-event experimental-window :key-pressed  k))

    ;; (eq 0  (~> model be::cursor be::row))
    ;; (is (eq 12 (~> model be::cursor be::col)))
    ))

(test single-line-one-character-no-newline
      "one character file without NEWLINE"
      (with-fixture prepare-text ((file-single-line-one-character-no-newline-fname))
        (is (equal loaded-text (format nil "a")))

        (process-event experimental-window :key-pressed '("" "End" 115 NIL))
        (is (eq 0 (~> model be::cursor be::row)))
        (is (eq 0 (~> model be::cursor be::col)))

        (process-event experimental-window :key-pressed '("" "Home" 110 NIL))
        (is (eq 0 (~> model be::cursor be::row)))
        (is (eq 0 (~> model be::cursor be::col))))

      )

(test single-line-one-character-with-newline
  "one character file with NEWLINE"
  (with-fixture prepare-text ((file-single-line-one-character-with-newline-fname))
    (is (equal loaded-text (format nil "b~%")))

    (process-event experimental-window :key-pressed '("" "End" 115 NIL))
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 1 (~> model be::cursor be::col)))

    (process-event experimental-window :key-pressed '("" "Home" 110 NIL))
    (is (eq 0 (~> model be::cursor be::row)))
    (is (eq 0 (~> model be::cursor be::col)))))

(in-suite basic-editor-cursor-validation )        ; ==================================


(test single-line-one-character-with-newline
  "one character with NEWLINE"
  (with-fixture prepare-text-no-window ((format nil "A~%"))
    (is (equal (be::text model) (format nil "A~%")))

    (is (eq t    (be::validate-cursor-position model 0 0)))
    (is (eq nil  (be::validate-cursor-position model -1 -1)))
    (is (eq nil  (be::validate-cursor-position model 0 -1)))
    (is (eq nil  (be::validate-cursor-position model -1 0)))
    (is (eq t    (be::validate-cursor-position model 0 1)))
    (is (eq nil  (be::validate-cursor-position model 1000 0)))
    (is (eq nil  (be::validate-cursor-position model 0 1000)))
    (is (eq nil  (be::validate-cursor-position model 0 2)))
    (is (eq nil  (be::validate-cursor-position model 1 0)))))
