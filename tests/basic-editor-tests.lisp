(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:basic-editor-test)

;; (ql:quickload :basic-editor/tests)
;; (in-package #:basic-editor-test)
;; (run! 'basic-editor-suite)

(defun test-all ()
  "Compile and run all test in one command."
  (ql:quickload :basic-editor/tests)
  (fiveam:run!  'basic-editor-test:basic-editor-suite))

(defun char-kids (model)
  (serapeum:~>  model
                basic-editor::world
                boxes::children
                (nth 1 _)
                boxes::children))

(def-suite basic-editor-suite
  :description "Suite to hold other suites and tests")

(in-suite basic-editor-suite)

(test test-equality
  "test some equalities"
  (is (= 2 2))
  (let ((expected 3)
        (got 3))
    (is (= expected got )))
  (is (= 4 (* 2 2))))

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

(test first-scrolling
  "test resizing"
  (let ((experimental-window (main :testing T))
        (model *basic-editor-model*))
    (process-event experimental-window :resize '(710 420))
    (is (= 710 (width experimental-window)))
    (is (= 420 (height experimental-window)))
    (is (= 0 (view-port-first-line model)))
    (is (= 0 (view-port-first-column model)))
    (is (eq 'boxes::node-down  (type-of (be::world model))))

    (is (eq #\b (basic-editor::bchar (nth 5 (char-kids model)))))
    (is (= 5 (be::col (nth 5 (char-kids model)))))
    (is (= 0 (be::row (nth 5 (char-kids model)))))

    (process-event experimental-window  :key-pressed '("n" "n" 57 NIL))
    (is (= 1 (view-port-first-line model)))
    (is (= 0 (view-port-first-column model)))
    (is (= 1 (be::row (nth 5 (char-kids model)))))

    (process-event experimental-window  :key-pressed '("n" "n" 57 NIL))
    (is (= 2 (view-port-first-line model)))
    (is (= 0 (view-port-first-column model)))
    (is (= 2 (be::row (nth 5 (char-kids model)))))

    (process-event experimental-window  :key-pressed '("p" "p" 33 NIL))
    (is (= 1 (view-port-first-line model)))
    (is (= 0 (view-port-first-column model)))
    (is (= 1 (be::row (nth 5 (char-kids model)))))

    (process-event experimental-window  :key-pressed '("f" "f" 41 NIL))
    (is (= 1 (view-port-first-line model)))
    (is (= 1 (view-port-first-column model)))
    (is (= 6 (be::col (nth 5 (char-kids model)))))
    (is (= 1 (be::row (nth 5 (char-kids model)))))

    (process-event experimental-window  :key-pressed '("f" "f" 41 NIL))
    (is (= 1 (view-port-first-line model)))
    (is (= 2 (view-port-first-column model)))
    (process-event experimental-window  :key-pressed '("b" "b" 56 NIL))
    (is (= 1 (view-port-first-line model)))
    (is (= 1 (view-port-first-column model)))

    (process-event experimental-window  :key-pressed '("p" "p" 33 NIL))
    (process-event experimental-window  :key-pressed '("b" "b" 56 NIL))
    (is (= 5 (be::col (nth 5 (char-kids model)))))
    (is (= 0 (be::row (nth 5 (char-kids model)))))

    ;;(break "check the model ~S" *basic-editor-model*)
    ))

(test char-children
  "children for the characters"
  (let ((experimental-window (main :testing T))
        (world (boxes::make-node-down
                0 0 600 400 "#cccccc88"))
        (model *basic-editor-model*))


    (process-event experimental-window :resize '(710 420))
    (is (= 710 (width experimental-window)))
    (is (= 420 (height experimental-window)))

    (be::new-file)
    (be::open-file (cons :selected "file:///home/jacek/.bashrc"))
    (basic-editor::adding-children world)


    (let* ((children (~> world boxes::children (nth 1 _) boxes::children))
           (loaded-text (sycamore:rope-string (be::text model))))

      (is (equal (type-of model) 'BE::BASIC-EDITOR-MODEL))
      (is (equal (subseq loaded-text 0 11) "# ~/.bashrc"))

      (is (= 420 (length children)))
      (is (equal #\b (be::bchar (nth 5 children))))
      (is (equal #\a (be::bchar (nth 6 children))))
      (is (equal #\s (be::bchar (nth 7 children))))
      (is (equal #\h (be::bchar (nth 8 children)))))))
