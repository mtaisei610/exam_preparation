;;; quiz-script.lisp
;;; Usage: sbcl --script quiz-script.lisp data.tsv

;;; --- ANSI Color Definitions ---
(defparameter *esc* (format nil "~c" #\Esc))
(defparameter *red* (format nil "~a[31m" *esc*))
(defparameter *green* (format nil "~a[32m" *esc*))
(defparameter *cyan* (format nil "~a[36m" *esc*))
(defparameter *reset* (format nil "~a[0m" *esc*))

;;; --- Helpers ---

(defun get-target-filename ()
  "Robustly gets the TSV filename from command line args."
  ;; in 'sbcl --script quiz.lisp data.tsv', *posix-argv* usually looks like:
  ;; (".../sbcl" "quiz.lisp" "data.tsv") OR ("/path/to/quiz.lisp" "data.tsv")
  ;; We look for the first argument that ends in .tsv or is not the script itself.
  (let ((args sb-ext:*posix-argv*))
    ;; We generally want the last argument provided
    (car (last args))))

(defun normalize-input (str)
  "Lowercases and removes punctuation."
  (if (null str) ""
      (let ((down (string-downcase str)))
        (string-trim '(#\Space #\Tab #\Return #\Newline)
                     (remove-if (lambda (c) (member c '(#\. #\,))) down)))))

(defun split-line (line)
  "Splits a line by Tab. Returns (Japanese English)."
  (let ((pos (position #\Tab line)))
    (if pos
        (list (subseq line 0 pos) 
              (string-trim '(#\Return #\Space) (subseq line (1+ pos))))
        ;; If no tab found, ignore line or return nil
        nil)))

(defun load-tsv (filename)
  (format t "Loading file: ~a~%" filename)
  (unless (probe-file filename)
    (format t "~aError: File not found: ~a~a~%" *red* filename *reset*)
    (sb-ext:exit :code 1))
    
  (with-open-file (in filename :direction :input)
    (loop for line = (read-line in nil)
          while line
          for parts = (split-line line)
          when (and parts (> (length (second parts)) 0))
          collect parts)))

(defun shuffle (list)
  (let* ((vec (coerce list 'vector))
         (len (length vec)))
    (loop for i from (1- len) downto 1 do
          (rotatef (aref vec i) (aref vec (random (1+ i)))))
    (coerce vec 'list)))

;;; --- Main ---

(defun main ()
  (setf *random-state* (make-random-state t))
  
  (let ((filename (get-target-filename)))
    ;; Sanity check: if filename looks like the script name, user forgot the arg
    (when (or (search "quiz-script.lisp" filename) 
              (null filename))
      (format t "~aError: Please provide a TSV file.~a~%" *red* *reset*)
      (format t "Usage: sbcl --script quiz-script.lisp data.tsv~%")
      (sb-ext:exit :code 1))

    (let ((questions (load-tsv filename)))
      (when (null questions)
        (format t "~aError: No valid questions found in ~a (check for Tabs)~a~%" *red* filename *reset*)
        (sb-ext:exit :code 1))

      (format t "~%~a>>> Starting Quiz with ~D questions <<<~a~%" *cyan* (length questions) *reset*)

      (dolist (q (shuffle questions))
        (let ((jp (first q))
              (en (second q)))
          
          ;; Loop until correct
          (loop
            (format t "~%-----------------------------------~%")
            (format t "[Japanese]: ~a~%" jp)
            (format t "~a[English] > ~a" *cyan* *reset*)
            (finish-output)
            
            (let ((ans (read-line)))
              (if (string= (normalize-input ans) (normalize-input en))
                  (progn
                    (format t "~aCorrect! (~a)~a~%" *green* en *reset*)
                    (return)) ;; Break loop
                  (progn
                    (format t "~aIncorrect...~a~%" *red* *reset*)
                    (format t "Correct answer: ~a~a~a~%" *green* en *reset*)
                    (format t "Type it exactly to continue:~%")))))))
      
      (format t "~%~aAll questions finished! Great job!~a~%" *green* *reset*))))

;; Run main immediately
(main)
