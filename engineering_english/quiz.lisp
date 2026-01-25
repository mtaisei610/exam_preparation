;;; quiz-enhanced.lisp
;;; Usage: sbcl --script quiz-enhanced.lisp data.tsv

;;; --- 色の定義 (ANSI Escape Code) ---
(defparameter *esc* (format nil "~c" #\Esc))
(defparameter *reset* (format nil "~a[0m" *esc*))
(defparameter *red* (format nil "~a[31m" *esc*))     ; 不正解用
(defparameter *green* (format nil "~a[32m" *esc*))   ; 完全正解用
(defparameter *yellow* (format nil "~a[33m" *esc*))  ; おしい！用
(defparameter *cyan* (format nil "~a[36m" *esc*))    ; 質問文用

;;; --- ユーティリティ関数 ---

(defun get-target-filename ()
  "コマンドライン引数の最後の要素を取得します"
  (car (last sb-ext:*posix-argv*)))

(defun normalize-input (str)
  "小文字化し、記号(. ,)を除去します"
  (if (null str) ""
      (let ((down (string-downcase str)))
        (string-trim '(#\Space #\Tab #\Return #\Newline)
                     (remove-if (lambda (c) (member c '(#\. #\,))) down)))))

(defun remove-trailing-s (str)
  "文字列の末尾が 's' であれば除去します (単複判定用)"
  (let ((len (length str)))
    (if (and (> len 0) (char= (char str (1- len)) #\s))
        (subseq str 0 (1- len))
        str)))

(defun split-line (line)
  "タブ区切りで分割します"
  (let ((pos (position #\Tab line)))
    (if pos
        (list (subseq line 0 pos) 
              (string-trim '(#\Return #\Space) (subseq line (1+ pos))))
        nil)))

(defun save-to-marks (jp en)
  "marks.tsv に問題を追加します"
  (with-open-file (out "marks.tsv" 
                       :direction :output 
                       :if-exists :append 
                       :if-does-not-exist :create)
    (format out "~a~t~a~%" jp en))
  (format t "~a[System] Added to marks.tsv~a~%" *yellow* *reset*))

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

;;; --- メイン処理 ---

(defun main ()
  (setf *random-state* (make-random-state t))
  
  (let ((filename (get-target-filename)))
    ;; 引数チェック
    (when (or (search "quiz-enhanced.lisp" filename) (null filename))
      (format t "~aUsage: sbcl --script quiz-enhanced.lisp <tsv-file>~a~%" *red* *reset*)
      (sb-ext:exit :code 1))

    (let ((questions (load-tsv filename)))
      (when (null questions)
        (format t "~aError: No valid questions found.~a~%" *red* *reset*)
        (sb-ext:exit :code 1))

      (format t "~%~a>>> Quiz Started! (~D questions) <<<~a~%" *cyan* (length questions) *reset*)
      (format t "Commands: ~a\\skip~a to skip, ~a\\mark~a to save to list.~%" *yellow* *reset* *yellow* *reset*)

      ;; 問題ループ
      (dolist (q (shuffle questions))
        (let ((jp (first q))
              (en (second q)))
          
          ;; 解答ループ (正解かスキップするまで繰り返す)
          (loop
            (format t "~%-----------------------------------~%")
            (format t "[Japanese]: ~a~%" jp)
            (format t "~a[English] > ~a" *cyan* *reset*)
            (finish-output)
            
            (let* ((raw-input (read-line))
                   (norm-input (normalize-input raw-input))
                   (norm-ans (normalize-input en)))

              (cond
                ;; --- Command: Skip ---
                ((string= raw-input "\\skip")
                 (format t "~aSkipped.~a (Answer was: ~a)~%" *yellow* *reset* en)
                 (return)) ;; inner loopを抜ける

                ;; --- Command: Mark ---
                ((string= raw-input "\\mark")
                 (save-to-marks jp en)
                 ;; ループは抜けない（まだ正解していないため）
                 )

                ;; --- Case 1: 完全一致 ---
                ((string= norm-input norm-ans)
                 (format t "~aCorrect! (~a)~a~%" *green* en *reset*)
                 (return))

                ;; --- Case 2: 単数・複数の違いのみ (sの有無) ---
                ((string= (remove-trailing-s norm-input) 
                          (remove-trailing-s norm-ans))
                 ;; 正解扱いでループを抜けるが、注意を表示
                 (format t "~aCorrect! (singular/plural ignored)~a~%" *yellow* *reset*)
                 (format t "Strict answer: ~a~a~a~%" *green* en *reset*)
                 (return))

                ;; --- Case 3: 不正解 ---
                (t
                 (format t "~aIncorrect...~a~%" *red* *reset*)
                 (format t "Correct answer: ~a~a~a~%" *green* en *reset*)
                 (format t "Type it again to memorize it.~%"))))))) ;; ここを修正しました (括弧を6つに調整)
      
      (format t "~%~aAll questions finished! Good job!~a~%" *green* *reset*))))

(main)
