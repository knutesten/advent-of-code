(defun parse-line (line)
  (let ((col-pos (position #\: line)))
    (list 
      (parse-integer (subseq line 0 col-pos))
      (parse-integer (subseq line (1+ col-pos) (length line))))))

(defvar input (with-open-file (in "./input.txt")
  (loop for line = (read-line in nil)
        while line
        collect (parse-line line))))

(defvar test_input '((0 3) (1 2) (4 4) (6 4)))

(defun get-scanner-pos (time size)
  (let ((len (1- size)))
    (let ((round-length (* 2 len)))
      (let ((pos (mod time round-length)))
        (if (<= pos len)
          pos
          (- len (mod pos len)))))))

(defun calc-severity (wall delay)
  (let ((severity 0))
    (loop for layer in wall do
          (when (= (get-scanner-pos (+ (first layer) delay) (second layer)) 0)
            (setf severity (+ severity (* (first layer) (second layer))))))
    severity))

(print (calc-severity input 0))

(defun is-caught (wall delay)
  (loop for layer in wall do
        (when (= (get-scanner-pos (+ (first layer) delay) (second layer)) 0)
          (return t))))

(defun find-delay (wall)
  (let ((delay 0))
    (loop while t do
          (when (not (is-caught wall delay)) (return delay))
          (setf delay (1+ delay)))))

(terpri)
(print (find-delay input))

