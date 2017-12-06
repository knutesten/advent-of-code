(defun read-input (file)
   (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-integer line))))

(defun escape1 (lst)
  (let ((lst (copy-list lst))
        (index 0)
        (steps 0)
        (new-index 0))
    (loop 
      (setf new-index (+ (nth index lst) index))
      (setf (nth index lst) (1+ (nth index lst)))
      (setf index new-index)
      (setf steps (1+ steps))
      (when (or (< index 0) (>= index (length lst)))
        (return steps)))))
      
(defvar input (read-input "./input.txt"))

(write-line "Number of steps part 1:")
(write (escape1 input))
(terpri)
(terpri)

(defun escape2 (lst)
  (let ((lst (copy-list lst))
        (index 0)
        (steps 0)
        (new-index 0))
    (loop 
      (setf new-index (+ (nth index lst) index))
      (if (> (nth index lst) 2)
        (setf (nth index lst) (1- (nth index lst)))
        (setf (nth index lst) (1+ (nth index lst))))
      (setf index new-index)
      (setf steps (1+ steps))
      (when (or (< index 0) (>= index (length lst)))
        (return steps)))))

(write-line "Number of steps part 2:")
(write (escape2 input))
