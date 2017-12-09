(defun split-by-tab (string)
  (loop for i = 0 then (1+ j)
        as j = (position #\tab string :start i)
        collect (subseq string i j)
        while j))

(defun read-input (file)
  (mapcar 'parse-integer (split-by-tab (read-line (open file) nil))))

(defvar input (read-input "./input.txt"))
;(defvar input '(0 2 7 0))

(defun redistribute (lst)
  (let ((lst (copy-list lst))
        (states nil)
        (state nil)
        (steps 0)
        (mx nil)
        (pos nil))
    (loop
      (setf state (format nil "~s" lst))
      (setf steps (1+ steps))
      (setf mx (reduce #'max lst))
      (setf pos (position mx lst))
      (setf (nth pos lst) 0)
      (loop 
        (setf pos (mod (1+ pos) (length lst)))
        (setf (nth pos lst) (1+ (nth pos lst)))
        (setf mx (1- mx))
        (when (= mx 0) (return)))
      (if (position state states :test #'equal)
        (return (list (1- steps) (- steps (- (length states) (position state states :test #'equal)))))
        (setf states (cons state states))))))

(defvar result (redistribute input))
(write-line "Number of steps (part 1):")
(write (first result))
(terpri)
(terpri)
(write-line "Number of cycles between (part 2):")
(write (nth 1 result))
