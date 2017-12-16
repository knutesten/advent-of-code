(defun spin (line nr)
  (let ((nr (parse-integer nr)))
    (append
      (subseq line (- (length line) nr))
      (subseq line 0 (- (length line) nr)))))

(defun exchange (line a b)
  (let ((tmp nil)
        (line (copy-list line))
        (a (parse-integer a))
        (b (parse-integer b)))
    (setq tmp (nth a line))
    (setf (nth a line) (nth b line))
    (setf (nth b line) tmp)
    line))

(defun partner (line a b) (exchange line (write-to-string (position a line :test 'equal)) (write-to-string (position b line :test 'equal))))

(defun dance (line steps)
  (let ((line (copy-list line)))
    (loop for step in steps do
          (cond
            ((equal "s" (first step)) (setq line (spin line (second step))))
            ((equal "x" (first step)) (setq line (exchange line (second step) (third step))))
            (t (setq line (partner line (second step) (third step))))))
    line
    ))

(defun split (string divider)
  (loop for i = 0 then (1+ j)
        as j = (position divider string :start i)
        collect (subseq string i j)
        while j))

(defvar steps 
  (mapcar
    #'(lambda (x) 
        (append 
          (list (subseq x 0 1))
          (split (subseq x 1) #\/)
          ))
    (split (read-line (open "./input.txt") nil) #\,)))

(defvar line '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"))

(print (apply 'concatenate 'string (dance line steps)))
(terpri)

(defun one-billion-dance (initial-state steps)
  (let ((line (copy-list initial-state))
        (count 0))
    (loop while t do
          (setq line (dance line steps))
          (setq count (1+ count))
          (when (equal line initial-state) (return count)))

    (setq line initial-state)
    (loop for i from 0 below (mod 1000000000 count) do
          (setq line (dance line steps)))

    (apply 'concatenate 'string line)))


(print (one-billion-dance line steps))
(terpri)

