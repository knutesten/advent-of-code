(defun calc-next-value (factor prev-value) (mod (* factor prev-value) 2147483647))

(defun pad-with-zero (str len)
  (let ((zeroes (- len (length str))))
    (if (> zeroes 0)
      (concatenate 'string (make-string zeroes :initial-element #\0) str)
      str)))

(defun last-16-bits (num)
  (let ((bit-str (pad-with-zero (write-to-string num :base 2) 16)))
    (subseq bit-str (- (length bit-str) 16) (length bit-str))))

(defun last-16-bits-match (a b)
  (equal (last-16-bits a) (last-16-bits b)))

(defun nr-of-equal-pairs (start_a start_b)
  (let ((prev-value-a start_a)
        (prev-value-b start_b)
        (i 0)
        (count 0))
    (loop while (< i 40000000) do
          (setq prev-value-a (calc-next-value 16807 prev-value-a))
          (setq prev-value-b (calc-next-value 48271 prev-value-b))
          (when (last-16-bits-match prev-value-a prev-value-b)
            (setf count (1+ count)))
          (setq i (1+ i)))
    count))

(print (nr-of-equal-pairs 883 879))
(terpri)


(defun nr-of-equal-pairs-part-2 (start-a start-b)
  (let ((prev-value-a start-a)
        (prev-value-b start-b)
        (count 0))
    (loop for i from 0 below 5000000 do
          (loop while t do 
                (setq prev-value-a (calc-next-value 16807 prev-value-a))
                (when (= 0 (mod prev-value-a 4)) (return)))

          (loop while t do
                (setq prev-value-b (calc-next-value 48271 prev-value-b))
                (when (= 0 (mod prev-value-b 8)) (return)))

          (when (last-16-bits-match prev-value-a prev-value-b)
            (setf count (1+ count)))
    count))

(print (nr-of-equal-pairs-part-2 883 879))
(terpri)

