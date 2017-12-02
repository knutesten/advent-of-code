(defun split-by-tab (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\tab string :start i)
          collect (subseq string i j)
          while j))

(defun splt (str) 
  (map 'list #'(lambda (x) (parse-integer x)) (split-by-tab str)))

(defun read-input (strm lst)
  (let ((line (read-line strm nil)))
    (if line
      (read-input strm (cons (splt line) lst))
      lst)))

(defvar input (read-input (open "./input.txt") nil))

(defun lg-sml-diff (lst lg sml)
  (cond 
    ((not (first lst)) (- lg sml))
    ((< (first lst) sml) (lg-sml-diff (rest lst) lg (first lst)))
    ((> (first lst) lg) (lg-sml-diff (rest lst) (first lst) sml))
    (t (lg-sml-diff (rest lst) lg sml))))

(defun lg-sml-diff-sum-calc (lst sum)
  (if (first lst)
    (lg-sml-diff-sum-calc 
      (rest lst) 
      (+ sum (lg-sml-diff (first lst) (first (first lst)) (first (first lst)))))
    sum))

(defun lg-sml-diff-sum (lst) (lg-sml-diff-sum-calc lst 0))

(write-line "Checksum1: ")
(write (lg-sml-diff-sum input))
(terpri)
(terpri)

(defun devisable (lst divisors dividend)
  (if (not (first divisors))
    (devisable (rest lst) (rest (rest lst)) (first lst))
    (if (= (mod dividend (first divisors)) 0)
      (/ dividend (first divisors))
      (devisable lst (rest divisors) dividend))))

(defun devisable-sum-calc (lst sum)
  (if (not (first lst))
    sum
    (let ((srt-lst (sort (first lst) '>)))
      (devisable-sum-calc
        (rest lst)
        (+ 
          (devisable (rest srt-lst) (rest (rest srt-lst)) (first srt-lst))
          sum)))))

(defun devisable-sum (lst) (devisable-sum-calc lst 0))

(write-line "Checksum2:")
(write (devisable-sum input))
