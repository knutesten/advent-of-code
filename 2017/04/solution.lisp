(defun split-by-space (string)
  (loop for i = 0 then (1+ j)
        as j = (position #\space string :start i)
        collect (subseq string i j)
        while j))

(defun read-input (strm lst)
  (let ((line (read-line strm nil)))
    (if line
      (read-input strm (cons line lst))
      lst)))

(defun has-duplicates (lst prev)
  (cond 
    ((not lst) nil)
    ((string= (first lst) prev) t)
    (t (has-duplicates (rest lst) (first lst)))))

(defun check-pwd (pwd)
  (let ((pwd-list (sort (split-by-space pwd) 'string<)))
    (not (has-duplicates (rest pwd-list) (first pwd-list)))))

(defun nr-of-valid-pwds (pwds)
  (reduce #'+ 
          (mapcar #'(lambda (pwd) (if (check-pwd pwd) 1 0)) pwds)))


(defvar input (read-input (open "./input.txt") nil))

(write-line "Number of valid passwords:")
(write (nr-of-valid-pwds input))


(defun check-pwd2 (pwd)
  (let ((pwd-list (sort 
                    (mapcar 
                      #'(lambda (str) (sort (copy-seq str) 'char<)) 
                      (split-by-space pwd))
                    'string<)))
    (not (has-duplicates (rest pwd-list) (first pwd-list)))))

(defun nr-of-valid-pwds2 (pwds)
  (reduce #'+ 
          (mapcar #'(lambda (pwd) (if (check-pwd2 pwd) 1 0)) pwds)))

(terpri)
(terpri)
(write-line "Number of valid passwords (second policy):")
(write (nr-of-valid-pwds2 input))

