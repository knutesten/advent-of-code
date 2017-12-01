(defun convert-string-to-digit-list (str)
  (map 'list #'(lambda (x) (- (char-code x) 48)) str))

(defvar digit-list (convert-string-to-digit-list (read-line (open "./input.txt") nil)))

(defun add-if-equal (x y sum)
  (if (= x y) (+ sum x) sum))

(defun captcha-calc (l sum frst)
  (if (= (length l) 1) 
    (add-if-equal (first l) frst sum)
    (captcha-calc (rest l) (add-if-equal (first l) (second l) sum) frst)))

(defun captcha (l)
  (captcha-calc l 0 (first l)))

(write-line "Solution for captcha:")
(write (captcha digit-list))

(terpri)
(terpri)

(defun captcha2-calc (lst sum pos)
  (if (= pos (length lst))
    sum
    (captcha2-calc 
      lst
      (if (= (nth pos lst) (nth (mod (+ (/ (length lst) 2) pos) (length lst)) lst))
        (+ sum (nth pos lst))
        sum)
      (+ pos 1))))

(defun captcha2 (lst) (captcha2-calc lst 0 0))

(write-line "Solution for captcha2:")
(write (captcha2 digit-list))
