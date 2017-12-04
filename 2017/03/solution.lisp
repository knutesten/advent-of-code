(defun calc-radius (squares)
  (ceiling (/ (- (sqrt squares) 1) 2)))

(defun dist-outer-circ (squares)
  (- squares (expt (+ (* 2 (- (calc-radius squares)) 1) 1) 2)))

(defun solution (squares)
  (let ((dst (dist-outer-circ squares))
        (side (* 2 (calc-radius squares))))
    (let ((md (mod dst side)))
      (if (< md (/ side 2))
        (- side md)
        md ))))

(defvar input 368078)

(write-line "Number of steps: ")
(write (solution input))

;(setf (gethash '(0 0) mp) 3824)
;(print (gethash '(0 1) mp))

(defun zero-if-nil (x) (if x x 0))

(defun get-value (coor mp)
  (+ 
    (zero-if-nil (gethash (mapcar '+ coor '(0 1)) mp))
    (zero-if-nil (gethash (mapcar '+ coor '(0 -1)) mp))
    (zero-if-nil (gethash (mapcar '+ coor '(1 0)) mp))
    (zero-if-nil (gethash (mapcar '+ coor '(1 1)) mp))
    (zero-if-nil (gethash (mapcar '+ coor '(1 -1)) mp))
    (zero-if-nil (gethash (mapcar '+ coor '(-1 0)) mp))
    (zero-if-nil (gethash (mapcar '+ coor '(-1 1)) mp))
    (zero-if-nil (gethash (mapcar '+ coor '(-1 -1)) mp))))

(defun should-turn (nr)
  (let ((dst (dist-outer-circ nr))
        (rad (/ (- (sqrt nr) 1) 2))
        (rad-1 (/ (- (sqrt (- nr 1)) 1) 2))
        (side (* 2 (calc-radius nr))))
    (cond
    ((= (ceiling rad) rad) nil)
    ((= (ceiling rad-1) rad-1) t)
    (t (= (mod dst side) 0)))))



;37 36 35 34 33 32 31
;38 17 16 15 14 13 30
;39 18  5  4  3 12 29
;40 19  6  1  2 11 28
;41 20  7  8  9 10 27
;42 21 22 23 24 25 26
;43 44 45 46 47 48 49 50

(defun solution (goal)
  (let ((directions '((1 0) (0 1) (-1 0) (0 -1)))
        (mp (make-hash-table :test 'equal))
        (dir 3)
        (nr 1)
        (value 1)
        (coor '(0 0)))

    (setf (gethash '(0 0) mp) 1)
    ;(terpri)
    ;(format t "~d ~d ~d" nr value coor)

    (loop
      (setf coor (mapcar '+ coor (nth dir directions)))
      (setf value (get-value coor mp))
      (setf (gethash coor mp) value)
      (setf nr (+ 1 nr))

      (when (should-turn nr) 
        (setf dir (mod (+ 1 dir) 4)))

      ;(terpri)
      ;(format t "~d ~d ~d" nr value coor)
      (when (> value goal) (return value)))
    ))

(terpri)
(terpri)
(write-line "First value larger than input: ")
(write (solution input))



