(defvar directions (make-hash-table :test 'equal))

(setf (gethash "n" directions) '(1 0 -1))
(setf (gethash "ne" directions) '(1 -1 0))
(setf (gethash "se" directions) '(0 -1 1))
(setf (gethash "s" directions) '(-1 0 1))
(setf (gethash "sw" directions) '(-1 1 0))
(setf (gethash "nw" directions) '(0 1 -1))

(defun calculate-distance (x y)
  (let ((x0 (first x))
        (y0 (first y))
        (x1 (second x))
        (y1 (second y))
        (x2 (third x))
        (y2 (third y)))
    (/ (+ (abs (- x0 y0)) (abs (- x1 y1)) (abs (- x2 y2))) 2)))

(defun find-end-coordinate (path)
  (let ((current-coor '(0 0 0))
        (dir nil)
        (dist nil)
        (max-dist 0))
    (loop for stp in path
          do 
          (setf dir (gethash stp directions))
          (setf current-coor
            (list
              (+ (first current-coor) (first dir))
              (+ (second current-coor) (second dir))
              (+ (third current-coor) (third dir))
              ))
          (setf dist (calculate-distance '(0 0 0) current-coor))
          (when (> dist max-dist)
            (setf max-dist dist)))
    (list current-coor max-dist)))

(defun split-by-comma (string)
  (loop for i = 0 then (1+ j)
        as j = (position #\, string :start i)
        collect (subseq string i j)
        while j))

(defvar path (split-by-comma (read-line (open "./input.txt") nil)))

(defvar result (find-end-coordinate path))
(print (calculate-distance '(0 0 0) (first result)))
(terpri)
(print (second result))


