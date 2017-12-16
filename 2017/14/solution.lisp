(defun range (len) (loop for i from 0 below len collect i))

(defun reverse-sublist (lst start len)
  (let ((result (copy-list lst))
        (cnt 0))
    (loop for i from start to (+ start (1- len))
          do 
          (setf 
            (nth (mod i (length result)) result) 
            (nth (mod (- (+ start (1- len)) cnt) (length lst)) lst))
          (setf cnt (1+ cnt)))
    result))

(defun knot-hashing (input circ-list-length)
  (let ((lst (range circ-list-length))
        (index 0)
        (skip-size 0))
    (loop for i from 1 to 64 do
          (loop for len in input
                do 
                (setf lst (reverse-sublist lst index len))
                (setf index (+ index len skip-size))
                (setf skip-size (1+ skip-size))))
    lst))

(defun to-char-array (string)
  (append
    (loop for char across string collect (char-code char))
    '(17 31 73 47 23)))

(defun knot-hash (input circ-list-length)
  (let ((hash-lst (knot-hashing (to-char-array input) circ-list-length)))
    (format nil "~{~A~}" 
            (loop for i from 0 below circ-list-length by 16
                  collect 
                  (format nil "~8,'0d"
                          (write-to-string 
                            (apply 'logxor (subseq hash-lst i (+ i 16))) 
                            :base 2))))))

(defun count-ones (string)
  (let ((sum 0))
    (loop for char across string
          do (when (char= #\1 char) (setf sum (1+ sum))))
    sum
    ))

(defvar disk (loop for i from 0 below 128
                       collect (knot-hash (format nil "~{~A~}" (list "oundnydw-" i)) 256)))

(defun used-squares (disk) (apply '+ (mapcar #'count-ones disk)))

(print (used-squares disk))
(terpri)

(defun coor (row col) (format nil "~d ~d" row col)) 

(defun find-neighbour (row col disk map)
  (let ((directions '((1 0) (0 1) (-1 0) (0 -1))))
    (loop for dir in directions
          if (let ((new-row (+ row (first dir)))
                   (new-col (+ col (second dir))))
               (and 
                 (>= new-col 0)
                 (>= new-row 0)
                 (< new-col (length (first disk)))
                 (< new-row (length disk))
                 (not (gethash (coor new-row new-col) map))
                 (char= #\1 (char (nth new-row disk) new-col))
                 ))
          collect (list (+ row (first dir)) (+ col (second dir))))))

(defun find-group (row col disk map)
  (let ((open-list (list (list row col)))
        (new-coor nil))
    (loop while (> (length open-list) 0) do
          (setf new-coor (first open-list))
          (setf open-list (rest open-list))
          (setf (gethash (coor (first new-coor) (second new-coor)) map) t)
          (loop for coor in (find-neighbour (first new-coor) (second new-coor) disk map) do
                (setf open-list (adjoin coor open-list :test 'equal)))
          )))

(defun nr-of-groups (disk)
  (let ((group-map (make-hash-table :test 'equal))
        (group-count 0))
    (loop for row from 0 below (length disk) do 
          (loop for col from 0 below (length (nth row disk)) do
                (when (char= #\1 (char (nth row disk) col))
                  (when (not (gethash (coor row col) group-map))
                    (find-group row col disk group-map)
                    (setf group-count (1+ group-count))
                    ))))
    group-count
    ))

(print (nr-of-groups disk))
