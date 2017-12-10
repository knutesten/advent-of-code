(defvar test-input '( 3 4 1 5))
(defvar test-circ-list-length 5) 

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

(defun weak-encrypt (input circ-list-length)
  (let ((lst (range circ-list-length))
        (index 0)
        (skip-size 0))
    (loop for len in input
          do 
          (setf lst (reverse-sublist lst index len))
          (setf index (+ index len skip-size))
          (setf skip-size (1+ skip-size)))
    lst))

(defun split-by-comma (string)
  (loop for i = 0 then (1+ j)
        as j = (position #\, string :start i)
        collect (subseq string i j)
        while j))

(defun read-input-part1 (file)
  (loop for len-str in (split-by-comma (read-line (open file) nil))
        collect (parse-integer len-str)))

(defvar input-part1 (read-input-part1 "./input.txt"))
(defvar circ-list-length 256)

(defvar result (weak-encrypt input-part1 circ-list-length))
(print (* (first result) (second result)))
(terpri)

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


(defun knot-hash (input circ-list-length)
  (let ((hash-lst (knot-hashing input circ-list-length)))
    (string-downcase 
      (format nil "~{~A~}" 
              (loop for i from 0 to 255 by 16
                    collect 
                    (format nil "~2,'0d"
                            (write-to-string 
                              (apply 'logxor (subseq hash-lst i (+ i 16))) 
                              :base 16)))))))

(defun read-input-part2 (file)
  (append
    (loop for len-str across (read-line (open file) nil) collect (char-code len-str))
    '(17 31 73 47 23)))



(defvar input-part2 (read-input-part2 "./input.txt"))

(print (knot-hash input-part2 circ-list-length))

