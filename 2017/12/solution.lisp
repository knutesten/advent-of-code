(defun parse-line (line)
  (list 
    (parse-integer (subseq line 0 (position #\< line)))
    (let ((str (subseq line (1+ (position #\> line)) (length line))))
      (loop for i = 0 then (1+ j)
            as j = (position #\, str :start i)
            collect (parse-integer (subseq str i j))
            while j))))

(defvar input (with-open-file (in "./input.txt")
                (loop for line = (read-line in nil)
                      while line
                      collect (parse-line line))))

(defun in-group (prgm-list frst)
  (let ((prgm nil)
        (group nil)
        (open-list (list frst)))

    (loop while (> (length open-list) 0)
          do 
          (setf prgm (first open-list))
          (setf open-list (rest open-list))
          (when (not (position (first prgm) group))
            (setf group (cons (first prgm) group))
            (setf open-list (append 
                              open-list 
                              (mapcar #'(lambda (x) (nth x prgm-list)) (second prgm))))))
    group))

(print  (length (in-group input (first input))))
(terpri)

(defun find-groups (prgm-list)
  (let ((groups 0)
        (group nil)
        (done nil))
    (loop for prgm in prgm-list
          do
          (when (not (position (first prgm) done))
            (setf group (in-group prgm-list prgm))
            (setf done (append done group))
            (setf groups (1+ groups))))
    groups))

(print (find-groups input))

