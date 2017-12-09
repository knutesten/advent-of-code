;(defvar raw-input '( "pbga (66)"
;"xhth (57)"
;"ebii (61)"
;"havc (66)"
;"ktlj (57)"
;"fwft (72) -> ktlj, cntj, xhth"
;"qoyq (66)"
;"padx (45) -> pbga, havc, qoyq"
;"tknk (41) -> ugml, padx, fwft"
;"jptl (61)"
;"ugml (68) -> gyxo, ebii, jptl"
;"gyxo (61)"
;"cntj (57)"))

(defvar raw-input 
  (with-open-file (stream "./input.txt")
    (loop for line = (read-line stream nil)
          while line
          collect line)))


(defun split (string divider)
  (loop for i = 0 then (1+ j)
        as j = (position divider string :start i)
        collect (subseq string i j)
        while j))

(defun parse-input-row (row)
  (let ((lst (split row #\space)))
    (list 
      (first lst)
      (parse-integer (subseq (second lst) 1 (1- (length (second lst)))))
      (mapcar 
        #'(lambda (x) (if (string= (subseq x (1- (length x)) (length x)) ",") 
                        (subseq x 0 (1- (length x))) 
                        x )) 
        (when (> (length lst) 2) 
          (subseq lst 3 (length lst)))))))


(defun build-hash (input)
  (let ((hashtbl (make-hash-table :test 'equal)))
    (loop for row in input 
          do (when (nth 2 row)
               (loop for prgm in (nth 2 row)
                     do (setf (gethash prgm hashtbl) (first row)))))
    hashtbl))

(defun print-hash-entry (key value)
  (format t "~s ~s" key value)
  (terpri))

(defun find-root (prgm-list hashtbl)
  (loop for prgm in prgm-list
        do (when (not (gethash (first prgm) hashtbl))
             (return (first prgm)))))

(defvar input (mapcar #'parse-input-row raw-input))

(print (find-root input (build-hash input))) 

(defun build-hash-children (input)
  (let ((hashtbl (make-hash-table :test 'equal)))
    (loop for row in input 
          do (setf (gethash (first row) hashtbl) (nth 2 row)))
    hashtbl))

(defun build-hash-weight (input)
  (let ((hashtbl (make-hash-table :test 'equal)))
    (loop for row in input
          do (setf (gethash (first row) hashtbl) (nth 1 row)))
    hashtbl))

(terpri)

(defvar children-map (build-hash-children input))
(defvar weight-map (build-hash-weight input))

(defun find-weight (prgm children-map weight-map)
  (+ 
    (gethash prgm weight-map)
    (reduce 
      #'+ 
      (mapcar 
        #'(lambda (child) (find-weight child children-map weight-map))
        (gethash prgm children-map)))))

(defun find-weight-correction (lst)
  (let ((srt-list (sort lst #'(lambda (a b) (< (first a) (first b))))))
    (let ((diff (- (first (first srt-list)) (first (first (last  srt-list))))))
      (if (= (first (first srt-list)) (first (second srt-list)))
        (+ (second (first (last srt-list))) diff)
        (+ (second (first srt-list)) diff)))))

(defun find-uneven (input children-map weight-map)
  (loop for row in input
        when (and (nth 2 row) (not (apply '= (mapcar 
                                               #'(lambda (x) (find-weight x children-map weight-map))
                                               (nth 2 row)))))
        collect (find-weight-correction 
                  (mapcar
                    #'list
                    (mapcar 
                      #'(lambda (x) (find-weight x children-map weight-map))
                      (nth 2 row))
                    (mapcar #'(lambda (x) (gethash x weight-map)) (nth 2 row))))))

(print (apply 'min (find-uneven input children-map weight-map)))

