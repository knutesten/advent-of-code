(defvar maze (with-open-file (in "./input.txt")
               (loop for line = (read-line in nil)
                     while line
                     collect (coerce line 'list))))

(defun calc-start-pos (maze)
  (loop for i from 0 below (length maze) do 
        (let ((row (nth i maze))
              (last-idx (1- (length (nth i maze)))))
          (when (or (= 0 i) (= i (1- (length maze))))
            (let ((pos (loop for j from 0 to last-idx do 
                             (when (not (char= #\Space (nth j row))) 
                               (return (list i j))))))
              (when pos (return pos))))

          (when (not (char= #\Space (nth 0 row)))
            (return (list i 0)))

          (when (not (char= #\Space (nth last-idx row)))
            (return (list i last-idx))))))

(defun get-elm (pos maze) (nth (second pos) (nth (first pos) maze)))

(defun calc-start-direction (pos maze)
  (if (= 0 (first pos))
    (if (char= #\| (get-elm pos maze)) 
      (list 1 0)
      (if (= 0 (second pos)) (list 0 1) (list 0 -1)))

    (if (char= #\| (get-elm pos maze)) 
      (list -1 0)
      (if (= 0 (second pos)) (list 0 1) (list 0 -1)))))

(defun next-pos (pos dir) (list (+ (first pos) (first dir)) (+ (second pos) (second dir))))

(defun next-dir (pos dir maze)
  (if (= 0 (first dir))
    (if (not (char= #\Space (get-elm (next-pos pos (list 1 0)) maze)))
      (list 1 0) (list -1 0))
    (if (not (char= #\Space (get-elm (next-pos pos (list 0 1)) maze)))
      (list 0 1) (list 0 -1))))

(defun walk-path (maze)
  (let ((pos (calc-start-pos maze)))
    (let ((elm nil)
          (path nil)
          (dir (calc-start-direction pos maze)))
      (format nil "~{~A~}"
              (loop 
                (setq pos (next-pos pos dir))
                (setq elm (get-elm pos maze))
                (cond
                  ((char= elm #\Space) (return path))
                  ((char= elm #\+) (setq dir (next-dir pos dir maze)))
                  ((and (not (char= elm #\|)) (not (char= elm #\-))) (setq path (append path (list elm))))))))))


(print (walk-path maze))
(terpri)

(defun find-path-length (maze)
  (let ((pos (calc-start-pos maze)))
    (let ((elm nil)
          (len 0)
          (path nil)
          (dir (calc-start-direction pos maze)))
      (loop 
        (setq len (1+ len))
        (setq pos (next-pos pos dir))
        (setq elm (get-elm pos maze))
        (cond
          ((char= elm #\Space) (return len))
          ((char= elm #\+) (setq dir (next-dir pos dir maze)))
          ((and (not (char= elm #\|)) (not (char= elm #\-))) (setq path (append path (list elm)))))
        ))))

(print (find-path-length maze))
(terpri)

