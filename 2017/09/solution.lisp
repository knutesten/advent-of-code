(defvar test-input-list 
  '("{}"
    "{{{}}}"
    "{{},{}}"
    "{{{},{},{{}}}}"
    "{<a>,<a>,<a>,<a>}"
    "{{<ab>},{<ab>},{<ab>},{<ab>}}"
    "{{<!!>},{<!!>},{<!!>},{<!!>}}"
    "{{<a!>},{<a!>},{<a!>},{<ab>}}"))

(defun calculate-score (str)
  (let ((depth 0)
        (score 0)
        (in-ignore nil)
        (in-garbage nil)
        (garbage-count 0))
    (loop for ch across str
          do 
          (if (not in-ignore)
            (progn
              (if (not in-garbage)
                (cond
                  ((char= ch #\<) (setf in-garbage t))
                  ((char= ch #\{) (setf depth (1+ depth)))
                  ((char= ch #\}) 
                   (setf score (+ score depth))
                   (setf depth (1- depth))))
                (when (and 
                        (not (char= ch #\>)) 
                        (not (char= ch #\!)))
                  (setf garbage-count (1+ garbage-count))))
              (cond
                ((char= ch #\!) (setf in-ignore t))
                ((char= ch #\>) (setf in-garbage nil))))
            (setf in-ignore nil)))
    (list score garbage-count)))

;(loop for input in test-input-list do (print (calculate-score input)))

(defvar input (read-line (open "./input.txt") nil))
(defvar result (calculate-score input))

(print (first result))
(terpri)
(print (second result))

