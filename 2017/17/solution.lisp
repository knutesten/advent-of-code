(defun hurricane (step-forward)
  (let ((mem '(0))
        (new-pos 0))
    (loop for i from 1 to 2017 do
          (setq new-pos (1+ (mod (+ new-pos step-forward) (length mem))))
          (setq mem (append 
                      (subseq mem 0 new-pos)
                      (list i)
                      (subseq mem new-pos))))
    (nth (mod (1+ (position 2017 mem)) (length mem)) mem)
    ))

(print (hurricane 356))
(terpri)

(defun hurricane-2 (step-forward)
  (let ((new-pos 0)
        (value nil))
    (loop for i from 1 to 50000000 do
          (setq new-pos (1+ (mod (+ new-pos step-forward) i)))
          (when (= 1 new-pos)
            (setq value i)))
    value
    ))

(print (hurricane-2 356))
(terpri)

