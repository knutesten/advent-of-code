(defvar test-input '(
                     "b inc 5 if a > 1"
                     "a inc 1 if b < 5"
                     "c dec -10 if a >= 1"
                     "c inc -20 if c == 10"
                     ))

(defvar input 
  (with-open-file (stream "./input.txt")
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun convert-to-instruction (str) (read-from-string (concatenate 'string "(part1 " str ")")))

(defvar instructions (mapcar #'convert-to-instruction input))
(defvar max-value nil)
(defvar registers (make-hash-table :test 'equal))

(defun get-reg (reg)
  (let ((value (gethash reg registers)))
    (if value value 0)))

(defun set-reg (reg value)
  (when (or (not max-value) (> value max-value))
    (setf max-value value))
  (setf (gethash reg registers) value))

(defun == (x y) (= x y))
(defun != (x y) (not (= x y)))

(defmacro
  part1
  (register1 action value1 if register2 conditional value2)
  `(when (,conditional (get-reg ',register2) ,value2)
     (set-reg 
       ',register1 
       (cond 
         ((equal ',action 'inc) (+ (get-reg ',register1) ,value1))
         ((equal ',action 'dec) (- (get-reg ',register1) ,value1))))))

(loop for instruction in instructions do (eval instruction))
(print (apply 'max (loop for value being the hash-values of registers collect value)))
(terpri)
(print max-value)

