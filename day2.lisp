;; aoc21 - day2

(defun line-to-list (x)
  (read-from-string (concatenate 'string "(" x ")")))

(defun get-input ()
  (with-open-file (in "day2.input")
    (loop for line = (read-line in nil nil)
          while line
          collect (line-to-list line))))

(defun part-1 ()
  (let ((x 0) (y 0))
    (loop for command in (get-input)
          for (a b) = command
          do (case a ('forward (incf x b))
                     ('down    (incf y b))
                     ('up      (decf y b))))
    (* x y)))

(defun part-2 ()
  (let ((x 0) (y 0) (aim 0))
    (loop for command in (get-input)
          for (a b) = command
          do (case a ('forward (incf x b)
                               (incf y (* aim b)))
                     ('down    (incf aim b))
                     ('up      (decf aim b))))
    (* x y)))
