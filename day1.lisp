;; aoc21 -- day1

(defvar *test-input*
  (list 199
        200
        208
        210
        200
        207
        240
        269
        260
        263))

(defun count-increases (depths)
  (count T (mapcar #'< depths (cdr depths))))

;; part 1 solution:
;; (count-increases
;;  (with-open-file (in "day1.input")
;;    (for line in (read-line in nil)
;;         while line
;;         collect (parse-integer line))))

(defun windows (xs len)
  (loop for i from 0 to (length xs)
        for l = (min (+ i len) (length xs))
        for ts = (subseq xs i l)
        when (= (length ts) len)
        collect ts))

;; part 2 actual solution:
;; (count-increases
;;  (mapcar
;;   (lambda (xs) (apply #'+ xs))
;;   (windows
;;    (with-open-file (in "day1.input")
;;      (loop for line = (read-line in nil)
;;            while line
;;            collect (parse-integer line)))
;;    3)))
