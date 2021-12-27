;; aoc21 -- day7

(ql:quickload :iterate)
(use-package :iterate)

;; wrangling input data
(defun chars->int (chars)
  (parse-integer (concatenate 'string chars)))
(defun read-til (stream delim)
  (iter (for cc = (read-char stream nil))
    (while (and cc (not (equal cc delim))))
    (collect cc)))
(defun get-input (fname)
  (with-open-file (in fname)
    (iter (for c = (peek-char t in nil))
      (while (and c (not (equal c #\Newline))))
      (collect (chars->int (read-til in #\,))))))

;; cost functions
(defun mvcost-p1 (xs p)
  (iter (for x in xs)
    (for dp = (abs (- x p)))
    (sum dp)))
(defun mvcost-p2 (xs p)
  (iter (for x in xs)
    (for dp = (abs (- x p)))
    (for local-cost = (* dp (/ (1+ dp) 2)))
    (sum local-cost)))

;; algorithms
(defun lc-bruteforce (xs mvcost)
  (iter
    (with lo = (apply #'min xs))
    (with hi = (apply #'max xs))
    (for p from lo to hi)
    (for c = (funcall mvcost xs p))
    (finding (list p c) minimizing c)))
;; for fun - it's faster
(defun lc-bisect (xs mvcost &optional (low nil) (high nil))
  (setf low (or low (apply #'min xs)))
  (setf high (or high (apply #'max xs)))
  (if (= low high)
      (list low (funcall mvcost xs low))
      (let ((lores (funcall mvcost xs low))
            (hires (funcall mvcost xs high))
            (mid   (floor (/ (+ low high) 2))))
        (cond ((= lores hires) (list low lores))
              ((< lores hires) (lc-bisect xs mvcost low mid))
              (t               (lc-bisect xs mvcost mid high))))))
