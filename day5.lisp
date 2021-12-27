;; aoc21 - day5

;; prefer (iter) over (loop) for clever shit
(ql:quickload :iterate)
(use-package :iterate)

;; general string split utility
(defun split (string delim)
  (loop for i = 0 then (1+ j)
        as j = (position delim string :start i)
        for substr = (subseq string i j)
        when (> (length substr) 0)
        collect substr
        while j))

;; building up fns to parse line segment definitions
(defstruct point x y)
(defun fpt (pt) (format nil "(~a,~a)" (point-x pt) (point-y pt)))
(defun ->point (xs)
  (make-point :x (car xs) :y (cadr xs)))
(defun parse-linedef (linedef)
  (let ((parts (split linedef #\ )))
    (list (->point (mapcar 'parse-integer (split (car parts) #\,)))
          (->point (mapcar 'parse-integer (split (caddr parts) #\,))))))
(defun get-input (fname)
  (with-open-file (in fname)
    (loop for line = (read-line in nil)
          while line
          collect (parse-linedef line))))

;; point utils
(defun point- (a b)
  (make-point :x (- (point-x b) (point-x a))
              :y (- (point-y b) (point-y a))))
(defun point+ (a b)
  (make-point :x (+ (point-x a) (point-x b))
              :y (+ (point-y a) (point-y b))))
(defun unit-point (p)
  (make-point :x (signum (point-x p))
              :y (signum (point-y p))))
(defun cardinal? (p)
  (or (and (= 1 (abs (point-x p))) (= 0 (point-y p)))
      (and (= 1 (abs (point-y p))) (= 0 (point-x p)))))
(defun mag (p)
  (let ((x (point-x p)) (y (point-y p)))
    (sqrt (+ (* x x) (* y y)))))

;; answers
(defun part-1 (fname)
  (setf *hash* (make-hash-table :test 'equal))
  (iter (for (p0 p1) in (get-input fname))
    (for dp = (point- p0 p1))
    (for udp = (unit-point dp))
    (when (cardinal? udp)
      (iter (for step from 0 to (mag dp))
        (for p initially p0 then (point+ p udp))
        (for k = (fpt p))
        (incf (gethash k *hash* 0)))))
  (iter (for (nil v) in-hashtable *hash*)
    (when (> v 1) (count v))))

(defun part-2 (fname)
  (setf *hash* (make-hash-table :test 'equal))
  (iter (for (p0 p1) in (get-input fname))
    (for dp = (point- p0 p1))
    (for dir = (unit-point dp))
    (for steps = (1+ (max
                      (abs (point-x dp))
                      (abs (point-y dp)))))
    (format t
            "~a -> ~a (~a x ~a)~%"
            (fpt p0) (fpt p1) steps (fpt dir))
    (iter
      (repeat steps)
      (for p initially p0 then (point+ p dir))
      (incf (gethash (fpt p) *hash* 0))))
  (iter (for (nil v) in-hashtable *hash*)
    (counting (> v 1))))
