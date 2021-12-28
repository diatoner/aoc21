;; aoc21 - day8

;; utility fn: split a sequence by a delimiter
(defun split (xs delim)
  (iter
    (for i first 0 then (1+ j))
    (for j = (position delim xs :start i :test 'equal))
    (for piece = (if j (subseq xs i j) (subseq xs i (length xs))))
    (collect piece into pieces)
    (when (null j) (return pieces))))

;; gets lines from a file
(defun get-input (fname)
  (with-open-file (in fname)
    (iter
      (for line = (read-line in nil))
      (while line)
      (collect line))))

;; data wrangling for part 1
(defun output-values (lines)
  (iterp
    (for line in lines)
    (for pdelim = (position #\| line))
    (for output-value-raw = (subseq line (+ 2 pdelim)))
    (for output-value = (split output-value-raw #\ ))
    (collect output-value)))

;; part 1 solution
(defun part-1 ()
  (iter
    (with displays = (get-input "day8.input"))
    (with outputs  = (output-values displays))
    (for ov in outputs)
    (sum (iter
           (for n in ov)
           (for l = (length n))
           (counting (or (= l 2) ;; 2 lines = digit 1
                         (= l 3) ;; 3 lines = digit 7
                         (= l 4) ;; 4 lines = digit 4
                         (= l 7))))))) ;; 7 lines = 8

;; set intersection of strings, returns list of chars 
(defun strintersection (a b)
  (intersection (coerce a 'list) (coerce b 'list)))

;; parsing digits
(defun is-one (cs) (= 2 (length cs)))
(defun is-seven (cs) (= 3 (length cs)))
(defun is-four (cs) (= 4 (length cs)))
(defun is-eight (cs) (= 8 (length cs)))
(defun is-zero (cs digit-cache)
  (and
   (= 6 (length cs))
   (= 3 (length (strintersection cs (gethash 4 digit-cache))))
   (= 2 (length (strintersection cs (gethash 1 digit-cache))))))
(defun is-two (cs digit-cache)
  (and
   (= 5 (length cs))
   (= 2 (length (strintersection cs (gethash 4 digit-cache))))))
(defun is-three (cs digit-cache)
  (and
   (= 5 (length cs))
   (= 3 (length (strintersection cs (gethash 4 digit-cache))))
   (= 3 (length (strintersection cs (gethash 7 digit-cache))))))
(defun is-five (cs digit-cache)
  (and
   (= 5 (length cs))
   (= 2 (length (strintersection cs (gethash 7 digit-cache))))
   (= 3 (length (strintersection cs (gethash 4 digit-cache))))))
(defun is-six (cs digit-cache)
  (and
   (= 6 (length cs))
   (= 2 (length (strintersection cs (gethash 7 digit-cache))))
   (= 3 (length (strintersection cs (gethash 4 digit-cache))))))
(defun is-nine (cs digit-cache)
  (and
   (= 6 (length cs))
   (= 4 (length (strintersection cs (gethash 4 digit-cache))))))

;; this is the main element of part 2's solution
(defun map-digits (values)
  (let ((digit-cache (make-hash-table :test 'equal)))
    (iter
      (for s in values)
      (for n = (sort s #'char-lessp))
      (when (= 2 (length n)) (setf (gethash 1 digit-cache) n))
      (when (= 3 (length n)) (setf (gethash 7 digit-cache) n))
      (when (= 4 (length n)) (setf (gethash 4 digit-cache) n))
      (when (= 7 (length n)) (setf (gethash 8 digit-cache) n)))
    (iter
      (for s in values)
      (for n = (sort s #'char-lessp))
      (when (is-zero n digit-cache)
        (setf (gethash 0 digit-cache) n))
      (when (is-two n digit-cache)
        (setf (gethash 2 digit-cache) n))
      (when (is-three n digit-cache)
        (setf (gethash 3 digit-cache) n))
      (when (is-five n digit-cache)
        (setf (gethash 5 digit-cache) n))
      (when (is-six n digit-cache)
        (setf (gethash 6 digit-cache) n))
      (when (is-nine n digit-cache)
        (setf (gethash 9 digit-cache) n)))
    digit-cache))

;; utility func for part2
(defun invert-hash-table (x)
  (let ((res (make-hash-table :test 'equal)))
    (iter
      (for (k v) in-hashtable x)
      (setf (gethash v res) k))
    res))

;; used in part2 solution, just refactored type wrangling here
(defun digits->int (ds)
  (parse-integer
   (format nil "~{~A~}" (mapcar #'write-to-string ds))))

;; utilities for line splitting, for use in part 2
(defun signals (line)
  (subseq (split line #\ ) 0 10))
(defun output-value (line)
  (subseq (split line #\ ) 11))

;; part2 solution: (parse (get-input "day8.input"))
(defun parse (lines)
  (iter
    (for line in lines)
    (for sigs = (signals line))
    (for dmap = (invert-hash-table (map-digits sigs)))
    (for oval = (output-value line))
    (sum (digits->int (iter
                        (for v in oval)
                        (for n = (sort v #'char-lessp))
                        (for d = (gethash n dmap))
                        (collect d))))))
