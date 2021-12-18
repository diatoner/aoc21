;; aoc21 - day3

(defvar *test-input* '(00100
                       11110
                       10110
                       10111
                       10101
                       01111
                       00111
                       11100
                       10000
                       11001
                       00010
                       01010))

(defun get-input ()
  (with-open-file (in "day3.input")
    (loop for line = (read-line in nil nil)
          while line
          collect (parse-integer line))))

(defun int->digitlist (x)
  (loop for c across (write-to-string x)
        collect (digit-char-p c)))

(defun lpad (xs len)
  (if (< (length xs) len)
      (lpad (cons 0 xs) len)
      xs))

(defun bin2dec (bits)
  (let ((res 0))
    (loop for x in bits
          for i = (1- (length bits)) then (1- i)
          do (incf res (* x (expt 2 i))))
    res))

(defun part-1 (input)
  (let* ((most-common-bit-threshold (/ (length input) 2))
         (biggest (apply 'max input))
         (maxlen  (ceiling (log biggest 10)))
         (nums (mapcar
                (lambda (x) (lpad (int->digitlist x) maxlen))
                input))
         (1counts (reduce (lambda (a b) (mapcar #'+ a b)) nums))
         (gamma (mapcar
                 (lambda (x) (if (> x most-common-bit-threshold) 1 0))
                 1counts))
         (epsilon (mapcar (lambda (x) (- 1 x)) gamma)))
    (apply #'* (mapcar 'bin2dec (list gamma epsilon)))))

(defun parse-input (input)
  (let* ((biggest (apply #'max input))
         (maxlen  (ceiling (log biggest 10))))
    (mapcar
     (lambda (x) (lpad (int->digitlist x) maxlen))
     input)))

(defun part-2 (bitlists n f)
  (if (= 1 (length bitlists))
      (car bitlists)
      (part-2 (funcall f bitlists n) (1+ n) f)))

(defun agg-at (xss f n)
  (funcall f (apply #'+
                    (mapcar (lambda (x) (nth n x)) xss))))

(defun mcb>= (bitlists n)
  (if (agg-at bitlists (lambda (x) (>= x (/ (length bitlists) 2))) n)
      1
      0))

(defun lcb< (bitlists n)
  (if (agg-at bitlists
              (lambda (x)
                (< x (/ (length bitlists) 2)))
              n)
      1
      0))

(defun o2? (bitlists n)
  (loop for xs in bitlists
        when (= (nth n xs) (mcb bitlists n))
        collect xs))

(defun co2? (bitlists n)
  (loop for xs in bitlists
        when (= (nth n xs) (lcb< bitlists n))
        collect xs))

; solution:
; (* (bin2dec (part-2 (parse-input (get-input)) 0 'o2?))
;    (bin2dec (part-2 (parse-input (get-input)) 0 'co2?)))
