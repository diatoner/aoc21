; aoc21 - day4 - bingo

(defstruct board nums marked-coords)
(defstruct game nums boards)

(defun split (string delim)
  (loop for i = 0 then (1+ j)
        as j = (position delim string :start i)
        for substr = (subseq string i j)
        when (> (length substr) 0)
        collect substr
        while j))

(defun splitints (string delim)
  (mapcar #'parse-integer (split string delim)))

(defun get-input (fname)
  (let* ((lines (with-open-file (in fname)
                  (loop for line = (read-line in nil)
                        while line
                        when (> (length line) 0)
                        collect line)))
         (nums (splitints (car lines) #\,))
         (boards (loop for i = 1 then (+ 5 i)
                       while (< i (length lines))
                       for rows = (mapcar
                                   (lambda (r) (splitints r #\ ))
                                   (subseq lines i (+ 5 i)))
                       collect (make-board :nums rows :marked-coords '()))))
    (make-game :nums nums :boards boards)))

(defun find-on-board (num board)
  (loop for i from 0 to (length (board-nums board))
        for y = (nth i (board-nums board))
        for x = (position num y)
        when x
        return (list x i)))

(defun check-victory (board coord)
  (when coord 
    (or (= 5 (loop for c in (board-marked-coords board)
                   count (= (car c) (car coord))))
        (= 5 (loop for c in (board-marked-coords board)
                   count (= (cadr c) (cadr coord)))))))

(defun already-marked (board coord)
  (loop for c in (board-marked-coords board)
        when (and (= (car c) (car coord))
                  (= (cadr c) (cadr coord)))
        return T
        finally (return nil)))

(defun mark-on-board (num board)
  (let ((coord (find-on-board num board)))
    (when (and coord (not (already-marked board coord)))
      (setf (board-marked-coords board)
            (append (board-marked-coords board) (list coord))))))


(defun pp-board (board)
  (let ((row (nth 0 board)))
    (format nil "(~a ~a ~a ...)"
            (nth 0 row)
            (nth 1 row)
            (nth 2 row))))

;; -- part 1 solution
(let ((game (get-input "day4.input"))
      (winning-board nil)
      (winning-num nil))
  (loop for num in (game-nums game)
        while (null winning-board)
        for boards = (loop for board in (game-boards game)
                           for has-num = (find-on-board num board)
                           collect board)
        do (mapcar
            (lambda (b)
              (mark-on-board num b)
              (if 
               (check-victory b (find-on-board num b))
               (progn
                 (setf winning-board b)
                 (setf winning-num num))))
            boards))
  (when (and winning-num winning-board)
    (* winning-num
       (loop for x from 0 to 4
             sum (loop for y from 0 to 4
                       when (not (already-marked winning-board (list x y)))
                       sum (nth x (nth y (board-nums winning-board))))))))
