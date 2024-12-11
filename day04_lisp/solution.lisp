(setq *print-case* :capitalize)
(defvar *filename* "input.txt")


(defun read-input(filename)
  (let ((grid (list)))
    (let ((in (open filename)))
      (when in
        (loop for line = (read-line in nil) while line do
          (setq grid (append grid (list (coerce line 'vector)))))
        (close in)))
    ;; convert the grid list into an array
    (coerce grid 'vector)))

;;; count occurrences of a word in a line
(defun count-occurrences(line word)
  (let (
    (len (length word))
    (i 0) ; word index
    (freq 0))

    (dotimes (j (length line))
      (cond
        ((char= (char word i) (char line j)) (setq i (+ i 1)))
        (t (progn
          (setq i 0)
          (if (char= (char word i) (char line j)) (setq i 1)))))
      (if (= i len)
        (progn
          (setq freq (+ freq 1))
          (setq i 0))))

    freq)
)


(defun solve-part-1(grid)
  (let (
    (rows (array-dimension grid 0))
    (cols (length (aref grid 0)))
    (line "")
    (xmas-count 0)
    (word "XMAS"))
  
    ;; check columns forwards and backwards
    (dotimes (col cols)
      (setq line "")
      (dotimes (row rows)
        (setq line (concatenate 'string line (string (char (aref grid row) col)))))
      (setq xmas-count (+ xmas-count (count-occurrences line word) (count-occurrences (reverse line) word))))
  
    ;; check rows forwards and backwards
    (loop for line across grid do 
      (setq xmas-count (+ xmas-count (count-occurrences line word) (count-occurrences (reverse line) word))))

    ;; check back slashing diagonals forwards and backwards
    ;; check every point along the top and left sides

    ;; top side
    (loop for col from 0 to cols do
      (setq line "")
      (dotimes (i (- rows col))
        (setq line (concatenate 'string line (string (char (aref grid i) (+ col i))))))
      (setq xmas-count (+ xmas-count (count-occurrences line word) (count-occurrences (reverse line) word))))
    ;; left side
    (loop for row from 1 to rows do
      (setq line "")
      (dotimes (i (- cols row))
        (setq line (concatenate 'string line (string (char (aref grid (+ row i)) i)))))
      (setq xmas-count (+ xmas-count (count-occurrences line word) (count-occurrences (reverse line) word))))

    ;; check forward slashing diagonals forwards and backwards
    ;; check every point along the edge of the grid in a "seven" pattern (top and right sides)
    ;; I'm just going to reverse each row to make this easier on myself

    ;; top side (backwards)
    (loop for col from 0 to cols do
      (setq line "")
      (dotimes (i (- rows col))
        (setq line (concatenate 'string line (string (char (reverse (aref grid i)) (+ col i))))))
      (setq xmas-count (+ xmas-count (count-occurrences line word) (count-occurrences (reverse line) word))))
    ;; left side
    (loop for row from 1 to rows do
      (setq line "")
      (dotimes (i (- cols row))
        (setq line (concatenate 'string line (string (char (reverse (aref grid (+ row i))) i)))))
      (setq xmas-count (+ xmas-count (count-occurrences line word) (count-occurrences (reverse line) word))))

    (format t "~a~%" xmas-count)
  )
)


;; check if a line matches the passed configuration.
;; * characters are wildcards, and the only relevant characters are A, M, and SPs
(defun matches-configuration(line configuration)
  (cond
    ((= (length line) 1) (char= (char line 0) (char configuration 0)))
    (t (and (char= (char line 0) (char configuration 0)) (matches-configuration (subseq line 2) (subseq configuration 2))))
  )
)


(defun is-xmas(line)
  (cond
    ((matches-configuration line "M*M*A*S*S") t)
    ((matches-configuration line "S*S*A*M*M") t)
    ((matches-configuration line "S*M*A*S*M") t)
    ((matches-configuration line "M*S*A*M*S") t)
    (t nil)
  ))

;; Flatten a 3x3 kernel into a 9 character line
(defun flatten-kernel(grid row col)
  (let (
    (line ""))

    (dotimes (i 3)
      (dotimes (j 3)
        (setq line (concatenate 'string line (string (char (aref grid (+ col i)) (+ row j)))))))

    line
  )
)

(defun solve-part-2(grid)
  (let (
    (rows (array-dimension grid 0))
    (cols (length (aref grid 0)))
    (xmas-count 0))

    (dotimes (row (- rows 2))
      (dotimes (col (- cols 2))
        (if (is-xmas (flatten-kernel grid row col)) (setq xmas-count (+ xmas-count 1)))))

    (format t "~w~%" xmas-count)
  )
)


(defvar *grid* (read-input *filename*))
(format t "Part 1:~%")
(solve-part-1 *grid*)
(format t "Part 2:~%")
(solve-part-2 *grid*)