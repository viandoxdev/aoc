; side of the cube we place the lava in
(defconstant SIDE 20)
; macro that does nothing, used to keep code aligned (id x) is the same length as (1- x)
(defmacro id (v) v)

; split string by delim character
(defun split (string delim)
  (loop :for beg = 0
    :then (1+ end)
    :for end = (and beg (position delim string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

; read and parse the input into a list of vec3 (vec3 is a list of 3 integers)
(defun input () (with-open-file (stream "input")
  (loop for line = (read-line stream nil)
    while line 
    collect (mapcar #'parse-integer (split line #\,)))))

; put the parsed input in a SIDE^3 grid of integers, 1 means air 0 means lava
(defun grid (input) 
  (let ((array (make-array (list SIDE SIDE SIDE) :initial-element 1)))
    (loop for cube in input do (setf (aref array (nth 0 cube) (nth 1 cube) (nth 2 cube)) 0))
    array))

; print the grid
(defun show-grid (grid) 
  (loop for z below SIDE do
    (loop for y below SIDE do
      (loop for x below SIDE do
        (format t "~[[]~;. ~]" (aref grid x y z)))
      (format t "~%"))
    (format t "~2%")))

; get the value at index (x y z) of grid, returns 1 if out of bounds
(defun acc (arr x y z)
  (or (and 
    (< x SIDE)
    (< y SIDE)
    (< z SIDE)
    (>= x 0)
    (>= y 0)
    (>= z 0)
    (aref arr x y z)) 1))

; solves part1: count exposed sides
(defun count-exposed-sides (input) 
  (let ((g (grid input)))
    (apply #'+ 
      (loop for (x y z) in input 
        collect (+
          (acc g (1+ x) (id y) (id z))
          (acc g (1- x) (id y) (id z))
          (acc g (id x) (1+ y) (id z))
          (acc g (id x) (1- y) (id z))
          (acc g (id x) (id y) (1+ z))
          (acc g (id x) (id y) (1- z)))))))

; return '(x y z) if the cell at index (x y z) of grid is both in bounds and not lava, otherwise returns nil
(defun valid-empty (grid x y z)
  (and 
    (< x SIDE)
    (< y SIDE)
    (< z SIDE)
    (>= x 0)
    (>= y 0)
    (>= z 0)
    (if ( = 1 (aref grid x y z)) (list x y z) nil)))

; get the empty unvisited neighbours of a cell
(defun empty-neighbours (grid c unvisited)
  (destructuring-bind (x y z) c 
      (remove-if-not
        (lambda (el) 
          (and el (aref unvisited (nth 0 el) (nth 1 el) (nth 2 el))))
        (list
          (valid-empty grid (1+ x) (id y) (id z))
          (valid-empty grid (1- x) (id y) (id z))
          (valid-empty grid (id x) (1+ y) (id z))
          (valid-empty grid (id x) (1- y) (id z))
          (valid-empty grid (id x) (id y) (1+ z))
          (valid-empty grid (id x) (id y) (1- z))))))

; get the number of sides exposed to lava of a cell
(defun lava-exposed-sides (g c)
  (destructuring-bind (x y z) c 
    (- 6 (+ 
      (acc g (1+ x) (id y) (id z))
      (acc g (1- x) (id y) (id z))
      (acc g (id x) (1+ y) (id z))
      (acc g (id x) (1- y) (id z))
      (acc g (id x) (id y) (1+ z))
      (acc g (id x) (id y) (1- z))))))

; count the number of exterior lava exposed sides, excluding the ones touching the bounds
; this works by flood filling from (0 0 0) and counting the lava exposed sides from there
(defun _count-exterior-sides (grid &optional (els '((0 0 0))) (unvisited (make-array (list SIDE SIDE SIDE) :initial-element t)) )
  (if (null els) 0
    (let ((neighbours (remove-duplicates (mapcan (lambda (el) (empty-neighbours grid el unvisited)) els) :test #'equal)))
      (loop for el in neighbours do (setf (aref unvisited (nth 0 el) (nth 1 el) (nth 2 el)) nil))
      (+
        (apply #'+ 
          (mapcar (lambda (el) (lava-exposed-sides grid el)) els))
        (_count-exterior-sides grid neighbours unvisited)))))

; solves part 2 adds the result from the function above to the number of lava sides touching the bounds
(defun count-exterior-sides (input)
  (let ((grid (grid input)))
    (+
      (_count-exterior-sides grid)
      (apply #'+ (mapcar (lambda (el) (if (= 0 (nth 0 el)) 1 0)) input))
      (apply #'+ (mapcar (lambda (el) (if (= 0 (nth 1 el)) 1 0)) input))
      (apply #'+ (mapcar (lambda (el) (if (= 0 (nth 2 el)) 1 0)) input))
      (apply #'+ (mapcar (lambda (el) (if (= (1- SIDE) (nth 0 el)) 1 0)) input))
      (apply #'+ (mapcar (lambda (el) (if (= (1- SIDE) (nth 1 el)) 1 0)) input))
      (apply #'+ (mapcar (lambda (el) (if (= (1- SIDE) (nth 2 el)) 1 0)) input)))))

; compute and print results
(let ((input (input)))
  (show-grid (grid input))
  (format t "Part1: ~d~%" (count-exposed-sides input))
  (format t "Part2: ~d~%" (count-exterior-sides input)))
