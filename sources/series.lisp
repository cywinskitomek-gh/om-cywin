(in-package :om)

;==================================================

(defun collatz (x)
  "Collatz sequence starting from X (ends with 1)."

  (loop collect x into list
        if (= x 1)
        return list
        do (setf x (if (evenp x) (/ x 2) (1+ (* x 3)))))

)

(defun 0&natural-ser (x)
  "List of integers 0..X."

  (loop for i to x
        collect i)

)

(defun interpole2 (list-x list-y x-min x-max nbsamples)
  "Linear interpolation of the points LIST-X, LIST-Y: NBSAMPLES values from X-MIN to X-MAX."
  (if (= 1 nbsamples) (list (x-transfer (mat-trans (list list-x list-y)) (+ x-min (/ (- x-max x-min) 2))))
  (let ((step (/ (- x-max x-min) (1- nbsamples))))
    (loop with x = (pop list-x) and xx = (pop list-x) 
          and y = (pop list-y) and yy = (pop list-y)
          with x-index = x-min
          with s-index = 0
          while (and xx  (< s-index nbsamples))
          if (and (>= x-index x) (<= x-index xx))
          collect (linear-interpol x xx y yy x-index)
          and do (setf x-index (+ x-min (* (incf s-index) step)))
          else do (setf x xx xx (pop list-x) y yy yy (pop list-y))))))