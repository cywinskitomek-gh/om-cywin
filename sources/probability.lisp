;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod! choice-weights ((choice list) (weights list))
  :doc "Picks one element of <choice> at random with the probabilities <weights>. With an empty <choice> returns the chosen index."

                :initvals '((0 1 2) (5 2 3))
                :icon 9114

                (let* ((probabilities (om-scale/sum weights 1))
                       (index (do ((i 0 (1+ i))
                                   (u (/ (random 10000.0) 10000.0))
                                   (val 0))
                                  ((> val u) (1- i))
                                (setf val (+ val (nth i probabilities))))))
                  (if choice 
                      (nth index choice)
                      index)))


(defmethod! pascals-triangle ((n integer))
  :doc "The first <n> rows of Pascal's triangle."

            :initvals '(5)
            :icon 9114

  (let (triangle)
    (loop for i from 0 to (1- n)
          do (let (line)
               (loop for j from 0 to i
                     do (if (and (> j 0) (< j i))
                            (let* ((prevline (car triangle))
                                   (prevb (nth (1- j) prevline))
                                   (preva (nth j prevline)))
                              (setf line (cons
                                          (+ prevb
                                             preva) line)))
                          (setf line (cons 1 line))))
               (setf triangle (cons (reverse line) triangle))))
    (reverse triangle))

)


(defmethod! pascals-triangle-r ((r t))
  :doc "Row <r> of Pascal's triangle (r = 1 gives (1))."

            :initvals '(5)
            :icon 9114

  (if (and (numberp r) (plusp r))
      (labels ((nrow (i-row i)
                 (if (= i r)
                     (return-from nrow i-row)
                     (nrow (mapcar #'+ (cons 0 i-row) (append i-row '(0))) (1+ i)))))
        (nrow '(1) 1))
      '())

)


(defmethod! n-row-pascals-triangle ((n integer))
  :doc "Row <n> of Pascal's triangle."

            :initvals '(5)
            :icon 9114

(pascals-triangle-r n)

)

(defmethod! choice-gaussian-weights ((choice list))
  :doc "Picks one element of <choice> at random with weights from Pascal's triangle (most often the middle elements)."

            :initvals '(0 1 2)
            :icon 9114

   (choice-weights choice (pascals-triangle-r (length choice)))
)