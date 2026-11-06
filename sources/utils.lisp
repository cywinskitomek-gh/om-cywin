(in-package :om)

;==================================================



;==================================================


;========================================================

(defun list-ever (it)
  "Always returns a flat list (an atom becomes a one-element list)."

  (flat (list it))

)

(defun list-sum (it)
  "Sum of all numbers in a (nested) list, nil skipped."

  (loop for x in (remove nil (list-ever it))
        sum x
        )

)

(defun list-maximum (it)
  "Largest number in a (nested) list, nil skipped."

  (reduce #'(lambda (x y) (max x y)) (remove nil (list-ever it)))

)

(defun list-minimum (it)
  "Smallest number in a (nested) list, nil skipped."

  (reduce #'(lambda (x y) (min x y)) (remove nil (list-ever it)))

)

(defun list-min-max (list)
  "Smallest and largest number in a (nested) list: (min max)."

  (list (list-minimum list) (list-maximum list))

)

(defun diff-min-max (list)
  "Difference between the largest and smallest number in a list."

  (- (list-maximum list) (list-minimum list))

)

(defun number-of-pitches (list)
  "Number of semitones spanned by a list of midicents (max - min)/100 + 1."

  (1+ (/ (diff-min-max list) 100))

)

;==================================================

;==================================================
;                  DATE & TIME
;==================================================

(defun current-date ()
  "Current date as (year month day)."
  (multiple-value-bind (s min h d m y) (get-decoded-time) (list y m d)))
  
(defun current-time ()
  "Current time as (hour minute second)."
  (multiple-value-bind (s min h d m y) (get-decoded-time) (list h min s)))

(defun current-date-&-time ()
  "Current date and time as (year month day hour minute second)."
  (multiple-value-bind (s min h d m y) (get-decoded-time) (list y m d h min s)))

(defmethod! format-current-date ((order list) (separator string))
  :doc "Current date as a string, with the elements in <order> (e.g. d m y) joined by <separator>."

            :initvals '('(d m y) ".")
            :menuins '((0 (("d m y" '(d m y)) ("y m d" '(y m d)) ("m d y" '(m d y)) ("d m" '(d m)) ("m d" '(m d)) ("m y" '(m y)) ("y m" '(y m)) ("d" '(d)) ("m" '(m)) ("y" '(y))))
                       (1 (("." ".") ("-" "-") ("/" "/") ("space" " ") ("_" "_") ("'" "'") ("*" "*") ("," ",") (":" ":") (";" ";") ("\\" "\\")))
                      )
            :icon 9119

  (let* ((dformat (copy-seq order)))

    (loop for x in '(2 1 0)
          for y in '(d m y)
          do (nsubstitute x y dformat))

  (let* ((list (mapcar #'(lambda (x) (string+ (format nil "~2,'0D" (nth x (current-date))) separator)) dformat))
         (string (format nil "~{~A~}" list)))

    (if (> (length separator) 0) (subseq string 0 (1- (length string))) (subseq string 0 (length string)))

  ))
)

(defmethod! format-current-time ((order list) (separator string))
  :doc "Current time as a string, with the elements in <order> (e.g. h m s) joined by <separator>."

            :initvals '('(h m s) ":")
            :menuins '((0 (("h m s" '(h m s)) ("h m" '(h m)) ("m s" '(m s)) ("h" '(h)) ("m" '(m)) ("s" '(s))))
                       (1 ((":" ":") ("." ".") ("-" "-") ("/" "/") ("space" " ") ("_" "_") ("'" "'") ("*" "*") ("," ",") (";" ";") ("\\" "\\")))
                      )
            :icon 9119

  (let* ((dformat (copy-seq order)))

    (loop for x in '(0 1 2)
          for y in '(h m s)
          do (nsubstitute x y dformat))

  (let* ((list (mapcar #'(lambda (x) (string+ (format nil "~2,'0D" (nth x (current-time))) separator)) dformat))
         (string (format nil "~{~A~}" list)))

    (if (> (length separator) 0) (subseq string 0 (1- (length string))) (subseq string 0 (length string)))

  ))

)

(defmethod! format-current-date-&-time ((date-order list) (date-separator string) (date-time-separator string) (time-order list) (time-separator string))
  :doc "Current date and time as a string: date in <date-order> with <date-separator>, then <date-time-separator>, then time in <time-order> with <time-separator>."

            :initvals '('(y m d) "-" " " '(h m s) ":")
            :menuins '((0 (("d m y" '(d m y)) ("y m d" '(y m d)) ("m d y" '(m d y)) ("d m" '(d m)) ("m d" '(m d)) ("m y" '(m y)) ("y m" '(y m)) ("d" '(d)) ("m" '(m)) ("y" '(y))))
                       (1 (("." ".") ("-" "-") ("/" "/") ("space" " ") ("_" "_") ("'" "'") ("*" "*") ("," ",") (":" ":") (";" ";") ("\\" "\\")))
                       (2 ((":" ":") ("." ".") ("-" "-") ("/" "/") ("space" " ") ("_" "_") ("'" "'") ("*" "*") ("," ",") (";" ";") ("\\" "\\")))
                       (3 (("h m s" '(h m s)) ("h m" '(h m)) ("m s" '(m s)) ("h" '(h)) ("m" '(m)) ("s" '(s))))
                       (4 ((":" ":") ("." ".") ("-" "-") ("/" "/") ("space" " ") ("_" "_") ("'" "'") ("*" "*") ("," ",") (";" ";") ("\\" "\\")))
                      )
            :icon 9119

            (string+ (format-current-date date-order date-separator) date-time-separator (format-current-time time-order time-separator))

)




;==================================================


(defun remove-chars-from-list-in-string (string char-list)
  "Removes the characters of CHAR-LIST from a string."
  (remove-if #'(lambda (char) (find char char-list)) string)
)



;==================================================

(defmethod! semitone-number-frequency-ratio ((number-of-semitones number))
  :doc "Frequency ratio of <number-of-semitones> tempered semitones."

            :initvals '(12)
            :icon 9119

  (expt *semitone* number-of-semitones)

)

(defmethod! 1/semitone-number-frequency-ratio ((number-of-semitones number))
  :doc "Inverse of the frequency ratio of <number-of-semitones> tempered semitones."

            :initvals '(12)
            :icon 9119

  (/ (semitone-number-frequency-ratio number-of-semitones))

)


(defmethod! frequency-ratio-to-midic ((ratio number))
  :doc "Interval in midicents for a frequency ratio."

            :initvals '(1/2)
            :icon 9119

  (round (* (log ratio *semitone*) 100))

)

(defmethod! frequency-ratio-to-semitones ((ratio number))
  :doc "Interval in semitones for a frequency ratio."

            :initvals '(1/2)
            :icon 9119

  (/ (frequency-ratio-to-midic ratio) 100)

)

(defmethod! make-equal-temperament-scale ((max-interval-midic number) (keys-per-max-interval-number integer) (first-tone number))
  :doc "Equal-tempered scale: <max-interval-midic> divided into <keys-per-max-interval-number> steps, starting from <first-tone> (midicents)."

            :initvals '(1200 12 6000)
            :icon 9119

  (let ((step (/ max-interval-midic keys-per-max-interval-number)))

   (loop for i from 0 to keys-per-max-interval-number
         collect (round (+ (* i step) first-tone)))
    )

)

(defmethod! multi-by-phi ((number number))
  :doc "Multiplies a number by phi."

        :initvals '(1)
        :icon 9119

   (* number *phi*)

)

(defmethod! divide-by-phi ((number number))
  :doc "Divides a number by phi."

        :initvals '(1)
        :icon 9119

   (/ number *phi*)

)

;==================================================

(defmethod! done-signal ((self t) (message string))
  :doc "Prints '<message> is done' in the Listener and passes <self> through."

            :initvals '(nil "text")
            :icon 9119

  (om-print (format nil "~48<-> ~A is done ~>" message))

  self

)

(defun program-start (self)
  "Marks the start of a program: beeps, prints the date and START in the Listener and starts the timer. Passes SELF through."

  (om-beep)

  (om-print (format nil "~48,,,'_<~>~2%~48:@<~A~>~%~48:@<~21,,,'_<~>~>~2%~48:@<S T A R T~>~%~48:@<~21,,,'_<~>~>~%" (format-current-date-&-time '(y m d) "-" " " '(h m s) ":")))
  (setq start-program-time (get-internal-real-time))
  self

)

(defun program-stop (self)
  "Marks the end of a program: beeps and prints the time elapsed since program-start. Passes SELF through."

    (let* ((time (/ (- (get-internal-real-time) start-program-time) internal-time-units-per-second))
           (minutes (floor time 60))
           (seconds (mod time 60))
           (hours (floor minutes 60)))
    (setf minutes (mod minutes 60))

      (makunbound 'start-program-time)

      (om-beep)

      (om-print (format nil (string+ "~48,,,'_<~>~2%~48:@<elapsed program time:~>~%~48:@<" (if (zerop hours) "~*" "~D hour~:P ") (if (zerop minutes) "~*" "~D minute~:P ") "~,3F second~:P~>~%~48,,,'_<~>~%") hours minutes seconds))
     )

)

(defmethod! elapsed-time ((self t))
  :doc "Prints the time elapsed since program-start in the Listener and passes <self> through."

            :initvals '(nil)
            :icon 9119

  (let* ((time (/ (- (get-internal-real-time) start-program-time) internal-time-units-per-second))
         (minutes (floor time 60))
         (seconds (mod time 60))
         (hours (floor minutes 60)))
    (setf minutes (mod minutes 60))

    (om-print 
     (format nil (string+ "~48,,,'_<~>~2%~48:@<elapsed time:~>~%~48:@<" (if (zerop hours) "~*" "~D hour~:P ") (if (zerop minutes) "~*" "~D minute~:P ") "~,3F second~:P~>~%~48,,,'_<~>~%")  hours minutes seconds)))

  self

)

(defmethod! elapsed-time-2 ((self t))
  :doc "Prints the time elapsed since program-start in the Listener (also stored as a string) and passes <self> through."

            :initvals '(nil)
            :icon 9119

  (let* ((time (/ (- (get-internal-real-time) start-program-time) internal-time-units-per-second))
         (minutes (floor time 60))
         (seconds (mod time 60))
         (hours (floor minutes 60)))
    (setf minutes (mod minutes 60))
    (setq elpsdtm (reverse (subseq (reverse (msec-to-format-dur (om-round (* time 1000) -3))) 4)))
    (om-print 
     (format nil (string+ "~48,,,'_<~>~2%~48:@<elapsed time:~>~%~48:@<" (if (zerop hours) "~*" "~D hour~:P ") (if (zerop minutes) "~*" "~D minute~:P ") "~,3F second~:P~>~%~48,,,'_<~>~%")  hours minutes seconds)))

  self

)

;____________________________________
;

(defun random-value (value diff)
  "VALUE multiplied by a random factor between 1/(1+DIFF) and 1+DIFF."
 (* value (/ (coerce
              (om-random (round (* (/ (1+ diff)) 10E+12)) (round (* (1+ diff) 10E+12)))
              'double-float) 10E+12))
)


(defun phi*n (n)
  "N divided by phi (N * 0.618...): the golden section point of N."
  (* (/ *phi*) n))


(defun 0-phi-n (n)
  "List (0 n/phi n) with n/phi rounded down." (list 0 (floor (phi*n n)) n))

(defun 0-phi-n-2 (n)
  "List (0 n/phi n)." (list 0 (phi*n n) n))

(defun pascal (r n)
  "Element N of row R of Pascal's triangle (from 1)."
  (if (or (> n r) (< n 1) (< n 1))
      0
      (if (= n r)
          1
          (+ (pascal (1- r) (1- n)) (pascal (1- r) n)))))

;____________________________________
;

(defun eval-tree (list)
  "Evaluates a list repeatedly until it is flat (tree depth 0)."

  (labels ((evaltree (self)
             (if (> (treedepth self) 0)
                 (evaltree (eval self))
                 (return-from evaltree self))))

             (evaltree list))
)

;____________________________________
;

(defun numbers-xs-ys (a c)
  "Point lists of circles and related curves of radius A*C (helper for drawing)."
  (let ((b (* a c)))
  (list 
   ;0
   (mat-trans 
      (om/ (append (loop for x from (- b) to b
                         collect (list x (sqrt (- (expt b 2) (expt x 2)))))
                   (loop for x from b downto (- b)
                         collect (list x (- (sqrt (- (expt b 2) (expt x 2))))))
                   )
           c))
   ;1
   (mat-trans
    (om/ (append 
          (list (list (- (/ b 2)) (/ b 2)))
          (list (list 0 b))
          (list (list 0 (- b)))
          (list (list (- (/ b 2)) (- b)))
          (list (list (/ b 2) (- b)))
          )
         c))
   ;2
   (mat-trans
    (om/ (append
          (loop for x from (- b) to b
                collect (list x (+ (* (sqrt (- (expt b 2) (expt x 2))) 1/2) (/ b 2))))
          (loop for x from b downto (* b 3/4)
                collect (list x (+ (* (- (sqrt (- (expt b 2) (expt x 2)))) 1/2) (/ b 2))))
          (loop for x from (- (* b 3/4)) downto (- b)
                collect (list x (- (* (sqrt (- (expt b 2) (expt x 2))) 1/2) b)))
          (list (list (- b) (- b)))
          (list (list b (- b)))
          )
         c))
   ;3
   (mat-trans
    (om/ (append (loop for x from (- b) to b
                       collect (list (* x 12/13) (+ (* (sqrt (- (expt b 2) (expt x 2))) 6/13) (* b 7/13))))
                 (loop for x from b downto 0
                       collect (list (* x 12/13) (+ (* (- (sqrt (- (expt b 2) (expt x 2)))) 6/13) (* b 7/13))))
                 (loop for x from 0 to b
                       collect (list x (- (* (sqrt (- (expt b 2) (expt x 2))) 7/13) (* b 6/13))))
                 (loop for x from b downto (- b)
                       collect (list x (- (* (- (sqrt (- (expt b 2) (expt x 2)))) 7/13) (* b 6/13))))
                 )
         c))
   ;4
   (mat-trans
    (om/ (append 
          (list (list (/ b 2) (- b)))
          (list (list (/ b 2) b))
          (list (list (- b) (- (/ b 2))))
          (list (list b (- (/ b 2))))
          )
         c))
   ;5
   (mat-trans
    (om/ (append
          (list (list (* b 7/8) b))
          (list (list (- (* b 3/4)) b))
          (list (list (- (* b 7/8)) (* b 1/4)))
          (loop for x from 0 to b
                collect (list x (- (* (sqrt (- (expt b 2) (expt x 2))) 5/8) (* b 3/8))))
          (loop for x from b downto 0
                collect (list x (- (* (- (sqrt (- (expt b 2) (expt x 2)))) 5/8) (* b 3/8))))
          (loop for x from 0 downto (- b)
                collect (list x (- (/ (- (sqrt (- (expt b 2) (expt x 2)))) 2) (/ b 2))))
          )
         c))
   ;6
   (mat-trans
    (om/ (append
          (loop for x from (- b) to 0
                collect (list x (- (* (sqrt (- (expt b 2) (expt x 2))) 1/2) (* b 1/4))))
          (loop for x from 0 to b
                collect (list x (- (* (sqrt (- (expt b 2) (expt x 2))) 5/8) (* b 3/8))))
          (loop for x from b downto (- b)
                collect (list x (- (* (- (sqrt (- (expt b 2) (expt x 2)))) 5/8) (* b 3/8))))
          (loop for x from (- b) to 0
                collect (list x (sqrt (- (expt b 2) (expt x 2)))))
          (loop for x from 0 to (* b 2/3)
                collect (list x (+ (/ (sqrt (- (expt b 2) (expt x 2))) 2) (/ b 2))))
          )
         c))
   ;7
   (mat-trans
    (om/ (append 
          (list (list (- b) b))
          (list (list b b))
          (list (list (- (/ b 3)) (- b)))
          )
         c))
   ;8
   (mat-trans
    (om/ (append
          (loop for x from (- b) to b
                collect (list (* x 12/13) (+ (* (sqrt (- (expt b 2) (expt x 2))) 6/13) (* b 7/13))))
          (loop for x from b downto 0
                collect (list (* x 12/13) (+ (* (- (sqrt (- (expt b 2) (expt x 2)))) 6/13) (* b 7/13))))
          (loop for x from 0 downto (- b)
                collect (list x (- (* (sqrt (- (expt b 2) (expt x 2))) 7/13) (* b 6/13))))
          (loop for x from (- b) to b
                collect (list x (- (* (- (sqrt (- (expt b 2) (expt x 2)))) 7/13) (* b 6/13))))
          (loop for x from b downto 0
                collect (list x (- (* (sqrt (- (expt b 2) (expt x 2))) 7/13) (* b 6/13))))
          (loop for x from 0 downto (- b)
                collect (list (* x 12/13) (+ (* (- (sqrt (- (expt b 2) (expt x 2)))) 6/13) (* b 7/13))))
          )
         c))
   ;9
   (mat-trans
    (om/ (append
          (loop for x from b downto 0
                collect (list x (+ (* (- (sqrt (- (expt b 2) (expt x 2)))) 1/2) (* b 1/4))))
          (loop for x from 0 downto (- b)
                collect (list x (+ (* (- (sqrt (- (expt b 2) (expt x 2)))) 5/8) (* b 3/8))))
          (loop for x from (- b) to b
                collect (list x (+ (* (sqrt (- (expt b 2) (expt x 2))) 5/8) (* b 3/8))))
          (loop for x from b downto 0
                collect (list x (- (sqrt (- (expt b 2) (expt x 2))))))
          (loop for x from 0 downto (- (* b 2/3))
                collect (list x (- (/ (- (sqrt (- (expt b 2) (expt x 2)))) 2) (/ b 2))))
          )
         c))
   )
  )
)

(defvar *numbers-xs-ys*
  (numbers-xs-ys 1 25)
)

;____________________________________
;

(defun harmonic-ser (fund-midic n-harmonics)
  "Harmonic series: midicents of the first N-HARMONICS harmonics of FUND-MIDIC."

  (let ((ffund (mc->f fund-midic)))

    (f->mc (arithm-ser ffund (* ffund n-harmonics) ffund))

    )
)
 
;============ extensions ==============;

(defmethod object-midi-tempo ((self eventmidi-seq))
;  (get-midievents self 
  (cadr (flat (tempo-evts (get-tempomap self)) 1))
)


;=========================================

;=========================================================

(defun t/nil-random ()
  "Randomly t or nil."

  (nth-random '(t nil))

)

(defun simple-color-bpf-from-list (x y mode decimals rgb)
  "Makes a BPF from lists X and Y with the given mode, decimals and colour (r g b 0..255)."

  (set-color (simple-bpf-from-list x y mode decimals) (make-color-255 (first rgb) (second rgb) (third rgb)))

)

(defun join-string-list (string-list)
  "Joins a list of strings with spaces."

    (format nil "~{~A~^ ~}" string-list))

;=========================================================

(defvar *digits*
  (make-array '(10 3)
              :element-type 'string
              :initial-contents
              '(
                ("|¯|"
                 "| |"
                 " ¯ ")
                ("  |"
                 "  |"
                 "   ")
                (" ¯|"
                 "|¯ "
                 " ¯ ")
                (" ¯|"
                 " ¯|"
                 " ¯ ")
                ("| |"
                 " ¯|"
                 "   ")
                ("|¯ "
                 " ¯|"
                 " ¯ ")
                ("|¯ "
                 "|¯|"
                 " ¯ ")
                (" ¯|"
                 "  |"
                 "   ")
                ("|¯|"
                 "|¯|"
                 " ¯ ")
                ("|¯|"
                 " ¯|"
                 " ¯ ")
                ))
)

(defun simple-digits (n)
  "Prints the number N in large digits in the Listener and returns N."

  (let* ((str-n (write-to-string n))
         (asc-str-n-list (loop for i from 0 below (length str-n)
                               collect (parse-integer (subseq str-n i (1+ i))))))
    (om-print (format nil "~%~{~48:@<~A~>~%~}" (loop for i from 0 to 2 collect (format nil "~{~A~^ ~}" (loop for j in asc-str-n-list collect (aref *digits* j i)))))))
  n
)

;====================================================================================

(defun find-closest (it seq key)
  "Element of SEQ whose KEY value is closest to IT."

  (let ((key-seq (funcall key (mat-trans seq))))

    (setf it (om-clip it (list-min key-seq) (list-max key-seq))
          seq (sort seq #'< :key #'(lambda (x) (funcall key x)))))

  (let* ((bel (find it seq :test #'>= :from-end t :key #'(lambda (x) (funcall key x))))
         (abv (find it seq :test #'<= :from-end nil :key #'(lambda (x) (funcall key x))))
         )

    (cond
     ((and (funcall key bel) (< (abs (- it (funcall key bel))) (abs (- (funcall key abv) it)))) bel)
     ((and (funcall key abv) (< (abs (- (funcall key abv) it)) (abs (- it (funcall key bel))))) abv)
     (t (nth-random (list bel abv))))

    )
)

(defun find-closest2 (it seq)
  "Number in SEQ closest to IT."

  (let* ((bel (find it (remove nil seq) :test #'>= :from-end t))
         (abv (find it (remove nil seq) :test #'<= :from-end nil))
         )

    (cond
     ((not bel) abv)
     ((not abv) bel)
     ((< (abs (- it bel)) (abs (- abv it))) bel)
     ((< (abs (- abv it)) (abs (- it bel))) abv)
     (t (nth-random (list bel abv))))

    )
)


(defun m-limit (z mn mx)
  "Keeps Z inside MN..MX (values outside are set just inside the borders)."
  (cond
   ((> z mx) (1- mx))
   ((< z mn) (1+ mn))
   (t z))
)

(defun aser (n)
  "List 0..N-1."

  (loop for i from 0 below n collect i)

)

(defun line-interpole (self n)
  "Resamples a list to N values with linear interpolation." 

    (interpole (aser (length self)) self 0 (1- (length self)) n)

)

(defun line-in-pic-interpole (self n)
  "Resamples every column of a pixel row to N values with linear interpolation." 
  (mat-trans
   (loop for s in (mat-trans self)
         collect (mapcar #'(lambda (x) (coerce x 'single-float)) (line-interpole s n)))
  )
)

(defun mtrans (matrix)
  "Transposes a matrix (list of rows)."

    (loop for i below (length (first matrix))
          collect (mapcar #'(lambda (list) (nth i list)) matrix))

)

(defun pic-bilinear-resampling (pic-pathname new-x new-y)
  "Resizes a picture to NEW-X by NEW-Y pixels with bilinear interpolation."

  (oa::pix-list-to-image
   (mapcar
    #'(lambda (x) (line-in-pic-interpole x new-x))
    (mat-trans
     (mapcar
      #'(lambda (y) (line-in-pic-interpole y new-y))
      (mat-trans
       (oa::image-to-arraylist
        (om-get-picture-file pic-pathname)))))))

)

(defun sinc (x)
  "Normalised sinc function: sin(pi x)/(pi x)."
  (if (zerop x)
      1
      (/ (sin (* pi x)) (* pi x)))
)

(defun spline-interpole  (list-x list-y nbsamples degree)
  "Spline interpolation of the points LIST-X, LIST-Y: NBSAMPLES values, spline of the given DEGREE."

  (let* ((points (mat-trans (list list-x list-y)))
         (n (1- (length points)))
         )
    (mapcar #'second (SplineCurve2D points n (SplineKnots n degree) degree nbsamples))
         )
)

(defun line-spline-interpole (self n degree)
  "Resamples a list to N values with a spline of the given DEGREE."

  (spline-interpole (aser (length self)) self n degree)
  
)

(defun line-in-pic-spline-interpole (self n degree)
  "Resamples every column of a pixel row to N values with a spline of the given DEGREE."

  (mat-trans
   (loop for s in (mat-trans self)
         collect (mapcar #'(lambda (x) (coerce x 'single-float)) (line-spline-interpole s n degree)))
  )

)

(defun pic-spline-resampling (pic-pathname new-x new-y degree)
  "Resizes a picture to NEW-X by NEW-Y pixels with spline interpolation of the given DEGREE."

  (oa::pix-list-to-image
   (mapcar
    #'(lambda (x) (line-in-pic-spline-interpole x new-x degree))
    (mat-trans
     (mapcar
      #'(lambda (y) (line-in-pic-spline-interpole y new-y degree))
      (mat-trans
       (oa::image-to-arraylist
        (om-get-picture-file pic-pathname)))))))

)


(defun pic-bicubic-resampling (pic-pathname new-x new-y)
  "Resizes a picture to NEW-X by NEW-Y pixels with bicubic interpolation."

  (pic-spline-resampling pic-pathname new-x new-y 3)

)

(defun round-to-multiple (n divisor)
  "Rounds N to the nearest multiple of DIVISOR."

  (* (round n divisor) divisor)

)

(defun floor-to-multiple (n divisor)
  "Rounds N down to a multiple of DIVISOR."

  (* (floor n divisor) divisor)

)

(defun ceiling-to-multiple (n divisor)
  "Rounds N up to a multiple of DIVISOR."

  (* (ceiling n divisor) divisor)

)

(defmethod! set-matrix-lmidic ((toler number) (n-harm number) (range list))
  :doc "The tension matrix: for each tension 0, 1, 2 ... the chord (midicents) of the skeleton of octaves and twelfths (12a + 19b semitones) anchored lower and lower, cut to <range>. <toler> - tolerance in cents for harmonics, <n-harm> - number of harmonics (default 2, 3, piano 2100..10800)."
            
            :initvals '(2 3 '(2100 10800))
            :indoc '("toler"  "n-harmonics" "range mc")
            :icon 9119

  (let* ((harm-ser (harmonic-ser 0 n-harm))
         (x-list (dx->x 0 (x->dx (approx-m (mapcar #'cadr (remove
                                                           toler
                                                           (mapcar #'list (om-abs (om- (approx-m harm-ser 2) harm-ser)) harm-ser)
                                                           :test #'< :key #'first)) 2))))
         (r-list (list (car x-list)))
         (dx-harm-pattern
          (labels ((remove-mod-oct (d-x-list)
                     (if (null d-x-list) (return-from remove-mod-oct (reverse r-list)))
                     (let ((car-d-x-list (car d-x-list)))
                       (push car-d-x-list r-list)
                       (remove-mod-oct (remove-if #'(lambda (x) (= (mod car-d-x-list 1200) (mod x 1200))) d-x-list)))))
            (om/ (x->dx (remove-mod-oct (cdr x-list))) 100)))
         mtrx
         (get-mtrx
          (labels ((make-chords-list (i n-harmonics)
                     (when (<= i 500)
                       (mapcar #'(lambda (n) (push n n-harmonics)) (reverse (dx->x i dx-harm-pattern)))
                       (let ((d-n-harmonics (sort (remove-duplicates n-harmonics) #'<)))
                         (if (= i 500) (return-from make-chords-list (remove 500 d-n-harmonics :test #'<)))
                         (make-chords-list (cadr (member i d-n-harmonics)) d-n-harmonics)))
                     ))

            (make-chords-list 0 nil)))
         (range-min (/ (list-min range) 100))
         (range-max (/ (list-max range) 100))
         )

    (loop for i from range-max by -1
          do (push (band-filter (om+ get-mtrx i) (list (list range-min range-max)) 'pass) mtrx)
          while (not (and (= (- (last-elem (first-n (car mtrx) 13)) (first (first-n (car mtrx) 13))) 12) (= (first (first-n (car mtrx) 13)) range-min))))

    (reverse (mapcar #'(lambda (x) (om* x 100)) mtrx))
    )
)



;;; Matryca liczona raz dla danych (toler n-harm range) i pamietana
;;; (uzywaja jej funkcje tension w conversions.lisp i tension-spiral.lisp).

(defvar *spiral-matrix-cache* nil)

(defun spiral-matrix (toler n-harm range)   ; ukryj
  "Result of set-matrix-lmidic, computed only on the first use of the same parameters."
  (let* ((key (list toler n-harm (copy-list range)))
         (hit (assoc key *spiral-matrix-cache* :test #'equal)))
    (if hit
        (cdr hit)
        (let ((m (set-matrix-lmidic toler n-harm range)))
          (push (cons key m) *spiral-matrix-cache*)
          m))))

(defun spiral-matrix-clear ()   ; ukryj
  "Czysci pamiec matryc (np. po zmianie set-matrix-lmidic)."
  (setf *spiral-matrix-cache* nil))


(defun random-rgb ()
  "Random colour (r g b) with values 0..255."

  (loop repeat 3
        collect (om-random 0 255))

)

(defun /phi (x)
  "X divided by phi."

(/ x *phi*)

)



;==================================================
; Kolor zapisany szesnastkowo -> (r g b)

(defun hex-digits-value (str start len)   ; ukryj
  (parse-integer str :start start :end (+ start len) :radix 16))

(defmethod! hex->rgb ((hex string) &key (range 1))
  :icon 9119
  :initvals '("#d4af37")
  :indoc '("hexadecimal colour, e.g. \"#d4af37\", \"d4af37\", \"#fa0\" or with alpha \"#d4af37cc\"")
  :doc "Converts a hexadecimal colour notation into a list (r g b).
Accepts: #rrggbb, rrggbb, the short form #rgb and versions with an alpha channel (#rrggbbaa, #rgba) - then the result is (r g b a).
<range> 1 (default): values 0..1 (as in the LUT functions and the PICTURE class);
        255: integers 0..255.
A list of colours can also be given - the result is a list of lists."
  (let* ((s (string-left-trim "#" (string-trim " " hex)))
         (n (length s))
         (vals (case n
                 ((6 8) (loop for i from 0 below n by 2 collect (hex-digits-value s i 2)))
                 ((3 4) (loop for i from 0 below n collect (* 17 (hex-digits-value s i 1))))
                 (t (om-beep-msg (format nil "hex->rgb: nieznany zapis koloru ~s" hex))
                    (return-from hex->rgb nil)))))
    (if (and (numberp range) (= range 255))
        vals
        (mapcar #'(lambda (v) (/ v 255.0)) vals))))

(defmethod! hex->rgb ((hex list) &key (range 1))
  :icon 9119
  :doc "HEX->RGB for a list of colours."
  (mapcar #'(lambda (h) (hex->rgb h :range range)) hex))

(defun hex->rgb-test ()   ; ukryj
  (let ((ok (and (equal (hex->rgb "#d4af37" :range 255) '(212 175 55))
                 (equal (hex->rgb "D4AF37" :range 255) '(212 175 55))
                 (equal (hex->rgb "#fa0" :range 255) '(255 170 0))
                 (equal (hex->rgb "#ffd700cc" :range 255) '(255 215 0 204))
                 (equal (hex->rgb '("#000" "#fff") :range 255) '((0 0 0) (255 255 255)))
                 (every #'(lambda (a b) (< (abs (- a b)) 1e-6))
                        (hex->rgb "#ff8000") '(1.0 0.5019608 0.0)))))
    (om-print (format nil "hex->rgb-test: ~a" (if ok "OK" "BLAD")))
    ok))

; (r g b) 0..1 -> kolor zapisany szesnastkowo

(defmethod! rgb->hex ((rgb list))
  :icon 9119
  :initvals '((0.886 0.737 0.502))
  :indoc '("colour as (r g b) or (r g b a), values 0..1")
  :doc "Converts a colour (r g b) with values 0..1 into hexadecimal notation \"#rrggbb\".
With an alpha channel (r g b a) the result is \"#rrggbbaa\". Values outside 0..1 are clipped.
A list of colours can also be given - the result is a list of strings.
Inverse of hex->rgb."
  (if (listp (car rgb))
      (mapcar #'rgb->hex rgb)
      (format nil "#~{~2,'0x~}"
              (mapcar #'(lambda (v) (round (* 255 (max 0 (min 1 v))))) rgb))))

(defun rgb->hex-test ()   ; ukryj
  (let ((ok (and (string-equal (rgb->hex '(1 0.5019608 0)) "#ff8000")
                 (string-equal (rgb->hex '(0.886 0.737 0.502)) "#e2bc80")
                 (string-equal (rgb->hex '(1 0.843 0 0.8)) "#ffd700cc")
                 (equal (mapcar #'string-downcase (rgb->hex '((0 0 0) (1 1 1)))) '("#000000" "#ffffff"))
                 (string-equal (rgb->hex '(1.2 -0.1 0)) "#ff0000")
                 (string-equal (rgb->hex (hex->rgb "#d4af37")) "#d4af37"))))
    (om-print (format nil "rgb->hex-test: ~a" (if ok "OK" "BLAD")))
    ok))

;===========================================
; KOMPILACJA BIBLIOTEKI
; OM laduje om-cywin ze zrodel .lisp, a funkcje sa wtedy interpretowane
; (wielokrotnie wolniejsze). OM nie ma kompilatora plikow (compile-file), ale ma
; kompilator funkcji (compile). Ta funkcja, wywolywana w om-cywin.lisp po
; wczytaniu wszystkich plikow, czyta wszystkie pliki zrodlowe i kompiluje w nich kazda definicje funkcji
; i metody (defun, defmethod, defmethod!, defmethod*). Inne formy (defparameter,
; defclass! ...) zostaja bez zmian. Blad kompilacji jednej definicji nie przerywa
; ladowania - ta definicja zostaje w wersji interpretowanej.

(defvar *om-cywin-compiling* nil)   ; defvar: ponowne wczytanie nie zeruje flagi

(defvar *om-cywin-compile-report* "om-cywin: kompilacja jeszcze sie nie odbyla")

(defparameter *om-cywin-compiled-forms* '(defun defmethod defmethod! defmethod*))

(defun om-cywin-compile-library ()   ; ukryj
  (unless *om-cywin-compiling*
    (let ((*om-cywin-compiling* t)
          ;; katalog sources/: wywolanie idzie z om-cywin.lisp (katalog biblioteki)
          (dir (let* ((base (make-pathname :name nil :type nil :version nil
                                           :defaults (or *load-truename* *load-pathname*)))
                      (d (pathname-directory base)))
                 (if (string-equal (car (last d)) "sources")
                     base
                     (make-pathname :directory (append d (list "sources")) :defaults base))))
          (t0 (get-internal-real-time))
          (ok 0) (failed nil))
      (dolist (src (sort (directory (make-pathname :name :wild :type "lisp" :defaults dir))
                         #'string< :key #'namestring))
        (with-open-file (in src :external-format :latin-1)
          (let ((*package* (find-package :om))
                (eof (list nil)))
            (loop for form = (handler-case (read in nil eof)
                               (error () eof))
                  until (eq form eof)
                  do (when (and (consp form)
                                (symbolp (car form))
                                (member (symbol-name (car form)) *om-cywin-compiled-forms*
                                        :key #'symbol-name :test #'string=))
                       (if (ignore-errors
                             (funcall (compile nil `(lambda () ,form)))
                             t)
                           (incf ok)
                           (push (format nil "~A/~A" (pathname-name src)
                                         (if (consp (cdr form)) (cadr form) "?"))
                                 failed)))))))
      (setf *om-cywin-compile-report*
            (format nil "om-cywin: skompilowano ~A definicji w ~,1F s~@[; bez kompilacji: ~{~A~^, ~}~]"
                    ok (/ (- (get-internal-real-time) t0) internal-time-units-per-second)
                    (reverse failed)))
      (om-print *om-cywin-compile-report*))))

(defun om-cywin-compile-report ()   ; ukryj
  "Report of the library compilation at OM startup (the Listener opens later)."
  (om-print *om-cywin-compile-report*)
  *om-cywin-compile-report*)

; Wywolanie jest w om-cywin.lisp, zaraz po wczytaniu plikow (dopisuje je
; generator make-my-library-menu).

;===========================================
; KOMPILACJA LATEK
; OM zamienia kazda latke (i kazda funkcje Lisp w latce) na funkcje Lispu przez
; eval, bez kompilacji - wiec jest ona interpretowana. Ponizej:
;  - metoda :after na compile-patch - latki z pudelek,
;  - nakladka na compile-lisp-patch-fun - funkcje Lisp (lispfunction, pliki .oml);
;    OM wywoluje ja przy wczytaniu i po kazdej edycji funkcji, z pominieciem compile-patch.
; Wylaczenie: (setf om::*compile-patches* nil).

(defvar *compile-patches* t)

(defun compile-patch-symbol (sym)   ; ukryj
  "Compiles the function SYM if it is interpreted (errors and warnings are ignored)."
  (when (and *compile-patches* sym (fboundp sym)
             (not (compiled-function-p (fdefinition sym))))
    (ignore-errors
      (handler-bind ((warning #'muffle-warning))
        (let ((*compile-print* nil) (*compile-verbose* nil))
          (compile sym))))))

(defmethod compile-patch :after ((self OMPatch))
  (compile-patch-symbol (ignore-errors (intern (string (code self)) :om))))

;; oryginal z OM zapamietany raz (ponowne wczytanie biblioteki nie owija go drugi raz)
(defvar *om-compile-lisp-patch-fun* (fdefinition 'compile-lisp-patch-fun))

(setf (fdefinition 'compile-lisp-patch-fun)
      #'(lambda (patch)
          (prog1 (funcall *om-compile-lisp-patch-fun* patch)
            (compile-patch-symbol (ignore-errors (intern (string (code patch)) :om))))))


;;; --------------------------- czcionki ---------------------------------------
;;; Przy kazdym ladowaniu biblioteki: Listener i okna tekstowe (TextFile, Edit,
;;; edytor Lisp) w czcionce o stalej szerokosci. Zmien wartosci ponizej wedlug potrzeb.

(defvar *my-listener-font* '("Andale Mono" 11))
(defvar *my-text-edit-font* '("Andale Mono" 14))

(defun om-cywin-set-fonts ()   ; ukryj
  "Sets the font of the Listener and text windows; also changes the open Listener."
  (ignore-errors
    (let ((lf (gp::make-font-description :family (first *my-listener-font*)
                                        :size (second *my-listener-font*) :weight :normal))
          (tf (gp::make-font-description :family (first *my-text-edit-font*)
                                        :size (second *my-text-edit-font*) :weight :normal)))
      (setf om-lisp::*listener-font* lf
            om-lisp::*def-text-edit-font* tf)
      (let ((l om-lisp::*om-listener*))
        (when l
          (capi::execute-with-interface
           l
           #'(lambda ()
               (ignore-errors
                 (dolist (pane (list (om-lisp::ip l) (om-lisp::op l)))
                   (when pane
                     (setf (capi::simple-pane-font pane) lf)
                     (let ((echo (ignore-errors (capi::editor-pane-echo-area pane))))
                       (when echo (setf (capi::simple-pane-font echo) lf))))))))))
      t)))

(om-cywin-set-fonts)
