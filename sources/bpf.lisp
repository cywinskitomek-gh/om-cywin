(in-package :om)

(defmethod! little-bpf ((bpf bpf) &optional (x-min 0) (x-max 46) (y-min 0) (y-max 9) (sleep 0) (round 1))
  :doc "Prints a small text plot of a BPF in the Listener (grid x-min..x-max by y-min..y-max)."

	:initvals '(nil 0 46 0 9 0 1)
	:icon 9117

  (let* ((range (om-round (list-min-max (y-points bpf)) round 1))
         (shift (1+ (max (length (write-to-string (first range))) (length (write-to-string (second range))))))
         (x-max-shift (1- (- x-max shift)))
         (bpf-scale-sample (om-sample (bpf-scale bpf :x1 x-min :x2 x-max-shift :y1 y-min :y2 y-max) (1+ (- x-max-shift x-min))))
         (pairs (mat-trans (om-round (list (om- y-max (y-points bpf-scale-sample)) (x-points bpf-scale-sample)))))
         (col (1+ (- x-max x-min)))
         (sheet (loop for y from y-min to y-max
                      collect (second (mat-trans (remove y pairs :test #'/= :key #'first)))))
         (legends (loop for y from y-min to y-max
                        collect (cond
                                 ((= y y-min) (format nil "~V,,,' <~D|~>" shift (second range)))
                                 ((= y y-max) (format nil "~V:<~D|~>" shift (first range)))
                                 (t (format nil "~V<|~>" shift)))))
         )

    (om-print (format nil "~%~V,,,'_<|_~>~V,,,' <_|_~>" (1+ shift) (- col shift)))

    (loop for l in sheet
          for i from 0
          for legend in legends
          do (princ legend *om-stream*)
             (loop for m from 0 below (- x-max shift)
                   do (princ (if (find m l) "¤" " ") *om-stream*))
             (if (= i y-max) (princ "|" *om-stream*))
             (terpri *om-stream*))          

    (om-print (format nil "~V,,,'¯<|~>~V,,,'¯<|¯~>" shift (1+ (- col shift))))
    (om-print (format nil "~V:<~>~V<~D~;~D~>" (- shift 2) (+ (- col shift) 3) (om-round (list-min (x-points bpf)) round 1) (om-round (list-max (x-points bpf)) round 1)))
    )
  (sleep sleep)
  (terpri *om-stream*)
  bpf
)

(defmethod! litle-bpf ((bpf bpf) &optional (x-min 0) (x-max 46) (y-min 0) (y-max 9) (sleep 0) (round 1)) ; ukryj

            	:initvals '(nil 0 46 0 9 0 1)
                :icon 122

            (little-bpf bpf x-min x-max y-min y-max sleep round)

)

#|(defmethod! litle-bpf-string ((bpf bpf) &optional (x-min 0) (x-max 47) (y-min 0) (y-max 9) (sleep 1/4) (round 1))

	:initvals '(nil 0 47 0 9 1/4 1)
	:icon 122

  (let* ((range (om-round (list-min-max (y-points bpf)) round 1))
         (shift (1+ (max (length (write-to-string (first range))) (length (write-to-string (second range))))))
         (x-max-shift (1- (- x-max shift)))
         (bpf-scale-sample (om-sample (bpf-scale bpf :x1 x-min :x2 x-max-shift :y1 y-min :y2 y-max) (1+ (- x-max-shift x-min))))
         (pairs (mat-trans (om-round (list (om- y-max (y-points bpf-scale-sample)) (x-points bpf-scale-sample)))))
         (col (1+ (- x-max x-min)))
         (sheet (loop for y from y-min to y-max
                      collect (second (mat-trans (remove y pairs :test #'/= :key #'first)))))
         (legends (loop for y from y-min to y-max
                        collect (cond
                                 ((= y y-min) (format nil "~V:<~D|~>" shift (second range)))
                                 ((= y y-max) (format nil "~V:<~D|~>" shift (first range)))
                                 (t (format nil "~V<|~>" shift)))))
         )

    


    (om-print (format nil "~%~V,,,'_<|~>~V,,,'_<|~>" shift (- col shift)))

    (loop for l in sheet
          for legend in legends
          do (princ legend *om-stream*)
          (loop for m from 0 to (- x-max shift 1)
                do (princ (if (find m l) "Ë‡" " ") *om-stream*))
                (princ "|" *om-stream*)
                (terpri *om-stream*))

    (om-print (format nil "~V,,,'Å»<|~>~V,,,'Å»<|~>" shift (- col shift)))
    (om-print (format nil "~V:<~>~V<~D~;~D~>" (1- shift) (1+ (- col shift)) (om-round (list-min (x-points bpf)) round 1) (om-round (list-max (x-points bpf)) round 1)))
    )
  (sleep sleep)
  (terpri *om-stream*)
  bpf
)|#

(defmethod! invert-bpf ((bpf bpf))
  :doc "Inverts a BPF vertically: y values are mirrored between their minimum and maximum. x and colour unchanged."

	:initvals '(nil)
	:icon 9117

  (let ((ypoints (y-points bpf)))

    (set-color (simple-bpf-from-list (x-points bpf) (om+ (list-min ypoints) (om- (list-max ypoints) ypoints)) 'bpf (decimals bpf)) (bpfcolor bpf))

  )
)


(defmethod! reverse-bpf ((bpf bpf))
  :doc "Reverses a BPF in time: x values are mirrored between their minimum and maximum. y and colour unchanged."

	:initvals '(nil)
	:icon 9117

  (let ((xpoints (x-points bpf)))

    (set-color (simple-bpf-from-list (om+ (list-min xpoints) (om- (list-max xpoints) xpoints)) (y-points bpf) 'bpf (decimals bpf)) (bpfcolor bpf))

  )
)


(defmethod! reverse/invert-bpf ((bpf bpf))
  :doc "Reverses a BPF in time and inverts it vertically (reverse-bpf + invert-bpf)."

	:initvals '(nil)
	:icon 9117

        (invert-bpf (reverse-bpf bpf))

)

(defun set-bpf-decimals (bpf dec)
  "Sets the precision (number of decimals) of a BPF and returns it."

   (setf (decimals bpf) dec)
   bpf

)

;;; --------------------------- losowe odchylenia y ----------------------------

(defun bpf-y-at-x (bpf x)   ; ukryj
  "Value of the BPF at X (linear; outside the range - the end value)."
  (let ((xs (x-points bpf)) (ys (y-points bpf)))
    (cond ((null (cdr xs)) (first ys))
          ((<= x (first xs)) (first ys))
          (t (loop for (x1 x2) on xs
                   for (y1 y2) on ys
                   when (and x2 (<= x x2))
                     return (if (= x1 x2) y2 (+ y1 (* (- y2 y1) (/ (- x x1) (- x2 x1)))))
                   finally (return (car (last ys))))))))

(defun bpf-random-deviation (dist s)   ; ukryj
  "Losowe odchylenie o rozkladzie DIST i odchyleniu standardowym S."
  (let ((s (float s 1d0)))
    (case dist
      (:uniform (* s (sqrt 3d0) (- (* 2 (random 1d0)) 1)))
      (:triangular (* s (sqrt 6d0) (- (random 1d0) (random 1d0))))
      (:laplace (let ((u (- (random 1d0) 0.5d0)))
                  (* -1 (/ s (sqrt 2d0)) (signum u) (log (max 1d-300 (- 1 (* 2 (abs u))))))))
      (t (* s (sqrt (* -2 (log (- 1 (random 1d0))))) (cos (* 2 pi (random 1d0))))))))

(defun bpf-param-at (v i u)   ; ukryj
  "Parameter for the I-th point (U = position 0..1 in the x range): a number, a list or a BPF
(the BPF stretched over the whole range, regardless of its own x)."
  (cond ((typep v 'bpf)
         (let ((vx (x-points v)))
           (bpf-y-at-x v (+ (first vx) (* u (- (car (last vx)) (first vx)))))))
        ((consp v) (nth (min i (1- (length v))) v))
        (t v)))

(defmethod! jitter-bpf ((bpf bpf) (sigma t) &key (dist :normal) (limit nil) (clip nil) (decimals nil))
  :icon 9117
  :numouts 2
  :initvals '(nil 1)
  :indoc '("BPF" "sigma - standard deviation (a number, a list for consecutive points or a BPF)")
  :outdoc '("BPF with deviated y" "list of deviations")
  :doc "Returns a copy of <bpf> in which every y value is randomly deviated.
<sigma> - standard deviation in y units: a number (the same everywhere),
          a list (for consecutive points, the last one repeated) or a BPF (sigma depending on x;
          the BPF is stretched over the whole x range of <bpf>, so its own x values do not matter).
<dist> - distribution of the deviations, always with the same standard deviation sigma
         (the shape changes, not the spread):
         :normal - Gaussian (default), small deviations most frequent, large ones rare;
         :uniform - every deviation within +/- 1.73 sigma equally probable;
         :triangular - +/- 2.45 sigma, most often close to zero;
         :laplace - sharp peak at zero and long tails (rare large jumps).
<limit> - nil or k: the deviation is clipped to +/- k*sigma (e.g. 2 = no extreme jumps).
<clip> - nil or (min max): the resulting y is clipped to this range.
<decimals> - precision of the result; nil = as in <bpf> (note: with 0 and sigma < 0.5 the deviations
             round to zero).
x and colour unchanged."
  (let* ((xs (x-points bpf))
         (x0 (first xs)) (span (- (car (last xs)) x0))
         (devs (loop for x in xs for i from 0
                     collect (let* ((s (max 0 (bpf-param-at sigma i (if (zerop span) 0 (/ (- x x0) span)))))
                                    (d (bpf-random-deviation dist s)))
                               (if limit (max (- (* limit s)) (min (* limit s) d)) d))))
         (ys (loop for y in (y-points bpf) for d in devs
                   collect (let ((v (+ y d)))
                             (if clip (max (first clip) (min (second clip) v)) v))))
         (dec (or decimals (decimals bpf))))
    (values (set-color (simple-bpf-from-list (copy-list xs) ys 'bpf dec) (bpfcolor bpf))
            devs)))

(defun jitter-bpf-test ()   ; ukryj
  (flet ((near (a b eps) (< (abs (- a b)) eps))
         (stats (l) (let* ((n (length l)) (m (/ (reduce #'+ l) n)))
                      (list m (sqrt (/ (reduce #'+ (mapcar #'(lambda (x) (expt (- x m) 2)) l)) n))))))
    (let* ((n 4000)
           (b (simple-bpf-from-list (loop for i below n collect i) (make-list n :initial-element 50) 'bpf 4))
           (checks
            (list
             (list "sigma 0" (every #'= (y-points (jitter-bpf b 0)) (y-points b)))
             (list "normal" (destructuring-bind (m s) (stats (nth-value 1 (jitter-bpf b 2)))
                              (and (near m 0 0.15) (near s 2 0.15))))
             (list "uniform" (let ((d (nth-value 1 (jitter-bpf b 2 :dist :uniform))))
                               (and (every #'(lambda (x) (<= (abs x) (* 2 (sqrt 3d0)))) d)
                                    (near (second (stats d)) 2 0.15))))
             (list "triangular" (near (second (stats (nth-value 1 (jitter-bpf b 2 :dist :triangular)))) 2 0.15))
             (list "laplace" (near (second (stats (nth-value 1 (jitter-bpf b 2 :dist :laplace)))) 2 0.25))
             (list "limit" (every #'(lambda (x) (<= (abs x) 2)) (nth-value 1 (jitter-bpf b 2 :limit 1))))
             (list "clip" (every #'(lambda (y) (<= 49 y 51)) (y-points (jitter-bpf b 5 :clip '(49 51)))))
             (list "sigma lista" (every #'zerop (nth-value 1 (jitter-bpf (simple-bpf-from-list '(0 1 2) '(1 2 3) 'bpf 2) '(0 0 0)))))
             (list "sigma BPF" (let* ((sb (simple-bpf-from-list '(0 0.5 1) '(5 0 5) 'bpf 2))
                                      (d (nth-value 1 (jitter-bpf (simple-bpf-from-list '(0 50 100) '(1 2 3) 'bpf 2) sb))))
                                 (zerop (second d))))
             (list "x bez zmian" (every #'= (x-points (jitter-bpf b 3)) (x-points b)))))
           (bad (remove-if #'second checks)))
      (dolist (c bad) (om-print (format nil "jitter-bpf-test: BLAD w: ~A" (first c))))
      (null bad))))


;;; --------------------------- suma BPF-ow -----------------------------------

(defun bpf+-rescale (v a b ta tb)   ; ukryj
  "V z zakresu A..B przeniesione liniowo na TA..TB (A = B - bez zmian)."
  (if (= a b) v (+ ta (* (- v a) (/ (- tb ta) (- b a))))))

(defun bpf+-sampler (b)   ; ukryj
  "Function x -> y for the BPF B (linear interpolation, binary search; outside the range the end y)."
  (let* ((xv (coerce (x-points b) 'simple-vector))
         (yv (coerce (y-points b) 'simple-vector))
         (n (length xv)))
    #'(lambda (x)
        (cond ((= n 1) (svref yv 0))
              ((<= x (svref xv 0)) (svref yv 0))
              ((>= x (svref xv (1- n))) (svref yv (1- n)))
              (t (let ((lo 0) (hi (1- n)))
                   (loop while (> (- hi lo) 1)
                         do (let ((mid (floor (+ lo hi) 2)))
                              (if (<= (svref xv mid) x) (setf lo mid) (setf hi mid))))
                   (let ((x1 (svref xv lo)) (x2 (svref xv hi)) (y1 (svref yv lo)) (y2 (svref yv hi)))
                     (if (= x1 x2) y2 (+ y1 (* (- y2 y1) (/ (- x x1) (- x2 x1))))))))))))

(defmethod! bpf+ ((bpfs list) &key (scale-x t) (scale-y t) (scale-result t) (decimals nil)
                  (x-range nil) (y-range nil))
  :icon 9117
  :numouts 2
  :initvals '(nil)
  :indoc '("list of BPFs (or a bpf-lib)")
  :outdoc '("BPF - the sum" "list of the BPFs after scaling (before summing)")
  :doc "Sums the y values of several BPFs.
<scale-x> t - the x of every BPF stretched to the x range of the BPF with the widest x span
          (all start and end together); nil - x unchanged.
<scale-y> t - the y of every BPF stretched to the y range (min..max) of the BPF with the widest
          y span; nil - y unchanged. A flat (constant) BPF stays unchanged.
<x-range> - nil or (from to): own target x range instead of the widest one (e.g. (0 100)).
<y-range> - nil or (from to): own target y range instead of the widest one (e.g. (0 1)).
Negative x and y values are fine - scaling is linear, the minimum goes to the start
of the range, the maximum to the end.
<scale-result> t - the sum mapped back onto that widest y range
          (the result has the same span as the widest BPF); nil - raw sum.
The sum is computed at the x points of all BPFs (after scaling), with linear interpolation;
outside a BPF's range its end value is used.
<decimals> - precision of the result; nil = the highest of the BPFs. Colour as in the first BPF."
  (let* ((bpfs (remove-if-not #'(lambda (b) (typep b 'bpf)) bpfs)))
    (when bpfs
      (let* ((xr (mapcar #'(lambda (b) (list (first (x-points b)) (car (last (x-points b))))) bpfs))
             (yr (mapcar #'(lambda (b) (list (reduce #'min (y-points b)) (reduce #'max (y-points b)))) bpfs))
             (bx0 (nth (position (reduce #'max (mapcar #'(lambda (r) (- (second r) (first r))) xr))
                                (mapcar #'(lambda (r) (- (second r) (first r))) xr))
                      xr))
             (by0 (nth (position (reduce #'max (mapcar #'(lambda (r) (- (second r) (first r))) yr))
                                (mapcar #'(lambda (r) (- (second r) (first r))) yr))
                      yr))
             (bx (or x-range bx0))
             (by (or y-range by0))
             (dec (or decimals (reduce #'max (mapcar #'decimals bpfs))))
             (scaled (loop for b in bpfs for r in xr for q in yr
                           collect (simple-bpf-from-list
                                    (if scale-x
                                        (mapcar #'(lambda (x) (bpf+-rescale x (first r) (second r) (first bx) (second bx))) (x-points b))
                                        (copy-list (x-points b)))
                                    (if scale-y
                                        (mapcar #'(lambda (y) (bpf+-rescale y (first q) (second q) (first by) (second by))) (y-points b))
                                        (copy-list (y-points b)))
                                    'bpf 10)))
             (xs (let ((all (sort (loop for b in scaled append (copy-list (x-points b))) #'<)) (out nil))
                   (dolist (x all (nreverse out))
                     (unless (and out (= x (first out))) (push x out)))))
             (fns (mapcar #'bpf+-sampler scaled))
             (sums (mapcar #'(lambda (x) (loop for f in fns sum (funcall f x))) xs))
             (ys (if (and scale-result (cdr bpfs))
                     (let ((lo (reduce #'min sums)) (hi (reduce #'max sums)))
                       (mapcar #'(lambda (s) (bpf+-rescale s lo hi (first by) (second by))) sums))
                     sums)))
        (values (set-color (simple-bpf-from-list xs ys 'bpf dec) (bpfcolor (first bpfs)))
                scaled)))))

(defmethod! bpf+ ((bpfs bpf-lib) &key (scale-x t) (scale-y t) (scale-result t) (decimals nil)
                  (x-range nil) (y-range nil))
  :icon 9117
  :doc "BPF+ for a bpf-lib: sums the BPFs of the library (see bpf+ for a list of BPFs)."
  (bpf+ (bpf-list bpfs) :scale-x scale-x :scale-y scale-y :scale-result scale-result :decimals decimals
                        :x-range x-range :y-range y-range))

(defun bpf+-test ()   ; ukryj
  (flet ((same (a b) (every #'(lambda (p q) (< (abs (- p q)) 1d-6)) a b)))
    (let* ((a (simple-bpf-from-list '(0 10) '(0 10) 'bpf 4))
           (b (simple-bpf-from-list '(0 1) '(0 1) 'bpf 4))
           (checks
            (list
             (list "skalowanie x i y" (let ((r (bpf+ (list a b) :scale-result nil)))
                                        (and (same (x-points r) '(0 10)) (same (y-points r) '(0 20)))))
             (list "wynik w zakresie" (let ((r (bpf+ (list a b))))
                                        (same (y-points r) '(0 10))))
             (list "wlasne zakresy" (let ((r (bpf+ (list a b) :x-range '(-5 5) :y-range '(-1 1))))
                                      (and (same (x-points r) '(-5 5)) (same (y-points r) '(-1 1)))))
             (list "probkowanie" (let ((f (bpf+-sampler (simple-bpf-from-list '(-10 0 10) '(-4 0 8) 'bpf 4))))
                                   (same (mapcar f '(-20 -5 0 5 20)) '(-4 -2 0 4 8))))
             (list "bez skalowania" (let ((r (bpf+ (list a b) :scale-x nil :scale-y nil :scale-result nil)))
                                      (and (same (x-points r) '(0 1 10)) (same (y-points r) '(0 2 11)))))))
           (bad (remove-if #'second checks)))
      (dolist (c bad) (om-print (format nil "bpf+-test: BLAD w: ~A" (first c))))
      (null bad))))


;;; --------------------------- BPF ze szkicownika (iPhone) -------------------

(defun bpf-pad-parse (text)   ; ukryj
  "From the sketchbook text: (values xs ys decimals). The first two lists of numbers = x and y;
a single list = y only (x = 0, 1, 2 ...). Comments ; skipped."
  (let ((dec (let ((p (search "precyzja" text)))
               (when p (ignore-errors (parse-integer text :start (+ p 8) :junk-allowed t)))))
        (lists nil))
    (with-input-from-string (in text)
      (let ((*read-eval* nil) (eof (list nil)))
        (loop for form = (handler-case (read in nil eof) (error () eof))
              until (or (eq form eof) (= (length lists) 2))
              do (when (and (consp form) (every #'numberp form)) (push form lists)))))
    (setf lists (nreverse lists))
    (cond ((= (length lists) 2) (values (first lists) (second lists) dec))
          ((= (length lists) 1) (values (loop for i below (length (first lists)) collect i) (first lists) dec))
          (t (values nil nil dec)))))

(defmethod! bpf-from-text ((text t) &key (decimals nil) (color nil))
  :icon 9117
  :initvals '(nil)
  :indoc '("text from the BPF Sketchbook (text-box / textfile), a file or nil")
  :doc "BPF from text copied from the BPF Sketchbook (a web page for iPhone).
<text>: text (e.g. from a text-box or textfile box), the path of a .txt file
or nil = file dialog. Format: two lists of numbers - x and y, e.g.
;; bpf-pad: center (precision 0)
(0 1 2 ...)
(6000 6040 ...)
A single list = y only (x = 0, 1, 2 ...).
<decimals> BPF precision; nil = the one stored in the text (default 0).
<color> nil or a colour, e.g. from hex->rgb / make-color-255."
  (let* ((str (cond ((stringp text)
                     (if (and (not (find #\( text)) (not (find #\Newline text)) (ignore-errors (probe-file text)))
                         (bpf-pad-read-file text) text))
                    ((pathnamep text) (bpf-pad-read-file text))
                    ((null text)
                     (let ((f (om-choose-file-dialog :prompt "Plik ze Szkicownika BPF")))
                       (and f (bpf-pad-read-file f))))
                    ((listp text) (format nil "~{~A~^ ~}" text))
                    (t (format nil "~A" text)))))
    (when str
      (multiple-value-bind (xs ys dec) (bpf-pad-parse str)
        (if (and xs ys (= (length xs) (length ys)))
            (let* ((d0 (or decimals dec 0))
                   ;; precyzja BPF dotyczy tez x: podnies ja, az x po zaokragleniu beda rosnace
                   (d (loop for d from d0 to 10
                            when (let ((r (mapcar #'(lambda (x) (/ (round (* x (expt 10 d))) (expt 10 d))) xs)))
                                   (every #'< r (cdr r)))
                              return d
                            finally (return 10)))
                   (b (simple-bpf-from-list xs ys 'bpf d)))
              (when (> d d0)
                (om-print (format nil "bpf-from-text: precyzja podniesiona z ~D do ~D, zeby x sie nie powtarzaly" d0 d)))
              (when color (setf (bpfcolor b) color))
              b)
            (progn (om-print "bpf-from-text: nie znaleziono dwoch list liczb (x i y) tej samej dlugosci") nil))))))

(defun bpf-pad-read-file (path)   ; ukryj
  (with-open-file (in path :external-format :utf-8 :element-type 'character)
    (let ((s (make-string (file-length in) :element-type 'character)))
      (subseq s 0 (read-sequence s in)))))

(defun bpf-from-text-test ()   ; ukryj
  (let* ((txt (format nil ";; bpf-pad: center  (precyzja 2)~%(0 50 100)~%(1.5 2.25 3)~%"))
         (b (bpf-from-text txt))
         (b1 (bpf-from-text "(5 6 7)"))
         (checks
          (list (list "dwie listy" (and b (every #'= (x-points b) '(0 50 100)) (every #'= (y-points b) '(1.5 2.25 3))))
                (list "precyzja" (and b (= (decimals b) 2)))
                (list "jedna lista" (and b1 (every #'= (x-points b1) '(0 1 2)) (every #'= (y-points b1) '(5 6 7))))
                (list "zly tekst" (null (bpf-from-text "bez liczb")))
                (list "gesty x" (let ((g (bpf-from-text (format nil ";; (precyzja 0)~%(0 0.4 0.8 1.2)~%(1 2 3 4)~%"))))
                                  (and g (= (length (x-points g)) 4) (= (decimals g) 1))))))
         (bad (remove-if #'second checks)))
    (dolist (c bad) (om-print (format nil "bpf-from-text-test: BLAD w: ~A" (first c))))
    (null bad)))
