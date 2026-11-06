(in-package :om)

;;; ==========================================================================
;;;  tension-spiral.lisp  -  spirala napiecia (Alfabet Torunski)
;;;
;;;  Domyslnie (od 28.10.2026) odcinek 113..244 = 132 punkty (22 x 6: litery rowno co 6 krokow),
;;;  igla 66 dokladnie naprzeciw (wczesniej 120..239);
;;;  punkty odniesienia z matrycy: pentatonika 113, klaster 246 (tension-octave-landmarks).
;;;  Odcinek prostej tension od pierwszej pentatoniki (113) do pierwszego klastra
;;;  12-dzwiekowego (246) w oktawie C3-B3 (6000-7100), zwiniety raz w kolo barw:
;;;  134 punkty (113..246, po 246 znowu 113). Kazda litera to igla kompasu:
;;;  biegun 1 (grot, wazniejszy) pod barwa litery, biegun 2 naprzeciw - zawsze
;;;  67 krokow dalej po kole. Na kole jest jeden szew (246 | 113) przy A; igla
;;;  przechodzaca przez szew odwraca sie (biegun 2 raz wyzej, raz nizej) - jak
;;;  na wstedze Moebiusa. Bieguny maja rozne wagi (jak barwa litery i barwa
;;;  przeciwna w LUT-ach: sat-letter 1.40 / sat-opposite 1.25 przy sat-other 0.9,
;;;  czyli podbicia 0.5 : 0.35 = 1 : 0.7).
;;;  Litery maja barwy z palety cyklu (22 litery co 360/22 st., A = 352.8 st.).
;;;
;;;  Trzy modele (parametr :model) do porownania:
;;;   :circle     - kolo 113..246 ze szwem przy A, igla 67, odwraca sie przy L,
;;;   :interleave - przeplot: litery ida w gore co drugi wiersz i wracaja po
;;;                 pominietych, bez duzych skokow; igla 67 (w gore, gdy sie miesci),
;;;   :helix      - spirala: biegun glowny rosnie plynnie od A do Z, alfabet jest
;;;                 otwarta droga (A i Z sasiaduja tylko w barwie); igla zawsze
;;;                 w te sama strone - :needle :up (obrot 118, igla 59, do 285)
;;;                 albo :down (obrot 134, igla 67, biegun 2 ponizej pentatoniki).
;;; ==========================================================================

(defparameter *spiral-letters* "ABCDEFGHIJKLMNOPRSTUWZ")   ; 22 litery palety

;; domyslne przesuniecie biegunow (:shift) we wszystkich funkcjach spirali i eksportu do Kola tension;
;; zmiana tutaj zmienia je wszedzie naraz (31.10.2026: -2; od 02.11.2026: 0 - barwa A 352.8 st. jest juz
;; wliczona w TENSION-HUE, wiec -2 przesuwalo bieguny niepotrzebnie)
(defparameter *spiral-shift* 0)

;; polskie litery z ogonkami -> litera podstawowa (kody Unicode, plik jest w latin-1)
(defparameter *spiral-diacritics*
  '((260 . #\A) (262 . #\C) (280 . #\E) (321 . #\L) (323 . #\N)
    (211 . #\O) (346 . #\S) (377 . #\Z) (379 . #\Z)))

(defun spiral-letter-index (letter)   ; ukryj
  "Index 0..21 of the letter in the palette (A = 0); letters with diacritics as their base letters."
  (let* ((c (char-upcase (character (if (stringp letter) (char letter 0) letter))))
         (base (or (cdr (assoc (char-code c) *spiral-diacritics*)) c)))
    (or (position base *spiral-letters*)
        (error "Litera ~A nie nalezy do palety Alfabetu" letter))))

(defun spiral-letter-hue (k)   ; ukryj
  "Hue (degrees) of the letter with index K - as letterHue in Remotion (palette.ts)."
  (let ((step (/ 360d0 22)))
    (mod (- 361d0 (* k step) (/ step 2)) 360d0)))

(defun spiral-circle-size (lo hi)   ; ukryj
  (1+ (- hi lo)))

(defmethod! tension-hue ((tension number) &key (lo 113) (hi 244) (direction 1) (align-hue nil))
  :icon 9106
  :initvals '(113)
  :indoc '("tension")
  :doc "Hue (degrees 0..360) for a tension on the wheel <lo>..<hi> (113..246 = from the pentatonic
to the cluster). <lo> has the hue of letter A; <direction> 1 = the next letters of the alphabet go up
the segment, -1 = down. A tension outside the segment wraps around the wheel.
<align-hue> t = hue straight from the tension: hue = (tension mod n) * 360/n, n = hi-lo+1."
  (let ((n (spiral-circle-size lo hi)))
    (if align-hue
        (mod (* (mod tension n) (/ 360d0 n)) 360d0)
    (mod (- (spiral-letter-hue 0) (* direction (- tension lo) (/ 360d0 n))) 360d0))))

(defmethod! tension-hue ((tension list) &key (lo 113) (hi 244) (direction 1) (align-hue nil))
  :icon 9106
  :doc "TENSION-HUE for a list of tensions."
  (mapcar #'(lambda (x) (tension-hue x :lo lo :hi hi :direction direction :align-hue align-hue)) tension))

(defun spiral-default-turn (model needle lo hi)   ; ukryj
  (if (and (eq model :helix) (eq needle :up)) 118 (spiral-circle-size lo hi)))

(defun spiral-letter-poles-raw (k lo hi direction &optional (model :circle) (needle :up) turn align-hue)   ; ukryj
  "Poles of letter K: (pole-1 pole-2), integers, according to MODEL."
  (let* ((n (spiral-circle-size lo hi))
         (kk (mod (* direction k) 22)))            ; kolejnosc liter po kole
    (when align-hue
      ;; barwa wyznacza tension wprost: t = barwa * n/360 (mod n), w oknie lo..hi
      (flet ((place (h) (+ lo (mod (- (round (* h n) 360) lo) n))))
        (let ((h (spiral-letter-hue k)))
          (return-from spiral-letter-poles-raw
            (list (place h) (place (mod (+ h 180d0) 360d0)))))))
    (ecase model
      (:circle
       (let ((pos (mod (round (* direction k n) 22) n)))
         (list (+ lo pos) (+ lo (mod (+ pos (floor n 2)) n)))))
      (:interleave
       ;; 22 szczebli lo + round(m*n/22); w gore parzyste m, w dol nieparzyste
       (let* ((m (if (<= kk 10) (* 2 kk) (1+ (* 2 (- 21 kk)))))
              (p1 (+ lo (round (* m n) 22)))
              (half (floor n 2)))
         (list p1 (if (<= (+ p1 half) hi) (+ p1 half) (- p1 half)))))
      (:helix
       (let* ((tr (or turn (spiral-default-turn model needle lo hi)))
              (p1 (+ lo (round (* kk tr) 22)))
              (half (floor tr 2)))
         (list p1 (if (eq needle :down) (- p1 half) (+ p1 half))))))))

(defmethod! letter-tension-poles ((letter t) &key (lo 113) (hi 244) (direction 1) (weights '(1 0.7))
                                  (model :circle) (needle :up) (turn nil) (align-hue nil)
                                  (toler 2) (n-harm 3) (range '(2100 10800)) (shift *spiral-shift*))
  :icon 9106
  :numouts 4
  :initvals '("A")
  :indoc '("letter (e.g. \"A\", \"S\", \"Z\") or a list of letters")
  :outdoc '("tension poles (pole-1 pole-2)" "pole weights" "pole hues in degrees"
            "matrix notes for the poles (midicents)")
  :doc "Compass needle of a letter on the tension wheel <lo>..<hi> (default 113..244 = 132 points, letters evenly every 6 steps; landmarks
from the matrix: first pentatonic 113, first cluster 246 in C3-B3).
Pole 1 (the arrowhead) lies under the letter's hue, pole 2 opposite (hue + 180 deg.), always
half the wheel away (66 steps for 113..244). The poles are not equal: <weights> (default 1 and 0.7,
like the boost of the letter's hue and of the opposite hue in the LUTs).
E.g. for 113..244: A 113 -> 179, L (opposite A) 179 -> 113.
<direction> 1 = the next letters go up the segment, -1 = down.
<model> :circle (a circle with a seam), :interleave, :helix (spiral);
<needle> for :helix - :up (needle up) or :down; <turn> for :helix
- length of one turn (nil = 118 for :up, 134 for :down).
<align-hue> t = the hue determines the tension directly: tension = hue * n/360 (mod n), n = number
of wheel points (hi-lo+1), placed in the window <lo>..<hi>; 0 deg. (red) = a multiple of n.
Then direction and model do not matter. E.g. for 118..237: A 118 -> 178, B 232 -> 172.
<toler> <n-harm> <range> - as in set-matrix-lmidic.
<shift> - integer added to the poles (om+), default *spiral-shift* (0); the matrix notes
are taken for the shifted poles, hues unchanged.
For a list of letters every output is a list of results."
  (if (and (listp letter) letter)
      (let ((r (mapcar #'(lambda (l)
                           (multiple-value-list
                            (letter-tension-poles l :lo lo :hi hi :direction direction :weights weights
                                                    :model model :needle needle :turn turn :align-hue align-hue
                                                    :toler toler :n-harm n-harm :range range :shift shift)))
                       letter)))
        (values (mapcar #'first r) (mapcar #'second r) (mapcar #'third r) (mapcar #'fourth r)))
      (let* ((k (spiral-letter-index letter))
             (poles (mapcar #'(lambda (p) (+ p shift))
                            (spiral-letter-poles-raw k lo hi direction model needle turn align-hue)))
             (matrix (spiral-matrix toler n-harm range))
             (h (spiral-letter-hue k)))
        (values poles
                weights
                (list h (mod (+ h 180d0) 360d0))
                (mapcar #'(lambda (p) (copy-list (nth p matrix))) poles)))))

(defmethod! spiral-letters-table (&key (lo 113) (hi 244) (direction 1) (model :circle) (needle :up) (turn nil) (align-hue nil) (shift *spiral-shift*))
  :icon 9106
  :doc "Table for the 22 letters of the palette: (letter pole-1 pole-2 hue), also printed
in the Listener - for comparing directions and models (:circle :interleave :helix).
<shift> - integer added to the poles (om+), default *spiral-shift* (0)."
  (let ((rows (loop for k from 0 below 22
                    collect (destructuring-bind (p1 p2) (spiral-letter-poles-raw k lo hi direction model needle turn align-hue)
                              (list (string (char *spiral-letters* k)) (+ p1 shift) (+ p2 shift) (round (spiral-letter-hue k)))))))
    (om-print (format nil "~:[model ~A~@[ (igla ~A)~]~;barwa = tension~*~*~], tension ~A..~A, kierunek ~A~[~:;, przesuniecie ~:*~@D~]~%~{~{~A: ~A -> ~A  (~A st.)~}~^~%~}"
                      align-hue model (and (eq model :helix) needle) lo hi direction shift rows))
    rows))

;;; --------------------------- wybor akordow --------------------------------
;;; Matryca: spiral-matrix (utils.lisp, przy set-matrix-lmidic).

;;; Tension wokol bieguna - rozklad rownomierny; dzwieki z akordu matrycy - wagi
;;; Gaussa po rejestrze (ciagly rozklad normalny, wiersz trojkata Pascala albo
;;; dowolna lista wag), losowanie bez powtorzen.

(defun spiral-pick-index (weights)   ; ukryj
  "Index drawn with the weights WEIGHTS (negative ones treated as 0)."
  (let* ((weights (mapcar #'(lambda (w) (max 0 w)) weights))
         (total (reduce #'+ weights))
         (u (random (float (if (> total 0) total 1) 1d0)))
         (acc 0))
    (loop for w in weights
          for i from 0
          do (incf acc w)
          when (< u acc) return i
          finally (return (1- (length weights))))))

(defun spiral-gauss-weights (pitches center sigma)   ; ukryj
  "Wagi rozkladu normalnego wokol CENTER (midicenty) o odchyleniu SIGMA (midicenty)."
  (let ((s (float (max 1 sigma) 1d0)))
    (mapcar #'(lambda (m) (exp (- (/ (expt (- m center) 2) (* 2 s s))))) pitches)))

(defun spiral-pick-without-replacement (items weights n)   ; ukryj
  "N roznych elementow z ITEMS, losowanych kolejno z wagami WEIGHTS."
  (let ((items (copy-list items)) (weights (copy-list weights)) (out nil))
    (loop repeat (min n (length items))
          do (let ((i (if (every #'zerop weights)
                          (random (length items))
                          (spiral-pick-index weights))))
               (push (nth i items) out)
               (setf items (append (subseq items 0 i) (nthcdr (1+ i) items))
                     weights (append (subseq weights 0 i) (nthcdr (1+ i) weights)))))
    out))

(defun spiral-gauss-pick (pitches n center sigma)   ; ukryj
  "N different notes from PITCHES, one by one, with Gaussian weights computed relative to the nearest
remaining note (which always has weight 1), so with a small SIGMA it picks the notes nearest to
CENTER instead of drawing blindly."
  (let ((items (copy-list pitches)) (out nil)
        (s2 (* 2 (expt (float (max 1 sigma) 1d0) 2))))
    (loop repeat (min n (length items))
          do (let* ((d (mapcar #'(lambda (m) (expt (- m center) 2)) items))
                    (dmin (reduce #'min d))
                    (i (spiral-pick-index (mapcar #'(lambda (x) (exp (- (/ (- x dmin) s2)))) d))))
               (push (nth i items) out)
               (setf items (append (subseq items 0 i) (nthcdr (1+ i) items)))))
    out))

(defun spiral-band-filter (pitches weights center half)   ; ukryj
  "Notes (with weights) in the band CENTER +/- HALF, the others dropped.
Returns two lists: notes in ascending order and their weights. Empty band - the nearest note."
  (let* ((lo (- center half)) (hi (+ center half))
         (inside (loop for p in pitches for w in weights
                       when (<= lo p hi) collect (cons p w)))
         (pairs inside))
    (if pairs
        (values (mapcar #'car pairs) (mapcar #'cdr pairs))
        (let ((best (loop with b = 0 for p in pitches for i from 0
                          when (< (abs (- p center)) (abs (- (nth b pitches) center))) do (setf b i)
                          finally (return b))))
          (values (list (nth best pitches)) (list (nth best weights)))))))

(defmethod! gauss-pick-pitches ((lmidic list) (n integer) &key (center 6000) (sigma 1200) (weights :normal)
                                (band 1))
  :icon 9106
  :initvals '((4800 5500 6000 6200 6700 7100 7600) 3)
  :indoc '("notes (midicents), e.g. a chord from the matrix" "how many notes to pick")
  :doc "Picks <n> different notes from <lmidic>, at random, weighted by register.
<weights> :normal (default) - normal distribution around <center> (midicents) with standard deviation
          <sigma> (midicents; 1200 = an octave),
          :pascal - a row of Pascal's triangle over the consecutive notes (peak in the middle of the chord),
          a list of numbers - own weights for the consecutive notes (from the lowest).
<band> - band of allowed notes: center +/- band*sigma (1 = +/- sigma, 2 = +/- 2 sigma);
          nil = no band (the whole chord).
          Notes outside the band are dropped (not transposed by octaves - that would change the tension),
          so the chord may have fewer than n notes.
When no note lies in the band, one remains - the one closest to center.
Returns the chosen notes in ascending order. n >= number of notes = all."
  (let* ((pitches (sort (remove-duplicates (copy-list lmidic) :test #'=) #'<))
         (w (cond ((eq weights :pascal) (pascals-triangle-r (length pitches)))
                  ((consp weights) (append weights
                                           (make-list (max 0 (- (length pitches) (length weights)))
                                                      :initial-element 0)))
                  (t (spiral-gauss-weights pitches center sigma)))))
    (when (and band pitches)
      (multiple-value-setq (pitches w)
        (spiral-band-filter pitches (subseq w 0 (length pitches)) center (* band (max 0 sigma)))))
    (sort (if (or (eq weights :pascal) (consp weights))
              (spiral-pick-without-replacement pitches (subseq w 0 (length pitches)) n)
              (spiral-gauss-pick pitches n center sigma))
          #'<)))

(defun spiral-gauss-offset (width k)   ; ukryj
  "Random offset from a normal distribution (sigma = k * width), rounded and clipped to -width..width."
  (if (<= width 0)
      0
      (let* ((u1 (max 1d-12 (random 1d0)))
             (u2 (random 1d0))
             (z (* (sqrt (* -2d0 (log u1))) (cos (* 2d0 pi u2)))))
        (max (- width) (min width (round (* z k width)))))))

(defmethod! pick-pole-tension ((poles list) &key (weights '(1 0.7)) (width 3) (offset nil) (offset-sigma 0.5) (tmin 0) (tmax 285))
  :icon 9106
  :numouts 3
  :initvals '((113 179))
  :indoc '("tension poles (pole-1 pole-2), e.g. from letter-tension-poles")
  :outdoc '("tension" "pole number (1 or 2)" "offset from the pole (tension steps)")
  :doc "Picks a pole with the weights <weights> (default 1 and 0.7), then a tension
in the window pole +/- <width>.
<offset> nil or :uniform (default) - random, uniform distribution (every value equally probable);
:gauss - random, normal distribution around the pole (sigma = <offset-sigma> * width, default 0.5;
the result is clipped to the window, so the poles come up more often than the window edges);
a number -1..1 - position in the window directly: -1 = pole - width, 0 = pole, 1 = pole + width
(rounded to the nearest tension, values outside -1..1 clipped).
The result is clipped to <tmin>..<tmax> (piano matrix: 0..285)."
  (let* ((i (spiral-pick-index weights))
         (p (nth i poles))
         (tt (max tmin (min tmax
                            (+ p (cond ((numberp offset)
                                        (round (* (max -1 (min 1 offset)) width)))
                                       ((eq offset :gauss)
                                        (spiral-gauss-offset width (or offset-sigma 0.5)))
                                       (t (- (random (1+ (* 2 width))) width))))))))
    (values tt (1+ i) (- tt p))))

(defmethod! pole-chord ((letter t) (n integer) &key (width 3) (offset nil) (offset-sigma 0.5) (pole-weights '(1 0.7))
                        (center 6000) (sigma 1200) (weights :normal) (band 1)
                        (lo 113) (hi 244) (direction 1) (model :circle) (needle :up) (turn nil) (align-hue nil)
                        (toler 2) (n-harm 3) (range '(2100 10800)) (shift *spiral-shift*))
  :icon 9106
  :numouts 4
  :initvals '("A" 5)
  :indoc '("letter" "number of notes in the chord")
  :outdoc '("chord (midicents)" "tension" "pole number (1 or 2)" "offset from the pole")
  :doc "One chord of a letter: a pole with the weights <pole-weights>, a tension in the window pole +/- <width>
(uniform distribution), the matrix chord for this tension, and from it <n> notes picked
with register weights (<weights> :normal with <center> and <sigma>, :pascal or a list of weights).
Other inputs as in letter-tension-poles and set-matrix-lmidic.
<offset> nil / :uniform = random tension in the window (uniform); :gauss = random, normal distribution
around the pole (sigma = <offset-sigma> * width); a number -1..1 = position in the window (as in pick-pole-tension).
Call it in a loop (e.g. omloop, mapcar) to get a sequence of chords."
  (let ((poles (letter-tension-poles letter :lo lo :hi hi :direction direction :model model
                                            :needle needle :turn turn :align-hue align-hue
                                            :toler toler :n-harm n-harm :range range :shift shift)))
    (multiple-value-bind (tt pole off) (pick-pole-tension poles :weights pole-weights :width width :offset offset :offset-sigma offset-sigma)
      (values (gauss-pick-pitches (nth tt (spiral-matrix toler n-harm range)) n
                                  :center center :sigma sigma :weights weights
                                  :band band)
              tt pole off))))

;;; --------------------------- forma z krzywych -------------------------------
;;; Parametry akordow (center, sigma, n, waga bieguna 1, okno) jako liczba,
;;; lista albo BPF narysowany recznie - BPF jest probkowany na liczbe akordow.

(defun spiral-bpf-at (bpf u)   ; ukryj
  "Value of the BPF at U (0..1 = from the first to the last x), linear."
  (let* ((xs (x-points bpf)) (ys (y-points bpf))
         (x (+ (first xs) (* u (- (car (last xs)) (first xs))))))
    (cond ((null (cdr xs)) (first ys))
          ((<= x (first xs)) (first ys))
          (t (loop for (x1 x2) on xs
                   for (y1 y2) on ys
                   when (and x2 (<= x x2))
                     return (if (= x1 x2) y2 (+ y1 (* (- y2 y1) (/ (- x x1) (- x2 x1)))))
                   finally (return (car (last ys))))))))

(defun spiral-param-list (v count)   ; ukryj
  "V as a list of COUNT values: a number - constant, a list - in order (the last repeated), a BPF - samples."
  (cond ((typep v 'bpf)
         (loop for i from 0 below count
               collect (spiral-bpf-at v (if (> count 1) (/ i (float (1- count))) 0))))
        ((consp v)
         (loop for i from 0 below count collect (nth (min i (1- (length v))) v)))
        (t (make-list count :initial-element v))))

(defun spiral-color-bpc (xs ys color name)   ; ukryj
  (let ((b (simple-bpf-from-list xs ys 'bpc 0)))
    (setf (bpfcolor b) (om-make-color (first color) (second color) (third color)))
    (set-name b name)
    b))

(defmethod! pole-chord-seq ((letter t) (n-chords integer) &key (n 5) (center 6000) (sigma 1200)
                            (pole-weight nil) (width 3) (offset nil) (offset-sigma 0.5) (weights :normal) (band 1)
                            (dur 1000) (vel 80)
                            (lo 113) (hi 244) (direction 1) (model :circle) (shift *spiral-shift*)
                            (toler 2) (n-harm 3) (range '(2100 10800)))
  :icon 9106
  :numouts 3
  :initvals '("A" 16)
  :indoc '("letter" "number of chords")
  :outdoc '("chord-seq" "bpc-lib: center, band center +/- band*sigma, notes" "chord parameters"
            "BPFs after reduction: (pole-weight center sigma n offset)" "band limits of the whole run (from to)"
            "chord-seq of the full matrix chords for the drawn tensions")
  :doc "A sequence of <n-chords> chords of a letter (like pole-chord in a loop), every <dur> ms.
Each of the parameters <n> <center> <sigma> <pole-weight> <width> <offset> can be:
a number (constant), a list (one per chord, the last value repeated)
or a BPF (drawn; sampled evenly from the first to the last x).
<center>, <sigma> in midicents. <n> is rounded, at least 1.
<pole-weight> - weight of pole 1 (0..1, pole 2 gets 1 - weight);
nil = fixed weights 1 and 0.7. A list of pairs can also be given, e.g. ((0.8 0.2) (0.7 0.3)).
<offset> - choice of the tension in the window pole +/- width: nil or :uniform (default) = random (uniform),
:gauss = random from a normal distribution around the pole (sigma = <offset-sigma> * width; <offset-sigma>
a number, list or BPF, default 0.5 - smaller = closer to the pole),
-1..1 = position in the window (-1 bottom, 0 pole, 1 top), e.g. a BPF drawn in -1..1.
<vel> - velocity: a number, list or BPF (like center), rounded to 1..127.
<weights> :normal (Gaussian with center and sigma), :pascal or a list of weights - as in gauss-pick-pitches.
Output 2: for bpc-lib - red center, orange band limits (center +/- band*sigma),
blue notes. <band> as in gauss-pick-pitches (default 1).
Output 3: a list (center sigma n notes-in-chord (band from to) (ambitus from to) weights tension pole offset)
for every chord (n - requested, notes-in-chord - actual, may be fewer because of the band <band>;
band - center +/- band*sigma, nil when band = nil; ambitus - the actual extreme notes of the chord;
offset - tension minus pole, in steps).
Output 6 (optional): chord-seq of the full matrix chords (the whole <range>, without note selection
and without band) for the same drawn tensions, in the same rhythm.
Output 5 (optional): band limits of the whole run (from to) in midicents - the lowest
value of center - band*sigma and the highest center + band*sigma; for preview in CHORD.
Output 4 (optional - add with the > key, remove with <): a list of 6 BPFs with the values used for the consecutive chords (x = chord number 0, 1, 2...):
weight of pole 1 (share, 0..1), center, sigma, n, offset (steps), velocity - preview of the curves after reduction."
  (let* ((cs (spiral-param-list center n-chords))
         (ss (spiral-param-list sigma n-chords))
         (ns (mapcar #'(lambda (x) (max 1 (round x))) (spiral-param-list n n-chords)))
         (ws (spiral-param-list width n-chords))
         (os (spiral-param-list offset n-chords))
         (oss (spiral-param-list offset-sigma n-chords))
         (pw (if (and (consp pole-weight) (consp (first pole-weight)))
                 (loop for i from 0 below n-chords
                       collect (nth (min i (1- (length pole-weight))) pole-weight))
                 (mapcar #'(lambda (w) (if w (list (max 0 (min 1 w)) (- 1 (max 0 (min 1 w)))) '(1 0.7)))
                         (spiral-param-list pole-weight n-chords))))
         (onsets (loop for i from 0 below n-chords collect (* i dur)))
         (vs (mapcar #'(lambda (v) (max 1 (min 127 (round v)))) (spiral-param-list vel n-chords)))
         (chords nil) (full nil) (params nil) (offs nil))
    (loop for i from 0 below n-chords
          do (multiple-value-bind (ch tt pole off)
                 (pole-chord letter (nth i ns) :width (max 0 (round (nth i ws))) :offset (nth i os) :offset-sigma (nth i oss)
                             :pole-weights (nth i pw)
                             :center (nth i cs) :sigma (nth i ss) :weights weights
                             :band band
                             :lo lo :hi hi :direction direction :model model :shift shift
                             :toler toler :n-harm n-harm :range range)
               (push ch chords)
               (push off offs)
               (push (copy-list (nth tt (spiral-matrix toler n-harm range))) full)
               (push (list (round (nth i cs)) (round (nth i ss)) (nth i ns) (length ch)
                                 (if band (list (round (- (nth i cs) (* band (nth i ss)))) (round (+ (nth i cs) (* band (nth i ss))))) nil)
                                 (if ch (list (reduce #'min ch) (reduce #'max ch)) nil)
                                 (mapcar #'(lambda (w) (/ (round (* w 100)) 100.0)) (nth i pw)) tt pole off) params)))
    (setf chords (nreverse chords) full (nreverse full) params (nreverse params) offs (nreverse offs))
    (let* ((seq (make-instance 'chord-seq
                               :lmidic chords
                               :lonset onsets
                               :ldur (mapcar #'(lambda (c) (make-list (length c) :initial-element dur)) chords)
                               :lvel (mapcar #'(lambda (c v) (make-list (length c) :initial-element v)) chords vs)))
           (nx (loop for o in onsets for c in chords append (make-list (length c) :initial-element o)))
           (ny (loop for c in chords append c))
           (lib (make-instance 'bpc-lib
                               :bpf-list (list (spiral-color-bpc onsets (mapcar #'round cs) '(0.85 0.1 0.1) "center")
                                               (spiral-color-bpc onsets (mapcar #'(lambda (c s) (round (+ c (* (or band 1) s)))) cs ss) '(0.95 0.6 0.1) "center+pas")
                                               (spiral-color-bpc onsets (mapcar #'(lambda (c s) (round (- c (* (or band 1) s)))) cs ss) '(0.95 0.6 0.1) "center-pas")
                                               (spiral-color-bpc nx ny '(0.1 0.2 0.85) "dzwieki")))))
      (values seq lib params
              (let ((xs (loop for i from 0 below n-chords collect i)))
                (list (simple-bpf-from-list xs (mapcar #'(lambda (w) (let ((tot (+ (first w) (second w))))
                                                                        (if (> tot 0) (/ (first w) (float tot)) 0.5)))
                                                       pw)
                                            'bpf 2)
                      (simple-bpf-from-list xs (mapcar #'round cs) 'bpf 0)
                      (simple-bpf-from-list xs (mapcar #'round ss) 'bpf 0)
                      (simple-bpf-from-list xs ns 'bpf 0)
                      (simple-bpf-from-list xs offs 'bpf 0)
                      (simple-bpf-from-list xs vs 'bpf 0)))
              (let ((k (or band 1)))
                (list (round (loop for c in cs for x in ss minimize (- c (* k x))))
                      (round (loop for c in cs for x in ss maximize (+ c (* k x))))))
              (make-instance 'chord-seq
                             :lmidic full
                             :lonset onsets
                             :ldur (mapcar #'(lambda (c) (make-list (length c) :initial-element dur)) full)
                             :lvel (mapcar #'(lambda (c v) (make-list (length c) :initial-element v)) full vs))))))

;;; Pudelko z opcjonalnymi wyjsciami 4, 5 i 6 (jak list-elements: > dodaje, < usuwa).

(defclass OMBoxPoleChordSeq (OMBoxSplit)
  ((numouts :initform 3 :accessor numouts)))

(defmethod get-boxcallclass-fun ((self (eql 'pole-chord-seq))) 'OMBoxPoleChordSeq)

(defmethod do-add-one-input ((self OMBoxPoleChordSeq))
  (if (< (numouts self) 6)
      (setf (numouts self) (1+ (numouts self)))
      (om-beep))
  t)

(defmethod do-delete-one-input ((self OMBoxPoleChordSeq))
  (if (> (numouts self) 3)
      (setf (numouts self) (1- (numouts self)))
      (om-beep))
  t)

;;; --------------------------- obraz kola (SVG) ------------------------------

(defun spiral-svg-rgb (h &optional (s 0.72) (v 0.82))   ; ukryj
  "Colour 'rgb(r,g,b)' for the hue H - as hsv(h, 0.72, 0.82) of the letters in Remotion."
  (multiple-value-bind (r g b) (hsv->rgb h s v)
    (format nil "rgb(~D,~D,~D)" (round (* 255 r)) (round (* 255 g)) (round (* 255 b)))))

(defun spiral-svg-string (lo hi shift direction model needle turn align-hue size dark
                          font-family font-size title needles)   ; ukryj
  (let* ((n (spiral-circle-size lo hi))
         (w0 (+ lo shift))
         (c (/ size 2d0))
         (r (* size 0.33d0))
         (ring (* size 0.035d0))
         (fs (or font-size (* size 0.028d0)))
         (hh (+ size (* fs 3.2)))
         (cy (+ c (* fs 1.4)))
         (bg (if dark "rgb(18,18,20)" "rgb(255,255,255)"))
         (fg (if dark "rgb(235,230,220)" "rgb(30,30,30)"))
         (dim (if dark "rgb(120,118,112)" "rgb(150,150,150)")))
    (labels ((ang (tt) (* (/ pi 180) (+ -90 (* 360d0 (/ (mod (- tt w0) n) n)))))
             (px (tt rad) (+ c (* rad (cos (ang tt)))))
             (py (tt rad) (+ cy (* rad (sin (ang tt))))))
      (with-output-to-string (o)
        (format o "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"~D\" height=\"~D\" viewBox=\"0 0 ~D ~,2F\" font-family=\"~A\">~%"
                size (round hh) size hh font-family)
        (format o "<rect width=\"100%\" height=\"100%\" fill=\"~A\"/>~%" bg)
        (when title
          (format o "<text x=\"~,2F\" y=\"~,2F\" fill=\"~A\" font-size=\"~,2F\" text-anchor=\"middle\">~A</text>~%"
                  c (* fs 1.7) fg (* fs 1.15) title))
        ;; pierscien barw tension
        (loop for tt from w0 below (+ w0 n)
              do (let ((h (tension-hue (- tt shift) :lo lo :hi hi :direction direction :align-hue align-hue))
                       (a0 (- tt 0.5)) (a1 (+ tt 0.55)))
                   (format o "<path d=\"M ~,2F ~,2F A ~,2F ~,2F 0 0 1 ~,2F ~,2F\" stroke=\"~A\" stroke-width=\"~,2F\" fill=\"none\"/>~%"
                           (px a0 r) (py a0 r) r r (px a1 r) (py a1 r) (spiral-svg-rgb h) ring)))
        ;; podzialka
        (loop for tt from w0 below (+ w0 n)
              when (zerop (mod tt 10))
                do (let ((long (zerop (mod tt 30))))
                     (format o "<line x1=\"~,2F\" y1=\"~,2F\" x2=\"~,2F\" y2=\"~,2F\" stroke=\"~A\" stroke-width=\"~,2F\"/>~%"
                             (px tt (- r (* ring 0.5))) (py tt (- r (* ring 0.5)))
                             (px tt (- r (* ring (if long 1.3 0.9)))) (py tt (- r (* ring (if long 1.3 0.9))))
                             dim (if long 1.6 1))
                     (when long
                       (format o "<text x=\"~,2F\" y=\"~,2F\" fill=\"~A\" font-size=\"~,2F\" text-anchor=\"middle\" dominant-baseline=\"middle\">~D</text>~%"
                               (px tt (- r (* ring 2.1))) (py tt (- r (* ring 2.1))) dim (* fs 0.75) tt))))
        ;; igly i bieguny liter
        (let ((items (loop for k below 22
                           collect (let ((pp (mapcar #'(lambda (x) (+ x shift))
                                                     (spiral-letter-poles-raw k lo hi direction model needle turn align-hue)))
                                         (h (spiral-letter-hue k)))
                                     (list k pp h)))))
          (when needles
            (dolist (it items)
              (destructuring-bind (k (p1 p2) h) it
                (declare (ignore k))
                ;; igla: polowa od bieguna 1 w barwie litery, polowa do bieguna 2 w barwie przeciwnej
                (let* ((rr (- r (* ring 2.9)))
                       (x1 (px p1 rr)) (y1 (py p1 rr)) (x2 (px p2 rr)) (y2 (py p2 rr))
                       (mx (/ (+ x1 x2) 2)) (my (/ (+ y1 y2) 2)))
                  (format o "<line x1=\"~,2F\" y1=\"~,2F\" x2=\"~,2F\" y2=\"~,2F\" stroke=\"~A\" stroke-width=\"~,2F\" stroke-opacity=\"0.6\"/>~%"
                          x1 y1 mx my (spiral-svg-rgb h) (* size 0.0025))
                  (format o "<line x1=\"~,2F\" y1=\"~,2F\" x2=\"~,2F\" y2=\"~,2F\" stroke=\"~A\" stroke-width=\"~,2F\" stroke-opacity=\"0.6\"/>~%"
                          mx my x2 y2 (spiral-svg-rgb (mod (+ h 180) 360)) (* size 0.0025))))))
          (dolist (it items)
            (destructuring-bind (k (p1 p2) h) it
              ;; biegun 2 (waga 0.7): mniejsza kropka wewnatrz, barwa przeciwna
              (format o "<circle cx=\"~,2F\" cy=\"~,2F\" r=\"~,2F\" fill=\"~A\" stroke=\"~A\" stroke-width=\"~,2F\"/>~%"
                      (px p2 (- r (* ring 1.0))) (py p2 (- r (* ring 1.0))) (* ring 0.24)
                      (spiral-svg-rgb (mod (+ h 180) 360)) (spiral-svg-rgb h) (* size 0.002))
              ;; biegun 1 (waga 1): kropka na zewnatrz pierscienia i litera
              (format o "<circle cx=\"~,2F\" cy=\"~,2F\" r=\"~,2F\" fill=\"~A\"/>~%"
                      (px p1 (+ r (* ring 1.0))) (py p1 (+ r (* ring 1.0))) (* ring 0.34) (spiral-svg-rgb h))
              (format o "<text x=\"~,2F\" y=\"~,2F\" fill=\"~A\" font-size=\"~,2F\" font-weight=\"bold\" text-anchor=\"middle\" dominant-baseline=\"middle\">~A</text>~%"
                      (px p1 (+ r (* ring 2.1))) (py p1 (+ r (* ring 2.1))) (spiral-svg-rgb h) fs
                      (char *spiral-letters* k))
              (format o "<text x=\"~,2F\" y=\"~,2F\" fill=\"~A\" font-size=\"~,2F\" text-anchor=\"middle\" dominant-baseline=\"middle\">~D</text>~%"
                      (px p1 (+ r (* ring 3.05))) (py p1 (+ r (* ring 3.05))) dim (* fs 0.62) p1))))
        ;; legenda
        (format o "<text x=\"~,2F\" y=\"~,2F\" fill=\"~A\" font-size=\"~,2F\">tension ~D..~D~@[, przesuniecie ~@D~]; model ~(~A~)</text>~%"
                (* fs 0.8) (- hh (* fs 2.0)) dim (* fs 0.7) w0 (+ w0 n -1) (unless (zerop shift) shift) model)
        (format o "<text x=\"~,2F\" y=\"~,2F\" fill=\"~A\" font-size=\"~,2F\">kropka z litera - biegun 1 (waga 1), kropka wewnatrz - biegun 2 (waga 0.7)</text>~%"
                (* fs 0.8) (- hh (* fs 0.8)) dim (* fs 0.7))
        (format o "</svg>~%")))))

(defmethod! tension-spiral->svg (&key path (lo 113) (hi 244) (shift *spiral-shift*) (direction 1) (model :circle) (needle :up)
                                      (turn nil) (align-hue nil) (size 1000) (dark t)
                                      (font-family "Helvetica, Arial, sans-serif") (font-size nil)
                                      (title "Alfabet Torunski - bieguny tension liter") (needles t))
  :icon 9106
  :doc "Saves to an SVG file the tension wheel with the poles of the 22 letters of the palette (for the report).
Ring - tension hues (tension-hue), ticks every 10, numbers every 30.
Each letter: a dot with the letter and the number outside = pole 1 (weight 1), a smaller dot
in the opposite hue inside = pole 2 (weight 0.7), the line between them = the compass needle.
<path> file; nil = file dialog. <lo> <hi> <shift> <direction> <model> <needle> <turn> <align-hue>
as in letter-tension-poles. <size> side of the image in pixels. <dark> t = dark background (as in the films),
nil = white (for print). <title> heading or nil. <needles> nil = no needles.
<font-family>, <font-size> as in radar->svg. Returns the file path."
  (let ((pathname (or path (om-choose-new-file-dialog :directory (def-save-directory)
                                                      :prompt "Nowy plik SVG"
                                                      :types '("SVG Files" "*.svg")))))
    (when pathname
      (with-open-file (s pathname :direction :output :if-exists :supersede :external-format :utf-8)
        (write-string (spiral-svg-string lo hi shift direction model needle turn align-hue size dark
                                         font-family font-size title needles)
                      s))
      (om-print (format nil "tension-spiral->svg: zapisano ~A" (namestring pathname)))
      pathname)))

;;; --------------------------- test -----------------------------------------

(defun tension-spiral-test ()   ; ukryj
  (flet ((near (a b) (< (abs (- a b)) 1d-6)))
    (let* ((checks
            (list
             (list "A (domyslnie 113..244, shift wg *spiral-shift*)" (equal (letter-tension-poles "A") (list (+ 113 *spiral-shift*) (+ 179 *spiral-shift*))))
             (list "A bez przesuniecia" (equal (letter-tension-poles "A" :shift 0) '(113 179)))
             (list "rowne odstepy" (let ((ps (loop for k below 22 collect (first (letter-tension-poles (string (char *spiral-letters* k)))))))
                                     (every #'(lambda (a b) (= (- b a) 6)) ps (cdr ps))))
             (list "L naprzeciw A" (equal (spiral-letter-poles-raw 11 113 246 1) '(180 113)))
             (list "B" (equal (spiral-letter-poles-raw 1 113 246 1) '(119 186)))
             (list "Z" (equal (spiral-letter-poles-raw 21 113 246 1) '(241 174)))
             (list "kierunek -1" (equal (spiral-letter-poles-raw 1 113 246 -1) '(241 174)))
             (list "igla 67" (loop for k below 22
                                   always (destructuring-bind (p1 p2) (spiral-letter-poles-raw k 113 246 1)
                                            (and (<= 113 p1 246) (<= 113 p2 246)
                                                 (= (mod (- p2 p1) 134) 67)))))
             (list "barwa A" (near (spiral-letter-hue 0) (mod (- 361 (/ 360d0 44)) 360)))
             (list "barwa tension" (and (near (tension-hue 113 :lo 113 :hi 246) (spiral-letter-hue 0))
                                        (near (tension-hue 180 :lo 113 :hi 246) (mod (+ (spiral-letter-hue 0) 180) 360))
                                        (near (tension-hue 247 :lo 113 :hi 246) (spiral-letter-hue 0))
                                        (near (tension-hue 179) (mod (+ (spiral-letter-hue 0) 180) 360))))
             (list "przeplot" (and (equal (spiral-letter-poles-raw 0 113 246 1 :interleave) '(113 180))
                                   (equal (spiral-letter-poles-raw 1 113 246 1 :interleave) '(125 192))
                                   (equal (spiral-letter-poles-raw 10 113 246 1 :interleave) '(235 168))
                                   (equal (spiral-letter-poles-raw 11 113 246 1 :interleave) '(241 174))
                                   (equal (spiral-letter-poles-raw 21 113 246 1 :interleave) '(119 186))))
             (list "spirala" (and (equal (spiral-letter-poles-raw 1 113 246 1 :helix :up) '(118 177))
                                  (equal (spiral-letter-poles-raw 21 113 246 1 :helix :up) '(226 285))
                                  (equal (spiral-letter-poles-raw 0 113 246 1 :helix :down) '(113 46))
                                  (equal (spiral-letter-poles-raw 21 113 246 1 :helix :down) '(241 174))))
             (list "barwa = tension" (and (equal (spiral-letter-poles-raw 0 118 237 1 :circle :up nil t) '(118 178))
                                          (equal (spiral-letter-poles-raw 1 118 237 1 :circle :up nil t) '(232 172))
                                          (near (tension-hue 240 :lo 120 :hi 239 :align-hue t) 0)
                                          (near (tension-hue 180 :lo 120 :hi 239 :align-hue t) 180)))
             (list "losowanie z wagami" (and (loop repeat 200 always (= (spiral-pick-index '(0 1 0)) 1))
                                             (loop repeat 200 always (member (spiral-pick-index '(1 1)) '(0 1)))))
             (list "bez powtorzen" (let ((r (spiral-pick-without-replacement '(1 2 3 4 5) '(1 1 1 1 1) 5)))
                                     (equal (sort (copy-list r) #'<) '(1 2 3 4 5))))
             (list "gauss" (let ((w (spiral-gauss-weights '(4800 6000 7200) 6000 1200)))
                             (and (near (second w) 1) (near (first w) (exp -0.5d0)) (near (third w) (exp -0.5d0)))))
             (list "wybor dzwiekow" (and (equal (gauss-pick-pitches '(7100 4800 6000) 5) '(4800 6000 7100))
                                         (equal (gauss-pick-pitches '(4800 6000 7100) 1 :weights '(0 0 1)) '(7100))
                                         (= (length (gauss-pick-pitches '(4800 5500 6000 6200 6700) 3 :weights :pascal)) 3)))
             (list "okno tension" (loop repeat 200
                                        always (multiple-value-bind (tt p) (pick-pole-tension '(120 180) :weights '(1 0) :width 3)
                                                 (and (= p 1) (<= 117 tt 123)))))
             (list "pamiec matrycy" (eq (spiral-matrix 2 3 '(2100 10800)) (spiral-matrix 2 3 '(2100 10800))))
             (list "probki BPF" (let ((b (simple-bpf-from-list '(0 50 100) '(0 100 0) 'bpf 0)))
                                  (equal (mapcar #'round (spiral-param-list b 5)) '(0 50 100 50 0))))
             (list "parametry" (and (equal (spiral-param-list 7 3) '(7 7 7))
                                    (equal (spiral-param-list '(1 2) 3) '(1 2 2))))
             (list "mala sigma" (and (equal (gauss-pick-pitches '(3600 5500 6000 6200 8400) 2 :center 6100 :sigma 0 :band nil)
                                            '(6000 6200))
                                     (equal (gauss-pick-pitches '(3600 5500 6000 6200 8400) 2 :center 6100 :sigma 0)
                                            '(6000))))
             (list "pas :cut" (equal (gauss-pick-pitches '(3600 5500 6000 6200 8400) 5 :center 6000 :sigma 600)
                                     '(5500 6000 6200)))
             (list "pas pusty" (equal (gauss-pick-pitches '(3600 8400) 3 :center 6000 :sigma 100) '(3600)))
             (list "offset gauss" (let ((offs (loop repeat 1000 collect (nth-value 2 (pick-pole-tension '(150 200) :weights '(1 0) :width 3 :offset :gauss)))))
                                    (and (every #'(lambda (o) (<= -3 o 3)) offs)
                                         (> (count-if #'(lambda (o) (<= -1 o 1)) offs) 550)
                                         (<= -3 (nth-value 2 (pick-pole-tension '(150 200) :weights '(1 0) :width 3 :offset :uniform)) 3))))
             (list "offset" (and (equal (multiple-value-list (pick-pole-tension '(120 180) :weights '(1 0) :width 3 :offset 1)) '(123 1 3))
                                 (equal (multiple-value-list (pick-pole-tension '(120 180) :weights '(0 1) :width 3 :offset -1)) '(177 2 -3))
                                 (= (pick-pole-tension '(120 180) :weights '(1 0) :width 3 :offset 0) 120)
                                 (= (pick-pole-tension '(120 180) :weights '(1 0) :width 4 :offset 0.5) 122)))
             (list "svg" (let ((x (spiral-svg-string 120 239 0 1 :circle :up nil nil 400 t "Helvetica" nil "t" t)))
                           (and (search "<svg" x) (search "</svg>" x) (= (count #\< x) (count #\> x)))))
             (list "przesuniecie" (and (equal (letter-tension-poles "A" :shift -2) '(111 177))
                                       (equal (letter-tension-poles '("A") :shift 3) '((116 182)))))
             (list "wagi" (equal (nth-value 1 (letter-tension-poles "A")) '(1 0.7)))
             (list "ogonki" (= (spiral-letter-index (string (code-char 321))) (spiral-letter-index "L")))))
           (bad (loop for c in checks unless (second c) collect (first c))))
      (om-print (if bad
                    (format nil "tension-spiral-test: BLAD w: ~{~a~^, ~}" bad)
                    "tension-spiral-test: OK"))
      (null bad))))

;;; ==========================================================================
;;; Eksport do artefaktu "Kolo tension": jedna nuta w linii
;;;   onset(ms) midicent dur(ms) vel tension
;;; tension przypisane do onsetu (to samo dla wszystkich nut akordu).
;;; ==========================================================================

(defun tension-text-value (x)   ; ukryj
  "Tension from a list element: a number, a row of output 3 of pole-chord-seq (the 8th element) or a list of numbers (the first)."
  (cond ((numberp x) x)
        ((and (consp x) (>= (length x) 8) (numberp (nth 7 x))) (nth 7 x))
        ((and (consp x) (numberp (first x))) (first x))
        (t nil)))

(defun tension-text-nth (j l)   ; ukryj
  "J-th element of the list L (the last repeated); L may be a number."
  (cond ((numberp l) l)
        ((consp l) (nth (min j (1- (length l))) l))
        (t 0)))

(defun tension-text-string (chords onsets durs vels tl &optional (lo 113) (hi 244) (direction 1) (shift *spiral-shift*))   ; ukryj
  (with-output-to-string (o)
    (format o ";; tension-text: onset(ms) midicent dur(ms) vel tension~%")
    (format o ";; kolo: lo ~D hi ~D direction ~D shift ~D~%" lo hi direction shift)
    (loop for ch in chords
          for i from 0
          for on = (or (nth i onsets) 0)
          for tt = (nth i tl)
          when tt
            do (loop for mc in ch
                     for j from 0
                     do (format o "~D  ~D  ~D  ~D  ~D~%"
                                (round on) (round mc)
                                (round (tension-text-nth j (nth i durs)))
                                (round (tension-text-nth j (nth i vels)))
                                (round tt))))))

(defun tension-text-by-onset (onsets ref-onsets ref-tensions)   ; ukryj
  "Dla kazdego onsetu: tension ostatniego onsetu odniesienia <= onset (nil, gdy brak)."
  (let ((pairs (sort (loop for o in ref-onsets for tt in ref-tensions when tt collect (cons o tt)) #'< :key #'car)))
    (loop for o in onsets
          collect (let ((best nil))
                    (dolist (p pairs best)
                      (if (<= (car p) (+ o 1e-6)) (setf best (cdr p)) (return best)))))))

(defmethod! chseq->tension-text ((self chord-seq) (tensions t) &key (path nil) (ref nil) (lo 113) (hi 244) (direction 1) (shift *spiral-shift*) (toler 2) (n-harm 3) (range '(2100 10800)))
  :icon 9106
  :initvals '(nil nil)
  :indoc '("chord-seq" "tension: a number, a list (per chord), output 3 of pole-chord-seq or nil")
  :outdoc '("text for the Tension Wheel artifact")
  :doc "Text for the 'Tension Wheel' artifact: one note per line - onset(ms) midicent dur(ms) vel tension.
The tension is assigned to the onset: all notes of a chord get the same one.
<tensions>: a number (for all chords), a list (one per chord, the last repeated),
output 3 of the pole-chord-seq box (a params list - the tension is the 8th element of each row)
or nil - then the tension is computed from the matrix (<toler> <n-harm> <range>, like lmidic-to-tension):
from the chords of <ref> when <ref> is a chord-seq (assigned by onsets), otherwise from the chords of <self>;
a chord that fits no tension is skipped (message in the Listener).
<path>: nil - only the text on the output; t - dialog for saving a .txt file; a string - the file path.
<ref>: when the chord-seq was changed on the way (e.g. tie-chseq-notes merges notes and changes the number of chords),
connect here the source chord-seq (output 1 of pole-chord-seq) or the list of its onsets - then the tension
is assigned by time: a chord gets the tension of the source chord with the same (or the last earlier) onset.
Without <ref>, when the number of tensions does not match the number of chords, a warning appears in the Listener.
<lo> <hi> <direction> <shift> - wheel layout as in pole-chord-seq / letter-tension-poles; they go into the header
(;; kolo: ...), and the Tension Wheel artifact sets up the wheel from them (e.g. shift -2 moves the letters onto the poles)."
  (let* ((chords (lmidic self))
         (n (length chords))
         (ref-on (cond ((typep ref 'chord-seq) (lonset ref))
                       ((consp ref) ref)
                       (t nil)))
         (tl (cond ((numberp tensions) (make-list n :initial-element tensions))
                   ((and (consp tensions) ref-on)
                    (tension-text-by-onset (subseq (lonset self) 0 n) ref-on
                                           (mapcar #'tension-text-value tensions)))
                   ((consp tensions)
                    (loop for i from 0 below n
                          collect (tension-text-value (nth (min i (1- (length tensions))) tensions))))
                   ((typep ref 'chord-seq)
                    ;; tensions nil + :ref chord-seq: tension z akordow zrodlowych (matryca), przypisane po onsetach
                    (let* ((mat (spiral-matrix toler n-harm range))
                           (rt (loop for ch in (lmidic ref)
                                     collect (loop for i from 0
                                                   for m in mat
                                                   when (subsetp ch m :test #'=) return i))))
                      (tension-text-by-onset (subseq (lonset self) 0 n) (lonset ref) rt)))
                   (t (let ((mat (spiral-matrix toler n-harm range)))
                        (loop for ch in chords
                              collect (loop for i from 0
                                            for m in mat
                                            when (subsetp ch m :test #'=) return i))))))
         (missing (count nil tl))
         (warn-len (and (consp tensions) (null ref-on) (/= (length tensions) n)))
         (txt (tension-text-string chords (lonset self) (ldur self) (lvel self) tl lo hi direction shift)))
    (when warn-len
      (om-print (format nil "chseq->tension-text: ~D tension, a ~D akordow - tension moga byc przesuniete; podlacz chord-seq zrodlowy do :ref"
                        (length tensions) n)))
    ;; tension z matrycy dla akordow <self>, a kilka akordow ma ten sam onset - zwykle chord-seq po tie-chseq-notes bez :ref
    (when (and (null tensions) (not (typep ref 'chord-seq)))
      (let* ((ons (subseq (lonset self) 0 n))
             (dup (- n (length (remove-duplicates ons :test #'=)))))
        (when (> dup 0)
          (om-print (format nil "chseq->tension-text: ~D akord(ow) ma powtorzony onset - tension liczone z pojedynczych akordow; po tie-chseq-notes podlacz chord-seq zrodlowy do :ref"
                            dup)))))
    (when (> missing 0)
      (om-print (format nil "chseq->tension-text: pominieto ~D akord(ow) bez tension" missing)))
    (let ((file (cond ((stringp path) path)
                      ((pathnamep path) path)
                      ((eq path t) (om-choose-new-file-dialog :directory (def-save-directory)
                                                              :types '("Text Files" "*.txt")))
                      (t nil))))
      (when file
        (with-open-file (o file :direction :output :if-exists :supersede :external-format :utf-8)
          (write-string txt o))
        (om-print (format nil "chseq->tension-text: zapisano ~A" (namestring file)))))
    txt))

(defun chseq->tension-text-test ()   ; ukryj
  "Checks the format, the assignment of tension to onsets and reading the params output of pole-chord-seq."
  (let* ((cs (make-instance 'chord-seq
                            :lmidic '((6000 6700) (6200))
                            :lonset '(0 1000)
                            :ldur '((1500 1000) (750))
                            :lvel '((100 90) (80))))
         (t1 (chseq->tension-text cs '(150 187)))
         (t2 (chseq->tension-text cs '((5000 1200 3 2 nil (6000 6700) (1 0.7) 140 138 2)
                                       (5000 1200 3 1 nil (6200 6200) (1 0.7) 203 204 -1))))
         (t3 (chseq->tension-text cs 120))
         (src (make-instance 'chord-seq :lmidic '((6000) (6200) (6400)) :lonset '(0 500 1000)
                             :ldur '((400) (400) (400)) :lvel '((100) (100) (100))))
         (t4 (chseq->tension-text cs '(150 160 170) :ref src))
         (checks (list (list "linie z liczbami"
                             (search (format nil "0  6000  1500  100  150~%0  6700  1000  90  150~%1000  6200  750  80  187") t1))
                       (list "params pole-chord-seq" (and (search "0  6000  1500  100  140" t2) (search "1000  6200  750  80  203" t2)))
                       (list "jedno tension" (search "1000  6200  750  80  120" t3))
                       (list "naglowek" (eql 0 (search ";; tension-text" t1)))
                       (list "naglowek kola" (and (search (format nil ";; kolo: lo 113 hi 244 direction 1 shift ~D" *spiral-shift*) t1)
                                                  (search ";; kolo: lo 113 hi 244 direction -1 shift -2"
                                                          (chseq->tension-text cs 150 :direction -1 :shift -2))))
                       (list "ref po onsetach" (and (search "0  6700  1000  90  150" t4) (search "1000  6200  750  80  170" t4)))
                       (list "nil + ref: tension z akordow zrodlowych"
                             (let* ((split (make-instance 'chord-seq :lmidic '((6000) (6700) (6200)) :lonset '(0 0 1000)
                                                                     :ldur '((1500) (1000) (750)) :lvel '((100) (90) (80))))
                                    (t5 (chseq->tension-text split nil :ref cs))
                                    (a (lmidic-to-tension '(6000 6700) 2 3 '(2100 10800)))
                                    (b (lmidic-to-tension '(6200) 2 3 '(2100 10800))))
                               (and (search (format nil "0  6000  1500  100  ~D" a) t5)
                                    (search (format nil "0  6700  1000  90  ~D" a) t5)
                                    (search (format nil "1000  6200  750  80  ~D" b) t5))))))
         (bad (loop for c in checks unless (second c) collect (first c))))
    (om-print (if bad
                  (format nil "chseq->tension-text-test: BLAD w: ~{~a~^, ~}" bad)
                  "chseq->tension-text-test: OK"))
    (null bad)))

;;; --------------------------- tension-text -> lista ---------------------------

(defun tension-text-lines (txt)   ; ukryj
  "Splits a string into lines (LF, CR or CRLF)."
  (let ((lines nil) (start 0) (n (length txt)))
    (loop for i from 0 below n
          for ch = (char txt i)
          when (or (char= ch #\Newline) (char= ch #\Return))
            do (push (subseq txt start i) lines)
               (setf start (1+ i)))
    (when (< start n) (push (subseq txt start) lines))
    (nreverse lines)))

(defun tension-text-numbers (line)   ; ukryj
  "Numbers from a line separated by spaces/tabs; words that are not numbers are skipped."
  (let ((nums nil) (start nil) (n (length line)))
    (flet ((token (a b)
             (let ((x (ignore-errors
                        (let ((*read-eval* nil))
                          (read-from-string line nil nil :start a :end b)))))
               (when (numberp x) (push x nums)))))
      (loop for i from 0 below n
            for ch = (char line i)
            do (if (or (char= ch #\Space) (char= ch #\Tab))
                   (when start (token start i) (setf start nil))
                   (unless start (setf start i))))
      (when start (token start n)))
    (nreverse nums)))

(defmethod! tension-text->list ((text t))
  :icon 9106
  :initvals '(nil)
  :indoc '("text from chseq->tension-text (a string), a list of lines or the path of a .txt file")
  :outdoc '("list of (midicent onset dur vel tension)")
  :doc "Turns the text from CHSEQ->TENSION-TEXT into a list of lists of numbers, one list per line (note),
in lnotes order: (midicent onset(ms) dur(ms) vel tension)
(in the text the onset comes first - here it is swapped with the midicent); it can be connected directly
to lnotes functions, e.g. list-lnotes-by-lonset.
Header lines (starting with ;) and empty lines are skipped.
<text>: the string from the output of chseq->tension-text, a list of lines (strings) or a file path
(a pathname or a string without a newline pointing to an existing file)."
  (let* ((txt (cond ((pathnamep text)
                     (with-open-file (in text :external-format :utf-8 :element-type 'character)
                       (let ((s (make-string (file-length in))))
                         (subseq s 0 (read-sequence s in)))))
                    ((and (stringp text)
                          (not (find #\Newline text))
                          (not (find #\Return text))
                          (> (length text) 0)
                          (not (char= (char text 0) #\;))
                          (ignore-errors (probe-file text)))
                     (tension-text->list (pathname text)))
                    ((stringp text) text)
                    ((consp text) (format nil "~{~a~%~}" text))
                    (t ""))))
    (if (listp txt)
        txt
        (let ((rows (loop for line in (tension-text-lines txt)
                          for trimmed = (string-left-trim '(#\Space #\Tab) line)
                          for nums = (unless (or (zerop (length trimmed)) (char= (char trimmed 0) #\;))
                                       (tension-text-numbers trimmed))
                          when nums collect nums)))
          ;; kolejnosc lnotes: midicent onset dur vel ...
          (mapcar #'(lambda (r) (if (cdr r) (list* (second r) (first r) (cddr r)) r)) rows)))))

(defun tension-text->list-test ()   ; ukryj
  "Text from chseq->tension-text -> list in lnotes order (midicent onset dur vel tension); header skipped."
  (let* ((cs (make-instance 'chord-seq
                            :lmidic '((6000 6700) (6200))
                            :lonset '(0 1000)
                            :ldur '((1500 1000) (750))
                            :lvel '((100 90) (80))))
         (txt (chseq->tension-text cs '(150 187)))
         (ref '((6000 0 1500 100 150) (6700 0 1000 90 150) (6200 1000 750 80 187)))
         (checks
           (list (list "napis" (equal (tension-text->list txt) ref))
                 (list "lista linii" (equal (tension-text->list (tension-text-lines txt)) ref))
                 (list "CRLF i tabulatory"
                       (equal (tension-text->list (format nil ";; x~c~c0~c6000 1500  100 150~c~c"
                                                          #\Return #\Newline #\Tab #\Return #\Newline))
                              '((6000 0 1500 100 150))))))
         (bad (loop for c in checks unless (second c) collect (first c))))
    (om-print (if bad
                  (format nil "tension-text->list-test: BLAD w: ~{~a~^, ~}" bad)
                  "tension-text->list-test: OK"))
    (null bad)))

;;; --------------------------- tension -> litera ---------------------------

(defun spiral-signed-dist (a b n circular)   ; ukryj
  "A - B; na kole (CIRCULAR) zawiniete do przedzialu (-n/2, n/2]."
  (if circular
      (let ((d (mod (- a b) n)))
        (if (> (* 2 d) n) (- d n) d))
      (- a b)))

(defun spiral-nearest-pole (tension pole lo hi direction model needle turn align-hue shift)   ; ukryj
  "Index of the letter (0..21) whose pole POLE (0 = arrowhead, 1 = pole 2) lies nearest to TENSION,
and the distance tension - pole. Tie (exactly halfway) - the negative distance wins."
  (let ((n (spiral-circle-size lo hi))
        (circular (or align-hue (eq model :circle)))
        (best nil) (best-d nil))
    (loop for k from 0 below 22
          for p = (+ shift (nth pole (spiral-letter-poles-raw k lo hi direction model needle turn align-hue)))
          for d = (spiral-signed-dist tension p n circular)
          when (or (null best-d)
                   (< (abs d) (abs best-d))
                   (and (= (abs d) (abs best-d)) (< d best-d)))
            do (setf best k best-d d))
    (values best best-d)))

(defmethod! tension->letter ((tension t) &key (lo 113) (hi 244) (direction 1) (model :circle) (needle :up)
                             (turn nil) (align-hue nil) (shift *spiral-shift*))
  :icon 9106
  :numouts 5
  :initvals '(113)
  :indoc '("tension (a number or a list)")
  :outdoc '("letter (nearest arrowhead - pole 1)" "offset: tension - pole 1 of this letter"
            "letter whose pole 2 lies nearest" "tension hue in degrees"
            "tension moved into <lo>..<hi>")
  :doc "Inverse of LETTER-TENSION-POLES: for a tension gives the letter of the Alphabet on the wheel <lo>..<hi>.
The tension is always first moved into the alphabet range <lo>..<hi> (default 113..244):
t' = lo + ((t - lo) mod n), n = hi - lo + 1 = 132; e.g. 250 -> 118, 60 -> 192 (output 5).
Output 1: the letter whose pole 1 (arrowhead, letter hue) lies nearest the tension,
output 2: the offset tension - pole 1 of this letter (for 113..244 from -3 to +2; exactly
halfway between letters the higher letter wins, with offset -3),
output 3: the letter whose pole 2 lies nearest (on the wheel: the opposite letter),
output 4: the tension hue (TENSION-HUE).
E.g. for 113..244: 113 -> A 0 (pole 2 of letter L), 179 -> L 0 (pole 2 of letter A), 121 -> B +2.
On the wheel (:circle, <align-hue>) the distance is measured around the wheel (244 -> A -1); with :interleave and :helix directly
(after the move).
Other keys as in LETTER-TENSION-POLES (<shift> default *spiral-shift*).
For a list of tensions every output is a list."
  (if (listp tension)
      (let ((r (mapcar #'(lambda (x)
                           (multiple-value-list
                            (tension->letter x :lo lo :hi hi :direction direction :model model :needle needle
                                               :turn turn :align-hue align-hue :shift shift)))
                       tension)))
        (values (mapcar #'first r) (mapcar #'second r) (mapcar #'third r) (mapcar #'fourth r) (mapcar #'fifth r)))
      (let ((tt (+ lo (mod (- tension lo) (spiral-circle-size lo hi)))))   ; przeniesienie do lo..hi
        (multiple-value-bind (k1 d1)
            (spiral-nearest-pole tt 0 lo hi direction model needle turn align-hue shift)
          (let ((k2 (spiral-nearest-pole tt 1 lo hi direction model needle turn align-hue shift)))
            (values (string (char *spiral-letters* k1))
                    d1
                    (string (char *spiral-letters* k2))
                    (tension-hue (- tt shift) :lo lo :hi hi :direction direction :align-hue align-hue)
                    tt))))))

(defun tension->letter-test ()   ; ukryj
  "Wheel 113..244: known poles, tie, wrap-around, shift, list and the inverse of letter-tension-poles in all models."
  (flet ((tl (x &rest keys) (multiple-value-list (apply #'tension->letter x keys))))
    (let* ((checks
             (list (list "A 113" (equal (subseq (tl 113 :shift 0) 0 3) '("A" 0 "L")))
                   (list "L 179" (equal (subseq (tl 179 :shift 0) 0 3) '("L" 0 "A")))
                   (list "B +2" (equal (subseq (tl 121 :shift 0) 0 2) '("B" 2)))
                   (list "remis 116" (equal (subseq (tl 116 :shift 0) 0 2) '("B" -3)))
                   (list "zawiniecie 244" (equal (subseq (tl 244 :shift 0) 0 3) '("A" -1 "L")))
                   (list "shift 2" (equal (subseq (tl 115 :shift 2) 0 2) '("A" 0)))
                   (list "przeniesienie 250" (equal (tl 250 :shift 0) (tl 118 :shift 0)))
                   (list "przeniesienie 60" (let ((r (tl 60 :shift 0)))
                                              (and (equal (subseq r 0 2) '("N" 1)) (= (fifth r) 192))))
                   (list "przeniesienie :helix" (= (fifth (tl 300 :model :helix :shift 0)) 168))
                   (list "lista" (equal (first (tl '(113 179 215) :shift 0)) '("A" "L" "S")))
                   (list "barwa A" (< (abs (- (fourth (tl 113 :shift 0)) (spiral-letter-hue 0))) 1d-6))
                   (list "odwrotnosc"
                         (loop for (model needle) in '((:circle :up) (:interleave :up) (:helix :up) (:helix :down))
                               always (loop for k from 0 below 22
                                            for letter = (string (char *spiral-letters* k))
                                            for p1 = (first (letter-tension-poles letter :model model :needle needle :shift 0))
                                            always (equal (subseq (tl p1 :model model :needle needle :shift 0) 0 2)
                                                          (list letter 0)))))))
           (bad (loop for c in checks unless (second c) collect (first c))))
      (om-print (if bad
                    (format nil "tension->letter-test: BLAD w: ~{~a~^, ~}" bad)
                    "tension->letter-test: OK"))
      (null bad))))
