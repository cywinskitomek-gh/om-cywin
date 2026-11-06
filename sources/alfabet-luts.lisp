;;; ===========================================================================
;;;  alfabet-luts.lisp  --  seria tablic LUT (.cube) dla cyklu "Alfabet Torunski"
;;;
;;;  Grading "kinowy", dwubiegunowy: kazda litera ma swoj kolor (biegun 1)
;;;  i kolor dokladnie naprzeciwko na kole H (biegun 2).
;;;   - wzmocnienie saturacji w obu biegunach, plynne przejscia,
;;;   - pozostale kolory lekko przyciagane do blizszego bieguna,
;;;   - zachowana jasnosc (luma Rec.2020),
;;;   - split toning: swiatla w strone koloru litery, cienie w strone przeciwnego,
;;;   - opcjonalna zmiana ekspozycji (w swietle liniowym) i lagodna krzywa S.
;;;  Wejscie i wyjscie LUT-a: Rec.2020 HLG (Final Cut: Custom LUT, obie
;;;  przestrzenie ustawione na Rec.2020 HLG).
;;;
;;;  Wartosci domyslne = komplet "kino HLG pelna ekspozycja".
;;;  (Pierwszy komplet "kino HLG" mial :exposure 0.6.)
;;;
;;;  UZYCIE (Listener OpenMusic / LispWorks):
;;;
;;;    (load (compile-file "/sciezka/do/alfabet-luts.lisp"))
;;;    (alfabet-lut-test)                         ; kontrola poprawnosci, ~1 s
;;;    (make-alfabet-luts "~/Desktop/LUT/")
;;;
;;;  Przyklady zmian:
;;;    (make-alfabet-luts "~/Desktop/LUT_ciemne/" :exposure 0.6 :name "kino_HLG")
;;;    (make-alfabet-luts "~/Desktop/LUT_test/" :only '("W" "Z") :size 33)
;;;    (make-alfabet-luts "~/Desktop/LUT_mocne/" :sat-letter 1.6 :pull 0.25)
;;;
;;;  compile-file jest wazne: skompilowany kod liczy 22 tablice 65^3
;;;  w kilkanascie sekund, interpretowany - wielokrotnie dluzej.
;;; ===========================================================================

(in-package :om)

(defparameter *alfabet-torunski-letters*
  '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K"
    "L" "M" "N" "O" "P" "R" "S" "T" "U" "W" "Z")
  "22 litery cyklu; kolejnosc wyznacza kolor (A = 352,6 st., dalej malejaco).")

(defmethod! alfabet-torunski-letters ()
  :icon 9109
  :outdoc '("the 22 letters of the Alfabet Torunski cycle")
  :doc "List of the 22 letters of the Alfabet Torunski cycle (\"A\" \"B\" ... \"Z\") in palette order.
This is not the full alphabet: Q, V, X, Y are missing; letters with diacritics share the colours of their base letters.
Returns a copy of the constant *ALFABET-TORUNSKI-LETTERS*."
  (copy-list *alfabet-torunski-letters*))

;;; --------------------------- stale -----------------------------------------

(defparameter +hlg-a+ 0.17883277d0)
(defparameter +hlg-b+ 0.28466892d0)
(defparameter +hlg-c+ 0.55991073d0)
(defparameter +ky-r+ 0.2627d0)          ; luma Rec.2020
(defparameter +ky-g+ 0.6780d0)
(defparameter +ky-b+ 0.0593d0)

;;; --------------------------- narzedzia -------------------------------------

(declaim (inline clip01))

(defun clip01 (x)   ; ukryj
  (max 0d0 (min 1d0 x)))

(defun hlg->lin (e)   ; ukryj
  (let ((e (clip01 e)))
    (if (<= e 0.5d0)
        (/ (* e e) 3d0)
        (/ (+ (exp (/ (- e +hlg-c+) +hlg-a+)) +hlg-b+) 12d0))))

(defun lin->hlg (x)   ; ukryj
  (let ((x (clip01 x)))
    (if (<= x (/ 1d0 12d0))
        (sqrt (* 3d0 x))
        (+ (* +hlg-a+ (log (max (- (* 12d0 x) +hlg-b+) 1d-9))) +hlg-c+))))

(defun luma (r g b)   ; ukryj
  (+ (* +ky-r+ r) (* +ky-g+ g) (* +ky-b+ b)))

(defun smoothstep (x a b)   ; ukryj
  (let ((tt (clip01 (/ (- x a) (- b a)))))
    (* tt tt (- 3d0 (* 2d0 tt)))))

(defun rgb->hsv (r g b)   ; ukryj
  "Returns (values h s v), h in degrees 0..360."
  (let* ((mx (max r g b)) (mn (min r g b)) (d (- mx mn)) (h 0d0))
    (when (> d 1d-9)
      (setf h (cond ((= mx r) (mod (/ (- g b) d) 6d0))
                    ((= mx g) (+ (/ (- b r) d) 2d0))
                    (t        (+ (/ (- r g) d) 4d0)))))
    (values (* h 60d0)
            (if (> mx 1d-9) (/ d mx) 0d0)
            mx)))

(defun hsv->rgb (h s v)   ; ukryj
  "Returns (values r g b)."
  (let* ((hh (/ (mod h 360d0) 60d0))
         (fl (floor hh))
         (i (mod fl 6))
         (f (- hh fl))
         (p (* v (- 1d0 s)))
         (q (* v (- 1d0 (* s f))))
         (tt (* v (- 1d0 (* s (- 1d0 f))))))
    (case i
      (0 (values v tt p))
      (1 (values q v p))
      (2 (values p v tt))
      (3 (values p q v))
      (4 (values tt p v))
      (t (values v p q)))))

(defun hue-diff (h c)   ; ukryj
  "Signed angle difference in the range -180..180."
  (- (mod (+ (- h c) 180d0) 360d0) 180d0))

(defun pole-weight (d core fall)   ; ukryj
  "1 in the core of the pole, cosine transition, 0 outside it."
  (let ((a (abs d)))
    (cond ((<= a core) 1d0)
          ((< a (+ core fall)) (* 0.5d0 (+ 1d0 (cos (/ (* pi (- a core)) fall)))))
          (t 0d0))))

(defun pull-taper (a half-gap)   ; ukryj
  "Taper of the colour pull: 1 up to halfway to the border between the poles,
then smoothly to 0 at the border itself (HALF-GAP deg. from the pole). This way
colours on both sides of the border do not jump to different poles."
  (- 1d0 (smoothstep a (* 0.5d0 half-gap) half-gap)))

(defun hue-chroma (hue)   ; ukryj
  "Chroma of the pure colour of hue HUE (without lightness): (values r g b)."
  (multiple-value-bind (r g b) (hsv->rgb hue 1d0 1d0)
    (let ((y (luma r g b)))
      (values (- r y) (- g y) (- b y)))))

(defun letter-center (k)   ; ukryj
  "Hue of the letter with index k (0 = A): (361 - k*360/22) - 360/44, mod 360."
  (let ((step (/ 360d0 22d0)))
    (mod (- (- 361d0 (* step k)) (/ step 2d0)) 360d0)))

;;; --------------------------- grading jednego koloru ------------------------

(defun alfabet-grade (r g b k &key (exposure 1d0) (contrast 0.22d0)   ; ukryj
                                   (core 20d0) (fall 35d0) (pull 0.15d0)
                                   (sat-letter 1.40d0) (sat-opposite 1.25d0)
                                   (sat-other 0.9d0) (split 0.06d0))
  "Processes one HLG colour (r g b in 0..1) for the letter with index k.
Returns (values r g b)."
  (let* ((exposure (float exposure 1d0)) (contrast (float contrast 1d0))
         (core (float core 1d0)) (fall (float fall 1d0)) (pull (float pull 1d0))
         (s1 (float sat-letter 1d0)) (s2 (float sat-opposite 1d0))
         (s0 (float sat-other 1d0)) (split (float split 1d0))
         (p1 (letter-center k))
         (p2 (mod (+ p1 180d0) 360d0)))
    (flet ((tone (x)
             ;; ekspozycja w swietle liniowym, potem lagodna krzywa S
             (let ((x (lin->hlg (* (hlg->lin (float x 1d0)) exposure))))
               (clip01 (+ x (* contrast (- x 0.45d0)
                                (- 1d0 (abs (- (* 2d0 (clip01 x)) 1d0)))))))))
      (let* ((r (tone r)) (g (tone g)) (b (tone b))
             (y0 (luma r g b)))
        (multiple-value-bind (h s v) (rgb->hsv r g b)
          (let* ((d1 (hue-diff h p1)) (d2 (hue-diff h p2))
                 (w1 (pole-weight d1 core fall))
                 (w2 (pole-weight d2 core fall))
                 (dn (if (<= (abs d1) 90d0) d1 d2))
                 (h2 (- h (* pull dn (pull-taper (abs dn) 90d0) (- 1d0 (max w1 w2)))))
                 (gain (+ s0 (* (- s1 s0) w1) (* (- s2 s0) w2 (- 1d0 w1)))))
            (multiple-value-bind (nr ng nb) (hsv->rgb h2 (clip01 (* s gain)) v)
              ;; przywrocenie pierwotnej jasnosci
              (let ((dy (- y0 (luma nr ng nb))))
                (incf nr dy) (incf ng dy) (incf nb dy))
              ;; split toning
              (let ((hi (smoothstep y0 0.45d0 0.85d0))
                    (lo (- 1d0 (smoothstep y0 0.1d0 0.45d0))))
                (multiple-value-bind (c1r c1g c1b) (hue-chroma p1)
                  (multiple-value-bind (c2r c2g c2b) (hue-chroma p2)
                    (values (clip01 (+ nr (* split (+ (* hi c1r) (* lo c2r)))))
                            (clip01 (+ ng (* split (+ (* hi c1g) (* lo c2g)))))
                            (clip01 (+ nb (* split (+ (* hi c1b) (* lo c2b))))))))))))))))

;;; --------------------------- zapis tablic ----------------------------------

(defun ensure-slash (dir)   ; ukryj
  (let ((s (namestring dir)))
    (if (char= (char s (1- (length s))) #\/) s (concatenate 'string s "/"))))

(defmethod! make-alfabet-luts ((directory string) &rest grade-args
                          &key (size 65) (name "kino_HLG_pelna_ekspozycja")
                               (only nil) (letters *alfabet-torunski-letters*)
                               exposure contrast core fall pull
                               sat-letter sat-opposite sat-other split)
  :icon 9109
  :doc "Creates the files AlfabetTorunski_<NAME>_<nr>_<letter>.cube in DIRECTORY.
SIZE - grid density (65 as before; 33 for preview, e.g. in Blackmagic Camera).
ONLY - list of letters to make, e.g. '(\"W\" \"Z\"); all by default.
Other parameters as in ALFABET-GRADE. Returns the list of created files."

  (declare (ignore exposure contrast core fall pull
                   sat-letter sat-opposite sat-other split))
  (let ((dir (ensure-slash directory))
        (gargs (loop for (key val) on grade-args by #'cddr
                     unless (member key '(:size :name :only :letters))
                       append (list key val)))
        (n1 (float (1- size) 1d0))
        (files '()))
    (ensure-directories-exist dir)
    (loop for letter in letters
          for k from 0
          when (or (null only) (member letter only :test #'string-equal))
            do (let* ((c (letter-center k))
                      (path (format nil "~aAlfabetTorunski_~a_~2,'0d_~a.cube"
                                    dir name (1+ k) letter)))
                 (with-open-file (out path :direction :output
                                           :if-exists :supersede
                                           :if-does-not-exist :create)
                   (format out "TITLE \"Alfabet Torunski ~a ~a\"~%" letter
                           (substitute #\Space #\_ name))
                   (format out "# wejscie i wyjscie: Rec.2020 HLG; bieguny ~,1f i ~,1f st.~%"
                           c (mod (+ c 180d0) 360d0))
                   (format out "LUT_3D_SIZE ~d~%DOMAIN_MIN 0.0 0.0 0.0~%DOMAIN_MAX 1.0 1.0 1.0~%"
                           size)
                   ;; kolejnosc .cube: R zmienia sie najszybciej, potem G, potem B
                   (dotimes (ib size)
                     (dotimes (ig size)
                       (dotimes (ir size)
                         (multiple-value-bind (r g b)
                             (apply #'alfabet-grade (/ ir n1) (/ ig n1) (/ ib n1) k gargs)
                           (format out "~,6f ~,6f ~,6f~%" r g b))))))
                 (om-print (format nil "~&~a  (~,1f / ~,1f st.)~%" path c (mod (+ c 180d0) 360d0)))
                 (push path files)))
    (nreverse files)))


;;; --------------------------- test ------------------------------------------
;;; Wzorce policzone niezaleznie (ten sam algorytm w Pythonie), 12.10.2026,
;;; po wprowadzeniu plynnego przyciagania (pull-taper).

(defparameter *alfabet-reference*
  ;; litera, wejscie RGB (HLG), oczekiwane wyjscie przy wartosciach domyslnych
  '(("A" 0.75 0.45 0.30  0.879167 0.420006 0.197112)
    ("A" 0.40 0.55 0.80  0.374301 0.578603 0.805013)
    ("A" 0.20 0.60 0.25  0.191013 0.616873 0.279279)
    ("A" 0.90 0.90 0.88  0.964405 0.903086 0.896201)
    ("A" 0.10 0.08 0.12  0.040009 0.083728 0.108586)
    ("A" 0.60 0.30 0.55  0.631942 0.279819 0.549605)
    ("A" 0.56 0.70 0.28  0.615287 0.716756 0.294202)
    ("W" 0.75 0.45 0.30  0.887870 0.418299 0.178079)
    ("W" 0.40 0.55 0.80  0.357770 0.576719 0.899781)
    ("W" 0.20 0.60 0.25  0.208555 0.611370 0.264481)
    ("W" 0.90 0.90 0.88  0.947382 0.912273 0.866568)
    ("W" 0.10 0.08 0.12  0.054280 0.076011 0.133596)
    ("W" 0.60 0.30 0.55  0.601210 0.292463 0.541179)
    ("W" 0.34 0.70 0.28  0.360495 0.716653 0.282260)))

(defun alfabet-lut-test ()   ; ukryj
  "Compares the result with reference values. Returns T if everything matches."
  (let ((worst 0d0))
    (dolist (row *alfabet-reference*)
      (destructuring-bind (letter r g b er eg eb) row
        (let ((k (position letter *alfabet-torunski-letters* :test #'string=)))
          (multiple-value-bind (nr ng nb) (alfabet-grade (float r 1d0) (float g 1d0)
                                                         (float b 1d0) k)
            (setf worst (max worst (abs (- nr er)) (abs (- ng eg)) (abs (- nb eb))))))))
    (om-print (format nil "alfabet-lut-test: najwieksza roznica od wzorca ~,7f -> ~a"
                      worst (if (< worst 1d-5) "OK" "BLAD")))
    (< worst 1d-5)))
