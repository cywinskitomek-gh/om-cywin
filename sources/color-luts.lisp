;;; ===========================================================================
;;;  color-luts.lisp  --  ogolna tablica LUT (.cube) z dowolnymi kolorami
;;;
;;;  Uogolnienie alfabet-luts.lisp: zamiast litery podajesz liste kolorow
;;;  do podbicia (2-3 albo wiecej, w dowolnym ukladzie na kole H).
;;;  Kazdy kolor ("biegun") ma wlasny odcien, wlasne podbicie saturacji
;;;  i opcjonalnie wlasna szerokosc.
;;;
;;;  WYMAGA funkcji pomocniczych z alfabet-luts.lisp (clip01, hlg->lin,
;;;  lin->hlg, rgb->hsv, hsv->rgb, hue-diff, pole-weight, smoothstep),
;;;  wiec w om-cywin.lisp "color-luts" musi byc PO "alfabet-luts".
;;;
;;;  BIEGUNY (argument POLES), kazdy jako lista:
;;;     (odcien podbicie)            np. (30 1.5)
;;;     (odcien podbicie szerokosc)  np. (120 1.2 30)
;;;   odcien w stopniach 0-360 (0 czerwien, 60 zolty, 120 zielen,
;;;   180 cyjan, 240 niebieski, 300 magenta); podbicie: 1.0 = bez zmian;
;;;   szerokosc = polowa rdzenia w stopniach (domyslnie :core 20).
;;;   Przyklad: '((30 1.5) (200 1.4) (120 1.2 30))
;;;
;;;  KLUCZE (jak w alfabecie):
;;;   :exposure   1.0   ekspozycja w swietle liniowym (0.6 = ok. -3/4 EV)
;;;   :contrast   0.22  lagodna krzywa S
;;;   :core       20    domyslna polowa rdzenia bieguna (st.)
;;;   :fall       35    szerokosc plynnego przejscia (st.)
;;;   :pull       0.15  przyciaganie pozostalych barw do najblizszego bieguna
;;;   :sat-other  0.9   saturacja kolorow poza biegunami
;;;   :split      0.06  sila split toningu (0 = wylacz)
;;;   :highlights nil   odcien swiatel; nil = pierwszy biegun
;;;   :shadows    nil   odcien cieni; nil = drugi biegun
;;;                     (przy jednym biegunie: kolor przeciwny)
;;;   :transfer   :hlg  :hlg = Rec.2020 HLG (klipy z iPhone'a w HDR),
;;;                     :sdr = Rec.709 SDR (zwykle zdjecia, grafika)
;;;   :size       65    gestosc siatki (33 dla Blackmagic Camera)
;;;   :title      nil   tytul w pliku; nil = nazwa pliku
;;;
;;;  UZYCIE w Listenerze:
;;;   (make-color-lut "~/Desktop/LUT/zachod.cube"
;;;                   '((30 1.5) (200 1.4) (120 1.2 30)) :exposure 0.8)
;;;   (color-lut-test)   ; kontrola poprawnosci
;;;
;;;  Przy biegunach (c 1.4) i (c+180 1.25) wynik jest identyczny
;;;  z make-alfabet-luts dla litery o odcieniu c.
;;; ===========================================================================

(in-package :om)

(defun transfer-sdr-p (transfer)   ; ukryj
  (and transfer (string-equal (string transfer) "SDR")))

(defun luma-t (sdr r g b)   ; ukryj
  (if sdr
      (+ (* 0.2126d0 r) (* 0.7152d0 g) (* 0.0722d0 b))
      (+ (* 0.2627d0 r) (* 0.6780d0 g) (* 0.0593d0 b))))

(defun hue-chroma-t (sdr hue)   ; ukryj
  "Chroma of the pure colour of hue HUE, without lightness: (values r g b)."
  (multiple-value-bind (r g b) (hsv->rgb (float hue 1d0) 1d0 1d0)
    (let ((y (luma-t sdr r g b)))
      (values (- r y) (- g y) (- b y)))))

(defun normalize-poles (poles core)   ; ukryj
  "Turns the list of poles into a list of (hue boost core) as double numbers."
  (let ((poles (if (numberp (first poles)) (list poles) poles)))  ; jeden biegun bez nawiasow
    (mapcar #'(lambda (p)
                (list (mod (float (first p) 1d0) 360d0)
                      (float (or (second p) 1.3d0) 1d0)
                      (float (or (third p) core) 1d0)))
            poles)))

(defun pole-half-gap (p d hues)   ; ukryj
  "Distance (deg.) from pole P to the border with the neighbouring pole on the side
D points to (hue minus pole). HUES - hues of all poles."
  (let ((sg (if (>= d 0d0) 1d0 -1d0))
        (gap 360d0))
    (dolist (q hues)
      (let ((dist (mod (* sg (- q p)) 360d0)))
        (when (> dist 1d-9) (setf gap (min gap dist)))))
    (* 0.5d0 gap)))

(defun color-grade-1 (r g b poles sdr exposure contrast fall pull s0 split   ; ukryj
                      hr hg hb sr sg sb)
  "One input colour -> (values r g b). POLES already normalised,
HR HG HB / SR SG SB - chroma of highlights / shadows."
  (flet ((tone (x)
           (let* ((lin (if sdr (expt (clip01 (float x 1d0)) 2.4d0) (hlg->lin (float x 1d0))))
                  (x (if sdr (expt (clip01 (* lin exposure)) (/ 1d0 2.4d0))
                         (lin->hlg (* lin exposure)))))
             (clip01 (+ x (* contrast (- x 0.45d0)
                              (- 1d0 (abs (- (* 2d0 (clip01 x)) 1d0)))))))))
    (let* ((r (tone r)) (g (tone g)) (b (tone b))
           (y0 (luma-t sdr r g b)))
      (multiple-value-bind (h s v) (rgb->hsv r g b)
        (let ((acc 0d0) (sumw 0d0) (maxw 0d0) (best 1000d0) (bd 0d0) (bp 0d0))
          (dolist (p poles)
            (destructuring-bind (hue gain core) p
              (let* ((d (hue-diff h hue))
                     (w (pole-weight d core fall)))
                (incf acc (* w (- gain s0)))
                (incf sumw w)
                (setf maxw (max maxw w))
                (when (< (abs d) best) (setf best (abs d) bd d bp hue)))))
          (let ((gain (+ s0 (/ acc (max 1d0 sumw))))
                (h2 (- h (* pull bd
                           (pull-taper (abs bd) (pole-half-gap bp bd (mapcar #'first poles)))
                           (- 1d0 maxw)))))
            (multiple-value-bind (nr ng nb) (hsv->rgb h2 (clip01 (* s gain)) v)
              (let ((dy (- y0 (luma-t sdr nr ng nb))))
                (incf nr dy) (incf ng dy) (incf nb dy))
              (let ((hi (smoothstep y0 0.45d0 0.85d0))
                    (lo (- 1d0 (smoothstep y0 0.1d0 0.45d0))))
                (values (clip01 (+ nr (* split (+ (* hi hr) (* lo sr)))))
                        (clip01 (+ ng (* split (+ (* hi hg) (* lo sg)))))
                        (clip01 (+ nb (* split (+ (* hi hb) (* lo sb))))))))))))))

(defun color-grade-setup (poles core highlights shadows)   ; ukryj
  "Returns (values poles hh sh): normalised poles and hues of highlights/shadows."
  (let* ((pl (normalize-poles poles (float core 1d0)))
         (hh (if highlights (float highlights 1d0) (first (first pl))))
         (sh (cond (shadows (float shadows 1d0))
                   ((second pl) (first (second pl)))
                   (t (mod (+ (first (first pl)) 180d0) 360d0)))))
    (values pl hh sh)))

(defun color-grade (r g b poles &key (exposure 1d0) (contrast 0.22d0) (core 20d0)   ; ukryj
                                     (fall 35d0) (pull 0.15d0) (sat-other 0.9d0)
                                     (split 0.06d0) highlights shadows (transfer :hlg))
  "Processes one colour (r g b in 0..1). Returns (values r g b)."
  (let ((sdr (transfer-sdr-p transfer)))
    (multiple-value-bind (pl hh sh) (color-grade-setup poles core highlights shadows)
      (multiple-value-bind (hr hg hb) (hue-chroma-t sdr hh)
        (multiple-value-bind (sr sg sb) (hue-chroma-t sdr sh)
          (color-grade-1 r g b pl sdr (float exposure 1d0) (float contrast 1d0)
                         (float fall 1d0) (float pull 1d0) (float sat-other 1d0)
                         (float split 1d0) hr hg hb sr sg sb))))))

(defmethod! make-color-lut ((path string) (poles list)
                            &key (exposure 1.0) (contrast 0.22) (core 20) (fall 35)
                                 (pull 0.15) (sat-other 0.9) (split 0.06)
                                 (highlights nil) (shadows nil) (transfer :hlg)
                                 (size 65) (title nil))
  :icon 9110
  :initvals '("~/Desktop/LUT/kolor.cube" ((30 1.5) (200 1.4)))
  :indoc '("path of the .cube file" "poles: ((hue boost [width]) ...)")
  :doc "Creates one .cube LUT that boosts chosen colours.
POLES: list of (hue boost [width]), e.g. ((30 1.5) (200 1.4) (120 1.2 30)).
Keys: exposure contrast core fall pull sat-other split highlights shadows
transfer (:hlg / :sdr) size title. Returns the file path."
  (let* ((sdr (transfer-sdr-p transfer))
         (n1 (float (1- size) 1d0))
         (ttl (or title (pathname-name path))))
    (multiple-value-bind (pl hh sh) (color-grade-setup poles core highlights shadows)
      (multiple-value-bind (hr hg hb) (hue-chroma-t sdr hh)
        (multiple-value-bind (sr sg sb) (hue-chroma-t sdr sh)
          (ensure-directories-exist path)
          (with-open-file (out path :direction :output :if-exists :supersede
                                    :if-does-not-exist :create)
            (format out "TITLE \"~a\"~%" ttl)
            (format out "# wejscie i wyjscie: ~a~%"
                    (if sdr "Rec.709 SDR" "Rec.2020 HLG"))
            (format out "# bieguny (odcien podbicie rdzen):~{ (~{~,1f~^ ~})~}~%" pl)
            (format out "# exposure ~,2f sat-other ~,2f pull ~,2f split ~,2f, swiatla ~,1f st., cienie ~,1f st.~%"
                    exposure sat-other pull split hh sh)
            (format out "LUT_3D_SIZE ~d~%DOMAIN_MIN 0.0 0.0 0.0~%DOMAIN_MAX 1.0 1.0 1.0~%" size)
            (let ((ex (float exposure 1d0)) (co (float contrast 1d0)) (fa (float fall 1d0))
                  (pu (float pull 1d0)) (s0 (float sat-other 1d0)) (sp (float split 1d0)))
              ;; kolejnosc .cube: R najszybciej, potem G, potem B
              (dotimes (ib size)
                (dotimes (ig size)
                  (dotimes (ir size)
                    (multiple-value-bind (r g b)
                        (color-grade-1 (/ ir n1) (/ ig n1) (/ ib n1) pl sdr ex co fa pu s0 sp
                                       hr hg hb sr sg sb)
                      (format out "~,6f ~,6f ~,6f~%" r g b)))))))
          (om-print (format nil "~a" path))
          path)))))

;;; --------------------------- test ------------------------------------------

(defparameter *color-lut-reference*
  ;; (bieguny klucze) i pary: wejscie -> oczekiwane wyjscie
  ;; wzorce z 12.10.2026 (plynne przyciaganie, pull-taper)
  '((((30 1.5) (200 1.4) (120 1.2 30)) (:exposure 0.8 :sat-other 0.6)
     ((0.75 0.45 0.30  0.870470 0.353097 0.131936)
      (0.40 0.55 0.80  0.288860 0.516511 0.912732)
      (0.20 0.60 0.25  0.101101 0.595392 0.153808)
      (0.90 0.90 0.88  0.908112 0.877855 0.823577)
      (0.10 0.08 0.12  0.039386 0.071824 0.110729)
      (0.60 0.30 0.55  0.474639 0.289505 0.438480)
      (0.64 0.28 0.70  0.507238 0.282061 0.559583)))
    (((15 1.6) (250 1.3)) (:pull 0.25 :split 0.08 :transfer :sdr)
     ((0.75 0.45 0.30  0.938776 0.416723 0.151138)
      (0.40 0.55 0.80  0.392944 0.563489 0.888179)
      (0.20 0.60 0.25  0.213082 0.613832 0.249190)
      (0.90 0.90 0.88  0.970447 0.907763 0.872808)
      (0.10 0.08 0.12  0.092715 0.056470 0.182750)
      (0.60 0.30 0.55  0.601675 0.288510 0.560290)
      (0.28 0.70 0.35  0.309607 0.716436 0.349827)))))

(defun color-lut-test ()   ; ukryj
  "Compares the result with reference values. Returns T if everything matches."
  (let ((worst 0d0))
    (dolist (tc *color-lut-reference*)
      (destructuring-bind (poles keys rows) tc
        (dolist (row rows)
          (destructuring-bind (r g b er eg eb) row
            (multiple-value-bind (nr ng nb)
                (apply #'color-grade (float r 1d0) (float g 1d0) (float b 1d0) poles keys)
              (setf worst (max worst (abs (- nr er)) (abs (- ng eg)) (abs (- nb eb)))))))))
    (om-print (format nil "Najwieksza roznica od wzorca: ~,7f -> ~a"
                      worst (if (< worst 1d-5) "OK" "BLAD - prosze przyslac ten wynik")))
    (< worst 1d-5)))
