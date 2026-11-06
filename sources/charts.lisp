(in-package :om)

;;; ==========================================================================
;;;  my-classes.lisp  -  wlasne klasy biblioteki om-cywin
;;;
;;;  PIE-CHART   wykres kolowy (jak BPF: pudelko-klasa z podgladem w patchu
;;;              i edytorem po dwukliku)
;;;  PIE-PERCENTAGES   udzialy procentowe wycinkow
;;;
;;;  Kolory domyslne korzystaja z HSV->RGB z alfabet-luts.lisp
;;;  ("my-classes" laduje sie po "alfabet-luts").
;;; ==========================================================================

(defclass! pie-chart ()
  ((pie-values :initform '(3 2 1) :initarg :pie-values :accessor pie-values :type list
               :documentation "slice values (positive numbers; others are skipped)")
   (pie-labels :initform nil :initarg :pie-labels :accessor pie-labels :type list
               :documentation "slice labels (strings, symbols or numbers; nil = no labels)")
   (pie-colors :initform nil :initarg :pie-colors :accessor pie-colors :type list
               :documentation "slice colours: list of (r g b) in the range 0..1; nil = automatic palette"))
  (:icon 9121)
  (:documentation "
PIE-CHART: a pie chart.

<pie-values> list of (positive) values - each one is a slice.
<pie-labels> optional slice labels (shown in the legend in the editor).
<pie-colors> optional colours (r g b) 0..1; missing ones are chosen automatically.

The first slice starts at the top, the next ones go clockwise.
Preview in the box: turn on content display (key m). Double-click opens the editor with a legend."))

;;; --------------------------- obliczenia -----------------------------------

(defun pie-default-rgb (i n)   ; ukryj
  "Automatic colour of slice I of N: evenly spread hues, as on a colour wheel."
  (multiple-value-bind (r g b)
      (hsv->rgb (* 360d0 (/ i (max 1 n))) 0.55d0 0.9d0)
    (list r g b)))

(defun pie-color (self i n)   ; ukryj
  "OM colour (omcolor) of slice I."
  (let ((c (nth i (pie-colors self))))
    (cond ((om-color-p c) c)
          ((and (consp c) (>= (length c) 3) (every #'numberp (subseq c 0 3)))
           (om-make-color (float (first c)) (float (second c)) (float (third c))))
          (t (let ((d (pie-default-rgb i n)))
               (om-make-color (float (first d)) (float (second d)) (float (third d))))))))

(defun pie-slices (self)   ; ukryj
  "List of slices: (index value share start-angle slice-angle label), angles in degrees,
0 = top, increasing clockwise. Skips values that are not numbers > 0."
  (let* ((vals (pie-values self))
         (items (loop for v in vals
                      for i from 0
                      when (and (numberp v) (> v 0))
                        collect (list i v (nth i (pie-labels self)))))
         (total (reduce #'+ items :key #'second :initial-value 0))
         (angle 0d0))
    (when (> total 0)
      (loop for (i v lab) in items
            for frac = (/ v total)
            for sweep = (* 360d0 frac)
            collect (prog1 (list i v (float frac 1d0) angle sweep lab)
                      (incf angle sweep))))))

(defmethod! pie-percentages ((self pie-chart))
  :icon 9120
  :indoc '("pie-chart")
  :doc "Returns the percentage shares of the consecutive values of a pie chart (sum = 100).
Skipped values (non-numbers, <= 0) give 0."
  (let ((slices (pie-slices self)))
    (loop for v in (pie-values self)
          for i from 0
          collect (let ((s (find i slices :key #'first)))
                    (if s (* 100d0 (third s)) 0d0)))))

;;; --------------------------- rysowanie ------------------------------------

(defun pie-arc-points (cx cy r a0 sweep)   ; ukryj
  "Wielokat wycinka: srodek + punkty luku co ok. 3 stopnie."
  (let ((steps (max 2 (ceiling sweep 3d0))))
    (cons (list (round cx) (round cy))
          (loop for k from 0 to steps
                for a = (* (/ pi 180d0) (+ a0 (* sweep (/ k steps))))
                collect (list (round (+ cx (* r (sin a))))
                              (round (- cy (* r (cos a)))))))))

(defun draw-pie-chart (self view x x1 y y1)   ; ukryj
  "Draws the pie inscribed in the rectangle (x..x1, y..y1). Call inside om-with-focused-view."
  (let* ((slices (pie-slices self))
         (n (length (pie-values self)))
         (cx (/ (+ x x1) 2d0))
         (cy (/ (+ y y1) 2d0))
         (r (max 1d0 (- (/ (min (- x1 x) (- y1 y)) 2d0) 1))))
    (if (null slices)
        (om-with-fg-color view *om-gray-color*
          (om-draw-ellipse (round cx) (round cy) (round r) (round r)))
        (progn
          (dolist (s slices)
            (om-with-fg-color view (pie-color self (first s) n)
              (if (>= (fifth s) 359.999d0)
                  (om-fill-ellipse (round cx) (round cy) (round r) (round r))
                  (om-fill-polygon (pie-arc-points cx cy r (fourth s) (fifth s))))))
          ;; biale linie miedzy wycinkami
          (when (> (length slices) 1)
            (om-with-fg-color view *om-white-color*
              (dolist (s slices)
                (let ((a (* (/ pi 180d0) (fourth s))))
                  (om-draw-line (round cx) (round cy)
                                (round (+ cx (* r (sin a))))
                                (round (- cy (* r (cos a)))))))))))))

(defmethod draw-obj-in-rect ((self pie-chart) x x1 y y1 edparams view)
  (declare (ignore edparams))
  (om-with-focused-view view
    (draw-pie-chart self view x x1 y y1)))

(defmethod draw-mini-view ((self t) (value pie-chart))
  (draw-obj-in-rect value 4 (- (w self) 4) 4 (- (h self) 4) nil self))

;;; --------------------------- edytor ---------------------------------------

(defmethod Class-has-editor-p ((self pie-chart)) t)
(defmethod get-editor-class ((self pie-chart)) 'pie-chart-editor)
(defmethod get-win-ed-size ((self pie-chart)) (om-make-point 560 405))

(defclass pie-chart-editor (editorview)
  ((drag :initform nil :accessor pie-drag)                  ; numer przeciaganej granicy albo nil
   (show-handles :initform t :accessor pie-show-handles)    ; czy rysowac szare kropki
   (handles-box :initform nil :accessor pie-handles-box)))  ; check-box na dole okna

(defmethod initialize-instance :after ((self pie-chart-editor) &rest args)
  (declare (ignore args))
  (om-add-subviews self
    (setf (pie-handles-box self)
          (om-make-dialog-item 'om-check-box (om-make-point 15 (- (h self) 24)) (om-make-point 200 16)
                               "Kropki na granicach"
                               :di-action (om-dialog-item-act item
                                            (setf (pie-show-handles self) (om-checked-p item))
                                            (om-invalidate-view self))
                               :font *om-default-font1*
                               :checked-p (pie-show-handles self)))))

(defmethod update-subviews ((self pie-chart-editor))
  (when (pie-handles-box self)
    (om-set-view-position (pie-handles-box self) (om-make-point 15 (- (h self) 24)))))

(defun pie-editor-circle (self)   ; ukryj
  "Polozenie kola w edytorze: srodek x, srodek y, promien, bok kwadratu, wysokosc okna."
  (let* ((size (om-view-size self))
         (w (om-point-h size))
         (h (- (om-point-v size) 25))   ; miejsce na check-box na dole
         (side (- (min h (if (> w (* 1.3 h)) h w)) 30)))
    (values (+ 15 (/ side 2d0)) (+ 15 (/ side 2d0)) (- (/ side 2d0) 1) side h)))

;;; --------------------------- zmiana myszka --------------------------------

(defun pie-point-angle (cx cy px py)   ; ukryj
  "Kat punktu (px py) wzgledem srodka: 0 = gora, rosnaco zgodnie z ruchem wskazowek zegara."
  (let ((a (* (/ 180d0 pi) (atan (float (- px cx) 1d0) (float (- cy py) 1d0)))))
    (if (< a 0) (+ a 360d0) a)))

(defun pie-angle-diff (a b)   ; ukryj
  "Odleglosc katowa (0..180)."
  (let ((d (mod (- a b) 360d0)))
    (min d (- 360d0 d))))

(defun pie-hit-boundary (obj cx cy r px py)   ; ukryj
  "Number of the border near the point (slice position in pie-slices, from 1) or nil."
  (let* ((slices (pie-slices obj))
         (d (sqrt (+ (expt (- px cx) 2) (expt (- py cy) 2)))))
    (when (and (> (length slices) 1) (> d 6) (< d (+ r 8)))
      (let* ((a (pie-point-angle cx cy px py))
             (best nil)
             (bestdiff (min 20d0 (max 3d0 (* (/ 180d0 pi) (/ 8d0 d))))))
        (loop for s in (cdr slices)
              for j from 1
              for diff = (pie-angle-diff a (fourth s))
              when (<= diff bestdiff) do (setf best j bestdiff diff))
        best))))

(defun pie-from-hundredths (n)   ; ukryj
  "N hundredths as a number: an integer if possible, otherwise a float."
  (let ((q (/ n 100)))
    (if (integerp q) q (/ n 100.0))))

(defun pie-split-pair (psum frac)   ; ukryj
  "Splits PSUM in the proportion FRAC (0..1) into two values with precision 0.01, each >= 0.01."
  (let* ((n (max 2 (round (* psum 100))))
         (k (min (1- n) (max 1 (round (* n frac))))))
    (values (pie-from-hundredths k) (pie-from-hundredths (- n k)))))

(defun pie-drag-boundary (obj j a)   ; ukryj
  "Moves border J (between slices j-1 and j of pie-slices) to the angle A.
The sum of both slices and the other values do not change."
  (let* ((slices (pie-slices obj))
         (prev (nth (1- j) slices))
         (cur (nth j slices)))
    (when (and prev cur)
      (let* ((lo (fourth prev))
             (hi (+ (fourth cur) (fifth cur)))
             (a (if (<= lo a hi) a
                    (if (< (pie-angle-diff a lo) (pie-angle-diff a hi)) lo hi))))
        (multiple-value-bind (v1 v2)
            (pie-split-pair (+ (second prev) (second cur)) (/ (- a lo) (- hi lo)))
          (let ((vals (copy-list (pie-values obj))))
            (setf (nth (first prev) vals) v1
                  (nth (first cur) vals) v2)
            (setf (pie-values obj) vals)))))))

(defmethod om-view-click-handler ((self pie-chart-editor) where)
  (multiple-value-bind (cx cy r) (pie-editor-circle self)
    (let ((j (pie-hit-boundary (object self) cx cy r (om-point-h where) (om-point-v where))))
      (setf (pie-drag self) j)
      (if j self (call-next-method)))))

(defmethod om-click-motion-handler ((self pie-chart-editor) pos)
  (when (pie-drag self)
    (multiple-value-bind (cx cy) (pie-editor-circle self)
      (pie-drag-boundary (object self) (pie-drag self)
                         (pie-point-angle cx cy (om-point-h pos) (om-point-v pos)))
      (om-invalidate-view self))))

(defmethod om-click-release-handler ((self pie-chart-editor) pos)
  (declare (ignore pos))
  (when (pie-drag self)
    (setf (pie-drag self) nil)
    (report-modifications self)
    (om-invalidate-view self)))

(defun pie-label-string (s)   ; ukryj
  "Legend text: label, value and percent."
  (format nil "~@[~A   ~]~A   (~,1F%)"
          (sixth s) (second s) (* 100 (third s))))

(defmethod om-draw-contents :after ((self pie-chart-editor))
  (multiple-value-bind (cx cy r side h) (pie-editor-circle self)
  (let* ((obj (object self))
         (slices (pie-slices obj))
         (n (length (pie-values obj))))
    (om-with-focused-view self
      (draw-pie-chart obj self 15 (+ 15 side) 15 (+ 15 side))
      ;; uchwyty granic do przeciagania
      (when (pie-show-handles self)
       (om-with-fg-color self *om-gray-color*
        (dolist (s (cdr slices))
          (let ((a (* (/ pi 180d0) (fourth s))))
            (om-fill-ellipse (round (+ cx (* r (sin a)))) (round (- cy (* r (cos a)))) 4 4)))))
      ;; legenda po prawej
      (let ((lx (+ side 40)) (ly 30))
        (dolist (s slices)
          (when (< ly (- h 10))
            (om-with-fg-color self (pie-color obj (first s) n)
              (om-fill-rect lx (- ly 10) 12 12))
            (om-with-fg-color self *om-black-color*
              (om-draw-string (+ lx 20) ly (pie-label-string s)))
            (incf ly 20))))))))

;;; --------------------------- eksport SVG ----------------------------------

(defun pie-svg-string (self size show-legend dots &optional (font-family "Helvetica, Arial, sans-serif") font-size)   ; ukryj
  "Text of an SVG file with the pie chart (colours as in the editor), optionally with a legend on the right."
  (let* ((slices (pie-slices self))
         (n (length (pie-values self)))
         (size (float size 1d0))
         (r (- (/ size 2) (* size 0.02d0)))
         (cx (/ size 2)) (cy (/ size 2))
         (font (if (and (realp font-size) (> font-size 0)) font-size (max 8 (round size 30))))
         (row (* 1.6 font))
         (texts (mapcar #'pie-label-string slices))
         (legend-w (if (and show-legend slices)
                       (+ (* 2.5 font) (* 0.6 font (reduce #'max (mapcar #'length texts))) font)
                       0))
         (w (+ size legend-w))
         (h (max size (if (and show-legend slices) (+ font (* row (length slices))) 0))))
    (flet ((pt (a rr) (let ((a (* (/ pi 180d0) a)))
                        (list (+ cx (* rr (sin a))) (- cy (* rr (cos a)))))))
      (with-output-to-string (s)
        (format s "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
        (format s "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"~A\" height=\"~A\" viewBox=\"0 0 ~A ~A\">~%"
                (round w) (round h) (round w) (round h))
        (if (null slices)
            (format s "<circle cx=\"~A\" cy=\"~A\" r=\"~A\" fill=\"none\" stroke=\"#808080\" stroke-width=\"1\"/>~%"
                    (radar-svg-num cx) (radar-svg-num cy) (radar-svg-num r))
            (progn
              ;; wycinki
              (dolist (sl slices)
                (let ((col (omcolor->hex (pie-color self (first sl) n))))
                  (if (>= (fifth sl) 359.999d0)
                      (format s "<circle cx=\"~A\" cy=\"~A\" r=\"~A\" fill=\"~A\"/>~%"
                              (radar-svg-num cx) (radar-svg-num cy) (radar-svg-num r) col)
                      (let ((p0 (pt (fourth sl) r))
                            (p1 (pt (+ (fourth sl) (fifth sl)) r)))
                        (format s "<path d=\"M~A,~A L~A,~A A~A,~A 0 ~A 1 ~A,~A Z\" fill=\"~A\"/>~%"
                                (radar-svg-num cx) (radar-svg-num cy)
                                (radar-svg-num (first p0)) (radar-svg-num (second p0))
                                (radar-svg-num r) (radar-svg-num r)
                                (if (> (fifth sl) 180d0) 1 0)
                                (radar-svg-num (first p1)) (radar-svg-num (second p1))
                                col)))))
              ;; biale linie miedzy wycinkami
              (when (> (length slices) 1)
                (dolist (sl slices)
                  (let ((p (pt (fourth sl) r)))
                    (format s "<line x1=\"~A\" y1=\"~A\" x2=\"~A\" y2=\"~A\" stroke=\"#FFFFFF\" stroke-width=\"~A\"/>~%"
                            (radar-svg-num cx) (radar-svg-num cy) (radar-svg-num (first p)) (radar-svg-num (second p))
                            (radar-svg-num (max 1d0 (/ size 300)))))))
              ;; kropki na granicach
              (when dots
                (dolist (sl (cdr slices))
                  (let ((p (pt (fourth sl) r)))
                    (format s "<circle cx=\"~A\" cy=\"~A\" r=\"~A\" fill=\"#808080\"/>~%"
                            (radar-svg-num (first p)) (radar-svg-num (second p)) (if (numberp dots) dots 4)))))
              ;; legenda
              (when show-legend
                (loop for sl in slices
                      for txt in texts
                      for y = (+ font (* 0.5 row)) then (+ y row)
                      do (format s "<rect x=\"~A\" y=\"~A\" width=\"~A\" height=\"~A\" fill=\"~A\"/>~%"
                                 (radar-svg-num (+ size font)) (radar-svg-num (- y (* 0.45 font)))
                                 (radar-svg-num (* 0.9 font)) (radar-svg-num (* 0.9 font))
                                 (omcolor->hex (pie-color self (first sl) n)))
                         (format s "<text x=\"~A\" y=\"~A\" font-family=\"~A\" font-size=\"~A\" fill=\"#000000\" dominant-baseline=\"middle\">~A</text>~%"
                                 (radar-svg-num (+ size (* 2.5 font))) (radar-svg-num y) (radar-svg-escape font-family) font
                                 (radar-svg-escape txt))))))
        (format s "</svg>~%")))))

(defmethod! pie->svg ((self pie-chart) &key path (size 600) (show-legend t) (dots nil)
                                       (font-family "Helvetica, Arial, sans-serif") (font-size nil))
  :icon 9120
  :initvals '(nil)
  :indoc '("a PIE-CHART object")
  :doc "Saves a pie chart to an SVG file (the same colours as in the editor).
<path> target file; nil = file dialog.
<size> diameter of the pie with margin (side of the square holding the pie) in pixels.
<show-legend> t = legend on the right (colour, label, value, percent), nil = the pie only.
<dots> nil = no dots, t = grey dots on the slice borders, a number = their radius.
<font-family> legend typeface, e.g. \"Cormorant Garamond\" (as in CSS; several can be given, comma-separated).
<font-size> legend text size in pixels; nil = 1/30 of the pie side.
Returns the path of the saved file or nil.
The same is available in the editor: File > Export > SVG."
  (let ((pathname (or path (om-choose-new-file-dialog :directory (def-save-directory)
                                                      :prompt "Nowy plik SVG"
                                                      :types '("SVG Files" "*.svg")))))
    (when pathname
      (with-open-file (s pathname :direction :output :if-exists :supersede :external-format :utf-8)
        (write-string (pie-svg-string self size show-legend dots font-family font-size) s))
      (om-print (format nil "pie->svg: zapisano ~A" (namestring pathname)))
      pathname)))

(defmethod get-menubar ((self pie-chart-editor))
  (list (om-make-menu "File"
                      (list
                       (om-make-menu "Export"
                                     (list (om-new-leafmenu "SVG" #'(lambda ()
                                                                      (pie->svg (object self)
                                                                                :dots (pie-show-handles self))))))
                       (om-new-leafmenu "Close" #'(lambda () (om-close-window (window self))) "w")))
        (make-om-menu 'windows :editor self)
        (make-om-menu 'help :editor self :disable '())))

;;; --------------------------- test -----------------------------------------

(defun pie-chart-test ()   ; ukryj
  "Sprawdza obliczenia wykresu kolowego; wypisuje nazwy nieudanych sprawdzen."
  (flet ((near (xs ys eps) (every #'(lambda (a b) (< (abs (- a b)) eps)) xs ys)))
    (let* ((p (make-instance 'pie-chart :pie-values '(1 1 2 0 x) :pie-labels '("a" "b" "c")))
           (s (pie-slices p))
           (checks
            (list
             (list "liczba wycinkow" (= (length s) 3))
             (list "indeksy" (equal (mapcar #'first s) '(0 1 2)))
             (list "udzialy" (near (mapcar #'third s) '(0.25d0 0.25d0 0.5d0) 1d-9))
             (list "katy poczatku" (near (mapcar #'fourth s) '(0d0 90d0 180d0) 1d-9))
             (list "katy wycinkow" (near (mapcar #'fifth s) '(90d0 90d0 180d0) 1d-9))
             (list "etykiety" (equal (mapcar #'sixth s) '("a" "b" "c")))
             (list "procenty" (near (pie-percentages p) '(25d0 25d0 50d0 0d0 0d0) 1d-9))
             (list "pusty wykres" (null (pie-slices (make-instance 'pie-chart :pie-values '(0 -1)))))
             (list "kat myszy" (near (list (pie-point-angle 0 0 0 -10) (pie-point-angle 0 0 10 0)
                                           (pie-point-angle 0 0 0 10) (pie-point-angle 0 0 -10 0))
                                     '(0 90 180 270) 1d-6))
             (list "podzial 8" (near (multiple-value-list (pie-split-pair 8 0.25)) '(2 6) 1e-5))
             (list "podzial 5" (near (multiple-value-list (pie-split-pair 5 0.49)) '(2.45 2.55) 1e-5))
             (list "svg"
                   (let* ((q (make-instance 'pie-chart :pie-values (list 1 1 2) :pie-labels (list "a<b" nil nil)))
                          (txt (pie-svg-string q 200 nil nil))
                          (txt2 (pie-svg-string q 200 t 3))
                          (one (pie-svg-string (make-instance 'pie-chart :pie-values (list 5)) 200 nil nil)))
                     (and (search "<svg" txt)
                          (search "M100.00,100.00 L100.00,4.00 A96.00,96.00 0 0 1 196.00,100.00 Z" txt)
                          (= 3 (count-subseq "<path" txt))
                          (null (search "<text" txt))
                          (search "a&lt;b" txt2)
                          (= 2 (count-subseq "<circle" txt2))
                          (= 1 (count-subseq "<circle" one))
                          (= 0 (count-subseq "<path" one)))))
             (list "przeciaganie"
                   (let ((q (make-instance 'pie-chart :pie-values (list 1 1 2))))
                     ;; granica 1 (miedzy 1. a 2. wycinkiem, teraz 90 st.) na 45 st.
                     (pie-drag-boundary q 1 45d0)
                     (near (pie-values q) '(0.5 1.5 2) 1e-5)))))
           (bad (loop for c in checks unless (second c) collect (first c))))
      (om-print (if bad
                    (format nil "pie-chart-test: BLAD w: ~{~a~^, ~}" bad)
                    "pie-chart-test: OK"))
      (null bad))))

;;; ==========================================================================
;;;  RADAR-CHART   wykres siatkowy ("zwiniety" BPF): kazda wartosc to os
;;;                wychodzaca ze srodka, punkty polaczone w wielokat.
;;;  Korzysta z pomocnikow pie-... powyzej (katy, zaokraglanie do 0.01).
;;; ==========================================================================

(defclass! radar-chart ()
  ((radar-values :initform '(5 3 4 2 4) :initarg :radar-values :accessor radar-values :type list
                 :documentation "values on the consecutive axes (numbers >= 0; others count as 0)")
   (radar-labels :initform nil :initarg :radar-labels :accessor radar-labels :type list
                 :documentation "axis labels (nil = no labels)")
   (radar-max :initform nil :initarg :radar-max :accessor radar-max
              :documentation "value at the edge of the grid; nil = the largest value")
   (radar-ref :initform nil :initarg :radar-ref :accessor radar-ref
              :documentation "reference value - red ring; nil = none")
   (radar-title :initform nil :initarg :radar-title :accessor radar-title
                :documentation "chart title (a string) - in the top left corner of the preview, in the editor and in SVG; nil = none"))
  (:icon 9122)
  (:documentation "
RADAR-CHART: a radar (spider) chart, i.e. a BPF wrapped into a circle.

<radar-values> values - each on its own axis; the first axis at the top, the next ones clockwise.
<radar-labels> optional axis labels.
<radar-max> value corresponding to the edge of the grid (nil = the largest value).
<radar-ref> reference value drawn as a red ring (nil = none).
<radar-title> chart title (nil = none).

With more than 24 axes only axes with a non-nil label get labels
(without a label list: none), grid axes are drawn only for them, and the list
of values is not shown in the editor.

Preview in the box: key m. Double-click opens the editor; vertices can be dragged with the mouse along the axes."))

;;; --------------------------- obliczenia -----------------------------------

(defun radar-value (v)   ; ukryj
  "Value to draw: a number >= 0."
  (if (realp v) (max 0 v) 0))

(defun radar-scale (self)   ; ukryj
  "Value at the edge of the grid."
  (let ((m (radar-max self)))
    (if (and (realp m) (> m 0))
        m
        (let ((top (reduce #'max (mapcar #'radar-value
                                          (cons (radar-ref self) (radar-values self)))
                           :initial-value 0)))
          (if (> top 0) top 1)))))

(defparameter *radar-many-axes* 24)   ; powyzej: tylko wybrane etykiety i osie, bez listy

(defun radar-axis-label (self i)   ; ukryj
  "Label of axis I or nil (no label)."
  (let ((labs (radar-labels self))
        (n (length (radar-values self))))
    (cond (labs (let ((l (nth i labs))) (and l (format nil "~A" l))))
          ((<= n *radar-many-axes*) (format nil "~A" (1+ i)))
          (t nil))))

(defun radar-axis-angle (i n)   ; ukryj
  "Kat osi I z N w stopniach (0 = gora, zgodnie z ruchem wskazowek zegara)."
  (* 360d0 (/ i (max 1 n))))

(defun radar-vertex (i n cx cy r frac)   ; ukryj
  "Punkt (x y) na osi I w odleglosci FRAC*R od srodka."
  (let ((a (* (/ pi 180d0) (radar-axis-angle i n))))
    (list (+ cx (* r frac (sin a)))
          (- cy (* r frac (cos a))))))

(defun radar-fracs (self)   ; ukryj
  "Polozenia wierzcholkow jako ulamki promienia (0..1)."
  (let ((s (radar-scale self)))
    (mapcar #'(lambda (v) (min 1d0 (/ (radar-value v) s))) (radar-values self))))

(defun radar-drag-value (i n cx cy r px py scale)   ; ukryj
  "Value on axis I corresponding to the mouse point (projection onto the axis), 0..SCALE, with precision 0.01."
  (let* ((a (* (/ pi 180d0) (radar-axis-angle i n)))
         (tt (/ (+ (* (- px cx) (sin a)) (* (- py cy) (- (cos a)))) r))
         (tt (max 0d0 (min 1d0 tt))))
    (pie-from-hundredths (round (* tt scale 100)))))

(defun radar-set-value (self i v)   ; ukryj
  "Changes the value on axis I (copies the list)."
  (let ((vals (copy-list (radar-values self))))
    (setf (nth i vals) v)
    (setf (radar-values self) vals)))

;;; --------------------------- rysowanie ------------------------------------

(defun radar-ipoints (pts)   ; ukryj
  (mapcar #'(lambda (p) (list (round (first p)) (round (second p)))) pts))

(defun radar-draw-closed (view pts)   ; ukryj
  "Lamana zamknieta przez punkty PTS."
  (declare (ignore view))
  (loop for (p q) on (append pts (list (car pts)))
        while q
        do (om-draw-line (round (first p)) (round (second p)) (round (first q)) (round (second q)))))

(defun draw-radar-chart (self view cx cy r)   ; ukryj
  "Draws the grid, the axes and the value polygon. Call inside om-with-focused-view."
  (let* ((n (length (radar-values self)))
         (fracs (radar-fracs self)))
    (if (zerop n)
        (om-with-fg-color view *om-gray-color*
          (om-draw-ellipse (round cx) (round cy) (round r) (round r)))
        (progn
          ;; siatka: 4 pierscienie i osie
          (om-with-fg-color view (om-make-color 0.82 0.82 0.82)
            (dolist (k '(0.25d0 0.5d0 0.75d0 1d0))
              (if (>= n 3)
                  (radar-draw-closed view (loop for i below n collect (radar-vertex i n cx cy r k)))
                  (om-draw-ellipse (round cx) (round cy) (round (* r k)) (round (* r k)))))
            (dotimes (i n)
              (when (or (<= n *radar-many-axes*) (radar-axis-label self i))
                (let ((p (radar-vertex i n cx cy r 1d0)))
                  (om-draw-line (round cx) (round cy) (round (first p)) (round (second p)))))))
          ;; wielokat wartosci
          (let ((pts (loop for i below n for f in fracs collect (radar-vertex i n cx cy r f))))
            (when (>= n 3)
              (om-with-fg-color view (om-make-color 0.62 0.78 0.95)
                (om-fill-polygon (radar-ipoints pts))))
            ;; pierscien odniesienia
            (let ((ref (radar-ref self)))
              (when (and (realp ref) (> ref 0))
                (let ((k (min 1d0 (/ ref (radar-scale self)))))
                  (om-with-fg-color view (om-make-color 0.85 0.2 0.2)
                    (if (>= n 3)
                        (radar-draw-closed view (loop for i below n collect (radar-vertex i n cx cy r k)))
                        (om-draw-ellipse (round cx) (round cy) (round (* r k)) (round (* r k))))))))
            (om-with-fg-color view (om-make-color 0.15 0.35 0.7)
              (om-with-line-size 2
                (if (>= n 3)
                    (radar-draw-closed view pts)
                    (dolist (p pts)
                      (om-draw-line (round cx) (round cy) (round (first p)) (round (second p))))))))))))

(defmethod draw-obj-in-rect ((self radar-chart) x x1 y y1 edparams view)
  (declare (ignore edparams))
  (om-with-focused-view view
    (draw-radar-chart self view (/ (+ x x1) 2d0) (/ (+ y y1) 2d0)
                      (max 1d0 (- (/ (min (- x1 x) (- y1 y)) 2d0) 2)))
    (when (radar-title self)
      (om-with-fg-color view *om-black-color*
        (om-draw-string (+ x 3) (+ y 11) (format nil "~A" (radar-title self)))))))

(defmethod draw-mini-view ((self t) (value radar-chart))
  (draw-obj-in-rect value 4 (- (w self) 4) 4 (- (h self) 4) nil self))

;;; --------------------------- edytor ---------------------------------------

(defmethod Class-has-editor-p ((self radar-chart)) t)
(defmethod get-editor-class ((self radar-chart)) 'radar-chart-editor)
(defmethod get-win-ed-size ((self radar-chart)) (om-make-point 600 440))

(defclass radar-chart-editor (editorview)
  ((drag :initform nil :accessor radar-drag)                 ; numer przeciaganej osi albo nil
   (drag-scale :initform 1 :accessor radar-drag-scale)       ; skala zapamietana na czas przeciagania
   (show-handles :initform t :accessor radar-show-handles)
   (handles-box :initform nil :accessor radar-handles-box)))

(defmethod initialize-instance :after ((self radar-chart-editor) &rest args)
  (declare (ignore args))
  (let ((obj (ignore-errors (object self))))
    (when (typep obj 'radar-chart)
      (setf (radar-show-handles self) (<= (length (radar-values obj)) *radar-many-axes*))))
  (om-add-subviews self
    (setf (radar-handles-box self)
          (om-make-dialog-item 'om-check-box (om-make-point 15 (- (h self) 24)) (om-make-point 220 16)
                               "Kropki na wierzcholkach"
                               :di-action (om-dialog-item-act item
                                            (setf (radar-show-handles self) (om-checked-p item))
                                            (om-invalidate-view self))
                               :font *om-default-font1*
                               :checked-p (radar-show-handles self)))))

(defmethod update-subviews ((self radar-chart-editor))
  (when (radar-handles-box self)
    (om-set-view-position (radar-handles-box self) (om-make-point 15 (- (h self) 24)))))

(defun radar-editor-circle (self)   ; ukryj
  "Srodek x, srodek y, promien siatki, bok kwadratu, wysokosc obszaru rysowania."
  (let* ((size (om-view-size self))
         (w (om-point-h size))
         (h (- (om-point-v size) 25))   ; miejsce na check-box
         (side (- (min h (if (> w (* 1.3 h)) h w)) 30)))
    (values (+ 15 (/ side 2d0)) (+ 15 (/ side 2d0)) (max 10d0 (- (/ side 2d0) 35)) side h)))

(defun radar-item-string (self i)   ; ukryj
  (let ((lab (nth i (radar-labels self))))
    (format nil "~@[~A   ~]~A" lab (nth i (radar-values self)))))

(defmethod om-draw-contents :after ((self radar-chart-editor))
  (multiple-value-bind (cx cy r side h) (radar-editor-circle self)
    (let* ((obj (object self))
           (n (length (radar-values obj)))
           (fracs (radar-fracs obj)))
      (om-with-focused-view self
        (draw-radar-chart obj self cx cy r)
        ;; etykiety przy koncach osi
        (om-with-fg-color self *om-black-color*
          (dotimes (i n)
            (let ((str (radar-axis-label obj i)))
             (when str
             (let* ((p (radar-vertex i n cx cy (+ r 14) 1d0))
                   (sw (om-string-size str))
                   (sa (sin (* (/ pi 180d0) (radar-axis-angle i n))))
                   (x (cond ((> sa 0.3) (first p))
                            ((< sa -0.3) (- (first p) sw))
                            (t (- (first p) (/ sw 2)))))
                   (y (+ (second p) 4)))
              (om-draw-string (round x) (round y) str))))))
        ;; uchwyty wierzcholkow
        (when (radar-show-handles self)
          (om-with-fg-color self (om-make-color 0.15 0.35 0.7)
            (loop for i below n for f in fracs
                  do (let ((p (radar-vertex i n cx cy r f)))
                       (om-fill-ellipse (round (first p)) (round (second p)) 4 4)))))
        ;; lista wartosci po prawej
        (om-with-fg-color self *om-black-color*
          (when (radar-title obj)
            (om-with-fg-color self *om-black-color*
              (om-draw-string (+ side 40) 14 (format nil "~A" (radar-title obj)))))
          (let ((lx (+ side 40)) (ly 30))
            (dotimes (i (if (<= n *radar-many-axes*) n 0))
              (when (< ly (- h 10))
                (om-draw-string lx ly (radar-item-string obj i))
                (incf ly 20)))
            (when (< ly (- h 10))
              (om-with-fg-color self *om-gray-color*
                (om-draw-string lx (+ ly 10) (format nil "brzeg siatki: ~A" (radar-scale obj)))))
            (when (and (realp (radar-ref obj)) (< (+ ly 30) (- h 10)))
              (om-with-fg-color self (om-make-color 0.85 0.2 0.2)
                (om-draw-string lx (+ ly 30) (format nil "odniesienie: ~A" (radar-ref obj)))))))))))

(defmethod om-view-click-handler ((self radar-chart-editor) where)
  (multiple-value-bind (cx cy r) (radar-editor-circle self)
    (let* ((obj (object self))
           (n (length (radar-values obj)))
           (px (om-point-h where)) (py (om-point-v where))
           (best nil) (bestd 9))
      (loop for i below n for f in (radar-fracs obj)
            do (let* ((p (radar-vertex i n cx cy r f))
                      (d (sqrt (+ (expt (- px (first p)) 2) (expt (- py (second p)) 2)))))
                 (when (< d bestd) (setf best i bestd d))))
      (setf (radar-drag self) best
            (radar-drag-scale self) (radar-scale obj))
      (if best self (call-next-method)))))

(defmethod om-click-motion-handler ((self radar-chart-editor) pos)
  (when (radar-drag self)
    (multiple-value-bind (cx cy r) (radar-editor-circle self)
      (let ((obj (object self)))
        (radar-set-value obj (radar-drag self)
                         (radar-drag-value (radar-drag self) (length (radar-values obj))
                                           cx cy r (om-point-h pos) (om-point-v pos)
                                           (radar-drag-scale self)))
        (om-invalidate-view self)))))

(defmethod om-click-release-handler ((self radar-chart-editor) pos)
  (declare (ignore pos))
  (when (radar-drag self)
    (setf (radar-drag self) nil)
    (report-modifications self)
    (om-invalidate-view self)))

;;; --------------------------- eksport SVG ----------------------------------

(defun radar-svg-num (x)   ; ukryj
  (format nil "~,2F" x))

(defun radar-svg-escape (str)   ; ukryj
  "Zamienia & < > \" na encje XML."
  (with-output-to-string (s)
    (loop for ch across str
          do (case ch
               (#\& (write-string "&amp;" s))
               (#\< (write-string "&lt;" s))
               (#\> (write-string "&gt;" s))
               (#\" (write-string "&quot;" s))
               (t (write-char ch s))))))

(defun radar-svg-points (pts)   ; ukryj
  (format nil "~{~A~^ ~}"
          (mapcar #'(lambda (p) (format nil "~A,~A" (radar-svg-num (first p)) (radar-svg-num (second p)))) pts)))

(defun radar-svg-string (self size show-labels dots line-size &optional (font-family "Helvetica, Arial, sans-serif") font-size)   ; ukryj
  "Text of an SVG file with the radar chart (the same colours as in the editor)."
  (let* ((n (length (radar-values self)))
         (fracs (radar-fracs self))
         (size (float size 1d0))
         (cx (/ size 2)) (cy (/ size 2))
         (font (if (and (realp font-size) (> font-size 0)) font-size (max 8 (round size 40))))
         (maxlen (reduce #'max (loop for i below n collect (length (or (radar-axis-label self i) ""))) :initial-value 0))
         ;; margines: miejsce na etykiety (szacunkowo 0.6 wielkosci litery na znak)
         (r (- (/ size 2) (if show-labels
                              (max (* size 0.12d0) (* font (+ 1 (* 0.6 maxlen))))
                              (* size 0.03d0))))
         (grid "#D1D1D1") (fill-col "#9EC7F2") (ref-col "#D93333") (line-col "#2659B3"))
    (with-output-to-string (s)
      (format s "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
      (format s "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"~A\" height=\"~A\" viewBox=\"0 0 ~A ~A\">~%"
              (round size) (round size) (round size) (round size))
      (flet ((ring (k col width)
               (if (>= n 3)
                   (format s "<polygon points=\"~A\" fill=\"none\" stroke=\"~A\" stroke-width=\"~A\"/>~%"
                           (radar-svg-points (loop for i below n collect (radar-vertex i n cx cy r k))) col width)
                   (format s "<circle cx=\"~A\" cy=\"~A\" r=\"~A\" fill=\"none\" stroke=\"~A\" stroke-width=\"~A\"/>~%"
                           (radar-svg-num cx) (radar-svg-num cy) (radar-svg-num (* r k)) col width))))
        ;; siatka i osie
        (dolist (k '(0.25d0 0.5d0 0.75d0 1d0)) (ring k grid 1))
        (dotimes (i n)
          (when (or (<= n *radar-many-axes*) (radar-axis-label self i))
            (let ((p (radar-vertex i n cx cy r 1d0)))
              (format s "<line x1=\"~A\" y1=\"~A\" x2=\"~A\" y2=\"~A\" stroke=\"~A\" stroke-width=\"1\"/>~%"
                      (radar-svg-num cx) (radar-svg-num cy) (radar-svg-num (first p)) (radar-svg-num (second p)) grid))))
        ;; wielokat wartosci
        (let ((pts (loop for i below n for f in fracs collect (radar-vertex i n cx cy r f))))
          (when (>= n 3)
            (format s "<polygon points=\"~A\" fill=\"~A\" stroke=\"none\"/>~%" (radar-svg-points pts) fill-col))
          ;; pierscien odniesienia
          (let ((ref (radar-ref self)))
            (when (and (realp ref) (> ref 0))
              (ring (min 1d0 (/ ref (radar-scale self))) ref-col 1)))
          (if (>= n 3)
              (format s "<polygon points=\"~A\" fill=\"none\" stroke=\"~A\" stroke-width=\"~A\" stroke-linejoin=\"round\"/>~%"
                      (radar-svg-points pts) line-col line-size)
              (dolist (p pts)
                (format s "<line x1=\"~A\" y1=\"~A\" x2=\"~A\" y2=\"~A\" stroke=\"~A\" stroke-width=\"~A\"/>~%"
                        (radar-svg-num cx) (radar-svg-num cy) (radar-svg-num (first p)) (radar-svg-num (second p)) line-col line-size)))
          ;; kropki na wierzcholkach
          (when dots
            (dolist (p pts)
              (format s "<circle cx=\"~A\" cy=\"~A\" r=\"~A\" fill=\"~A\"/>~%"
                      (radar-svg-num (first p)) (radar-svg-num (second p)) (if (numberp dots) dots 4) line-col)))))
      ;; tytul
      (when (radar-title self)
        (format s "<text x=\"~A\" y=\"~A\" font-family=\"~A\" font-size=\"~A\" fill=\"#000000\" dominant-baseline=\"hanging\">~A</text>~%"
                (radar-svg-num (* 0.5 font)) (radar-svg-num (* 0.5 font)) (radar-svg-escape font-family) font
                (radar-svg-escape (format nil "~A" (radar-title self)))))
      ;; etykiety osi
      (when show-labels
        (dotimes (i n)
          (let ((str (radar-axis-label self i)))
            (when str
              (let* ((p (radar-vertex i n cx cy (+ r (* 0.6 font)) 1d0))
                     (sa (sin (* (/ pi 180d0) (radar-axis-angle i n))))
                     (anchor (cond ((> sa 0.3) "start") ((< sa -0.3) "end") (t "middle"))))
                (format s "<text x=\"~A\" y=\"~A\" font-family=\"~A\" font-size=\"~A\" fill=\"#000000\" text-anchor=\"~A\" dominant-baseline=\"middle\">~A</text>~%"
                        (radar-svg-num (first p)) (radar-svg-num (second p)) (radar-svg-escape font-family) font anchor (radar-svg-escape str)))))))
      (format s "</svg>~%"))))

(defmethod! radar->svg ((self radar-chart) &key path (size 600) (show-labels t) (dots nil) (line-size 2)
                                            (font-family "Helvetica, Arial, sans-serif") (font-size nil))
  :icon 9120
  :initvals '(nil)
  :indoc '("a RADAR-CHART object")
  :doc "Saves a radar chart to an SVG file (the same colours as in the editor).
<path> target file; nil = file dialog.
<size> side of the image in pixels.
<show-labels> t = axis labels, nil = none.
<dots> nil = no dots, t = dots on the vertices, a number = their radius.
<line-size> width of the value line.
<font-family> label typeface, e.g. \"Cormorant Garamond\" (as in CSS; several can be given, comma-separated).
<font-size> label size in pixels; nil = 1/40 of the image side.
Returns the path of the saved file or nil.
The same is available in the editor: File > Export > SVG."
  (let ((pathname (or path (om-choose-new-file-dialog :directory (def-save-directory)
                                                      :prompt "Nowy plik SVG"
                                                      :types '("SVG Files" "*.svg")))))
    (when pathname
      (with-open-file (s pathname :direction :output :if-exists :supersede :external-format :utf-8)
        (write-string (radar-svg-string self size show-labels dots line-size font-family font-size) s))
      (om-print (format nil "radar->svg: zapisano ~A" (namestring pathname)))
      pathname)))

(defmethod get-menubar ((self radar-chart-editor))
  (list (om-make-menu "File"
                      (list
                       (om-make-menu "Export"
                                     (list (om-new-leafmenu "SVG" #'(lambda ()
                                                                      (radar->svg (object self)
                                                                                  :dots (radar-show-handles self))))))
                       (om-new-leafmenu "Close" #'(lambda () (om-close-window (window self))) "w")))
        (make-om-menu 'windows :editor self)
        (make-om-menu 'help :editor self :disable '())))

;;; --------------------------- test -----------------------------------------

(defun count-subseq (sub str)   ; ukryj
  "Ile razy SUB wystepuje w STR."
  (loop with start = 0
        for pos = (search sub str :start2 start)
        while pos count t do (setf start (1+ pos))))

(defun radar-chart-test ()   ; ukryj
  "Sprawdza obliczenia wykresu siatkowego; wypisuje nazwy nieudanych sprawdzen."
  (flet ((near (xs ys eps) (every #'(lambda (a b) (< (abs (- a b)) eps)) xs ys)))
    (let* ((p (make-instance 'radar-chart :radar-values (list 1 4 2 -1 'x)))
           (checks
            (list
             (list "skala auto" (= (radar-scale p) 4))
             (list "skala max" (= (radar-scale (make-instance 'radar-chart :radar-values (list 1 2) :radar-max 10)) 10))
             (list "skala zero" (= (radar-scale (make-instance 'radar-chart :radar-values (list 0 -2))) 1))
             (list "ulamki" (near (radar-fracs p) '(0.25 1 0.5 0 0) 1d-9))
             (list "wierzcholki" (and (near (radar-vertex 0 4 0 0 10 1) '(0 -10) 1d-9)
                                      (near (radar-vertex 1 4 0 0 10 1) '(10 0) 1d-9)
                                      (near (radar-vertex 2 4 0 0 10 0.5) '(0 5) 1d-9)))
             (list "przeciaganie" (and (= (radar-drag-value 1 4 0 0 100 50 20 4) 2)
                                       (= (radar-drag-value 1 4 0 0 100 300 0 4) 4)
                                       (= (radar-drag-value 1 4 0 0 100 -50 0 4) 0)
                                       (near (list (radar-drag-value 0 4 0 0 100 0 -33 3)) '(0.99) 1e-5)))
             (list "zmiana wartosci" (progn (radar-set-value p 2 3)
                                            (equal (radar-values p) (list 1 4 3 -1 'x))))
             (list "skala z odniesieniem"
                   (= (radar-scale (make-instance 'radar-chart :radar-values (list 1 2) :radar-ref 5)) 5))
             (list "svg"
                   (let* ((sq (make-instance 'radar-chart :radar-values (list 1 1 1 1) :radar-max 1 :radar-labels (list "a&b" nil nil nil)))
                          (txt (radar-svg-string sq 200 nil nil 2))
                          (txt2 (radar-svg-string sq 200 t 3 2)))
                     (and (search "<svg" txt)
                          (search "100.00,6.00 194.00,100.00 100.00,194.00 6.00,100.00" txt)
                          (null (search "<text" txt))
                          (search "a&amp;b</text>" txt2)
                          (= 4 (count-subseq "<circle" txt2)))))
             (list "etykiety"
                   (let ((few (make-instance 'radar-chart :radar-values (list 1 2 3)))
                         (lab (make-instance 'radar-chart :radar-values (list 1 2 3) :radar-labels (list "a" nil 7)))
                         (many (make-instance 'radar-chart :radar-values (make-list 30 :initial-element 1))))
                     (and (equal (radar-axis-label few 1) "2")
                          (equal (radar-axis-label lab 0) "a")
                          (null (radar-axis-label lab 1))
                          (equal (radar-axis-label lab 2) "7")
                          (null (radar-axis-label many 5)))))))
           (bad (loop for c in checks unless (second c) collect (first c))))
      (om-print (if bad
                    (format nil "radar-chart-test: BLAD w: ~{~a~^, ~}" bad)
                    "radar-chart-test: OK"))
      (null bad))))
