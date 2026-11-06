(in-package :om)

;;; ==========================================================================
;;;  lut-view.lisp  -  podglad dzialania LUT-a (.cube)
;;;
;;;  LUT-CURVES   krzywe: nasycenie, przesuniecie barwy i jasnosc po LUT-cie
;;;               w funkcji barwy wejsciowej (0..360 st.) -> 3 BPF-y
;;;  LUT-RADAR    to samo co LUT-CURVES jako 3 wykresy siatkowe (RADAR-CHART)
;;;  LUT-PALETTE  paleta przed i po LUT-cie jako lista pikseli
;;;               -> wejscie <background> klasy PICTURE
;;;  LUT-WHEEL    dwa kola barw (przed i po LUT-cie) -> dwie klasy PICTURE
;;;  SAVE-HDR-PNG zapis pikseli do PNG 16 bit Rec.2020 HLG (bez klasy PICTURE)
;;;  LUT-VIEW-TEST  test bez zewnetrznych plikow (sam tworzy dwa LUT-y)
;;;
;;;  Wymaga alfabet-luts.lisp (clip01, rgb->hsv, hsv->rgb) - w om-cywin.lisp
;;;  wpisz "lut-view" PO "alfabet-luts".
;;;
;;;  Uwaga: LUT-y cyklu dzialaja na wartosciach HLG. Kolory w palecie to te
;;;  same wartosci pokazane wprost na ekranie (jak podglad SDR) - proporcje
;;;  barw i nasycen sa wierne, bezwzgledna jasnosc HDR nie.
;;; ==========================================================================

(defstruct lut3d size data)          ; data: R najszybciej, potem G, potem B

(defparameter *lut3d-cache* (make-hash-table :test #'equal))

;;; --------------------------- wczytanie .cube -------------------------------

(defun read-lut3d (path)   ; ukryj
  "Reads a .cube file (3D, domain 0..1). Returns a LUT3D structure."
  (let ((size nil) (data nil) (k 0)
        (*read-default-float-format* 'double-float)
        (*read-eval* nil))
    (with-open-file (in path :direction :input :external-format :latin-1)
      (loop for raw = (read-line in nil nil)
            while raw
            do (let ((line (string-trim '(#\Space #\Tab #\Return) raw)))
                 (cond ((or (zerop (length line)) (char= (char line 0) #\#)))
                       ((and (> (length line) 11)
                             (string-equal "LUT_3D_SIZE" line :end2 11))
                        (setf size (parse-integer line :start 11))
                        (setf data (make-array (* size size size 3)
                                               :element-type 'double-float
                                               :initial-element 0d0)))
                       ((alpha-char-p (char line 0)))   ; TITLE, DOMAIN_MIN... pomijamy
                       ((and data (< k (length data)))
                        (with-input-from-string (s line)
                          (dotimes (c 3)
                            (setf (aref data k) (float (read s) 1d0))
                            (incf k))))))))
    (unless size
      (error "~a: brak LUT_3D_SIZE (to nie jest LUT 3D?)" path))
    (unless (= k (length data))
      (om-print (format nil "uwaga: ~a - wczytano ~d z ~d wartosci" path k (length data))))
    (make-lut3d :size size :data data)))

(defun load-lut3d (path)   ; ukryj
  "Like READ-LUT3D, but caches loaded files (reloads when the file changes)."
  (unless (probe-file path)
    (error "Nie ma pliku ~a" path))
  (let* ((p (namestring (truename path)))
         (date (file-write-date p))
         (hit (gethash p *lut3d-cache*)))
    (if (and hit (eql (car hit) date))
        (cdr hit)
        (let ((lut (read-lut3d p)))
          (setf (gethash p *lut3d-cache*) (cons date lut))
          lut))))

;;; --------------------------- zastosowanie ----------------------------------

(defun lut3d-apply (lut r g b)   ; ukryj
  "Trilinear interpolation. Returns (values r g b)."
  (let* ((n (lut3d-size lut))
         (d (lut3d-data lut))
         (m (1- n))
         (x (* (clip01 r) m)) (y (* (clip01 g) m)) (z (* (clip01 b) m))
         (x0 (min (floor x) (1- m))) (y0 (min (floor y) (1- m))) (z0 (min (floor z) (1- m)))
         (fx (- x x0)) (fy (- y y0)) (fz (- z z0))
         (out (list 0d0 0d0 0d0)))
    (flet ((v (ix iy iz c) (aref d (+ c (* 3 (+ ix (* n (+ iy (* n iz)))))))))
      (setf out
            (loop for c from 0 below 3
                  collect (let* ((c00 (+ (* (- 1 fx) (v x0 y0 z0 c))           (* fx (v (1+ x0) y0 z0 c))))
                                 (c10 (+ (* (- 1 fx) (v x0 (1+ y0) z0 c))      (* fx (v (1+ x0) (1+ y0) z0 c))))
                                 (c01 (+ (* (- 1 fx) (v x0 y0 (1+ z0) c))      (* fx (v (1+ x0) y0 (1+ z0) c))))
                                 (c11 (+ (* (- 1 fx) (v x0 (1+ y0) (1+ z0) c)) (* fx (v (1+ x0) (1+ y0) (1+ z0) c))))
                                 (c0 (+ (* (- 1 fy) c00) (* fy c10)))
                                 (c1 (+ (* (- 1 fy) c01) (* fy c11))))
                            (+ (* (- 1 fz) c0) (* fz c1))))))
    (values (first out) (second out) (third out))))

(defun lut-hue-shift (h2 h1)   ; ukryj
  "Hue difference h2 - h1 in degrees, in the range -180..180."
  (let ((dd (mod (- h2 h1) 360d0)))
    (if (> dd 180d0) (- dd 360d0) dd)))

;;; --------------------------- krzywe ----------------------------------------

(defmethod! lut-curves ((path string) &key (sat 0.7) (val 0.7) (steps 360) (as-bpf t))
  :icon 9111
  :initvals '("")
  :indoc '("path of the .cube file")
  :doc "How a LUT changes colours of hue 0..360 deg. (at saturation <sat> and value <val>).
Returns a list of 3 BPFs (X axis = input hue in degrees):
 1. saturation after the LUT (compare with <sat>: higher = the LUT saturates, lower = desaturates),
 2. hue shift in degrees (-180..180; 0 = hue unchanged),
 3. value (V) after the LUT (compare with <val>).
<as-bpf> nil: instead of BPFs a list of 4 lists (hues, saturations, shifts, values)."
  (let ((lut (load-lut3d path))
        (s (float sat 1d0)) (v (float val 1d0))
        hues sats shifts vals)
    (loop for i from 0 to steps
          for h = (* 360d0 (/ i steps))
          do (multiple-value-bind (r g b) (hsv->rgb h s v)
               (multiple-value-bind (r2 g2 b2) (lut3d-apply lut r g b)
                 (multiple-value-bind (h2 s2 v2) (rgb->hsv r2 g2 b2)
                   (push h hues)
                   (push s2 sats)
                   (push (if (> s2 1d-6) (lut-hue-shift h2 h) 0d0) shifts)
                   (push v2 vals)))))
    (setf hues (nreverse hues) sats (nreverse sats)
          shifts (nreverse shifts) vals (nreverse vals))
    (if as-bpf
        (list (simple-bpf-from-list hues sats 'bpf 3)
              (simple-bpf-from-list hues shifts 'bpf 1)
              (simple-bpf-from-list hues vals 'bpf 3))
        (list hues sats shifts vals))))

;;; --------------------------- krzywe na kole -------------------------------
;;; Zwraca obiekty klasy RADAR-CHART (my-classes.lisp); klasa jest potrzebna
;;; dopiero przy wywolaniu, wiec kolejnosc ladowania plikow nie przeszkadza.

;;; Osie biegna jak w RADAR-CHART (0 = gora, zgodnie z ruchem wskazowek), a barwa
;;; osi jak w LUT-WHEEL (<direction>, <top-hue>) - radar pokrywa sie z kolem.

(defun lut-radar-axis-hue (i n direction top)   ; ukryj
  "Hue of axis I of N (degrees): axis angle as in RADAR-AXIS-ANGLE, hue as in LUT-WHEEL."
  (mod (+ (float top 1d0)
          (* (if (minusp direction) -1d0 1d0) (/ (* 360d0 i) (max 1 n))))
       360d0))

(defun lut-radar-label (h step half)   ; ukryj
  "Label of the multiple of STEP nearest to the hue H, when the difference is within -HALF..HALF (right end excluded); otherwise nil."
  (when (and (realp step) (> step 0))
    (let* ((m (* step (round h step)))
           (d (lut-hue-shift (float h 1d0) (float m 1d0))))
      (when (and (>= d (- half)) (< d half))
        (format nil "~d" (mod (round m) 360))))))

(defmethod! lut-radar ((path string) &key (sat 0.7) (val 0.7) (steps 360) (label-step 30)
                       (direction -1) (top-hue nil))
  :icon 9111
  :numouts 3
  :initvals '("")
  :indoc '("path of the .cube file")
  :outdoc '("saturation after the LUT" "hue shift (+180 deg.)" "value (V) after the LUT")
  :doc "Like LUT-CURVES, but instead of BPFs returns three radar charts (RADAR-CHART),
wrapped like the LUT-WHEEL circle: <direction> and <top-hue> work the same way, so with the
same settings each direction on the radar corresponds to the same hue on the wheel.
Default (-1, nil): the hue of letter A (352.8 deg.) at the top, hue decreasing clockwise;
<direction> 1 <top-hue> 0 = the old layout (red at the top, hue increasing clockwise).
Connect each output to the <self> input of a RADAR-CHART class.
 1. saturation after the LUT (grid edge = 1, red ring = <sat>),
 2. hue shift + 180 deg. (edge = 360, red ring 180 = hue unchanged;
    outside the ring = the hue moves clockwise on the wheel),
 3. value (V) after the LUT (edge = 1, red ring = <val>).
<steps> number of hues (axes), <label-step> hue label every how many degrees (nil = no labels);
a label stands on the axis closest to the given hue."
  (let* ((lut (load-lut3d path))
         (s (float sat 1d0)) (v (float val 1d0))
         (n (max 1 steps))
         (top (or top-hue (spiral-letter-hue 0)))
         (sgn (if (minusp direction) -1d0 1d0))
         (half (/ 180d0 n))
         labs sats shifts vals)
    (dotimes (i n)
      (let ((h (lut-radar-axis-hue i n direction top)))
        (multiple-value-bind (r g b) (hsv->rgb h s v)
          (multiple-value-bind (r2 g2 b2) (lut3d-apply lut r g b)
            (multiple-value-bind (h2 s2 v2) (rgb->hsv r2 g2 b2)
              (push (lut-radar-label h label-step half) labs)
              (push s2 sats)
              (push (+ 180d0 (* sgn (if (> s2 1d-6) (lut-hue-shift h2 h) 0d0))) shifts)
              (push v2 vals))))))
    (setf labs (nreverse labs) sats (nreverse sats)
          shifts (nreverse shifts) vals (nreverse vals))
    (values (make-instance 'radar-chart :radar-values sats
                           :radar-labels labs :radar-max 1 :radar-ref sat
                           :radar-title "nasycenie")
            (make-instance 'radar-chart :radar-values shifts
                           :radar-labels (copy-list labs) :radar-max 360 :radar-ref 180
                           :radar-title "przesuniecie barwy")
            (make-instance 'radar-chart :radar-values vals
                           :radar-labels (copy-list labs) :radar-max 1 :radar-ref val
                           :radar-title "jasnosc"))))

;;; --------------------------- paleta ----------------------------------------

(defun lut-palette-row (lut s v width)   ; ukryj
  "One row of pixels (R G B 1.0): hues 0..360 deg.; LUT = nil -> without a LUT."
  (loop for i from 0 below width
        for h = (* 360d0 (/ (+ i 0.5d0) width))
        collect (multiple-value-bind (r g b) (hsv->rgb h s v)
                  (if lut
                      (multiple-value-bind (r2 g2 b2) (lut3d-apply lut r g b)
                        (list (clip01 r2) (clip01 g2) (clip01 b2) 1.0))
                      (list r g b 1.0)))))

(defmethod! lut-palette ((path string) &key (width 360) (height 100) (val 0.7) (gap 4))
  :icon 9111
  :initvals '("")
  :indoc '("path of the .cube file")
  :doc "Palette before and after the LUT as a list of pixels - connect it to the <background> input of a PICTURE class.
Top: original colours, bottom: after the LUT. Horizontally hue 0..360 deg., vertically saturation from 1 (top of the panel) to 0 (bottom).
<val> value (V) of the colours, <gap> height of the black strip between the panels."
  (let* ((lut (load-lut3d path))
         (v (float val 1d0))
         (black (make-list width :initial-element (list 0.0 0.0 0.0 1.0)))
         (sats (loop for j from 0 below height
                     collect (if (> height 1) (- 1d0 (/ j (- height 1d0))) 1d0))))
    (append
     (loop for s in sats collect (lut-palette-row nil s v width))
     (loop repeat gap collect (copy-list black))
     (loop for s in sats collect (lut-palette-row lut s v width)))))

;;; --------------------------- kolo barw -------------------------------------

(defun lut-wheel-pixels (lut size v margin bg &optional (direction 1) (top-hue 0))   ; ukryj
  "Colour wheel as a list of pixel rows. Hue = angle (0 deg. at the top; DIRECTION 1 = hue increasing
clockwise, -1 = counterclockwise, as on the Tension Wheel), saturation = distance
from the centre (0 in the centre, 1 at the edge). LUT = nil -> the original wheel."
  (let* ((total (+ size (* 2 margin)))
         (c (/ (- size 1) 2d0))
         (rad (/ size 2d0))
         (bgpix (list (float (first bg) 1.0) (float (second bg) 1.0)
                      (float (third bg) 1.0) 1.0)))
    (loop for j from 0 below total
          collect
          (loop for i from 0 below total
                collect
                (let* ((dx (- i margin c))
                       (dy (- j margin c))
                       (rr (/ (sqrt (+ (* dx dx) (* dy dy))) rad)))
                  (if (> rr 1d0)
                      (copy-list bgpix)
                      (let ((h (mod (+ (float top-hue 1d0) (* (/ 180d0 pi) (atan (if (minusp direction) (- dx) dx) (- dy)))) 360d0)))
                        (multiple-value-bind (r g b) (hsv->rgb h rr v)
                          (if lut
                              (multiple-value-bind (r2 g2 b2) (lut3d-apply lut r g b)
                                (list (clip01 r2) (clip01 g2) (clip01 b2) 1.0))
                              (list r g b 1.0))))))))))

(defmethod! lut-wheel ((path string) &key (size 300) (val 0.7) (margin 10) (bg '(0.0 0.0 0.0)) (direction -1) (top-hue nil))
  :icon 9111
  :numouts 2
  :initvals '("")
  :indoc '("path of the .cube file")
  :doc "Two colour wheels as lists of pixels - connect each output to the <background> of a separate PICTURE class.
Output 1: the original wheel, output 2: the same wheel after the LUT.
Hue = angle (0 deg. = red at the top); <direction> -1 (default) = hue decreasing
clockwise (red, magenta, violet, blue...) - like the letters on the tension wheel
and in tension-spiral->svg; 1 = hue increasing clockwise (red, yellow, green...).
<top-hue> hue at the top of the wheel in degrees; nil (default) = the hue of letter A (352.8 deg.), so
every letter of the tension wheel stands exactly above its hue; 0 = pure red at the top.
saturation = distance from the centre (grey centre, full saturation at the edge).
<size> diameter in pixels, <val> value (V) of the colours,
<margin> empty margin around the wheel (increase it if PICTURE cuts off the top),
<bg> background colour (R G B) in the range 0..1."
  (let ((lut (load-lut3d path))
        (v (float val 1d0)))
    (let ((top (or top-hue (spiral-letter-hue 0))))
      (values (lut-wheel-pixels nil size v margin bg direction top)
              (lut-wheel-pixels lut size v margin bg direction top)))))

;;; --------------------------- zapis PNG HDR ---------------------------------
;;; PNG 16 bit z blokiem cICP (BT.2020 + HLG), z kompresja DEFLATE (ponizej) - wystarczy
;;; czysty Common Lisp, bez zewnetrznych bibliotek i bez klasy PICTURE.

(defparameter *png-crc-table*
  (let ((tab (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 tab)
      (let ((c n))
        (dotimes (k 8)
          (setf c (if (logbitp 0 c) (logxor #xEDB88320 (ash c -1)) (ash c -1))))
        (setf (aref tab n) c)))))

(defun png-crc (bytes)   ; ukryj
  "CRC-32 (as in PNG and ZIP) of a byte vector."
  (let ((c #xFFFFFFFF))
    (loop for x across bytes
          do (setf c (logxor (aref *png-crc-table* (logand (logxor c x) #xFF))
                             (ash c -8))))
    (logxor c #xFFFFFFFF)))

(defun png-adler32 (bytes)   ; ukryj
  "Adler-32 checksum (zlib) of a byte vector."
  (let ((a 1) (b 0))
    (loop for x across bytes
          do (setf a (mod (+ a x) 65521)
                   b (mod (+ b a) 65521)))
    (logior (ash b 16) a)))

(defun png-u32 (n)   ; ukryj
  (list (ldb (byte 8 24) n) (ldb (byte 8 16) n) (ldb (byte 8 8) n) (ldb (byte 8 0) n)))

(defun png-bytes (list)   ; ukryj
  (coerce list '(vector (unsigned-byte 8))))

(defun png-write-chunk (out type data)   ; ukryj
  "TYPE - a 4-character string, DATA - a byte vector."
  (let ((all (concatenate '(vector (unsigned-byte 8)) (map 'list #'char-code type) data)))
    (write-sequence (png-bytes (png-u32 (length data))) out)
    (write-sequence all out)
    (write-sequence (png-bytes (png-u32 (png-crc all))) out)))

(defun png-zlib-stored (raw)   ; ukryj
  "Strumien zlib z blokami bez kompresji (po max. 65535 bajtow)."
  (let* ((n (length raw))
         (nblocks (max 1 (ceiling n 65535)))
         (z (make-array (+ 2 (* 5 nblocks) n 4) :element-type '(unsigned-byte 8)))
         (j 2))
    (setf (aref z 0) #x78 (aref z 1) #x01)
    (loop for blk from 1 to nblocks
          for pos from 0 by 65535
          do (let* ((len (min 65535 (- n pos)))
                    (nlen (logxor len #xFFFF)))
               (setf (aref z j) (if (= blk nblocks) 1 0)
                     (aref z (+ j 1)) (logand len #xFF)
                     (aref z (+ j 2)) (ash len -8)
                     (aref z (+ j 3)) (logand nlen #xFF)
                     (aref z (+ j 4)) (ash nlen -8))
               (incf j 5)
               (replace z raw :start1 j :start2 pos :end2 (+ pos len))
               (incf j len)))
    (dolist (byte (png-u32 (png-adler32 raw)))
      (setf (aref z j) byte)
      (incf j))
    z))

;;; --------------------------- kompresja PNG (DEFLATE) -----------------------
;;; Filtry wierszy PNG (jak w libpng: dla kazdego wiersza filtr o najmniejszej
;;; sumie |reszt|) + DEFLATE: LZ77 (okno 32 KB, lancuch 8 kandydatow) i dynamiczne
;;; kody Huffmana. Czysty Common Lisp. Wynik sprawdzony zlib-em (Python):
;;; dekompresja daje dokladnie dane wejsciowe.

(defparameter *defl-len-base* #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258))
(defparameter *defl-len-extra* #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))
(defparameter *defl-dist-base* #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537 2049 3073 4097 6145 8193 12289 16385 24577))
(defparameter *defl-dist-extra* #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))
(defparameter *defl-cl-order* #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

(defun defl-len-code (l)   ; ukryj
  "Index 0..28 of the length code for the match length L (3..258)."
  (loop for i from 28 downto 0
        when (<= (svref *defl-len-base* i) l) return i))

(defun defl-dist-code (d)   ; ukryj
  "Index 0..29 of the distance code D (1..32768)."
  (loop for i from 29 downto 0
        when (<= (svref *defl-dist-base* i) d) return i))

(defun defl-lz77 (data)   ; ukryj
  "LZ77 on a byte vector. Returns (values lengths values count): length 0 = literal
(value = byte), otherwise a match (value = distance)."
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (optimize (speed 3) (safety 0)))
  (let* ((n (length data))
         (win 32768) (hsize 32768) (chain 8) (maxm 258)
         (head (make-array hsize :element-type 'fixnum :initial-element -1))
         (prev (make-array win :element-type 'fixnum :initial-element -1))
         (lens (make-array (max 1 n) :element-type 'fixnum :initial-element 0))
         (vals (make-array (max 1 n) :element-type 'fixnum :initial-element 0))
         (cnt 0) (i 0))
    (declare (type fixnum n win hsize chain maxm cnt i)
             (type (simple-array fixnum (*)) head prev lens vals))
    (flet ((hash (k)
             (declare (type fixnum k))
             (logand (logxor (ash (aref data k) 10) (ash (aref data (+ k 1)) 5) (aref data (+ k 2)))
                     (1- hsize))))
      (loop while (< i n) do
        (let ((best 0) (bd 0))
          (declare (type fixnum best bd))
          (when (< (+ i 2) n)
            (let* ((h (hash i))
                   (j (aref head h))
                   (c 0))
              (declare (type fixnum h j c))
              (loop while (and (>= j 0) (<= (- i j) win) (< c chain)) do
                (when (and (< (+ i best) n)
                           (= (aref data (+ j best)) (aref data (+ i best))))
                  (let ((m 0) (lim (min maxm (- n i))))
                    (declare (type fixnum m lim))
                    (loop while (and (< m lim) (= (aref data (+ j m)) (aref data (+ i m))))
                          do (incf m))
                    (when (> m best) (setf best m bd (- i j)))
                    (when (>= m lim) (return))))
                (setf j (aref prev (logand j (1- win))))
                (incf c))
              (setf (aref prev (logand i (1- win))) (aref head h)
                    (aref head h) i)))
          (if (>= best 3)
              (progn
                (setf (aref lens cnt) best (aref vals cnt) bd)
                (incf cnt)
                (loop for k from (1+ i) below (+ i best)
                      do (when (< (+ k 2) n)
                           (let ((h (hash k)))
                             (declare (type fixnum h))
                             (setf (aref prev (logand k (1- win))) (aref head h)
                                   (aref head h) k))))
                (incf i best))
              (progn
                (setf (aref lens cnt) 0 (aref vals cnt) (aref data i))
                (incf cnt)
                (incf i))))))
    (values lens vals cnt)))

(defun defl-huff-lengths (freq limit)   ; ukryj
  "Huffman code lengths for the frequencies FREQ (a vector), at most LIMIT bits.
Ties are broken by the pair (weight, id) - as in the reference Python version."
  (let ((f (copy-seq freq))
        (n (length freq)))
    (loop
      (let ((lens (make-array n :initial-element 0))
            (items (loop for s below n when (> (aref f s) 0) collect (list (aref f s) s))))
        (cond ((null items) (return lens))
              ((null (cdr items)) (setf (aref lens (second (car items))) 1) (return lens)))
        (let ((parent (make-hash-table)) (k 0) (mx 0))
          (loop while (cdr items) do
            (setf items (sort items #'(lambda (a b) (or (< (first a) (first b))
                                                         (and (= (first a) (first b)) (< (second a) (second b)))))))
            (let* ((a (pop items)) (b (pop items)) (nid (+ 1000 k)))
              (incf k)
              (setf (gethash (second a) parent) nid
                    (gethash (second b) parent) nid)
              (setf items (append items (list (list (+ (first a) (first b)) nid))))))
          (dotimes (s n)
            (when (> (aref f s) 0)
              (let ((d 0) (x s))
                (loop (multiple-value-bind (p found) (gethash x parent)
                        (unless found (return))
                        (setf x p) (incf d)))
                (setf (aref lens s) d mx (max mx d)))))
          (when (<= mx limit) (return lens))
          (dotimes (s n)
            (when (> (aref f s) 0)
              (setf (aref f s) (max 1 (floor (1+ (aref f s)) 2))))))))))

(defun defl-codes (lens)   ; ukryj
  "Canonical codes for the lengths LENS, already bit-reversed (DEFLATE sends them from the most significant bit)."
  (let* ((n (length lens))
         (maxl (reduce #'max lens :initial-value 0))
         (bl (make-array (+ maxl 2) :initial-element 0))
         (nxt (make-array (+ maxl 2) :initial-element 0))
         (codes (make-array n :initial-element 0))
         (code 0))
    (dotimes (s n) (let ((l (aref lens s))) (when (> l 0) (incf (aref bl l)))))
    (loop for b from 1 to maxl
          do (setf code (ash (+ code (aref bl (1- b))) 1)
                   (aref nxt b) code))
    (dotimes (s n)
      (let ((l (aref lens s)))
        (when (> l 0)
          (let ((c (aref nxt l)) (r 0))
            (incf (aref nxt l))
            (dotimes (k l) (setf r (logior (ash r 1) (logand (ash c (- k)) 1))))
            (setf (aref codes s) r)))))
    codes))

;; zapis bitow: najmlodszy bit pierwszy
(defstruct (defl-bits (:constructor make-defl-bits ()))
  (buf (make-array 65536 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (acc 0 :type fixnum)
  (n 0 :type fixnum))

(defun defl-put (bw v nbits)   ; ukryj
  (declare (type fixnum v nbits) (optimize (speed 3) (safety 0)))
  (let ((acc (logior (defl-bits-acc bw) (the fixnum (ash v (defl-bits-n bw)))))
        (n (+ (defl-bits-n bw) nbits))
        (buf (defl-bits-buf bw)))
    (declare (type fixnum acc n))
    (loop while (>= n 8)
          do (vector-push-extend (logand acc 255) buf 1048576)
             (setf acc (ash acc -8))
             (decf n 8))
    (setf (defl-bits-acc bw) acc (defl-bits-n bw) n)))

(defun defl-flush (bw)   ; ukryj
  (when (> (defl-bits-n bw) 0)
    (vector-push-extend (logand (defl-bits-acc bw) 255) (defl-bits-buf bw))
    (setf (defl-bits-acc bw) 0 (defl-bits-n bw) 0)))

(defun defl-write-block (bw lens vals start end final)   ; ukryj
  "Jeden blok DEFLATE z dynamicznymi kodami Huffmana dla tokenow START..END-1."
  (let ((lf (make-array 286 :initial-element 0))
        (df (make-array 30 :initial-element 0)))
    (loop for k from start below end
          do (let ((l (aref lens k)))
               (if (> l 0)
                   (progn (incf (aref lf (+ 257 (defl-len-code l))))
                          (incf (aref df (defl-dist-code (aref vals k)))))
                   (incf (aref lf (aref vals k))))))
    (incf (aref lf 256))
    (when (every #'zerop df) (setf (aref df 0) 1))
    (let* ((ll (defl-huff-lengths lf 15))
           (dl (defl-huff-lengths df 15))
           (hlit (loop with h = 286 while (and (> h 257) (= 0 (aref ll (1- h)))) do (decf h) finally (return h)))
           (hdist (loop with h = 30 while (and (> h 1) (= 0 (aref dl (1- h)))) do (decf h) finally (return h)))
           (seq (concatenate 'vector (subseq ll 0 hlit) (subseq dl 0 hdist)))
           (cf (make-array 19 :initial-element 0)))
      (loop for x across seq do (incf (aref cf x)))
      (let* ((cl (defl-huff-lengths cf 7))
             (hclen (loop with h = 19
                          while (and (> h 4) (= 0 (aref cl (svref *defl-cl-order* (1- h)))))
                          do (decf h) finally (return h)))
             (cc (defl-codes cl)) (lc (defl-codes ll)) (dc (defl-codes dl)))
        (defl-put bw (if final 1 0) 1)
        (defl-put bw 2 2)
        (defl-put bw (- hlit 257) 5)
        (defl-put bw (- hdist 1) 5)
        (defl-put bw (- hclen 4) 4)
        (dotimes (k hclen) (defl-put bw (aref cl (svref *defl-cl-order* k)) 3))
        (loop for x across seq do (defl-put bw (aref cc x) (aref cl x)))
        (loop for k from start below end
              do (let ((l (aref lens k)) (v (aref vals k)))
                   (if (> l 0)
                       (let* ((li (defl-len-code l)) (s (+ 257 li)) (di (defl-dist-code v)))
                         (defl-put bw (aref lc s) (aref ll s))
                         (defl-put bw (- l (svref *defl-len-base* li)) (svref *defl-len-extra* li))
                         (defl-put bw (aref dc di) (aref dl di))
                         (defl-put bw (- v (svref *defl-dist-base* di)) (svref *defl-dist-extra* di)))
                       (defl-put bw (aref lc v) (aref ll v)))))
        (defl-put bw (aref lc 256) (aref ll 256))))))

(defun png-zlib-deflate (data)   ; ukryj
  "zlib stream (DEFLATE with compression) for the byte vector DATA."
  (let ((data (coerce data '(simple-array (unsigned-byte 8) (*))))
        (bw (make-defl-bits))
        (block 65536))
    (multiple-value-bind (lens vals cnt) (defl-lz77 data)
      (if (zerop cnt)
          (defl-write-block bw lens vals 0 0 t)
          (loop for k from 0 below cnt by block
                do (defl-write-block bw lens vals k (min cnt (+ k block)) (>= (+ k block) cnt)))))
    (defl-flush bw)
    (let* ((body (defl-bits-buf bw))
           (z (make-array (+ 2 (length body) 4) :element-type '(unsigned-byte 8))))
      (setf (aref z 0) #x78 (aref z 1) #x01)
      (replace z body :start1 2)
      (loop for byte in (png-u32 (png-adler32 data))
            for j from (+ 2 (length body))
            do (setf (aref z j) byte))
      z)))

(defun png-filter-rows (raw w h bpp)   ; ukryj
  "PNG filters: RAW is rows (filter byte 0 + W*BPP bytes). For every row chooses
the filter 0..4 with the smallest sum of |residuals| and returns a new vector in the same layout."
  (declare (type (simple-array (unsigned-byte 8) (*)) raw)
           (type fixnum w h bpp)
           (optimize (speed 3) (safety 0)))
  (let* ((rl (* w bpp))
         (out (make-array (length raw) :element-type '(unsigned-byte 8)))
         (tmp (make-array rl :element-type '(unsigned-byte 8)))
         (bestv (make-array rl :element-type '(unsigned-byte 8))))
    (declare (type fixnum rl))
    (dotimes (j h)
      (let ((row (1+ (* j (1+ rl))))
            (prow (* (1- j) (1+ rl)))
            (best -1) (bestf 0))
        (declare (type fixnum row prow best bestf))
        (dotimes (ft 5)
          (let ((sum 0))
            (declare (type fixnum sum))
            (dotimes (i rl)
              (let* ((x (aref raw (+ row i)))
                     (a (if (>= i bpp) (aref raw (+ row i (- bpp))) 0))
                     (b (if (> j 0) (aref raw (+ prow 1 i)) 0))
                     (c (if (and (> j 0) (>= i bpp)) (aref raw (+ prow 1 i (- bpp))) 0))
                     (p (case ft
                          (0 0) (1 a) (2 b) (3 (ash (+ a b) -1))
                          (t (let ((pa (abs (- b c))) (pb (abs (- a c))) (pc (abs (- (+ a b) c c))))
                               (declare (type fixnum pa pb pc))
                               (cond ((and (<= pa pb) (<= pa pc)) a)
                                     ((<= pb pc) b)
                                     (t c))))))
                     (v (logand (- x p) 255)))
                (declare (type fixnum x a b c p v))
                (setf (aref tmp i) v)
                (incf sum (if (< v 128) v (- 256 v)))))
            (when (or (< best 0) (< sum best))
              (setf best sum bestf ft)
              (replace bestv tmp))))
        (setf (aref out (1- row)) bestf)
        (replace out bestv :start1 row)))
    out))

;; Wzorce policzone ta sama metoda w Pythonie i sprawdzone zlib.decompress.
(defun png-deflate-test ()   ; ukryj
  (let* ((d1 (coerce (loop for i below 3000 collect (mod (* i 7) 251)) '(simple-array (unsigned-byte 8) (*))))
         (d2 (coerce (loop for i below 20000 collect (mod (floor (* i i) 7) 256)) '(simple-array (unsigned-byte 8) (*))))
         (z1 (png-zlib-deflate d1))
         (z2 (png-zlib-deflate d2))
         (raw (coerce (loop for j below 3
                            append (cons 0 (loop for i below 24 collect (mod (+ (* i 13) (* j 29) (mod (* i j) 7)) 256))))
                      '(simple-array (unsigned-byte 8) (*))))
         (f (png-filter-rows raw 4 3 6))
         (checks (list (list "deflate 1" (and (= (length z1) 333) (= (png-crc z1) 4273777697)))
                       (list "deflate 2" (and (= (length z2) 1010) (= (png-crc z2) 1737823189)))
                       (list "filtry" (and (= (length f) 75) (= (png-crc f) 273756499)
                                           (equal (list (aref f 0) (aref f 25) (aref f 50)) '(0 2 2))))))
         (bad (loop for c in checks unless (second c) collect (first c))))
    (om-print (if bad
                  (format nil "png-deflate-test: BLAD w: ~{~a~^, ~}" bad)
                  "png-deflate-test: OK"))
    (null bad)))


(defmethod! save-hdr-png ((pixels list) (path string) &key (transfer :hlg) (compress t))
  :icon 9111
  :initvals '(nil "~/Desktop/kolo.png")
  :indoc '("pixels: list of rows of (R G B [A]) 0..1, e.g. from lut-wheel or lut-palette"
           "path of the .png file")
  :doc "Saves a list of pixels to a 16-bit PNG file, bypassing the PICTURE class.
<transfer> :hlg (default) - the file is tagged as Rec.2020 HLG (cICP chunk), i.e.
exactly as the values enter the LUT; a browser with HDR support shows it
in the full gamut. :sdr - a plain PNG without a tag (values as sRGB).
<compress> t (default) - PNG compression (row filters + DEFLATE), the file is usually several times
smaller; nil - no compression (faster, about 6 bytes per pixel).
The alpha channel is ignored. Returns the file path."
  (let* ((h (length pixels))
         (w (length (first pixels)))
         (raw (make-array (* h (1+ (* w 6))) :element-type '(unsigned-byte 8)))
         (k 0)
         (hlg (string-equal (string transfer) "HLG")))
    (unless (every #'(lambda (row) (= (length row) w)) pixels)
      (error "save-hdr-png: wiersze maja rozna dlugosc"))
    (dolist (row pixels)
      (setf (aref raw k) 0)                       ; filtr wiersza: brak
      (incf k)
      (dolist (px row)
        (dotimes (c 3)
          (let ((v (round (* 65535 (clip01 (float (if (consp px) (nth c px) px) 1d0))))))
            (setf (aref raw k) (ash v -8)
                  (aref raw (1+ k)) (logand v #xFF))
            (incf k 2)))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                              :if-exists :supersede :if-does-not-exist :create)
      (write-sequence (png-bytes '(137 80 78 71 13 10 26 10)) out)
      (png-write-chunk out "IHDR" (png-bytes (append (png-u32 w) (png-u32 h) '(16 2 0 0 0))))
      (when hlg
        ;; cICP: barwy BT.2020 (9), przebieg HLG (18), RGB (0), pelny zakres (1)
        (png-write-chunk out "cICP" (png-bytes '(9 18 0 1))))
      (png-write-chunk out "IDAT" (if compress (png-zlib-deflate (png-filter-rows raw w h 6)) (png-zlib-stored raw)))
      (png-write-chunk out "IEND" (png-bytes '())))
    (om-print (format nil "zapisano ~a (~dx~d, 16 bit~a)" path w h (if hlg ", Rec.2020 HLG" "")))
    path))

;;; --------------------------- test ------------------------------------------
;;; Test tworzy dwa male LUT-y w /tmp: identycznosciowy i zamieniajacy R z B.
;;; Dla identycznosci nic sie nie zmienia; przy zamianie R<->B barwa h
;;; przechodzi w 240-h, a nasycenie i jasnosc zostaja. Oba wyniki sa znane
;;; dokladnie (interpolacja trojliniowa odtwarza je bez bledu), wiec rozne
;;; od zera moga byc tylko zaokraglenia zapisu .cube (~1e-6).

(defun write-test-cube (path size fn)   ; ukryj
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format out "TITLE \"lut-view test\"~%LUT_3D_SIZE ~d~%DOMAIN_MIN 0.0 0.0 0.0~%DOMAIN_MAX 1.0 1.0 1.0~%" size)
    (let ((n1 (float (1- size) 1d0)))
      (dotimes (ib size)
        (dotimes (ig size)
          (dotimes (ir size)
            (multiple-value-bind (r g b) (funcall fn (/ ir n1) (/ ig n1) (/ ib n1))
              (format out "~,6f ~,6f ~,6f~%" r g b))))))
    path))

(defun lut-view-test ()   ; ukryj
  (let* ((p1 (write-test-cube "/tmp/lut-view-identity.cube" 17
                              #'(lambda (r g b) (values r g b))))
         (p2 (write-test-cube "/tmp/lut-view-swap-rb.cube" 17
                              #'(lambda (r g b) (values b g r))))
         (worst 0d0))
    (flet ((note (x) (setf worst (max worst (abs x)))))
      ;; 1. identycznosc
      (destructuring-bind (hues sats shifts vals) (lut-curves p1 :as-bpf nil)
        (declare (ignore hues))
        (dolist (x sats) (note (- x 0.7d0)))
        (dolist (x shifts) (note (/ x 360d0)))
        (dolist (x vals) (note (- x 0.7d0))))
      ;; 2. zamiana R<->B: barwa h -> 240-h
      (destructuring-bind (hues sats shifts vals) (lut-curves p2 :as-bpf nil)
        (loop for h in hues for s in sats for dh in shifts for v in vals
              do (note (- s 0.7d0))
                 (note (- v 0.7d0))
                 ;; porownanie po okregu (180 i -180 to to samo przesuniecie)
                 (note (/ (lut-hue-shift dh (lut-hue-shift (- 240d0 h) h)) 360d0))))
      ;; 2a. wykresy siatkowe: z LUT-em identycznosciowym nasycenie = sat, przesuniecie = 180
      (multiple-value-bind (rs rh rv) (lut-radar p1 :steps 36 :label-step 90)
        ;; domyslnie: A (352.8) u gory, barwa maleje zgodnie z ruchem wskazowek
        (unless (and (= (length (radar-values rs)) 36)
                     (equal (remove nil (radar-labels rs)) '("270" "180" "90" "0"))
                     (< (abs (- (lut-radar-axis-hue 0 36 -1 (spiral-letter-hue 0)) 352.8d0)) 0.05d0)
                     (< (abs (- (lut-radar-axis-hue 9 36 -1 (spiral-letter-hue 0)) 262.8d0)) 0.05d0))
          (setf worst 1d0))
        (dolist (x (radar-values rs)) (note (- x 0.7d0)))
        (dolist (x (radar-values rh)) (note (/ (- x 180d0) 360d0)))
        (dolist (x (radar-values rv)) (note (- x 0.7d0))))
      ;; 2b. dawny uklad (czerwien u gory, barwa rosnie zgodnie z ruchem wskazowek)
      (let ((rs (lut-radar p1 :steps 36 :label-step 90 :direction 1 :top-hue 0)))
        (unless (equal (remove nil (radar-labels rs)) '("0" "90" "180" "270"))
          (setf worst 1d0)))
      ;; 2c. R<->B (h -> 240-h): os i ma barwe h; przy kierunku -1 przesuniecie na radarze
      ;;     ma odwrotny znak (barwa rosnaca = ruch przeciwny do wskazowek na kole)
      (multiple-value-bind (rs rh) (lut-radar p2 :steps 36)
        (declare (ignore rs))
        (loop for x in (radar-values rh) for i from 0
              do (let ((h (lut-radar-axis-hue i 36 -1 (spiral-letter-hue 0))))
                   (note (/ (lut-hue-shift (- x 180d0) (- (lut-hue-shift (- 240d0 h) h))) 360d0)))))
      ;; 3. paleta: wymiary i format pikseli
      (let ((pix (lut-palette p2 :width 36 :height 10 :gap 2)))
        (unless (and (= (length pix) 22)
                     (every #'(lambda (row) (= (length row) 36)) pix)
                     (every #'(lambda (row) (every #'(lambda (px) (= (length px) 4)) row)) pix))
          (setf worst 1d0)))
      ;; 4. kolo barw: z LUT-em identycznosciowym oba kola takie same
      (multiple-value-bind (w1 w2) (lut-wheel p1 :size 40 :margin 5)
        (unless (and (= (length w1) 50) (= (length (first w1)) 50) (= (length w2) 50))
          (setf worst 1d0))
        ;; kierunek -1 = lustro kierunku 1 wzgledem pionowej osi
        (let ((m1 (lut-wheel p1 :size 40 :margin 5 :direction 1 :top-hue (spiral-letter-hue 0))))
          (loop for row-a in w1 for row-b in m1
                do (loop for a in row-a for b in (reverse row-b)
                         do (dotimes (c 3) (note (- (nth c a) (nth c b)))))))
        (loop for row1 in w1 for row2 in w2
              do (loop for a in row1 for b in row2
                       do (dotimes (c 3) (note (- (nth c a) (nth c b)))))))
      ;; 5. PNG: sumy kontrolne ze znanymi wartosciami i dlugosc pliku
      (unless (and (= (png-crc (png-bytes (map 'list #'char-code "IEND"))) #xAE426082)
                   (= (png-adler32 (png-bytes (map 'list #'char-code "Wikipedia"))) #x11E60398))
        (setf worst 1d0))
      (let* ((pix (lut-palette p1 :width 30 :height 4 :gap 2))   ; 30 x 10
             (f (save-hdr-png pix "/tmp/lut-view-test.png" :compress nil))   ; dlugosc liczona dla zapisu bez kompresji
             (n (with-open-file (in f :element-type '(unsigned-byte 8)) (file-length in))))
        ;; 8 + IHDR 25 + cICP 16 + IDAT (12 + 2 + 5 + 10*181 + 4) + IEND 12
        (unless (= n (+ 8 25 16 (+ 12 2 5 1810 4) 12))
          (setf worst 1d0))))
    (om-print (format nil "lut-view-test: najwieksza roznica ~,7f -> ~a"
                      worst (if (< worst 1d-4) "OK" "BLAD")))
    worst))
