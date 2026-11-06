(in-package :om)

;==================================================

;==================================================
; SAVE-PICTURE-SAFE: zapis PICTURE (tlo + grafiki) bez wyciekow pamieci.
; save-picture z OM przy with-graphics = t za kazdym razem tworzy bufor
; graficzny (pixmap), metaplik i obraz i zadnego z nich nie zwalnia.
; Przy zapisie wielu klatek 1920x1080 w petli (mapcar/mapc) po kilkudziesieciu
; klatkach system przerywa OM (Abort(6) w __pthread_kill). Ta wersja robi
; to samo, ale zwalnia wszystko po kazdym zapisie.

(defun picture-file-type (file)   ; ukryj
  "File format by the file extension (TIFF by default)."
  (let ((ext (string-downcase (or (pathname-type (pathname file)) ""))))
    (cond ((string= ext "png") :png)
          ((member ext '("jpg" "jpeg") :test #'string=) :jpeg)
          ((string= ext "bmp") :bmp)
          (t :tiff))))

(defun picture-render-image (self view bg w h pictsize mode with-graphics)   ; ukryj
  "Draws a PICTURE into a new w x h image. MODE: :bg (background from the image BG or white),
:black, :white (uniform background - to compute the transparency of the graphics).
Returns the image; the caller frees it (gp:free-image)."
  (let ((metafile nil))
    (unwind-protect
        (progn
          (setf metafile
                (capi:with-internal-metafile (port :pane nil :bounds (list 0 0 w h))
                  (let ((oa::*curstream* port) (oa::*pox* 0) (oa::*poy* 0))
                    (cond ((and (eq mode :bg) bg)
                           (gp:draw-image port bg 0 0
                                          :from-width (gp:image-width bg) :from-height (gp:image-height bg)
                                          :to-width w :to-height h))
                          (t (om-with-fg-color nil (if (eq mode :black) *om-black-color* *om-white-color*)
                               (om-fill-rect 0 0 w h))))
                    (when with-graphics
                      (loop for o in (extraobjs self)
                            do (draw-pict-extraobj nil o (om-make-point 0 0) pictsize))))))
          (capi:draw-metafile-to-image view metafile))
      (when metafile (ignore-errors (capi:free-metafile metafile))))))

(defun picture-image-bgra (view image)   ; ukryj
  "Pixels of the image as a byte vector B G R A (row by row), width, height."
  (let* ((ia (gp:make-image-access view image))
         (w (gp:image-access-width ia))
         (h (gp:image-access-height ia))
         (v (make-array (* w h 4) :element-type '(unsigned-byte 8))))
    (unwind-protect
        (progn (gp:image-access-transfer-from-image ia)
               (gp:image-access-pixels-to-bgra ia v))
      (gp:free-image-access ia))
    (values v w h)))

(defun picture-bg-pixel-rows (self w h)   ; ukryj
  "Background of a PICTURE as a list of rows (r g b) 0..1, if the background was given as a list of pixels of size w x h; otherwise nil."
  (let ((bg (slot-value self 'background)))
    (when (and (consp bg) (consp (car bg))
               (= (length bg) h) (= (length (car bg)) w))
      (mapcar #'(lambda (row)
                  (mapcar #'(lambda (px)
                              (if (consp px)
                                  (list (float (first px) 1d0) (float (second px) 1d0) (float (third px) 1d0))
                                  (let ((v (float px 1d0))) (list v v v))))
                          row))
              bg))))

(defun picture-hdr-rows (base-rows black white w h)   ; ukryj
  "Composes the background (BASE-ROWS, 0..1, full precision) with graphics drawn on black (BLACK)
and white (WHITE) backgrounds (BGRA vectors). Transparency of the graphics in channel c: a = 1 - (W - B)/255,
result = background*(1 - a) + B/255. Without graphics (BLACK nil) returns the background."
  (if (null black)
      base-rows
      (loop for row in base-rows
            for j from 0
            collect (loop for px in row
                          for i from 0
                          collect (let ((k (* 4 (+ (* j w) i))))
                                    (loop for c from 0 below 3
                                          for bi = (- (+ k 2) c)   ; BGRA: R = k+2, G = k+1, B = k
                                          collect (let* ((b (/ (aref black bi) 255d0))
                                                         (wv (/ (aref white bi) 255d0))
                                                         (a (max 0d0 (min 1d0 (- 1d0 (- wv b))))))
                                                    (max 0d0 (min 1d0 (+ (* (nth c px) (- 1d0 a)) b))))))))))

(defun picture-bgra-rows (v w h)   ; ukryj
  "BGRA vector -> list of rows (r g b) 0..1."
  (loop for j below h
        collect (loop for i below w
                      collect (let ((k (* 4 (+ (* j w) i))))
                                (list (/ (aref v (+ k 2)) 255d0)
                                      (/ (aref v (+ k 1)) 255d0)
                                      (/ (aref v k) 255d0))))))

(defmethod! save-picture-safe ((self picture) &optional (path nil) (with-graphics t) (size nil) (hdr nil))
  :icon 9112
  :initvals '(nil nil t nil nil)
  :indoc '("a PICTURE object" "file path" "t = with graphics (3rd input of PICTURE), nil = background only"
           "size (om-make-point w h) or nil" "nil = plain save; t / :hlg = 16-bit PNG Rec.2020 HLG; :sdr = 16-bit PNG without HDR")
  :doc "Saves a PICTURE to a file - like save-picture, but without memory leaks,
so it is suitable for saving many frames in a loop.
<path> path; nil = file dialog.
<with-graphics> t = background together with vector graphics (default), nil = background only.
<size> image size (om-make-point width height); nil = size of the background.
<hdr> nil = format by extension (.png, .jpg, .bmp, otherwise TIFF), 8 bit;
      t or :hlg = 16-bit PNG tagged Rec.2020 HLG (like save-hdr-png);
      :sdr = 16-bit PNG without an HDR tag.
   With HDR a background given as a list of pixels keeps full precision (16 bit), and the graphics
   are drawn over it with anti-aliased edges. A background from a file and a size other than the
   background give 8-bit background precision. Graphic colours are HLG signal values, like the background pixels.
Returns the path of the saved file."
  (let ((file (or path (om-choose-new-file-dialog :directory (def-save-directory)
                                                   :types '("PNG File" "*.png" "TIFF File" "*.tiff;*.tif"
                                                            "JPEG File" "*.jpg;*.jpeg" "BMP File" "*.bmp")))))
    (when file
      (let* ((view oa::*record-view*)
             (pict (thepict self))
             (bg (when pict (gp:load-image view pict)))
             (pictsize (or size
                           (and bg (om-make-point (gp:image-width bg) (gp:image-height bg)))
                           (om-make-point 100 100)))
             (w (om-point-h pictsize))
             (h (om-point-v pictsize))
             (images nil))
        (unwind-protect
            (if (null hdr)
                ;; zwykly zapis 8 bit
                (let ((image (picture-render-image self view bg w h pictsize :bg with-graphics)))
                  (push image images)
                  (gp:externalize-and-write-image view image file
                                                  :type (picture-file-type file) :if-exists :supersede))
                ;; PNG 16 bit (HLG albo SDR)
                (let* ((graphics (and with-graphics (extraobjs self)))
                       (base (or (picture-bg-pixel-rows self w h)
                                 (let ((img (picture-render-image self view bg w h pictsize :bg nil)))
                                   (push img images)
                                   (picture-bgra-rows (picture-image-bgra view img) w h))))
                       (black nil) (white nil))
                  (when graphics
                    (let ((ib (picture-render-image self view nil w h pictsize :black t))
                          (iw (picture-render-image self view nil w h pictsize :white t)))
                      (push ib images) (push iw images)
                      (multiple-value-bind (v vw vh) (picture-image-bgra view ib)
                        (unless (and (= vw w) (= vh h))
                          (error "save-picture-safe: obraz ma ~Ax~A zamiast ~Ax~A" vw vh w h))
                        (setf black v))
                      (setf white (picture-image-bgra view iw))))
                  (save-hdr-png (picture-hdr-rows base black white w h) (namestring file)
                                :transfer (if (eq hdr :sdr) :sdr :hlg))))
          ;; sprzatanie - zawsze, takze po bledzie
          (dolist (im images) (ignore-errors (gp:free-image view im)))
          (when bg (ignore-errors (gp:free-image view bg))))
        file))))

(defun save-picture-safe-test (&optional (n 200))   ; ukryj
  "Saves a 64x48 image (background from a list of pixels) N times to a temporary file and checks that the file was created."
  (let* ((pix (loop for y below 48 collect
                    (loop for x below 64 collect (list (/ x 64.0) (/ y 48.0) 0.5 1))))
         (pic (let ((p (make-instance 'picture))) (setf (background p) pix) p))
         (file "/tmp/save-picture-safe-test.png")
         (ok (ignore-errors
               (dotimes (i n) (save-picture-safe pic file t))
               (probe-file file))))
    (let* ((hdr-file "/tmp/save-picture-safe-test-hdr.png")
           (hdr-ok (ignore-errors (save-picture-safe pic hdr-file t nil t) (probe-file hdr-file)))
           ;; skladanie: czarna grafika o kryciu ok. 50% (W = 128, B = 0) na tle 0.8 -> 0.8 * 128/255
           (mix-ok (let ((r (picture-hdr-rows '(((0.8d0 0.8d0 0.8d0)))
                                              (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(0 0 0 255))
                                              (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(128 128 128 255))
                                              1 1)))
                     (< (abs (- (first (caar r)) (+ (* 0.8d0 (/ 128 255d0)) 0d0))) 1d-9))))
      (om-print (format nil "save-picture-safe-test (~A zapisow): ~A, HDR: ~A, skladanie: ~A"
                        n (if ok "OK" "BLAD") (if hdr-ok "OK" "BLAD") (if mix-ok "OK" "BLAD")))
      (and ok hdr-ok mix-ok t))))
