(in-package :om)

;==========================================;

(defun snd-frame-value (buf nch i channel)   ; ukryj
  "Value of frame I of an interleaved buffer (NCH channels): CHANNEL nil = mean of the channels,
a number = that channel (0 = left). For mono simply sample I."
  (cond ((<= nch 1) (fli:dereference buf :index i))
        (channel (fli:dereference buf :index (+ (max 0 (min (round channel) (1- nch))) (* nch i))))
        (t (/ (loop for c from 0 below nch
                    sum (fli:dereference buf :index (+ c (* nch i))))
              nch))))

(defun get-samples-from-sound (sound &optional (channel nil))
  "List of samples (SIZE frames). Stereo: CHANNEL nil = mean of the channels (like sound-stereo-to-mono),
a number = that channel (0 = left)."
  (let* ((snddata (get-om-sound-data sound))
         (size (size snddata))
         (nch (max 1 (or (nch snddata) 1)))
         (buf (buffer snddata)))
    (loop for i from 0 below size
          collect (snd-frame-value buf nch i channel))))

;========= Goertzel algorithm =============;



(defun goertzel (x sr f)
  "Goertzel power of frequency F (Hz) in the sample list X at sample rate SR."
  
  (let ((coeff (* 2 (cos (/ (* (2pi) f) sr)))))

    (labels ((goertz (xn sprev2 sprev)
               (if xn
                   (goertz (cdr xn) sprev (- (+ (car xn) (* coeff sprev)) sprev2))
                   (return-from goertz (- (+ (expt sprev 2) (expt sprev2 2)) (* coeff sprev sprev2))))
               ))

             (goertz x 0 0))
    )
)

(defun goertzel-from-references (sound bound1 bound2 f &optional (channel nil))
  "Goertzel power for frames BOUND1..BOUND2 of SOUND (a sound or om-sound-data).
Stereo: CHANNEL nil = mean of the channels, a number = that channel."
  (goertzel-from-references-snddata (get-om-sound-data sound) bound1 bound2 f channel))

(defun goertzel-from-references-snddata (snddata bound1 bound2 f &optional (channel nil))
  "Goertzel power for frames BOUND1..BOUND2 of om-sound-data (interleaved buffer for stereo).
CHANNEL nil = mean of the channels, a number = that channel (0 = left)."
  (let* ((sr (sr snddata))
         (nch (max 1 (or (nch snddata) 1)))
         (buf (buffer snddata))
         (coeff (* 2 (cos (/ (* (2pi) f) sr))))
         (sprev 0) (sprev2 0))
    (loop for i from bound1 to bound2
          do (let ((s0 (- (+ (snd-frame-value buf nch i channel) (* coeff sprev)) sprev2)))
               (setf sprev2 sprev sprev s0)))
    (- (+ (expt sprev 2) (expt sprev2 2)) (* coeff sprev sprev2))))

;--- amplituda zamiast mocy ---
; Goertzel zwraca moc bez normalizacji: dla sinusa o amplitudzie A
; i N probkach okolo (A*N/2)^2. Ponizsze funkcje przeliczaja ja na
; amplitude (niezalezna od dlugosci fragmentu): 2*sqrt(moc)/N.

(defun goertzel-power->amp (power n)
  "Converts Goertzel power computed over N samples into amplitude (independent of the fragment length): 2*sqrt(power)/N."
  (if (and (numberp n) (> n 0))
      (/ (* 2 (sqrt (max 0 power))) n)
      0))

(defun goertzel-amp (x sr f)
  "Goertzel amplitude of frequency F (Hz) in the sample list X at sample rate SR."
  (goertzel-power->amp (goertzel x sr f) (length x)))

(defun goertzel-amp-from-references (sound bound1 bound2 f &optional (channel nil))
  "Goertzel amplitude of frequency F for frames BOUND1..BOUND2 of SOUND. CHANNEL nil = mean of the channels, a number = that channel."
  (goertzel-power->amp (goertzel-from-references sound bound1 bound2 f channel)
                       (1+ (- bound2 bound1))))

(defun goertzel-amp-from-references-snddata (snddata bound1 bound2 f &optional (channel nil))
  "Goertzel amplitude of frequency F for frames BOUND1..BOUND2 of om-sound-data. CHANNEL nil = mean of the channels, a number = that channel."
  (goertzel-power->amp (goertzel-from-references-snddata snddata bound1 bound2 f channel)
                       (1+ (- bound2 bound1))))

;--- "sonogram" Goertzela: amplitudy midicentow w kolejnych okienkach ---
; Szybka wersja (03.11.2026): probki kopiowane raz do tablicy double-float,
; petla Goertzela z deklaracjami typow (kompiluje ja om-cywin-compile-library).
; Wynik jak z goertzel-amp-from-references-snddata; stara petla mogla liczyc czesciowo
; w single-float, wiec male roznice (test: < 1e-4 maksimum, ok. 0,001 dB) sa dopuszczalne.

(defun goertzel-sound-array (snddata &optional (channel nil))   ; ukryj
  "Samples of om-sound-data as a double-float array (one channel, SIZE frames).
The buffer from get-om-sound-data is INTERLEAVED: frame i, channel c = index c + nch*i.
CHANNEL nil = mean of all channels (mono), a number = that channel (0 = left)."
  (let* ((n (size snddata))
         (nch (max 1 (or (nch snddata) 1)))
         (buf (buffer snddata))
         (arr (make-array n :element-type 'double-float :initial-element 0d0)))
    (if (or (= nch 1) channel)
        (let ((c (if channel (max 0 (min (round channel) (1- nch))) 0)))
          (dotimes (i n)
            (setf (aref arr i) (float (fli:dereference buf :index (+ c (* nch i))) 1d0))))
        (let ((k (/ 1d0 nch)))
          (dotimes (i n)
            (setf (aref arr i)
                  (* k (loop for c from 0 below nch
                             sum (float (fli:dereference buf :index (+ c (* nch i))) 1d0)))))))
    arr))

(defun goertzel-array-power (x b1 b2 coeff)   ; ukryj
  "Moc Goertzela dla probek X od B1 do B2 wlacznie; COEFF = 2cos(2 pi f / sr)."
  (declare (type (simple-array double-float (*)) x)
           (type fixnum b1 b2) (type double-float coeff)
           (optimize (speed 3) (safety 0) (float 0)))
  (let ((s1 0d0) (s2 0d0))
    (declare (type double-float s1 s2))
    (loop for i of-type fixnum from b1 to b2
          do (let ((s0 (- (+ (aref x i) (* coeff s1)) s2)))
               (declare (type double-float s0))
               (setf s2 s1 s1 s0)))
    (- (+ (* s1 s1) (* s2 s2)) (* coeff s1 s2))))

(defmethod! goertzel-sonogram ((snd t) (win-ms number) (range list) &key (step 100) (channel nil))
  :icon 9108
  :numouts 3
  :initvals '(nil 125 (2100 10800))
  :indoc '("sound (or om-sound-data)" "window length in ms" "midicent range (from to)")
  :outdoc '("amplitude windows" "midicents" "window onsets in ms")
  :doc "Cuts the sound into consecutive windows of <win-ms> milliseconds and in each window
computes, with the Goertzel algorithm, the amplitude for the midicents of <range> every <step> (default 100).
Outputs:
 1. list of windows; each window is a list of amplitudes in the order of the midicents of output 2,
 2. list of midicents,
 3. window onsets in ms.
The last, shorter window is computed too (the amplitude does not depend on its length).
<channel> nil = mean of the channels (stereo -> mono, default), 0 = left, 1 = right, etc.
Note: the resolution is about 1000/win-ms Hz - at 125 ms about 8 Hz, so in the low register
(below about 140 Hz) neighbouring semitones blend."
  (let* ((snddata (if (typep snd 'om-sound-data) snd (get-om-sound-data snd)))
         (sr (sr snddata))
         (size (size snddata))
         (x (goertzel-sound-array snddata channel))
         (n (max 1 (round (* sr win-ms) 1000)))
         (midics (loop for m from (first range) to (second range) by step collect m))
         (coeffs (mapcar #'(lambda (f) (float (* 2 (cos (/ (* (2pi) f) sr))) 1d0))
                         (mc->f midics)))
         (starts (loop for s from 0 below size by n collect s)))
    (values
     (loop for s in starts
           for e = (min (1- size) (+ s n -1))
           collect (loop for c in coeffs
                         collect (goertzel-power->amp (goertzel-array-power x s e c)
                                                      (1+ (- e s)))))
     midics
     (loop for s in starts collect (/ (* 1000.0 s) sr)))))

(defun goertzel-stereo-test ()   ; ukryj
  "Interleaved stereo as from get-om-sound-data: L = 880 Hz A 0.75, R = 440 Hz A 0.5, 0.5 s.
Time and pitch must match (4 windows of 125 ms), channels separately and the mean."
  (let* ((sr 48000) (n 24000)
         (buf (om-make-pointer (* 2 n) :type :float :clear t)))
    (dotimes (i n)
      (setf (fli:dereference buf :index (* 2 i))
            (coerce (* 0.75d0 (sin (/ (* 2 pi 880 i) sr))) 'single-float))
      (setf (fli:dereference buf :index (1+ (* 2 i)))
            (coerce (* 0.5d0 (sin (/ (* 2 pi 440 i) sr))) 'single-float)))
    (let* ((snd (make-instance 'om-sound-data :buffer buf :size n :nch 2 :sr sr))
           (range '(6900 8100)))
      (flet ((amps (ch)
               (multiple-value-bind (grid mcs times) (goertzel-sonogram snd 125 range :step 1200 :channel ch)
                 (list grid mcs times))))
        (let* ((l (amps 0)) (r (amps 1)) (m (amps nil))
               (near (lambda (a b) (< (abs (- a b)) 1d-3)))
               (checks
                 (list (list "liczba okienek" (= (length (first m)) 4))
                       (list "onsety" (equal (mapcar #'round (third m)) '(0 125 250 375)))
                       (list "lewy" (every #'(lambda (fr) (and (< (first fr) 1d-3) (funcall near (second fr) 0.75d0))) (first l)))
                       (list "prawy" (every #'(lambda (fr) (and (funcall near (first fr) 0.5d0) (< (second fr) 1d-3))) (first r)))
                       (list "srednia" (every #'(lambda (fr) (and (funcall near (first fr) 0.25d0) (funcall near (second fr) 0.375d0))) (first m)))
                       (list "goertzel-amp-from-references-snddata"
                             (and (funcall near (goertzel-amp-from-references-snddata snd 0 5999 880) 0.375d0)
                                  (funcall near (goertzel-amp-from-references-snddata snd 0 5999 880 0) 0.75d0)
                                  (funcall near (goertzel-amp-from-references-snddata snd 0 5999 440 1) 0.5d0)))
                       (list "goertzel-amp-from-references"
                             (funcall near (goertzel-amp-from-references snd 6000 11999 440) 0.25d0))
                       (list "get-samples-from-sound"
                             (let ((xs (get-samples-from-sound snd)) (xl (get-samples-from-sound snd 0)))
                               (and (= (length xs) n)
                                    (funcall near (nth 7 xl) (* 0.75d0 (sin (/ (* 2 pi 880 7) sr))))
                                    (funcall near (nth 7 xs) (/ (+ (* 0.75d0 (sin (/ (* 2 pi 880 7) sr)))
                                                                   (* 0.5d0 (sin (/ (* 2 pi 440 7) sr))))
                                                                2)))))))
               (bad (loop for c in checks unless (second c) collect (first c))))
          (om-print (if bad
                        (format nil "goertzel-stereo-test: BLAD w: ~{~a~^, ~}" bad)
                        "goertzel-stereo-test: OK"))
          (null bad))))))

(defun goertzel-sonogram-test ()   ; ukryj
  "New goertzel-sonogram versus the old loop (goertzel-amp-from-references-snddata), with a timing comparison."
  (let* ((snd (chseq-to-sound (make-instance 'chord-seq :lmidic '((4800 6400 6700 7900)) :lonset '(0)
                                                        :ldur '((2000 2000 2000 2000))
                                                        :lvel '((127 100 80 60)))
                              :n-harm 4 :release 0))
         (range '(4000 9000))
         (t0 (get-internal-real-time))
         (grid (goertzel-sonogram snd 150 range))
         (t1 (get-internal-real-time))
         (sr (sr snd)) (size (size snd))
         (n (round (* sr 150) 1000))
         (midics (loop for m from 4000 to 9000 by 100 collect m))
         (freqs (mc->f midics))
         (old (loop for s from 0 below size by n
                    for e = (min (1- size) (+ s n -1))
                    collect (loop for f in freqs
                                  collect (goertzel-amp-from-references-snddata snd s e f))))
         (t2 (get-internal-real-time))
         (amax (reduce #'max (mapcar #'(lambda (fr) (reduce #'max fr)) old)))
         (worst (loop for a in grid for b in old
                      maximize (loop for x in a for y in b maximize (abs (- x y))))))
    (om-print (format nil "goertzel-sonogram-test: nowy ~,3f s, stary ~,3f s, najwieksza roznica ~,2e (wzgl. maks. ~,2e)"
                      (/ (- t1 t0) internal-time-units-per-second)
                      (/ (- t2 t1) internal-time-units-per-second)
                      worst (/ worst amax)))
    (let ((ok (and (= (length grid) (length old)) (< (/ worst amax) 1d-4))))
      (om-print (format nil "goertzel-sonogram-test: ~a" (if ok "OK" "BLAD")))
      ok)))

;===========================================

(defun sqnr (bit-depth)
  "Theoretical signal-to-quantisation-noise ratio in dB for the given bit depth."

    (* 20 (log (* (sqrt 1.5) (expt 2 bit-depth)) 10))

)


(defmethod! simple-sound-generator ((f number) (a number) (phase number) (dur number) (res number))
  :doc "Generates a sine wave as om-sound-data: frequency <freq> Hz, amplitude <amplitude>, initial <phase> in radians, duration <dur> in seconds, sample rate <res>."

  :initvals '(880 3/4 0 1000 48000) 
  :indoc '("freq" "amplitude" "phase" "dur in s" "res")
  :icon 9108

  (let* ((size (ceiling (* res dur)))
         (omegat (* (/ res) f (2pi)))
         (buffer (om-make-pointer size :type :float :clear t)))

    (setf phase (mod phase (2pi)))

    (dotimes (i size)
      (setf (fli:dereference buffer :index i)
            (coerce (* a (sin (+ (* i omegat) phase))) 'single-float)))

    (make-instance 'om-sound-data 
                   :buffer buffer
                   :size size
                   :nch 1
                   :sr res)
    )
)
;===========================================
; Test Goertzela: sinus 880 Hz, A = 0.75, sr 48000, 2400 probek.
; Wzorce policzone niezaleznie (Python, probki float32 jak w buforze OM).

(defparameter *goertzel-test-ref*
  '((100 1099 880 138527.8721988909)     ; fragment 100..1099, f = 880 Hz
    (100 1099 1000 2427.024297603348)    ; ten sam fragment, f = 1000 Hz
    (0 1099 880 168774.3906613104)))     ; od poczatku do 1099

(defun goertzel-test ()   ; ukryj
  (let* ((snd (simple-sound-generator 880 0.75 0 0.05 48000))
         (samples (loop for i from 0 below 2400
                        collect (fli::dereference (buffer snd) :index i)))
         (worst 0d0))
    (dolist (r *goertzel-test-ref*)
      (destructuring-bind (b1 b2 f ref) r
        (let ((v1 (goertzel-from-references-snddata snd b1 b2 f))
              (v2 (goertzel (subseq samples b1 (1+ b2)) 48000 f)))
          (setf worst (max worst (abs (/ (- v1 ref) ref)) (abs (/ (- v2 ref) ref)))))))
    ;; amplituda: 600 probek = dokladnie 11 okresow 880 Hz -> A = 0.75
    (let ((a1 (goertzel-amp-from-references-snddata snd 0 599 880))
          (a2 (goertzel-amp (subseq samples 0 600) 48000 880)))
      (setf worst (max worst (abs (- a1 0.75d0)) (abs (- a2 0.75d0)))))
    ;; goertzel-sonogram: 880 Hz = 8100 mc, 0.5 s, okienka 125 ms (po 110 okresow)
    (multiple-value-bind (grid mcs times)
        (goertzel-sonogram (simple-sound-generator 880 0.75 0 0.5 48000) 125 '(8000 8200))
      (unless (and (= (length grid) 4)
                   (equal mcs '(8000 8100 8200))
                   (< (abs (- (second times) 125)) 1e-6))
        (setf worst 1d0))
      (dolist (frame grid)
        (setf worst (max worst (abs (- (second frame) 0.75d0))))
        ;; sasiednie polcienie: ok. 0.02 i 0.035
        (unless (and (< (first frame) 0.05) (< (third frame) 0.05))
          (setf worst 1d0))))
    (om-print (format nil "goertzel-test: najwieksza roznica wzgledna ~,8f -> ~a"
                      worst (if (< worst 1d-5) "OK" "BLAD")))
    worst))
