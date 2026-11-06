(in-package :om)

;==================================================

(defmethod! separate-chseq-notes ((self chord-seq))

            :initvals '(nil)
            :icon 9102
            :doc "Like OMTristan's EXPLOSION: separates the notes of a chord-seq."

            (lnotes-to-chseq (chseq-to-lnotes self))

)

(defmethod! separate-chseq-notes ((self multi-seq))
  :icon 9102
  :doc "Separates the notes of every chord-seq of a multi-seq (like EXPLOSION)."

            (make-instance 'multi-seq
                           :chord-seqs (mapcar #'separate-chseq-notes (chord-seqs self))
                           )

)

(defmethod! consolidate-chseq-notes ((chseq chord-seq))
  :doc "Merges notes with the same onset of a chord-seq into chords."

     :initvals '(nil)
     :icon 9102

     (lnotes-to-chseq (consolidate-lnotes-notes (chseq-to-lnotes chseq)))

)

;==================================================


(defmethod! transp-seq ((self chord-seq) (semitones number))

	:initvals '(nil 12)
	:icon 9102
	:doc "Transposes a chord-seq or multi-seq."
	
  (make-instance 'chord-seq
                 :lmidic (om+ (lmidic self) (* semitones 100))
                 :lonset (lonset self)
                 :ldur (ldur self)
                 :lvel (lvel self)
                 :loffset (loffset self)
                 :lchan (lchan self)
                 :legato (legato self)
                 )
	
)


(defmethod! transp-seq ((self multi-seq) (semitones number))
  :icon 9102
  :doc "Transposes a multi-seq by <semitones> semitones."

	(make-instance 'multi-seq
			:chord-seqs (mapcar #'(lambda (x) (transp-seq x semitones)) (chord-seqs self))
			)

)


;==================================================


(defmethod! change-seq-chan ((self chord-seq) (new-channel number))

            :initvals '(nil 1)
            :icon 9102
            :doc "Returns the chord-seq with a changed MIDI channel."

            (make-instance 'chord-seq
                           :lmidic (lmidic self)
                           :lonset (lonset self)
                           :ldur (ldur self)
                           :lvel (lvel self)
                           :loffset (loffset self)
                           :lchan new-channel
                           :legato (legato self)
                           )

)


(defmethod! change-seq-chan ((self multi-seq) (new-channel number))
  :icon 9102
  :doc "Returns the multi-seq with a changed MIDI channel."

            (make-instance 'multi-seq
                           :chord-seqs (mapcar #'(lambda (x) (change-seq-chan x new-channel)) (chord-seqs self))
                           )

)


(defmethod! change-seq-list-chan ((chseqs list) (new-channel number))

	    :initvals '(nil 1)
            :icon 9102
            :doc "Returns the list of chord-seqs with a changed MIDI channel."

	(mapcar #'(lambda (x) (change-seq-chan x new-channel)) chseqs)

) 


;==================================================


(defmethod! change-seq-tempo ((self chord-seq) (multiplier number))

            :initvals '(nil 2)
            :icon 9102
            :doc "Changes the tempo of a chord-seq or multi-seq."

  (let ((ratio (/ multiplier)))

  (make-instance 'chord-seq
                 :lmidic (lmidic self)
                 :lonset (om* (lonset self) ratio)
                 :ldur (om* (ldur self) ratio)
                 :lvel (lvel self)
                 :loffset (om* (loffset self) ratio)
                 :lchan (lchan self)
                 :legato (legato self)
                 )
   )
)


(defmethod! change-seq-tempo ((self multi-seq) (multiplier number))
  :icon 9102
  :doc "Changes the tempo of a multi-seq: onsets and durations divided by <multiplier>."

	(make-instance 'multi-seq
			:chord-seqs (mapcar #'(lambda (x) (change-seq-tempo x multiplier)) (chord-seqs self))
			)

)

(defmethod! change-seq-list-tempo ((chseqs list) (multiplier number))

            :initvals '(nil 2)
            :icon 9102
            :doc "Changes the tempo of a list of chord-seqs or multi-seqs."

	(mapcar #'(lambda (x) (change-seq-tempo x multiplier)) chseqs)

)

;==================================================


(defmethod! reverse-seq ((self chord-seq))

            :initvals '(nil)
            :icon 9102
            :doc "Returns the sequence reversed in time (retrograde)."
	
            (let* ((self (separate-chseq-notes self)))

              (make-instance 'chord-seq
                             :lmidic (lmidic self)
                             :lonset
                             (let ((new-onset-rev (om+ (flat (lonset self)) (flat (ldur self)))))
                               (om- (list-max new-onset-rev) new-onset-rev))
                             :ldur (ldur self)
                             :lvel (lvel self)
                             :loffset (loffset self)
                             :lchan (lchan self)
                             :legato (legato self)
                             )
              )

)

"
;(defmethod! reverse-notes ((self multi-seq))

            (make-instance 'multi-seq
                           :chord-seqs (mapcar #'reverse-notes-of-sequence (chord-seqs self))
                           )

)
"
;==================================================

(defmethod! get-gcd-seq-ldur ((self chord-seq))

	:initvals '(nil)
	:icon 9102
	:doc "Greatest common divisor of the note durations of a chord-seq."

    (labels ((gcd-loop (d-list-a r)

               (if (null (cdr d-list-a))
                   (return-from gcd-loop r)
                 (gcd-loop (cdr d-list-a) (gcd r (cadr d-list-a))) 
                 )

              ))

      (gcd-loop (flat (ldur self)) (car (flat (ldur self))))
      )
    
)


;==================================================


(defmethod! shift-seq-lonset ((self chord-seq) (shift number))

	:initvals '(nil 13)
	:icon 9102
	:doc "Returns the chord-seq with lonset shifted by shift (ms)."

  (make-instance 'chord-seq
                 :lmidic (lmidic self)
                 :lonset (om+ (lonset self) shift)
                 :ldur (ldur self)
                 :lvel (lvel self)
                 :loffset (loffset self)
                 :lchan (lchan self)
                 :legato (legato self)
                 )

)


(defmethod! shift-seq-lonset ((self multi-seq) (shift number))
  :icon 9102
  :doc "Returns the multi-seq with lonset shifted by <shift> ms."

  (make-instance 'multi-seq
	         :chord-seqs (mapcar #'(lambda (x) (shift-seq-lonset x shift)) (chord-seqs self))
	         )

)


(defmethod! shift-seq-lonset-to-beginning ((self chord-seq))

	:initvals '(nil)
	:icon 9102
	:doc "Returns the chord-seq with lonset shifted so that the first chord starts at 0 ms."
	
  (shift-seq-lonset self (- (list-min (lonset self))))
	
)


(defmethod! shift-seq-lonset-to-beginning ((self multi-seq))
  :icon 9102
  :doc "Returns the multi-seq shifted so that its first chord starts at 0 ms."

  (let* ((shift (- (list-min (mapcar #'lonset (chord-seqs self))))))

  (make-instance 'multi-seq
	         :chord-seqs (mapcar #'(lambda (x) (shift-seq-lonset x shift)) (chord-seqs self))
	         )

	)

)


;==================================================


(defmethod! transp-&-change-tempo-seq ((self chord-seq) (semitones number))

            :initvals '(nil -12)
            :icon 9102
            :doc "Returns the chord-seq with transposed pitches and, in proportion to this change, a changed tempo (lonset and ldur)."
            
  (let ((ratio (/ (semitone-number-frequency-ratio semitones))))

    (make-instance 'chord-seq
                   :lmidic (om+ (lmidic self) (* semitones 100))
                   :lonset (om* (lonset self) ratio)
                   :ldur (om* (ldur self) ratio)
                   :lvel (lvel self)
                   :loffset (om* (loffset self) ratio)
                   :lchan (lchan self)
                   :legato (legato self)
                   )

    )

)


(defmethod! transp-&-change-tempo-seq ((self multi-seq) (semitones number))
  :icon 9102
  :doc "Transposes a multi-seq and changes its tempo in proportion to the transposition."

	(make-instance 'multi-seq
			:chord-seqs (mapcar #'(lambda (x) (transp-&-change-tempo-seq x semitones)) (chord-seqs self))
			)

)


;===================================================


(defmethod! get-seq-lonset-no-duplicates ((self chord-seq))

            :initvals '(nil)
            :icon 9102
            :doc "Returns the list of lonset without duplicates."           

            (sort (remove-duplicates (lonset self)) #'<)

)


;==================================================


(defmethod! approx-seq-lmidic ((self chord-seq) (approx number))

            :initvals '(nil 2)
            :icon 9102
            :doc "Rounds the lmidic of a chord sequence to: 1 - whole tone, 2 - semitone, 4 - quarter tone, ..."

            (make-instance 'chord-seq
                           :lmidic (approx-m (lmidic self) approx)
                           :lonset (lonset self)
                           :ldur (ldur self)
                           :lvel (lvel self)
                           :loffset (loffset self)
                           :lchan (lchan self)
                           :legato (legato self)
                           )
                 
)


(defmethod! approx-seq-lmidic ((self multi-seq) (approx number))
  :icon 9102
  :doc "Rounds the lmidic of a multi-seq to: 1 - whole tone, 2 - semitone, 4 - quarter tone, ..."
            (make-instance 'multi-seq
                           :chord-seqs (mapcar #'(lambda (x) (approx-seq-lmidic x approx)) (chord-seqs self))
                           )
)


;==================================================


(defmethod! cut-notes-below-vel ((self chord-seq) (min-vel number))
  :doc "Removes notes with velocity below <min-vel> from a chord-seq."

            :initvals '(nil 20)
            :icon 9102

            (lnotes-to-chseq (remove min-vel (chseq-to-lnotes self) :test #'> :key #'fourth))

)


(defmethod! limit-seq-velocity ((self chord-seq) (max-vel number))
  :doc "Limits the velocities of a chord-seq to <max-vel>."

	:initvals '(nil 79)
	:icon 9102

            (make-instance 'chord-seq
                           :lmidic (lmidic self)
                           :lonset (lonset self)
                           :ldur (ldur self)
                           :lvel (om-clip (lvel self) 0 max-vel)
                           :loffset (loffset self)
                           :lchan (lchan self)
                           :legato (legato self)
                           )            

)

;==================================================

(defmethod! tie-chseq-notes ((self chord-seq) (vel-toler integer))
  :doc "Ties repeated notes of a chord-seq when their velocities differ by at most <vel-toler>."

	:initvals '(nil 20)
	:icon 9102

            (lnotes-to-chseq (tie-lnotes (chseq-to-lnotes self) vel-toler))

)

(defmethod! cut-n-chseq-lnotes-from ((chseq chord-seq) (from-chord integer) (how-many integer) (gcd-lnotes-dur number))
  :doc "Cuts <how-many> chords from a chord-seq starting from chord <from-chord> (grid of <gcd-lnotes-dur> ms)."

     :initvals '(nil 1 16 125)
     :icon 9102

     (lnotes-to-chseq (cut-n-lnotes-from (chseq-to-lnotes chseq) from-chord how-many gcd-lnotes-dur))

)

(defmethod! repeat-chseq ((chseq chord-seq) (n integer))
  :doc "Repeats a chord-seq <n> times one after another."

     :initvals '(nil 1)
     :icon 9102

     (lnotes-to-chseq (repeat-lnotes (chseq-to-lnotes chseq) n))

)


(defmethod! chseq-scale-filter ((chseq chord-seq) (scale list))
  :doc "Keeps only the notes of a chord-seq whose pitch class belongs to <scale>."

            :initvals '(nil nil)
            :icon 9102

            (lnotes-to-chseq (lnotes-scale-filter (chseq-to-lnotes chseq) scale))
)

(defmethod! explode-chseq ((chseq chord-seq))
  :doc "Splits a chord-seq into chord-seqs of notes that do not overlap in time (like EXPLOSION)."

            :initvals '(nil)
            :icon 9102

#| (let ((lnotes (sort (sort (chseq-to-lnotes chseq) #'> :key #'first) #'< :key #'second))
       (d-lnotes nil)
       (out-llnotes nil))

   (loop if (null lnotes)
         do (return nil)
         do (loop do (push (pop lnotes) d-lnotes)
                  if (null (remove (+ (second (first d-lnotes)) (third (first d-lnotes))) lnotes :test #'> :key #'second))
                  do (push (reverse d-lnotes) out-llnotes)
                     (setf d-lnotes nil)
                     (setf lnotes (sort (sort lnotes #'> :key #'first) #'< :key #'second))
                     (return nil)
                  do (setf lnotes (rotate lnotes (position (+ (second (first d-lnotes)) (third (first d-lnotes))) lnotes :test #'<= :key #'second)))))
    
   (llnotes-to-chseq-list (reverse out-llnotes))

   )
|#

            (llnotes-to-chseq-list (explode-lnotes (chseq-to-lnotes chseq)))

)



;;; --------------------------- chord-seq ze Szkicownika nut ------------------

(defun nuty-pad-parse (text)   ; ukryj
  "From the Notes Sketchbook text: (values lmidic lonset ldur velocity) - the first three lists of numbers;
velocity from the header (\"velocity 100\"), default 100."
  (let ((vel (let ((p (search "velocity" text)))
               (or (and p (ignore-errors (parse-integer text :start (+ p 8) :junk-allowed t))) 100)))
        (lists nil))
    (with-input-from-string (in text)
      (let ((*read-eval* nil) (eof (list nil)))
        (loop for form = (handler-case (read in nil eof) (error () eof))
              until (or (eq form eof) (= (length lists) 3))
              do (when (and (consp form) (every #'numberp form)) (push form lists)))))
    (setf lists (nreverse lists))
    (values (first lists) (second lists) (third lists) vel)))

(defmethod! chseq-from-text ((text t) &key (vel nil) (chan 1))
  :icon 9102
  :initvals '(nil)
  :indoc '("text from the Notes Sketchbook (text-box / textfile), a file or nil")
  :doc "Chord-seq from the text of the Notes Sketchbook (a web page for iPhone with a piano roll).
<text>: text (e.g. from a text-box or textfile box), the path of a .txt file
or nil = file dialog. Format: three lists - pitches (midicents), onsets (ms), durations (ms):
;; nuty-pad: melody  (tempo 120, metre 4/4, velocity 100)
(6000 6400 6700)
(0 500 1000)
(500 500 1000)
<vel> dynamics; nil = the one stored in the text. <chan> MIDI channel.
Notes with the same onset are merged into one chord."
  (let ((str (cond ((stringp text)
                    (if (and (not (find #\( text)) (not (find #\Newline text)) (ignore-errors (probe-file text)))
                        (bpf-pad-read-file text) text))
                   ((pathnamep text) (bpf-pad-read-file text))
                   ((null text)
                    (let ((f (om-choose-file-dialog :prompt "Plik ze Szkicownika nut")))
                      (and f (bpf-pad-read-file f))))
                   (t (format nil "~A" text)))))
    (when str
      (multiple-value-bind (mc on du v) (nuty-pad-parse str)
        (if (and mc on du (= (length mc) (length on) (length du)))
            (let ((groups nil))
              ;; laczenie nut o tym samym poczatku w akordy
              (loop for m in mc for o in on for d in du
                    do (let ((g (assoc o groups :test #'=)))
                         (if g (push (list m d) (cdr g)) (push (list o (list m d)) groups))))
              (setf groups (sort (mapcar #'(lambda (g) (cons (car g) (reverse (cdr g)))) groups) #'< :key #'car))
              (make-instance 'chord-seq
                             :lmidic (mapcar #'(lambda (g) (mapcar #'first (cdr g))) groups)
                             :lonset (mapcar #'(lambda (g) (round (car g))) groups)
                             :ldur (mapcar #'(lambda (g) (mapcar #'(lambda (x) (round (second x))) (cdr g))) groups)
                             :lvel (mapcar #'(lambda (g) (make-list (length (cdr g)) :initial-element (or vel v))) groups)
                             :lchan (mapcar #'(lambda (g) (make-list (length (cdr g)) :initial-element chan)) groups)))
            (progn (om-print "chseq-from-text: potrzebne trzy listy liczb tej samej dlugosci (wysokosci, poczatki, dlugosci)") nil))))))

(defun chseq-from-text-test ()   ; ukryj
  (let* ((cs (chseq-from-text (format nil ";; nuty-pad: t  (tempo 120, metrum 4/4, velocity 90)~%(6000 6400 6700)~%(0 500 500)~%(500 250 1000)~%")))
         (checks
          (list (list "akordy" (and cs (equal (subseq (lonset cs) 0 2) '(0 500)) (equal (lmidic cs) '((6000) (6400 6700)))))
                (list "velocity" (and cs (equal (lvel cs) '((90) (90 90)))))
                (list "zly tekst" (null (chseq-from-text "bez liczb")))))
         (bad (remove-if #'second checks)))
    (dolist (c bad) (om-print (format nil "chseq-from-text-test: BLAD w: ~A" (first c))))
    (null bad)))
