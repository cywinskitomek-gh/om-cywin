(in-package :om)

;==================================================
;                  SHIFT-LNOTES
;==================================================

(defmethod! shift-lnotes-lonset ((lnotes list) (shift integer))

	:initvals '(nil 13)
	:icon 9100
	:doc "Returns \"lnotes\" with \"lonset\" shifted by \"shift\" (ms)."
	
          (setf lnotes (mat-trans lnotes))

	(mapcar #'list
                (first lnotes)
                (om+ (second lnotes) shift)
                (third lnotes)
                (fourth lnotes)
                (fifth lnotes)
                (sixth lnotes)
                (seventh lnotes))

)


(defmethod! shift-lnotes-lonset-to-beginning ((lnotes list))

	:initvals '(nil)
	:icon 9100
	:doc "Returns lnotes with lonset shifted so that the first chord starts at 0 ms."
	
  (shift-lnotes-lonset lnotes (- (list-min (second (mat-trans lnotes)))))
	
)

;==================================================

(defmethod! cut-n-lnotes-from ((lnotes list) (from-chord integer) (n integer) (gcd-lnotes-dur number))
  :doc "Cuts <n> chords from lnotes starting from chord <from-chord> (chords on a grid of <gcd-lnotes-dur> ms); notes crossing the borders are shortened."

     :initvals '(nil 1 16 125)
     :icon 9100

     (let* ((to-chord (1- (+ n from-chord)))
            (from-onset (* (1- from-chord) gcd-lnotes-dur))
            (to-offset (* to-chord gcd-lnotes-dur)))

       (setf lnotes (remove to-offset lnotes :test #'<= :key #'second))

       (setf lnotes (remove 0 (mapcar #'(lambda (x)
                                          (if (< (second x) from-onset)
                                              (subs-posn x '(1 2) (list from-onset (- (third x) (- from-onset (second x)))))
                                            x))
                                      lnotes)
                            :test #'>= :key #'third))

       (setf lnotes (mapcar #'(lambda (x)
                                (if (> (+ (second x) (third x)) to-offset)
                                    (subs-posn x 2 (- to-offset (second x)))
                                  x)
                                )
                            lnotes))

       (shift-lnotes-lonset lnotes (- from-onset))
       )
)

(defmethod! cut-lnotes-from-to ((lnotes list) (from-onset integer) (to-offset integer))
  :doc "Cuts lnotes to the time span <from-onset>..<to-offset> ms and shifts the result to start at 0; notes crossing the borders are shortened."

     :initvals '(nil 0 1000)
     :icon 9100

       (setq lnotes (remove to-offset lnotes :test #'<= :key #'second))

       (setf lnotes (remove 0 (mapcar #'(lambda (x)
                                          (if (< (second x) from-onset)
                                              (subs-posn x '(1 2) (list from-onset (- (third x) (- from-onset (second x)))))
                                            x))
                                      lnotes)
                            :test #'>= :key #'third))

       (setf lnotes (mapcar #'(lambda (x)
                                (if (> (+ (second x) (third x)) to-offset)
                                    (subs-posn x 2 (- to-offset (second x)))
                                  x)
                                )
                            lnotes))

       (shift-lnotes-lonset lnotes (- from-onset))

)

(defmethod! cut-lnotes-from-to-no-shift ((lnotes list) (from-onset integer) (to-offset integer))
  :doc "Cuts lnotes to the time span <from-onset>..<to-offset> ms without shifting; notes crossing the borders are shortened."

     :initvals '(nil 0 1000)
     :icon 9100

       (setq lnotes (remove to-offset lnotes :test #'<= :key #'second))

       (setf lnotes (remove 0 (mapcar #'(lambda (x)
                                          (if (< (second x) from-onset)
                                              (subs-posn x '(1 2) (list from-onset (- (third x) (- from-onset (second x)))))
                                            x))
                                      lnotes)
                            :test #'>= :key #'third))

       (mapcar #'(lambda (x)
                   (if (> (+ (second x) (third x)) to-offset)
                       (subs-posn x 2 (- to-offset (second x)))
                     x)
                   )
               lnotes)

)


;==================================================
;                changes-on-lnotes
;==================================================


(defmethod! transp-lnotes ((lnotes list) (semitones number))

	:initvals '(nil 12)
	:icon 9100
	:doc "Transposes lnotes by <semitones> semitones."
	
          (setf lnotes (mat-trans lnotes))
	
	(mapcar #'list
                (om+ (first lnotes) (* semitones 100))
                (second lnotes)
                (third lnotes)
                (fourth lnotes)
                (fifth lnotes)
                (sixth lnotes)
                (seventh lnotes))

)

(defmethod! change-lnotes-chan ((lnotes list) (new-channel integer))

	:initvals '(nil 1)
	:icon 9100
	:doc "Sets the MIDI channel of all lnotes to <new-channel>."
	
          (setf lnotes (mat-trans lnotes))
	
	(mapcar #'list
                (first lnotes)
                (second lnotes)
                (third lnotes)
                (fourth lnotes)
                (fifth lnotes)
                (repeat-n new-channel (length (first lnotes)))
                (seventh lnotes))

)


(defmethod! change-lnotes-tempo ((lnotes list) (multiplier number))

            :initvals '(nil 0.5)
            :icon 9100
            :doc "Changes the tempo of lnotes: onsets and durations divided by <multiplier> (2 = twice as fast)."

	(let ((ratio (/ multiplier)))

          (setf lnotes (mat-trans lnotes))
	
	(mapcar #'list
                (first lnotes)
                (om-round (om* (second lnotes) ratio))
                (om-round (om* (third lnotes) ratio))
                (fourth lnotes)
                (om-round (om* (fifth lnotes) ratio))
                (sixth lnotes)
                (seventh lnotes))
        )

)

(defmethod! transp-&-change-tempo-lnotes ((lnotes list) (semitones number))

            :initvals '(nil -12)
            :icon 9100
            :doc "Returns lnotes with transposed pitches and, in proportion to this change, a changed tempo."
            
            (let ((ratio (/ (semitone-number-frequency-ratio semitones))))

          (setf lnotes (mat-trans lnotes))
              
            	(mapcar #'list
                	(om+ (first lnotes) (* semitones 100))
                        (om-round (om* (second lnotes) ratio))
                	(om-round (om* (third lnotes) ratio))
                	(fourth lnotes)
                	(om-round (om* (fifth lnotes) ratio))
                	(sixth lnotes)
                        (seventh lnotes))
            
            )
)


;==================================================
;                LNOTES-VELOCITY
;==================================================

(defmethod! cut-lnotes-below-vel ((lnotes list) (min-vel number))
  :doc "Removes notes with velocity below <min-vel>."

            :initvals '(nil 6)
            :icon 9100

            (remove min-vel lnotes :test #'> :key #'fourth)

)

(defmethod! cut-lnotes-below-dur ((lnotes list) (min-dur number))
  :doc "Removes notes shorter than <min-dur> ms."

            :initvals '(nil 10)
            :icon 9100

            (remove min-dur lnotes :test #'> :key #'third)

)


(defmethod! limit-lnotes-velocity ((lnotes list) (max-vel integer))
  :doc "Limits note velocities to <max-vel>."

            :initvals '(nil 79)
            :icon 9100

          (setf lnotes (mat-trans lnotes))
	
	(mapcar #'list
                (first lnotes)
                (second lnotes)
                (third lnotes)
                (mapcar #'(lambda (x) (if (> x max-vel) max-vel x)) (fourth lnotes))
                (fifth lnotes)
                (sixth lnotes)
                (seventh lnotes))


)


(defmethod! scale-lnotes-velocity ((lnotes list) (in-maxout number) (exponent number))
  :doc "Scales velocities from 0..<in-maxout> to 0..127 with the exponent <exponent>."

            :initvals '(nil 79 1.0)
            :icon 9100

	(let* ((dlnotes (mat-trans lnotes)))
	(remove nil
	(mapcar #'list
                (first dlnotes)
                (second dlnotes)
                (third dlnotes)
                (om-round (om-scale^ (fourth dlnotes) 0 in-maxout exponent 0 127))
                (fifth dlnotes)
                (sixth dlnotes)
                (seventh dlnotes)))
        )

)

(defmethod! scale-lnotes-velocity-2 ((lnotes list) (in-minout number) (in-maxout number) (exponent number))
  :doc "Scales velocities from <in-minout>..<in-maxout> to 0..127 with the exponent <exponent>."

            :initvals '(nil 10 79 1.0)
            :icon 9100

	(let* ((dlnotes (mat-trans lnotes)))
	(remove nil
	(mapcar #'list
                (first dlnotes)
                (second dlnotes)
                (third dlnotes)
                (om-round (om-scale^ (fourth dlnotes) in-minout in-maxout exponent 0 127))
                (fifth dlnotes)
                (sixth dlnotes)
                (seventh dlnotes)))
        )

)

(defmethod! scale-lnotes-velocity-3 ((lnotes list) (in-minout number) (in-maxout number) (exponent number) (minin number) (maxin number))
  :doc "Scales velocities from <in-minout>..<in-maxout> to <minin>..<maxin> with the exponent <exponent>."

            :initvals '(nil 10 79 1.0 0 127)
            :icon 9100

	(let* ((dlnotes (mat-trans lnotes)))
	(remove nil
	(mapcar #'list
                (first dlnotes)
                (second dlnotes)
                (third dlnotes)
                (om-round (om-scale^ (fourth dlnotes) in-minout in-maxout exponent minin maxin))
                (fifth dlnotes)
                (sixth dlnotes)
                (seventh dlnotes)))
        )

)

(defmethod! scale-lnotes-velocity-3a ((lnotes list) (in-minout number) (in-maxout number) (exponent number))
  :doc "Scales velocities from <in-minout>..<in-maxout> to the current velocity range of lnotes with the exponent <exponent>."

            :initvals '(nil 10 79 1.0)
            :icon 9100

     (let ((lnotes-lvel (get-lnotes-lvel-no-duplicates lnotes)))

       (scale-lnotes-velocity-3 lnotes in-minout in-maxout exponent (list-min lnotes-lvel) (list-max lnotes-lvel))

       )
)

(defmethod! scale-lnotes-velocity-bpf ((lnotes list) (min number) (max number) (bpf bpf))
  :doc "Scales velocities over time with a BPF stretched over the duration of lnotes, between <min> and <max>."

            :initvals '(nil 10 79 nil)
            :icon 9100

  (let ((lonset (get-lnotes-lonset-no-duplicates lnotes))
        (velocities (get-lnotes-lvel-no-duplicates lnotes))
        )

  (setf bpf-temp (bpf-scale bpf :x1 (list-minimum lonset) :x2 (list-maximum lonset) :y1 (om-round (/ min (list-maximum velocities)) 10 1) :y2 (om-round (/ max (list-maximum velocities)) 10 1)))

  (little-bpf bpf-temp)
	
	(mapcar #'(lambda (dlnotes)
                    (list
                     (first dlnotes)
                     (second dlnotes)
                     (third dlnotes)
                     (round (om-clip (* (fourth dlnotes)
                                        (x-transfer bpf-temp (second dlnotes))
                                        ) min max))
                     (fifth dlnotes)
                     (sixth dlnotes)
                     (seventh dlnotes)))
                lnotes)

    )
)

(defmethod! bpf-set-lnotes-velocity ((lnotes list) (vel-min number) (vel-max number) (bpf bpf))
  :doc "Sets velocities over time with a BPF stretched over the duration of lnotes and scaled to <vel-min>..<vel-max>."

            :initvals '(nil 10 79 nil)
            :icon 9100

  (let ((lonset (get-lnotes-lonset-no-duplicates lnotes))
        bpf-temp)

  (setf bpf-temp (bpf-scale bpf :x1 (list-minimum lonset) :x2 (list-maximum lonset) :y1 vel-min :y2 vel-max))

  (little-bpf bpf-temp)

        (mapcar #'(lambda (dlnotes)
                    (list
                     (first dlnotes)
                     (second dlnotes)
                     (third dlnotes)
                     (round (om-clip (x-transfer bpf-temp (second dlnotes)) vel-min vel-max))
                     (fifth dlnotes)
                     (sixth dlnotes)
                     (seventh dlnotes)))
                lnotes)

    )
)

(defmethod! add-lnotes-velocity ((lnotes list) (add-vel integer))
  :doc "Adds <add-vel> to all velocities (clipped to 0..127)."

            :initvals '(nil 10)
            :icon 9100

	(let* ((dlnotes (mat-trans lnotes)))
	
	(mapcar #'list
                (first dlnotes)
                (second dlnotes)
                (third dlnotes)
                (om-clip (om+ add-vel (fourth dlnotes)) 0 127)
                (fifth dlnotes)
                (sixth dlnotes)
                (seventh dlnotes))
        )

)

(defmethod! multi-lnotes-velocity ((lnotes list) (mult-vel number))
  :doc "Multiplies all velocities by <mult-vel> (clipped to 0..127)."

            :initvals '(nil 10)
            :icon 9100

	(let* ((dlnotes (mat-trans lnotes)))
	
	(mapcar #'list
                (first dlnotes)
                (second dlnotes)
                (third dlnotes)
                (om-clip (om* mult-vel (fourth dlnotes)) 0 127)
                (fifth dlnotes)
                (sixth dlnotes)
                (seventh dlnotes))
        )

)

(defmethod! add-random-lnotes-velocity ((lnotes list) (random-from integer) (random-to integer))
  :doc "Adds a random value between <random-from> and <random-to> to every velocity (clipped to 0..127)."

            :initvals '(nil -5 5)
            :icon 9100

	(let* ((dlnotes (mat-trans lnotes)))
	
	(mapcar #'list
                (first dlnotes)
                (second dlnotes)
                (third dlnotes)
                (om-clip (mapcar #'(lambda (x) (+ x (om-random random-from random-to))) (fourth dlnotes)) 0 127)
                (fifth dlnotes)
                (sixth dlnotes)
                (seventh dlnotes))
        )

)

(defmethod! approx-lnotes-lmidic ((lnotes list) (approx integer))

            :initvals '(nil 2)
            :icon 9100
            :doc "Rounds the pitches of lnotes to: 1 - whole tone, 2 - semitone, 4 - quarter tone, 8 - eighth tone."

          (setf lnotes (mat-trans lnotes))
	
	(mapcar #'list
                (approx-m (first lnotes) approx)
                (second lnotes)
                (third lnotes)
                (fourth lnotes)
                (fifth lnotes)
                (sixth lnotes)
                (seventh lnotes))

)


;==================================================

(defun get-lnotes-lonset-no-duplicates (lnotes)
  "Sorted list of the onsets of lnotes without duplicates."

          (sort (remove-duplicates (second (mat-trans lnotes))) #'<)

)

(defun get-lnotes-lmidic-no-duplicates (lnotes)
  "Sorted list of the pitches (midicents) of lnotes without duplicates."

          (sort (remove-duplicates (first (mat-trans lnotes))) #'<)

)

(defun get-lnotes-lvel-no-duplicates (lnotes)
  "Sorted list of the velocities of lnotes without duplicates."

          (sort (remove-duplicates (fourth (mat-trans lnotes))) #'<)

)

(defun get-lnotes-lchan-no-duplicates (lnotes)
  "Sorted list of the MIDI channels of lnotes without duplicates."

          (sort (remove-duplicates (sixth (mat-trans lnotes))) #'<)

)

(defun get-start-lnotes-lonset (lnotes)
  "Earliest onset of lnotes."

  (list-min (get-lnotes-lonset-no-duplicates lnotes))

)

;==================================================

(defmethod! get-gcd-lnotes-ldur ((lnotes list))
  :doc "Greatest common divisor of the note durations of lnotes."

            :initvals '(nil)
            :icon 9100

  (reduce #'gcd (third (mat-trans lnotes)))


)

;==================================================

(defmethod! divide-lnotes-duration ((lnotes list))
  :doc "Splits every note of lnotes at all onsets and offsets of other notes, so that notes no longer overlap partially."

            :initvals '(nil)
            :icon 9100

          (let* ((onsets (sort (remove-duplicates (append '(0) (mapcar #'second lnotes) (mapcar #'(lambda (x) (+ (second x) (third x))) lnotes) (list (total-lnotes-duration lnotes)))) #'<))
                 (ons-ons (mapcar #'list (butlast onsets) (cdr onsets)))
                )

            (flat
             (loop for o-d in ons-ons
                   collect (sort (cut-lnotes-from-to-no-shift lnotes (first o-d) (second o-d)) #'< :key #'first))
            1)

            )
)

(defmethod! divide-lnotes-duration-by-lonset ((lnotes list) (lonset list))
  :doc "Splits the notes of lnotes at the given onsets <lonset>."

            :initvals '(nil '(0 125 250 300))
            :icon 9100

            (let ((ons-ons (mapcar #'list (butlast lonset) (cdr lonset))))

              (flat
               (loop for o-d in ons-ons
                     collect (sort (cut-lnotes-from-to-no-shift lnotes (first o-d) (second o-d)) #'< :key #'first))
               1)

              )
)


(defmethod! divide-lnotes-duration-dur-min ((lnotes list) (dur-min number))
  :doc "Splits every note into repeated notes of duration <dur-min> ms."

            :initvals '(nil 125)
            :icon 9100

            (flat
             (loop for note in lnotes
                   collect (let ((n-div (1- (round (third note) dur-min))))
                             (loop for i from 0 to n-div
                                   collect (list
                                            (first note)
                                            (+ (* i dur-min) (second note))
                                            dur-min
                                            (fourth note)
                                            (fifth note)
                                            (sixth note)
                                            (seventh note)))))
             1)

)

(defmethod! divide-lnotes-duration-dur-min-ceiling ((lnotes list) (dur-min number))
  :doc "Splits every note into repeated notes of duration <dur-min> ms (durations rounded up)."

            :initvals '(nil 125)
            :icon 9100

            (flat
             (loop for note in lnotes
                   collect (let ((n-div (1- (round (third note) dur-min))))
                             (loop for i from 0 to n-div
                                   collect (list
                                            (first note)
                                            (+ (* i dur-min) (second note))
                                            (ceiling dur-min)
                                            (fourth note)
                                            (fifth note)
                                            (sixth note)
                                            (seventh note)))))
             1)

)

;==================================================
;                   LIST-LNOTES
;==================================================

(defun list-lnotes-by-lmidic (lnotes)
  "Groups lnotes by pitch: a list of lnotes, one per pitch, sorted by onset."

  (loop for midic in (get-lnotes-lmidic-no-duplicates lnotes)
        collect (sort (remove midic lnotes :test #'/= :key #'first) #'< :key #'second))

)

(defun list-lnotes-by-lonset (lnotes)
  "Groups lnotes by onset: a list of chords (lnotes with the same onset), sorted by pitch."

  (loop for onset in (get-lnotes-lonset-no-duplicates lnotes)
        collect (sort (remove onset lnotes :test #'/= :key #'second) #'< :key #'first))

)

(defun list-lnotes-by-lchan (lnotes)
  "Groups lnotes by MIDI channel: a list of lnotes, one per channel."

  (loop for chan in (get-lnotes-lchan-no-duplicates lnotes)
        collect (sort (remove chan lnotes :test #'/= :key #'sixth) #'< :key #'second))

)


(defun list-lnotes-by-chan (lnotes chan)
  "Notes of lnotes on the MIDI channel CHAN, sorted by onset."

  (if lnotes (sort (remove chan lnotes :test #'/= :key #'sixth) #'< :key #'second) nil)

)

(defun list-lnotes-by-chan-list (lnotes lchan)
  "Notes of lnotes for every channel in LCHAN (a list of lnotes)."

  (loop for chan in lchan
        collect (list-lnotes-by-chan lnotes chan))

)

(defun lnotes-no-chan (lnotes chan)
  "Notes of lnotes without the MIDI channel CHAN."

  (if lnotes (sort (remove chan lnotes :test #'= :key #'sixth) #'< :key #'second) nil)

)

;==================================================

(defmethod! divide-dur-&-reduct-dupl-lnotes ((lnotes list))
  :doc "Splits overlapping notes (divide-lnotes-duration) and removes duplicates: at the same onset and pitch only the loudest note remains."

            :initvals '(nil)
            :icon 9100

            (let* ((dlnotes (divide-lnotes-duration lnotes))
                   (llnotes (list-lnotes-by-lonset dlnotes)))

              (setf llnotes (flat
                             (loop for tlnotes in llnotes
                                   collect (list-lnotes-by-lmidic tlnotes)
                                   )
                             1))
              (loop for tlnotes in llnotes
                    collect (first (sort tlnotes #'> :key #'fourth)))
              )

)

(defmethod! divide-dur-min-dur-&-reduct-dupl-lnotes ((lnotes list) (dur-min number))
  :doc "Splits notes into pieces of <dur-min> ms and removes duplicates: at the same onset and pitch only the loudest note remains."

            :initvals '(nil 125)
            :icon 9100

            (let* ((dlnotes (divide-lnotes-duration-dur-min lnotes dur-min))
                   (llnotes (list-lnotes-by-lonset dlnotes)))

              (setf llnotes (flat
                             (loop for tlnotes in llnotes
                                   collect (list-lnotes-by-lmidic tlnotes)
                                   )
                             1))
              (loop for tlnotes in llnotes
                    collect (first (sort tlnotes #'> :key #'fourth)))
              )

)

(defmethod! reduct-dupl-lnotes ((lnotes list))
  :doc "Removes duplicates: at the same onset and pitch only the loudest note remains."

            :initvals '(nil 125)
            :icon 9100

            (let* ((llnotes (list-lnotes-by-lonset lnotes)))

              (setf llnotes (flat
                             (loop for tlnotes in llnotes
                                   collect (list-lnotes-by-lmidic tlnotes)
                                   )
                             1))
              (loop for tlnotes in llnotes
                    collect (first (sort tlnotes #'> :key #'fourth)))
              )

)


;==================================================
;                LOUDEST-LNOTES
;==================================================

(defmethod! loudest-lnotes-pno-facture ((lnotes list) (number integer) &optional (from 1))
  :doc "For every onset keeps the <number> loudest notes, starting from the <from>-th loudest."

            :initvals '(nil 2 1)
            :icon 9100

              (flat
               (loop for onset in (get-lnotes-lonset-no-duplicates lnotes)
                     collect (first-n (subseq (sort (remove onset lnotes :test #'/= :key #'second) #'> :key #'fourth) (1- from)) number))
               1)
)                            


(defmethod! loudest-lnotes-with-tie ((lnotes list) (number integer) &optional (from 1))
  :doc "Keeps the <number> loudest notes at every moment and ties repeated notes."

            :initvals '(nil 4 1)
            :icon 9100

         (let ((lnotes (divide-dur-&-reduct-dupl-lnotes lnotes)))

           (tie-lnotes (loudest-lnotes-pno-facture lnotes number from) 1)

           )

)

(defmethod! loudest-lnotes-without-tie ((lnotes list) (number integer) &optional (from 1))
  :doc "Keeps the <number> loudest notes at every moment, without tying."

            :initvals '(nil 4 1)
            :icon 9100

         (let ((lnotes (divide-dur-&-reduct-dupl-lnotes lnotes)))

           (loudest-lnotes-pno-facture lnotes number from)

           )

)

;==================================================

(defmethod! list-lnotes-by-instruments ((lnotes list) (instruments-ranges list) (instruments-number list) &optional (min-vel 1))
  :doc "Distributes lnotes among instruments with the given ranges and numbers (e.g. from set-orchestra or set-choir). Returns a list of lnotes, one per instrument."

	:initvals '(nil nil 4 1)
	:icon 9100

            (let* ((instruments-sequences nil)
                   (in-chords-list (list-lnotes-by-lonset lnotes)))

              (setf lnotes (cut-lnotes-below-vel lnotes min-vel))

              (om-print (format nil "~48:@<voices are creating~>~%"))
              (remove nil
                      (flat
                       (labels ((set-instruments-sequences (d-in-chords-list d-instruments-ranges d-instruments-number)

                                  (if (null d-instruments-ranges) (return-from set-instruments-sequences instruments-sequences))
                                  (let* ((instrument-range (car d-instruments-ranges))
                                         (instruments-number-parts (car d-instruments-number))
                                         (instruments-in-chords-list (mapcar 
                                                                      #'(lambda (x)
                                                                          (remove
                                                                           (list-max instrument-range)
                                                                           (remove (list-min instrument-range) x :key #'first :test #'>)
                                                                           :key #'first :test #'<))
                                                                      d-in-chords-list))
                                         (instrument-sequences (remove nil (loop for chord in instruments-in-chords-list
                                                                                 collect (sort (first-n (sort chord #'> :key #'fourth) instruments-number-parts) #'> :key #'first))))
                                         (instrument-sequences-part (loop for i from 0 below (car d-instruments-number)
                                                                          collect (remove nil (mapcar #'(lambda (x) (nth i x)) instrument-sequences)))))

                                    (when (car instrument-sequences-part)
                                      (push instrument-sequences-part instruments-sequences)
                                      (om-print (format nil "~48:@<voice: ~D~>" (length (remove nil (flat instruments-sequences 1))))))

                                    (set-instruments-sequences (labels ((d-in-chords-list-upgrade (d-d-in-chords-list d-instrument-sequences)
                                                                          (if (null d-instrument-sequences) (return-from d-in-chords-list-upgrade d-d-in-chords-list))
                                                                          (d-in-chords-list-upgrade (loop for c in d-d-in-chords-list
                                                                                                          collect (remove (car d-instrument-sequences) c))
                                                                                                    (cdr d-instrument-sequences))))
                                                                 (d-in-chords-list-upgrade d-in-chords-list (flat instrument-sequences 1)))
                                                               (cdr d-instruments-ranges)
                                                               (cdr d-instruments-number)))))
                         (set-instruments-sequences in-chords-list (reverse instruments-ranges) (reverse instruments-number)))
                       1)
                      )
              )
)

;__________________________________________________
;

(defmethod! list-lnotes-by-streams ((in-lnotes list) (min-dur number) &optional (min-vel 1) (max-streams integer) (tied-lnotes t) (level-tie number))
  :doc "Splits lnotes into monophonic streams (voices), from the loudest notes; <min-dur>, <min-vel>, <max-streams> limit the result, <tied-lnotes> and <level-tie> control tying."

     :initvals '(nil 125 1 40 t 256)
     :icon 9100

 (setf in-lnotes (cut-lnotes-below-vel in-lnotes min-vel))
 (let* ((lnotes in-lnotes)
        seqs
        seq)
   (om-print (format nil "~%~48:@<streams are creating~>~2%min vel cut set"))
   (loop for i from min-vel
         do (princ "*" *om-stream*)
            (setf lnotes (cut-lnotes-below-vel in-lnotes i)
                  lnotes (sort (sort (sort
                                      (if tied-lnotes (tie-lnotes lnotes level-tie) lnotes)
                                      #'> :key #'third) #'< :key #'first) #'< :key #'second)
                  seqs nil seq nil)
            (loop if (null lnotes)
                  do (return nil)
                  do (loop named seq-loop
                           if (null lnotes)
                           do (return-from seq-loop nil)
                           if (null seq)
                           do (push (pop lnotes) seq)
                           do (let ((d-lnotes (if lnotes (remove (+ (cadar seq) (caddar seq)) lnotes :test #'> :key #'cadr))))
                                (loop if (null d-lnotes)
                                      do (return-from seq-loop (push (loop if seq collect (pop seq) into d-seq else do (return d-seq)) seqs))
                                      do (let ((m-d-lnote (caar d-lnotes)))
                                           (when
                                               (and
                                                (>= (cadar d-lnotes) (+ (cadar seq) (caddar seq)))
                                                (/= (abs (- m-d-lnote (caar seq))) 700)
                                                (<= (abs (- m-d-lnote (caar seq))) 1000)
                                                (if (null (cdr seq)) t (and (<= (abs (- m-d-lnote (caadr seq))) 1000)
                                                                            (null
                                                                             (let ((last3 (sort (list (caadr seq) (caar seq) m-d-lnote) #'<)))
                                                                               (= (- (second last3) (first last3)) (- (third last3) (second last3)) 100))
                                                                             )))
                                                (if (null (cddr seq)) t (<= (abs (- m-d-lnote (caaddr seq))) 1000))
                                                (if (null (cdddr seq)) t (<= (abs (- m-d-lnote (car (cadddr seq)))) 1200))
                                                (if (null (cddddr seq)) t (<= (abs (- m-d-lnote (caar (cddddr seq)))) 1200))
                                                (let* ((dseq (remove (list-min *string-orch-ranges*) (remove (list-max *string-orch-ranges*) seq :test #'< :key #'car) :test #'> :key #'car))
                                                       (min-seq (if dseq (list-min (mapcar #'car dseq)) 1))
                                                       (max-seq (if dseq (list-max (mapcar #'car dseq)) 2))
                                                       (min-ranges (mapcar #'(lambda (x) (if (<= (car x) min-seq (cadr x)) (car x))) *string-orch-ranges*))
                                                       (max-ranges (mapcar #'(lambda (x) (if (<= (car x) max-seq (cadr x)) (cadr x))) *string-orch-ranges*))
                                                       (min-max-ranges (remove nil (mapcar #'(lambda (x y) (if (and x y) (list x y))) min-ranges max-ranges)))
                                                       (min-range (if dseq (list-min (mapcar #'car min-max-ranges)) 1))
                                                       (max-range (if dseq (list-max (mapcar #'cadr min-max-ranges)) 2)))
                                                  (<= min-range m-d-lnote max-range)
                                                  )
                                                )
                                             (push (pop d-lnotes) seq)
                                             (setf lnotes (remove (car seq) lnotes))
                                             (return nil)))
                                         (pop d-lnotes)))))
            if (<= (length seqs) max-streams)
            do (om-print (format nil "~%~48:@<real vel-cut: ~D~>~%" i))
               (return nil))
   (let ((streams-number (length seqs))
         out)
     (om-print (format nil "~48:@<~D streams:~>~%" streams-number))
     (setf out
           (mapcar #'(lambda (x) (remove nil x)) (loop for i from 1
                                                       for div-seq in (mapcar #'(lambda (x) (divide-lnotes-duration-dur-min x min-dur)) seqs)
                                                       do (om-print (format nil "~48:@<stream: ~D/~D~>" i streams-number))
                                                          (om-listener-echo (string+ " " (write-to-string (round (/ i streams-number) (/ 100))) " %"))
                                                       collect (loop for note in div-seq
                                                                     collect (loop for in-lnote in in-lnotes
                                                                                   if (every #'= (subseq note 0 2) (subseq in-lnote 0 2))
                                                                                   do (setf in-lnotes (remove in-lnote in-lnotes))
                                                                                      (return in-lnote))))))
     (om-listener-echo "Running...")
     out
     )
   )
)
                                          
;__________________________________________________
;

(defmethod! list-lnotes-by-streams-choir ((in-lnotes list) (min-dur integer) &optional (min-vel 1))
  :doc "Splits lnotes into monophonic streams for a choir (notes tied first)."

     :initvals '(nil 1)
     :icon 9100

 (setq in-lnotes (cut-lnotes-below-vel in-lnotes min-vel))

 (let* ((lnotes in-lnotes)
        seqs
        seq)

   (setq lnotes (sort (sort (sort (tie-lnotes lnotes 256) #'> :key #'third) #'< :key #'first) #'< :key #'second))
   (om-print (format nil "~48:@<streams are creating~>~%"))
   (loop if (null lnotes)
         do (return nil)
         do (loop named seq-loop
                  if (null lnotes)
                  do (return-from seq-loop nil)
                  if (null seq)
                  do (push (pop lnotes) seq)
                  do (let ((d-lnotes (remove (+ (cadar seq) (caddar seq)) lnotes :test #'> :key #'cadr)))
                       (loop if (null d-lnotes)
                             do (return-from seq-loop (push (loop if seq collect (pop seq) into d-seq else do (return d-seq)) seqs))
                             do (let ((m-d-lnote (caar d-lnotes)))
                                  (when
                                      (and
                                       (>= (cadar d-lnotes) (+ (cadar seq) (caddar seq)))
                                       (<= (abs (- m-d-lnote (caar seq))) 400)
                                       (if (null (cdr seq)) t (<= (abs (- m-d-lnote (caadr seq))) 600))
                                       (if (null (cddr seq)) t (<= (abs (- m-d-lnote (caaddr seq))) 800))
                                       (if (null (cdddr seq)) t (<= (abs (- m-d-lnote (car (cadddr seq)))) 1000))
                                       (if (null (cddddr seq)) t (<= (abs (- m-d-lnote (caar (cddddr seq)))) 1200))
                                       (let* ((dseq (remove (list-max *string-orch-ranges*) seq :test #'< :key #'car))
                                              (dseq (remove (list-min *string-orch-ranges*) dseq :test #'> :key #'car))
                                              (min-seq (list-min (mapcar #'car dseq)))
                                              (max-seq (list-max (mapcar #'car dseq)))
                                              (min-ranges (mapcar #'(lambda (x) (if (<= (car x) min-seq (cadr x)) (car x))) *choir-ranges*))
                                              (max-ranges (mapcar #'(lambda (x) (if (<= (car x) max-seq (cadr x)) (cadr x))) *choir-ranges*))
                                              (min-max-ranges (remove nil (mapcar #'(lambda (x y) (if (and x y) (list x y))) min-ranges max-ranges)))
                                              (min-range (list-min (mapcar #'car min-max-ranges)))
                                              (max-range (list-max (mapcar #'cadr min-max-ranges))))
                                         (<= min-range m-d-lnote max-range)
                                        )
                                       )
                                  (push (pop d-lnotes) seq)
                                  (setq lnotes (remove (car seq) lnotes))
                                  (return nil)
                                ))
                             do (pop d-lnotes)))))
   (om-print (format nil "~48:@<~D streams:~>~%" (length seqs)))
   (let* ((streams-number (length seqs))
          (div-seqs (mapcar #'(lambda (x) (divide-lnotes-duration-dur-min x min-dur)) seqs))
          (out-lnotes-list (loop for i from 1
                                 for div-seq in div-seqs
                                 do (om-print (format nil "~48:@<stream: ~D/~D~>" i streams-number))
                                 collect (loop for note in div-seq
                                               collect (loop for in-lnote in in-lnotes
                                                             if (every #'= (subseq note 0 2) (subseq in-lnote 0 2))
                                                             do (progn (setq in-lnotes (remove in-lnote in-lnotes)) (return in-lnote)))))))

     out-lnotes-list)
   )
)

;==================================================

(defmethod! reverse-lnotes ((lnotes list))
  :doc "Reverses lnotes in time (retrograde)."

     :initvals '(nil)
     :icon 9100

          (setf lnotes (mat-trans lnotes))
  
    (sort (mapcar #'list
                    (first lnotes)
                    (let ((new-onset-rev (om+ (flat (second lnotes)) (flat (third lnotes)))))
          	       (om- (list-max new-onset-rev) new-onset-rev))
                    (third lnotes)
                    (fourth lnotes)
                    (fifth lnotes)
                    (sixth lnotes)
                    (seventh lnotes))
    #'< :key #'second)

)

(defmethod! mirror-lnotes ((lnotes list) (mirror integer))
  :doc "Mirrors lnotes in time around <mirror> ms (note ends become starts)."

     :initvals '(nil 1000)
     :icon 9100

          (setf mlnotes (mat-trans lnotes))
  
    (sort (mapcar #'list
                    (first mlnotes)
                    (om- mirror (om+ (second mlnotes) (third mlnotes)))
                    (third mlnotes)
                    (fourth mlnotes)
                    (fifth mlnotes)
                    (sixth mlnotes)
                    (seventh mlnotes))
    #'< :key #'second)

)

(defmethod! mirror_2-lnotes ((lnotes list) (mirror integer))
  :doc "Mirrors the onsets of lnotes around <mirror> ms (durations unchanged)."

     :initvals '(nil 1000)
     :icon 9100

          (setf mlnotes (mat-trans lnotes))
  
    (sort (mapcar #'list
                    (first mlnotes)
                    (om- mirror (second mlnotes))
                    (third mlnotes)
                    (fourth mlnotes)
                    (fifth mlnotes)
                    (sixth mlnotes)
                    (seventh mlnotes))
    #'< :key #'second)

)


(defmethod! repeat-lnotes ((lnotes list) (n integer))
  :doc "Repeats lnotes <n> times one after another."

     :initvals '(nil 1)
     :icon 9100

  (setf lnotes (sort lnotes #'< :key #'second))

  (let* ((lnotes (mat-trans lnotes))
         (lmidic (first lnotes))
         (lonset (second lnotes))
         (ldur (third lnotes))
         (lvel (fourth lnotes))
         (loffset (fifth lnotes))
         (lchan (sixth lnotes))
         (legato (seventh lnotes))

         (duration-1 (+ (car (last lonset)) (car (last ldur)))))
 
    	(mapcar #'list
                (flat (loop repeat n collect lmidic))
                (flat (loop for i from 1 to n
                            collect (om+ lonset (* (1- i) duration-1))))
                (flat (loop repeat n collect ldur))
                (flat (loop repeat n collect lvel))
                (flat (loop repeat n collect loffset))
                (flat (loop repeat n collect lchan))
                (flat (loop repeat n collect legato))
                )

    )

)



(defmethod! lnotes-staccato ((in-lnotes list))
  :doc "Staccato: halves every note duration."

     :initvals '(nil)
     :icon 9100

 	(let* ((lnotes (mat-trans in-lnotes))
               (n-ldur (length (third lnotes)))
               )

	(mapcar #'list
                (first lnotes)
                (second lnotes)
                (mapcar #'(lambda (x) (floor x 2)) (third lnotes))
                (fourth lnotes)
                (fifth lnotes)
                (sixth lnotes)
                (seventh lnotes))
        )

)

(defmethod! lnotes-staccato-mindur ((in-lnotes list) (dur integer))
  :doc "Staccato: sets every note duration to <dur> ms."

     :initvals '(nil 125)
     :icon 9100

 	(let* ((lnotes (mat-trans in-lnotes))
               (n-ldur (length (third lnotes)))
               )

	(mapcar #'list
                (first lnotes)
                (second lnotes)
                (loop repeat n-ldur collect dur)
                (fourth lnotes)
                (fifth lnotes)
                (sixth lnotes)
                (seventh lnotes))
        )

)

(defmethod! lnotes-overlaps ((in-lnotes list))
  :doc "Lengthens every note by 5% so that consecutive notes overlap."

     :initvals '(nil)
     :icon 9100



 	(let* ((lnotes (mat-trans in-lnotes))
               )

	(mapcar #'list
                (first lnotes)
                (second lnotes)
                (om* 21/20 (third lnotes))
                (fourth lnotes)
                (fifth lnotes)
                (sixth lnotes)
                (seventh lnotes))
        )

)

(defmethod! lnotes-overlaps-mindur ((in-lnotes list) (min-dur integer))
  :doc "Lengthens notes by 1/20 of <min-dur> on every channel so that consecutive notes overlap."

     :initvals '(nil 125)
     :icon 9100

  (let ((llnotes-by-chan (list-lnotes-by-lchan in-lnotes)))
    (flat
     (loop for lnotes in llnotes-by-chan
           collect (let* ((dur+ (round (* 1/20 min-dur))))
                     (setf lnotes (mat-trans lnotes))
                     
                     (mapcar #'list
                             (first lnotes)
                             (second lnotes)
                             (om+ dur+ (third lnotes))
                             (fourth lnotes)
                             (fifth lnotes)
                             (sixth lnotes)
                             (seventh lnotes))
                     ))
     1)
    )
)

(defmethod! lnotes-fixed-length ((in-lnotes list) (dur integer))
  :doc "Sets every note duration to <dur> ms."

     :initvals '(nil 125)
     :icon 9100

 	(let* ((lnotes (mat-trans in-lnotes))
               (n-ldur (length (third lnotes)))
               )

	(mapcar #'list
                (first lnotes)
                (second lnotes)
                (loop repeat n-ldur collect dur)
                (fourth lnotes)
                (fifth lnotes)
                (sixth lnotes)
                (seventh lnotes))
        )

)



(defmethod! lnotes-portato ((in-lnotes list))
  :doc "Portato: shortens every note duration to 3/4."

     :initvals '(nil)
     :icon 9100

 	(let* ((lnotes (mat-trans in-lnotes))
               )

	(mapcar #'list
                (first lnotes)
                (second lnotes)
                (mapcar #'(lambda (x) (floor x 4/3)) (third lnotes))
                (fourth lnotes)
                (fifth lnotes)
                (sixth lnotes)
                (seventh lnotes))
        )

)

(defmethod! lnotes-legato ((in-lnotes list))
  :doc "Legato: every note lasts until the next one (overlaps removed)."

    :initvals '(nil)
    :icon 9100

    
    (lnotes-delete-overlaps (append (butlast (lnotes-fixed-length in-lnotes 5000)) (list (last-elem (sort in-lnotes #'< :key #'second)))))

)


(defmethod! lnotes-scale-filter ((lnotes list) (scale list))
  :doc "Keeps only notes whose pitch class belongs to <scale> (midicents, transposed by octaves)."

     :initvals '(nil nil)
     :icon 9100

  (let* ((lmidic-min (list-min (mapcar #'first lnotes)))
         (lmidic-max (list-max (mapcar #'first lnotes)))
         (d-scale (remove-duplicates (flat (remove nil (loop for note in scale
                                                             collect (loop for i from 0
                                                                           until (< (- note (* i 1200)) lmidic-min)
                                                                           collect (- note (* i 1200)))
                                                             collect (loop for i from 0
                                                                           until (> (+ note (* i 1200)) lmidic-max)
                                                                           collect (+ note (* i 1200)))))))))
    (sort
     (sort
      (remove nil
              (flat
               (loop for note in d-scale
                     collect (remove note lnotes :test #'/= :key #'first))
               1)
              )
      #'< :key #'first)
     #'< :key #'second)
    )
)

(defmethod! separate-lnotes-notes ((lnotes list))
  :doc "Separates the notes of lnotes (via chord-seq), like EXPLOSION."

     :initvals '(nil)
     :icon 9100

     (chseq-to-lnotes (lnotes-to-chseq lnotes))

)

(defmethod! consolidate-lnotes-notes ((lnotes list))
  :doc "Merges notes with the same onset into chords: one entry per onset with lists of pitches, durations, velocities etc."

     :initvals '(nil)
     :icon 9100

  (let ((llnotes (list-lnotes-by-lonset lnotes))
        temp)

    (loop for notes in llnotes
          collect (progn 
                    (setf temp (mat-trans notes))
                    (list 
                     (first temp)
                     (car (second temp))
                     (car (third temp))
                     (round (om-mean (fourth temp)))
                     (car (fifth temp))
                     (car (sixth temp))
                     (car (seventh temp))
                     )))

    )

)

(defmethod! lnotes-band-midic-filter ((lnotes list) (band-midic list))
  :doc "Keeps only notes within the pitch band <band-midic> (min max)."

     :initvals '(nil '(5500 9300))
     :icon 9100

     (if lnotes
         (remove nil (remove (list-max band-midic) (remove nil (remove (list-min band-midic) lnotes :key #'first :test #'>)) :key #'first :test #'<)))

)

(defmethod! lnotes-vn-range-filter ((lnotes list))
  :doc "Keeps only notes within the violin range."

     :initvals '(nil)
     :icon 9100

     (lnotes-band-midic-filter lnotes *vn-range*)

)

(defmethod! lnotes-vl-range-filter ((lnotes list))
  :doc "Keeps only notes within the viola range."

     :initvals '(nil)
     :icon 9100

     (lnotes-band-midic-filter lnotes *vl-range*)

)

(defmethod! lnotes-vc-range-filter ((lnotes list))
  :doc "Keeps only notes within the cello range."

     :initvals '(nil)
     :icon 9100

     (lnotes-band-midic-filter lnotes *vc-range*)

)

(defmethod! lnotes-cb-range-filter ((lnotes list))
  :doc "Keeps only notes within the double bass range."

     :initvals '(nil)
     :icon 9100

     (lnotes-band-midic-filter lnotes *cb-range*)

)




(defmethod! lnotes-delete-overlaps ((in-lnotes list))
  :doc "Removes overlaps on every channel: each note is shortened to end at the start of the next one."

     :initvals '(nil)
     :icon 9100

  (let ((llnotes-by-chan (list-lnotes-by-lchan in-lnotes)))
    (flat
     (loop for lnotes in llnotes-by-chan
           collect (let* ((llnotes-sorted (list-lnotes-by-lonset lnotes))
                          (llnotes-sorted-butlast (butlast llnotes-sorted))
                          (llnotes-sorted-cdr (cdr llnotes-sorted))
                          )
                     (flat
                      (append
                       (loop for lnotes-sorted-but in llnotes-sorted-butlast
                             for lnotes-sorted-cdr in llnotes-sorted-cdr
                             collect (loop for lnote in lnotes-sorted-but
                                           collect (list (first lnote)
                                                         (second lnote)
                                                         (let ((3rdlnote (third lnote))
                                                               (cut3rdlnote (- (second (car lnotes-sorted-cdr)) (second lnote))))
                                                           (if (< 3rdlnote cut3rdlnote) 3rdlnote cut3rdlnote))
                                                         (fourth lnote)
                                                         (fifth lnote)
                                                         (sixth lnote)
                                                         (seventh lnote))))
                       (list (last-elem llnotes-sorted)))
                      1)
                     ))
     1)
   )
)

(defmethod! lnotes-quantize ((lnotes list) (note-dur integer))
  :doc "Quantises onsets and durations to multiples of <note-dur> ms; notes with duration 0 are removed."

     :initvals '(nil 125)
     :icon 9100

     (loop for lnote in lnotes
           do (setf
               (third lnote) (* (round (/ (third lnote) note-dur)) note-dur)
               (second lnote) (* (round (/ (second lnote) note-dur)) note-dur)))

     (remove 0 lnotes :test #'= :key #'third)

)

(defmethod! lnotes-quantize-3 ((lnotes list) (note-dur integer))
  :doc "Quantises onsets and durations to multiples of <note-dur> ms; durations of 0 become <note-dur>."

     :initvals '(nil 125)
     :icon 9100

     (let (third-note)

     (loop for lnote in lnotes
           do (setf
               third-note (* (round (/ (third lnote) note-dur)) note-dur)
               (third lnote) (if (= third-note 0) note-dur third-note)
               (second lnote) (* (round (/ (second lnote) note-dur)) note-dur)))
     )
     (remove 0 lnotes :test #'= :key #'third)

)

(defmethod! lnotes-quantize-4 ((lnotes list) (note-dur integer))
  :doc "Quantises onsets down and durations to the nearest multiples of <note-dur> ms; durations of 0 become <note-dur>."

     :initvals '(nil 125)
     :icon 9100

     (let (third-note)

     (loop for lnote in lnotes
           do (setf
               third-note (* (round (/ (third lnote) note-dur)) note-dur)
               (third lnote) (if (= third-note 0) note-dur third-note)
               (second lnote) (* (floor (/ (second lnote) note-dur)) note-dur)))
     )
     (remove 0 lnotes :test #'= :key #'third)

)

(defmethod! lnotes-quantize-2 ((lnotes list))
  :doc "Quantises onsets and durations to the greatest common divisor of the durations."

     :initvals '(nil)
     :icon 9100

     (let ((note-dur (get-gcd-lnotes-ldur lnotes)))
     (loop for lnote in lnotes
           do (setf
               (third lnote) (* (round (/ (third lnote) note-dur)) note-dur)
               (second lnote) (* (round (/ (second lnote) note-dur)) note-dur)))
     (remove 0 lnotes :test #'= :key #'third)
     )

)

(defmethod! lnotes-quantize-5 ((lnotes list) (note-dur integer))
  :doc "Quantises only durations to multiples of <note-dur> ms; notes with duration 0 are removed."

     :initvals '(nil 125)
     :icon 9100

     (loop for lnote in lnotes
           do (setf
               (third lnote) (* (round (/ (third lnote) note-dur)) note-dur)
               ))

     (remove 0 lnotes :test #'= :key #'third)

)

(defmethod! lnotes-quantize-5a ((lnotes list) (note-dur number))
  :doc "Quantises onsets and durations to multiples of <note-dur> ms (any number); notes with duration 0 are removed."

     :initvals '(nil 125)
     :icon 9100

     (loop for lnote in lnotes
           do (setf
               (third lnote) (* (round (/ (third lnote) note-dur)) note-dur)
               (second lnote) (* (round (/ (second lnote) note-dur)) note-dur)
               ))

     (remove 0 lnotes :test #'= :key #'third)

)


;==================================================

(defmethod! explode-lnotes ((in-lnotes list))
  :doc "Splits lnotes into groups of notes that do not overlap in time (like EXPLOSION). Returns llnotes."

            :initvals '(nil)
            :icon 9100

 (let ((lnotes (sort (sort in-lnotes #'> :key #'first) #'< :key #'second))
       d-lnotes
       out-llnotes)

   (loop if (null lnotes)
         do (return nil)
         do (loop do (push (pop lnotes) d-lnotes)
                  if (null (remove (+ (second (first d-lnotes)) (third (first d-lnotes))) lnotes :test #'> :key #'second))
                  do (push (reverse d-lnotes) out-llnotes)
                     (setf d-lnotes nil)
                     (setf lnotes (sort (sort lnotes #'> :key #'first) #'< :key #'second))
                     (return nil)
                  do (setf lnotes (rotate lnotes (position (+ (second (first d-lnotes)) (third (first d-lnotes))) lnotes :test #'<= :key #'second)))))
   (reverse out-llnotes))

)

(defmethod! total-lnotes-duration ((lnotes list))
  :doc "Total duration of lnotes in ms (the latest onset + duration)."

        :initvals '(nil)
        :icon 9100

        (total-llnotes-duration (list lnotes))
)

(defun min-max-list-lnotes-vel (lnotes)
  "Lowest and highest velocity of lnotes."

  (let ((vel (get-lnotes-lvel-no-duplicates lnotes)))
    (list (first vel) (last-elem vel))
    )

)

;==================================================
;                    TIE-LNOTES
;==================================================


(defmethod! tie-lnotes ((in-lnotes list) (vel-toler integer))
  :doc "Ties repeated notes of the same pitch on every channel when their velocities differ by at most <vel-toler>."

            :initvals '(nil 20)
            :icon 9100

  (let ((llnotes-by-chan (list-lnotes-by-lchan in-lnotes)))
    (flat
     (loop for lnotes in llnotes-by-chan
           collect (let ((llnotes (list-lnotes-by-lmidic (cut-lnotes-below-vel lnotes 1)))
                         vels
                         durs
                         temp
                         out)
                     (loop for lmnotes in llnotes
                           do (loop for note in lmnotes
                                    if (not temp)
                                      do (push note temp)
                                         (push (fourth note) vels)
                                         (push (third note) durs)
                                    else
                                      do (if (or
                                              (< (+ (second (car temp)) (third (car temp))) (second note))
                                              (> (fourth note) (+ (list-min vels) vel-toler))
                                              (< (fourth note) (- (list-max vels) vel-toler))
                                              )
                                             (progn
                                               (setf (fourth (car temp)) (round (om-mean vels durs))
                                                     vels nil
                                                     durs nil)
                                               (push (fourth note) vels)
                                               (push (third note) durs)
                                               (push note temp)
                                               )
                                           (progn
                                             (setf (third (car temp)) (- (+ (second note) (third note)) (second (car temp))))
                                             (push (fourth note) vels)
                                             (push (third note) durs)
                                             )
                                           )
                                      )
                              (setf (fourth (car temp)) (round (om-mean vels durs))
                                    vels nil
                                    durs nil)             
                              (push temp out)
                              (setf temp nil))
                     (sort (flat out 1) #'< :key #'second)
                     )
           )
     1)
    )
)


(defmethod! tie-some-lnotes ((lnotes list) (vel-toler integer) (factor number))
  :doc "Ties repeated notes in a random part (<factor> 0..1) of lnotes."

            :initvals '(nil 20 0.5)
            :icon 9100

   (let* ((n (floor (* factor (length lnotes))))
          temp-lnotes)

     (loop repeat n
           do (setq lnotes (rotate lnotes (om-random 0 (1- (length lnotes)))))
              (push (pop lnotes) temp-lnotes))

     (append lnotes (tie-lnotes temp-lnotes vel-toler)))
     
)


(defmethod! tie-lnotes-256 ((in-lnotes list))
  :doc "Ties all repeated notes of the same pitch on every channel (velocity tolerance 256)."

            :initvals '(nil)
            :icon 9100

  (let ((llnotes-by-chan (list-lnotes-by-lchan in-lnotes)))
    (flat
     (loop for lnotes in llnotes-by-chan
           collect (let ((llnotes (list-lnotes-by-lmidic (cut-lnotes-below-vel (separate-lnotes-notes lnotes) 1)))
                         vels
                         durs
                         temp
                         out)
                     (loop for lmnotes in llnotes
                           do (loop for note in lmnotes
                                    if (null temp)
                                      do (push note temp)
                                         (push (fourth note) vels)
                                         (push (third note) durs)
                                    else
                                      do (if (< (+ (second (car temp)) (third (car temp))) (second note))
                                             (progn
                                               (setf (fourth (car temp)) (round (om-mean vels durs))
                                                     vels nil
                                                     durs nil)
                                               (push (fourth note) vels)
                                               (push (third note) durs)
                                               (push note temp)
                                               )
                                           (progn
                                             (setf (third (car temp)) (- (+ (second note) (third note)) (second (car temp))))
                                             (push (fourth note) vels)
                                             (push (third note) durs)
                                             )
                                           )
                                      )
                              (setf (fourth (car temp)) (round (om-mean vels durs))
                                    vels nil
                                    durs nil)             
                              (push temp out)
                              (setf temp nil))
                     
                     (sort (flat out 1) #'< :key #'second)
                     )
           )
     1)
    )
)


(defun midic-lnotes (lnotes midic)
  "Notes of lnotes with pitch MIDIC, sorted by onset."

        (sort (remove midic lnotes :test #'/= :key #'first) #'< :key #'second)

)

;==================================================
;-------;
; ***** ;
;  ***  ;
;-------;

