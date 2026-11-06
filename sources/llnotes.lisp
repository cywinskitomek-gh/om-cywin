(in-package :om)

(defmethod! change-llnotes-tempo ((llnotes list) (multiplier number))
  :doc "Changes the tempo of every lnotes in llnotes (change-lnotes-tempo)."

            :initvals '(nil 0.5)
            :icon 9101

            (mapcar #'(lambda (x) (change-lnotes-tempo x multiplier)) llnotes)

)

(defmethod! cut-llnotes-below-vel ((llnotes list) (min-vel integer))
  :doc "Removes notes with velocity below <min-vel> from every lnotes in llnotes."

            :initvals '(nil 6)
            :icon 9101

            (remove nil (mapcar #'(lambda (x) (cut-lnotes-below-vel x min-vel)) llnotes))

)

(defmethod! scale-llnotes-velocity ((llnotes list) (maxout integer) (expotent number))
  :doc "Scales the velocities of llnotes to 0..<maxout> with the exponent <expotent> (common scale for all lnotes)."

            :initvals '(nil 79 1.0)
            :icon 9101

	(let* ((lnotes (mapcar #'mat-trans llnotes))
	       (lvel (mapcar #'fourth lnotes))
	       (minout (* (/ maxout (list-max lvel)) (list-min lvel)))
	      )
	
	(mapcar #'(lambda (x)
                    (mapcar #'list
                            (first x)
                            (second x)
                            (third x)
                            (om-round (om-scale^ (fourth x) minout maxout expotent))
                            (fifth x)
                            (sixth x)
                            (seventh x)))
                lnotes)
        )

)

(defmethod! shift-llnotes-lonset ((llnotes list) (shift integer))
  :doc "Shifts the onsets of every lnotes in llnotes by <shift> ms."

	:initvals '(nil 13)
        :icon 9101

	(mapcar #'(lambda (x) (shift-lnotes-lonset x shift)) llnotes)

)

(defmethod! shift-llnotes-lonset-to-beginning ((llnotes list))
  :doc "Shifts llnotes so that the earliest note starts at 0 ms."

	:initvals '(nil)
        :icon 9101

	(let* ((lnotes (mapcar #'mat-trans llnotes))
	       (lonset (mapcar #'second lnotes))
	       (shift (- (list-min lonset))))

	(mapcar #'(lambda (x) (shift-lnotes-lonset x shift)) llnotes)

	)

)

(defmethod! tie-llnotes ((llnotes list) (vel-toler integer))
  :doc "Ties repeated notes in every lnotes in llnotes (tie-lnotes with <vel-toler>)."

            :initvals '(nil 20)
            :icon 9101

            (mapcar #'(lambda (x) (tie-lnotes x vel-toler)) llnotes)
)

(defmethod! total-llnotes-duration ((llnotes list))
  :doc "Total duration of llnotes in ms (the latest onset + duration)."

        :initvals '(nil)
        :icon 9101

	(let* ((lnotes (mapcar #'mat-trans llnotes))
	       (lonset+dur (mapcar #'(lambda (x) (om+ (second x) (third x))) lnotes)))

          (list-max lonset+dur)
         )

)

(defmethod! reverse-llnotes ((llnotes list))
  :doc "Reverses llnotes in time (retrograde), keeping the total duration."

        :initvals '(nil)
        :icon 9101

	(let ((mlnotes (mapcar #'mat-trans llnotes))
              (duration (total-llnotes-duration llnotes)))

	(mapcar #'(lambda (x)
                    (sort (mapcar #'list
                            (first x)
                            (om- duration (om+ (second x) (third x)))
                            (third x)
                            (fourth x)
                            (fifth x)
                            (sixth x)
                            (seventh x))
                    #'< :key #'second))
                mlnotes)

        )

)


(defmethod! cut-n-llnotes-from ((llnotes list) (from-chord integer) (how-many integer) (gcd-lnotes-dur number))
  :doc "Cuts <how-many> chords starting from chord <from-chord> from every lnotes in llnotes (grid of <gcd-lnotes-dur> ms)."

     :initvals '(nil 1 16 125)
     :icon 9101

     (remove nil (mapcar #'(lambda (x) (cut-n-lnotes-from x from-chord how-many gcd-lnotes-dur)) llnotes) :key #'(lambda (x) (caar x)))

)


(defmethod! repeat-llnotes ((llnotes list) (n integer))
  :doc "Repeats llnotes <n> times one after another."

     :initvals '(nil 1)
     :icon 9101

	(let* ((mlnotes (mapcar #'mat-trans llnotes))
	       (duration (total-llnotes-duration llnotes)))

          (loop for lnotes in llnotes
                collect (let* ((lnotes (mat-trans lnotes))
                               (lmidic (first lnotes))
                               (lonset (second lnotes))
                               (ldur (third lnotes))
                               (lvel (fourth lnotes))
                               (loffset (fifth lnotes))
                               (lchan (sixth lnotes))
                               (legato (seventh lnotes)))
 
                               (mapcar #'list
                                       (flat (loop repeat n collect lmidic))
                                       (flat (loop for i from 1 to n
                                                   collect (om+ lonset (* (1- i) duration))))
                                       (flat (loop repeat n collect ldur))
                                       (flat (loop repeat n collect lvel))
                                       (flat (loop repeat n collect loffset))
                                       (flat (loop repeat n collect lchan))
                                       (flat (loop repeat n collect legato))
                                       )
                               )
                )

          )

)

(defmethod! llnotes-staccato ((llnotes list))
  :doc "Staccato for llnotes: halves every note duration."

      :initvals '(nil)
      :icon 9101

      (mapcar #'lnotes-staccato llnotes)

)

(defmethod! llnotes-portato ((llnotes list))
  :doc "Portato for llnotes: shortens every note duration to 3/4."

      :initvals '(nil)
      :icon 9101

      (mapcar #'lnotes-portato llnotes)

)

(defmethod! get-gcd-llnotes-ldur ((llnotes list))
  :doc "Greatest common divisor of all note durations in llnotes."

            :initvals '(nil)
            :icon 9101

            (get-gcd-lnotes-ldur (flat llnotes 1))

)

(defun get-start-llnotes-lonset (llnotes)
  "Earliest onset in llnotes."

  (get-start-lnotes-lonset (flat llnotes 1))

)

(defmethod! llnotes-scale-filter ((llnotes list) (scale list))
  :doc "Keeps in every lnotes only the notes whose pitch class belongs to <scale>."

            :initvals '(nil nil)
            :icon 9101

            (mapcar #'(lambda (x) (lnotes-scale-filter x scale)) llnotes)
)
