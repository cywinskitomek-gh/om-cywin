(in-package :om)

;==================================================

(defvar *all-harp-scales* (let* ((scale (om+ '(0 200 400 500 700 900 1100) 5900))
                                (scales-out nil))
                           (loop for i from 0 to 2186 
                                 do (let ((diff-scl (reverse (format nil "~3,V,'0,R" 7 i))))
                                      (push
                                       (loop for j in '(1 0 6 2 3 4 5)
                                             collect (+ (nth j scale) (* 100 (parse-integer (string (elt diff-scl j))))))
                                       scales-out)))
                           (reverse scales-out)
                           )
)

(defun harp-scales ()
  "All 2187 harp scales (3^7 pedal combinations) in midicents." *all-harp-scales*)

(defvar *all-accidentals-strings* (let ((accidentals-strings nil))
                                    (loop for i from 0 to 2186 
                                          do (let* ((diff-scl (reverse (format nil "~3,V,'0,R" 7 i))))
                                               (loop for x in '(#\0 #\1 #\2)
                                                     for y in '(#\b #\Space #\#)
                                                     do (nsubstitute y x diff-scl))
                                               (push
                                                (coerce
                                                 (loop for j in '(1 0 6 2 3 4 5)
                                                       collect (elt diff-scl j))
                                                 'string)
                                                accidentals-strings)
                                               )
                                          )
                                    (reverse accidentals-strings)
                                  )
)

(defun accidentals-strings ()
  "All 2187 accidentals strings (b, space, # for the 7 harp pedals)." *all-accidentals-strings*)  

(defvar *all-harp-pedals-settings*  (let* ((scale '("C" "D" "E" "F" "G" "A" "B"))
                                           (scales-out nil))
                                      (loop for i from 0 to 2186 
                                            do (let* ((diff-scl (reverse (format nil "~3,V,'0,R" 7 i))))
                                                 (loop for x in '(#\0 #\1 #\2)
                                                       for y in '(#\b #\Space #\#)
                                                       do (nsubstitute y x diff-scl))
                                                 (push
                                                  (loop for j in '(1 0 6 2 3 4 5)
                                                        collect (string+ (nth j scale) (string (elt diff-scl j))))
                                                  scales-out)))
                                      (reverse scales-out)
                                      )
)

(defun harp-pedals-settings ()
  "All 2187 harp pedal settings." *all-harp-pedals-settings*)

(defvar *all-harp-scales-from-0* (let* ((scale '(0 200 400 500 700 900 1100))
                                        (scales-out nil))
                                   (loop for i from 0 to 2186 
                                         do (let ((diff-scl (reverse (format nil "~3,V,'0,R" 7 i))))
                                              (push
                                               (loop for j in '(1 0 6 2 3 4 5)
                                                     collect (mod (+ (nth j scale) (* 100 (parse-integer (string (elt diff-scl j))))) 1200))
                                               scales-out)
                                              ))
                                   (reverse scales-out)
                                   )
)

(defun harp-scales-from-0 ()
  "All 2187 harp scales from 0 (pitch classes relative to B)." *all-harp-scales-from-0*)

(defmethod! harp-pedals-setting-filter-lnotes ((lnotes list) (pedals-setting list))
  :doc "Keeps only the lnotes whose pitch class belongs to the harp scale of <pedals-setting>."

            :initvals '('(nil) '("D " "C " "B " "E " "F " "G " "A "))
            :icon 9105

     (let* ((harp-scale (harp-pedals-setting-to-harp-scale-from-0 pedals-setting))
            (out-lnotes (flat (remove nil (mapcar #'(lambda (x) (loop for tone in harp-scale
                                                                      if (= (mod (+ (first x) 100) 1200) tone)
                                                                      collect x))
                                                  lnotes)) 1)))

       (setq scale-setting harp-scale g-pedals-setting pedals-setting)

       (done-signal out-lnotes "harp-filter")
     )

)

(defmethod! harp-pedals-setting-filter-chseq ((chseq chord-seq) (pedals-setting list))
  :doc "Keeps only the notes of a chord-seq whose pitch class belongs to the harp scale of <pedals-setting>."

            :initvals '('(nil) '("D " "C " "B " "E " "F " "G " "A "))
            :icon 9105

     (lnotes-to-chseq (harp-pedals-setting-filter-lnotes (chseq-to-lnotes chseq) pedals-setting))

)

(defmethod! harp-filter-lnotes ((in-lnotes list))
  :doc "Chooses the harp pedal setting that keeps the most important notes (velocity x duration) of lnotes and filters the notes with it."

     :initvals '(nil)
     :icon 9105

  (let* ((lnotes (divide-dur-&-reduct-dupl-lnotes in-lnotes))

         (stock nil)

         (lmidic-mod-weight (lnotes-to-lmidicmod+sumveldur+count lnotes))

         (all-scales (loop for scale in *all-harp-scales-from-0*
                           if (and (>= (first scale) (second scale))
                                   (>= (second scale) (third scale))
                                   (>= (fourth scale) (first scale))
                                   (>= (fifth scale) (fourth scale))
                                   (>= (sixth scale) (fifth scale))
                                   (>= (seventh scale) (sixth scale)))
                           collect scale))

         (scale-setting-from-0 (cdr (first (sort
                                              (loop for scale in all-scales
                                                    collect (let ((scl (remove-duplicates scale)))
                                                              (cons (loop for tone in scl
                                                                          sum (second (find tone lmidic-mod-weight :key #'first)))
                                                                    scale)))
                                              #'> :key #'first))))
         )
    (setq pedals-setting (harp-scale-from-0-to-harp-pedals-setting scale-setting-from-0))

    (om-print (format nil "~48:@<harp pedals setting:~>~%~48:@< ~{~A ~^~}~>~%" pedals-setting))
    (harp-pedals-setting-filter-lnotes in-lnotes pedals-setting)
    )

)

(defmethod! harp-filter-chseq ((self chord-seq))
  :doc "Chooses the best harp pedal setting for a chord-seq and filters its notes with it (harp-filter-lnotes)."

     :initvals '(nil)
     :icon 9105

       (lnotes-to-chseq (harp-filter-lnotes (chseq-to-lnotes self)))

)

(defmethod! get-scales-from-0-diffs-l&r-harp-ped-sets ((harp-scale-from-0-1 list) (harp-scale-from-0-2 list))
  :doc "Compares two harp scales from 0 and returns the differences between their pedal settings (number of pedal changes)."

            :initvals '('(200 0 0 400 500 700 900) '(300 0 1100 400 500 800 1100))
            :icon 9105

  (setf harp-scale-from-0-1 (om/ harp-scale-from-0-1 100)
        harp-scale-from-0-2 (om/ harp-scale-from-0-2 100))

  (let* ((primary-scale '(3 1 0 5 6 8 10))
         (diff-scl-1 (mapcar #'(lambda (z) (mod (+ z 1) 12)) (mapcar #'- harp-scale-from-0-1 primary-scale)))
         (diff-scl-2 (mapcar #'(lambda (z) (mod (+ z 1) 12)) (mapcar #'- harp-scale-from-0-2 primary-scale)))
         (diff-harp-ped-set (loop for i from 0 to 6
                                  collect (abs (- (nth i diff-scl-1) (nth i diff-scl-2)))))
         )
    (list (count-if #'plusp (subseq diff-harp-ped-set 0 3)) (count-if #'plusp (subseq diff-harp-ped-set 3 7)))
    )
)

(defvar *vn-range* '(5500 9300))

(defvar *vl-range* '(4800 8100))

(defvar *vc-range* '(3600 6900))

(defvar *cb-range* '(2400 5500))

(defvar *string-orch-ranges* (list *vn-range* *vl-range* *vc-range* *cb-range*))

(defmethod! set-orchestra ((vn-n integer) (vl-n integer) (vc-n integer) (cb-n integer))
  :doc "Instrument ranges for a string orchestra: <vn-n> violins, <vl-n> violas, <vc-n> cellos, <cb-n> double basses. Returns (ranges numbers), e.g. for list-lnotes-by-instruments."

  :initvals '(9 3 2 1)
  :icon 9105

  (list (reverse (remove nil (append (loop repeat vn-n collect *vn-range*)
                                     (loop repeat vl-n collect *vl-range*)
                                     (loop repeat vc-n collect *vc-range*)
                                     (loop repeat cb-n collect *cb-range*))))
        (list cb-n vc-n vl-n vn-n))
)



(defvar *S-range* '(6400 7900))

(defvar *A-range* '(5900 7100))

(defvar *T-range* '(5300 6700))

(defvar *B-range* '(4300 6000))

(defvar *choir-ranges* (list *S-range* *A-range* *T-range* *B-range*))

(defmethod! set-choir ((S-n integer) (A-n integer) (T-n integer) (B-n integer))
  :doc "Voice ranges for a choir: <S-n> sopranos, <A-n> altos, <T-n> tenors, <B-n> basses. Returns (ranges numbers), e.g. for list-lnotes-by-instruments."

  :initvals '(1 1 1 1)
  :icon 9105

  (list (reverse (remove nil (append (loop repeat S-n collect *S-range*)
                                     (loop repeat A-n collect *A-range*)
                                     (loop repeat T-n collect *T-range*)
                                     (loop repeat B-n collect *B-range*))))
        (list B-n T-n A-n S-n))
)
