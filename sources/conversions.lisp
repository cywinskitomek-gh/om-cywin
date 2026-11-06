(in-package :om)

;==================================================

(defmethod! chord-to-lnotes ((self chord))

            :initvals '(nil)
            :icon 9107
            :doc "Converts a chord into lnotes: a list of notes (midic onset dur vel offset chan), all with onset 0."

            (mat-trans
             (list
              (lmidic self)
              (repeat-n 0 (length (lmidic self)))
              (ldur self)
              (lvel self)
              (loffset self)
              (lchan self)
              (repeat-n 0 (length (lmidic self)))
              ))
)

(defmethod! chseq-to-lnotes ((self chord-seq))

            :initvals '(nil)
            :icon 9107
            :doc "Converts a chord-seq into lnotes: a flat list of notes (midic onset dur vel offset chan)."

            (flat (loop for note in (mapcar #'list
                                            (lmidic self)
                                            (lonset self)
                                            (ldur self)
                                            (lvel self)
                                            (loffset self)
                                            (lchan self)
                                            (repeat-n (legato self) (length (lmidic self)))
                                            )
                        collect (mapcar #'list
                                        (first note)
                                        (repeat-n (second note) (length (first note)))
                                        (third note)
                                        (fourth note)
                                        (fifth note)
                                        (sixth note)
                                        (repeat-n (seventh note) (length (first note)))
                                        )
                        ) 1)

)


(defmethod! lnotes-to-chseq ((in-lnotes list))

            :initvals '('((6000 0 125 95 0 1 0)(8400 0 125 95 0 1 0)))
            :icon 9107
            :doc "Converts lnotes (a list of notes (midic onset dur vel offset chan)) into a chord-seq. Notes with the same onset form one chord."

            (let ((lnotes (mat-trans in-lnotes)))

              (make-instance 'chord-seq
                             :lmidic (first lnotes)
                             :lonset (second lnotes)
                             :ldur (third lnotes)
                             :lvel (fourth lnotes)
                             :loffset (fifth lnotes)
                             :lchan (sixth lnotes)
                             :legato (first (seventh lnotes))
                             )

              )

)


(defmethod! mchseq-to-llnotes ((self multi-seq))

            :initvals '(nil)
            :icon 9107
            :doc "Converts a multi-seq into llnotes: a list of lnotes, one per chord-seq."

            (mapcar #'chseq-to-lnotes (chord-seqs self))

)

(defmethod! chseq-list-to-llnotes ((chseq-list list))

            :initvals '(nil)
            :icon 9107
            :doc "Converts a list of chord-seqs into llnotes: a list of lnotes, one per chord-seq."
            
            (mapcar #'chseq-to-lnotes chseq-list)

)


(defmethod! llnotes-to-chseq-list ((llnotes list))

            :initvals '(nil)
            :icon 9107
            :doc "Converts llnotes (a list of lnotes) into a list of chord-seqs."
            
            (mapcar #'(lambda (x) (lnotes-to-chseq x)) llnotes)

)

(defmethod! llnotes-to-mchseq ((llnotes list))

            :initvals '(nil)
            :icon 9107
            :doc "Converts llnotes (a list of lnotes) into a multi-seq, one chord-seq per lnotes."
            
            (make-instance 'multi-seq
                           :chord-seqs (llnotes-to-chseq-list llnotes))

)

;==================================================

(defmethod! harp-scale-to-harp-pedals-setting ((scale list))
  :doc "Converts a harp scale (7 midicents in pedal order D C B E F G A) into pedal settings, e.g. (\"Db\" \"C \" \"B#\" ...)."

            :initvals '('(6200 6000 7100 6400 6500 6700 6900))
            :icon 9107

            (let* ((primary-scale '(6200 6000 7100 6400 6500 6700 6900))
                   (scale (flat scale))
                   (diff-scl (mapcar #'write-to-string (om+ (mapcar #'- scale primary-scale) 100)))
                   (scale-names '("D" "C" "B" "E" "F" "G" "A")))

            (loop for x in '("0" "100" "200")
                  for y in '("b" " " "#")
                  do (nsubstitute y x diff-scl :test #'string-equal))

            (mapcar #'string+ scale-names diff-scl)

            )

)

(defmethod! harp-scale-to-harp-scale-from-0 ((scale list))
  :doc "Reduces a harp scale to pitch classes in midicents relative to B (0..1100)."

            :initvals '('(6200 6000 7100 6400 6500 6700 6900))
            :icon 9107

            (let ((pre-scale-from-0 (om- scale 5900)))
              (mapcar #'(lambda (x) (mod x 1200)) pre-scale-from-0)
            )

)

(defmethod! harp-scale-from-0-to-harp-scale ((scale list))
  :doc "Converts a harp scale from 0 (pitch classes relative to B) back to midicents in the one-line octave."

            :initvals '('(300 100 0 500 600 800 1000))
            :icon 9107

            (om+ scale 5900)

)

(defmethod! harp-scale-from-0-to-harp-pedals-setting ((scale list))
  :doc "Converts a harp scale from 0 (pitch classes relative to B) into pedal settings (D C B E F G A with b, space or #)."

            :initvals '('(300 100 0 500 600 800 1000))
            :icon 9107

            (let* ((primary-scale '(300 100 0 500 600 800 1000))
                   (scale (flat scale))
                   (diff-scl (mapcar #'write-to-string (mapcar #'(lambda (z) (mod (+ z 100) 1200)) (mapcar #'- scale primary-scale))))
                   (scale-names '("D" "C" "B" "E" "F" "G" "A")))

              (loop for x in '("0" "100" "200")
                    for y in '("b" " " "#")
                    do (nsubstitute y x diff-scl :test #'string-equal))

              (mapcar #'string+ scale-names diff-scl)

            )

)

(defmethod! harp-pedals-setting-to-harp-scale ((pedals-setting list))
  :doc "Converts harp pedal settings, e.g. (\"Db\" \"C \" \"B#\" ...), into a harp scale in midicents."

            :initvals '('("D " "C " "B " "E " "F " "G " "A "))
            :icon 9107

            (let* ((primary-scale '(6200 6000 7100 6400 6500 6700 6900))
                   (diff-scl-n (mapcar #'(lambda (x) (string (elt x 1))) (mapcar #'string-capitalize pedals-setting)))
                   )

            (loop for y in '("-100" "0" "100")
                  for x in '("b" " " "#")
                  do (nsubstitute y x diff-scl-n :test #'string-equal))

            (mapcar #'(lambda (x y) (+ x (parse-integer y))) primary-scale diff-scl-n) 

            )

)

(defmethod! harp-pedals-setting-to-harp-scale-from-0 ((pedals-setting list))
  :doc "Converts harp pedal settings into a harp scale from 0 (pitch classes relative to B)."

  :initvals '(nil) 
  :indoc '("midi-object") 
  :icon 9107

            :initvals '('("D " "C " "B " "E " "F " "G " "A "))
            :icon 9107

     (harp-scale-to-harp-scale-from-0 (harp-pedals-setting-to-harp-scale (mapcar #'string-capitalize pedals-setting)))

)

(defmethod! accidentals-string-to-harp-pedals-setting ((accidentals-string string))
  :doc "Converts a 7-character accidentals string (b, space or # for D C B E F G A) into harp pedal settings."

            :initvals '("       ")
            :icon 9107

  (let ((scale-names '("D" "C" "B" "E" "F" "G" "A")))

  (loop for name in scale-names
        for i from 0 to 6
        collect (string+ name (string (elt accidentals-string i))))
  )
) 


(defmethod! accidentals-string-to-harp-scale ((accidentals-string string))
  :doc "Converts a 7-character accidentals string (b, space or # for D C B E F G A) into a harp scale in midicents."

            :initvals '("       ")
            :icon 9107

     (harp-pedals-setting-to-harp-scale (accidentals-string-to-harp-pedals-setting accidentals-string))

)

(defun lnotes-to-lmidicmod+sumveldur+count (lnotes)
  "For lnotes returns, for each of the 12 pitch classes (from B), the sum of velocity x duration and the number of notes. Used to choose harp pedal settings."

     (let* ((lnotes (mat-trans lnotes))
            (lmidic (first lnotes))
            (ldur (third lnotes))
            (lvel (fourth lnotes))
            (lmidic-mod (mapcar #'(lambda (x) (mod (+ x 100) 1200)) lmidic))
            (lmidic-main-scale (arithm-ser 0 1100 100))
            (sumveldur (loop for midic in lmidic-main-scale
                                collect (loop for note-midic in lmidic-mod
                                              for vel in lvel
                                              for dur in ldur
                                              if (= note-midic midic)
                                              sum (/ vel dur))))
            (count (loop for midic in lmidic-main-scale
                         collect (loop for note-midic in lmidic-mod
                                       count (= note-midic midic))))
            )

       (mapcar #'list
               lmidic-main-scale
               sumveldur
               count
               )
       )
)



(defmethod! msec-to-format-dur ((dur number))
  :doc "Formats a duration in milliseconds as a string mm:ss.mmm."

  :initvals '(23456) 
  :indoc '("mseconds") 
  :icon 9107

  (let* ((duration (/ dur 1000))
         (minutes (floor duration 60))
         (seconds (mod duration 60)))
     (format nil "~2,'0D:~6,,,'0:<~,3F~>"  minutes seconds)
     )
)

(defmethod! midi-to-string ((self t))
  :doc "Returns the MIDI events of a MIDI object as a list of strings."

  :initvals '(nil) 
  :indoc '("a MIDI file or sequence or...") 
  :icon 9107

  (mapcar #'evt-to-string (get-midievents self))

)


(defmethod! 16thdur-to-tempo ((note-dur number))
  :doc "Tempo (quarter = MM) for a sixteenth-note duration in ms."

  :initvals '(125) 
  :indoc '("note duration") 
  :icon 9107

  (/ 15000 note-dur)

)

(defmethod! tempo-to-16thdur ((tempo number))
  :doc "Sixteenth-note duration in ms for a tempo (quarter = MM)."

  :initvals '(125) 
  :indoc '("tempo") 
  :icon 9107

  (/ 15000 tempo)

)

#| (defmethod! lnotes-to-lmidievents ((lnotes list))

  :initvals '(nil) 
  :indoc '("lnotes") 
  :icon 170

  (mapcar #'list 

)
|#

(defun midilist-to-lnotes (midi-notes)
  "Converts a midilist (pitch onset dur vel chan) into lnotes (midic onset dur vel offset chan)."

  (loop for midi-note in midi-notes
        collect (list (* (first midi-note) 100)
                      (second midi-note)
                      (third midi-note)
                      (fourth midi-note)
                      0
                      (fifth midi-note)
                      0))
)

(defmethod! midiseq-to-lnotes ((midiseq eventmidi-seq))
  :doc "Converts a MIDI sequence into lnotes (midic onset dur vel offset chan)."

  :initvals '(nil) 
  :indoc '("a MIDI file or sequence") 
  :icon 9107

  (midilist-to-lnotes (evm-seq2midilist midiseq))

)

(defmethod! midi-to-lmidievents ((midi t))
  :doc "Converts the MIDI events of a MIDI object into lists (type date ref port chan fields)."

  :initvals '(nil) 
  :indoc '("midi object") 
  :icon 9107

  (mapcar #'(lambda (x) (list (ev-type x)
                              (ev-date x)
                              (ev-ref x)
                              (ev-port x)
                              (ev-chan x)
                              (ev-fields x)))
          (get-midievents midi))

)



(defmethod! lmidievent-to-midievent ((lmidievent list))
  :doc "Builds a MIDIEVENT from a list (type date ref port chan fields)."

  :initvals '(nil) 
  :indoc '("lmidievent") 
  :icon 9107

  (make-instance 'midievent
                 :ev-type (first lmidievent)
                 :ev-date (second lmidievent)
                 :ev-ref (third lmidievent)
                 :ev-port (fourth lmidievent)
                 :ev-chan (fifth lmidievent)
                 :ev-fields (sixth lmidievent))

)

(defmethod! midievents-to-midiseq ((midievents list))
  :doc "Builds a time-sorted MIDI sequence from a list of MIDI events."

  :initvals '(nil) 
  :indoc '("midievents") 
  :icon 9107

  (temporal-sort (create-midiseq midievents))

)

(defmethod! lmidievents-to-midievents ((lmidievents list))
  :doc "Converts a list of lmidievents (type date ref port chan fields) into MIDI events."

  :initvals '(nil) 
  :indoc '("lmidievents") 
  :icon 9107

  (mapcar #'lmidievent-to-midievent lmidievents)

)

(defmethod! lmidievents-to-midiseq ((lmidievents list))
  :doc "Builds a MIDI sequence from a list of lmidievents (type date ref port chan fields)."

  :initvals '(nil) 
  :indoc '("midiseq") 
  :icon 9107

  (midievents-to-midiseq (lmidievents-to-midievents lmidievents))

)

(defmethod! midievents-to-midifile ((midievents list) (fname string) (dir list))
  :doc "Saves a list of MIDI events to a MIDI file <file name> in the subdirectories <directories>."

  :initvals '(nil "test.mid" '("midi" "test")) 
  :indoc '("midievents" "file name" "directories") 
  :icon 9107

  (midiseq-to-midifile (midievents-to-midiseq midievents) fname dir)

)

(defmethod! midievents-to-midifile ((midievents list) (fname string) (dir pathname))
  :icon 9107
  :doc "Saves a list of MIDI events to a MIDI file <file name> in the directory <dir> (a pathname)."

  (midiseq-to-midifile (midievents-to-midiseq midievents) fname dir)

)

(defmethod! midievents-to-midifile ((midievents list) (fname string) (dir string))
  :icon 9107
  :doc "Saves a list of MIDI events to a MIDI file <file name> in the directory <dir> (a string)."

  (midiseq-to-midifile (midievents-to-midiseq midievents) fname (list dir))

)

(defmethod! midievents-to-lnotes ((midievents list))
  :doc "Converts a list of MIDI events into lnotes (midic onset dur vel offset chan)."

  :initvals '(nil) 
  :indoc '("midievents") 
  :icon 9107

  (midiseq-to-lnotes (midievents-to-midiseq midievents))

)

(defmethod! midievents-to-chseq ((midievents list))
  :doc "Converts a list of MIDI events into a chord-seq."

  :initvals '(nil) 
  :indoc '("midievents") 
  :icon 9107

  (lnotes-to-chseq (midiseq-to-lnotes (midievents-to-midiseq midievents)))

)


(defmethod! midi-to-lnotes ((midi t))
  :doc "Converts the note-on/note-off events of a MIDI object into lnotes (midic onset dur vel offset chan)."

  :initvals '(nil) 
  :indoc '("midi-object") 
  :icon 9107

  (midievents-to-lnotes (get-keyon-keyoff-midievents midi))

)


(defmethod! lnotes-to-midievents ((lnotes list))
  :doc "Converts lnotes into a list of MIDI events."

  :initvals '(nil)
  :indoc '("lnotes") 
  :icon 9107

  (get-midievents (lnotes-to-chseq lnotes))

)

(defun velocity-to-dynamics-markings (vel)
  "Dynamic marking (ppp ... ffff) for a MIDI velocity."

  (cond
   ((>= vel 115) "ffff")
   ((>= vel 102) "fff")
   ((>= vel 89) "ff")
   ((>= vel 77) "f")
   ((>= vel 64) "mf")
   ((>= vel 51) "mp")
   ((>= vel 39) "p")
   ((>= vel 26) "pp")
   ((>= vel 13) "ppp")
   (t "pppp") 
   )

)

(defun vel-to-dyn-mark (vel)
  "Dynamic marking (ppp ... ffff) for a MIDI velocity (short name of velocity-to-dynamics-markings)."

  (velocity-to-dynamics-markings vel)

)


(defun midievents-velocity-to-volume (midevts)
  "Adds volume (CC 7) events to MIDI events, derived from the note velocities on a dB scale." 

  (let* ((set-vol-midevts (get-midievents-type (copy-seq midevts) :keyon))
         (velocities (get-velocities-from-midi midevts))
         (min-vel-in (list-min velocities))
         (max-vel-in (list-max velocities))
         (lindb (lin->db (om-scale velocities 0.0 1.0 0 127)))
         (db-min (list-min lindb))
         (vol-list (om-round (om-scale lindb min-vel-in max-vel-in db-min 0.0)))
         )

    (loop for midevt in set-vol-midevts
          for vol in vol-list
          do (setf (ev-type midevt) :ctrlchange 
                   (ev-fields midevt) (list 7 vol)))

    (append (midievents-fixed-velocity midevts 100) set-vol-midevts)
    )
)

(defmethod! seq-to-midifile ((seq named-score-object) (fname string) (dir list))
  :doc "Saves a score object (seq) to a MIDI file <file name> in the subdirectories <directories>."

  :initvals '(nil "test.mid" '("midi" "test")) 
  :indoc '("seq" "file name" "directories") 
  :icon 9107

  (midievents-to-midifile (get-midievents seq) fname dir)

)

(defmethod! seq-to-midifile ((seq named-score-object) (fname string) (dir pathname))
  :icon 9107
  :doc "Saves a score object to a MIDI file <file name> in the directory <dir> (a pathname)."

  (midievents-to-midifile (get-midievents seq) fname dir)

)

(defmethod! seq-to-midifile ((seq tonal-object) (fname string) (dir list))
  :icon 9107
  :doc "Saves a score object to a MIDI file <file name> in the subdirectories <dir>."

  (midievents-to-midifile (get-midievents seq) fname dir)

)

(defmethod! seq-to-midifile ((seq tonal-object) (fname string) (dir pathname))
  :icon 9107
  :doc "Saves a score object to a MIDI file <file name> in the directory <dir> (a pathname)."

  (midievents-to-midifile (get-midievents seq) fname dir)

)

(defmethod! seq-to-midifile ((seq eventmidi-seq) (fname string) (dir list))
  :icon 9107
  :doc "Saves a MIDI sequence to a MIDI file <file name> in the subdirectories <dir>."

  (save-as-midi seq (outfile fname :subdirs dir))

)

(defmethod! seq-to-midifile ((seq eventmidi-seq) (fname string) (dir pathname))
  :icon 9107
  :doc "Saves a MIDI sequence to a MIDI file <file name> in the directory <dir> (a pathname)."

  (save-as-midi seq (merge-pathnames fname dir))

)

(defmethod! midiseq-to-midifile ((midiseq eventmidi-seq) (fname string) (dir list))
  :doc "Saves a MIDI sequence to a MIDI file <file name> in the subdirectories <directories>."

  :initvals '(nil "test.mid" '("midi" "test")) 
  :indoc '("midiseq" "file name" "directories") 
  :icon 9107

  (seq-to-midifile midiseq fname dir)

)

(defmethod! midiseq-to-midifile ((midiseq eventmidi-seq) (fname string) (dir pathname))
  :icon 9107
  :doc "Saves a MIDI sequence to a MIDI file <file name> in the directory <dir> (a pathname)."

  (seq-to-midifile midiseq fname dir)

)

(defmethod! score-object-to-midifile ((score-object chord-seq) (fname string) (dir t))
  :doc "Saves a score object (chord-seq, multi-seq, voice, poly, chord or note) to a MIDI file <file name> in <directories>."

  :initvals '(nil "test.mid" '("midi" "test")) 
  :indoc '("score-object" "file name" "directories") 
  :icon 9107

  (seq-to-midifile score-object fname dir) 

)

(defmethod! score-object-to-midifile ((score-object multi-seq) (fname string) (dir list))
  :icon 9107
  :doc "Saves a multi-seq to a MIDI file <file name> in the subdirectories <dir>."

  (seq-to-midifile score-object fname dir)

)

(defmethod! score-object-to-midifile ((score-object voice) (fname string) (dir list))
  :icon 9107
  :doc "Saves a voice to a MIDI file <file name> in the subdirectories <dir>."

  (seq-to-midifile score-object fname dir)

)

(defmethod! score-object-to-midifile ((score-object poly) (fname string) (dir list))
  :icon 9107
  :doc "Saves a poly to a MIDI file <file name> in the subdirectories <dir>."

  (seq-to-midifile score-object fname dir)

)

(defmethod! score-object-to-midifile ((score-object chord) (fname string) (dir list))
  :icon 9107
  :doc "Saves a chord to a MIDI file <file name> in the subdirectories <dir>."

  (seq-to-midifile score-object fname dir)

)

(defmethod! score-object-to-midifile ((score-object note) (fname string) (dir list))
  :icon 9107
  :doc "Saves a note to a MIDI file <file name> in the subdirectories <dir>."

  (seq-to-midifile score-object fname dir)

)

(defmethod! voice-to-midifile ((voice voice) (fname string) (dir list))
  :doc "Saves a voice to a MIDI file <file name> in the subdirectories <directories>."

  :initvals '(nil "test.mid" '("midi" "test")) 
  :indoc '("voice" "file name" "directories") 
  :icon 9107

  (score-object-to-midifile voice fname dir)

)

(defmethod! poly-to-midifile ((poly poly) (fname string) (dir list))
  :doc "Saves a poly to a MIDI file <file name> in the subdirectories <directories>."

  :initvals '(nil "test.mid" '("midi" "test")) 
  :indoc '("poly" "file name" "directories") 
  :icon 9107

  (score-object-to-midifile poly fname dir)

)

(defmethod! chord-to-midifile ((chord chord) (fname string) (dir list))
  :doc "Saves a chord to a MIDI file <file name> in the subdirectories <directories>."

  :initvals '(nil "test.mid" '("midi" "test")) 
  :indoc '("chord" "file name" "directories") 
  :icon 9107

  (score-object-to-midifile chord fname dir)

)

(defmethod! note-to-midifile ((note note) (fname string) (dir list))
  :doc "Saves a note to a MIDI file <file name> in the subdirectories <directories>."

  :initvals '(nil "test.mid" '("midi" "test")) 
  :indoc '("note" "file name" "directories") 
  :icon 9107

  (score-object-to-midifile note fname dir)

)

(defmethod! chseq-to-midifile ((chseq chord-seq) (fname string) (dir list))
  :doc "Saves a chord-seq to a MIDI file <file name> in the subdirectories <directories>."

  :initvals '(nil "test.mid" '("midi" "test")) 
  :indoc '("chseq" "file name" "directories") 
  :icon 9107

  (score-object-to-midifile chseq fname dir)

)

(defmethod! mchseq-to-midifile ((mchseq multi-seq) (fname string) (dir list))
  :doc "Saves a multi-seq to a MIDI file <file name> in the subdirectories <directories>."

  :initvals '(nil "test.mid" '("midi" "test")) 
  :indoc '("mchseq" "file name" "directories") 
  :icon 9107

  (score-object-to-midifile mchseq fname dir)

)

(defun f->n (f)
  "Note name for a frequency in Hz."

  (mc->n (f->mc f))

)



#|-------MATRIX--------|#



(defmethod! tension-to-lmidic ((tension number) (toler number) (n-harm number) (range list))
  :doc "Matrix chord (midicents) for a tension: the harmonic field of that tension in the matrix (set-matrix-lmidic with <toler>, <n-harm>, <range>)."

  :initvals '(100 2 3 '(2100 10800))
  :indoc '("tension" "toler" "n-harmonic" "range mc")
  :icon 9107

  (copy-list (nth tension (spiral-matrix toler n-harm range)))

)

(defmethod! tensions-to-llmidic ((tensions list) (toler number) (n-harm number) (range list))
  :doc "Matrix chords for a list of tensions (tension-to-lmidic for each)."

  :initvals '('(100 102 107) 2 3 '(2100 10800))
  :indoc '("tensions" "toler" "n-harmonic" "range mc")
  :icon 9107

  (mapcar #'(lambda (x) (tension-to-lmidic x toler n-harm range)) tensions)

)

(defmethod! lmidic-to-tension ((lmidic list) (toler number) (n-harm number) (range list))
  :doc "Tension of a chord: the lowest matrix index whose chord contains all notes of <lmidic>. NIL when the chord does not fit the matrix (outside the range, microtones, floats)."

  :initvals '('(6000 6200 6700) 2 3 '(2100 10800))
  :indoc '("lmidic" "toler" "n-harmonic" "range mc")
  :icon 9107

  (let ((matrix (spiral-matrix toler n-harm range)))

    (loop for i from 0
          for m in matrix
          if (subsetp lmidic m)
            return i)
    )
)


(defmethod! lmidic-to-all-tensions ((lmidic list) (toler number) (n-harm number) (range list))
  :doc "All matrix indices (tensions) whose chord contains all notes of <lmidic>."

  :initvals '('(6000 6200 6700) 2 3 '(2100 10800))
  :indoc '("lmidic" "toler" "n-harmonic" "range mc")
  :icon 9107

  (let ((matrix (spiral-matrix toler n-harm range)))

  (loop for i from 0
        for m in matrix
        if (subsetp lmidic m)
        collect i)

  )

)

(defmethod! llmidic-to-tensions ((llmidic list) (toler number) (n-harm number) (range list))
  :doc "Tensions of a list of chords (lmidic-to-tension for each)."

  :initvals '('((6000 6200 6700) (6200 6400 6900) (6400 6600 7100)) 2 3 '(2100 10800))
  :indoc '("lmidic list" "toler" "n-harmonic" "range mc")
  :icon 9107

  (mapcar #'(lambda (x) (lmidic-to-tension x toler n-harm range)) llmidic)

)


(defmethod! llmidic-to-all-tensions ((llmidic list) (toler number) (n-harm number) (range list))
  :doc "All tensions of each chord in a list (lmidic-to-all-tensions for each)."

  :initvals '('((6000 6200 6700) (6200 6400 6900) (6400 6600 7100)) 2 3 '(2100 10800))
  :indoc '("lmidic list" "toler" "n-harmonic" "range mc")
  :icon 9107

  (mapcar #'(lambda (x) (lmidic-to-all-tensions x toler n-harm range)) llmidic)

)


(defmethod! lmidic-to-more-lmidic ((lmidic list) (toler number) (n-harm number) (range list))
  :doc "The full matrix chord of the tension of <lmidic>: the chord completed to its whole harmonic field."

  :initvals '('(6000 6200 6700) 2 3 '(2100 10800))
  :indoc '("lmidic" "toler" "n-harmonic" "range mc")
  :icon 9107

  (tension-to-lmidic (lmidic-to-tension lmidic toler n-harm range) toler n-harm range)

)


(defmethod! llmidic-to-more-llmidic ((llmidic list) (toler number) (n-harm number) (range list))
  :doc "The full matrix chords of the tensions of a list of chords (lmidic-to-more-lmidic for each)."

  :initvals '('((6000 6200 6700) (6200 6400 6900) (6400 6600 7100)) 2 3 '(2100 10800))
  :indoc '("lmidic list" "toler" "n-harmonic" "range mc")
  :icon 9107

  (mapcar #'(lambda (x) (lmidic-to-more-lmidic x toler n-harm range)) llmidic)

)

;==============================================;
; TENSION-KEY-SETS (added 2026-10)
;==============================================;


(defun %lmidic-lex< (a b)   ; ukryj
  (loop for x in a
        for y in b
        do (cond ((< x y) (return t))
                 ((> x y) (return nil)))
        finally (return (< (length a) (length b)))))

(defmethod! tension-key-sets ((tension number) (group-range list)
                              (toler number) (n-harm number) (range list)
                              &optional (max-size 24))

  :initvals '(144 '(5500 9300) 2 3 '(2100 10800) 24)
  :indoc '("tension" "group range mc" "toler" "n-harmonics" "range mc" "max notes")
  :icon 9107
  :doc "Returns all smallest sets of key notes (lmidic) taken from the matrix chord
of the given tension and limited to group-range, that keep exactly this tension.
NIL if the group cannot reach this tension within its range."

  (let* ((tension (round tension))
         (matrix (spiral-matrix toler n-harm range))
         (g-min (list-min group-range))
         (g-max (list-max group-range)))

    (if (or (< tension 0) (>= tension (length matrix)))

        (progn
          (om-print (format nil "tension-key-sets: tension ~D outside matrix (0-~D)"
                            tension (1- (length matrix))))
          nil)

        (let* ((mt (sort (remove-if-not #'(lambda (x) (<= g-min x g-max))
                                        (copy-list (nth tension matrix)))
                         #'<))
               (mtv (coerce mt 'vector))
               (n (length mt))
               (ds nil)
               (unreachable (null mt)))

          ;; dla kazdej nizszej pozycji matrycy: maska dzwiekow akordu,
          ;; ktorych tam NIE ma (choc jeden z nich musi zostac zachowany)
          (unless unreachable
            (loop for i from 0 below tension
                  for m in matrix
                  do (let ((d 0))
                       (loop for x in mt
                             for j from 0
                             unless (member x m :test #'=)
                               do (setf d (logior d (ash 1 j))))
                       (when (zerop d)
                         (setf unreachable t)
                         (return))
                       (pushnew d ds))))

          (cond

            (unreachable
             (om-print (format nil "tension-key-sets: tension ~D unreachable in range ~D-~D"
                               tension g-min g-max))
             nil)

            ;; tension 0: brak warunkow, kazdy dzwiek osobno
            ((null ds) (mapcar #'list mt))

            (t
             ;; usun warunki nadmiarowe (nadzbiory innych warunkow)
             (let ((red nil))
               (dolist (d (sort ds #'< :key #'logcount))
                 (unless (some #'(lambda (e) (= (logand e d) e)) red)
                   (push d red)))

               (or
                (loop for k from 1 to (min max-size n)
                      do (let ((res nil))
                           (labels ((srch (chosen mask depth)
                                      (let ((unhit (remove-if #'(lambda (d) (logtest d mask)) red)))
                                        (cond
                                          ((null unhit)
                                           (pushnew (sort (copy-list chosen) #'<) res :test #'equal))
                                          ((< depth k)
                                           ;; rozgaleziaj po najmniejszym niespelnionym warunku
                                           (let ((d (reduce #'(lambda (a b) (if (<= (logcount a) (logcount b)) a b))
                                                            unhit)))
                                             (loop for j from 0 below n
                                                   when (logbitp j d)
                                                     do (srch (cons (aref mtv j) chosen)
                                                              (logior mask (ash 1 j))
                                                              (1+ depth)))))))))
                             (srch nil 0 0))
                           (when res
                             (om-print (format nil "tension-key-sets: ~D set~:P of ~D note~:P"
                                               (length res) k))
                             (return (sort res #'%lmidic-lex<)))))
                (progn
                  (om-print (format nil "tension-key-sets: no set within ~D notes" max-size))
                  nil)))))))))


;==============================================;
; TENSION-KEY-SETS+ / FILTER-KEY-SETS /
; RANK-KEY-SETS (added 2026-10)
;==============================================;


(defvar *key-sets-max-results* 5000
  "Upper limit of sets returned by tension-key-sets+ (protection against
combinatorial explosion when extra > 0).")


(defun %key-sets-list (x)   ; ukryj
  (cond ((null x) nil)
        ((listp x) x)
        (t (list x))))


(defun %key-sets-size< (a b)   ; ukryj
  (if (= (length a) (length b))
      (%lmidic-lex< a b)
      (< (length a) (length b))))


(defun %key-sets-hit-p (conds mask budget)   ; ukryj
  ;; czy da sie trafic wszystkie warunki (maski bitowe) dobierajac
  ;; co najwyzej budget dodatkowych dzwiekow
  (let ((unhit (remove-if #'(lambda (d) (logtest d mask)) conds)))
    (cond ((null unhit) t)
          ((<= budget 0) nil)
          (t (let ((d (reduce #'(lambda (a b) (if (<= (logcount a) (logcount b)) a b)) unhit)))
               (loop for j from 0 below (integer-length d)
                     thereis (and (logbitp j d)
                                  (%key-sets-hit-p conds (logior mask (ash 1 j)) (1- budget)))))))))


(defmethod! tension-key-sets+ ((tension number) (group-range list)
                               (toler number) (n-harm number) (range list)
                               &optional (required nil) (forbidden nil) (extra 0) (max-size 24))

  :initvals '(144 '(5500 9300) 2 3 '(2100 10800) nil nil 0 24)
  :indoc '("tension" "group range mc" "toler" "n-harmonics" "range mc"
           "required notes mc" "forbidden notes mc" "extra notes" "max notes")
  :icon 9107
  :doc "Extended version of tension-key-sets.
Returns sets of key notes (lmidic) taken from the matrix chord of the given
tension and limited to group-range, that keep exactly this tension.

required  - notes (mc) that every set must contain
forbidden - notes (mc) that no set may contain
extra     - 0 = only the smallest sets (as tension-key-sets),
            1 = also sets with one note more, etc.
max-size  - maximum number of notes in a set

Result is sorted by size, then from the lowest notes.
With default optional inputs the result equals tension-key-sets.
NIL (with a message in the Listener) if no set exists."

  (block tks
    (let* ((tension (round tension))
           (matrix (spiral-matrix toler n-harm range))
           (g-min (list-min group-range))
           (g-max (list-max group-range))
           (required (remove-duplicates (%key-sets-list required) :test #'=))
           (forbidden (%key-sets-list forbidden))
           (extra (max 0 (round (or extra 0))))
           (max-size (round (or max-size 24))))

      (when (or (< tension 0) (>= tension (length matrix)))
        (om-print (format nil "tension-key-sets+: tension ~D outside matrix (0-~D)"
                          tension (1- (length matrix))))
        (return-from tks nil))

      (let ((clash (intersection required forbidden :test #'=)))
        (when clash
          (om-print (format nil "tension-key-sets+: ~A both required and forbidden" clash))
          (return-from tks nil)))

      (let* ((mt (sort (remove-if-not #'(lambda (x) (and (<= g-min x g-max)
                                                          (not (member x forbidden :test #'=))))
                                      (copy-list (nth tension matrix)))
                       #'<))
             (mtv (coerce mt 'vector))
             (n (length mt))
             (rmask 0)
             (ds nil))

        ;; dzwieki obowiazkowe musza byc w akordzie, w zakresie i nie zakazane
        (dolist (r required)
          (let ((j (position r mt :test #'=)))
            (unless j
              (om-print (format nil "tension-key-sets+: required note ~D not available (not in chord of tension ~D, outside group range or forbidden)"
                                r tension))
              (return-from tks nil))
            (setf rmask (logior rmask (ash 1 j)))))

        (when (null mt)
          (om-print (format nil "tension-key-sets+: tension ~D unreachable in range ~D-~D"
                            tension g-min g-max))
          (return-from tks nil))

        ;; warunki: dla kazdej nizszej pozycji matrycy maska dzwiekow, ktorych tam nie ma
        (loop for i from 0 below tension
              for m in matrix
              do (let ((d 0))
                   (loop for x in mt
                         for j from 0
                         unless (member x m :test #'=)
                           do (setf d (logior d (ash 1 j))))
                   (when (zerop d)
                     (om-print (format nil "tension-key-sets+: tension ~D unreachable in range ~D-~D~:[~; without forbidden notes~]"
                                       tension g-min g-max forbidden))
                     (return-from tks nil))
                   (pushnew d ds)))

        (let* ((red (let ((r nil))
                      (dolist (d (sort ds #'< :key #'logcount) r)
                        (unless (some #'(lambda (e) (= (logand e d) e)) r)
                          (push d r)))))
               ;; warunki jeszcze nie spelnione przez dzwieki obowiazkowe
               (open (remove-if #'(lambda (d) (logtest d rmask)) red))
               (nr (logcount rmask))
               (kmin (loop for k from (max 1 nr) to (min max-size n)
                           when (%key-sets-hit-p open rmask (- k nr))
                             return k)))

          (unless kmin
            (om-print (format nil "tension-key-sets+: no set within ~D notes" max-size))
            (return-from tks nil))

          (let* ((kmax (min (+ kmin extra) n max-size))
                 (free (coerce (loop for j from 0 below n
                                     unless (logbitp j rmask) collect j)
                               'vector))
                 (nf (length free))
                 (res nil)
                 (count 0)
                 (cut nil))

            (block walk-all
              (loop for k from kmin to kmax
                    do (labels ((walk (p left mask)
                                  (let ((unhit (remove-if #'(lambda (d) (logtest d mask)) open)))
                                    (cond
                                      ((zerop left)
                                       (when (null unhit)
                                         (push (loop for j from 0 below n
                                                     when (logbitp j mask) collect (aref mtv j))
                                               res)
                                         (when (>= (incf count) *key-sets-max-results*)
                                           (setf cut t)
                                           (return-from walk-all))))
                                      ;; kazdy niespelniony warunek musi miec jeszcze dostepny dzwiek
                                      ((and (< p nf)
                                            (every #'(lambda (d) (plusp (ash d (- (aref free p))))) unhit))
                                       (loop for q from p to (- nf left)
                                             do (walk (1+ q) (1- left)
                                                      (logior mask (ash 1 (aref free q))))))))))
                         (walk 0 (- k nr) rmask))))

            (when cut
              (om-print (format nil "tension-key-sets+: result cut at ~D sets" *key-sets-max-results*)))
            (om-print (format nil "tension-key-sets+: ~D set~:P of ~D~:[-~D~;~*~] note~:P"
                              count kmin (= kmin kmax) kmax))
            (sort res #'%key-sets-size<)))))))


;----------------------------------------------;
; prowadzenie glosow
;----------------------------------------------;
;; Dopasowanie dwoch akordow bez krzyzowania glosow (na posortowanych
;; wysokosciach). Przy rownej liczbie dzwiekow: glos do glosu, od dolu.
;; Przy roznej: kazdy dzwiek wiekszego akordu ma dokladnie jednego partnera
;; w mniejszym, dzwieki mniejszego moga sie rozdzielac (lub laczyc).
;; Zwraca (values laczny-ruch najwiekszy-skok) albo NIL, gdy przy max-leap
;; takie dopasowanie nie istnieje.

(defun %voice-leading (a b &optional max-leap)   ; ukryj
  (let* ((a (coerce (sort (copy-list a) #'<) 'vector))
         (b (coerce (sort (copy-list b) #'<) 'vector)))
    (when (> (length a) (length b)) (rotatef a b))   ; a = mniejszy akord
    (let ((m (length a))
          (n (length b)))
      (when (and (plusp m) (plusp n))
        (let ((dc (make-array (list m n) :initial-element nil))
              (dx (make-array (list m n) :initial-element nil)))
          (dotimes (j n)
            (dotimes (i m)
              (let ((c (abs (- (aref a i) (aref b j)))))
                (unless (and max-leap (> c max-leap))
                  (let ((best nil) (bx 0))
                    (if (= j 0)
                        (when (= i 0) (setf best 0))
                        ;; b[j] laczy sie z tym samym a[i] co b[j-1] albo z nastepnym
                        (dolist (pa (list i (1- i)))
                          (when (and (>= pa 0) (aref dc pa (1- j)))
                            (let ((pc (aref dc pa (1- j))) (px (aref dx pa (1- j))))
                              (when (or (null best) (< pc best) (and (= pc best) (< px bx)))
                                (setf best pc bx px))))))
                    (when best
                      (setf (aref dc i j) (+ best c)
                            (aref dx i j) (max bx c))))))))
          (when (aref dc (1- m) (1- n))
            (values (aref dc (1- m) (1- n)) (aref dx (1- m) (1- n)))))))))


(defun %key-sets-lists (sets)   ; ukryj
  ;; pozwala podac pojedynczy akord zamiast listy akordow
  (if (and sets (numberp (car sets))) (list sets) sets))


(defmethod! filter-key-sets ((sets list) &optional (range nil) (max-span nil) (prev nil) (max-leap nil))

  :initvals '('((6000 6400 6700) (5500 6400 7600)) nil nil nil nil)
  :indoc '("lmidic list" "range mc" "max span mc" "previous chord mc" "max leap mc")
  :icon 9107
  :doc "Keeps only the sets (lmidic lists) that are playable for one part:

range    - (min max) all notes inside this range (instrument or chosen part of the spectrum)
max-span - maximum distance between lowest and highest note (mc)
prev     - previous chord (mc), needed for max-leap
max-leap - maximum move of a single voice from prev (mc);
           voices are connected without crossing, from the bottom; when the
           number of notes differs, voices of the smaller chord may split/merge

Empty inputs are ignored. Works also on results of tension-to-lmidic etc."

  (let ((sets (%key-sets-lists sets))
        (r-min (and range (list-min range)))
        (r-max (and range (list-max range))))
    (when (and max-leap (null prev))
      (om-print "filter-key-sets: max-leap ignored (no previous chord)"))
    (remove-if-not
     #'(lambda (s)
         (and s
              (or (null range) (every #'(lambda (x) (<= r-min x r-max)) s))
              (or (null max-span) (<= (- (reduce #'max s) (reduce #'min s)) max-span))
              (or (null prev) (null max-leap) (%voice-leading prev s max-leap))))
     sets)))


(defmethod! rank-key-sets ((sets list) &optional (prev nil) (criterion 'voice-leading) (center nil))

  :initvals '('((6000 6400 6700) (5500 6400 7600)) nil 'voice-leading nil)
  :indoc '("lmidic list" "previous chord mc" "criterion" "register center mc")
  :menuins '((2 (("voice-leading" 'voice-leading) ("span" 'span) ("center" 'center))))
  :numouts 2
  :icon 9107
  :doc "Sorts the sets (lmidic lists) from the best one.

criterion:
voice-leading - smallest total voice movement from prev (then span)
span          - smallest distance between lowest and highest note (then voice-leading)
center        - mean pitch closest to center (then voice-leading, span);
                without center the mean pitch of prev is used

Voices are connected without crossing, from the bottom; when the number
of notes differs, voices of the smaller chord may split/merge.

1st output: sorted sets
2nd output: for each set (total-movement biggest-leap span distance-to-center)
            total-movement and biggest-leap are NIL without prev."

  (let* ((sets (%key-sets-lists sets))
         (crit (string-upcase (string (if (consp criterion) (car (last criterion)) criterion))))
         (center (or center
                     (and prev (/ (reduce #'+ prev) (length prev)))))
         (scored
          (mapcar
           #'(lambda (s)
               (multiple-value-bind (vl leap) (if prev (%voice-leading prev s) (values nil nil))
                 (list s
                       (list vl leap
                             (- (reduce #'max s) (reduce #'min s))
                             (and center (round (abs (- (/ (reduce #'+ s) (length s)) center))))))))
           (remove nil sets)))
         (key-fn
          (cond ((string= crit "SPAN")
                 #'(lambda (sc) (list (third sc) (first sc) (second sc))))
                ((string= crit "CENTER")
                 #'(lambda (sc) (list (fourth sc) (first sc) (third sc))))
                (t
                 #'(lambda (sc) (list (first sc) (second sc) (third sc)))))))

    (when (and (string= crit "VOICE-LEADING") (null prev))
      (om-print "rank-key-sets: no previous chord, sorted by span"))
    (when (and (string= crit "CENTER") (null center))
      (om-print "rank-key-sets: no center and no previous chord, sorted by span"))

    (labels ((key< (ka kb)
               ;; NIL w kluczu = brak danych, traktowane jako rownosc
               (loop for x in ka
                     for y in kb
                     do (when (and x y (/= x y))
                          (return (< x y)))
                     finally (return :tie))))
      (let ((sorted (stable-sort (copy-list scored)
                                 #'(lambda (a b)
                                     (let ((r (key< (funcall key-fn (second a))
                                                    (funcall key-fn (second b)))))
                                       (if (eq r :tie)
                                           (%key-sets-size< (first a) (first b))
                                           r))))))
        (values (mapcar #'first sorted)
                (mapcar #'second sorted))))))

;==================================================
; sonogram->chord-seq: wyjscia GOERTZEL-SONOGRAM -> CHORD-SEQ
; Velocity liniowo w decybelach: najglosniejsza skladowa calego dzwieku = 127,
; <range-db> dB ponizej niej = 0 (np. przy 40 dB: -10 dB -> 95, -20 dB -> 64).

(defun sonogram-vel (a amax range-db)   ; ukryj
  "Velocity 0..127 dla amplitudy A wzgledem AMAX."
  (if (or (not (realp a)) (<= a 0) (<= amax 0))
      0
      (let ((db (* 20 (log (/ a amax) 10))))
        (max 0 (min 127 (round (* 127 (+ 1 (/ db range-db)))))))))

(defun sonogram-notes (grid midics times range-db min-vel)   ; ukryj
  "Chords, onsets (ms) and velocities; skips notes below MIN-VEL and empty windows."
  (let ((amax (reduce #'max (mapcar #'(lambda (fr) (reduce #'max fr :initial-value 0)) grid)
                      :initial-value 0))
        chords onsets vels)
    (loop for frame in grid
          for tm in times
          do (let (ms vs)
               (loop for a in frame
                     for m in midics
                     for v = (sonogram-vel a amax range-db)
                     when (>= v (max 1 min-vel))
                       do (push m ms) (push v vs))
               (when ms
                 (push (nreverse ms) chords)
                 (push (round tm) onsets)
                 (push (nreverse vs) vels))))
    (values (nreverse chords) (nreverse onsets) (nreverse vels))))

(defmethod! sonogram->chord-seq ((grid list) (midics list) (times list)
                                 &key (range-db 40) (min-vel 1) (dur nil) (chan 1))
  :icon 9107
  :initvals '(nil nil nil)
  :indoc '("amplitude windows (output 1 of goertzel-sonogram)"
           "midicents (output 2 of goertzel-sonogram)"
           "window onsets in ms (output 3 of goertzel-sonogram)")
  :doc "Turns the outputs of GOERTZEL-SONOGRAM into a CHORD-SEQ: each window is one chord.
Velocity is computed in decibels: the loudest component in the whole sound = 127,
<range-db> dB below it = 0 (default 40; at 40 dB: -10 dB -> 95, -20 dB -> 64, -30 dB -> 32).
<min-vel> notes with a lower velocity are skipped (default 1, i.e. only zeros).
<dur> note length in ms (nil = the distance between windows), <chan> MIDI channel."
  (multiple-value-bind (chords onsets vels)
      (sonogram-notes grid midics times range-db min-vel)
    (let ((d (or dur (if (cdr times) (round (- (second times) (first times))) 1000))))
      (make-instance 'chord-seq
                     :lmidic chords
                     :lonset onsets
                     :ldur (mapcar #'(lambda (c) (make-list (length c) :initial-element d)) chords)
                     :lvel vels
                     :lchan (mapcar #'(lambda (c) (make-list (length c) :initial-element chan)) chords)))))

(defun sonogram->chord-seq-test ()   ; ukryj
  "Two windows, amplitudes relative to the maximum 1.0: 0 dB -> 127, -10 dB -> 95, -40 dB and 0 -> skipped."
  (multiple-value-bind (chords onsets vels)
      (sonogram-notes '((1.0 0.31622777 0) (0 1.0 0.01)) '(6000 6100 6200) '(0.0 250.0) 40 1)
    (let ((ok (and (equal chords '((6000 6100) (6100)))
                   (equal onsets '(0 250))
                   (equal vels '((127 95) (127)))
                   (= (sonogram-vel 0.1 1.0 50) 76))))
      (om-print (format nil "sonogram->chord-seq-test: ~a" (if ok "OK" "BLAD")))
      ok)))

;===========================================
; SOUND->TENSIONS: dzwiek -> akordy z tension
; goertzel-sonogram -> akordy jak w sonogram->chord-seq -> tension jak w lmidic-to-tension
; (matryca liczona raz i pamietana - spiral-matrix).

(defun sound-chord-tension (chord matrix)   ; ukryj
  "The smallest tension (matrix index) whose set contains all notes of CHORD; NIL when none."
  (loop for m in matrix
        for i from 0
        when (subsetp chord m :test #'=)
          return i))

(defmethod! sound->tensions ((snd t) (win-ms number) (range list)
                             &key (step 100) (channel nil) (range-db 40) (min-vel 1) (dur nil)
                               (toler 2) (n-harm 3) (matrix-range '(2100 10800)))
  :icon 9107
  :initvals '(nil 125 (2100 10800))
  :indoc '("sound (or om-sound-data)" "window length in ms" "midicent range (from to)")
  :outdoc '("list of (lmidic onset ldur lvel tension)")
  :doc "Turns a sound into chords with tension.
Combines GOERTZEL-SONOGRAM (<win-ms>, <range>, <step>, <channel>: nil = mean of the channels), SONOGRAM->CHORD-SEQ (<range-db>, <min-vel>, <dur>)
and LMIDIC-TO-TENSION (<toler>, <n-harm>, <matrix-range>).
Returns a list of lists, one per chord (a window with at least one note):
  (lmidic onset ldur lvel tension)
  lmidic - midicents of the chord, onset - start in ms, ldur - note lengths in ms,
  lvel - note velocities (the loudest component of the whole sound = 127),
  tension - the lowest tension of the chord in the matrix (nil when the chord does not fit the matrix,
  e.g. notes outside <matrix-range> or quarter tones with <step> 50).
Tension is always relative to the piano matrix (2100 10800), also when <range> is narrower.
For real recordings raise <min-vel> (e.g. 64 = -20 dB), because weak components and Goertzel
leakage give dense chords with very high tension."
  (multiple-value-bind (grid midics times)
      (goertzel-sonogram snd win-ms range :step step :channel channel)
    (multiple-value-bind (chords onsets vels)
        (sonogram-notes grid midics times range-db min-vel)
      (let ((d (or dur (if (cdr times) (round (- (second times) (first times))) 1000)))
            (matrix (spiral-matrix toler n-harm matrix-range)))
        (loop for c in chords
              for o in onsets
              for v in vels
              collect (list c o
                            (make-list (length c) :initial-element d)
                            v
                            (sound-chord-tension c matrix)))))))

(defun sound->tensions-test ()   ; ukryj
  "Fifth C3-G3 (6000 6700) in pure sines, 1 s: chords, onsets, durations and tension
consistent with lmidic-to-tension."
  (let* ((cs (make-instance 'chord-seq :lmidic '((6000 6700)) :lonset '(0)
                                       :ldur '((1000 1000)) :lvel '((127 127))))
         (snd (chseq-to-sound cs :n-harm 1 :attack 0 :release 0))
         (res (sound->tensions snd 125 '(5000 8000) :min-vel 100))
         (ref (lmidic-to-tension '(6000 6700) 2 3 '(2100 10800)))
         (checks
           (list (list "liczba akordow" (= (length res) 8))
                 (list "akordy" (every #'(lambda (r) (equal (first r) '(6000 6700))) res))
                 (list "onsety" (equal (mapcar #'second res) '(0 125 250 375 500 625 750 875)))
                 (list "dlugosci" (every #'(lambda (r) (equal (third r) '(125 125))) res))
                 (list "velocity" (every #'(lambda (r) (every #'(lambda (v) (>= v 120)) (fourth r))) res))
                 (list "tension" (and ref (every #'(lambda (r) (eql (fifth r) ref)) res)))
                 (list "brak tension" (null (sound-chord-tension '(6050) (spiral-matrix 2 3 '(2100 10800)))))))
         (bad (loop for c in checks unless (second c) collect (first c))))
    (om-print (format nil "sound->tensions-test: tension kwinty C3-G3 = ~a" ref))
    (om-print (if bad
                  (format nil "sound->tensions-test: BLAD w: ~{~a~^, ~}" bad)
                  "sound->tensions-test: OK"))
    (null bad)))

;===========================================
; SOUND->CHORD-SEQ: goertzel-sonogram + sonogram->chord-seq w jednym pudelku

(defmethod! sound->chord-seq ((snd t) (win-ms number) (range list)
                              &key (step 100) (channel nil) (range-db 40) (min-vel 1) (dur nil) (chan 1))
  :icon 9107
  :initvals '(nil 125 (2100 10800))
  :indoc '("sound (or om-sound-data)" "window length in ms" "midicent range (from to)")
  :outdoc '("chord-seq")
  :doc "Turns a sound into a CHORD-SEQ: GOERTZEL-SONOGRAM and SONOGRAM->CHORD-SEQ in one box.
Each window of <win-ms> ms is one chord of the midicents of <range> every <step> (default 100).
<channel> nil = mean of the channels (stereo -> mono, default), 0 = left, 1 = right.
<range-db> dynamic range: the loudest component of the whole sound = 127, <range-db> dB below = 0 (default 40).
<min-vel> notes with a lower velocity are skipped (default 1, i.e. only zeros);
   for real recordings raise it (e.g. 64 = -20 dB with range-db 40), because leakage gives dense chords.
<dur> note length in ms (nil = the distance between windows), <chan> MIDI channel."
  (multiple-value-bind (grid midics times)
      (goertzel-sonogram snd win-ms range :step step :channel channel)
    (sonogram->chord-seq grid midics times
                         :range-db range-db :min-vel min-vel :dur dur :chan chan)))

(defun sound->chord-seq-test ()   ; ukryj
  "Fifth C3-G3 (6000 6700) in pure sines, 1 s: 8 chords every 125 ms, result consistent with sound->tensions."
  (let* ((cs (make-instance 'chord-seq :lmidic '((6000 6700)) :lonset '(0)
                                       :ldur '((1000 1000)) :lvel '((127 127))))
         (snd (chseq-to-sound cs :n-harm 1 :attack 0 :release 0))
         (res (sound->chord-seq snd 125 '(5000 8000) :min-vel 100 :chan 2))
         (ref (sound->tensions snd 125 '(5000 8000) :min-vel 100))
         (lm (lmidic res))
         (checks
           (list (list "liczba akordow" (= (length lm) 8))
                 (list "akordy" (every #'(lambda (c) (equal c '(6000 6700))) lm))
                 (list "onsety" (equal (first-n (lonset res) 8)   ; chord-seq dopisuje czas konca
                                       '(0 125 250 375 500 625 750 875)))
                 (list "dlugosci" (every #'(lambda (d) (equal d '(125 125))) (ldur res)))
                 (list "kanal" (every #'(lambda (ch) (equal ch '(2 2))) (lchan res)))
                 (list "zgodnosc z sound->tensions"
                       (and (equal lm (mapcar #'first ref))
                            (equal (lvel res) (mapcar #'fourth ref))))))
         (bad (loop for c in checks unless (second c) collect (first c))))
    (om-print (if bad
                  (format nil "sound->chord-seq-test: BLAD w: ~{~a~^, ~}" bad)
                  "sound->chord-seq-test: OK"))
    (null bad)))

;===========================================
; CHSEQ-TO-SOUND: chord-seq -> dzwiek (sinusoidy z alikwotami)
; Velocity -> amplituda liniowo w dB (odwrotnosc sonogram->chord-seq):
;   amp = 10^((vel/127 - 1) * range-db / 20)

(defparameter *chseq-table-size* 4096)   ; dlugosc tablicy jednego okresu

(defun chseq-vel->amp (vel range-db)   ; ukryj
  "Amplituda 0..1 dla velocity 0..127 (127 = 1, 0 = cisza)."
  (if (and (realp vel) (> vel 0))
      (expt 10d0 (/ (* (- (/ (min vel 127) 127d0) 1) range-db) 20))
      0d0))

(defun chseq-harm-weights (n-harm saturation)   ; ukryj
  "Wagi harmonicznych 1..N-HARM: saturation^(k-1), znormalizowane do sumy 1."
  (let* ((w (loop for k from 1 to (max 1 n-harm) collect (expt (float saturation 1d0) (1- k))))
         (s (reduce #'+ w)))
    (mapcar #'(lambda (x) (/ x s)) w)))

(defun chseq-wavetable (weights)   ; ukryj
  "Jeden okres przebiegu: suma sinusow harmonicznych o wagach WEIGHTS."
  (let* ((n *chseq-table-size*)
         (tab (make-array (1+ n) :element-type 'double-float :initial-element 0d0)))
    (dotimes (i n)
      (setf (aref tab i)
            (loop for w in weights
                  for k from 1
                  sum (* w (sin (/ (* 2 pi k i) n))))))
    (setf (aref tab n) (aref tab 0))   ; ulatwia interpolacje
    tab))

(defun chseq-render-note (mix tab total start len on-n att-n rel-n amp inc tn)   ; ukryj
  "Dodaje jedna nute do bufora MIX (szybka petla z deklaracjami typow)."
  (declare (type (simple-array double-float (*)) mix tab)
           (type fixnum total start len on-n att-n rel-n tn)
           (type double-float amp inc)
           (optimize (speed 3) (safety 0) (float 0)))
  (let ((ph 0d0)
        (tnd (float tn 1d0))
        (inv-att (/ 1d0 (float att-n 1d0)))
        (inv-rel (/ 1d0 (float (max 1 rel-n) 1d0)))
        (end (min len (- total start))))
    (declare (type double-float ph tnd inv-att inv-rel) (type fixnum end))
    (dotimes (j end)
      (declare (type fixnum j))
      (let* ((i0 (truncate ph))
             (fr (- ph (float i0 1d0)))
             (a0 (aref tab i0))
             (v (+ a0 (* fr (- (aref tab (1+ i0)) a0))))
             (env (* (min 1d0 (* (float (1+ j) 1d0) inv-att))
                     (if (< j on-n) 1d0 (- 1d0 (* (float (- j on-n) 1d0) inv-rel))))))
        (declare (type fixnum i0) (type double-float fr a0 v env))
        (incf (aref mix (+ start j)) (* amp env v))
        (incf ph inc)
        (when (>= ph tnd) (setf ph (- ph (* tnd (ffloor ph tnd)))))))
    mix))

(defun chseq-mix-to-buffer (mix buffer total gain)   ; ukryj
  "Normalizuje MIX do szczytu GAIN i kopiuje do bufora OM (float)."
  (declare (type (simple-array double-float (*)) mix)
           (type fixnum total) (type double-float gain)
           (optimize (speed 3) (safety 0) (float 0)))
  (let ((peak 0d0))
    (declare (type double-float peak))
    (dotimes (i total)
      (declare (type fixnum i))
      (let ((x (abs (aref mix i))))
        (declare (type double-float x))
        (when (> x peak) (setf peak x))))
    (let ((scale (if (> peak 0d0) (/ gain peak) 0d0)))
      (declare (type double-float scale))
      (dotimes (i total)
        (declare (type fixnum i))
        (setf (fli:dereference buffer :index i :type :float)
              (coerce (* scale (aref mix i)) 'single-float))))
    buffer))

;; Biblioteka laduje sie ze zrodel (bez plikow .64xfasl), wiec funkcje bylyby
;; interpretowane. Petle po probkach kompilujemy jawnie przy ladowaniu.
(ignore-errors (compile 'chseq-render-note))
(ignore-errors (compile 'chseq-mix-to-buffer))
(ignore-errors (compile 'chseq-wavetable))

(defmethod! chseq-to-sound ((self chord-seq) &key (sr 48000) (n-harm 6) (saturation 0.5)
                            (attack 10) (release 80) (range-db 40) (gain 0.8) (path nil))
  :icon 9107
  :initvals '(nil)
  :indoc '("chord-seq")
  :doc "Turns a chord-seq into sound: each note is a sine wave with overtones.
<sr> sample rate.
<n-harm> number of harmonics (1 = pure sine); harmonics above sr/2 are skipped.
<saturation> overtone saturation 0..1: weight of the k-th harmonic = saturation^(k-1)
   (0 = pure sine, 0.5 = soft, 1 = all equal - bright, sharp sound).
<attack> rise time in ms; <release> decay after the end of the note in ms.
<range-db> dynamic range: velocity 127 = 0 dB, velocity 0 = -range-db (as in sonogram->chord-seq).
<gain> peak of the whole after normalisation (0..1).
<path> nil = returns om-sound-data (connect it to save-sound or SOUND - SOUND asks for a file);
       a file path (.aif/.wav) = saves the file and returns a SOUND object of that file.
Note: for long sequences use the compiled library."
  (let* ((sr (round sr))
         (midics (lmidic self)) (onsets (lonset self)) (durs (ldur self))
         (vels (lvel self)) (offs (loffset self))
         (rel-n (max 0 (round (* release sr) 1000)))
         (notes (loop for chord in midics
                      for on in onsets
                      for cdur in durs
                      for cvel in vels
                      for coff in offs
                      append (loop for m in chord
                                   for i from 0
                                   collect (list m
                                                 (+ on (or (nth i coff) 0))
                                                 (or (nth i cdur) (car (last cdur)) 0)
                                                 (or (nth i cvel) (car (last cvel)) 100)))))
         (total (max 1 (loop for (m on dur) in notes
                             maximize (+ (round (* (max 0 (+ on dur)) sr) 1000) rel-n))))
         (mix (make-array total :element-type 'double-float :initial-element 0d0))
         (tables (make-hash-table))
         (tn *chseq-table-size*))
    (dolist (note notes)
      (destructuring-bind (m on dur vel) note
        (let* ((f (float (mc->f m) 1d0))
               (kmax (max 1 (min n-harm (floor (/ (* 0.5 sr) f)))))
               (tab (or (gethash kmax tables)
                        (setf (gethash kmax tables)
                              (chseq-wavetable (chseq-harm-weights kmax saturation)))))
               (amp (chseq-vel->amp vel range-db))
               (start (max 0 (round (* on sr) 1000)))
               (on-n (max 1 (round (* dur sr) 1000)))
               (att-n (max 1 (min (round (* attack sr) 1000) on-n)))
               (len (+ on-n rel-n))
               (inc (/ (* f tn) sr)))
          (when (> amp 0)
            (chseq-render-note mix tab total start len on-n att-n rel-n
                               (float amp 1d0) (float inc 1d0) tn)))))
    ;; normalizacja i zapis do bufora OM
    (let ((buffer (om-make-pointer total :type :float :clear t)))
      (chseq-mix-to-buffer mix buffer total (float gain 1d0))
      (let ((snd (make-instance 'om-sound-data :buffer buffer :size total :nch 1 :sr sr)))
        (if path
            ;; zapis i zwrot obiektu SOUND z pliku - pudelko SOUND nie pyta wtedy o plik
            (let ((file (save-sound snd path)))
              (om-print (format nil "chseq-to-sound: zapisano ~A" (namestring path)))
              (if file (load-sound-file file) snd))
            snd)))))

(defun chseq-to-sound-test ()   ; ukryj
  "Sine 440 Hz, overtones 1 : 0.5 : 0.25, length of the result."
  (let* ((cs (make-instance 'chord-seq :lmidic '((6900)) :lonset '(0) :ldur '((1000)) :lvel '((127))))
         (s1 (chseq-to-sound cs :n-harm 1 :attack 0 :release 0 :gain 0.8))
         ;; 4800 probek = dokladnie 44 okresy 440 Hz przy 48 kHz
         (a1 (goertzel-amp-from-references-snddata s1 9600 14399 440))
         (s3 (chseq-to-sound cs :n-harm 3 :saturation 0.5 :attack 0 :release 0))
         (h1 (goertzel-amp-from-references-snddata s3 9600 14399 440))
         (h2 (goertzel-amp-from-references-snddata s3 9600 14399 880))
         (h3 (goertzel-amp-from-references-snddata s3 9600 14399 1320))
         (s4 (chseq-to-sound cs :release 80))
         (checks (list (list "amplituda sinusa" (< (abs (- a1 0.8d0)) 1d-3))
                       (list "alikwoty" (and (< (abs (- (/ h2 h1) 0.5d0)) 1d-3)
                                             (< (abs (- (/ h3 h1) 0.25d0)) 1d-3)))
                       (list "dlugosc" (= (size s4) 51840))
                       (list "velocity" (and (= (chseq-vel->amp 127 40) 1d0)
                                             (< (abs (- (chseq-vel->amp 63.5 40) 0.1d0)) 1d-9)
                                             (= (chseq-vel->amp 0 40) 0d0)))))
         (bad (loop for c in checks unless (second c) collect (first c))))
    (om-print (if bad
                  (format nil "chseq-to-sound-test: BLAD w: ~{~a~^, ~}" bad)
                  "chseq-to-sound-test: OK"))
    (null bad)))

(defun chseq-to-sound-speed-test ()   ; ukryj
  "Synthesis time of 1600 notes (40 chords of 40 notes every 150 ms, n-harm 6), without writing a file."
  (let* ((chords (loop repeat 40 collect (loop for m from 3600 by 100 repeat 40 collect m)))
         (cs (make-instance 'chord-seq :lmidic chords
                                       :lonset (loop for i from 0 below 40 collect (* i 150))
                                       :ldur (mapcar #'(lambda (c) (make-list (length c) :initial-element 150)) chords)))
         (t0 (get-internal-real-time))
         (snd (chseq-to-sound cs))
         (sec (/ (- (get-internal-real-time) t0) internal-time-units-per-second 1.0)))
    (om-print (format nil "chseq-to-sound-speed-test: ~,3f s, ~d probek, render skompilowany: ~a"
                      sec (size snd) (compiled-function-p #'chseq-render-note)))
    sec))

;===========================================
; Punkty odniesienia tension w jednej oktawie (np. razkreslnej c1-h1):
; pierwsza pentatonika i pierwszy pelny klaster 12-dzwiekowy.

(defmethod! tension-octave-landmarks (&key (octave '(6000 7100)) (toler 2) (n-harm 3) (range '(2100 10800)))
  :icon 9107
  :numouts 3
  :outdoc '("tension of the first pentatonic" "tension of the first 12-note cluster"
            "course: list of (tension number-of-notes notes) at each change of the number of notes")
  :doc "Looks in the tension matrix (set-matrix-lmidic) for landmarks within one octave.
<octave> the octave range in midicents, by default the one-line octave (6000 7100 = C3..B3).
For consecutive tensions it counts the matrix notes lying in this octave.
Output 1: the smallest tension at which the octave holds 5 notes (pentatonic),
output 2: the smallest tension with all 12 notes (cluster),
output 3: the course - at each change of the number of notes (tension number notes).
<toler> <n-harm> <range> - as in set-matrix-lmidic (default 2, 3, piano matrix)."
  (let ((matrix (spiral-matrix toler n-harm range))
        (lo (first octave)) (hi (second octave))
        (pent nil) (cluster nil) (steps nil) (prev -1))
    (loop for chord in matrix
          for i from 0
          do (let* ((in (sort (remove-duplicates
                               (remove-if-not #'(lambda (m) (<= lo m hi)) chord) :test #'=)
                              #'<))
                    (n (length in)))
               (when (/= n prev)
                 (push (list i n in) steps)
                 (setf prev n))
               (when (and (null pent) (>= n 5)) (setf pent i))
               (when (and (null cluster) (>= n 12)) (setf cluster i))))
    (om-print (format nil "tension-octave-landmarks: pentatonika ~A, klaster ~A, roznica ~A"
                      pent cluster (and pent cluster (- cluster pent))))
    (values pent cluster (reverse steps))))
