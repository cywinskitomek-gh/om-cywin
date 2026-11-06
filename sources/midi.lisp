(in-package :om)

(defun midievents-time-shift (midevts time-shift)
  "Shifts MIDI events in time by <time-shift> ms." 

    (loop for midevt in midevts
          collect (make-instance 'MidiEvent   
                                 :ev-type (ev-type midevt)
                                 :ev-date (+ (ev-date midevt) time-shift)
                                 :ev-ref (ev-ref midevt)
                                 :ev-port (ev-port midevt)
                                 :ev-chan (ev-chan midevt)
                                 :ev-fields (ev-fields midevt)))

)



(defun midievents-transpose (midevts transpose-n-halftone)
  "Transposes note events by <transpose-n-halftone> semitones." 

    (loop for midevt in midevts
          collect (make-instance 'MidiEvent   
                                 :ev-type (ev-type midevt)
                                 :ev-date (ev-date midevt)
                                 :ev-ref (ev-ref midevt)
                                 :ev-port (ev-port midevt)
                                 :ev-chan (ev-chan midevt)
                                 :ev-fields (if (or (equal (ev-type midevt) :keyon)
                                                    (equal (ev-type midevt) :keyoff))
                                                (list (+ (first (ev-fields midevt)) transpose-n-halftone) (second (ev-fields midevt)))
                                                (ev-fields midevt))))

)



(defun midievents-add-velocity (midevts add-vel)
  "Adds <add-vel> to the velocities of note-on events." 

    (loop for midevt in midevts
          collect (make-instance 'MidiEvent   
                                 :ev-type (ev-type midevt)
                                 :ev-date (ev-date midevt)
                                 :ev-ref (ev-ref midevt)
                                 :ev-port (ev-port midevt)
                                 :ev-chan (ev-chan midevt)
                                 :ev-fields (if (equal (ev-type midevt) :keyon)
                                                (list (first (ev-fields midevt))
                                                      (om-clip (+ (second (ev-fields midevt)) add-vel) 0 127))
                                                (ev-fields midevt))))

)

(defun midievents-add-random-velocity (midevts rnd-min rnd-max)
  "Adds a random value between <rnd-min> and <rnd-max> to the velocities of note-on events." 

    (loop for midevt in midevts
          collect (make-instance 'MidiEvent   
                                 :ev-type (ev-type midevt)
                                 :ev-date (ev-date midevt)
                                 :ev-ref (ev-ref midevt)
                                 :ev-port (ev-port midevt)
                                 :ev-chan (ev-chan midevt)
                                 :ev-fields (if (equal (ev-type midevt) :keyon)
                                                (list (first (ev-fields midevt))
                                                      (om-clip (+ (second (ev-fields midevt)) (om-random rnd-min rnd-max)) 0 127))
                                                (ev-fields midevt))))

)



(defun midievents-add-velocity-bpf (midevts bpf)
  "Changes the velocities of note-on events over time with a BPF."

    (loop for midevt in midevts
          collect (make-instance 'MidiEvent   
                                 :ev-type (ev-type midevt)
                                 :ev-date (ev-date midevt)
                                 :ev-ref (ev-ref midevt)
                                 :ev-port (ev-port midevt)
                                 :ev-chan (ev-chan midevt)
                                 :ev-fields (if (equal (ev-type midevt) :keyon)
                                                (list (first (ev-fields midevt))
                                                      (om-clip (+ (second (ev-fields midevt))
                                                                  (x-transfer bpf (ev-date midevt) 0)) 0 127))
                                                (ev-fields midevt))))

)

(defun midievents-scale-velocity (midevts min-vel max-vel exponent)
  "Scales the velocities of MIDI events to <min-vel>..<max-vel> with the exponent <exponent>."

  (let* ((velocities (get-velocities-from-midi midevts))
         (min-vel-in (list-min velocities))
         (max-vel-in (list-max velocities)))

    (om-print (format nil "~% min vel in: ~D~% max vel in: ~D~%" min-vel-in max-vel-in))

    (loop for midevt in midevts
          collect (make-instance 'MidiEvent   
                                 :ev-type (ev-type midevt)
                                 :ev-date (ev-date midevt)
                                 :ev-ref (ev-ref midevt)
                                 :ev-port (ev-port midevt)
                                 :ev-chan (ev-chan midevt)
                                 :ev-fields (if (equal (ev-type midevt) :keyon)
                                                (list (first (ev-fields midevt))
                                                      (om-clip (round (om-scale^ (second (ev-fields midevt)) min-vel max-vel exponent min-vel-in max-vel-in)) 0 127))
                                                (ev-fields midevt))))
    )
)

(defun midievents-clip/scale-velocity (midevts min-vel max-vel exponent)
  "Clips and scales the velocities of MIDI events to <min-vel>..<max-vel> with the exponent <exponent>."

  (let* ((velocities (get-velocities-from-midi midevts))
         (min-vel-in (list-min velocities))
         (max-vel-in (list-max velocities))
         (vels-out (om-clip velocities min-vel max-vel))
         (min-vel-out (list-min vels-out))
         (max-vel-out (list-max vels-out)))

    (loop for midevt in midevts
          collect (make-instance 'MidiEvent   
                                 :ev-type (ev-type midevt)
                                 :ev-date (ev-date midevt)
                                 :ev-ref (ev-ref midevt)
                                 :ev-port (ev-port midevt)
                                 :ev-chan (ev-chan midevt)
                                 :ev-fields (if (equal (ev-type midevt) :keyon)
                                                (list (first (ev-fields midevt))
                                                      (om-clip (round (om-scale^ (second (ev-fields midevt)) min-vel-out max-vel-out exponent min-vel-in max-vel-in)) 0 127))
                                                (ev-fields midevt))))
    )

)

(defun midievents-limit-volume (midevts min-vol max-vol)
  "Limits volume (CC 7) events to <min-vol>..<max-vol>."

    (loop for midevt in midevts
          collect (make-instance 'MidiEvent   
                                 :ev-type (ev-type midevt)
                                 :ev-date (ev-date midevt)
                                 :ev-ref (ev-ref midevt)
                                 :ev-port (ev-port midevt)
                                 :ev-chan (ev-chan midevt)
                                 :ev-fields (if (and (equal (ev-type midevt) :ctrlchange)
                                                     (= (first (ev-fields midevt)) 7))
                                                (list 7
                                                      (om-clip (second (ev-fields midevt)) min-vol max-vol))
                                                (ev-fields midevt))))
)

(defun midievents-scale-volume (midevts min-vol max-vol)
  "Scales volume (CC 7) events to <min-vol>..<max-vol>."

  (let* (find-dt
         (mid-k-sus-time (remove-duplicates (reduce #'append (mapcar #'(lambda (x) (arithm-ser (second x) (+ (second x) (third x)) 1)) (midievents-to-lnotes midevts)))))
         (volume-evts (get-ctrlchange7-midievents midevts))
         (volumes (remove-duplicates (loop for vol-evt in volume-evts
                                           do (setq find-dt (find (ev-date vol-evt) mid-k-sus-time))
                                           if find-dt
                                           collect (second (ev-fields vol-evt)))))
         (in-vol-min (list-min volumes))
         (in-vol-max (list-max volumes))
         )

  (loop for midevt in midevts
          collect (make-instance 'MidiEvent   
                                 :ev-type (ev-type midevt)
                                 :ev-date (ev-date midevt)
                                 :ev-ref (ev-ref midevt)
                                 :ev-port (ev-port midevt)
                                 :ev-chan (ev-chan midevt)
                                 :ev-fields (if (and (equal (ev-type midevt) :ctrlchange)
                                                     (= (first (ev-fields midevt)) 7))
                                                (list 7
                                                      (om-clip (round (om-scale (second (ev-fields midevt)) min-vol max-vol in-vol-min in-vol-max)) 0 127))
                                                (ev-fields midevt))))
  )
)

(defun get-midievents-from-to (midi from to)
  "MIDI events of MIDI between <from> and <to> ms."

  (get-midievents midi #'(lambda (x) (test-date x from to)))

)

(defun midievents-fixed-velocity (midevts vel)
  "Sets the velocity of all note-on events to <vel>."

    (loop for midevt in midevts
          collect (make-instance 'MidiEvent   
                                 :ev-type (ev-type midevt)
                                 :ev-date (ev-date midevt)
                                 :ev-ref (ev-ref midevt)
                                 :ev-port (ev-port midevt)
                                 :ev-chan (ev-chan midevt)
                                 :ev-fields (if (equal (ev-type midevt) :keyon)
                                                (list (first (ev-fields midevt))
                                                      vel)
                                                (ev-fields midevt))))

)

(defun midievents-change-track (midevts track)
  "Sets the track (ev-ref) of all MIDI events to TRACK and returns them."

  (loop for midevt in midevts
        do (setf (ev-ref midevt) track))

  midevts

)

(defun get-midievents-type (midi type)
  "MIDI events of MIDI of the given TYPE (e.g. :keyon, :tempo)."

  (get-midievents midi #'(lambda (x) (test-type x type)))

)

(defun get-midievents-not-type (midi type)
  "MIDI events of MIDI of every type except TYPE."

  (get-midievents midi #'(lambda (x) (not (test-type x type))))

)

(defun get-tempo-midievents (midi)
  "Tempo events of MIDI."

  (get-midievents-type midi :tempo)

)

(defun get-tempo-timesign-midievents (midi)
  "Tempo and time signature events of MIDI."

  (get-midievents midi #'(lambda (x) (or (test-type x :tempo) (test-type x :timesign))))

)

(defun get-tempo-from-midi (midi)
  "Tempo (quarter = MM) of the first tempo event of MIDI."

  (caar (mapcar #'ev-fields (get-tempo-midievents midi)))

)

(defun get-16thdur-from-midi (midi)
  "Sixteenth-note duration in ms for the tempo of MIDI."

  (round (tempo-to-16thdur (get-tempo-from-midi midi)))

)

(defun get-timesign-midievents (midi)
  "Time signature events of MIDI."

  (get-midievents-type midi :timesign)

)

(defun get-keyon-midievents (midi)
  "Note-on events of MIDI."

  (get-midievents-type midi :keyon)

)

(defun get-keyoff-midievents (midi)
  "Note-off events of MIDI."

  (get-midievents-type midi :keyoff)

)

(defun get-keyon-keyoff-midievents (midi)
  "Note-on and note-off events of MIDI."

  (get-midievents midi #'(lambda (x) (or (test-type x :keyon) (test-type x :keyoff))))

)

(defun get-specific-keyon-keyoff-midievents (midi key)
  "Note-on and note-off events of MIDI for the MIDI key KEY."

  (get-midievents midi #'(lambda (x) (and (or (test-type x :keyon) (test-type x :keyoff)) (= (first (ev-fields x)) key))))

)

(defun get-not-keyon-keyoff-midievents (midi)
  "MIDI events other than note-on and note-off."

  (get-midievents midi #'(lambda (x) (and (not (test-type x :keyon)) (not (test-type x :keyoff)))))

)

(defun get-not-tempo-timesign-midievents (midi)
  "MIDI events other than tempo and time signature."

  (get-midievents midi #'(lambda (x) (not (or (test-type x :tempo) (test-type x :timesign)))))

)

(defun get-not-timesign-midievents (midi)
  "MIDI events other than time signature."

  (get-midievents midi #'(lambda (x) (not (test-type x :timesign))))

)

(defun get-ctrlchange-midievents (midi)
  "Control change events of MIDI."

  (get-midievents-type midi :ctrlchange)

)

(defun get-ctrlchange7-midievents (midi)
  "Volume (CC 7) events of MIDI."

  (remove-if #'(lambda (x) (/= (first (ev-fields x)) 7)) (get-ctrlchange-midievents midi))

)

(defun get-dates-midievents (midi)
  "Dates (ms) of all MIDI events."

  (mapcar #'ev-date (get-midievents midi))

)

(defun get-midievents-by-date (midi date)
  "MIDI events at the date DATE (ms)."

  (remove date (get-midievents midi) :test #'/= :key #'ev-date)

)

(defun midievents-change-date (midi date new-date)
  "Copies of the MIDI events at DATE moved to NEW-DATE."

   (loop for midevt in (get-midievents-by-date midi date)
         collect (make-instance 'MidiEvent   
                                :ev-type (ev-type midevt)
                                :ev-date new-date
                                :ev-ref (ev-ref midevt)
                                :ev-port (ev-port midevt)
                                :ev-chan (ev-chan midevt)
                                :ev-fields (ev-fields midevt)))
)

(defun get-not-ctrlchange-midievents (midi)
  "MIDI events other than control change."

  (get-midievents-not-type midi :ctrlchange)

)


(defun get-duration-midi (midi)
  "Duration of MIDI: the date of the last event in ms."

  (list-max (mapcar #'ev-date (get-midievents midi)))

)

(defun get-midi-duration (midi)
  "Duration of MIDI: the date of the last event in ms (same as get-duration-midi)."

  (get-duration-midi midi)

)

(defun get-important-midievents (midi)
  "Note, tempo, time signature and control change events of MIDI."

  (get-midievents midi #'(lambda (x) (or (test-type x :keyon) (test-type x :keyoff) (test-type x :tempo) (test-type x :timesign) (test-type x :ctrlchange))))

)


(defun get-midievents-from-midi-track (midi track)
  "MIDI events of MIDI on the track TRACK."

  (get-midievents midi #'(lambda (x) (test-track x track)))

)

(defun set-midievents-port (midevts port)
  "Copies of MIDI events with the port set to PORT."

  (loop for midevt in midevts
        collect (make-instance 'MidiEvent   
                               :ev-type (ev-type midevt)
                               :ev-date (ev-date midevt)
                               :ev-ref (ev-ref midevt)
                               :ev-port port
                               :ev-chan (ev-chan midevt)
                               :ev-fields (ev-fields midevt)))

)

(defun set-new-midievent-date (midevt date)
  "Sets the date of a MIDI event and returns it."

  (setf (ev-date midevt) date)

  midevt

)

(defun midievents-tempo-change (midevts new-tempo)
  "Changes the tempo of MIDI events to NEW-TEMPO: dates are scaled by the ratio of the old and new tempo."

  (let ((factor (/ (get-tempo-from-midi midevts) new-tempo)))

    (loop for midevt in midevts
          collect (make-instance 'MidiEvent   
                                 :ev-type (ev-type midevt)
                                 :ev-date (round (* (ev-date midevt) factor))
                                 :ev-ref (ev-ref midevt)
                                 :ev-port (ev-port midevt)
                                 :ev-chan (ev-chan midevt)
                                 :ev-fields (if (equal (ev-type midevt) :tempo)
                                                new-tempo
                                                (ev-fields midevt))))

    )

)

(defun midievents-date-change (midevts factor)
  "Multiplies the dates of MIDI events by FACTOR."

    (loop for midevt in midevts
          collect (make-instance 'MidiEvent   
                                 :ev-type (ev-type midevt)
                                 :ev-date (round (* (ev-date midevt) factor))
                                 :ev-ref (ev-ref midevt)
                                 :ev-port (ev-port midevt)
                                 :ev-chan (ev-chan midevt)
                                 :ev-fields (ev-fields midevt)))

)

(defun get-midifile-name (midifile)
  "File name (with extension) of a MIDI file object."

  (file-namestring (pathname (midifilename midifile)))

)

(defun get-midifile-pathname-name (midifile)
  "File name (without extension) of a MIDI file object."

  (pathname-name (file-namestring (pathname (midifilename midifile))))

)

(defun get-channel-midievents (midi channel)
  "MIDI events of MIDI on the channel CHANNEL."

  (get-midievents midi #'(lambda (x) (test-channel x channel)))

)

(defun get-channels-list-midievents-noduplicates (midi)
  "Sorted list of the MIDI channels used, without duplicates."

  (sort
   (remove-duplicates
    (loop for midievent in (get-midievents midi)
          collect (ev-chan midievent)))
   #'<)

)

(defun temporal-sort-midievents (midievents)
  "Sorts MIDI events by date; at the same date other events come first, then note-offs, then note-ons."

    (sort
     (flat
      (list (get-not-keyon-keyoff-midievents midievents)
            (get-keyoff-midievents midievents)
            (get-keyon-midievents midievents))
      )
     #'< :key #'ev-date)

)

(defun get-velocities-from-midi (midi)
  "Velocities of the note-on events (non-zero) of MIDI."

  (let ((midevts (get-keyon-keyoff-midievents midi)))

    (remove nil 
            (loop for midevt in midevts
                  collect (if (and (equal (ev-type midevt) :keyon) (/= (second (ev-fields midevt)) 0)) (second (ev-fields midevt)))))
    )
)

(defun midievents-quantize (midievents min-dur)
  "Quantises the dates of MIDI events to multiples of MIN-DUR ms."

     (loop for midievent in midievents
           do (setf (ev-date midievent) (* (round (/ (ev-date midievent) min-dur)) min-dur)))

     midievents
)

(defun get-midi-volume (midi)
  "Values of the volume (CC 7) events of MIDI."

  (mapcar #'(lambda (x) (second (ev-fields x))) (get-ctrlchange7-midievents midi))

)

(defun get-min-midi-volume (midi)
  "Lowest volume (CC 7) value of MIDI."

  (reduce #'min (get-midi-volume midi))

)

(defun get-max-midi-volume (midi)
  "Highest volume (CC 7) value of MIDI."

  (reduce #'max (get-midi-volume midi))

)

(defun get-min-max-midi-volume (midi)
  "Lowest and highest volume (CC 7) value of MIDI."

  (list-min-max (get-midi-volume midi))

)

(defun midievents-channel-change (midevts new-channel)
  "Copies of MIDI events with the channel set to NEW-CHANNEL (events on channel 0 unchanged)."

    (loop for midevt in midevts
          collect (make-instance 'MidiEvent   
                                 :ev-type (ev-type midevt)
                                 :ev-date (ev-date midevt)
                                 :ev-ref (ev-ref midevt)
                                 :ev-port (ev-port midevt)
                                 :ev-chan (if (zerop (ev-chan midevt))  (ev-chan midevt) new-channel)
                                 :ev-fields (ev-fields midevt)))

)

(defun bpf-set-midievents-velocity (midevts vel-min vel-max bpf)
  "Sets the velocities of note-on events over time with a BPF scaled to VEL-MIN..VEL-MAX."

  (let ((midievents-dates (get-dates-midievents midevts))
        ;(midievents-velocities (get-velocities-from-midi midevts))
        )

  (setf bpf (bpf-scale bpf :x1 (list-min midievents-dates) :x2 (list-max midievents-dates) :y1 vel-min :y2 vel-max))

    (loop for midevt in midevts
          collect (make-instance 'MidiEvent   
                                 :ev-type (ev-type midevt)
                                 :ev-date (ev-date midevt)
                                 :ev-ref (ev-ref midevt)
                                 :ev-port (ev-port midevt)
                                 :ev-chan (ev-chan midevt)
                                 :ev-fields (if (equal (ev-type midevt) :keyon)
                                                (list (first (ev-fields midevt))
                                                      (om-clip (x-transfer bpf (ev-date midevt) 0) 1 127))
                                                (ev-fields midevt))))
    )
)

(defun midievents-scale-velocity-bpf-min-max (midevts min max in-bpf)
  "Scales the velocities of note-on events over time with a BPF, between MIN and MAX."

  (let ((midievents-dates (get-dates-midievents midevts))
        (velocities (get-velocities-from-midi midevts))
        (bpf (simple-bpf-from-list (copy-list (x-points in-bpf)) (copy-list (y-points in-bpf)) 'bpf 10))
        )

  (setf bpf (bpf-scale bpf :x1 (list-min midievents-dates) :x2 (list-max midievents-dates) :y1 (om-round (/ min (list-max velocities)) 10 1) :y2 (om-round (/ max (list-max velocities)) 10 1)))
  (little-bpf bpf)

    (loop for midevt in midevts
          collect (make-instance 'MidiEvent   
                                 :ev-type (ev-type midevt)
                                 :ev-date (ev-date midevt)
                                 :ev-ref (ev-ref midevt)
                                 :ev-port (ev-port midevt)
                                 :ev-chan (ev-chan midevt)
                                 :ev-fields (if (equal (ev-type midevt) :keyon)
                                                (list (first (ev-fields midevt))
                                                      (om-clip (round (* (second (ev-fields midevt))
                                                                         (x-transfer bpf (ev-date midevt) 10)
                                                                         )) min max)
                                                      )
                                                (ev-fields midevt))))
    )

)

(defun midievents-time-random-shift (midevts rnd-min rnd-max)
  "Shifts every MIDI event in time by a random value between RND-MIN and RND-MAX ms." 

    (loop for midevt in midevts
          collect (make-instance 'MidiEvent   
                                 :ev-type (ev-type midevt)
                                 :ev-date (+ (ev-date midevt) (om-random rnd-min rnd-max))
                                 :ev-ref (ev-ref midevt)
                                 :ev-port (ev-port midevt)
                                 :ev-chan (ev-chan midevt)
                                 :ev-fields (ev-fields midevt)))

)
 