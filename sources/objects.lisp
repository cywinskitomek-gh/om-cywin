(in-package :om)

;==================================================


(defun make-chord-sequence (midic-list onset-list dur-list vel-list off-list chan-list legato-list)
  "Makes a CHORD-SEQ from lists of midicents, onsets, durations, velocities, offsets, channels and legato."

  (make-instance 'chord-seq
                 :lmidic midic-list
                 :lonset onset-list
                 :ldur dur-list
                 :lvel vel-list
                 :loffset off-list
                 :lchan chan-list
                 :legato legato-list
                 )

)


;===================================================


(defun make-voice (tree-list chord-list tempo-number legato-number ties-list)
  "Makes a VOICE from a rhythm tree, chords, tempo, legato and ties."

  (make-instance 'voice
                 :tree tree-list
                 :chords chord-list
                 :tempo tempo-number
                 :legato legato-number
                 :ties ties-list
                 )

)

;==================================================

(defun make-multi-sequence (chseq-list)
  "Makes a MULTI-SEQ from a list of chord-seqs."

  (make-instance 'multi-seq
  	:chord-seqs chseq-list
	)

)

;==================================================

(defun make-note (midic vel dur chan)
  "Makes a NOTE from midicents, velocity, duration and channel."

  (make-instance 'note
  	:midic midic
  	:vel vel
  	:dur dur
  	:chan chan
  	)
)

;==================================================

(defun make-chord (midic-list vel-list offset-list dur-list chan-list)
  "Makes a CHORD from lists of midicents, velocities, offsets, durations and channels."

  (make-instance 'chord
  	:lmidic midic-list
  	:lvel vel-list
  	:loffset offset-list
  	:ldur dur-list
  	:lchan chan-list
  	)
)

;==================================================

(defun make-poly (voice-list)
  "Makes a POLY from a list of voices."

  (make-instance 'poly
  	:voices voice-list
  	)
)

;==================================================

(defun make-midievent (ev-type ev-date ev-ref ev-port ev-chan ev-fields)
  "Makes a MIDIEVENT from type, date, ref (track), port, channel and fields."

  (make-instance 'MidiEvent   
                 :ev-type ev-type
                 :ev-date ev-date
                 :ev-ref ev-ref
                 :ev-port ev-port
                 :ev-chan ev-chan
                 :ev-fields ev-fields)

)