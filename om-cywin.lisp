;==============================================;
;             :.    .      r@@X;. ;;;:;        ;
;            ..   r;.. 70M   rSWXZ.:;;.        ;
;   .   . .   .:r8W   r@@@M    ;.r8 ;  .:      ;
;      ...   .ZB@@:  r@@@@@@;  rr 07.7 . .     ;
;       . .; r@@@;  r@@@@@@@@r  0r.8r7: :.     ;
;          ;;8@@X   rZrrX7rr77  ;8.Z0; . .     ;
;     .    .;8BZ  ...  @@@;   .  rXr0   : .    ;
;      .   ;       .  r@@@7       :.7  ..:     ;
;         .;..       r@@@@@;        ;   . .    ;
;    ... :rr7@@7   r@@@@@@@@BZ:   ;.r: .   .   ;
;     . ;;;;7@@; r@@@r;rrrr;@@@X. ZB;..::      ;
;         r7:W. r@@@S       r@@@@r 7r; . . .   ;
;     .   X7.r  rM@@@8MZ7X7Z@@@B0B  ;r.        ;
;        :;r   ;XX0BXr72ZXXWZB0BXZ:  r.        ;
;   . .  :;;; rMS;:::   . ..;rXX0Mr ;.. .      ;
;    .   ::.;;rZZ7.     ;:..  .rZ8r:::         ;
;              .;;: .  .7;:   :rrrX..          ;
;          ;.    . .;;777r;;...; .;.           ;
;     :;:   ;:     .;;X;r;;r;        .;;::     ;
;      ;r7; .; ;; ;r;;;  ;rr...... ;;.         ;
;             ;rr.rZZX7ZXr;.X7rr;.;;;          ;
;      .     ;;;;;70ZZW0Z2;Z8ZX7.    ;;        ;
;   .   ;;;     .;X7rrX227r0WZ7    r8Z7;....   ;
;         .r:.   ;X;;;:;;;:r;rr  r7Zr  ;   :   ;
;==============================================;
(in-package :om)
;==============================================;
(om-print (format nil "
;==============================================;
;                   om-cywin                   ;
;                 ver. 1.0.016                 ;
;              2026-11-06 19:27:55             ;
;              by Tomasz Cywinski              ;
;     code written with Claude (Anthropic)     ;
;==============================================;
"))
;==============================================;
(mapc #'(lambda (file)
  (compile&load
    (om-make-pathname
      :directory (append (pathname-directory *load-pathname*) (list "sources"))
      :name file)))
        '(
          "alfabet-luts"
          "audio"
          "bpf"
          "charts"
          "color-luts"
          "constants"
          "conversions"
          "files"
          "instruments"
          "llnotes"
          "lnotes"
          "lut-view"
          "midi"
          "objects"
          "pictures"
          "probability"
          "sequences"
          "series"
          "tension-spiral"
          "utils"
         )
)
;==============================================;
; kompilacja funkcji biblioteki (definicja na koncu sources/utils.lisp)
(when (fboundp (quote om-cywin-compile-library)) (om-cywin-compile-library))
;==============================================;
(set-lib-release 1.0)
;==============================================;
(fill-library

  '(

    ("ALFABET-LUTS" nil nil
      (
        alfabet-torunski-letters
        make-alfabet-luts
      )
    nil)


    ("AUDIO" nil nil
      (
        get-samples-from-sound
        goertzel
        goertzel-amp
        goertzel-amp-from-references
        goertzel-amp-from-references-snddata
        goertzel-from-references
        goertzel-from-references-snddata
        goertzel-power->amp
        goertzel-sonogram
        simple-sound-generator
        sqnr
      )
    nil)


    ("BPF" nil nil
      (
        bpf+
        bpf-from-text
        invert-bpf
        jitter-bpf
        little-bpf
        reverse-bpf
        reverse/invert-bpf
        set-bpf-decimals
      )
    nil)


    ("CHARTS" nil (pie-chart radar-chart)
      (
        pie->svg
        pie-percentages
        radar->svg
      )
    nil)


    ("COLOR-LUTS" nil nil
      (
        make-color-lut
      )
    nil)


    ("CONSTANTS" nil nil
      (
        1/cent
        1/phi
        1/phi^2
        1/phi^n
        1/tempered-semitone
        1/tempered-whole-tone
        2pi
        cent
        display-elements-numbers-list
        e_const
        phi
        phi^2
        phi^n
        pi
        tempered-semitone
        tempered-whole-tone
      )
    nil)


    ("CONVERSIONS" nil nil
      (
        16thdur-to-tempo
        accidentals-string-to-harp-pedals-setting
        accidentals-string-to-harp-scale
        chord-to-lnotes
        chord-to-midifile
        chseq-list-to-llnotes
        chseq-to-lnotes
        chseq-to-midifile
        chseq-to-sound
        f->n
        filter-key-sets
        harp-pedals-setting-to-harp-scale
        harp-pedals-setting-to-harp-scale-from-0
        harp-scale-from-0-to-harp-pedals-setting
        harp-scale-from-0-to-harp-scale
        harp-scale-to-harp-pedals-setting
        harp-scale-to-harp-scale-from-0
        llmidic-to-all-tensions
        llmidic-to-more-llmidic
        llmidic-to-tensions
        llnotes-to-chseq-list
        llnotes-to-mchseq
        lmidic-to-all-tensions
        lmidic-to-more-lmidic
        lmidic-to-tension
        lmidievent-to-midievent
        lmidievents-to-midievents
        lmidievents-to-midiseq
        lnotes-to-chseq
        lnotes-to-lmidicmod+sumveldur+count
        lnotes-to-midievents
        mchseq-to-llnotes
        mchseq-to-midifile
        midi-to-lmidievents
        midi-to-lnotes
        midi-to-string
        midievents-to-chseq
        midievents-to-lnotes
        midievents-to-midifile
        midievents-to-midiseq
        midievents-velocity-to-volume
        midilist-to-lnotes
        midiseq-to-lnotes
        midiseq-to-midifile
        msec-to-format-dur
        note-to-midifile
        poly-to-midifile
        rank-key-sets
        score-object-to-midifile
        seq-to-midifile
        sonogram->chord-seq
        sound->chord-seq
        sound->tensions
        tempo-to-16thdur
        tension-key-sets
        tension-key-sets+
        tension-octave-landmarks
        tension-to-lmidic
        tensions-to-llmidic
        vel-to-dyn-mark
        velocity-to-dynamics-markings
        voice-to-midifile
      )
    nil)


    ("FILES" nil nil
      (
        clear-directory
      )
    nil)


    ("INSTRUMENTS" nil nil
      (
        accidentals-strings
        get-scales-from-0-diffs-l&r-harp-ped-sets
        harp-filter-chseq
        harp-filter-lnotes
        harp-pedals-setting-filter-chseq
        harp-pedals-setting-filter-lnotes
        harp-pedals-settings
        harp-scales
        harp-scales-from-0
        set-choir
        set-orchestra
      )
    nil)


    ("LLNOTES" nil nil
      (
        change-llnotes-tempo
        cut-llnotes-below-vel
        cut-n-llnotes-from
        get-gcd-llnotes-ldur
        get-start-llnotes-lonset
        llnotes-portato
        llnotes-scale-filter
        llnotes-staccato
        repeat-llnotes
        reverse-llnotes
        scale-llnotes-velocity
        shift-llnotes-lonset
        shift-llnotes-lonset-to-beginning
        tie-llnotes
        total-llnotes-duration
      )
    nil)


    ("LNOTES" nil nil
      (
        add-lnotes-velocity
        add-random-lnotes-velocity
        approx-lnotes-lmidic
        bpf-set-lnotes-velocity
        change-lnotes-chan
        change-lnotes-tempo
        consolidate-lnotes-notes
        cut-lnotes-below-dur
        cut-lnotes-below-vel
        cut-lnotes-from-to
        cut-lnotes-from-to-no-shift
        cut-n-lnotes-from
        divide-dur-&-reduct-dupl-lnotes
        divide-dur-min-dur-&-reduct-dupl-lnotes
        divide-lnotes-duration
        divide-lnotes-duration-by-lonset
        divide-lnotes-duration-dur-min
        divide-lnotes-duration-dur-min-ceiling
        explode-lnotes
        get-gcd-lnotes-ldur
        get-lnotes-lchan-no-duplicates
        get-lnotes-lmidic-no-duplicates
        get-lnotes-lonset-no-duplicates
        get-lnotes-lvel-no-duplicates
        get-start-lnotes-lonset
        limit-lnotes-velocity
        list-lnotes-by-chan
        list-lnotes-by-chan-list
        list-lnotes-by-instruments
        list-lnotes-by-lchan
        list-lnotes-by-lmidic
        list-lnotes-by-lonset
        list-lnotes-by-streams
        list-lnotes-by-streams-choir
        lnotes-band-midic-filter
        lnotes-cb-range-filter
        lnotes-delete-overlaps
        lnotes-fixed-length
        lnotes-legato
        lnotes-no-chan
        lnotes-overlaps
        lnotes-overlaps-mindur
        lnotes-portato
        lnotes-quantize
        lnotes-quantize-2
        lnotes-quantize-3
        lnotes-quantize-4
        lnotes-quantize-5
        lnotes-quantize-5a
        lnotes-scale-filter
        lnotes-staccato
        lnotes-staccato-mindur
        lnotes-vc-range-filter
        lnotes-vl-range-filter
        lnotes-vn-range-filter
        loudest-lnotes-pno-facture
        loudest-lnotes-with-tie
        loudest-lnotes-without-tie
        midic-lnotes
        min-max-list-lnotes-vel
        mirror-lnotes
        mirror_2-lnotes
        multi-lnotes-velocity
        reduct-dupl-lnotes
        repeat-lnotes
        reverse-lnotes
        scale-lnotes-velocity
        scale-lnotes-velocity-2
        scale-lnotes-velocity-3
        scale-lnotes-velocity-3a
        scale-lnotes-velocity-bpf
        separate-lnotes-notes
        shift-lnotes-lonset
        shift-lnotes-lonset-to-beginning
        tie-lnotes
        tie-lnotes-256
        tie-some-lnotes
        total-lnotes-duration
        transp-&-change-tempo-lnotes
        transp-lnotes
      )
    nil)


    ("LUT-VIEW" nil nil
      (
        lut-curves
        lut-palette
        lut-radar
        lut-wheel
        save-hdr-png
      )
    nil)


    ("MIDI" nil nil
      (
        bpf-set-midievents-velocity
        get-16thdur-from-midi
        get-channel-midievents
        get-channels-list-midievents-noduplicates
        get-ctrlchange-midievents
        get-ctrlchange7-midievents
        get-dates-midievents
        get-duration-midi
        get-important-midievents
        get-keyoff-midievents
        get-keyon-keyoff-midievents
        get-keyon-midievents
        get-max-midi-volume
        get-midi-duration
        get-midi-volume
        get-midievents-by-date
        get-midievents-from-midi-track
        get-midievents-from-to
        get-midievents-not-type
        get-midievents-type
        get-midifile-name
        get-midifile-pathname-name
        get-min-max-midi-volume
        get-min-midi-volume
        get-not-ctrlchange-midievents
        get-not-keyon-keyoff-midievents
        get-not-tempo-timesign-midievents
        get-not-timesign-midievents
        get-specific-keyon-keyoff-midievents
        get-tempo-from-midi
        get-tempo-midievents
        get-tempo-timesign-midievents
        get-timesign-midievents
        get-velocities-from-midi
        midievents-add-random-velocity
        midievents-add-velocity
        midievents-add-velocity-bpf
        midievents-change-date
        midievents-change-track
        midievents-channel-change
        midievents-clip/scale-velocity
        midievents-date-change
        midievents-fixed-velocity
        midievents-limit-volume
        midievents-quantize
        midievents-scale-velocity
        midievents-scale-velocity-bpf-min-max
        midievents-scale-volume
        midievents-tempo-change
        midievents-time-random-shift
        midievents-time-shift
        midievents-transpose
        set-midievents-port
        set-new-midievent-date
        temporal-sort-midievents
      )
    nil)


    ("OBJECTS" nil nil
      (
        make-chord
        make-chord-sequence
        make-midievent
        make-multi-sequence
        make-note
        make-poly
        make-voice
      )
    nil)


    ("PICTURES" nil nil
      (
        save-picture-safe
      )
    nil)


    ("PROBABILITY" nil nil
      (
        choice-gaussian-weights
        choice-weights
        n-row-pascals-triangle
        pascals-triangle
        pascals-triangle-r
      )
    nil)



    ("SEQUENCES" nil nil
      (
        approx-seq-lmidic
        change-seq-chan
        change-seq-list-chan
        change-seq-list-tempo
        change-seq-tempo
        chseq-from-text
        chseq-scale-filter
        consolidate-chseq-notes
        cut-n-chseq-lnotes-from
        cut-notes-below-vel
        explode-chseq
        get-gcd-seq-ldur
        get-seq-lonset-no-duplicates
        limit-seq-velocity
        repeat-chseq
        reverse-seq
        separate-chseq-notes
        shift-seq-lonset
        shift-seq-lonset-to-beginning
        tie-chseq-notes
        transp-&-change-tempo-seq
        transp-seq
      )
    nil)


    ("SERIES" nil nil
      (
        0&natural-ser
        collatz
        interpole2
      )
    nil)


    ("TENSION-SPIRAL" nil nil
      (
        chseq->tension-text
        gauss-pick-pitches
        letter-tension-poles
        pick-pole-tension
        pole-chord
        pole-chord-seq
        spiral-letters-table
        spiral-svg-string
        tension->letter
        tension-hue
        tension-spiral->svg
        tension-text->list
      )
    nil)


    ("UTILS" nil nil
      (
        /phi
        0-phi-n
        0-phi-n-2
        1/semitone-number-frequency-ratio
        aser
        ceiling-to-multiple
        current-date
        current-date-&-time
        current-time
        diff-min-max
        divide-by-phi
        done-signal
        elapsed-time
        elapsed-time-2
        eval-tree
        find-closest
        find-closest2
        floor-to-multiple
        format-current-date
        format-current-date-&-time
        format-current-time
        frequency-ratio-to-midic
        frequency-ratio-to-semitones
        harmonic-ser
        hex->rgb
        join-string-list
        line-in-pic-interpole
        line-in-pic-spline-interpole
        line-interpole
        line-spline-interpole
        list-ever
        list-maximum
        list-min-max
        list-minimum
        list-sum
        m-limit
        make-equal-temperament-scale
        mtrans
        multi-by-phi
        number-of-pitches
        numbers-xs-ys
        pascal
        phi*n
        pic-bicubic-resampling
        pic-bilinear-resampling
        pic-spline-resampling
        program-start
        program-stop
        random-rgb
        random-value
        remove-chars-from-list-in-string
        rgb->hex
        round-to-multiple
        semitone-number-frequency-ratio
        set-matrix-lmidic
        simple-color-bpf-from-list
        simple-digits
        sinc
        spline-interpole
        t/nil-random
      )
    nil)

))

;==============================================;
;                                              ;
;                 Torun, Polska                ;
;                                              ;
;                     *****                    ;
;                      ***                     ;
;                                              ;
;==============================================;