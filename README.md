# om-cywin

An OpenMusic 8 library by Tomasz Cywiński and Claude (Anthropic).
Tomasz Cywiński is a composer, arranger and orchestrator from Toruń, Poland.

A general-purpose toolbox for algorithmic composition in OpenMusic: note lists and sequences,
MIDI, audio analysis, BPFs, probability, charts, colour LUTs and pictures — and a harmonic model
based on **tension**.

## Contents

| Menu | What is inside |
|---|---|
| LNOTES / LLNOTES / SEQUENCES | note-list tools: cut, tie, quantise, reverse, velocity, tempo |
| MIDI / OBJECTS / INSTRUMENTS | MIDI events, score objects, harp pedals, orchestra and choir ranges |
| AUDIO | Goertzel analysis, sonogram of a sound, simple sine generator |
| BPF | random deviation, BPF sums, BPFs from text |
| PROBABILITY / SERIES / CONSTANTS | weighted random choice, Pascal's triangle, series, phi and tempered ratios |
| CHARTS | PIE-CHART and RADAR-CHART classes with SVG export |
| COLOR-LUTS / ALFABET-LUTS / LUT-VIEW / PICTURES | .cube colour LUTs (HLG), LUT preview, 16-bit HDR PNG |
| CONVERSIONS / TENSION-SPIRAL | tension matrix, chord ↔ tension, sound → chords, chord-seq → sound, tension wheel |
| FILES / UTILS | helpers |

Every box has an English description; the reference pages are in `reference-pages/index.html`
([online](https://cywinskitomek-gh.github.io/om-cywin/reference-pages/index.html)).

**[Interactive map of the library](https://cywinskitomek-gh.github.io/om-cywin/map/)** — every box, what it calls
and what calls it, with links to its reference page and source. Use it as a help page.

The icons (`resources/icon/9100–9122`, without 9115) are the library's own set, one per menu group; their SVG sources are in `extras/icons-svg/`.

## Tension in short

The tempered fifth is about 1.955 cents narrower than a pure fifth. Only octaves sound pure; every
twelfth adds that small deviation. A chord built from more twelfths is more detuned and gets a higher
**tension**. The library builds a matrix of harmonic fields (tension 0 … 285 for the piano range) and
lets you go both ways:

- chord → tension (`lmidic-to-tension`, `sound->tensions`),
- tension → notes (`tension-to-lmidic`, `pole-chord`, `pole-chord-seq`, `tension-key-sets`).

The model is used, among others, in the author's cycle *Alfabet Toruński*
([tomaszcywinski.pl/alfabet](https://tomaszcywinski.pl/alfabet/)); the same page has a
[tension calculator](https://tomaszcywinski.pl/alfabet/#kalkulator) you can try in the browser.

Note names follow the OpenMusic convention: C3 = MIDI 60 = 6000 midicents.

## Authors

- Tomasz Cywiński — concept, the tension system, code
- Claude (Anthropic) — code, co-written with Tomasz Cywiński

## Installation

1. Download this repository and rename the folder to **`om-cywin 1.0`**.
2. Put it in your OpenMusic libraries folder (e.g. `~/OM/Libraries/`) or add its parent folder
   to the library search paths in OM 8 preferences.
3. Load the library from the Libraries menu (or in a patch). The first load compiles the sources.

Requirements: OpenMusic 8.

## Licence

Copyright (C) 2026 Tomasz Cywiński.

om-cywin is free software: you can redistribute it and/or modify it under the terms of the
GNU General Public License as published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version — the same licence as OpenMusic. See `LICENSE`.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
