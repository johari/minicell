# Revision history for Minicell

## Unreleased

* `=LAMBDA(arg1, arg2, ..., body)` or `=FUN`,
* Tuples inside cells,
* Collapsed lists inside cells,
* Experiments with `csound-expression`.

## [1.1.8] -- 2021-01-08

### Added

* Added some audio related formulas to `Piet.DSL.Audio`:
    * `=AUDIO(url)`, which downloads the file and creates a new `EAudio` value,
    * `=AOLAY(audio1, audio2)`,
    * `=ACONCAT(audio1, audio2)`,
    * `=AWAVE(audio)` which plots the waveform of the audio via `gnuplot`,
    * `=ACUT(audio, start, duration)` or `=ACROP(audio, start, duration)`,
* Added `youtube-dl` support for fetching audio from YouTube and SoundCloud,
* Serve blobs via HTTP (instead of serializing via `data` URLs),
* Added `EDouble`.

## [1.1.6] -- 2021-01-06

### Added

* Added some audio related formulas to `Piet.DSL.PDF`:
    * `=PDF(url, page)` that downloads a PDF.
    * `=CROP(img, x0, y0, width, height)`.
    * `=MONO(img)`.

## [1.1.0] -- 2021-01-06


### Changed

* I started using `shake` in the codebase and it's rocking my world!! :)
* Ported some of the rules in the `Makefile` to shake.

### Removed

* Removed the old mechanism to cache PDF results and spinning up system commands.

---

At this stage, I think it's a good strategy to have Minicell versions follow
the `<year>.<month>` format. For example, today is January 8th 2021, and I think
Minicell is ready for a 1.1.8 release.

---

I'm summarizing the existing features as `0.XX.YY`.

## [0.12.31] -- 2020-12-31

### Added

* Support for markdown-style raw text in cells (See `Piet.DSL.FAM`).
* Some support for calling into Python code. (very incomplete)
* Added an incomplete support for spilling.

## [0.0.2] -- 2019-1-05

A 10-minute video demo of this revision is available on [YouTube][1].

[1]: https://www.youtube.com/watch?v=NZo4cGzcSK0

### Added

* Added `=UNIXEPOCH()`,
* New formulas, now in `Piet.DSL.Graphics.Shapes`:
    * `=SHAPE(shapeName)`,
    * `=PAINT(shape, color)`,
    * `=TURN(shape, a, b)`,
    * `=HCONCAT(shape1, shape2)` and `=VCONCAT(shape1, shape2)`.
* New formulas for `Piet.DSL.Graphsheet`:
    * `=X(range)`,
    * `=SP(graph, src, dest)`,
    * `=MF(graph, src, dest)`.
* Switched to a Haskell-based `eval`,
* Support for uploading image files to a cell via drop on cell,
* Front-end now pulls values from backend, instead of evaluating formulas itself,
* {S,Des}erialization via JSON in Haskell and Elm.
* Basic Graphviz rendering in backend.

## [0.0.1] -- 2018-08-22

* First version. Released on an unsuspecting world.
