# PointLib

The repository acts as a hub for sketches, routines and algorithms developed for the project [Patterns of Intuition - POINT](http://point.kug.ac.at/) at the Institute of Electronic Music and Acoustics (IEM) Graz.

## composer_ot branch

Requires the JaCoP solver library. Place `JaCoP-3.2.jar` (download [here](https://sourceforge.net/projects/jacop-solver/files/JaCoP-3.2/)) in the `lib` directory to compile.

## quick manual

Chord sequences can be edited by double-clicking on an individual chord of a chromosome. This toggles between unselected and selected chord. If no modifier is pressed, the chord is selected as "good" and shown in blue. Good chords are frozen and will not change in the iteration. Successive good chords will not be broken up during cross over. If the Alt key is held down during double-click, the chord is selected as "bad" and shown in red. Bad chords will be preferred as cross-over points (the cut will be located _right before_ that chord).
