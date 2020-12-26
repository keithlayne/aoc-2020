# AoC 2020

This year I decided to use Racket. I have fond memories of using PLT Scheme
in school, and Racket is AFAICT the actual current version of that
implementation.

If you happen to look over this code, feedback is welcome. I'm not gonna
change anything at this point, but I'm happy to learn.

The main difference between the code in it's current state and what I used to
solve the problems is some cleaning up and refactoring to be less gross. The
exception is for day 23, which I struggled with. The original version worked
and ran in ~75 seconds, but was pretty terrible, and when I thought of a
better way to do it I went back and implemented it.

## Takeaways

1. I feel like I barely scratched the surface of Racket. It has so
   much..._more_ than I ever learned about it as a student.
2. The docs were good, but I struggled with discoverability of functions in
   the standard library. I kept coming across stuff in the docs that I had
   needed for previous days.
3. I didn't write any macros, which is disappointing.
4. Performance was just fine. The bottleneck was always me.

## Running

If for some reason anyone wants to run these, just make sure `racket` is in
your path and run i.e. `./run 23` in the repo root.
