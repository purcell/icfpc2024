# ICFP Contest 2024 solo entry, team sanityinc

## About this year's contest

There were several sub-challenges, each with a group of problems to solve. Even downloading those problems required
implementing an evaluator for a terse lazily-evaluated programming language that uses lambda calculus, "ICFP".

Thereafter, for each sub-challenge, the contestant needed to:

1. Download ICFP programs that, when run, produce problem definitions (ie. the problems are encoded as programs)
2. Solve those problems, with an completely independent solver
3. Upload an ICFP program in the special language that would evaluate to the solution when the organisers run it on their server

## What I managed, and failed at

Firstly, I had a stupid little bug with my evaluator that caused some problem definitions to not even execute.
Not for the first time, I therefore failed to score any solutions within the first 24 hours, doh!

There were several rounds of problems, and I only did the first, "lambdaman": each problem definition in that set generates a
big maze, and you need to provide a sequence of moves that would visit every square. So that’s already a bit tricky. I
used [support in the `ocamlgraph` library for calculating Minimum Spanning Trees](https://backtracking.github.io/ocamlgraph/ocamlgraph/Graph/Kruskal/index.html)
to find an efficient network around the maze, then walked it straightforwardly from the starting point, doubling back when
necessary. This produced sequence of steps that were relatively efficient. (I'd used the `fgl` library in
Haskell for similar things previously.)

Now, the maze solution you submit is an ICFP program, and not the raw move sequence, and you get a better score when that program is shorter,
so you can benefit from compressing that sequence into a program, e.g. using run-length encoding, to submit it. I wasn't
pre-loaded with lambda-calculus skills, despite my time in the Haskell community, so I didn't implement RLE,
but instead compressed the moves using a table of move sequences that I thought might occur often, e.g. "UUUUUUU" (Up Up Up...).

Even so, my evaluator wasn't efficient enough to decode the lambdaman21 problem definition.

This was a terrible year to be a solo competitor, as the four challenges (lambdaman, spaceship, 3d, efficiency)
could have been parallelised among team members.

Nevertheless, it was an enjoyable weekend, and I learned how to use ocamlgraph, as well as about beta-reduction
in the lambda calculus. OCaml was excellent once again: lots
of feedback from the compiler, robust sum/product types, excellent performance and fast build/development tooling.
Plus you can throw in print statements whenever you like, like a savage.

Thanks to the organisers for their obvious hard work in putting this contest together!
