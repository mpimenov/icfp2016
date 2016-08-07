This is the writeup for the ICFP Programming Contest 2016 by
team MIPT Lambda.


1. Contest writeup.

* Team structure

  Same as last year, there were two of us, one moderately
  exposed to Haskell and another quite proficient in it.
  We used one machine running Ubuntu and one running Mac OS X.

* Language choice

  In fact we chose to use Haskell before the contest started
  but we did not regret the choice:

  -- The need to deal with large rational numbers seemed like
     an unnecessary complication but at least it instantly killed
     the idea to use Boost.Geometry or similar libraries. In fact,
     we do not know of an open sourced library that would suit
     us well for this contest (and we expect that other teams either
     wrote all the geometry code from scratch as we did or used
     their templates from ICPC/TopCoder/Codeforces style contests).

  -- All the foldings, transformations, reversibility etc. felt
     nicely to be written in a functional language.

  -- We had no prior experience in solving hard geometrical tasks
     in Haskell, so it was a good chance to practice.

  -- A functional language seems appropriate to write a contest
     with such a name in.

  We had several small scripts in other languages that turned
  out to be not that useful in the end.

  A special shout-out goes to the Haskell Tool Stack (http://haskellstack.org)
  that made our contest experience much more comfortable than
  the last year's where we used cabal and had to fight some
  issues with the versioning of libraries.

* Lightning round

  The contest started at 3:00 AM for us and we decided to skip
  the lightning round. We had some coding done in the first day, though.

* Visualizer

  We started off with writing a simple renderer for the problem
  which -- we believe -- is almost always a good thing to do first
  in marathon-style contests where the organizers do not provide one.

  To our disappointment, we could not build GLFW that we used last
  year so we chose GLUT instead.

  The renderer can show the silhouette, the skeleton, and the results of
  the foldings that our solution produces one by one triggered
  by the pressing of the 'W' key.

  The silhouette is moved to the (0, 0) and a coordinate grid
  is shown too. Later, we started drawing the convex hull of the
  initial silhoutte.

* Solution overview

  The immediate idea was to learn how to fold convex shapes
  and approximate the silhouette by its convex hull. Sadly
  enough, that's pretty much all we managed to write in the end,
  together with translations and rotations, of course.

  If the silhouette does not fit in the inital square, we
  tried to rotate it and translate somehow but this code did
  not end up in the final solution.

  We toyed with the ideas of writing SA/beam search using the
  skeleton information but it turned out to be too time-consuming.
  For one, the resemblance is pretty hard to compute exactly
  and we settled on computing it approximately (build a grid and
  check whether each square is completely covered by one of
  the polygons or not) but we did not have good use for it.

  We also watched some lectures on origami by Erik Demaine
  (http://courses.csail.mit.edu/6.849/fall10/lectures/L02.html).
  The lectures are fine but they didn't help much (other than
  strengthening our conviction in the hardness of the task).
  Neither did our prior experience to the Paperama game do
  any good: for writing the solution, correctness was much
  more needed than shortness in the contest task, and for submitting
  the problems, we thought it unfair to submit the levels from
  the game.

* Using the scoreboard

  We did not use most of the information provided in the snapshot
  but there is one thing worth mentioning here. The information
  about resemblance results for the submitted solutions made
  it clear that even in the third day of the contest the distribution
  of the solutions was roughly as follows:

  -- 2-3 teams that wrote something nontrivial (1 team with something
     highly nontrivial closer to the end of the contest).
  -- 15-20 teams that wrote exactly the same solution as we did.
  -- Others.

  We would like to thank the organizers here because what
  at the first glance looked like a problem to be solved by isolated
  non-interacting teams made up for quite a real-time competition.

  We also had a persistent feeling that the problem was harder to approach
  than the last year's one and that fewer teams would keep pushing it till
  the end of the contest. The scoreboard seemed to confirm it.

* Submitting the solutions

  This part turned out to be more important than we had expected.
  The restriction on the submission rate, on the one hand, added
  unneeded complexity but, on the other hand, provided the constant
  feeling of progress, liveliness and interactivity (which we probably
  could do without).

  We submitted several small tests by hand and then settled for
  writing an interface that would download a problem, solve it and
  send the solution back. We basically used what was suggested in
  the API documentation by running curl from Haskell.

  We manually ran the tool nearly every hour of the third day.

* Submitting our own problems

  The simple idea that you do not actually need to know how to solve
  your own problems dawned upon us only too late in the contest.
  We did not really care about submitting the problems for the
  first two days and had time to think about them only near the
  end of the third day. Our estimation is that we lost about
  50000 points here.

  So what would be the right strategy for submitting the problems?
  Surprisingly enough, submitting sample #0 over and over again might
  result in a positive (but negligible) change in your score but we
  did not bother to do so (we did not think about this effect of the
  scoring formula either, but it still looks stupid even now).
  But once you have a visualizer, you can do better! It took us
  less than half an hour to set up a simple interface that would read
  an array of lines to be used as folds and simulate the process
  step by step writing the solution to stdout after every folding.
  So we could stop at any time and copy-paste the solution to the
  web form. We used another half an hour to generate some pseudorandom
  lines to build a starting figure and then manually fold it into
  something that we thought would be hard. Ideally, we would want
  to build a non-convex shape that had a much smaller area than its
  convex hull and had one or two holes in it but we didn't do the holes
  part in the end.

  We submitted only two problems one of which was solved (with
  resemblance 1.0) by 9 teams and the other by 1 team. The fact
  that we submitted them near the end of the contest is
  irrelevant because all the top teams seem to have been sitting
  there till the last minute (or at least had their bots running).

  We also had an idea to generate a large Pythagorian triple and use
  it to rotate the basic square hoping that most teams won't handle
  rotations well enough but we dismissed it after having written
  our own code for dealing with rotations because it turned out to
  be simpler than we had thought.


2. Information about the project.

  The project has the following structure:

  + core/             The main solution code.

  + input/            A list of the problems provided by
                      the organizers. We used them mostly during
                      the lightning round and to tune the renderer.

  + renderer/         The visualizer code.

  + solver/           A binary to tie up the libraries from core/
                      to produce solution descriptions.

  + submitter/        The code that interacts with the game server.

    submissions.txt   The file containing most of the server feedback
                      to our submissions (added after the end of the contest).


  The project is built with stack so there are some configuration files
  in the root directory.
