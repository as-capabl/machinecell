machinecell
===========

Provides arrow combinations for Machines.

Limitation
---------------
For all machines used in this library,  no `Yield` can appear before any `Aawit`s. This may be corrected by changing the signature of ProcessA from `ProcessA (forall t d. ProcessA_ a (c, d) t -> ProcessA_ a (b, d) t)` to `a (ProcessA_ a (c, d) t) (ProcessA_ a (b, d) t)`

Running
---------------
    git submodule init
    git submodule update
    cd test
    make

See example of test/Main.hs
