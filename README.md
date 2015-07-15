machinecell
===========

Arrow based stream transducer.

Description
---------------

As other iteratee or pipe libraries, machinecell abstracts general iteration processes.

Here is an example that is a simple iteration over a list.

```
>>> run (evMap (+1)) [1, 2, 3]
[2, 3, 4]
```

In above statement, "`evMap` (+1)" has a type "ProcessA (-\>) (Event Int) (Event Int)",
which denotes "A stream transducer that takes a series of Int as input,
gives a series of Int as output, run on base arrow (-\>)."


In addition to this simple iteration, machinecell has following features.

* Side effects
* Composite pipelines
* Arrow compositions
* Behaviours and switches

See [Control.Arrow.Machine](https://hackage.haskell.org/package/machinecell/docs/Control-Arrow-Machine.html) documentation.



Comparison to other libraries.
---------------

Some part of machinecell is similar to other stream transducer
libraries, namely pipes, conduit, or machines. machinecell can be
seen as a restricted variation of them to one-directional. But
additionally, machinecell supports arrow compositions.
Bidirectional communications can be taken place by ArrowLoop
feature.

Rather, there are several other arrowised stream transducer
libraries. streamproc shares the most concept to machinecell. But
actually it has a problem described later in this post. Machinecell
can be said as "Streamproc done right."

auto is a brand-new arrowised stream transducer library. Compared
to it, machinecell's advantage is await/yield coroutines, while
auto's one is serialization.



Motivation and background
---------------

"Generalizing monads to arrows," The original paper of arrow calculation
mentions a kind of stream transducer, which later implemented as streamproc.

http://www.cse.chalmers.se/~rjmh/Papers/arrows.pdf


And other people propose instance declarations of Arrow class for several existing stream processors.

http://stackoverflow.com/questions/19758744/haskell-splitting-pipes-broadcast-without-using-spawn

https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/coroutines-for-streaming/part-4-category-and-arrow


But actually, there is a problem argued in this post.

https://mail.haskell.org/pipermail/haskell-cafe/2010-January/072193.html


The core problem is, while arrow uses tuples as parallel data
stream, they cannot represent a composite streams if they carry
different numbers of data in parallel.

To solve this problem, some arrow libraries restrict transducers to
one-to-one data transformation. Yampa and netwire does so, as
mentioned in above post. And auto also takes this approach.

Machinecell's approach is different, but simple too. The key idea
is wrapping all types of data stream into a maybe-like type. Then
even tuples can represent different numbers of data, by inserting
appropreate number of 'Nothing's.

Furthermore, I identified the maybe-like type as the 'Event' type,
which appears in Yampa and netwire. Then I successively implemented
several arrows of Yampa and netwire.

API names come from stream libraries are named after machines',
while ones from FRPs are after Yampa's. Now, machinecell may be
seen as a hybrid of machines and Yampa.
