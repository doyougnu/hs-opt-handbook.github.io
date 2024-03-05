.. _Perf Chapter:

The Linux perf utility
======================


..
   .. exec:: /code/perf/app/Main.hs
       :process: haskell
       :project_dir: code/perf/
       :with: cabal
       :args: run

.. exec:: code/lethargy/bench/TooManyClosures.hs
   :process: haskell
   :project_dir: code/lethargy/
   :with: cabal
   :args: bench lethargy:tooManyClosures
..

this is the invocation to record page faults by symbol id. I'll have to build a
DWARF'd GHC.

perf record -F 2500 -e 'faults' ./dist-newstyle/build/x86_64-linux/ghc-9.2.4/perf-0.1.0.0/x/perf/build/perf/perf

this invocation reports by line number (but it doesn't really work)
perf report --stdio --no-children -g none -s srcline

tomorrow find weak pointers!

References
----------

*  `phoenixNAP IT's tutorial <https://sandsoftwaresound.net/perf/perf-tutorial-hot-spots/>`__

*  `The Linux Kernel Wiki <https://perf.wiki.kernel.org/index.php/Tutorial>`__

* `Brendan Gregg's One-liners <https://www.brendangregg.com/perf.html>`__

  * `This <http://sandsoftwaresound.net/perf/perf-tutorial-hot-spots/>`__ very
    in-depth blog post using perf on a raspberry pi.
