haskell-lib
===========

Albert Cardona 2014.

Exploring Haskell for high-performance computing, particularly for image processing and for the analysis of neural circuit structure.


Requirements
-------

In Ubuntu 14.04, in this order:

$ sudo apt-get install haskell-platform liblapack-dev libgsl0-dev libopenblas-dev libgtk2.0-dev libpango1.0-dev libglib2.0-dev libcairo2-dev

$ cabal install hmatrix

$ cabal install gtk2hs-buildtools

... then add the ~/.cabal.bin to the $PATH, then:

$ cabal install gtk

$ cabal install plot


Run it
------

$ cd CATMAID

$ ghc -O2 spectral-graph-analysis.hs

$ ./spectral-graph-analysis view /path/to/adjacency-matrix.csv

or

$ ./spectral-graph-analysis filename.pdf /path/to/adjacency-matrix.csv


In the case of 'view', notice that the window that opens is resizable, and automatically resizes the graph within.


The adjacency matrix is expected to be a square matrix similar to:

<pre>
"neurons", "neuron 1", "neuron 2", "neuron 3"
"neuron 1", 10, 2, 4
"neuron 2", 0, 5, 1
"neuron 3", 0, 1, 0
</pre>
