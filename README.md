GOLGL - Game of Life GL
=======================

GOLGL is a training project for learning Haskell.

It does work however, and you can play nicely with the 
simulation.

How to build
------------

    ghc GOLGL.hs

You might have to install some libraries in order to
compile the project. Some of which I can remeber are

    cabal install GLUT

How to use
----------

You can control the simulation with the 't' key. When
the simulation is paused, you can click and toggle living
cells with your mouse. There are also a couple of
patterns predefined and loadable by keys, look into
`GOL.hs` and `Bindings.hs` for those.
