# Connect4-AI
ASCII connect4 game written in Haskell that can be played in GHCI against a minimax depth 5 AI opponent with heuristic scoring. Arbitrary board sizes and connect requirements supported i.e. n x m connect k.

# Requirements
GHC compiler and Parallel Control Strategies Haskell package installed (allows multicore tree search, greatly speeds up AI thinking time)

# Compilation & Running
    $ ghc --make -threaded Human.hs  
    $ ./Human -N4  

where -N4 is used to specify the number of cores your machine has (i.e. in this case four).
