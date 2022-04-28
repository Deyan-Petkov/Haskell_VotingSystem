##Coursework for IN3043 Functional Programming



My program consists of 5 voting systems:
* First Past The Post
* Single Transfer Vote
* Alternative Voting System
* Borda Count Voting System
* Contingent Voting System

There exist many voting systems but most of them have subtle differences. I found
these five as most interesting and challenging.
The similarities in the voting systems allowed me to reuse some of the functions.

The project was mainly intended to produce limited lines of output(only final
results) in order to reduce the use of IO. However after implementing the systems
I decided to improve the IO of the program partly for practicing, partly because
of the nature of the project. Involving more IO required some changes of the
functions and adding new ones.
I successfully improved IO in all of the systems with little adjustments except
in Single Transfer Vote system as it uses recursion twice, which led to repeated
output, and I would also had to change entirely almost all of the functions
related to this voting algorithm as they are tightly coupled, which requires
almost entirely new solution. For this reason Single Transfer Vote system produces
only final output.

All of the algorithms required a lot of data manipulation which really helped me
to dive into Haskell's data structures and related functions. I used Map where
was appropriate to, as it gave me quick access and ordered structure.
As Borda Count is positional voting system I used pure list structures which kept
the votes in original order. This makes Borda Count the slowest algorithm in the
program.

I tried to comment as much as possible each function. For clarity I also left
commented out some of the substeps, that build the final string of expressions.  


The program load the input sequentially for each voting algorithm.
To start the program just call the readVotes function and pass path to one of the
input files as a parameter.
