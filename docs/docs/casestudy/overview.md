The following is a walkthrough of a Haskell project for displaying and updating a chess board via the command line:

Example:

update board
which square?
knight
that is not a square. You need to specify a square
a4
options:
    add piece / remove piece
which piece
black knight
which color 
black

The goal of this walkthrough is to show how to write a simple, but non-trivial application in Haskell, using 

- how to read files
- how to handle exceptions
- how to write an interactive program
- how to parse text
- how to implement a domain specific language


    exception handling, common packages (`megaparsec`, `aeson` and `haskeline`)