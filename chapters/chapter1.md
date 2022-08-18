# Chapter 1

As discussed in the introduction, each chapter of this book centers around a program written in Haskell. You can find the code for chapter 1 [here](/app/Chapter1.hs)


The goal of the chapter is to explain this code entirely. In so doing, we'll introduce the core ideas that make Haskell stand out.

There's a lot in this code that is likely to be unclear if you haven't used Haskell before. Our strategy will be to focus on small simple pieces of the code, and build up to more complex ones, bit by bit.

## How to run this program: GHCI

...

## Immutability

We'll start our explanation with the following lines from `Chapter1.hs`:

```haskell
outputLength :: Int
outputLength = 5
```


The first thing to understand about this value is that it's immutable...

## Types

The double semicolon on the first line above, `::`, indicates a type declaration. In this case, it is a declaration that the type of `outputLength` is an `Int`. This is enforced: the program will not compile if you change line 2 to e.g. `outputLength = "hello"`. 

In Haskell, **every expression has a type**. 


Additionally, **you don't need to explicitly declare the type**. Haskell is capable (in most circumstances) of inferring the type for you. So you could leave out line 1, and the code would still compile.
However, it is good practice to state the type explicitly, for ease of reading.

## Order of declarations doesn't matter

Notice that our program is made up of a series of definitions, of the form `something = something`. The order in which these definitions are made doesn't actually matter.
(You can even separate the type declaration from the definition, although it's conventional to keep them together for ease of use.)


## Functions

## Purity

## Universal quantification

## Types paramterized by other types


## Syntax: `where` and `let`

## Pattern matching

## Laziness

## Higher order functions
 