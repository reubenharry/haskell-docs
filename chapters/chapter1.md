# Chapter 1

As discussed in the introduction, each chapter of this book centers around a program written in Haskell. You can find the code for chapter 1 at

```haskell

outputLength :: Int
outputLength = 5

inputList :: [Double]
inputList = [1..20]

result :: [a] -> [a]
result =
    fmap snd 
    . take outputLength 
    . filter (not . even . fst) 
    . zip [1..] 

result = takeEveryOther $ words ""

...
```

The goal of the chapter is to explain this code entirely. In so doing, we'll introduce the core ideas that make Haskell stand out.

There's a lot in this code that is likely to be unclear if you haven't used Haskell before. Our strategy will be to focus on small simple pieces of the code, and build up to more complex ones, bit by bit.

## Immutability

We'll start line 2, which defines a value `outputLength`. 

The first thing to understand about this value is that it's immutable. `outputLength` is the integer `10` and cannot be updated at any point during the evaluation of the program. Understand line 2 to mean: `outputLength` is a name for the number 2. This contrasts to procedural languages, where `a = b` tends to mean "set `a` to have the value `b`".

## Types

The double semicolon on line 1, `::`, indicates a type declaration. In this case, it is a declaration that the type of `outputLength` is an `Int`. This is enforced: the program will not compile if you change line 2 to e.g. `outputLength = "hello"`. 

In Haskell, **every expression has a type**. 


Additionally, **you don't need to explicitly declare the type**. Haskell is capable (in most circumstances) of inferring the type for you. So you could leave out line 1, and the code would still compile.
However, it is good practice to state the type explicitly, for ease of reading.

## Order of declarations doesn't matter

Notice that our program is made up of a series of definitions, of the form `something = something`. The order in which these definitions are made doesn't actually matter. You can confirm this for yourself, for example by moving lines 
    above 
(You can even separate the type declaration from the definition, although it's conventional to keep them together for ease of use.)

We will next examine lines...

## Functions

## Purity

## Universal quantification

## Types paramterized by other types


## Syntax: `where` and `let`

## Pattern matching

## Laziness

## Higher order functions
 