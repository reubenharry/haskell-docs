Library API documentation is typically autogenerated from code comments, to create online interactive docs. 

## Example

As an example, `gloss` is a package for 2D animation and games. [Here are its API docs](https://hackage.haskell.org/package/gloss), where you can find the following package list:

![Gloss on Hackage](/img/hackage.png)

For well-maintained packages, you should expect to find some explanation of the package's goals and uses either on this front page or in the top-level module (here `Graphics.Gloss`).

!!! Warning

    Many published packages are experimental code, not intended for serious use. To get a sense of which packages to use, see [this guide](/packages/survey/).

## Hackage and Stackage 

There exist two major package repositories for Haskell, [Hackage](https://hackage.haskell.org/) and [Stackage](https://www.stackage.org/), with the main difference that Stackage provides a stabler subset of Hackage, which is more extensive. 

## How to read Haskell documentation

Haskell's expressive types are usually very helpful in understanding how to use a library. For example, the following function appears near the top of [the docs in the top level module](https://hackage.haskell.org/package/gloss-1.13.2.2/docs/Graphics-Gloss.html):

![Gloss docs](/img/gloss.png)

From the type signature, we see that we get a runnable process (i.e. a value of type `IO ()`) if we supply a few arguments, like `Color` and a function `#!haskell Float -> Picture`.

We can then understand what `Picture` is by following the link, to see its definition:

![Picture](/img/picture.png)

## Default libraries

Haskell's [Prelude](https://hackage.haskell.org/package/base) library is automatically imported into every module, and includes many familiar functions and types, like `take` and `Maybe`. 

!!! Note

    `Prelude` is sometimes replaced by a different base library in large projects, since it includes legacy features and outdated design decisions. For example, the function `head` is *unsafe* in the sense that it throws a hard error when you take the head of an empty list: `head []`.

    A good modern alternative can be found [here](https://kowainik.github.io/projects/relude).
