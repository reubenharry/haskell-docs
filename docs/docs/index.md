# Haskell ([9.2.5](/#haskell-version))

Haskell is a general purpose programming language. It is unique in being both very principled in its design (pure functions only, simple syntax, very expressive types), but also practical (heavily-engineered compiler that can generate fast code).

- Pros: highly modular code, low boilerplate, easy and safe refactoring
- Cons: no manual memory management, some [gaps in ecosystem](/packages/survey/)

Salient properties include:

- [functional](/thinkingfunctionally/hof)
- [pure](/thinkingfunctionally/purity) and [immutable](/thinkingfunctionally/immutability)
- [custom static types](/basics/createdata) and [automatically inferred](/thinkingfunctionally/typeinference) type annotations
- [lazy](/laziness/laziness)


[**Try it online**](https://code.world/haskell#PcIhU_JQliX5KxN8Rh7xIaA){ .md-button .md-button--primary }

[**Get set up**](gettingstarted/overview.md){ .md-button .md-button--primary }


## How to use this guide

If you're totally new to Haskell, we recommend reading through the tabs at the top of the page from the left to the right.

Feel free also to use this guide as a reference. For example, you can search the docs like so if you need a quick example of how to use a certain construction like `case`.

![](/img/search.png)

Ask questions about pages you don't understand via the `Comments` section at the bottom of each page.


!!! Disclaimer
    This documentation is a personal project, and not official Haskell documentation. Contributions are encouraged, by submitting a PR. See here for a [style guide](https://github.com/reubenharry/haskell-docs/blob/main/Contributing.md).

## Haskell Version

This guide is written for series 9 of the Glasgow Haskell Compiler (GHC). Almost all the content is applicable to older versions, but if in doubt, use [GHC 9.2.5](/gettingstarted/versions/#managing-software).
