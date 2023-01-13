# Contributing

This guide is designed with easy of community contribution in mind.

## Process

The guide is written with https://squidfunk.github.io/mkdocs-material/getting-started/, and consists of markdown files (in docs/docs/). To make an edit, modify a fork of the repo (either by changing an existing markdown file or by adding a new ones) and submit a PR.

If you feel like you'd like to be a more serious contributor and/or to be involved in reviewing changes, please reach out, and I can add you to the repo.

### Review

Currently, I will review contributions. Likely feedback:

- wording or formatting suggestions
- request for more/less detail
- request to add more internal links
- suggestion of `MkDocs` features to use.


# A guide for contributing to the Haskell Guide

## Who is the audience? 

The audience is someone who has been exposed to Python (or similar). The audience is *not* a complete newcomer to programming.

## What should the audience learn?

They should learn the core principles of Haskell (the type system, laziness, functional style, purity, immutability), be comfortable looking at real Haskell code, and understand what the appeal of Haskell is.

This guide isn't intended to teach them everything, but it should ideally be a large stepping stone to engaging with the language more seriously.

The best way to get a sense of the style is to browse https://haskell-docs.netlify.app/.  

The following are some high level principles.

## Style

### Documentation > Book

The Haskell Guide aims to be more like documentation than a book

Rather than having a linear structure, information should be compartmentalized in sections, with heavy cross-referencing. Links should be of the form: `/folder/#section-name-in-lowercase-with-hyphens`, e.g. `/basics/types/#universal-types`.

Use [boxes](https://squidfunk.github.io/mkdocs-material/reference/admonitions/) to put parenthetical information. For example, boxes named `Tip`, `Note`, `Warning`, or `Gotcha` can be used to highlight a nuance or a complexity without interrupting the flow of the documentation.

#### Example

You are writing a code example, but realize that `5 :: Num a => a` could be confusing.

**Do**: write a [code annotation](https://squidfunk.github.io/mkdocs-material/reference/code-blocks/#adding-annotations) or info box that links to the FAQ on numbers.

**Don't**: put an explanation of typeclasses and `Num` in the main body of the text of the current section.

### less > more

Try to include only information that, if absent, would impede this person's understanding. 

It is ok if features of Haskell are either a) never mentioned, or b) mentioned but never explained, as long as either:

- these features would be reasonably obvious to the target audience.
- not knowing these features wouldn't impede a fundamental understanding of the how and why of Haskell


#### Example

**Do**: have explanation of currying and partial application in the guide.

**Don't**: have explanation of `if` statements. At most, include them by example without explanation.

**Do** have explanation that `[..]` is infinite, and of laziness, since that is likely to be unfamiliar. Parenthetically mention Python's iterators for comparison.

**Don't**: explain whether `[0..9]` includes `9`, since a user can try for themselves. 

**Maybe** mention instance signatures and the corresponding extension. But probably don't, because once a user understands enough, they can learn about this themselves.

### show > tell

Where possible, illustrate an idea with a demonstration in ghci rather than a natural language explanation. You can begin the codeblock with " ```hs title="repl example... ". 

For longer code examples, where ghci is too restrictive, make sure to include all necessary imports and extensions. 

Add [code comments](https://squidfunk.github.io/mkdocs-material/reference/code-blocks/#adding-annotations) with " -- (1)!".

Use highlighting and line numbering where useful.


### non-numeric > numeric

When possible, use examples from a domain that isn't too number-centric.

The current default is to use chess-based examples like:

```hs
data Piece = Bishop | Rook

isBishop Bishop = True
isBishop _ = False
```

The case study is also built around a chess example.



Reasons:

- numbers are confusing in Haskell because of typeclasses
- too many numeric examples gives the misleading impression that Haskell is just for number-processing which people may incorrect have from the common motto that Haskell is a "mathematically inspired language"



# Where to contribute

Feel free to contribute to absolutely any part of the Guide, either to edit existing material, or to add more. (Or even to argue for the removal of material).

If you want to contribute, but are not sure where, here are some suggestions:

- search for "under :construction:" in the repo. These are places that have not yet been completed.
- look at the issues for the repo
- look through the docs for [MkDocs Material](https://squidfunk.github.io/mkdocs-material/) and [PyMdown](https://facelessuser.github.io/pymdown-extensions/) to find features that could be useful, and integrate them. E.g. better code highlighting or social cards.

