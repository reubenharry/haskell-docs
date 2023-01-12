# Contributing

This guide is designed with easy of community contribution in mind.

## Process

The guide is written with https://squidfunk.github.io/mkdocs-material/getting-started/, and consists of markdown files (in docs/docs/). To make an edit, modify a fork of the repo (either by changing an existing markdown file or by adding a new ones) and submit a PR.

If you feel like you'd like to be a more serious contributor and/or to be involved in reviewing changes, please reach out, and I can add you to the repo.

### Review

Currently, I will review contributions. Likely feedback:

- wording or formatting suggestions
- request for more/less detail
- suggestion of `MkDocs` features to use.


## Style Guide

The best way to get a sense of the style is to browse https://haskell-docs.netlify.app/.  

The following are some high level principles.

### Who is the audience? 

The audience is someone who has been exposed to Python (or similar). The audience is *not* a complete newcomer to programming.

### Be concise

Try to include only information that, if absent, would impede this person's understanding. 

It is ok if features of Haskell are either a) never mentioned, or b) mentioned but never explained, **as long as** these features would be reasonably obvious to the target audience.

For example, little if any explanation of `if` statements is needed, since these are unlikely to trip up a newcomer when they encounter them in Haskell. `case` statements, on the other hand, require explanation.

Similarly, don't bother explaining whether `[0..9]` includes `9`, since a user can try for themselves. But do explain that `[..]` is infinite, and explain laziness, since that is likely to be unfamiliar. Parenthetically mention Python's iterators for comparison.


### Use visual cues

Use [boxes](https://squidfunk.github.io/mkdocs-material/reference/admonitions/) to put parenthetical information. For example, boxes named `Tip`, `Note`, `Warning`, or `Gotcha` can be used to highlight a nuance or a complexity without interrupting the flow of the documentation.

Cross-reference heavily: where possible when mentioning a construction, supply a link to the section of the docs which mentions it. Links should be of the form: `/folder/#section-name-in-lowercase-with-hyphens`, e.g. `/basics/types/#universal-types`.

### Show, don't tell

Where possible, illustrate an idea with a demonstration in ghci. You can begin the codeblock with " ```hs title="repl example... ". 

For longer code examples, where ghci is too restrictive, make sure to include all necessary imports and extensions. 

Add [code comments](https://squidfunk.github.io/mkdocs-material/reference/code-blocks/#adding-annotations) with " -- (1)!".

Use highlighting and line numbering where useful.


### Prefer non-numeric examples

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

## Suggested changes

If you want to contribute, but are not sure where, here are some suggestions:

- search for "under :construction:" in the repo. These are places that have not yet been completed.
- look at the issues for the repo

