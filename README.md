**Under :construction: !!**

# Introduction to Haskell

This is the github repository for [an introductory guide to learning Haskell](https://haskell-docs.netlify.app/).

## Goals

The goal for this project is that it serves as a go-to resource for newcomers who have programming experience in a language like Python. It should contain all the information needed to write basic Haskell (roughly the content of "Learn You a Haskell"), but in the style of a reference guide.

With that in mind, it should be:

- **free and online**
- **maintainable**: mostly just markdown, easy for anyone to update via a PR
- **commentable**: easy for a reader to provide feedback where they don't understand
- **ergonomic**: pleasing to look at and use, good search, easy navigation
- **friendly**: clear usage examples of code, FAQs and gotchas, pointers to other resources, unambiguous set up instructions for Haskell, contains an example project with best practices
- **concise but not too concise**: avoids long prose where possible 

`Mkdocs Material` is a great platform for these goals. It's very easy to maintain (click Edit icon on any page to submit PR), is designed to look nice and display information well (info boxes, code comments, tabs and search for navigation), and it can be connected to Google Analytics to allow users to write feedback where they get stuck.

## What's wrong with existing resources?

|                    | Wiki               | LYAH               | RWH                | LHBG               | These Docs         | Various Books      |
| ------------------ | ------------------ | -----------------  | ------------------ | ------------------ | ------------------ | ------------------ |
| Free               | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                |
| Community editable | :white_check_mark: | :x:                | :x:                | :pushpin:          | :white_check_mark: | :x:                |
| Beginner friendy   | :x:                | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| Up-to-date         | :x:                | :x:                | :x:                | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| Looks nice         | :x:                | :white_check_mark: | :x:                | :shrug:            | :white_check_mark: | :white_check_mark: |
| Supports comments  | :x:                | :x:                | :x:                | :x:                | :white_check_mark: | :x:                |



There are many great resources, but:

- many are out of date and are hard to update
- many are not free 
- many are not online
- most are long-form books or lecture notes, not a concise "cheatsheet"

Of these, the first is the most important: making an *OK* intro guide to Haskell that is very easy for many people to continuously improve, edit and maintain, and which is easy to collect user feedback about, seems preferable to making a really good intro guide which will never be edited, improved or maintained.


## General "philosophy":

- avoid digressions: e.g. no history of Haskell, or discussion of the reason for the `String` type 
- show, don't tell: prefer code examples, visual cues, highlighting and unfoldable comments to paragraphs of prose explaining an idea
- don't be exhaustive: avoid being an exhaustive reference API like the Haskell report; if you believe a feature can be understood easily by the average Python coder without explanation, don't explain it. Explain currying, but don't bother explaining whether `[0..9]` includes `9`, since a user can easily figure that out. Don't even explain `if` statements.
- prefer non-numerical examples: e.g. instead of examples about calculating primes, give examples involving chess. (Motivation: only having numerical examples suggests misleadingly that Haskell is not a general purpose language or is hard to use practically)
