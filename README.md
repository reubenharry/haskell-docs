**Work in progress!!!**

# Introduction to Haskell



This is the github repository for [an introductory guide to learning Haskell](https://haskell-docs.netlify.app/).

## Goals

The goal for this project is that it serves as a go-to resource for newcomers who have programming experience in a language like Python. It should contain all the information needed to write basic Haskell (roughly the content of "Learn You a Haskell"), but in the style of a reference guide.

With that in mind, it should be:

- **free and online**
- **maintainable**: mostly just markdown, easy for anyone to update via a PR
- **ergonomic**: pleasing to look at and use
- **friendly**: clear usage examples of code, FAQs and gotchas, pointers to other resources, unambiguous set up instructions for Haskell
- **concise**: avoids long prose where possible 

`Mkdocs Material` is a great platform for these goals. It's very easy to maintain (click Edit icon on any page to submit PR), is designed to look nice and display information well (info boxes, code comments, tabs and search for navigation), and it can be connected to Google Analytics to allow users to write feedback where they get stuck.

## What's wrong with existing resources?

There are many great resources, but:

- many are not free 
- many are not online
- many are out of date and are hard to update
- most are long-form books or lecture notes, not a concise "cheatsheet"

It's impossible for any one resource to do everything, but I thought it was possible that there was a "gap in the market" for a clean, clear, concise, example heavy, constantly-maintained guide.


## General "philosophy":

- avoid digressions: e.g. no history of Haskell, or discussion of the reason for the `String` type 
- show, don't tell: prefer code examples, visual cues, highlighting and unfoldable comments to paragraphs of prose explaining an idea
- don't be exhaustive: if you believe some feature can be understood easily without explanation, don't explain it.

Note: these things are all fine, and appear to good effect elsewhere