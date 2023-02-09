# Introduction to Haskell

This is the github repository for [an introductory guide to learning Haskell](https://haskell-docs.netlify.app/).

If you're interested in making contribution to the guide, see [here](/Contributing.md) for more information.

```haskell
main :: IO ()
main = putStrLn "That was easy!"
```

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
| Community editable | :white_check_mark: | :x:                | :x:                | :pushpin:          | :white_check_mark: | :x:                |
| Free               | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                |
| Beginner friendy   | :x:                | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| Up-to-date         | :x:                | :x:                | :x:                | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| Looks nice         | :x:                | :white_check_mark: | :x:                | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| Supports comments  | :x:                | :x:                | :x:                | :x:                | :white_check_mark: | :x:                |



There are many great resources, but:

- many are out of date and are hard to update
- many are not free 
- many are not online
- most are long-form books or lecture notes, not a concise "cheatsheet"

Of these, the first is the most important: making an *OK* intro guide to Haskell that is very easy for many people to continuously improve, edit and maintain, and which is easy to collect user feedback about, seems preferable to making a really good intro guide which will never be edited, improved or maintained.


