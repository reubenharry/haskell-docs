# Introduction to Haskell

This is the github repo for [a beginner level guide to learning Haskell]().

The goal for this project is that it serves as a goto resource for newcomers to 

- understand the basic world view and concepts of Haskell
- get set up with the tooling


Despite the existence of many beginner tutorials, I think it's still pretty hard as a beginner to understand *why* Haskell is really great and *how* to write code in it. 

In terms of online, free, high quality materials, *Learn You a Haskell* is still the best thing out there for newcomers, but it has some flaws:

- not up to date, especially re. use of Haskell Language Server and other tooling
- has a particular aesthetic, whereas a more neutral and matter of fact presentation might be useful for beginners
- a little hard to learn how to do anything practical

What is useful 
    is a reference in the sense of 
        being able to find out quickly 
            descriptions and examples of common syntax like `let` or `case`
    but not in the sense of being an exhaustive compendium of all language features.

Being a book in the sense of providing explanations and context, but not long stretches of prose.





Goals of this documentation and I thought it was worth making in a crowded playing field:

- easy to contribute: if there's something you don't like, just make a PR here. It's all just markdown files!
- aimed at newcomers to Haskell, but more concise than a book and less concise than a reference API



# stuff

Haskell is a wonderful language. It makes it simple to write elegant, bug-free and modular code that is easy to refactor and reason about.
    makes writing large complex codebases simple, by emphasizing modularity

    propaganda:
        impulse to refactor code and abstract a pattern, to avoid duplication:
            this happens much more in Haskell than it can in other languages

This book is intended to be a joint effort. If you would like to make a contribution, simply make a PR.

<!-- 
# A proposal for a centralized Haskell learning resource

I think that ease of learning is a serious barrier to more widespread adoption of Haskell as a language. 

While there are many learning resources out there, most have at least one of the following drawbacks:

- Behind a pay barrier, and therefore unlikely for a beginner to invest in who is not convinced of the value of Haskell
- Out of date
- Hard to know if its a suitable option, as a beginner

I think it would be good to have a single resource, endorsed by the Haskell foundation, with the following properties:

- community editable: i.e. people can make changes and add material by submitting PRs to a github repo
- free
- online: in particlar, runnable code and the ability for readers to ask questions that are visible to subsequent readers about specific parts would be great
- directly available from haskell.org and endorsed as the community standard way to learn Haskell

Really the first point above is the most important: it should be easy for any member of the community to contribute. That way, information can be kept up-to-date, errata can be fixed, and new information can be added.

In addition, some design features I think would be good are:

- book is written in markdown
- book is built around an example Haskell repo, intended to be an exemplar of a Haskell project
- each chapter focuses on explaining a module of the codebase.
   -->