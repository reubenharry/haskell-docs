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
  