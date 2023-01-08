Getting set up with Haskell is very easy, here is the recommended route:

## Step 1 (installation)

Download the Haskell installer, [GHCup](https://www.haskell.org/ghcup/).

To check that this was successful, enter the comand `ghci` into your terminal. This will open a Haskell repl. where you can execute Haskell code (much like in Python). 


## Step 2 (make a project)

The repl is useful for evaluating simple expressions, and in-project debugging, but we recommend setting up a project as a first step to learning Haskell.

In a directory of your choice, create a new Haskell project by running:

```bash
cabal init --interactive
```

or if you prefer, clone [the repository from these docs](todo), which is itself a Haskell project.

This will create a file ending in the suffix `.cabal`, which is where you will specify package requirements and other build details. For instance:

```cabal
cabal-version:      2.4
name:               haskell-book
version:            0.1.0.0
maintainer:         reubenharry@gmail.com

extra-source-files:
    CHANGELOG.md
    README.md

library
    exposed-modules:  MyLib

    build-depends:    
        base ^>=4.16.3.0,
        text
    hs-source-dirs:   src
    default-language: Haskell2010

executable haskell-book
    main-is:          Main.hs
    build-depends:
        base ^>=4.16.3.0,
        haskell-book

    hs-source-dirs:   app
    default-language: Haskell2010
```

To run the functions in the project, first enter the Haskell repl (now in a project specific fashion):

```bash
cabal v2-repl
```

And then do:

```haskell
> import MyLib
> someFunc
```

To compile your project into an executable:

```bash
cabal build
```

which you can run with:

```bash
cabal run haskell-book
```

## Step 3 (set up the Haskell Language Server)

We highly recommend using the *Haskell Language Server*. This is an IDE which runs in VSCode (and other platforms).

In VSCode, activate it by installing the *Haskell* extension:

![](/img/hls.png)

This will make learning Haskell much smoother, by underlining type errors, showing inferred types by mouse-over, adding type signatures automatically, and adding language extensions and imports when needed.

## Step 4 (Hoogle)

Hoogle is a search engine for Haskell, where you can look up by type. This is accessible [at this site](https://hoogle.haskell.org/), but we recommend installing the `hoogle-vscode` extension. 

Open with ++option++ **h**, and use like so:

![](/img/hoogle.png)
