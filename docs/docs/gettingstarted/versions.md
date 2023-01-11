## Managing software

To change your version of GHC, cabal, or the Haskell Language Server:

```bash
> ghcup tui
```

You should see something like:

![](/img/ghcuptui.png)

This makes it easy to change version of the compiler, the Haskell language server, and cabal (the package manager).

??? stack
    `stack` is (roughly) an alternative package manager. Either `cabal` or `stack` work well, but this documentation defaults to `cabal`.

## Installing packages

To add packages, include their names to the package list in your `.cabal` file:

```cabal hl_lines="7 8"
library
    exposed-modules:  MyLib

    build-depends:    
        base ^>=4.16.3.0,
        text,
        NEW PACKAGE,
        ANOTHER NEW PACKAGE
    hs-source-dirs:   src
    default-language: GHC2021
```

Packages are hosted [here](https://hackage.haskell.org/).

## Extensions

Haskell allows new code features to be enabled/disabled with lines like:

```hs
{-# LANGUAGE OverloadedStrings #-}
```

at the top of files. 

We recommend setting the default-language to `GHC2021`, which enables a set of compatible and standard extensions. 

!!! Warning
    `GHC2021` will only work for GHC 9 and greater. Otherwise replace with `Haskell2010`.

```cabal hl_lines="8"
library
    exposed-modules:  MyLib

    build-depends:    
        base ^>=4.16.3.0,
        text,
    hs-source-dirs:   src
    default-language: GHC2021
```