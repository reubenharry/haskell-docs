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
    default-language: Haskell2010
```

Packages are hosted [here](https://hackage.haskell.org/).