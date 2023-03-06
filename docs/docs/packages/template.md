Template Haskell is a macro system for Haskell[^1], i.e. a way of writing Haskell code to generate Haskell code.

[^1]: A guide for developers on how Template Haskell works can be found here: https://markkarpov.com/tutorial/th.html

!!! Warning

    In some languages, like Lisp, macros provide a central means of generalization, i.e. a way to avoid writing many similar pieces of code and instead just write one.

    In Haskell, however, macros are better thought of as a last resort option, usually for very advanced applications where the generalization capabilities of standard Haskell (polymorphism, typeclasses, generics) are insufficient.

    As such, novice Haskellers should never consider writing their own macros, and only use them rarely, when offered by a library.

## Example

The typical use case of Template Haskell is in a library where it is useful to have a macro that automatically writes some boilerplate code.

For instance, the highlighted code is a Template Haskell macro to generate [lenses](/packages/lens/) for a newly defined type:

```haskell hl_lines="4"
data ChessEntity
  = Rank { _rank :: Int, _file :: Int }
  | Color { _color :: Text }
makeLenses ''ChessEntity
```

This will generate lenses, usable as follows:

```haskell title="repl example"
> chessColor = Color "white"

> chessColor ^.. color

["white"]
```



