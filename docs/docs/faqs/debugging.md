How do I debug, since `print` statements change the type?

Because Haskell is pure and functional, the approach of debugging by adding `print` statements is not needed as often. Instead, you can inspect the subcomponents of your program and their types in isolation, and check their behavior in the repl. 

However, when you do need `print` statements, you can use `trace` from `Debug.Trace`, like so:

todo

and traceM like so

todo