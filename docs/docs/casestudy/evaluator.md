---
comments: true
---

 

 
```hs title="Evaluator.hs" linenums="1"
--8<-- "../src/Evaluator.hs"
```

1. Throw an error of type `ChessError`. This is what requires the `MonadError ChessError` constraint.
2. `gets display` is the same as `fmap display get`: it accesses the state (of type `Board`), which requires the `MonadState Board` constraint, and applies `display` to it, to return a `Text` value.
3. `get` is the local state. It takes no arguments.
4. Recall that a `Board` value represents the board as a function from a `File` and `Rank` to a square state, so this function is what we need to change, when updating the `Board`.
5. `put` takes an argument and sets the state to that argument.

## Analysis

`MonadError` and `MonadState` are [typeclasses](/typeclasses/overview) for types with the ability to throw errors and mutate local state respectively. See the [monad transformer library (mtl)](https://haskell-docs.netlify.app/packages/mtl/) for more.

With that in mind, read the type signature of `evaluate` as follows: `evaluate` is a function that takes an `Instruction` and returns `Text`, but with the possibility of throwing an error of type `ChessError`, and of changing the state (of type `Board`).

We can think of `evaluate` as taking a synactic description of an `Instruction` and evaluating it into a result. For example `#!hs ReplInstruction "quit"` is a description of an instruction, but `#!hs throwError Exit` is the actually "program" that will quit the repl.

## `evaluate`

=== "original"

    ```haskell
    evaluate :: (MonadError ChessError m, MonadState Board m) => 
        Instruction -> m Text
    evaluate instr = case instr of
        ReplInstruction "quit" -> throwError Exit
        ReplInstruction "display" -> gets display
        Set file rank piece -> do
            (Board boardFunc) <- get
            let newBoard =
                Board
                    ( \f r ->
                        if f == file && r == rank
                        then HasPiece piece
                        else boardFunc f r
                    )
            put newBoard
            return $ display newBoard
        ReplInstruction _ -> throwError $ ReplError "no such instruction"
    ```

=== "with `\case`"

    ```hs hl_lines="3"
    evaluate' :: (MonadError ChessError m, MonadState Board m) => 
        Instruction -> m Text
    evaluate' = \case
        ReplInstruction "quit" -> throwError Exit
        ReplInstruction "display" -> gets display
        Set file rank piece -> do
            (Board boardFunc) <- get
            let newBoard =
                Board
                    ( \f r ->
                        if f == file && r == rank
                        then HasPiece piece
                        else boardFunc f r
                    )
            put newBoard
            return $ display newBoard
        ReplInstruction _ -> throwError $ ReplError "no such instruction"
    ```

=== "with `modify` and `gets`"

    ```haskell
    evaluate :: (MonadError ChessError m, MonadState Board m) => 
        Instruction -> m Text
    evaluate instr = case instr of
        ReplInstruction "quit" -> throwError Exit
        ReplInstruction "display" -> gets display
        Set file rank piece -> do
            let updateBoard (Board boardFunc) = Board ( \f r ->
                if f == file && r == rank
                    then HasPiece piece
                    else boardFunc f r)
            modify updateBoard
            gets display
        ReplInstruction _ -> throwError $ ReplError "no such instruction"
    ```