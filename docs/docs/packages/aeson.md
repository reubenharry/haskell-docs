---
comments: true
---

`Data.Aeson`, from the `aeson` package, is a library for encoding/decoding Haskell values to/from JSON.

One of the main appeals of the library is that a JSON string, which is untyped, can be automatically parsed into a typed Haskell value.

## Encoding to JSON

```hs title="repl example"
> encode [4, 3]
"[4,3]"
> encode [(4, 3)]
"[[4,3]]"
> encode ['a', 'b']
"\"ab\""
> encode 4
"4"


> encode (M.fromList [("John", 12), ("Jane", 13)]) -- (1)!
"{\"Jane\":13,\"John\":12}"

-- an example with a custom datatype
> :set -XDeriveGeneric -XDeriveAnyClass -- (2)!
> data Piece = Bishop | Knight deriving (Generic, ToJSON)
> encode [Bishop, Knight]
"[\"Bishop\",\"Knight\"]"
```

1. A [dictionary](/packages/containers).
2. These are needed to derive `Generic` and `ToJSON` respectively.



## Decoding from JSON

```hs title="repl example"
:set -XOverloadedStrings -- (1)!
import Data.Aeson

> eitherDecode "[1,2,3]" :: Either String [Int] -- (2)!
Right [1,2,3]

> eitherDecode "[1,2," :: Either String [Int]
Left "Error in $: not enough input. Expecting json list value" -- (3)!

> eitherDecode "[1,2,3]" :: Either String (Int, Int, Int)
Right (1,2,3) -- (4)!

> eitherDecode "[1,2,3]" :: Either String (Int, Int, Double)
Right (1,2,3.0) -- (5)!

> str = "{ \"name\": \"Joe\", \"age\": \"12\" }"
> eitherDecode str :: Either String (Map String String)
Right (fromList [("age","12"),("name","Joe")])

> eitherDecode str :: Either String Value -- (6)!
Right (Object (fromList [("age",String "12"),("name",String "Joe")]))

```

1. `aeson` uses the `ByteString` representation of text. See [this gotcha](/gotchas/strings)

2. `eitherDecode` attempts to decode a JSON bytestring into a Haskell datatype.

3. If it fails, it returns an error message.

4. The result of the decoding depends on the type you specify. Both [Int] and (Int, Int, Int) work for decoding "[1,2,3]".

5. As does `(Int, Int, Double)`.

6. `Value` is a type provided by `aeson` which corresponds to any JSON, but as a Haskell value.


The user can determine the Haskell type that the JSON should be decoded into. Types which are possible must implement the `FromJSON` [typeclass](/typeclasses/overview), which is automatically implemented for many types, and can be derived automatically for custom types: 

```hs title="repl example"
> :set -XDeriveGeneric -XDeriveAnyClass -XOverloadedStrings -- (1)!
> import GHC.Generics -- (2)!

> str = "{ \"name\": \"Joe\", \"age\": 12 }" -- (3)! 

> data Person = Person { name :: Text, age  :: Int} 
    deriving (Generic, Show, FromJSON)

    
> eitherDecode str :: Either String Person
Right (Person {name = "Joe", age = 12})
```

1. These extensions are needed.
2. A library used for automatic derivations, among other things.
3. The JSON bytestring.

## With lenses

Lenses are an extremely useful tool for working with JSON in Haskell. See [here](/packages/lens/#json)