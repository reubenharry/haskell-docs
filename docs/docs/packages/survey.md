
!!! Disclaimer

    This section is adapted from Gabriella Gonzalez' [State of the Ecosystem](https://github.com/Gabriella439/post-rfc/blob/main/sotu.md), which is licensed [here](https://github.com/Gabriella439/post-rfc/blob/main/LICENSE.md).

    Some of the text has been cropped or modified.

    ??? Contributors 

        * Aaron Levin
        * Alois Cochard
        * Ben Kovach
        * Benno Fünfstück
        * Carlo Hamalainen
        * Chris Allen
        * Curtis Gagliardi
        * Deech
        * David Howlett
        * David Johnson
        * Edward Cho
        * Greg Weber
        * Gregor Uhlenheuer
        * Juan Pedro Villa Isaza
        * Kazu Yamamoto
        * Kevin Cantu
        * Kirill Zaborsky
        * Liam O'Connor-Davis
        * Luke Randall
        * Marcio Klepacz
        * Mitchell Rosen
        * Nicolas Kaiser
        * Oliver Charles
        * Pierre Radermecker
        * Rodrigo B. de Oliveira
        * Stephen Diehl
        * Tim Docker
        * Tran Ma
        * Yuriy Syrovetskiy
        * @bburdette
        * @co-dan
        * @ExternalReality
        * @GetContented
        * @psibi
        * @newswim


  
!!! Legend

    🏆 = **Best in class**: the best experience in any language

    🥈 = **Mature**: suitable for most programmers

    🌱 = **Immature**: acceptable for early-adopters

    ⛔ = **Undeveloped**


## 🏆 Compilers

**Notable libraries:**

* [`parsec`](https://hackage.haskell.org/package/parsec) / [`megaparsec`](https://hackage.haskell.org/package/megaparsec) / [`attoparsec`](https://hackage.haskell.org/package/attoparsec) / [`trifecta`](https://hackage.haskell.org/package/trifecta) / [`alex`](https://hackage.haskell.org/package/alex)+[`happy`](https://hackage.haskell.org/package/happy) - parsing libraries
* [`bound`](https://hackage.haskell.org/package/bound) / [`unbound`](https://hackage.haskell.org/package/unbound) - manipulating bound variables
* [`hoopl`](https://hackage.haskell.org/package/hoopl) - optimization
* [`uuagc`](https://hackage.haskell.org/package/uuagc) - attribute grammars
* [`unification-fd`](https://hackage.haskell.org/package/unification-fd) - fast structural unification
* [`prettyprinter`](https://hackage.haskell.org/package/prettyprinter) - pretty-printing
* [`llvm-general`](https://hackage.haskell.org/package/llvm-general) - LLVM 3.5 API
* [`llvm-hs`](https://hackage.haskell.org/package/llvm-hs) - LLVM 5 API (actively maintained fork of llvm-general)
* `language-`{[`ecmascript`](https://hackage.haskell.org/package/language-ecmascript)|[`python`](https://hackage.haskell.org/package/language-python)|[`c-quote`](https://hackage.haskell.org/package/language-c-quote)|[`lua`](https://hackage.haskell.org/package/language-lua)|[`java`](https://hackage.haskell.org/package/language-java)|[`objc`](https://hackage.haskell.org/package/language-objc)|[`cil`](https://hackage.haskell.org/package/language-cil)} - parsers and
   pretty-printers for other languages

??? Commentary 

    Haskell originated in academia, and most languages of academic origin (such as
    the ML family of languages) excel at compiler-related tasks for obvious
    reasons.  As a result the language has a rich ecosystem of libraries dedicated
    to compiler-related tasks, such as parsing, pretty-printing, unification,
    bound variables, syntax tree manipulations, and optimization.

    **Some compilers written in Haskell:**

    * [`Elm`](http://elm-lang.org)
    * [`Purescript`](http://www.purescript.org)
    * [`Idris`](http://www.idris-lang.org)
    * [`Agda`](http://wiki.portal.chalmers.se/agda/pmwiki.php)
    * [`Pugs`](http://www.perlfoundation.org/perl6/index.cgi?pugs) (the first Perl 6 implementation)
    * [`ghc`](https://www.haskell.org/ghc/) (self-hosting)
    * [`frege`](https://github.com/Frege/frege) (very similar to Haskell, also self-hosting)
    * [`hython`](https://github.com/mattgreen/hython) (a Python3 interpreter written in Haskell)
    * [`Lasca`](http://lasca-lang.org) (a small Scala-like language with global type inference and optional dynamic mode on LLVM backend)
    * [`verve`](https://github.com/tadeuzagallo/verve-lang) - Functional language with object-oriented support
    * [`sixten`](https://github.com/ollef/sixten) - Haskell/Idris-style language with a focus on precise and efficient memory layout
    * [`carp`](https://github.com/carp-lang/Carp) - An efficient, statically typed Lisp with ownership tracking.
    * [`unison`](https://github.com/unisonweb/unison) - A purely functional distributed programming language with algebraic effects.
    * [`oden`](https://github.com/oden-lang/oden) (no longer in active development)

    **Educational resources:**

    * [Write you a Haskell](http://dev.stephendiehl.com/fun/)
    * [A Tutorial Implementation of a Dependently Typed Lambda Calculus](http://www.andres-loeh.de/LambdaPi/)
    * [Binders Unbound](http://ozark.hendrix.edu/~yorgey/pub/unbound.pdf)

    **Success stories:**

    * [Oden restrospective on rewrite from Racket to Haskell](https://github.com/oden-lang/oden-lang.github.io/blob/master/blog/_posts/2016-01-17-the-haskell-rewrite.md)

<!-- ## 🏆 Maintenance

??? Commentary

    Haskell is fantastic for maintaining large projects.  It's hard to fully convey how nice it is to modify existing Haskell code.  You can only appreciate this through experience.

    You can easily approach a large Haskell code base written by somebody else and make sweeping
    architectural changes to the project without breaking the code.

    You'll often hear people say: "if it compiles, it works".  A more accurate statement is: "if you refactor and
    it compiles, it works".  This lets you move fast without breaking things.

    Most statically typed languages are easy to maintain, but Haskell is on its
    own level for the following reasons:

    * Strong types
    * Purity
    * Global type inference
    * Type classes
    * Laziness

    The latter three features are what differentiate Haskell from other statically
    typed languages.

    If you've ever maintained code in other languages you know that usually your
    test suite breaks the moment you make large changes to your code base and you
    have to spend a significant amount of effort keeping your test suite up to date
    with your changes.  However, Haskell has a very powerful type system that lets
    you transform tests into invariants that are enforced by the types so that you
    can statically eliminate entire classes of errors at compile time.  These
    types are much more flexible than tests when modifying code and types require
    much less upkeep as you make large changes.

    The Haskell community and ecosystem use the type system heavily to "test" their
    applications, more so than other programming language communities.  That's not
    to say that Haskell programmers don't write tests (they do), but rather they
    prefer types over tests when they have the option.

    Global type inference means that you don't have to update types and interfaces
    as you change the code.  Whenever you do a large refactor, you can delete all type signatures and let the compiler infer the types and interfaces as you go.  When you're done refactoring, you can just insert back the type
    signatures that the compiler infers as machine-checked documentation.

    Type classes also assist refactoring because the compiler automatically
    infers type class constraints (analogous to interfaces in other languages) so
    that you don't need to explicitly annotate interfaces.  This is a huge time
    saver.

    Laziness deserves special mention because many outsiders do not appreciate how
    laziness simplifies maintenance.  Many languages require tight coupling between
    producers and consumers of data structures in order to avoid wasteful
    evaluation, but laziness avoids this problem by only evaluating data structures
    on demand.  This means that if your refactoring process changes the order in
    which data structures are consumed or even stops referencing them altogether
    you don't need to reorder or delete those data structures.  They will just
    sit around patiently waiting until they are actually needed, if ever, before
    they are evaluated.
 -->

## 🏆 Single-machine Concurrency


**Notable libraries:**

* [`stm`](https://hackage.haskell.org/package/stm) - Software transactional memory
* [`unagi-chan`](https://hackage.haskell.org/package/unagi-chan) - High performance channels
* [`async`](https://hackage.haskell.org/package/async) - Futures library
* [`streamly`](http://hackage.haskell.org/package/streamly) - A streaming library offering high performance concurrency

??? Commentary

    Haskell's concurrency runtime
    performs as well or better than other mainstream languages and is significantly easier
    to use due to the runtime support for software-transactional memory.

    The best explanation of Haskell's threading module is the documentation in
    `Control.Concurrent`:

    > Concurrency is "lightweight", which means that both thread creation and
    > context switching overheads are extremely low. Scheduling of Haskell threads
    > is done internally in the Haskell runtime system, and doesn't make use of any
    > operating system-supplied thread packages.

    In Haskell, all I/O is non-blocking by default, so for example a web server
    will just spawn one lightweight thread per connection and each thread can be
    written in an ordinary synchronous style instead of nested callbacks like in
    Node.js.

    The best way to explain the performance of Haskell's threaded runtime is to
    give hard numbers:

    * The Haskell thread scheduler can easily handle millions of threads
    * Each thread requires 1 kb of memory, so the hard limitation to thread count
    is memory (1 GB per million threads).
    * Haskell channel overhead for the standard library (using `TQueue`) is on the
    order of one microsecond per message and degrades linearly with increasing
    contention
    * Haskell channel overhead using the `unagi-chan` library is on the order of
    100 nanoseconds (even under contention)
    * Haskell's `MVar` (a low-level concurrency communication primitive) requires
    10-20 ns to add or remove values (roughly on par with acquiring or releasing
    a lock in other languages)

    Haskell also provides software-transactional memory, which allows programmers
    build composable and atomic memory transactions.  You can compose transactions
    together in multiple ways to build larger transactions:

    * You can sequence two transactions to build a larger atomic transaction
    * You can combine two transactions using alternation, falling back on the
    second transaction if the first one fails
    * Transactions can retry, rolling back their state and sleeping until one
    of their dependencies changes in order to avoid wasteful polling

    A few other languages provide software-transactional memory, but Haskell's
    implementation has two main advantages over other implementations:

    * The type system enforces that transactions only permit reversible memory
    modifications.  This guarantees at compile time that all transactions can
    be safely rolled back.
    * Haskell's STM runtime takes advantage of enforced purity to improve the
    efficiency of transactions, retries, and alternation.

    Haskell is also the only language that supports both software transactional
    memory and non-blocking I/O.


    **Educational resources:**

    * [Parallel and Concurrent Programming in Haskell](http://chimera.labs.oreilly.com/books/1230000000929)
    * [Parallel and Concurrent Programming in Haskell - Software transactional
    memory](http://chimera.labs.oreilly.com/books/1230000000929/ch10.html#sec_stm-async)
    * [Beautiful concurrency](https://www.fpcomplete.com/school/advanced-haskell/beautiful-concurrency) - a software-transactional memory tutorial
    * [Performance numbers for primitive operations](https://github.com/jberryman/chan-benchmarks#some-analysis-of-primitive-operations) - Latency timings for
    various low-level operations

    **Success Stories:**

    * [What is the Haskell response to Node.js?](http://stackoverflow.com/questions/3847108/what-is-the-haskell-response-to-node-js)
    * [Haskell and non-blocking asynchronous IO](https://blog.lahteenmaki.net/haskell-and-non-blocking-asynchronous-io.html)

## 🏆 Parsing / Pretty-printing

**Parsing libraries:**

<!-- * [`parsec`](https://hackage.haskell.org/package/parsec) - Best overall "value" -->
* [`megaparsec`](https://hackage.haskell.org/package/megaparsec) - Modern, actively maintained fork of `parsec`
* [`attoparsec`](https://hackage.haskell.org/package/attoparsec) - Extremely fast backtracking parser
* [`Earley`](https://hackage.haskell.org/package/Earley) - Earley parsing
  embedded within the Haskell language.  Parses all context-free
  grammars, even ambiguous ones, with no need to left factor.
  Returns all valid parses.
* [`trifecta`](https://hackage.haskell.org/package/trifecta) - Best error messages (`clang`-style)
* [`parsers`](https://hackage.haskell.org/package/parsers) - Interface compatible with `attoparsec`, `parsec` and `trifecta` which lets you easily switch between them.  People commonly use this library to begin with `trifecta` or `parsec` (for better error messages) then switch to `attoparsec` when done for performance
* [`alex`](https://hackage.haskell.org/package/alex) / [`happy`](https://hackage.haskell.org/package/happy) - Like `lexx` / `yacc` but with Haskell integration

??? Commentary 

    Haskell parsing is extremely powerful.  Recursive descent parser combinators are
    far-and-away the most popular parsing paradigm within the Haskell ecosystem, so
    much so that people use them even in place of regular expressions. 

    If you're not sure what library to pick, we generally recommend the `megaparsec`
    library as a default well-rounded choice because it strikes a decent balance
    between ease-of-use, performance, good error messages, and small dependencies.

    `attoparsec` deserves special mention as an extremely fast backtracking parsing
    library.  The speed and simplicity of this library will blow you away.  The
    main deficiency of `attoparsec` is the poor error messages.

    The pretty-printing front is also excellent.  Academic researchers just really
    love writing pretty-printing libraries in Haskell for some reason.


    **Pretty-printing libraries:**

    * [`prettyprinter`](https://hackage.haskell.org/package/prettyprinter) - Pretty-printing library
    * [`text-format`](https://hackage.haskell.org/package/text-format) - High-performance string formatting

    **Educational resources:**

    * [Monadic Parsing in Haskell](http://www.cs.nott.ac.uk/~gmh/pearl.pdf)

    **Success Stories:**

    * [A major upgrade to attoparsec: more speed, more power](http://www.serpentine.com/blog/2014/05/31/attoparsec/)



## 🥈 Server-side web programming

**Notable libraries:**

* [`aeson`](https://hackage.haskell.org/package/aeson) - Parsing and generation of JSON
* [`warp`](https://hackage.haskell.org/package/warp) / [`wai`](https://hackage.haskell.org/package/wai) - the low-level server and API that all server libraries share, with the exception of `snap`
* [`scotty`](https://hackage.haskell.org/package/scotty) - A beginner-friendly server framework analogous to Ruby's Sinatra
* [`spock`](https://www.spock.li/) - Lighter than the "enterprise" frameworks, but more featureful than scotty (type-safe routing, sessions, conn pooling, csrf protection, authentication, etc)
* [`yesod`](https://hackage.haskell.org/package/yesod) / [`yesod-*`](https://hackage.haskell.org/packages/search?terms=yesod) / [`snap`](https://hackage.haskell.org/package/snap) / [`snap-*`](https://hackage.haskell.org/packages/search?terms=snap) / [`happstack-server`](https://hackage.haskell.org/package/happstack-server) / [`happstack-*`](https://hackage.haskell.org/packages/search?terms=happstack) - "Enterprise" server frameworks with all the bells and whistles
* [`ihp`](https://ihp.digitallyinduced.com/) - batteries-included web framework with a friendly and helpful community. The best choice when getting started with haskell.
* [`servant`](https://hackage.haskell.org/package/servant) / [`servant-*`](https://hackage.haskell.org/packages/search?terms=servant) - Library for type-safe REST servers and clients that might blow your mind
* [`graphql-api`](http://hackage.haskell.org/package/graphql-api) - Implement a GraphQL API
* [`websockets`](https://hackage.haskell.org/package/websockets) - Standalone websockets client and server
* [`authenticate`](https://hackage.haskell.org/package/authenticate) / [`authenticate-*`](https://hackage.haskell.org/packages/search?terms=authenticate) - Shared authentication libraries
* [`ekg`](https://hackage.haskell.org/package/ekg) / [`ekg-*`](https://hackage.haskell.org/packages/search?terms=ekg) - Haskell service monitoring
* [`stm`](https://hackage.haskell.org/package/stm) - Software-transactional memory
* [`lucid`](https://hackage.haskell.org/package/lucid) - Haskell DSL for
  building HTML
* [`mustache`](https://hackage.haskell.org/package/mustache) / [`karver`](https://hackage.haskell.org/package/karver) - Templating libraries

??? Commentary

    The main features in this category that Haskell brings to the table are:

    * Server stability
    * Performance
    * Ease of concurrent programming
    * Excellent support for web standards

    The strong type system and polished runtime greatly improve server stability
    and simplify maintenance.  This is the greatest differentiator of Haskell from
    other backend languages, because it significantly reduces the
    total-cost-of-ownership.  You should expect that you can maintain Haskell-based
    services with significantly fewer programmers than other languages, even when
    compared to other statically typed languages.

    The greatest weakness of server stability is space leaks.  The most
    common solution that I know of is to use `ekg` (a process monitor) to examine
    a server's memory stability before deploying to production.  The second most
    common solution is to learn to detect and prevent space leaks with experience,
    which is not as hard as people think.

    Haskell's performance is excellent and currently comparable to Java.  Both
    languages give roughly the same performance in beginner or expert hands,
    although for different reasons.

    Where Haskell shines in usability is the runtime support for the following
    three features:

    * software transactional memory (which differentiates Haskell from Go)
    * lightweight threads that use non-blocking I/O (which differentiates Haskell from the JVM)
    * garbage collection (which differentiates Haskell from Rust)

    If you have never tried out Haskell's software transactional memory (STM), we highly recommend giving it a go, since it eliminates a large number of concurrency logic bugs.  STM is far and away the most underestimated feature of the Haskell runtime.

    **Some web sites,services, and projects powered by Haskell:**

    * Facebook's spam filter: Sigma
    * IMVU's REST API
    * Utrecht's bicycle parking guidance system
    * [elm-lang.org](http://elm-lang.org/)
    * [glot.io](http://glot.io/)
    * [The Perry Bible Fellowship](http://pbfcomics.com/)
    * [Silk](https://www.silk.co)
    * [Shellcheck](http://www.shellcheck.net/)
    * [instantwatcher.com](http://instantwatcher.com/)
    * [markup.rocks](http://markup.rocks/)
    * [ZoomHub](http://zoomhub.net/) ([Code](https://github.com/zoomhub/zoomhub))
    * [PostgREST](https://postgrest.com/en/v4.3/) - Generates a REST API for a
    Postgres database
    * [Hasura](https://github.com/hasura/graphql-engine)
    * [Mercury](https://mercury.com/)

    **Success Stories:**

    * [Fighting spam with Haskell - Haskell in production, at scale, at Facebook](https://code.facebook.com/posts/745068642270222/fighting-spam-with-haskell/)
    * [IMVU Engineering - What it's like to use Haskell](http://engineering.imvu.com/2014/03/24/what-its-like-to-use-haskell/)
    * [Haskell-based Bicycle Parking Guidance System in Utrecht](https://www.reddit.com/r/haskell/comments/3959r0/haskellbased_bicycle_parking_guidance_system_in/)
    * [Mio: A High-Performance Multicore IO Manager for GHC](http://haskell.cs.yale.edu/wp-content/uploads/2013/08/hask035-voellmy.pdf)
    * [The Performance of Open Source Applications - Warp](http://www.aosabook.org/en/posa/warp.html)
    * [Optimising Garbage Collection Overhead in Sigma](https://simonmar.github.io/posts/2015-07-28-optimising-garbage-collection-overhead-in-sigma.html)
    * instantwatcher.com author comments on rewrite from Ruby to Haskell - [\[1\]](http://www.reddit.com/r/haskell/comments/3am3qu/should_i_put_a_powered_by_haskell_tag_w_haskell/csef8eq)
    [\[2\]](http://www.reddit.com/r/haskell/comments/3e10ea/til_instantwatcher_is_made_with_haskell/ctavz2m)
    * [A lot of websockets in Haskell](https://blog.wearewizards.io/a-lot-of-websockets-in-haskell) - A load test showing that a Haskell server can handle 500K connections in 10 GB of memory.  The load tester requires more resources than the server

    **Educational resources:**

    * [Beautiful concurrency](https://www.schoolofhaskell.com/school/advanced-haskell/beautiful-concurrency) - a software-transactional memory tutorial
    * [The Yesod book](http://www.yesodweb.com/book)
    * [The Servant tutorial](https://haskell-servant.readthedocs.io/en/stable/tutorial/index.html)
    * [Overview of Happstack](http://www.happstack.com)
    * [IHP Guide](https://ihp.digitallyinduced.com/Guide/index.html)
    * [IHP Casts](https://ihpcasts.com/)

    **Notable hosting platforms:**

    * [IHP Cloud](https://ihpcloud.com/)

<!-- ## 🥈 Domain-specific languages (DSLs)

??? Commentary

    Haskell is great at DSL-building.  While not as flexible as a Lisp language I
    would venture that Haskell is the most flexible of the non-Lisp languages.
    You can overload a large amount of built-in syntax for your custom DSL.

    The most popular example of overloaded syntax is `do` notation, which you can
    overload to work with any type that implements the `Monad` interface.  This
    syntactic sugar for `Monad`s in turn led to a huge overabundance of `Monad`
    tutorials.

    However, there are lesser known but equally important things that you can
    overload, such as:

    * numeric and string literals
    * `if`/`then`/`else` expressions
    * list comprehensions
    * numeric operators

    **Educational resources:**

    * [You could have invented monads](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html)
    * [Rebindable syntax](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/rebindable_syntax.html)
    * [Monad comprehensions](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/monad_comprehensions.html)
    * [Overloaded strings](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions#overloadedstrings)

 -->

## 🥈 Testing



**Notable libraries:**

* [`QuickCheck`](https://hackage.haskell.org/package/QuickCheck) - property-based testing
* [`doctest`](https://hackage.haskell.org/package/doctest) - tests embedded directly within documentation
* [`free`](https://hackage.haskell.org/package/free) - Haskell's abstract version of "dependency injection"
* [`hspec`](https://hackage.haskell.org/package/hspec) - Testing library analogous to Ruby's RSpec
* [`HUnit`](https://hackage.haskell.org/package/HUnit) - Testing library analogous to Java's JUnit
* [`tasty`](https://hackage.haskell.org/package/tasty) - Combination unit / regression / property testing library
* [`hedgehog`](http://hackage.haskell.org/package/hedgehog) - property-based testing with integrated shrinking
- [`HTF`](https://hackage.haskell.org/package/HTF) - Preprocessor based unit testing with various output formats

??? Commentary 

    There are a few places where Haskell is the clear leader among all languages:

    * property-based testing
    * mocking / dependency injection

    Haskell's `QuickCheck` is the gold standard which all other property-based
    testing libraries are measured against.  The reason `QuickCheck` works so
    smoothly in Haskell is due to Haskell's type class system and purity.  The type
    class system simplifies automatic generation of random data from the input type
    of the property test.  Purity means that any failing test result can be
    automatically minimized by rerunning the check on smaller and smaller inputs
    until `QuickCheck` identifies the corner case that triggers the failure.

    Haskell also supports most testing functionality that you expect from other
    languages, including:

    * standard package interfaces for testing
    * unit testing libraries
    * test result summaries and visualization

    **Educational resources:**

    * [Why free monads matter](http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html)
    * [Purify code using free monads](http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html)
    * [Up-front Unit Testing in Haskell](https://github.com/kazu-yamamoto/unit-test-example/blob/master/markdown/en/tutorial.md)


## 🥈 Data structures and algorithms


**Notable libraries:**

* [`vector`](https://hackage.haskell.org/package/vector) - High-performance arrays
* [`containers`](https://hackage.haskell.org/package/containers) - High-performance `Map`s, `Set`s, `Tree`s, `Graph`s, `Seq`s
* [`unordered-containers`](https://hackage.haskell.org/package/unordered-containers) - High-performance `HashMap`s, HashSets
* [`accelerate`](https://hackage.haskell.org/package/accelerate) / [`accelerate-*`](https://hackage.haskell.org/packages/search?terms=accelerate) - GPU programming
* [`massiv`](https://hackage.haskell.org/package/massiv) / [`repa`](https://hackage.haskell.org/package/repa) / [`repa-*`](https://hackage.haskell.org/packages/search?terms=repa) - parallel shape-polymorphic arrays
* [`discrimination`](http://hackage.haskell.org/package/discrimination) - Efficient linear-time sorting for user-defined datatypes
* [`algebraic-graphs`](https://hackage.haskell.org/package/algebraic-graphs)

??? Commentary

    Haskell primarily uses persistent data structures, meaning that when you
    "update" a persistent data structure you just create a new data structure and
    you can keep the old one around (thus the name: persistent).  Haskell data
    structures are immutable, so you don't actually create a deep copy of the data
    structure when updating; any new structure will reuse as much of the original
    data structure as possible.

    The **Notable libraries** sections contains links to Haskell collections
    libraries that are heavily tuned.  You should realistically expect these
    libraries to compete with tuned Java code.  However, you should not expect
    Haskell to match expertly tuned C++ code.

    The selection of algorithms is not as broad as in Java or C++ but it is still
    pretty good and diverse enough to cover the majority of use cases.


## 🥈 Benchmarking


**Notable libraries:**

* [`criterion`](https://hackage.haskell.org/package/criterion)
* [`gauge`](http://hackage.haskell.org/package/gauge) offers a similar feature set as `criterion` but has much fewer dependencies
* [`tasty-bench`](https://hackage.haskell.org/package/tasty-bench) even lighter than `gauge` with support for comparing benchmarks

??? Commentary

    This boils down exclusively to the `criterion` library, which was done so well
    that nobody bothered to write a competing library.  Notable `criterion`
    features include:

    * Detailed statistical analysis of timing data
    * Beautiful graph output: ([Example](http://www.serpentine.com/criterion/report.html))
    * High-resolution analysis (accurate down to nanoseconds)
    * Customizable HTML/CSV/JSON output
    * Garbage collection insensitivity


    **Educational resources:**

    * [The `criterion` tutorial](http://www.serpentine.com/criterion/tutorial.html)



## 🥈 Unicode


**Notable libraries:**

* [`text`](https://hackage.haskell.org/package/text)
* [`text-icu`](https://hackage.haskell.org/package/text-icu)
* [`unicode-transforms`](https://hackage.haskell.org/package/unicode-transforms) – Unicode normalization

??? Commentary


    Haskell's Unicode support is excellent.  Just use the `text` and `text-icu`
    libraries, which provide a high-performance, space-efficient, and easy-to-use
    API for Unicode-aware text operations.

    Note that there is one big catch: the default `String` type in Haskell is
    inefficient.  You should always use `Text` whenever possible.


## 🥈 Stream programming



**Notable libraries:**

* [`conduit`](https://hackage.haskell.org/package/conduit) / [`io-streams`](https://hackage.haskell.org/package/io-streams) / [`pipes`](https://hackage.haskell.org/package/pipes) / [`streaming`](https://hackage.haskell.org/package/streaming) / [`streamly`](http://hackage.haskell.org/package/streamly) - Stream programming libraries
* [`machines`](https://hackage.haskell.org/package/machines) - Networked stream transducers library

??? Commentary 


    Haskell's streaming ecosystem is mature.  Probably the biggest issue is that
    there are too many good choices (and a lot of ecosystem fragmentation as a
    result), but each of the streaming libraries listed below has a sufficiently
    rich ecosystem including common streaming tasks like:

    * Network transmissions
    * Compression
    * External process pipes
    * High-performance streaming aggregation
    * Concurrent streams
    * Incremental parsing

    **Educational resources:**

    * [The official `streamly` manual](https://streamly.composewell.com/)
    * [The official `conduit` tutorial](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/conduit-overview)
    * [The official `pipes` tutorial](http://hackage.haskell.org/package/pipes/docs/Pipes-Tutorial.html)
    * [The official `io-streams` tutorial](http://hackage.haskell.org/package/io-streams/docs/System-IO-Streams-Tutorial.html)
    * [A benchmark of popular streaming libraries](https://github.com/composewell/streaming-benchmarks)

## 🥈 Serialization


**Notable libraries:**

* [`binary`](https://hackage.haskell.org/package/binary) / [`cereal`](https://hackage.haskell.org/package/cereal) / [`serialise`](http://hackage.haskell.org/package/serialise) / [`store`](http://hackage.haskell.org/package/store) - serialization / deserialization libraries

??? Commentary

    Haskell's serialization libraries are reasonably efficient and very easy to
    use.  You can easily automatically derive serializers/deserializers for
    user-defined data types and it's very easy to encode/decode values.

    Haskell's serialization does not suffer from any of the gotchas that
    object-oriented languages deal with (particularly Java/Scala).  Haskell data
    types don't have associated methods or state to deal with so
    serialization/deserialization is straightforward and obvious.  That's also
    why you can automatically derive correct serializers/deserializers.

    Serialization performance is pretty good.  You should expect to serialize data
    at a rate between 100 Mb/s to 1 Gb/s with careful tuning.  Serialization
    performance still has about 3x-5x room for improvement by multiple independent
    estimates.  See the "Faster binary serialization" link below for details of the
    ongoing work to improve the serialization speed of existing libraries.


    **Educational resources:**

    * [Benchmarks of several popular serialization libraries](https://github.com/haskell-perf/serialization)
    * [Faster binary serialization](http://code.haskell.org/~duncan/binary-experiment/binary.pdf) / [Better, faster binary serialization](https://github.com/meiersi/HaskellerZ/blob/master/meetups/20150529-ZuriHac2015_Duncan_Coutts-Better_Faster_Binary_Serialization/binary.pdf) - Slides on serialization efficiency improvements

## 🥈 IDE support

The *Haskell Language Server* provides IDE support for editors which support Microsoft's Language Service Protocol (LSP). The easiest of these to use is VSCode, but other choices like vim will work.

The *Haskell Language Server* is included as part of Haskell's installer, GHCup.


## 🥈 Support for file formats



**Notable libraries:**

* [`aeson`](https://hackage.haskell.org/package/aeson) - JSON encoding/decoding
* [`cassava`](https://hackage.haskell.org/package/cassava) / [`sv`](http://hackage.haskell.org/package/sv)- CSV encoding/decoding
* [`yaml`](https://hackage.haskell.org/package/yaml) - YAML encoding/decoding
* [`HsYAML`](https://hackage.haskell.org/package/HsYAML) - pure Haskell YAML 1.2 parser
* [`xml`](https://hackage.haskell.org/package/xml) - XML encoding/decoding
* [`tomland`](http://hackage.haskell.org/package/tomland) - TOML encoding/decoding

??? Commentary

    Haskell supports all the common domain-independent serialization formats (i.e.
    XML/JSON/YAML/CSV).  For more exotic formats Haskell won't be as good as, say,
    Python (which is notorious for supporting a huge number of file formats) but
    it's so easy to write your own quick and dirty parser in Haskell that this is
    not much of an issue.

## 🥈 Logging

* [`fast-logger`](https://hackage.haskell.org/package/fast-logger) - High-performance multicore logging system
* [`hslogger`](https://hackage.haskell.org/package/hslogger) - Logging library analogous to Python's `logging` library
* [`monad-logger`](https://hackage.haskell.org/package/monad-logger) - add logging with line numbers to your monad stack. Uses fast-logger under the hood.
* [`katip`](http://hackage.haskell.org/package/katip) - Structured logging
* [`log`](https://hackage.haskell.org/package/log-base) - Logging system with ElasticSearch, PostgreSQL and stdout sinks.
* [`co-log`](https://hackage.haskell.org/package/co-log) - Composable contravariant comonadic logging library.


## 🥈 Code formatting

Haskell has tools for automatic code formatting:

* [`ormolu`](https://hackage.haskell.org/package/ormolu) - More opinionated formatting tool that uses GHC's own parser
* [`fourmolu`](https://github.com/fourmolu/fourmolu) - like `ormolu` but with configurability
* [`stylish-haskell`](https://hackage.haskell.org/package/stylish-haskell) - Less opinionated code formatting tool that mostly formats imports, language extensions, and data type definitions


## 🥈 Scripting

**Notable libraries:**

* [`shelly`](https://hackage.haskell.org/package/shelly) / [`turtle`](https://hackage.haskell.org/package/turtle) / [`shellmet`](http://hackage.haskell.org/package/shellmet) - scripting libraries
* [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative) / [`cmdargs`](https://hackage.haskell.org/package/cmdargs) - command-line argument parsing
* [`haskeline`](https://hackage.haskell.org/package/haskeline) - a complete Haskell implementation of `readline` for console
  building
* [`process`](https://hackage.haskell.org/package/process) - low-level library for sub-process management
* [`ansi-terminal`](https://hackage.haskell.org/package/ansi-terminal) - de facto standard cross-platform terminal library (works on Windows as well)
* [`brick`](http://hackage.haskell.org/package/brick) - terminal user interfaces (TUIs)
* [`path`](https://hackage.haskell.org/package/path) / [`path-io`](https://hackage.haskell.org/package/path-io) - type safe handling of file paths
* [`http-client`](http://hackage.haskell.org/package/http-client) / [`http-client-*`](https://hackage.haskell.org/packages/search?terms=http-client) / [`req`](http://hackage.haskell.org/package/req) / [`req-*`](https://hackage.haskell.org/packages/search?terms=req) / [`wreq`](http://hackage.haskell.org/package/wreq) - HTTP clients

??? Commentary

    Haskell's biggest advantage as a scripting language is that Haskell is the
    most widely adopted language that supports global type inference.  Many
    languages support local type inference (such as Rust, Go, Java, C#), which
    means that function argument types and interfaces must be declared but
    everything else can be inferred.  In Haskell, you can omit everything: all
    types and interfaces are completely inferred by the compiler (with some
    caveats, but they are minor).

    Global type inference gives Haskell the feel of a scripting language while
    still providing static assurances of safety.  Script type safety matters in
    particular for enterprise environments where glue scripts running with elevated
    privileges are one of the weakest points in these software architectures.

    The second benefit of Haskell's type safety is ease of script maintenance.
    Many scripts grow out of control as they accrete arcane requirements and once
    they begin to exceed 1000 LOC they become difficult to maintain in a
    dynamically typed language.  People rarely budget sufficient time to create a
    sufficiently extensive test suite that exercises every code path for each and
    every one of their scripts.  Having a strong type system is like getting a
    large number of auto-generated tests for free that exercise all script code
    paths.  Moreover, the type system is more resilient to refactoring than a test
    suite.

    However, the language is also usable even for simple one-off disposable scripts.  These Haskell scripts are
    comparable in size and simplicity to their equivalent Bash or Python scripts. This lets you easily start small and finish big.

    Haskell has one advantage over many dynamic scripting languages, which is that
    Haskell can be compiled into a native and statically linked binary for
    distribution to others.

    Haskell's scripting libraries are feature complete and provide all the
    niceties that you would expect from scripting in Python or Ruby, including
    features such as:

    * rich suite of Unix-like utilities
    * advanced sub-process management
    * POSIX support
    * light-weight idioms for exception safety and automatic resource disposal


    **Some command-line tools written in Haskell:**

    * [`pandoc`](https://hackage.haskell.org/package/pandoc)
    * [`git-annex`](https://hackage.haskell.org/package/git-annex)
    * [`hledger`](http://hledger.org/)

    **Educational resources:**

    * [Shelly: Write your shell scripts in Haskell](http://www.yesodweb.com/blog/2012/03/shelly-for-shell-scripts)
    * [Use Haskell for shell scripting](http://www.haskellforall.com/2015/01/use-haskell-for-shell-scripting.html)


## 🌱 Data science


**Notable libraries:**

* [`diagrams`](https://hackage.haskell.org/package/diagrams) / [`diagrams-*`](https://hackage.haskell.org/packages/search?terms=diagrams) - Vector graphics library
* [`ihaskell`](https://hackage.haskell.org/package/ihaskell) - Haskell backend to IPython
* [`hmatrix`](https://hackage.haskell.org/package/hmatrix) - BLAS / LAPACK wrapper
* [`HaskellR`](http://tweag.github.io/HaskellR/) - Mix Haskell and R code in Jupyter notebooks
* [`Sparkle`](https://github.com/tweag/sparkle) - Haskell-to-Spark bridge
* [`cassava`](https://hackage.haskell.org/package/cassava) - CSV encoding and decoding
* [`Frames`](https://hackage.haskell.org/package/Frames) - Haskell data analysis tool analogous to Python's `pandas`
* [`statistics`](https://hackage.haskell.org/package/statistics)
* [`hvega`](https://hackage.haskell.org/package/hvega) - Plots via JS: quite powerful and well documented
* [`Chart`](https://hackage.haskell.org/package/Chart) / [`Chart-*`](https://hackage.haskell.org/packages/search?terms=Chart) - Charting library


??? Commentary 

    Haskell data science can take advantage of other data science ecosystems via the
    [`HaskellR`](http://tweag.github.io/HaskellR/) and
    [`Sparkle`](https://github.com/tweag/sparkle) projects. `HaskellR` is a
    Haskell-to-R bridge with Jupyter notebook integration, which lets you take
    advantage of the broad R ecosystem while benefiting from the speed and type
    safety of Haskell.  `Sparkle` is a Haskell-to-Spark bridge which lets you
    interface with the Spark subset of the Java/Scala data science ecosystem.
    However, to get a Mature rating Haskell data science needs to be able to
    stand alone without depending on other programming language ecosystems.

    If you restrict yourself to just the Haskell ecosystem then choices are more
    limited. 

    The Haskell analog of Python's `NumPy` is the `hmatrix` library, which provides Haskell
    bindings to BLAS, LAPACK.  `hmatrix`'s main limitation is that the API is a bit
    clunky, but all the tools are there.

    Haskell's charting story is okay.  Most charting APIs tend to be large, the types are a bit complex, and they
    have a very large number of dependencies.

    Fortunately, Haskell does integrate into IPython so you can use Haskell within
    an IPython shell or an online notebook.  For example, there is an online
    "IHaskell" notebook that you can use right now located here:

    * [IHaskell notebook](https://try.jupyter.org/) - Click on "Welcome to Haskell.ipynb"

    If you want to learn more about how to setup your own IHaskell notebook, visit
    this project:

    * [IHaskell Github repository](https://github.com/gibiansky/IHaskell)

    The closest thing to Python's `pandas` is the `frames` library.

    One Haskell library that deserves honorable mention here is the `diagrams`
    library which lets you produce complex data visualizations very easily if
    you want something a little bit fancier than a chart.  Check out the `diagrams`
    project if you have time:

    * [The Diagrams project](http://projects.haskell.org/diagrams/)
    * [Gallery of example diagrams](http://projects.haskell.org/diagrams/gallery.html)

    **Areas for improvement:**

    * Smooth user experience and integration across all of these libraries
    * Simple types and APIs.  The data science programmers I know dislike overly
    complex or verbose APIs
    * Beautiful data visualizations with very little investment


## 🌱 Numerical programming


**Notable libraries:**

* [`accelerate`](https://hackage.haskell.org/package/accelerate) / [`accelerate-*`](https://hackage.haskell.org/packages/search?terms=accelerate) - GPU programming
* [`vector`](https://hackage.haskell.org/package/vector) - high-performance arrays
* [`massiv`](https://hackage.haskell.org/package/massiv) / [`repa`](https://hackage.haskell.org/package/repa) / [`repa-*`](https://hackage.haskell.org/packages/search?terms=repa) - parallel shape-polymorphic arrays
* [`hmatrix`](https://hackage.haskell.org/package/hmatrix) / [`hmatrix-*`](https://hackage.haskell.org/packages/search?terms=hmatrix) - Haskell's BLAS / LAPACK wrapper
* [`ad`](https://hackage.haskell.org/package/ad) - automatic differentiation

??? Commentary

    Haskell's numerical programming story is not ready, but steadily improving.

    The biggest issues that the ecosystem faces are:

    * Really clunky matrix library APIs
    * Fickle rewrite-rule-based optimizations

    When the optimizations work they are amazing and produce code competitive with
    C.  However, small changes to your code can cause the optimizations to
    suddenly not trigger and then performance drops off a cliff.

    There is one Haskell library that avoids this problem entirely: `accelerate` generates LLVM and CUDA code at runtime and does not rely on Haskell's optimizer for code generation, which side-steps
    the problem.  `accelerate` has a large set of supported algorithms that you
    can find by just checking the library's reverse dependencies:

    * [Reverse dependencies of `accelerate`](http://packdeps.haskellers.com/reverse/accelerate)


    **Success Stories:**

    * [Exploiting vector instructions with generalized stream fusion](http://research.microsoft.com/en-us/um/people/simonpj/papers/ndp/haskell-beats-C.pdf)
    * [Type-safe Runtime Code Generation: Accelerate to LLVM](https://github.com/tmcdonell/tmcdonell.github.io/raw/master/papers/acc-llvm-haskell2015.pdf)

    **Educational Resources:**

    * [Parallel and concurrent programming in Haskell](http://chimera.labs.oreilly.com/books/1230000000929/index.html)



## 🌱 Front-end web programming


**Notable libraries:**

* [reflex](https://hackage.haskell.org/package/reflex) / [reflex-dom](https://hackage.haskell.org/package/reflex-dom) - Functional reactive programming library
  for the front end
* [miso](https://haskell-miso.org/) a small "[isomorphic](http://nerds.airbnb.com/isomorphic-javascript-future-web-apps/)" front-end framework featuring a virtual-dom, inspired by [Elm](http://elm-lang.org/), [Redux](http://redux.js.org/) and [Bobril](http://github.com/bobris/bobril).

??? Commentary 

    This boils down to Haskell's ability to compile to JavaScript and WASM. Upcoming GHC versions will allow compilation to both, but for now the technology is experimental.

    There are two Haskell-like languages for front-end programming: `elm` and `purescript`.
    These are both used in production today and have equally active maintainers and
    communities of their own.  `purescript` in particular is extremely similar to
    Haskell.

    **Areas for improvement:**

    * lack of clear story for smooth integration with existing
    JavaScript projects
    * lack of educational resources targeted at non-experts
    explaining how to translate existing front-end programming idioms to Haskell
    * lack of well-maintained and polished Haskell libraries for
    front-end programming
    * lack of documentation for `ghcjs` ecosystem.  There's not even
    a basic tutorial on how to actually use `ghcjs`

    **Notable Haskell-to-JavaScript compilers:**

    * [`ghcjs`](https://github.com/ghcjs/ghcjs)
    * [`haste`](https://hackage.haskell.org/package/haste-compiler)


## 🌱 Distributed programming

**Notable libraries:**

* [`glue-core`](https://hackage.haskell.org/package/glue-core) /
  [`glue-ekg`](https://hackage.haskell.org/package/glue-ekg) /
  [`glue-example`](https://hackage.haskell.org/package/glue-example) - Service toolkit supporting
* [`haxl`](https://hackage.haskell.org/package/haxl) - Facebook library for
  efficient batching and scheduling of concurrent data access
* [`distributed-process`](https://hackage.haskell.org/package/distributed-process) / [`distributed-process-*`](https://hackage.haskell.org/packages/search?terms=distributed-process) - Haskell analog to Erlang
* [`hadron`](https://github.com/Soostone/hadron) - Haskell wrapper around `hadoop`
* [`amazonka`](https://hackage.haskell.org/package/amazonka) / [`amazonka-*`](https://hackage.haskell.org/packages/search?terms=amazonka) - Auto-generated
  bindings to the entire Amazon Web Services SDK
* [`gogol`](http://hackage.haskell.org/package/gogol) / [`gogol-*`](https://hackage.haskell.org/packages/search?terms=gogol) - Auto-generated bindings to the entire Google Cloud Platform
* [`transient`](https://github.com/transient-haskell/transient) - composable primitives for concurrency / parallelism / distributed computing

??? Commentary 

    For distributed service architectures Haskell is catching up to its peers with
    service toolkit libraries, but for distributed computation Haskell still lags
    behind.

    There has been a lot of work in replicating Erlang-like functionality in
    Haskell through the Cloud Haskell project, not just in creating the low-level
    primitives for code distribution / networking / transport, but also in
    assembling a Haskell analog of Erlang's OTP.  Work on the higher-level libraries
    seems to have stopped, but the low-level libraries are still good for
    distributing computation.

    **Areas for improvement:**

    * More analytics libraries needed.  Haskell has no analog of `scalding` or
    `spark`.  The most we have is just a Haskell wrapper around `hadoop`
    * A polished consensus library (i.e. a high quality Raft
    implementation in Haskell) needed.


## 🌱 Standalone GUI applications

**Notable libraries:**

* [`brick`](https://github.com/jtdaugherty/brick) - Terminal UI based on vty package
* [`threepenny-gui`](https://hackage.haskell.org/package/threepenny-gui) - Framework for local apps that use the web browser as the interface
* [`gi-gtk`](https://hackage.haskell.org/package/gi-gtk) and various [other bindings](https://github.com/haskell-gi/haskell-gi/tree/master/bindings) such as GStreamer audio/video - GTK+ (and more generally, GObject) bindings done right (autogenerated using GObject Introspection, hence `gi`)
* [`wx`](https://hackage.haskell.org/package/wx) - wxWidgets bindings
* [`X11`](https://hackage.haskell.org/package/X11) - X11 bindings
* [`hsqml`](http://hackage.haskell.org/package/hsqml) - A Haskell binding for Qt Quick, a cross-platform framework for creating graphical user interfaces.
* [`fltkhs`](http://hackage.haskell.org/package/fltkhs) - A Haskell binding to FLTK. Easy install/use, cross-platform, self-contained executables.
* [`FregeFX`](https://github.com/Frege/FregeFX) - Frege bindings to Java FX
  (Frege is essentially the Haskell for the JVM)
* [`typed-spreadsheet`](http://hackage.haskell.org/package/typed-spreadsheet) -
  Library for building composable interactive forms

??? Commentary

    Most Haskell GUI libraries are wrappers around toolkits written in other
    languages (such as GTK+ or Qt). 
    However, the Haskell bindings to GTK+ have a strongly imperative feel to them.
    The way you do everything is communicating between callbacks by mutating
    `IORef`s.  Also, you can't take extensive advantage of Haskell's awesome
    threading features because the GTK+ runtime is picky about what needs to happen
    on certain threads. 

    There still isn't a Haskell binding to a widget toolkit that doesn't have some sort of setup issues with the
    toolkit.


    My impression is that most Haskell programmers interested in applications
    programming have collectively decided to concentrate their efforts on improving
    Haskell web applications instead of standalone GUI applications.  Honestly,
    that's probably the right decision in the long run.

    Another post that goes into more detail about this topic is this post written
    by Keera Studios:

    * [On the state of GUI programming in Haskell](http://keera.co.uk/blog/2014/05/23/state-gui-programming-haskell/)

    **Areas for improvement:**

    * A GUI toolkit binding that is maintained, comprehensive, and easy to use
    * Polished GUI interface builders


    **Some example applications:**

    * [`xmonad`](http://xmonad.org)
    * [`leksah`](http://leksah.org/index.html)

    **Educational resources:**

    * [Haskell port of the GTK tutorial](http://code.haskell.org/gtk2hs/docs/tutorial/Tutorial_Port/)
    * [Building pragmatic user interfaces in Haskell with HsQML](https://www.youtube.com/watch?v=JCSxWfUvi6o)
    * [FLTK GUIs, including support for the Fluid visual interface builder](https://github.com/deech/fltkhs-compose-conference-2016-talk/blob/master/Talk.pdf)


## 🌱 Machine learning


**Notable libraries:**

* [`hasktorch`](https://github.com/hasktorch/hasktorch#readme) - Haskell bindings to
  libtorch which is the C++ API for PyTorch
* [`ad`](https://hackage.haskell.org/package/ad) - Automatic differentiation,
  used as a substrate for many Haskell machine learning projects
* [`backprop`](https://hackage.haskell.org/package/backprop) - AD for heterogenous types
* [`ad-delcont`](https://hackage.haskell.org/package/ad-delcont)
* [`grenade`](https://github.com/HuwCampbell/grenade) - Machine learning
  library implemented in Haskell with a BLAS/LAPACK backend and a high-level
  type-based API
* [`tensorflow`](https://github.com/tensorflow/haskell) - Haskell bindings to
  Google's `tensorflow` project
* [`arrayfire`](https://github.com/arrayfire/arrayfire-haskell) - Haskell bindings to
  ArrayFire

??? Commentary 

    There are two approaches to using machine learning in Haskell:

    * Use a Haskell binding to an implementation in another language
    * Use a machine learning library implemented in Haskell

    You will most likely want to check out Haskell bindings to the `libtorch`
    library if you are interested in the first approach.

    Also, Tweag.io has released `Sparkle`, a Haskell integration with Spark.  This
    enables the use of MLib from Haskell.  MLib is widely used in the industry
    for machine learning. Sparkle itself is fairly new.

    * [Github repository for `Sparkle`](https://github.com/tweag/sparkle)


## 🌱 Game programming

**Notable libraries:**

* [`gloss`](https://hackage.haskell.org/package/gloss) - Simple graphics and
  game programming for beginners
* [`Yampa`](https://hackage.haskell.org/package/Yampa) - A reactive programming library which has been used to implement games in a reactive style
* [Code World](https://code.world/haskell) - Similar to `gloss`, but you can try
  it in your browser
* [`vulkan`](https://hackage.haskell.org/package/vulkan) - Low-level Vulkan bindings
* [`gl`](https://hackage.haskell.org/package/gl) - Comprehensive OpenGL bindings
* [`SDL`](https://hackage.haskell.org/package/SDL) / [`SDL-*`](https://hackage.haskell.org/packages/search?terms=SDL) / [`sdl2`](https://hackage.haskell.org/package/sdl2) - Bindings to the SDL library
* [`SFML`](https://hackage.haskell.org/package/SFML) - Bindings to the SFML library
* [`quine`](https://github.com/ekmett/quine) - Github project with cool 3D demos
* [`GPipe`](https://hackage.haskell.org/package/GPipe) - Type-safe OpenGL API
  that also lets you embed shader code directly within Haskell.  See the
  [GPipe wiki](https://wiki.haskell.org/GPipe) to learn more

??? Commentary

    Haskell is a garbage collected language, so Haskell is more appropriate for the
    scripting / logic layer of a game but not suitable manipulating a large object
    graph or for implementing a high-performance game engine due to the risk of
    introducing perceptible pauses due to GC pauses.  For simple games you can
    realistically use Haskell for the entire stack.

    Examples of games that could be fully implemented in Haskell:

    * Casual games
    * Turn-based strategy games
    * Adventure games
    * Platform / side-scrolling games
    * First-person shooter

    Examples of games that are difficult to implement at all in Haskell:

    * Real-time strategy games
    * MMORPGs

    Haskell has SDL, OpenGL, and Vulkan bindings, which are actually quite good, but that's
    about it.  You're on your own from that point onward.  There is not a rich
    ecosystem of higher-level libraries built on top of those bindings.  There is
    some work in this area, but nothing production quality or
    easy to use.

    The primary reason for the immature rating is the difficulty of integrating
    Haskell with existing game platforms, which often are biased towards a
    particular language or toolchain.  The only game platform where Haskell has no
    issues is native binaries for desktop games.  For the web, you must compile to
    JavaScript, which is doable.  For mobile games on Android you have to cross
    compile and interface the Haskell logic with Android through JNI + Haskell's
    foreign function interface.  For console games, you have no hope.

    **Areas for improvement:**

    * Improve the garbage collector and benchmark performance with large heap sizes
    * Provide higher-level game engines
    * Improve distribution of Haskell games on proprietary game platforms


    **Educational resources:**

    * [Purely Functional Games](https://gilmi.me/blog/post/2018/07/24/pfgames)



## 🌱 ARM processor support

??? Commentary

    On hobbyist boards like the Raspberry Pi its possible to compile Haskell code
    with GHC.  There are limitations; some libraries have problems on the arm platform,
    and GHCi only works on newer compilers.  Cross compiling doesn't work with
    template Haskell.  Stack and other large projects can take more than 1g of memory
    to compile.

    However, if the Haskell code builds, it runs with respectable performance on these machines.

    **Arch (Banana Pi)**

    update 2016-02-25:

    * installed today from pacman, current versions are GHC 7.10.3 and cabal-install 1.22.6.0
    * a compatible version of llvm also installed automatically.
    * GHCi passes hello world test; cabal/GHC compiled a modest project normally.

    **Raspian (Raspberry Pi, pi2, others)**

    * current version: GHC 7.4, cabal-install 1.14
    * GHCi doesn't work.

    **Debian Jesse (Raspberry Pi 3)**

    * works with: `ghc-7.10.3` and `stack-1.1.2`
    * Requires `llvm` version 3.5.2 or higher.  Do not use the `llvm-3.5` provided by default in the Jessie package distribution

    **Arch (Raspberry Pi 2)**

    * current version 7.8.2, but llvm is 3.6, which is too new.
    * downgrade packages for llvm not officially available.
    * with llvm downgrade to 3.4, GHC and GHCi work, but problems compiling yesod,
    scotty.
    * compiler crashes, segfaults, etc.



## 🌱 Computer Vision


**Notable libraries:**

* [`haskell-opencv`](https://github.com/LumiGuide/haskell-opencv)
* [`HOpenCV`](https://github.com/sinelaw/HOpenCV)
* [`HOpenCV` fork](https://github.com/acowley/HOpenCV)
* [`easyVision`](https://github.com/albertoruiz/easyVision)
* [`cv-combinators`](https://github.com/sinelaw/cv-combinators)
* [`Zef`](https://github.com/ethereon/Zef)

??? Commentary

    The largest real world Haskell usage of computer vision is LumiGuide, which
    powers municipal bicycle detection and guidance systems in Amsterdam.  They
    maintain `OpenCV` bindings in their `haskell-opencv` library.

    There are some interesting projects which try to tackle computer vision in a
    purely functional manner. `cv-combinators`, `easyVision`, and `Zef` are some
    examples.

    There are Haskell bindings for OpenCV available via `HOpenCV` which has bindings
    for versions up to `OpenCV 2.0`. A fork maintained by Anthony Cowley has bindings
    available for versions up to `OpenCV 2.4`, but it pretty much stops there.
    Currently, `OpenCV 3.0` has been released, and there are no Haskell bindings
    covering it.

    **Success Stories:**

    * [Google TechTalk on LumiGuide](https://www.youtube.com/watch?v=IKznN_TYjZk)



## 🌱 Mobile apps




??? Commentary 

    This greatly lags behind using languages that are natively supported by the
    mobile platform (i.e. Java for Android or Objective-C / Swift for iOS).

    However, one route is to compile Haskell to a supported language.  For
    example, you can compile Haskell to Java using [Eta](https://eta-lang.org/)
    to port Haskell games to Android.

    **Educational resources:**

    * [Android 2048 game in Eta](https://github.com/Jyothsnasrinivas/eta-android-2048)
    * [Android development in Haskell](https://wiki.haskell.org/Android)
    * [iPhone development in Haskell](https://wiki.haskell.org/IPhone)
  


## 🌱 Databases and data stores


**Notable libraries:**

* [`mysql-haskell`](http://hackage.haskell.org/package/mysql-haskell) / [`mysql-simple`](https://hackage.haskell.org/package/mysql-simple) - MySQL bindings
* [`postgresql-simple`](https://hackage.haskell.org/package/postgresql-simple) - Postgres bindings
* [`persistent`](https://hackage.haskell.org/package/persistent) - Database-agnostic ORM that supports automatic migrations
* [`esqueleto`](https://hackage.haskell.org/package/esqueleto) / [`relational-record`](https://hackage.haskell.org/package/relational-record) / [`opaleye`](https://hackage.haskell.org/package/opaleye) - type-safe APIs for building well-formed SQL queries
* [`acid-state`](https://hackage.haskell.org/package/acid-state) - Simple ACID data store that saves Haskell data types natively
* [`aws`](https://hackage.haskell.org/package/aws) - Bindings to Amazon DynamoDB
* [`hedis`](https://hackage.haskell.org/package/hedis) - Bindings to Redis
* [`groundhog`](https://hackage.haskell.org/package/groundhog) - A nice datatype to relational mapping library, similar to ORMs
* [`hasql`](https://hackage.haskell.org/package/hasql) - An efficient PostgreSQL driver and a flexible mapping API based on the binary protocol

??? Commentary

    The "Immature" ranking is based on the lack of bindings to commercial databases
    like Microsoft SQL server and Oracle.  So whether or not Haskell is right for
    you probably depends heavily on whether there are bindings to the specific data
    store you use.

## 🌱 Debugging

**Educational resources**:

* [GHC Manual - Profiling chapter](https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html) - Read the whole thing; you will thank me
  later
* [Debugging runtime options](https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html#rts-options-for-hackers-debuggers-and-over-interested-souls) - See the `+RTS -xc` flag which adds stack traces to all exceptions (requires profiling enabled)
* [`GHC.Stack`](http://hackage.haskell.org/package/base-4.8.1.0/docs/GHC-Stack.html) - Programmatic access to the call stack
* [Pinpointing space leaks in big programs](http://blog.ezyang.com/2011/06/pinpointing-space-leaks-in-big-programs/)
* [Real World Haskell - Profiling and Optimization](http://book.realworldhaskell.org/read/profiling-and-optimization.html)
* [The GHCi Debuggger](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#the-ghci-debugger) - Manual for GHCi-based breakpoints and
  single-stepping
* [Parallel and Concurrent Programming in Haskell - Debugging, Tuning, and Interfacing with Foreign Code](http://chimera.labs.oreilly.com/books/1230000000929/ch15.html#_debugging_concurrent_programs) - Debugging concurrent programs
* [Haskell wiki - ThreadScope](https://wiki.haskell.org/ThreadScope)

??? Commentary

    The main Haskell debugging features are:

    * Memory and performance profiling
    * Stack traces
    * Source-located errors, using [the `assert` function](http://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Exception-Base.html#v%3Aassert)
    * Breakpoints, single-stepping, and tracing within the GHCi REPL
    * Informal `printf`-style tracing using [`Debug.Trace`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Debug-Trace.html)
    * ThreadScope

## 🌱 Hot code loading

**Notable libraries:**

* [`essence-of-live-coding](https://hackage.haskell.org/package/essence-of-live-coding)
* [`dyre`](https://hackage.haskell.org/package/dyre) / [`halive`](https://hackage.haskell.org/package/halive) - Program reinitialization with saved state
* [`rapid`](http://hackage.haskell.org/package/rapid) - Code reloading within
  `ghci` that persists state across reloads
* [`plugins`](https://hackage.haskell.org/package/plugins) / [`hint`](https://hackage.haskell.org/package/hint) - Runtime compilation and linking





## 🌱 Systems / embedded programming


**Educational resources:**

* [Retrocomputing with Clash](https://gergo.erdi.hu/retroclash/)
* [/r/haskell - Haskell compiled down to Embedded Hardware](https://www.reddit.com/r/haskell/comments/3gyol1/haskell_compiled_down_to_embedded_hardware/)

??? Commentary

    Systems programming here means: programs where speed, memory layout, and latency really matter.

    Haskell fares really poorly in this area because:

    * The language is garbage collected, so there are no latency guarantees
    * Executable sizes are large
    * Memory usage is difficult to constrain (thanks to space leaks)
    * Haskell has a large and unavoidable runtime, which means you cannot easily
    embed Haskell within larger programs
    * You can't easily predict what machine code that Haskell code will compile to

    Typically people approach this problem from the opposite direction: they write
    the low-level parts in C or Rust and then write Haskell bindings to the
    low-level code.

    It's worth noting that there is an alternative approach which is Haskell DSLs
    that are strongly typed that generate low-level code at runtime.  This is the
    approach championed by the company Galois.

    **Notable libraries:**

    * [`copilot`](https://hackage.haskell.org/package/copilot) - Stream DSL that generates C code
    * [`atom`](https://hackage.haskell.org/package/atom) / [`ivory`](https://hackage.haskell.org/package/ivory) - DSL for generating embedded programs
    * [`improve`](https://hackage.haskell.org/package/improve) - High-assurance DSL for embedded code that generates C and Ada

