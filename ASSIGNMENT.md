[[_TOC_]]

# Implementing `jq` in Haskell

`jq` ([https://jqlang.github.io/jq/](https://jqlang.github.io/jq/)) is a JSON processor.
It's built in the spirit of Unix: doing one thing, but doing it well.
Think of `awk`, `sed` or `grep`, but for JSON.
And we're going to build its clone in Haskell!

In this text, we provide you with a description of what we expect you to implement.
**Please read it fully and carefully.**
The assignment is divided into two parts: a basic part and an advanced part. To get a passing grade for the project, you only have to implement the basic part.
Implementing the advanced part correctly will lead to a higher grade.
However, you will only score points for the advanced part if your basic part is of acceptable quality, more details on that below.

This is an *individual project*, which means you should not collaborate directly with other students.
However, you are allowed to discuss the assignment with other students and ask general questions on TU Delft Answers.
**Under no circumstances should you ever look at complete or partial solutions by another student, or show (part of) your code to another student.**
See [www.tudelft.nl/en/student/legal-position/fraud-plagiarism/](https://www.tudelft.nl/en/student/legal-position/fraud-plagiarism/) for the general TU Delft policy regarding fraud.

It is expected that completing the project will take you approximately 30-40 hours.
When you have finished the project, you should hand in your final solution via Weblab before **Friday, 19th of April, 23:59**.
Detailed instructions for submitting your solution will appear on Weblab.

## Introducing `jq`

First let's see what `jq` is all about.

### Case study: counting meteorites
You will find it in the [`JQ-CASE-STUDY.md`](./JQ-CASE-STUDY.md)

### Documentation and more
There's a [tutorial](https://jqlang.github.io/jq/tutorial/), which introduces some of the basic features of the tool.
Then there's official [documentation](https://jqlang.github.io/jq/manual/).
And finally, you can play with `jq` in your browser on [jqplay.org](https://jqplay.org/).

> **NOTE** : for some inputs that contain escaped characters,
`jq` and `jqplay` can give different results, this has to do with escaping backslashes in terminal or powershell.

## Task description

You're going to implement a clone of `jq` in Haskell.
However, it's a big and mature project, so we have to trim it down to be feasible to implement in 30-40 hours.
Below you'll find a list of requirements for your implementation.
Note that many of the descriptions of the features you have to implement are rather brief. In those cases it is part of the assignment that you pin down the exact semantics of jq yourself, either by consulting the documentation linked above or testing examples against the original implementation.

For full formal definition of JSON take a look at:
  * The [JSON parsing diagrams](https://www.json.org/json-en.html)
  * The [wiki](https://en.wikipedia.org/wiki/JSON)
  * The [rfc](https://tools.ietf.org/rfc/rfc8259.txt)

### Project structure

To get you started, this repository provides a basic template for the project.
The `src` folder contains all the source files needed of the program.
The program is split into the library (`Jq`), which contains all the code and a an executable (`exe`), which simply runs it.
The template code you are given already contains the functionality to parse `null` as an input and the identity `.` as a filter.

- `JSON.hs` contains a datatype `JSON` to represent JSON data. It only has a single constructor `JNull`, so you will need to extend it with additional constructors to represent all kinds of `JSON` data. You're also required to implement by hand `Show` and `Eq` type-class isntances for this datatype.
- `Filters.hs` contains a datatype `Filter` to represent `jq` filters. It has a single constructor for identity filter, so you will need to extend it too.
- `Compiler.hs` contains the function `compile` that transforms a `Filter` into a function of type `JSON -> Either String [JSON]`, that can be executed on `JSON` values to produce either an error `String` or a list of results. It is currently only implemented for the `Identity` filter, so you will need to add cases for the other filters. Once you learn about monads you see that `compile` function fits the use-case of `Reader` monad. While we can't check the style-code of every submission, we highly encourage you to (re)write your `compile` function with `Reader`. The final signature would be then `Reader JSON (Either String [JSON])`
- `CParser.hs` and `JParser.hs` contain functions `parseFilter` and  `parseJSON` for parsing filters and `JSON` data, respectively. They both make use of the monadic parsing library from Chapter 13 of *Programming in Haskell (second edition)* by Graham Hutton. The code from this chapter can be found in the file `Parsing/Parsing.hs`. The functionality of `CParser.hs` and `JParser.hs` is re-exported by the module `Parser.hs`.
- Finally, `Main.hs` contains the `main` function that collects the inputs, compiles the filter and runs it. You do not need to edit it yourself.
- `Parsing` contains parsing library by Graham Hutton.

The `test` directory contains some (extensible) tests for you to run. More information in the "Testing" section below.

You are free to add additional modules to your project should you need them, but we ask you to keep the existing structure intact to make grading easier.

In addition to the functions from the Haskell [`base`](https://hackage.haskell.org/package/base-4.17.0.0), you are allowed to use
functions from the following packages:

* `containers` (https://hackage.haskell.org/package/containers)
* `QuickCheck` (https://hackage.haskell.org/package/QuickCheck)

If you want to use these packages, you should uncomment the corresponding
line(s) in `JqClone.cabal`. Using other packages or copy-pasting code you found
online is not allowed.

### Base project

This section describes the minimum functionality we expect your implementation to satisfy.

*Grading*. In this section you can earn a maximum of 75 points, which constitutes 75% of your final grade for the project. "0 points" means that the feature is already implemented in the template. You don't *have* to do tasks above in any particular order, but we feel like this order corresponds to escalating difficulty in implementation.

1. (0 points) Read JSON input from STDIN and filters as the first argument.  
  `echo '{"this" : "that"}' | jq '.'`
2. (15 points) Parse and pretty-print valid JSONs.  
   This means, that your implementation should be able to handle  
   1. (0 points)`null`
   2. (3 points) Numbers (floating-point `1.0` and E-notation included `1.0E+20`)
   3. (3 points) Strings (with support for escape characters `"Hello, \"world\"!"` and unicode characters)
   4. (1 point) Booleans
   5. (4 points) Arrays
   6. (4 points) JSON Objects

   > *Hint*: Add constructors to the `JSON` type in `src/Jq/Json.hs` and define a parser for each constructor in `src/Jq/JParser.hs`  

   Please also note that since for grading purposes we will be test your program as whole, we have to rely heavily on the correctness of implementation for pretty-printing and parsing. So while this subtask might seem relatively easy, a big part of your grade depends on it transitively.

3. (45 points total) Implement all [basic filters](https://jqlang.github.io/jq/manual/#basic-filters).
   In particular:
   1. (0 points) Identity filter `.`, which returns an object given to it.
   2. (2 point) Parenthesis '()', used for grouping operations.
   3. (5 points) Object indexing, both identifier `.field_name` and generic `.["field_name"]`.  
      If the field doesn't exist, running the filter should return `null`.
      In this case "generic" means for all field names, as opposed to "identifier-like".
      For fully generic field access look at the first one of the advanced tasks.
   4. (5 points) Optional object indexing `.field?` (and `.["field"]?`), which doesn't rise an exception if the value indexed into isn't an object.
   5. (5 points) Array index and slice `.[0]`, `.[0:10]`.  
     Slices behave very similarly to Python or Go. Start index (`0` here) is inclusive, while end index (`10`) is not.
   6. (6 points) Array/Object Value Iterator `.[]`, `.[1,2,3]`.  
     When applied to an array, the `.[]` filter iterates over its elements, and when applied on an object it iterates over its values (*not* over the keys).  
     `.[0,1,2]` returns an iterator which goes over the first, second and third elements.
   7. (6 points) Optional counterparts for indexing, slicing and iterators.
   8. (8 points) Comma operator `op1 , op2`.  
     Returns results of both `op1` and `op2` akin to iterator elements.
   9. (8 points) Pipe operator `op1 | op2`.  
     Passes results of `op1` into `op2`.

   > *Hint*: for each basic filter, add a constructor to the `Filter` type in `src/Jq/Filters.hs`, then define parser for it in `src/Jq/CParser.hs`, and interpret the filter into Haskell code by adding a case to the `compile` function in `src/Jq/Compiler.hs`

4. (10 points) Simple value constructors  
   `jq` allows you to construct values from the input elements:  
   `echo '1' | jq '{"this" : [.]}'` (this produces `{"this": [1]})`), or ignoring them:  
   `echo 'null' | jq '{"this" : [42]}'` (this produces `{"this": [42]})`).  
   For this task you're asked to implement only the "simple" ones: numbers, booleans, strings, arrays without iteration (`[1,2,.field]`, not `[.[]]`), objects.

5. (5 points) [Recursive descent operator](https://jqlang.github.io/jq/manual/#recursive-descent) `..` iterates over all sub-values of the current value, including itself.  
  For example, `echo [{"a" : 1}] | jq '..'` results in

  ```json
  [
    {
      "a": 1
    }
  ]
  {
    "a": 1
  }
  1
  ```

### Advanced tasks

To get your grade to 100%, your implementation should also include some of the following
features. The order and the number of points corresponds to the expected
difficulty in implementation. Each point is worth 1 percent of the final grade,
but the total grade is capped at 100%. Please note that the tasks in this
section require the basic functionality from the previous section to be already
implemented, so it does not make sense to start on these advanced features
before you are confident in your implementation of the basic part.

* (7 points) Generic indexing with filters.  
  A more general counterpart for object and array indexing, allowing arbitrary filters and iterators in the brackets.  
  For example: `echo '{"this" : ["that"], "that" : 1}' | jq '.[.this[]]'`, which returns `1`.  
  To keep this assignment independent from one with comparison operators below, you are asked to implement indexing with arbitrary filters, which output either numbers/iterators or strings.  
  Mind that this task also includes slices, generated with filters, e.g. `echo '[1, 2, 3, 4]' | jq '.[.[0]:.[3]]'`.  
  In order for this subtask to count your implementation should handle all JSON values, have all basic filters, and all basic constructors.

* (10 points) More complex value constructors  
  This is complementary to the subtask with basic value constructors -- implement the constructors for arrays e.g. `[.items[].name]`, objects (`{user}`, `{(.[]) : null}`).  
  Be warned that this part is harder than it seems and some features interact in a non-obvious way, and not every aspect of behaviour is described precisely in the documentation.  
  In case of doubt, you can experiment with the reference implementation and follow what it does.
  In order for this subtask to count your implementation should handle all JSON values, have all basic filters, and all object constructors.

* (10 points) [Conditionals and comparisons](https://jqlang.github.io/jq/manual/#conditionals-and-comparisons):
  * "Equal" and "not equal" operators `==`, `!=`, which take two JSON values and output a Boolean.
  * If-then-else expression `if A then B else C end`.
  * Comparison operators for numbers `<`, `<=`, `>`, `>=`
  * Logic connectives: `and`, `or`, `not`.

   In order for this subtask to count your implementation should handle all JSON values and have all basic filters.

* (10 points) Arithmetic expressions: `+,-,*,/`, described [here](https://jqlang.github.io/jq/manual/#builtin-operators-and-functions).  
  Mind that these operations operate not only on numbers, but also on other JSON values such as arrays, strings, and objects.  
  In order for this subtask to count your implementation should handle all JSON values, have all basic filters, and simple object constructors.

* (10 points) [Try-catch expressions](https://jqlang.github.io/jq/manual/#try-catch) `try op1 catch expr` which tries to execute `op1` and if exception appears, returns `expr`.  
  In order for this subtask to count your implementation should handle all JSON values and have all basic filters.

* (12 points) [Syntactic variables](https://jqlang.github.io/jq/manual/#variable-symbolic-binding-operator) `expr as $id | op`, which allow you to bind the value expr to identifier `id` before passing it further to `op`.  
  In order for this subtask to count your implementation should handle all JSON values and have all basic filters.

* (15 points) [Reduction operator](https://jqlang.github.io/jq/manual/#reduce) `reduce`, which corresponds to a fold over results returned by the previous operation.  
  In order for this subtask to count your implementation should handle all JSON values and have all basic filters.

* (15 points) [Functions](https://jqlang.github.io/jq/manual/#defining-functions), which allows you to define syntactical functions in jq filters.  
  In order for this subtask to count your implementation should handle all JSON values, have all basic filters, and simple object constructors.

## Approaching the project

### Setting up your development.

1. Log in to [gitlab.ewi.tudelft.nl](https://gitlab.ewi.tudelft.nl) with your TU Delft login.
2. Clone the `jq-clone` repository you've been given access to.
3. Create a `dev` branch based on the `main` branch. `git branch dev main`  
4. Put your name and email in `JqClone.cabal`, commit and push the changes.  
5. Run `stack build` to build your project and `stack install` to install the `jq-clone` executable.  
6. To run your implementation use `echo "<your-input>" | jq-clone "<your-filter>"` or `echo "<your-input>" | stack run -- "<your-filter>"`  
   Usage of quotation marks is platform-specific: on Windows only `"` is allowed, while on *nix both `'` and `"` work.

### Doing the project in parts

While you're not required to submit your project earlier and can still do everything in a last-day crunch, this is not something we can recommend.
To help you approach the task gradually we chose the following stages to provide feedback on:  
* Week **3**. Define JSON and its typeclass instances for Eq, Show.
* Week **4**. Define some filters, start compiling programs.
* Define parsers after the lecture on monadic parsing. Also start implementing the rest of the base project features.
* Week **6** and onwards. Implement features from the advanced project.

### Suggested implementation order

If you are unsure where to start or feeling a bit lost, you can approach the project in the following manner:

1. Define a datatype for `JSON` and type class instances on week 3.
2. Define a datatype for `Filters` in week 4. You can start small and implement only identity and field indexing initially. Write respective `compile` clauses.
3. Implement pipe and comma operators.
4. Implement arrays and array operations.
5. Write a parser for numbers and strings.  
   Maybe skip floating point and escape characters for now -- you can always add them later.
6. Extend the parsing to handle objects (see the [JSON parsing diagrams](https://www.json.org/json-en.html)).
7. Write a parser for filters.
   At this point you should have a program that can be ran from the command-line, yay!
8. Add value construction and make sure that composed operations have the right semantics.
9. Implement recursive descent operator.
10. Catch up on everything you skipped above. Write some tests, squash some bugs!

#### Developing a project without parser

You probably noticed that if you follow the suggested implementation order you won't be able to test all the features you're working on from the shell until you have implemented parsers.
Our suggestion is to rely on GHCi and QuickCheck instead.
* For QuickCheck introduction you can consult lecture slides, the [blog post](https://jesper.cx/posts/quickcheck-intro.html) and included tests for week 3 and 4.
* For GHCi documentation you can consult the [official manual](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html) or Chapter 2 (First steps) for Graham Hutton's book.
  To start GHCi in the project navigate to the directory where the project is located and execute `stack ghci`(or `cabal repl`) in the shell.  
  By default this will start a REPL with the Main module loaded. You can then evaluate snippets you're interested in.
  For example, to compile and test a filter one could use the following commands in GHCi:
    ```
    > import Jq.Json as J
    > import Jq.Filters as F
    > import Jq.Compiler as C
    > let fil = F.Identity -- or any other filter
    > let json = J.JNull -- or any other JSON object
    > let res = C.run (C.compile fil) json
    > print res
    ```

### Testing

There are several test suites included in the project for you.
Keep in mind that they are intentionally incomplete and aren't used to grade your project directly.
They are here simply for your convenience.

* Tests for weeks 3 and 4.
  Since at the earlier stages of the project you don't have a parser yet, we came up with a few tests to help you make sure that your implementation is correct.
  They are located in `test/week3` and `test/week4` respectively.
  To run them type `stack test JqClone:week3` or `stack test JqClone:week4`. (or `cabal test week3` or `week4`, if you're using `cabal`)
  
  These tests are QuickCheck-based so you are encouraged to use them as inspiration or extend them as you see fit.
* We also provide a small "full-pipeline" test suite based on the original jq test suite.
  Test cases are in `test/from-upstream/data/jq.test`, which you're encouraged to add your own cases to.
  You can test your current implementation with `stack test JqClone:from-upstream` (if you're using `cabal` and `cabal test from-upstream` yields nothing run `cabal configure --enable-tests` first).
  You have to have `jq` installed on your machine and available on your `$PATH` for this -- tests use it for pretty-printing.
* Finally you'll also find some unit tests for different parts of the project in `test/unit-tests`.
  These include tests for the parsers of JSON, filters and compilation units.


### Getting help

If you have questions about a part of the project, you can create a question on
TU Delft Answers at [answers.ewi.tudelft.nl](https://answers.ewi.tudelft.nl/categories/1/tags/226).
Remember to phrase your question in general terms and avoid including part of your code.
Alternatively, you can ask questions to one of the teaching assistants during
the weekly lab sessions, or send an email to
[fp-cs-ewi@tudelft.nl](mailto:fp-cs-ewi@tudelft.nl).
