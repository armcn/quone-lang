# Quone

Quone is a statically typed functional language that compiles to R. If you like R's data ecosystem but want type safety, algebraic data types, and a clean syntax — Quone gives you that without leaving R behind.

Every Quone program compiles to readable, idiomatic R. No runtime. No FFI. Just `.Q` in, `.R` out.

> **Note:** Quone is an early experiment built with AI-assisted coding. It may or may not continue to be developed — treat it as a proof of concept, not a production tool. Expect rough edges, missing features, and breaking changes.
>
> The name follows the lineage of statistical languages: S, then R (the letter before S), then Q. It's also a reference to the [Seinfeld scene](https://www.youtube.com/watch?v=8P2mBViPkHs) where Kramer plays "quone" in Scrabble — a word that doesn't exist but sounds like it should.

## Why Quone?

R is powerful for data work, but dynamically typed code breaks in production. Quone catches errors at compile time while generating the same R you'd write by hand:

- **Type-checked dplyr** — column names and types are verified before your code ever runs
- **Vectorization for free** — simple lambdas over vectors compile to vectorized R expressions, not `purrr::map_dbl` loops
- **Full type inference** — write clean code without type annotations; add them when you want documentation
- **Algebraic data types** — model your domain precisely with sum types and pattern matching
- **Pipes that work** — `|>` compiles to R's native pipe

## Quick Start

```bash
cabal install
quone examples/dplyr.Q       # compile to R
quone examples/rmse.Q        # another example
quone repl                   # interactive REPL
```

## Examples

### Data manipulation with dplyr

Define a typed schema, build a dataframe, and transform it — the compiler checks that every column reference and operation is valid.

```elm
library dplyr

import desc : a -> a
import n : a -> Integer

type alias Students <-
    dataframe
        { name : Vector Character
        , score : Vector Double
        , dept : Vector Character
        }

students : Students
students <-
    dataframe
        { name = ["Alice", "Bob", "Carol", "Dan", "Eve"]
        , score = [92.0, 78.0, 85.0, 64.0, 91.0]
        , dept = ["math", "cs", "math", "cs", "math"]
        }

result <-
    students
    |> filter (score > 70.0)
    |> mutate { pct = score / 100.0 }
    |> arrange (desc score)
```

Compiles to:

```r
library(dplyr)

students <- data.frame(
  name = c("Alice", "Bob", "Carol", "Dan", "Eve"),
  score = c(92.0, 78.0, 85.0, 64.0, 91.0),
  dept = c("math", "cs", "math", "cs", "math")
)

result <- students |>
  dplyr::filter(score > 70.0) |>
  dplyr::mutate(pct = score / 100.0) |>
  dplyr::arrange(desc(score))
```

Try `mutate { grade = name / 100.0 }` — the compiler tells you `'name' is Character, cannot use '/'` before you ever run R.

### Automatic vectorization

```elm
rmse : Vector Double -> Vector Double -> Double
rmse predicted actual <-
    map2 (\p a -> p - a) predicted actual
    |> map (\e -> e * e)
    |> mean
    |> sqrt
```

Compiles to:

```r
rmse <- function(predicted, actual) {
  (predicted - actual) |>
    (\(.x) .x * .x)() |>
    mean() |>
    sqrt()
}
```

The `map2` and `map` calls are fused into vectorized arithmetic — no element-wise iteration.

### Algebraic data types

```elm
type Option a
    <- None
    | Some a

map_option : (Integer -> Integer) -> Option Integer -> Option Integer
map_option f opt <-
    case opt of
        None ->
            None

        Some x ->
            Some (f x)

result : Option Integer
result <-
    map_option (\x -> x + 1) (Some 41)
```

### Let bindings

```elm
std_dev : Vector Double -> Double
std_dev xs <-
    let
        avg <- mean xs
        n <- to_double (length xs)
        sum_sq <- map (\x -> (x - avg) * (x - avg)) xs |> sum
    in
    sqrt (sum_sq / n)
```

## Type System

Types map directly to R — no wrappers, no boxing:

| Quone | R |
|---|---|
| `Integer` | `integer` |
| `Double` | `double` |
| `Logical` | `logical` |
| `Character` | `character` |
| `Vector a` | atomic vector |
| `{ name : a, ... }` | named list |
| `dataframe { ... }` | `data.frame` |

Type inference is Hindley-Milner. Annotations are always optional but checked when present.

## Built-in Functions

| Function | Type | R |
|---|---|---|
| `map` | `(a -> b) -> Vector a -> Vector b` | vectorized or `purrr::map_dbl` |
| `map2` | `(a -> b -> c) -> Vector a -> Vector b -> Vector c` | vectorized or `purrr::map2_dbl` |
| `reduce` | `(b -> a -> b) -> b -> Vector a -> b` | `purrr::reduce` |
| `keep` | `(a -> Logical) -> Vector a -> Vector a` | `purrr::keep` |
| `discard` | `(a -> Logical) -> Vector a -> Vector a` | `purrr::discard` |
| `mean` | `Vector a -> Double` | `mean` |
| `sum` | `Vector a -> Double` | `sum` |
| `length` | `Vector a -> Integer` | `length` |
| `sqrt` | `Double -> Double` | `sqrt` |
| `to_double` | `Integer -> Double` | `as.numeric` |

## dplyr Verbs

All verbs are type-checked against the dataframe schema:

| Verb | Quone | R |
|---|---|---|
| Select columns | `select name score` | `dplyr::select(name, score)` |
| Filter rows | `filter (score >= 80.0)` | `dplyr::filter(score >= 80.0)` |
| Add columns | `mutate { pct = score / 100.0 }` | `dplyr::mutate(pct = score / 100.0)` |
| Aggregate | `summarize { avg = mean score }` | `dplyr::summarize(avg = mean(score))` |
| Group | `group_by dept` | `dplyr::group_by(dept)` |
| Sort | `arrange (desc score)` | `dplyr::arrange(desc(score))` |

## REPL

```bash
quone repl
```

```
quone> x <- [1.0, 2.0, 3.0, 4.0]
x : Vector Double
quone> x |> map (\n -> n * n) |> mean
[1] 7.5
quone> :t mean
Vector a -> Double
quone> :q
```

Multi-line input is automatic — the REPL continues reading on `<-`, `|>`, and unbalanced brackets.

## Editor Support

A Cursor/VS Code extension is included at `editors/cursor/quone-lang/` with syntax highlighting, diagnostics, hover types, and formatting.

## Usage

```bash
quone program.Q              # compile to R (stdout)
quone program.Q --format     # format source code
quone program.Q --package    # generate an R package
quone repl                   # interactive REPL
```

## Building

Requires GHC and Cabal ([GHCup](https://www.haskell.org/ghcup/)).

```bash
cabal build
cabal install
```

## More Examples

- [rmse.Q](examples/rmse.Q) — vectorized RMSE with pipes
- [dplyr.Q](examples/dplyr.Q) — typed dplyr pipelines
- [option.Q](examples/option.Q) — algebraic data types and pattern matching
- [normalize.Q](examples/normalize.Q) — z-score normalization with let bindings
- [records.Q](examples/records.Q) — record types and field access
- [dataframe.Q](examples/dataframe.Q) — dataframe construction
