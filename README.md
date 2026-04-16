# Quone

Quone is a statically typed functional language that compiles to readable R code. It has Elm-like syntax, Hindley-Milner type inference, algebraic data types with pattern matching, and first-class support for R's vector operations and dplyr-style data transformations.

## Example

```elm
rmse : Vector Double -> Vector Double -> Double
rmse predicted actual <-
    map2 (\p a -> p - a) predicted actual
    |> map (\e -> e * e)
    |> mean
    |> sqrt
```

Compile to R:

```bash
quone examples/rmse.Q
```

Output:

```r
rmse <- function(predicted, actual) {
  (predicted - actual) |>
    (\(.x) .x * .x)() |>
    mean() |>
    sqrt()
}
```

Simple lambdas over vectors are automatically fused into vectorized R expressions — no `purrr::map_dbl` overhead.

## Features

- **Hindley-Milner type inference** — types are inferred; annotations are optional but checked when present
- **Algebraic data types** with exhaustive pattern matching
- **Type aliases** for records and dataframes
- **Pipe operator** `|>` compiles to R's native pipe
- **Built-in vector functions** (`map`, `map2`, `reduce`, `keep`, `discard`) compile to purrr, with automatic vectorization when possible
- **dplyr integration** — `select`, `filter`, `mutate`, `summarize`, `group_by`, `arrange` with column type checking
- **R package generation** via `--package`
- **Source formatter** via `--format`

## Types

All types correspond directly to R types:

| Quone type  | R type              |
|-------------|---------------------|
| `Integer`   | `integer`           |
| `Double`    | `double`            |
| `Logical`   | `logical`           |
| `Character` | `character`         |
| `Vector a`  | atomic vector       |

## Built-in Functions

| Function    | Type                                         | Compiles to          |
|-------------|----------------------------------------------|----------------------|
| `map`       | `(a -> b) -> Vector a -> Vector b`           | `purrr::map_dbl`     |
| `map2`      | `(a -> b -> c) -> Vector a -> Vector b -> Vector c` | `purrr::map2_dbl` |
| `reduce`    | `(b -> a -> b) -> b -> Vector a -> b`        | `purrr::reduce`      |
| `keep`      | `(a -> Logical) -> Vector a -> Vector a`     | `purrr::keep`        |
| `discard`   | `(a -> Logical) -> Vector a -> Vector a`     | `purrr::discard`     |
| `sqrt`      | `Double -> Double`                           | `sqrt`               |
| `mean`      | `Vector a -> Double`                         | `mean`               |
| `sum`       | `Vector a -> Double`                         | `sum`                |
| `length`    | `Vector a -> Integer`                        | `length`             |
| `to_double` | `Integer -> Double`                          | `as.numeric`         |

## More Examples

- [examples/rmse.Q](examples/rmse.Q) — root mean square error with pipes and vectorization
- [examples/option.Q](examples/option.Q) — algebraic data types and pattern matching
- [examples/normalize.Q](examples/normalize.Q) — z-score normalization with let bindings
- [examples/records.Q](examples/records.Q) — record types, type aliases, and field access

## Usage

```bash
# Compile to R (prints to stdout)
quone program.Q

# Format source code
quone program.Q --format

# Generate an R package
quone program.Q --package
```

## Building

Requires GHC and Cabal.

```bash
cabal build
cabal install
```
