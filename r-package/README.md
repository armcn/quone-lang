# quone (R package)

R interface to the [Quone compiler](https://github.com/armcn/quone-lang) — compile, type-check, format, and evaluate `.Q` files from R.

## Install

```r
# install.packages("pak")
pak::pak("armcn/quone-lang/r-package")
```

Then install the compiler binary:

```r
quone::install_quone()
```

If no pre-built binary is available for your platform, `install_quone()` will attempt to build from source (requires [GHCup](https://www.haskell.org/ghcup/)).

## Usage

```r
library(quone)

# Compile a .Q file to R code
r_code <- compile("analysis.Q")
cat(r_code)

# Compile and write to an .R file
compile_file("analysis.Q")

# Compile and evaluate in the current session
source_quone("analysis.Q")

# Type-check without compiling
check("analysis.Q")

# Format source code
format_source("analysis.Q", in_place = TRUE)

# Launch the interactive REPL
repl()
```

## How it works

The package is a thin wrapper around the `quone` command-line compiler. Functions call the binary via `processx` and return results to R. `source_quone()` compiles a `.Q` file and immediately evaluates the generated R code, so you can use Quone as a drop-in for R scripts with type safety.
