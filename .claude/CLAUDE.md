# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

jugglr is an R package for validating and visualisating juggling patterns expressed in siteswap notation.

## Key development commands

General advice:
* When running R from the console, always run it with `--quiet --vanilla`
* Always run `air format .` after generating code

### Testing

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`.
- Use `devtools::test(reporter = "check")` to run all tests
- Use `devtools::test(filter = "name", reporter = "check")` to run tests for `R/{name}.R`
- DO NOT USE `devtools::test_active_file()`
- All testing functions automatically load code; you don't need to.

- All new code should have an accompanying test.
- If there are existing tests, place new tests next to similar existing tests.

### Documentation

- Run `devtools::document()` after changing any roxygen2 docs.
- Every user facing function should be exported and have roxygen2 documentation.
- Whenever you add a new documentation file, make sure to also add the topic name to `_pkgdown.yml`.
- Run `pkgdown::check_pkgdown()` to check that all topics are included in the reference index.
- Use sentence case for all headings
- Any user facing changes should be briefly described in a bullet point at the top of NEWS.md, following the tidyverse style guide (https://style.tidyverse.org/news.html).

### Code style

- Use newspaper style/high-level first function organisation. Main logic at the top and helper functions should come below.
- Don't define functions inside of functions unless they are very brief.
- Error messages should use `cli::cli_abort()` and follow the tidyverse style guide (https://style.tidyverse.org/errors.html)

## S7

jugglr uses the S7 OOP system.

**Key concepts:**

- **Classes**: Define classes with `new_class()`, specifying a name and properties (typed data fields). Properties are accessed using `@` syntax
- **Generics and methods**: Create generic functions with `new_generic()` and register class-specific implementations using `method(generic, class) <- implementation`
- **Inheritance**: Classes can inherit from parent classes using the `parent` argument, enabling code reuse through method dispatch up the class hierarchy
- **Validation**: Properties are automatically type-checked based on their definitions

**Basic example:**

```r
# Define a class
Dog <- new_class("Dog", properties = list(
  name = class_character,
  age = class_numeric
))

# Create an instance
lola <- Dog(name = "Lola", age = 11)

# Access properties
lola@age  # 11

# Define generic and method
speak <- new_generic("speak", "x")
method(speak, Dog) <- function(x) "Woof"
speak(lola)  # "Woof"
```

## Development Notes

### Code Organization
- Collate field in DESCRIPTION defines file loading order
- Provider files follow consistent naming pattern
- Utility functions grouped by purpose (`utils-*.R`)

### Documentation
- Roxygen2 comments for all exported functions
- Vignettes demonstrate key use cases
- pkgdown site provides comprehensive documentation
