# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**rssistat** - R package for Italian statistical data analysis (to be developed)

- **Author/Maintainer**: Giampaolo Montaletti (giampaolo.montaletti@gmail.com)
- **GitHub**: github.com/gmontaletti
- **ORCID**: https://orcid.org/0009-0002-5327-1122

## Development Commands

```r
# Core development workflow
devtools::load_all()           # Load package for interactive testing
devtools::document()           # Generate roxygen2 documentation
devtools::check()              # Full package validation (run before commits)
devtools::test()               # Run test suite
testthat::test_file("tests/testthat/test-name.R")  # Single test file

# Dependency management
renv::status()                 # Check dependency status
renv::snapshot()               # Lock current dependencies
```

## Package Structure

Standard CRAN-compliant structure:
- `R/` - Source code (one conceptual function per file)
- `man/` - Documentation (roxygen2-generated, do not edit manually)
- `tests/testthat/` - Test suite
- `vignettes/` - Package tutorials
- `data-raw/` - Raw data processing scripts

## Code Conventions

- Use **data.table** for data manipulation (performance over tidyverse)
- Section comments: `# 1. Section name -----` (not `####` rows)
- Roxygen2 with markdown support for documentation
- testthat >= 3.0.0 for testing

## Version Management

- Semantic versioning (MAJOR.MINOR.PATCH)
- Update NEWS.md for user-facing changes
- Update citation in README.md when bumping version
- Run `devtools::check()` before committing
- Snapshot renv when dependencies change

## Git Workflow

Conventional commits: `type(scope): description`
- Types: feat, fix, docs, style, refactor, test, chore

## Important Notes

- The `.claude/` directory must not be moved during cleanup
- Use agents for complex tasks
- Use btw MCP tools for R library documentation, not for websites or standard APIs
