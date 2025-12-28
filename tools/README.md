# Development Tools

This directory contains scripts for package development and maintenance.

## Code Coverage

### Quick Check

To check code coverage for the package:

```bash
Rscript tools/check-coverage.R
```

Or from R:

```r
source("tools/check-coverage.R")
```

This will:
- Run all 605 tests with coverage tracking
- Display a coverage summary by file
- List functions with zero coverage
- Generate a detailed HTML report (`coverage_report.html`)

### Coverage Goals

- **Target**: >80% overall coverage
- **Good**: >= 80% per file
- **Acceptable**: >= 50% per file
- **Needs work**: < 50% per file

### Interpreting Results

The coverage report shows:
- **Total coverage percentage** - Overall code coverage
- **Per-file coverage** - Coverage for each R source file
- **Zero-coverage functions** - Functions that have no tests

### Improving Coverage

To improve coverage:

1. Identify files/functions with low coverage
2. Write tests in `tests/testthat/`
3. Re-run coverage check
4. Iterate until coverage goals are met

### Alternative Methods

You can also check coverage using:

```r
# Interactive coverage viewer
devtools::test_coverage()

# Programmatic coverage
library(covr)
cov <- package_coverage()
percent_coverage(cov)
```

## Other Tools

Additional development scripts can be added to this directory as needed.
