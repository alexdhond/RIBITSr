# Lifecycle Management in RIBITSr

This document describes how RIBITSr manages the lifecycle of functions and features using the [lifecycle](https://lifecycle.r-lib.org/) package.

## Lifecycle Stages

RIBITSr functions use lifecycle badges to communicate their stability:

### Stable
Functions without any lifecycle badge are considered **stable** and safe to use in production.

### Experimental
`r lifecycle::badge("experimental")`

Experimental functions are new and may change significantly in future versions. Use with caution in production code.

### Deprecated
`r lifecycle::badge("deprecated")`

Deprecated functions are no longer recommended for use and will be removed in a future version. Users should migrate to the recommended alternative.

### Superseded
`r lifecycle::badge("superseded")`

Superseded functions are stable but have been replaced by better alternatives. They will not be removed but are no longer actively maintained.

## Deprecation Policy

RIBITSr follows a gradual deprecation process to minimize disruption:

### Phase 1: Soft Deprecation (Current release)
- Function works normally
- Warning issued on first use in a session
- Documentation shows deprecated badge
- Alternative function recommended

Example:
```r
.harmonize_transactions(api, csv)
#> Warning: `.harmonize_transactions()` was deprecated in RIBITSr 0.1.0.
#> ℹ Please use `.harmonize_transactions_threeway()` instead.
```

### Phase 2: Deprecation (Next minor release, ~3-6 months)
- Function still works but issues warning every time
- Help page explicitly states removal timeline

### Phase 3: Defunct (Next major release, ~6-12 months)
- Function throws error with informative message
- Users must update code to use alternative

### Phase 4: Removal (Future major release, ~12+ months)
- Function completely removed from package

## Currently Deprecated Functions

### Internal Functions

- **`.harmonize_transactions()`** (deprecated in 0.1.0)
  - **Replacement:** `.harmonize_transactions_threeway()`
  - **Reason:** New three-way merge provides better data coverage and quality
  - **Migration:** Internal function, no user action needed

## Version Numbering

RIBITSr follows [Semantic Versioning](https://semver.org/):

- **Major version** (x.0.0): Breaking changes, deprecated functions removed
- **Minor version** (0.x.0): New features, soft deprecations
- **Patch version** (0.0.x): Bug fixes, no API changes

## How to Handle Deprecation Warnings

If you see a deprecation warning:

1. **Read the warning message** - It will tell you what to use instead
2. **Update your code** - Replace the deprecated function with the recommended alternative
3. **Test thoroughly** - Ensure the new function works as expected
4. **Report issues** - If migration is difficult, file an issue at https://github.com/alexdhond/RIBITSr/issues

## For Package Developers

When deprecating a function:

1. Add `lifecycle::badge("deprecated")` to documentation
2. Call `lifecycle::deprecate_soft()` at function start
3. Document the alternative in `@description`
4. Update NEWS.md with deprecation notice
5. Set `when` parameter to current version
6. Provide helpful `with` parameter pointing to replacement

Example:
```r
#' Old Function (Deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Use [new_function()] instead.
#'
#' @export
old_function <- function(...) {
  lifecycle::deprecate_soft(
    when = "0.1.0",
    what = "old_function()",
    with = "new_function()"
  )

  # Function body
}
```

## References

- [lifecycle package documentation](https://lifecycle.r-lib.org/)
- [Tidyverse lifecycle guide](https://lifecycle.r-lib.org/articles/stages.html)
- [R Packages: Lifecycle chapter](https://r-pkgs.org/lifecycle.html)
