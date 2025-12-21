# RIBITSr

R package to interact with the RIBITS (Regulatory In-lieu fee and Bank Information Tracking System) API.

## Setup and Installation

To set up the development environment, run the `init.sh` script:

```bash
./init.sh
```

This script will install necessary R packages such as `devtools`, `roxygen2`, `testthat`, and `httr2`.

## Usage

Once the package is set up, you can load it in an R session from the project root directory:

```R
devtools::load_all()
```

## Testing

To run the tests for the package:

```R
devtools::test()
```

## Documentation

To generate documentation for the package:

```R
devtools::document()
```
