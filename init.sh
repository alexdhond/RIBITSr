#!/bin/bash

# Install required R packages if not already installed
install_r_package_if_missing() {
  if ! R -s -e "suppressPackageStartupMessages(require('$1'))" > /dev/null 2>&1; then
    echo "Installing R package: $1"
    R -s -e "install.packages('$1', repos = 'http://cran.us.r-project.org')"
  else
    echo "R package $1 is already installed."
  fi
}

install_r_package_if_missing "devtools"
install_r_package_if_missing "roxygen2"
install_r_package_if_missing "testthat"
install_r_package_if_missing "httr2"

echo ""
echo "RIBITSr Development Environment Setup Complete!"
echo ""
echo "To load and use the package in R, run the following commands in an R console from the project root directory:"
echo "  # Build and install the package (run once or after changes)"
echo "  devtools::load_all()"
echo "  # To run tests"
echo "  devtools::test()"
echo "  # To generate documentation"
echo "  devtools::document()"
echo ""
echo "You can now start developing your R package."
