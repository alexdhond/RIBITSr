# R/discrepancy-handling.R
#
# ✅ REFACTORING COMPLETE - This file has been split into focused modules:
#
# Split Date: 2025-12-27
# Original Size: 1,052 lines
# New Structure: 5 focused modules (all <400 lines each)
#
# Module Locations:
# - R/discrepancy-config.R (123 lines) - Configuration and priority rules
#   Functions: rb_discrepancy_config(), .get_discrepancy_config()
#
# - R/discrepancy-compare.R (294 lines) - Value and dataframe comparison logic
#   Functions: .compare_values(), .compare_dataframes(), .compare_geometries(), .fuzzy_match()
#
# - R/discrepancy-resolve.R (400 lines) - Resolution and auto-harmonization engine
#   Functions: .resolve_discrepancies(), .auto_harmonize(), .try_auto_harmonization_rules()
#   Helpers: .count_decimals(), .normalize_string()
#
# - R/discrepancy-report.R (130 lines) - User-facing reporting and export
#   Functions: rb_discrepancy_report(), rb_export_discrepancies()
#
# - R/discrepancy-merge.R (115 lines) - Data merging utilities
#   Functions: .merge_preserving_columns(), .merge_multiple_sources()
#
# This file is kept as a placeholder for documentation purposes.
# All functions have been moved to their respective modules.
# See CLAUDE.md for refactoring progress and rationale.
