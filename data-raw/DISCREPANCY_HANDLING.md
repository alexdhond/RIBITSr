# Discrepancy Handling in RIBITSr

RIBITS data comes from three sources that sometimes have **conflicting values**. RIBITSr automatically detects, reports, and resolves these conflicts.

## Data Sources & Priority

RIBITSr fetches from three sources and merges them:

| Priority | Source | Description |
|----------|--------|-------------|
| 1st | **CSV Reports** | Official RIBITS records - most complete and authoritative |
| 2nd | **RIBITS API** | Real-time data - most current but fewer fields |
| 3rd | **EPA ArcGIS** | Best spatial coverage - excellent for footprints/service areas |

**Default**: When conflicts occur, **CSV values win** because they represent the official record.

---

## Quick Start

```r
library(RIBITSr)

# Fetch data - discrepancies detected automatically
ca <- rb_banks(state = "CA")

# Check for discrepancies
ca$.meta$discrepancies

# View detailed report
rb_discrepancy_report(ca)
```

---

## What Gets Detected

### 1. Numeric Differences

```text
Field: available_credits
CSV: 452.3 credits
API: 450.5 credits
Difference: 0.4%
```

### 2. String Mismatches

```text
Field: district
CSV: "Los Angeles District"
API: "Los Angeles"
```

### 3. Date Differences

```text
Field: establishment_date
CSV: 2020-03-15
API: 2020-03-14
```

### 4. Missing Values

```text
Field: total_acres
CSV: 125.5
API: NA
```

### 5. Geometry Discrepancies

```text
Field: footprint
API: 125.5 acres
EPA: 127.2 acres
Difference: 1.4%
```

---

## Configuring Discrepancy Handling

### Change Source Priority

```r
# Default: CSV > API > EPA (CSV wins conflicts)
rb_discrepancy_config(source_priority = c("csv", "api", "epa"))

# Or prioritize API for most current data
rb_discrepancy_config(source_priority = c("api", "csv", "epa"))

# Or prioritize EPA for spatial work
rb_discrepancy_config(source_priority = c("epa", "api", "csv"))
```

### Adjust Tolerances

```r
rb_discrepancy_config(
  numeric_tolerance = 0.05,      # 5% difference is acceptable

  date_tolerance_days = 1,       # 1 day difference is acceptable
  flag_threshold = 10            # >10% difference = high severity
)
```

### String Matching Options

```r
# Exact match (default)
rb_discrepancy_config(string_matching = "exact")

# Ignore case: "Los Angeles" == "los angeles"
rb_discrepancy_config(string_matching = "ignore_case")

# Fuzzy: "Los Angeles District" == "Los-Angeles,District"
rb_discrepancy_config(string_matching = "fuzzy")
```

### Disable Auto-Resolution

```r
# Keep all conflicting values for manual review
rb_discrepancy_config(auto_resolve = FALSE)
```

---

## Viewing Discrepancies

### Quick Check
```r
ca <- rb_banks(state = "CA")
nrow(ca$.meta$discrepancies)  # How many discrepancies?
```

### Detailed Report
```r
rb_discrepancy_report(ca)

# Output:
# ══ Discrepancy Report ══════════════════════════════════════
# ℹ Total discrepancies: 45
# ℹ Severity breakdown:
#   × high: 3      # Needs attention
#   ! medium: 12   # Should review
#   ℹ low: 30      # Minor differences
```

### Filter & Group
```r
# Only high-severity issues
rb_discrepancy_report(ca, severity_filter = "high")

# Group by affected field
rb_discrepancy_report(ca, group_by = "field")

# Group by bank
rb_discrepancy_report(ca, group_by = "bank")
```

### Export for Review
```r
# Save to CSV for Excel/Google Sheets
rb_export_discrepancies(ca, "discrepancies_CA.csv")
```

---

## Resolution Strategies

### Strategy 1: Use Priority Rules (Default)
```r
# CSV values win by default
ca <- rb_banks(state = "CA")
```

### Strategy 2: Single Source Only
```r
# Use only CSV (official records) - no conflicts possible
ca <- rb_banks(state = "CA", sources = "csv")

# Use only API (most current)
ca <- rb_banks(state = "CA", sources = "api")
```

### Strategy 3: Manual Review
```r
rb_discrepancy_config(auto_resolve = FALSE)
ca <- rb_banks(state = "CA")

# Review all discrepancies manually
rb_export_discrepancies(ca, "review_these.csv")
```

---

## Common Scenarios

### Scenario 1: Official Reporting (Strict)

For official reports, use CSV only and be strict:

```r
# Use official records only
data <- rb_banks(state = "CA", sources = "csv")

# No discrepancies since single source
```

### Scenario 2: Research Analysis (Lenient)

For research, be more lenient and use all sources:

```r
rb_discrepancy_config(
  numeric_tolerance = 0.05,    # 5% OK
  date_tolerance_days = 7,     # 1 week OK
  string_matching = "fuzzy"    # Flexible matching
)

ca <- rb_banks(state = "CA")  # All sources merged
```

### Scenario 3: Spatial Analysis

For mapping, prioritize EPA:

```r
rb_discrepancy_config(source_priority = c("epa", "csv", "api"))

ca <- rb_banks(state = "CA", spatial = TRUE)
```

---

## Best Practices

### 1. Always Check Discrepancies
```r
ca <- rb_banks(state = "CA")

if (nrow(ca$.meta$discrepancies) > 0) {
  cli::cli_alert_warning("Found {nrow(ca$.meta$discrepancies)} discrepancies")
  rb_discrepancy_report(ca, severity_filter = "high")
}
```

### 2. Document Your Configuration
```r
# At the start of your analysis:
rb_discrepancy_config(
  source_priority = c("csv", "api", "epa"),
  numeric_tolerance = 0.02
)
# Reason: CSV is official record, allowing 2% numeric tolerance
```

### 3. Export for Reproducibility
```r
rb_export_discrepancies(ca, "analysis/discrepancies_2025.csv")
```

---

## Troubleshooting

### "Too many discrepancies!"
```r
# Relax tolerance settings
rb_discrepancy_config(
  numeric_tolerance = 0.05,
  flag_threshold = 10
)
```

### "I want exact matches only"
```r
rb_discrepancy_config(
  numeric_tolerance = 0,
  date_tolerance_days = 0,
  string_matching = "exact"
)
```

### "Ignore discrepancies entirely"
```r
# Use single source - no conflicts possible
data <- rb_banks(state = "CA", sources = "csv")
```

---

## Function Reference

| Function | Purpose |
|----------|---------|
| `rb_discrepancy_config()` | Set handling rules (priority, tolerance) |
| `rb_discrepancy_report()` | View formatted report |
| `rb_export_discrepancies()` | Export to CSV |
| `data$.meta$discrepancies` | Access raw discrepancy tibble |

---

## See Also

- `?rb_discrepancy_config` - Configuration options
- `?rb_discrepancy_report` - Report generation
- `?rb_banks` - Main data retrieval
