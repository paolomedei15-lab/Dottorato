# Quality upgrading in animal-source foods — Tanzania (NPS waves 3–5)

**Hypothesis.** As household income rises, expenditure on animal products
increases *faster* than quantity, because households shift towards
higher-quality items. Using unit values and the identity
`ln(expenditure) = ln(quantity) + ln(unit value)`, the expenditure elasticity
splits as `ε_expenditure = ε_quantity + ε_quality`, and we test
`H0: ε_quality = 0` vs `H1: ε_quality > 0`.

## How to run

```r
# install.packages(c("readxl","dplyr","tidyr","ggplot2","openxlsx","sandwich","lmtest"))
# edit the three file paths at the top of analysis.R, then:
source("analysis.R")
```

It produces:

- **`results.xlsx`** — one workbook (sheets: `Households`, `Items overall`,
  `By quartile`, `Q4 over Q1`, `By rural-urban`, `By livestock`,
  `Quantity source`, `Budget composition`, `Elasticities`, `Elasticity subgroups`).
- **`figures/`** — 14 charts, all showing the six items **together** (different
  units are made comparable with indices, Q1 = 100, and logs):
  - Descriptive: 01 participation · 02 participation rural/urban ·
    03 quantity index · 04 value index · 05 unit-value (quality) index ·
    06 purchased share (market integration) · 07 quantity source ·
    08 budget composition.
  - Elasticities: 09 Q4/Q1 ratios · 10 decomposition (quantity + quality) ·
    11 quality elasticity with 95% CI (the test) · 12 quality share ·
    13 quality by subgroup (rural/urban, livestock, wave) · 14 quality gradient.

## Method notes

- Items: goat meat, beef, pork, chicken & poultry, eggs, fresh milk.
  (Processed milk is not in the master files.)
- Units harmonised: meats → kg, eggs → pieces, milk → litres (each kept in its
  own unit; never mixed on one axis).
- Welfare = real total expenditure per adult equivalent; **quartiles** are built
  within wave (their levels are not comparable across waves).
- Descriptives are survey-weighted; unit values use the median; extreme
  unit-value/quantity outliers are trimmed (1st–99th percentile within item×wave).
- Consumption is valued at the unit price (home production at the item's median
  price), so quantity and value are on the same basis.
- Elasticities: three OLS regressions on log real expenditure per AE with log
  adult equivalents, an urban dummy and wave dummies; robust (HC1) standard
  errors. The purchased quantity/value are used so the identity holds exactly.

## Main result

Quality upgrading is confirmed: the quality elasticity is positive and
statistically significant for every item (pooled ≈ 0.12), so `H0` is rejected.
The income response works mainly through quantity, with a smaller but robust
quality component.
