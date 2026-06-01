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

- **`results.xlsx`** — one workbook with eight sheets:
  `Households`, `Items overall`, `By quartile`, `Q4 over Q1`, `By rural-urban`,
  `By livestock`, `Quantity source`, `Elasticities`.
- **`figures/`** — four figures:
  1. `fig1_Q4_Q1_ratios` — Q4/Q1 quantity vs value vs unit value (the hypothesis)
  2. `fig2_elasticity_decomposition` — expenditure elasticity = quantity + quality
  3. `fig3_quantity_by_quartile` — quantity per adult equivalent rises with income
  4. `fig4_unitvalue_by_quartile` — unit value (quality) rises with income

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
