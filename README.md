# Food consumption and quality upgrading in Tanzania (NPS waves 3–5)

First-stage analysis: descriptive statistics and an expenditure-elasticity
decomposition (quantity vs. quality) on the Tanzania National Panel Survey.

Using the identity `ln(expenditure) = ln(quantity) + ln(unit value)`, the
expenditure elasticity splits as `ε_expenditure = ε_quantity + ε_quality`; we
test `H0: ε_quality = 0` vs `H1: ε_quality > 0`.

## How to run

```r
# install.packages(c("readxl","dplyr","tidyr","stringr","ggplot2",
#                     "purrr","broom","sandwich","lmtest","scales"))
# edit the three file paths at the top of analysis.R, then:
source("analysis.R")
```

Outputs are written to `output/tables/` (CSV) and `output/figures/`
(`fig1`–`fig20`). A written summary is in **`FINAL_REPORT.md`**.

## Method notes

- Items: goat meat, beef, pork, chicken & poultry, eggs, fresh milk (processed
  milk is not in the master files).
- Units harmonised: meats → kg, eggs → pieces, milk → litres.
- Welfare = real total expenditure per adult equivalent; welfare groups are
  built within wave (levels are not comparable across waves), and the pooled
  regressions use wave fixed effects.
- Descriptive statistics are survey-weighted; unit values use the median;
  outliers are trimmed (1st–99th percentile within item × wave).
- Elasticities: OLS of log quantity, expenditure and unit value on log welfare
  per AE with log adult equivalents, a rural dummy, region and wave fixed
  effects; region-clustered standard errors.
