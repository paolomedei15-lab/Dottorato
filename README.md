# Food consumption and quality upgrading in Tanzania — first-stage analysis

R script: **`01_first_stage_analysis.R`**

This is the first stage of a study of animal-source-food consumption and
quality upgrading using the Tanzania National Panel Survey (NPS) waves 3, 4
and 5 household master files.

## What the script does

1. **Reads and harmonises** the three master files (Wave 3, 4, 5). Each
   `HH_Data` sheet has two header rows (section + full variable label); columns
   are selected by matching text tokens in those labels (which contain the
   survey codes, e.g. `itemcode=801]`).
2. **Descriptive statistics** (survey-weighted):
   - household size, adult equivalents, rural/urban shares, livestock ownership;
   - per item: share consuming, mean quantity and expenditure at **both
     household and adult-equivalent level**, and mean unit value.
3. **Distributional patterns** by **quintiles, quartiles and terciles** of
   welfare, for **all / rural / urban** households, for quantity, expenditure
   and unit value — including **Q5/Q1 ratios** and figures.
4. **Elasticity decomposition** (`output/tables/05_elasticities.csv`):
   `ln(expenditure) = ln(quantity) + ln(unit value)` estimated as three
   log-log Engel regressions on log real expenditure per adult equivalent,
   giving `eps_expenditure = eps_quantity + eps_quality`, with a one-sided
   test of **H0: eps_quality = 0 vs H1: eps_quality > 0** (robust HC1 SE).
5. **"When does quality start to matter?"** — quality elasticity by welfare
   quintile, plus a per-wave threshold income.

## Items

Goat meat (801), beef (802), pork (803), chicken & poultry (804), eggs (807),
fresh milk (901).

> **Processed milk is not available** in these master files (no canned/processed
> milk item exists), so it is excluded. Six items are analysed.

## Key methodological choices

| Choice | Decision |
|---|---|
| Welfare measure & elasticity budget | Real total monthly expenditure **per adult equivalent** |
| Pooling | Three waves **pooled with wave fixed effects** for elasticities |
| Survey weights | **Weighted** descriptives, **unweighted** regressions |
| Units | meats → **kg** (g→kg); eggs → **pieces**; fresh milk → **litres** (ml→litre); non-standard units dropped |
| Unit value | purchase expenditure ÷ purchased quantity (TSH per kg/litre/piece) |

## Two important data caveats

- **Wave 5 splits poultry** into Chicken (8041) + Other poultry (8042) in its
  main food module; these are **summed** to match the combined "chicken &
  poultry" (804) of Waves 3–4.
- **Real per-AE welfare is not level-comparable across waves** (Waves 3–4
  average ≈ 1.1–1.2M; Wave 5 ≈ 0.13M — different deflator base / reference
  period in the prepared files). Therefore **welfare groups are built within
  wave**, and elasticities rely on **wave fixed effects** (the slope is
  identified from within-wave variation, so it is unaffected by the level gap).

## Literature / method

The unit-value decomposition follows the standard approach for household survey
data: Deaton (1988, *Quality, Quantity, and Spatial Variation of Price*);
Cox & Wohlgenant (1986); Gibson & Kim (quality and unit values). Because
`ln(expenditure) = ln(quantity) + ln(unit value)` holds exactly in the same
sample with the same regressors, OLS guarantees the additive identity
`eps_expenditure = eps_quantity + eps_quality`.

## Indicative results (from a validation run)

- Household size ≈ 4.8; ≈ 66% rural; ≈ 48% own livestock.
- Expenditure elasticities ≈ 0.44–0.59 (all < 1, as expected for animal foods).
- **Quality elasticities positive and significant for every item** (≈ 0.05–0.15;
  pooled ≈ 0.10, t ≈ 10): **H0 rejected — quality upgrading is present.**
- Quality elasticity is **not significant in the poorest quintile but becomes
  significant from the 2nd quintile upward and rises with income** — i.e.
  quality starts to matter once households move above the bottom 20%.

## How to run

```r
# install.packages(c("readxl","dplyr","tidyr","stringr","ggplot2",
#                     "purrr","broom","sandwich","lmtest","scales"))
# 1. Edit the three file paths at the top of 01_first_stage_analysis.R
# 2. source("01_first_stage_analysis.R")
```

Outputs are written to `output/tables/` (CSV) and `output/figures/` (PNG).
