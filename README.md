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

## Indicative results (validation run, region-clustered SEs)

- Household size ≈ 4.8; ≈ 67% rural; ≈ 49% own livestock.
- Expenditure elasticities ≈ 0.37–0.55 — these are **intensive-margin,
  conditional-on-purchase** elasticities (below textbook unconditional ASF
  values, which also include the decision to start consuming).
- **Quality elasticities positive and significant for every item** (≈ 0.04–0.12;
  pooled ≈ 0.08): **H0 rejected — quality upgrading is present.** Quantity is
  the larger component (~80–88% of the expenditure response).
- **No clean income threshold:** the quality elasticity is positive and
  significant in *every* quintile (including the poorest) and is roughly flat,
  so quality upgrading happens across the whole distribution rather than
  switching on at a particular income.

See `FINAL_REPORT.md` for the full write-up and the robustness checks.

## Figures produced (`output/figures/`)

1. `fig1_Q5Q1_ratios` – Q5/Q1 ratios by item (quantity, expenditure, unit value)
2. `fig2_unitvalue_quintiles` – unit-value gradient across quintiles
3. `fig3_rural_urban_unitvalue` – rural vs urban unit value
4. `fig4_elasticity_decomposition` – expenditure elasticity split into quantity + quality
5. `fig5_quality_by_quintile` – quality elasticity by quintile (when quality switches on)
6. `fig6_household_composition` – rural/urban/livestock shares by wave
7. `fig7_participation` – share of households consuming each item
8. `fig8_quantity_quintiles` – quantity per AE across quintiles
9. `fig9_expenditure_quintiles` – expenditure per AE across quintiles
10. `fig10_Q5Q1_by_area` – Q5/Q1 ratios faceted by all / rural / urban
11. `fig11_rural_urban_expenditure` (+ `fig11b` quantity) – rural vs urban per AE
12. `fig12_engel_curves` – Engel curves (log expenditure vs log welfare) by item
13. `fig13_quality_gradient` – log unit value vs log welfare by item (quality slope)
14. `fig14_unitvalue_groups` – unit-value gradient across quintiles, quartiles, terciles
15. `fig15_quality_forest` – quality elasticity by item with 95% CI (hypothesis test)
16. `fig16_quantity_source_shares` (+ `fig16b` absolute) – quantity by source (purchased/own/gifts)
17. `fig17_source_by_livestock` – quantity source split by livestock ownership
18. `fig18_expenditure_by_livestock` – expenditure per AE by livestock ownership
19. `fig19_quality_share` – quality's share of the expenditure elasticity by group
20. `fig20_unitvalue_rural_urban` – unit-value gradient, rural vs urban (within-area quintiles)

## Final report (generated in R)

`02_report.Rmd` knits a self-contained HTML report from the saved tables and
figures. After running the analysis script:

```r
rmarkdown::render("02_report.Rmd")   # produces 02_report.html
```

`FINAL_REPORT.md` is the static written version with the robustness discussion.

## How to run

```r
# install.packages(c("readxl","dplyr","tidyr","stringr","ggplot2",
#                     "purrr","broom","sandwich","lmtest","scales"))
# 1. Edit the three file paths at the top of 01_first_stage_analysis.R
# 2. source("01_first_stage_analysis.R")
```

Outputs are written to `output/tables/` (CSV) and `output/figures/` (PNG).
