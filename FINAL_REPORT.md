# Food consumption and quality upgrading in Tanzania
### First-stage analysis — results report

*Data: Tanzania National Panel Survey (NPS), waves 3, 4 and 5 (pooled,
13,071 households). Script: `analysis.R`. Standard errors clustered by region.*

---

## 1. Framework

For each animal-source food the budget identity is exact:

$$\ln(\text{expenditure}) = \ln(\text{quantity}) + \ln(\text{unit value}).$$

Regressing each term on log real expenditure per adult equivalent (the welfare
measure *W*) gives the Deaton (1988) decomposition

$$\varepsilon_{\text{expenditure}} = \varepsilon_{\text{quantity}} + \varepsilon_{\text{quality}},$$

where the quality elasticity is the elasticity of the unit value (the price paid
per kg / litre / piece) with respect to *W*. The hypothesis tested is
`H0: ε_quality = 0` vs `H1: ε_quality > 0`.

**Items:** goat meat, beef, pork, chicken & poultry, eggs, fresh milk (processed
milk is not available in the files). **Welfare** = real total expenditure per
adult equivalent. Descriptive statistics are survey-weighted; regressions use
OLS with region-clustered standard errors. Units are harmonised (meats in kg,
eggs in pieces, milk in litres); unit values use the purchased quantity and
value; extreme outliers are trimmed (1st–99th percentile within item × wave).

## 2. Sample

| Wave | Households | HH size | Adult eq. | Rural % | Urban % | Livestock % | Welfare/AE |
|---|---:|---:|---:|---:|---:|---:|---:|
| Y3 | 5,010 | 4.89 | 3.96 | 68.8 | 31.2 | 49.5 | 1,155,797 |
| Y4 | 3,352 | 4.74 | 3.79 | 65.5 | 34.5 | 47.1 | 1,247,107 |
| Y5 | 4,709 | 4.85 | 3.82 | 67.6 | 32.4 | 50.0 | 127,215 |
| Pooled | 13,071 | 4.83 | 3.85 | 67.2 | 32.8 | 48.9 | 785,830 |

The welfare aggregate is deflated to a wave-specific base, so its *level* is not
comparable across waves (Wave 5 is about an order of magnitude lower). Welfare
groups are therefore built within wave and the pooled regressions use wave fixed
effects.

## 3. Descriptive statistics (per adult equivalent, consumers)

| Item | % consuming | Quantity/AE | Expenditure/AE (TSH) | Median unit value (TSH) |
|---|---:|---:|---:|---:|
| Goat meat | 10.8 | 0.47 kg | 1,621 | 5,000 /kg |
| Beef | 36.0 | 0.37 kg | 2,038 | 6,000 /kg |
| Pork | 4.7 | 0.31 kg | 1,673 | 5,000 /kg |
| Chicken & poultry | 16.5 | 0.45 kg | 2,999 | 6,725 /kg |
| Eggs | 17.1 | 2.08 pcs | 771 | 300 /piece |
| Fresh milk | 25.9 | 1.09 L | 1,065 | 1,000 /litre |

Across welfare quartiles, quantity, expenditure and unit value all rise with
income (figures 08, 09, 10).

## 4. Elasticity decomposition (main result)

Per-item regressions on log welfare per AE with log adult equivalents, a rural
dummy, region and wave fixed effects; region-clustered standard errors.

| Item | ε_expenditure | ε_quantity | ε_quality | p (one-sided) | Reject H0 |
|---|---:|---:|---:|---:|:--:|
| Goat meat | 0.368 | 0.301 | 0.067 | <0.001 | yes |
| Beef | 0.435 | 0.397 | 0.038 | <0.001 | yes |
| Pork | 0.420 | 0.332 | 0.087 | <0.001 | yes |
| Chicken & poultry | 0.438 | 0.313 | 0.125 | <0.001 | yes |
| Eggs | 0.449 | 0.383 | 0.067 | <0.001 | yes |
| Fresh milk | 0.546 | 0.473 | 0.073 | <0.001 | yes |
| **All items (pooled)** | **0.462** | **0.386** | **0.076** | <0.001 | yes |

The expenditure elasticity exceeds the quantity elasticity for every item; the
quality elasticity is positive and statistically significant in all cases, so H0
is rejected. Quantity accounts for the larger share of the expenditure response.

## 5. Quality elasticity across the distribution and by subgroup

- **By welfare quartile** (`09_quality_elasticity_by_quartile.csv`): positive and
  significant in all four income quartiles.
- **By subgroup** (`08_elasticities_by_subgroup.csv`): quality elasticity
  ≈ 0.089 rural, 0.056 urban, 0.085 livestock-owners, 0.088 non-owners.

### Heterogeneity tests (`10_heterogeneity_tests.csv`, figure 12)

Income is interacted with the group dummy; the interaction coefficient is the
difference in the quality elasticity.

- **Urban vs rural:** difference ≈ −0.012, p ≈ 0.23 — not statistically
  significant (quality upgrading does not differ systematically by residence).
- **Livestock owner vs non-owner:** difference ≈ −0.007, p < 0.01 —
  statistically significant but economically negligible.

### Robustness to the outlier rule (`11_sensitivity_elasticities.csv`, figure 13)

The unit-value method is sensitive to extreme values, so the elasticities are
re-estimated under three cleaning rules (within item × wave). The quality
elasticity stays positive and highly significant under all three:

| Rule | ε_expenditure | ε_quantity | ε_quality | t |
|---|---:|---:|---:|---:|
| Trim 1/99 (baseline) | 0.42 | 0.34 | 0.075 | 12.5 |
| Trim 2.5/97.5 | 0.35 | 0.28 | 0.064 | 10.4 |
| Winsorize 1/99 | 0.46 | 0.38 | 0.087 | 16.3 |

Expenditure elasticity exceeds quantity elasticity in every case, so the
quality-upgrading conclusion does not depend on the outlier rule.

The quality elasticity is positive and significant in all four income
quartiles, so quality influences purchases across the whole distribution rather
than starting at a single income threshold; the income level of each quartile
is in `01_sample_descriptives.csv`.

## 6. Outputs

Outputs are produced in a logical order.
- **Tables** (`output/tables/`): `01_sample_descriptives`,
  `02_consumption_expenditure_by_product`, `03_consumption_by_wave`,
  `04_sources_shares`, `05_group_means_by_quartile`,
  `06_inequality_ratios_Q4_Q1`, `07_elasticities_by_product`,
  `08_elasticities_by_subgroup` (rural / urban / livestock),
  `09_quality_elasticity_by_quartile`, `10_heterogeneity_tests`,
  `11_sensitivity_elasticities`, plus `00_data_cleaning_report`.
- **Figures** (`output/figures/`): `fig01`–`fig13` (incl. the sensitivity check) — share of consumers,
  product descriptives, consumption trends across waves, household composition
  by wave, urban/rural consumption, sources, consumption and expenditure by
  quartile, the six-panel quantity/expenditure/unit-value comparison, basket
  composition, the quality effect, and the heterogeneity test.

## 7. Notes and limitations

- Elasticities are intensive-margin, conditional-on-purchase elasticities with
  respect to total expenditure per adult equivalent.
- Unit value is used as the proxy for quality; it can also reflect spatial price
  differences and measurement error (mitigated by region fixed effects and
  trimming).
- Cross-wave welfare levels are not comparable (deflator base differs), so
  groups are built within wave and the regressions use wave fixed effects.

## How to run

```r
# install.packages(c("readxl","dplyr","tidyr","stringr","ggplot2",
#                     "purrr","broom","sandwich","lmtest","scales"))
# edit the three file paths at the top of analysis.R, then:
source("analysis.R")
```
