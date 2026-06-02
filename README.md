# Quality upgrading in animal-source foods — Tanzania (NPS waves 3–5)

**Hypothesis.** As household income rises, expenditure on animal products rises
*faster* than quantity, because households trade up to higher-quality items.
With `ln(expenditure) = ln(quantity) + ln(unit value)` the expenditure elasticity
splits as `ε_expenditure = ε_quantity + ε_quality`; the **wedge** (expenditure −
quantity) is the quality effect. We test `H0: ε_quality = 0` vs `H1: > 0`.

## How to run

```r
# install.packages(c("readxl","dplyr","tidyr","ggplot2","openxlsx","sandwich","lmtest"))
# edit the three file paths at the top of analysis.R, then:
source("analysis.R")
```

Outputs: **`results.xlsx`** (9 sheets) and **`figures/`** (12 charts, all six
products shown together; different units made comparable with indices and logs).

## Outputs mapped to the research outline

| # | Outline item | Output |
|---|---|---|
| 1 | Sample descriptive statistics (full / urban-rural / livestock) | sheet `1_Sample` |
| 2 | Product-level descriptive statistics | sheet `2_Products`, fig `01_participation` |
| 3 | Consumption trends across survey waves | sheet `3_Consumption_by_wave`, figs `03`, `03b` (expenditure) |
| 4 | Urban–rural consumption patterns | sheet `4_Urban_rural`, fig `04` |
| 5 | Sources of food consumption (purchase / own / gifts) | sheet `5_Sources`, fig `05` |
| 6 | Consumption by income quartile (line) | fig `06_consumption_by_quartile` |
| 7 | Expenditure by income quartile (line) | fig `07_expenditure_by_quartile` |
| 8 | Consumption vs expenditure (six panels, + unit value) | figs `08_…panels`, `08b_focus_fresh_milk` |
| 9 | Inequality ratios Q4/Q1 (quantity, expenditure, unit value) | sheet `9_Inequality_ratios`, fig `09` |
| 10 | Quantity vs expenditure elasticities | sheet `10_11_Elasticities`, fig `10` |
| 11 | The quality effect (the wedge) | sheet `10_11_Elasticities`, fig `11_quality_effect` |
| 12 | Heterogeneity (urban/rural, livestock) | sheet `12_Heterogeneity`, fig `12` |
| 13 | Key question — does it differ by residence / livestock? | sheet `13_Het_tests` |

## Method notes

- Items: goat meat, beef, pork, chicken & poultry, eggs, fresh milk.
- Units harmonised: meats → kg, eggs → pieces, milk → litres (never mixed on a
  raw axis: figures use indices, Q1 = 100, or logs).
- Welfare = real total expenditure per adult equivalent; quartiles built within
  wave. Descriptives are survey-weighted; unit values use the median; extreme
  outliers trimmed (1st–99th percentile within item×wave).
- Consumption is valued at the unit price (own production at the item's median
  price) so quantity and expenditure are on the same basis.
- Across waves, **quantity per AE is flat** (consumption is stable); nominal
  expenditure rises with inflation. The deflated **income** measure is on a
  wave-specific base, so its *level* is ~10× lower in Wave 5 — this is why
  quartiles are built within wave and does **not** mean consumption fell.
- Elasticities: Deaton unit-value method — OLS of log quantity, expenditure and
  unit value on log income per AE + log adult eq + urban + wave; HC1 robust SE.
- Heterogeneity (point 13): income is interacted with the urban and livestock
  dummies; the interaction coefficient is the difference in the quality
  elasticity (two-sided test).

## Main results

- Expenditure elasticity exceeds quantity elasticity for **every** product; the
  quality wedge is positive and significant (pooled ≈ 0.12) → **H0 rejected**.
- **Quality upgrading does not differ significantly between urban and rural
  households** (interaction p ≈ 0.23).
- **Livestock ownership** changes the quality elasticity by a statistically
  significant but **economically negligible** amount (≈ −0.007).
