# Food consumption and quality upgrading in Tanzania
### First-stage analysis — final report

*Data: Tanzania National Panel Survey (NPS), waves 3, 4 and 5 (pooled).
Script: `01_first_stage_analysis.R`. All standard errors clustered by region.*

> **Sintesi (IT).** I prodotti di origine animale in Tanzania sono beni normali
> ma "necessari" sul margine intensivo (elasticità di spesa 0.37–0.55). La spesa
> cresce col reddito soprattutto via **quantità** (~80–88%), ma esiste una
> componente di **qualità positiva e statisticamente significativa per tutti i
> prodotti** (≈0.04–0.12): **H₀ (nessun upgrading di qualità) è rifiutata.** Non
> esiste però una "soglia" di reddito netta: l'upgrading di qualità è presente in
> tutta la distribuzione, anche tra i più poveri. I risultati sono robusti a
> trimming, clustering e stima per singola ondata.

---

## 1. Framework

For each item the budget identity is exact:

```
expenditure = quantity × unit value   ⇒   ln(exp) = ln(q) + ln(unit value)
```

Regressing each term on log real expenditure per adult equivalent (the welfare
measure *W*) gives the Deaton (1988) decomposition

```
ε_expenditure = ε_quantity + ε_quality
```

and we test **H₀: ε_quality = 0** vs **H₁: ε_quality > 0** (one-sided).
The "quality" component is the elasticity of the **unit value** (price paid per
kg/litre/piece) with respect to welfare: richer households paying more per
physical unit is interpreted as buying higher quality within the item.

## 2. Data and sample

- 13,071 households pooled (Y3: 5,010; Y4: 3,352; Y5: 4,709).
- Six animal-source items: goat meat (801), beef (802), pork (803),
  chicken & poultry (804; Y5 = 8041+8042), eggs (807), fresh milk (901).
  **Processed milk is not in these files** and is excluded.
- Units harmonised: meats → kg, eggs → pieces, milk → litres; non-standard
  units dropped. Unit value = purchase value ÷ purchased quantity.
- Welfare = **real total expenditure per adult equivalent**. Because its *level*
  is not comparable across waves (Y3/Y4 ≈ 1.1–1.2 M; Y5 ≈ 0.13 M — different
  deflator base), welfare **groups are built within wave** and the regressions
  use **wave fixed effects**.
- Descriptive statistics are survey-weighted; regressions are unweighted.

## 3. Descriptive statistics

**Households (pooled, weighted):** mean size ≈ 4.8, ≈ 3.85 adult equivalents,
**67% rural / 33% urban**, **≈ 49% own livestock**. Stable across the three waves.

**Items (consumers only, per adult equivalent, 7-day recall):**

| Item | % consuming | Qty / AE | Exp / AE (TSH) | Median unit value (TSH) |
|---|---:|---:|---:|---:|
| Beef | 36.0 | 0.37 kg | 2,040 | 6,000 /kg |
| Fresh milk | 25.9 | 1.09 L | 1,065 | 1,000 /L |
| Eggs | 17.1 | 2.08 pc | 770 | 300 /pc |
| Chicken & poultry | 16.5 | 0.45 kg | 3,000 | ~6,700 /kg |
| Goat meat | 10.8 | 0.47 kg | 1,620 | 5,000 /kg |
| Pork | 4.7 | 0.31 kg | 1,670 | 5,000 /kg |

Unit values are economically realistic for Tanzania (after trimming data-entry
errors — e.g. one beef record at 10,000,000 TSH/kg).

**Q5/Q1 gradients (richest vs poorest quintile, per AE):** quantity ratios
≈ 1.9–3.5, expenditure ratios ≈ 3.2–4.6, **unit-value ratios ≈ 1.2–2.3**
(largest for chicken). So as income rises, households buy **more** and pay a
**modest premium per unit** — both quantity and quality respond, quantity more.

## 4. Elasticity decomposition (main result)

Per-item regressions of ln(exp), ln(quantity) and ln(unit value) on ln(W) with
controls (log adult-eq, rural, wave FE, region FE), region-clustered SEs:

| Item | ε_expenditure | ε_quantity | ε_quality | p (1-sided) |
|---|---:|---:|---:|---:|
| Goat meat | 0.37 | 0.30 | **0.067** | <0.001 |
| Beef | 0.44 | 0.40 | **0.038** | <0.001 |
| Pork | 0.42 | 0.33 | **0.087** | <0.001 |
| Chicken & poultry | 0.44 | 0.31 | **0.125** | <0.001 |
| Eggs | 0.45 | 0.38 | **0.067** | <0.001 |
| Fresh milk | 0.55 | 0.47 | **0.073** | <0.001 |
| **All items (pooled, item FE)** | **0.46** | **0.39** | **0.076** | <0.001 |

**Conclusion: H₀ is rejected for every item — quality upgrading is present.**
But the elasticity of expenditure is dominated by **quantity** (≈ 80–88%); the
quality component, while significant, is smaller (≈ 12–25% of the response).

## 5. Does quality matter more as income rises?

Quality elasticity by welfare quintile (pooled, item FE, clustered SEs):

| Quintile | Q1 | Q2 | Q3 | Q4 | Q5 |
|---|---:|---:|---:|---:|---:|
| ε_quality | 0.073 | 0.048 | 0.057 | 0.070 | 0.075 |
| significant (>0) | yes | yes | yes | yes | yes |
| quality share of ε_exp | 21% | 12% | 13% | 16% | 17% |

**There is no clean income threshold.** Quality upgrading is positive and
significant in *every* quintile, including the poorest, and the profile is
roughly flat / mildly U-shaped — it does **not** switch on at a particular
income. (The earlier "threshold at Q2/Q1" was an artifact; estimating an income
slope *within* a quintile uses exactly the variation a quintile compresses, so
those numbers are fragile and were dropped.)

## 6. Robustness checks performed

| Check | Result |
|---|---|
| **Adding-up** ε_exp = ε_q + ε_quality | holds exactly (residual 0) |
| **Outlier trimming** (none / 1–99 / 2.5–97.5 / winsorize) | pooled ε_quality stays 0.071–0.089 — stable |
| **SEs HC1 vs region-clustered** | very similar; all items remain significant |
| **Estimation by wave** | ε_exp 0.54 / 0.58 / 0.43; ε_quality 0.089 / 0.089 / 0.065 — consistent, pooling OK |
| **Pooled needs item FE** | without item FE the pooled ε_quality flips to −0.03 → per-item estimates are the headline, pooled is only a summary |
| **Small cells** | urban pork (n_Q1=2) and urban eggs (n_Q1=0) Q5/Q1 ratios suppressed (was the source of the absurd 8–11× ratios / NA) |

## 7. Where to be cautious

1. **Magnitude of expenditure elasticities (0.37–0.55) is low** vs textbook
   animal-food values. This is because they are **intensive-margin,
   conditional-on-purchase** elasticities w.r.t. *total expenditure per AE*. The
   *extensive* margin (whether to consume at all) is excluded; adding it would
   raise the total income response. They are internally consistent, not a bug.
2. **Total expenditure is a regressor that contains the item** (part–whole), a
   known feature of the Deaton approach. For these small-budget-share items the
   bias is minor, but the levels should be read as elasticities w.r.t. the
   welfare proxy, not structural income elasticities.
3. **Cross-wave welfare levels are not comparable** (deflator base differs);
   handled via within-wave groups + wave FE, but absolute monetary "income
   thresholds" across waves are not meaningful and are not reported.
4. **Unit value = quality is an assumption.** Unit-value variation also reflects
   bulk discounts, regional price differences and measurement error. Region FE
   and trimming mitigate this; it cannot be fully removed without item
   attributes the survey does not record.
5. **Thin items** (pork, goat meat) have smaller samples and wider intervals;
   chicken's higher ε_quality is plausible (local vs improved/whole birds) but
   rests on fewer observations.

## 8. Conclusions

- Animal-source foods are normal goods on the intensive margin; the income
  response works **mainly through quantity**.
- **Quality upgrading is real and statistically robust** for all six items
  (H₀ rejected), but **economically modest** (≈ 12–25% of the expenditure
  response) and **present throughout the income distribution**, not only at the
  top.
- The strongest quality response is for **chicken & poultry**; the weakest for
  **beef**.

### Output files
Tables `01`–`09` in `output/tables/` (note `08` is now *quality share by
quintile*, `09` is *elasticities by wave*); 15 figures in `output/figures/`.
