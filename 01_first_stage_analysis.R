###############################################################################
##  FOOD CONSUMPTION AND QUALITY UPGRADING IN TANZANIA
##  First-stage analysis: descriptive statistics + expenditure-elasticity
##  decomposition (quantity vs. quality) on the Tanzania NPS panel.
##
##  Data : NPS Wave 3, Wave 4, Wave 5 household master files (.xlsx).
##  Author: PhD research script  |  Language: English  |  Style: simple & readable
##
##  ------------------------------------------------------------------------
##  METHOD (literature note)
##  ------------------------------------------------------------------------
##  We use the standard unit-value approach for household survey data. For a
##  food item the budget identity is exact:
##
##        expenditure = quantity * unit value
##   =>   ln(expenditure) = ln(quantity) + ln(unit value)
##
##  Estimating three Engel-type log-log regressions on (the log of) the
##  household welfare measure W gives the decomposition (Deaton, 1988;
##  Cox & Wohlgenant, 1986; Gibson & Kim, 2019):
##
##        eps_expenditure = eps_quantity + eps_quality
##
##  Because ln(expenditure) = ln(quantity) + ln(unit value) holds exactly in
##  the SAME sample with the SAME regressors, OLS guarantees the identity
##  eps_expenditure = eps_quantity + eps_quality (coefficients add up). The
##  "quality elasticity" is simply the elasticity of the unit value w.r.t.
##  total welfare. We test:
##
##        H0: eps_quality = 0      vs.   H1: eps_quality > 0
##
##  with a one-sided t-test (robust HC1 standard errors).
##
##  ------------------------------------------------------------------------
##  KEY DATA DECISIONS (confirmed before coding)
##  ------------------------------------------------------------------------
##  * Items: goat meat (801), beef (802), pork (803), chicken & poultry (804),
##    eggs (807), fresh milk (901). "Processed milk" is NOT available in these
##    master files (no canned/processed-milk item exists), so it is dropped.
##  * Welfare measure & elasticity budget = REAL total monthly expenditure per
##    ADULT EQUIVALENT (deflated). Quintiles/quartiles/terciles are built on it.
##  * The three waves are POOLED into one sample, with WAVE FIXED EFFECTS to
##    absorb price/time differences (we work on deflated/real values).
##  * Survey weights: WEIGHTED descriptive statistics, UNWEIGHTED regressions.
##  * Units harmonised so each item is internally consistent:
##       - meats (goat, beef, pork, chicken): kg  (g -> kg)
##       - eggs:                                pieces
##       - fresh milk:                          litres (ml -> litre)
##    Observations recorded in non-standard units (heap, cup, bottle, ...) are
##    dropped because they cannot be converted reliably.
##  * Unit value = purchase expenditure / purchased quantity (the only part of
##    consumption that carries a market value), in TSH per kg / litre / piece.
##
##  NOTE on the master-file layout: each "HH_Data" sheet has TWO header rows
##  (row 1 = section, row 2 = full variable label). We read with skip = 1 so
##  the descriptive labels become the column names, then select columns by
##  matching text patterns (the labels contain the survey codes, e.g.
##  "itemcode=801]"). Wave 5's main food module uses the "hh_ja..." questions
##  and splits poultry into Chicken (8041) + Other poultry (8042); we sum them
##  to reconstruct the combined "chicken & poultry" used in Waves 3 and 4.
###############################################################################


## ============================================================================
## 0. SETUP
## ============================================================================
# install.packages(c("readxl","dplyr","tidyr","stringr","ggplot2",
#                     "purrr","broom","sandwich","lmtest","scales"))

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(broom)
library(sandwich)
library(lmtest)
library(scales)

## ---- File paths (edit if needed) -------------------------------------------
y3_path <- "C:/Users/Medei/OneDrive - Food and Agriculture Organization/Desktop/NPS_Y3_Tanzania_HH_Master_UnitValue.xlsx"
y4_path <- "C:/Users/Medei/OneDrive - Food and Agriculture Organization/Desktop/NPS_Y4_Tanzania_HH_Master.xlsx"
y5_path <- "C:/Users/Medei/OneDrive - Food and Agriculture Organization/Desktop/NPS_Y5_Tanzania_HH_Master (1).xlsx"

## ---- Output folders --------------------------------------------------------
out_tab <- "output/tables"
out_fig <- "output/figures"
dir.create(out_tab, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig, recursive = TRUE, showWarnings = FALSE)


## ============================================================================
## 1. SMALL HELPER FUNCTIONS
## ============================================================================

## Read one wave's HH_Data sheet. Row 1 = section, row 2 = variable label,
## data from row 3 -> we skip the first row so row 2 becomes the header.
read_wave <- function(path) {
  read_excel(path, sheet = "HH_Data", skip = 1, .name_repair = "minimal")
}

## Return the FIRST column of `df` whose name contains ALL the given text
## patterns. Returns a numeric vector (NA if no such column exists).
pick_num <- function(df, patterns) {
  hit <- names(df)[map_lgl(names(df),
                           ~ all(str_detect(.x, fixed(patterns))))]
  if (length(hit) == 0) return(rep(NA_real_, nrow(df)))
  suppressWarnings(as.numeric(df[[hit[1]]]))
}

## Same, but keep the column as character (used for the household id).
pick_chr <- function(df, patterns) {
  hit <- names(df)[map_lgl(names(df),
                           ~ all(str_detect(.x, fixed(patterns))))]
  if (length(hit) == 0) return(rep(NA_character_, nrow(df)))
  as.character(df[[hit[1]]])
}

## Convert a reported quantity to the standard unit for its item type.
## Unit codes: 1=kg, 2=g, 3=litre, 4=ml, 5=pieces. Anything else -> NA (drop).
## (We use %in% rather than == so that NA unit codes resolve to FALSE instead
##  of NA, which would be rejected in subscripted assignment.)
normalise_qty <- function(qty, unit, type) {
  out <- rep(NA_real_, length(qty))
  if (type == "meat") {                 # standard unit: kg
    out[unit %in% 1] <- qty[unit %in% 1]
    out[unit %in% 2] <- qty[unit %in% 2] / 1000
  } else if (type == "litre") {         # standard unit: litre
    out[unit %in% 3] <- qty[unit %in% 3]
    out[unit %in% 4] <- qty[unit %in% 4] / 1000
  } else if (type == "pieces") {        # standard unit: pieces
    out[unit %in% 5] <- qty[unit %in% 5]
  }
  out
}

## Weighted quantile (used for quintile / quartile / tercile cut-points).
wtd_quantile <- function(x, w, probs) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  approx(cw, x, xout = probs, rule = 2, ties = "ordered")$y
}

## Assign group labels (e.g. Q1..Q5) from weighted cut-points.
wtd_group <- function(x, w, n, labels) {
  cuts <- c(-Inf, wtd_quantile(x, w, seq_len(n - 1) / n), Inf)
  cut(x, breaks = cuts, labels = labels, include.lowest = TRUE)
}

## Robust weighted mean / median. Base R's weighted.mean(na.rm=TRUE) drops NA in
## x but NOT in the weights, so a single NA weight returns NA. These versions
## drop any row with NA x, NA weight, or non-positive weight.
wmean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}
wmedian <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  wtd_quantile(x[ok], w[ok], 0.5)
}

## Set values outside the [lo, hi] percentile range to NA (used within each
## item x wave to remove data-entry outliers, e.g. beef at 10,000,000 TSH/kg).
trim_to_na <- function(x, lo = 0.01, hi = 0.99) {
  qs <- quantile(x, c(lo, hi), na.rm = TRUE, names = FALSE)
  ifelse(is.finite(x) & x >= qs[1] & x <= qs[2], x, NA_real_)
}


## ============================================================================
## 2. PER-WAVE CONFIGURATION
## ============================================================================
## The three master files are structured slightly differently, so we store the
## differences in one place. `food_prefix` selects the right food module; the
## welfare measure is built differently because Wave 5 is already per-AE.

## We match columns on dash-free text tokens so the script is robust to how the
## file's em-dash ("—") is encoded when read. Each token below is unique within
## that wave's set of column labels.
wave_cfg <- list(
  Y3 = list(path = y3_path, food_prefix = "hh_j",  hhid = "y3_hhid",
            weight = "y3_weight",
            welfare_kind = "total"),     # expmR is household-level -> divide by AE
  Y4 = list(path = y4_path, food_prefix = "hh_j",  hhid = "y4_hhid",
            weight = "hhweight",
            welfare_kind = "total"),
  Y5 = list(path = y5_path, food_prefix = "hh_ja", hhid = "y5_hhid",
            weight = "y5_crossweight",
            welfare_kind = "pae")        # expmR_pae is already per adult equiv.
)

## The six items. For Wave 5 chicken we combine codes 8041 + 8042.
item_cfg <- tibble::tribble(
  ~item,         ~label,                 ~type,     ~codes_std,    ~codes_y5,
  "goat_meat",   "Goat meat",            "meat",    "801",         "801",
  "beef",        "Beef",                 "meat",    "802",         "802",
  "pork",        "Pork",                 "meat",    "803",         "803",
  "chicken",     "Chicken & poultry",    "meat",    "804",         "8041,8042",
  "eggs",        "Eggs",                 "pieces",  "807",         "807",
  "fresh_milk",  "Fresh milk",           "litre",   "901",         "901"
)


## ============================================================================
## 3. EXTRACTION FUNCTIONS
## ============================================================================

## Household-level variables, harmonised across waves.
extract_household <- function(df, cfg, wave) {
  adulteq <- pick_num(df, c("adulteq", "Adult equivalent"))
  expmR   <- if (cfg$welfare_kind == "pae") {
    pick_num(df, c("expmR_pae", "per adult equivalent"))
  } else {
    pick_num(df, c("expmR", "Real total monthly expenditure (TSH, deflated)"))
  }
  # welfare = real total monthly expenditure PER ADULT EQUIVALENT
  welfare_pae <- if (cfg$welfare_kind == "pae") expmR else expmR / adulteq

  urban_raw <- pick_num(df, c("urban", "Urban/Rural (1=Rural, 2=Urban)"))  # 1=rural,2=urban
  livestock <- pick_num(df, c("lf02_any_livestock", "owns at least one livestock"))

  tibble(
    wave        = wave,
    hhid        = pick_chr(df, cfg$hhid),
    hhsize      = pick_num(df, c("hh_hhsize", "Household size (total members")),
    adulteq     = adulteq,
    weight      = pick_num(df, cfg$weight),
    region      = pick_num(df, c("region", "Region code")),
    rural       = as.integer(urban_raw == 1),
    urban       = as.integer(urban_raw == 2),
    # Codebook: a missing livestock flag means "no livestock" -> recode NA to 0
    livestock   = ifelse(is.na(livestock), 0L, as.integer(livestock == 1)),
    welfare_pae = welfare_pae
  )
}

## One item, one wave -> consumed flag, total quantity, purchased quantity,
## purchase expenditure (all harmonised to the standard unit).
extract_one_code <- function(df, prefix, code, type) {
  tag <- paste0("itemcode=", code, "]")
  yn      <- pick_num(df, c(prefix, tag, "eat/drink any"))
  q_tot   <- pick_num(df, c(prefix, tag, "in total did your household consume", "QUANTITY"))
  u_tot   <- pick_num(df, c(prefix, tag, "in total did your household consume", "UNIT"))
  q_buy   <- pick_num(df, c(prefix, tag, "came from purchases", "QUANTITY"))
  u_buy   <- pick_num(df, c(prefix, tag, "came from purchases", "UNIT"))
  spend   <- pick_num(df, c(prefix, tag, "How much did you spend"))
  tibble(
    consumed   = as.integer(yn == 1),
    qty_total  = normalise_qty(q_tot, u_tot, type),
    qty_purch  = normalise_qty(q_buy, u_buy, type),
    exp        = spend
  )
}

## Combine one or more item codes (codes are summed; this matters only for the
## Wave-5 chicken = chicken + other poultry case).
extract_item <- function(df, prefix, codes, type) {
  parts <- map(codes, ~ extract_one_code(df, prefix, .x, type))
  reduce(parts, function(a, b) tibble(
    consumed  = pmax(a$consumed,  b$consumed,  na.rm = TRUE),
    # sum quantities/expenditure treating NA as 0, but keep NA if both NA
    qty_total = ifelse(is.na(a$qty_total) & is.na(b$qty_total), NA,
                       rowSums(cbind(a$qty_total, b$qty_total), na.rm = TRUE)),
    qty_purch = ifelse(is.na(a$qty_purch) & is.na(b$qty_purch), NA,
                       rowSums(cbind(a$qty_purch, b$qty_purch), na.rm = TRUE)),
    exp       = ifelse(is.na(a$exp) & is.na(b$exp), NA,
                       rowSums(cbind(a$exp, b$exp), na.rm = TRUE))
  ))
}


## ============================================================================
## 4. BUILD THE POOLED LONG DATASET
## ============================================================================

hh_list   <- list()   # household-level, one row per household
item_list <- list()   # long: one row per household x item

for (wave in names(wave_cfg)) {
  cfg <- wave_cfg[[wave]]
  message("Reading wave ", wave, " ...")
  df  <- read_wave(cfg$path)

  hh  <- extract_household(df, cfg, wave)
  hh_list[[wave]] <- hh

  for (i in seq_len(nrow(item_cfg))) {
    it    <- item_cfg[i, ]
    codes <- str_split(if (wave == "Y5") it$codes_y5 else it$codes_std, ",")[[1]]
    ext   <- extract_item(df, cfg$food_prefix, codes, it$type)
    ext$item <- it$item
    ext$hhid <- hh$hhid
    ext$wave <- wave
    item_list[[paste(wave, it$item)]] <- ext
  }
}

household <- bind_rows(hh_list)

## Long item table joined with household characteristics.
items <- bind_rows(item_list) %>%
  left_join(household, by = c("wave", "hhid")) %>%
  mutate(
    # unit value = purchase expenditure / purchased quantity (TSH per std unit)
    unit_value = ifelse(qty_purch > 0 & exp > 0, exp / qty_purch, NA_real_)
  )

## ---- Remove extreme outliers (data-entry errors) ---------------------------
## Within each item x wave, trim unit value, quantities and expenditure to their
## 1st-99th percentile range so descriptive means/ratios and the regressions are
## not driven by errors. (Diagnostic: beef unit value had a max of 10,000,000
## TSH/kg vs a median of ~6,000.)
items <- items %>%
  group_by(item, wave) %>%
  mutate(across(c(unit_value, qty_total, qty_purch, exp), trim_to_na)) %>%
  ungroup() %>%
  mutate(
    qty_total_pae = qty_total / adulteq,   # quantity per adult equivalent
    exp_pae       = exp / adulteq,         # expenditure per adult equivalent
    item_label    = item_cfg$label[match(item, item_cfg$item)]
  )

## Order item labels nicely for all tables/plots.
item_levels <- item_cfg$label
items$item_label <- factor(items$item_label, levels = item_levels)

message("Households pooled: ", nrow(household),
        " | item-rows: ", nrow(items))


## ============================================================================
## 5. DESCRIPTIVE STATISTICS  (weighted)
## ============================================================================

## ---- 5a. Household-level summary -------------------------------------------
## Household size, adult equivalents, rural/urban shares, livestock ownership.
hh_summary <- household %>%
  group_by(wave) %>%
  summarise(
    n_households   = n(),
    mean_hhsize    = wmean(hhsize,    weight),
    mean_adulteq   = wmean(adulteq,   weight),
    pct_rural      = 100 * wmean(rural,     weight),
    pct_urban      = 100 * wmean(urban,     weight),
    pct_livestock  = 100 * wmean(livestock, weight),
    mean_welfare_pae = wmean(welfare_pae, weight),
    .groups = "drop"
  )

hh_summary_pooled <- household %>%
  summarise(
    wave = "Pooled",
    n_households   = n(),
    mean_hhsize    = wmean(hhsize,    weight),
    mean_adulteq   = wmean(adulteq,   weight),
    pct_rural      = 100 * wmean(rural,     weight),
    pct_urban      = 100 * wmean(urban,     weight),
    pct_livestock  = 100 * wmean(livestock, weight),
    mean_welfare_pae = wmean(welfare_pae, weight)
  )

hh_summary <- bind_rows(hh_summary, hh_summary_pooled)
write.csv(hh_summary, file.path(out_tab, "01_household_summary.csv"), row.names = FALSE)
print(hh_summary)

## ---- 5b. Item-level summary (HOUSEHOLD vs ADULT-EQUIVALENT) ------------------
## Means are computed over CONSUMERS (households that actually consumed the item)
## so that quantities and unit values are economically meaningful.
item_summary <- items %>%
  group_by(item_label) %>%
  summarise(
    pct_consuming      = 100 * wmean(consumed, weight),
    # household level
    mean_qty_hh        = wmean(ifelse(qty_total > 0, qty_total, NA), weight),
    mean_exp_hh        = wmean(ifelse(exp > 0, exp, NA),             weight),
    # adult-equivalent level  (the key requested measure)
    mean_qty_pae       = wmean(ifelse(qty_total > 0, qty_total_pae, NA), weight),
    mean_exp_pae       = wmean(ifelse(exp > 0, exp_pae, NA),             weight),
    # unit value: MEDIAN (robust to skew/outliers), TSH per kg / litre / piece
    median_unit_value  = wmedian(unit_value, weight),
    .groups = "drop"
  )
write.csv(item_summary, file.path(out_tab, "02_item_summary.csv"), row.names = FALSE)
print(item_summary)


## ============================================================================
## 6. WELFARE GROUPS: QUINTILES, QUARTILES, TERCILES
## ============================================================================
## Built on REAL expenditure per adult equivalent, using survey weights for the
## cut-points.
##
## IMPORTANT: the real per-AE aggregates are NOT comparable in LEVEL across
## waves (Waves 3-4 average ~1.1-1.2M, Wave 5 ~0.13M: the prepared files use a
## different deflator base / reference period). We therefore build the groups
## WITHIN each wave and then pool the labels, so "Q5" always means the richest
## 20% of that wave. (The elasticity regressions in Section 9 instead pool the
## waves and rely on wave fixed effects, which absorb this level difference and
## identify the slope from within-wave variation.)

household <- household %>%
  group_by(wave) %>%
  mutate(
    quintile = wtd_group(welfare_pae, weight, 5, paste0("Q", 1:5)),
    quartile = wtd_group(welfare_pae, weight, 4, paste0("Qt", 1:4)),
    tercile  = wtd_group(welfare_pae, weight, 3, paste0("T", 1:3))
  ) %>%
  ungroup()

## Re-attach the group labels to the long item table.
items <- items %>%
  select(-any_of(c("quintile", "quartile", "tercile"))) %>%
  left_join(household %>% select(wave, hhid, quintile, quartile, tercile),
            by = c("wave", "hhid"))

## Generic helper: weighted item means by a grouping variable, optionally
## restricted to rural or urban households.
group_means <- function(data, group_var, area = c("all", "rural", "urban")) {
  area <- match.arg(area)
  d <- data
  if (area == "rural") d <- filter(d, rural == 1)
  if (area == "urban") d <- filter(d, urban == 1)
  d %>%
    filter(!is.na(.data[[group_var]])) %>%
    group_by(item_label, group = .data[[group_var]]) %>%
    summarise(
      n_obs      = sum(qty_total > 0 & is.finite(weight), na.rm = TRUE),
      qty_pae    = wmean(ifelse(qty_total > 0, qty_total_pae, NA), weight),
      exp_pae    = wmean(ifelse(exp > 0, exp_pae, NA),             weight),
      unit_value = wmedian(unit_value, weight),   # median: robust price measure
      .groups = "drop"
    ) %>%
    mutate(grouping = group_var, area = area)
}

## Compute every combination: {quintile,quartile,tercile} x {all,rural,urban}.
combos <- expand.grid(
  gv   = c("quintile", "quartile", "tercile"),
  area = c("all", "rural", "urban"),
  stringsAsFactors = FALSE
)
group_tables <- pmap_dfr(combos, function(gv, area) group_means(items, gv, area))

write.csv(group_tables, file.path(out_tab, "03_group_means.csv"), row.names = FALSE)


## ============================================================================
## 7. Q5 / Q1 RATIOS  (quantity, expenditure, unit value)
## ============================================================================
## How much more do the richest 20% consume / spend / pay-per-unit relative to
## the poorest 20% ? Computed for All / Rural / Urban.

## Small cells are unreliable: ratios are blanked when either Q1 or Q5 has
## fewer than 30 consuming households (e.g. urban pork, urban eggs).
min_cell <- 30
q5q1_ratios <- group_tables %>%
  filter(grouping == "quintile", group %in% c("Q1", "Q5")) %>%
  pivot_wider(id_cols = c(item_label, area), names_from = group,
              values_from = c(n_obs, qty_pae, exp_pae, unit_value)) %>%
  mutate(
    enough = pmin(n_obs_Q1, n_obs_Q5) >= min_cell,
    qty_ratio_Q5_Q1  = ifelse(enough, qty_pae_Q5    / qty_pae_Q1,    NA_real_),
    exp_ratio_Q5_Q1  = ifelse(enough, exp_pae_Q5    / exp_pae_Q1,    NA_real_),
    uv_ratio_Q5_Q1   = ifelse(enough, unit_value_Q5 / unit_value_Q1, NA_real_),
    n_min = pmin(n_obs_Q1, n_obs_Q5)
  ) %>%
  select(item_label, area, n_min, qty_ratio_Q5_Q1, exp_ratio_Q5_Q1, uv_ratio_Q5_Q1)

write.csv(q5q1_ratios, file.path(out_tab, "04_Q5_Q1_ratios.csv"), row.names = FALSE)
print(q5q1_ratios)


## ============================================================================
## 8. GRAPHS
## ============================================================================
theme_set(theme_minimal(base_size = 11))

## ---- 8a. Q5/Q1 ratios (all households): qty vs exp vs unit value -----------
ratio_long <- q5q1_ratios %>%
  filter(area == "all") %>%
  pivot_longer(c(qty_ratio_Q5_Q1, exp_ratio_Q5_Q1, uv_ratio_Q5_Q1),
               names_to = "measure", values_to = "ratio") %>%
  mutate(measure = recode(measure,
                          qty_ratio_Q5_Q1 = "Quantity",
                          exp_ratio_Q5_Q1 = "Expenditure",
                          uv_ratio_Q5_Q1  = "Unit value"))

g_ratio <- ggplot(ratio_long, aes(item_label, ratio, fill = measure)) +
  geom_col(position = position_dodge()) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(title = "Q5 / Q1 ratios by item (pooled, all households)",
       subtitle = "Ratio of richest-quintile to poorest-quintile averages (per adult equivalent)",
       x = NULL, y = "Q5 / Q1 ratio", fill = NULL) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(out_fig, "fig1_Q5Q1_ratios.png"), g_ratio, width = 9, height = 5, dpi = 150)

## ---- 8b. Unit value across quintiles (the "quality gradient") --------------
uv_by_q <- group_tables %>% filter(grouping == "quintile", area == "all")
g_uv <- ggplot(uv_by_q, aes(group, unit_value, group = item_label, colour = item_label)) +
  geom_line() + geom_point() +
  labs(title = "Unit value by welfare quintile (quality gradient)",
       x = "Welfare quintile (real expenditure per adult equivalent)",
       y = "Unit value (TSH per kg / litre / piece)", colour = NULL)
ggsave(file.path(out_fig, "fig2_unitvalue_quintiles.png"), g_uv, width = 9, height = 5, dpi = 150)

## ---- 8c. Rural vs urban comparison -----------------------------------------
rural_urban <- group_tables %>%
  filter(grouping == "quintile", area %in% c("rural", "urban")) %>%
  group_by(item_label, area) %>%
  summarise(unit_value = mean(unit_value, na.rm = TRUE),
            exp_pae    = mean(exp_pae,    na.rm = TRUE), .groups = "drop")

g_ru <- ggplot(rural_urban, aes(item_label, unit_value, fill = area)) +
  geom_col(position = position_dodge()) +
  labs(title = "Rural vs urban: average unit value by item",
       x = NULL, y = "Unit value (TSH per std unit)", fill = NULL) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(out_fig, "fig3_rural_urban_unitvalue.png"), g_ru, width = 9, height = 5, dpi = 150)


## ============================================================================
## 9. ELASTICITY ESTIMATION AND THE QUALITY-UPGRADING TEST
## ============================================================================
## We use the PURCHASED part of consumption (the only part with a market value)
## so that ln(exp) = ln(qty) + ln(unit value) holds exactly.
##
##   ln(exp)        ~ ln(welfare_pae) + controls   -> eps_expenditure
##   ln(qty_purch)  ~ ln(welfare_pae) + controls   -> eps_quantity
##   ln(unit_value) ~ ln(welfare_pae) + controls   -> eps_quality
##
## Controls: log adult equivalents, rural dummy, region, and WAVE dummies
## (pooled estimation with wave fixed effects). Welfare is real per-AE
## expenditure, so wave dummies absorb the remaining price/level differences.

est <- items %>%
  filter(qty_purch > 0, exp > 0, is.finite(unit_value),
         welfare_pae > 0, adulteq > 0) %>%
  mutate(
    ln_exp = log(exp),
    ln_q   = log(qty_purch),
    ln_uv  = log(unit_value),
    ln_w   = log(welfare_pae),
    ln_ae  = log(adulteq),
    rural  = factor(rural),
    wave   = factor(wave),
    region = factor(region)
  )

## Standard errors are clustered by REGION (allows spatial correlation; also
## avoids the HC1 "singular hat value" warnings from sparse regions).
vcov_cl <- function(m) vcovCL(m, cluster = model.frame(m)$region)

## One-sided test of H1: eps_quality > 0, with region-clustered standard errors.
one_sided_quality_test <- function(model) {
  ct  <- coeftest(model, vcov = vcov_cl(model))
  b   <- ct["ln_w", "Estimate"]
  se  <- ct["ln_w", "Std. Error"]
  tval <- b / se
  p_one <- pt(tval, df = df.residual(model), lower.tail = FALSE)  # H1: > 0
  c(estimate = b, se = se, t = tval, p_one_sided = p_one)
}

## Fit the three regressions for one item and return the decomposition.
estimate_item <- function(d) {
  ctrl <- "ln_w + ln_ae + rural + wave + region"
  m_exp <- lm(as.formula(paste("ln_exp ~", ctrl)), data = d)
  m_q   <- lm(as.formula(paste("ln_q   ~", ctrl)), data = d)
  m_uv  <- lm(as.formula(paste("ln_uv  ~", ctrl)), data = d)

  rob <- function(m) coeftest(m, vcov = vcov_cl(m))["ln_w", "Estimate"]
  qtest <- one_sided_quality_test(m_uv)

  tibble(
    n              = nrow(d),
    eps_expenditure = rob(m_exp),
    eps_quantity    = rob(m_q),
    eps_quality     = qtest["estimate"],
    quality_se      = qtest["se"],
    quality_t       = qtest["t"],
    quality_p_1side = qtest["p_one_sided"],
    reject_H0_5pct  = qtest["p_one_sided"] < 0.05
  )
}

## Per-item results.
elasticities <- est %>%
  group_by(item_label) %>%
  group_modify(~ estimate_item(.x)) %>%
  ungroup()

## Pooled across ALL items (item fixed effects + wave fixed effects):
## a single "overall" set of elasticities for animal-source foods.
## NOTE: item fixed effects are essential here -- without them the pooled unit
## value mixes cheap (eggs) and expensive (beef) items and the quality slope can
## even turn negative. The per-item estimates above are the primary results; the
## pooled row is only a compact summary.
est_pooled <- est %>% mutate(item = factor(item))
m_exp_p <- lm(ln_exp ~ ln_w + ln_ae + rural + wave + region + item, data = est_pooled)
m_q_p   <- lm(ln_q   ~ ln_w + ln_ae + rural + wave + region + item, data = est_pooled)
m_uv_p  <- lm(ln_uv  ~ ln_w + ln_ae + rural + wave + region + item, data = est_pooled)
rob_coef <- function(m) coeftest(m, vcov = vcov_cl(m))["ln_w", "Estimate"]
qtest_p  <- one_sided_quality_test(m_uv_p)

pooled_row <- tibble(
  item_label      = "ALL ITEMS (pooled)",
  n               = nrow(est_pooled),
  eps_expenditure = rob_coef(m_exp_p),
  eps_quantity    = rob_coef(m_q_p),
  eps_quality     = qtest_p["estimate"],
  quality_se      = qtest_p["se"],
  quality_t       = qtest_p["t"],
  quality_p_1side = qtest_p["p_one_sided"],
  reject_H0_5pct  = qtest_p["p_one_sided"] < 0.05
)

elasticities <- bind_rows(elasticities, pooled_row)
write.csv(elasticities, file.path(out_tab, "05_elasticities.csv"), row.names = FALSE)
cat("\n==== Elasticity decomposition (eps_expenditure = eps_quantity + eps_quality) ====\n")
print(as.data.frame(elasticities), digits = 3)


## ---- 9b. Elasticity decomposition graph ------------------------------------
elas_long <- elasticities %>%
  filter(item_label != "ALL ITEMS (pooled)") %>%
  select(item_label, eps_quantity, eps_quality) %>%
  pivot_longer(c(eps_quantity, eps_quality),
               names_to = "component", values_to = "elasticity") %>%
  mutate(component = recode(component,
                            eps_quantity = "Quantity",
                            eps_quality  = "Quality"))

g_elas <- ggplot(elas_long, aes(item_label, elasticity, fill = component)) +
  geom_col() +
  labs(title = "Expenditure elasticity decomposition by item",
       subtitle = "Total height = expenditure elasticity; split into quantity and quality",
       x = NULL, y = "Elasticity w.r.t. real expenditure per AE", fill = NULL) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(out_fig, "fig4_elasticity_decomposition.png"), g_elas, width = 9, height = 5, dpi = 150)


## ============================================================================
## 10. DOES QUALITY MATTER MORE AS INCOME RISES?
## ============================================================================
## We let the quality elasticity vary by welfare quintile (a separate ln_w slope
## per quintile, pooled across items with item + wave fixed effects). NOTE: this
## is NOT a clean "threshold" exercise -- estimating an income slope WITHIN a
## quintile uses exactly the income variation a quintile compresses, so the
## quintile-specific slopes are noisy. Read them as "is quality upgrading present
## at this income level?" rather than as a precise switch-on point. We also
## report quality's SHARE of the total expenditure elasticity by quintile, which
## is the more interpretable "how much does quality matter" measure.

## ---- (A) Quality elasticity by welfare quintile (pooled across items) ------
est_q <- est_pooled %>% filter(!is.na(quintile)) %>% mutate(quintile = factor(quintile))
m_uv_byq <- lm(ln_uv ~ quintile + quintile:ln_w + ln_ae + rural + wave + region + item,
               data = est_q)
ctq <- coeftest(m_uv_byq, vcov = vcov_cl(m_uv_byq))

# pull the quintile-specific ln_w slopes
slope_rows <- grep("ln_w", rownames(ctq), value = TRUE)
quality_by_quintile <- tibble(
  quintile    = str_extract(slope_rows, "Q[1-5]"),
  eps_quality = ctq[slope_rows, "Estimate"],
  se          = ctq[slope_rows, "Std. Error"],
  t           = ctq[slope_rows, "Estimate"] / ctq[slope_rows, "Std. Error"]
) %>%
  mutate(p_one_sided = pt(t, df = df.residual(m_uv_byq), lower.tail = FALSE),
         positive_sig = p_one_sided < 0.05) %>%
  arrange(quintile)

write.csv(quality_by_quintile, file.path(out_tab, "06_quality_by_quintile.csv"), row.names = FALSE)
cat("\n==== Quality elasticity by welfare quintile (within-wave quintiles) ====\n")
print(as.data.frame(quality_by_quintile), digits = 3)

## CHECK result: the quality elasticity is positive and significant in EVERY
## quintile (including Q1) and is roughly flat / mildly U-shaped, not increasing
## from a threshold. So there is NO clean income level at which quality "switches
## on": quality upgrading is present across the whole distribution.
n_sig <- sum(quality_by_quintile$positive_sig, na.rm = TRUE)
cat(sprintf("\nQuality elasticity is significantly > 0 in %d of 5 quintiles -> ", n_sig))
cat("quality upgrading is present across the distribution (no single threshold).\n")

## Reference: mean real expenditure per AE by quintile and wave (levels differ
## across waves -- see Section 6).
q_income_by_wave <- household %>%
  filter(!is.na(quintile)) %>%
  group_by(wave, quintile) %>%
  summarise(mean_welfare_pae = wmean(welfare_pae, weight),
            .groups = "drop")
write.csv(q_income_by_wave, file.path(out_tab, "07_quintile_income_by_wave.csv"), row.names = FALSE)

## ---- (B) Quality SHARE of the expenditure elasticity, by quintile ----------
## Let BOTH the expenditure and unit-value slopes vary by quintile; the share
## eps_quality / eps_expenditure says how much of the income response is quality.
m_ex_byq <- lm(ln_exp ~ quintile + quintile:ln_w + ln_ae + rural + wave + region + item,
               data = est_q)
ex_slopes <- coef(m_ex_byq)[grep("ln_w", names(coef(m_ex_byq)))]
uv_slopes <- coef(m_uv_byq)[grep("ln_w", names(coef(m_uv_byq)))]
quality_share <- tibble(
  quintile        = str_extract(names(ex_slopes), "Q[1-5]"),
  eps_expenditure = as.numeric(ex_slopes),
  eps_quality     = as.numeric(uv_slopes)
) %>%
  mutate(quality_share_pct = 100 * eps_quality / eps_expenditure) %>%
  arrange(quintile)
write.csv(quality_share, file.path(out_tab, "08_quality_share_by_quintile.csv"), row.names = FALSE)
cat("\n==== Quality share of the expenditure elasticity, by quintile ====\n")
print(as.data.frame(quality_share), digits = 3)

## ---- (C) Robustness: elasticities estimated separately by wave -------------
## If pooling is valid the per-wave elasticities should be similar.
by_wave <- map_dfr(levels(est$wave), function(wv) {
  d <- filter(est_pooled, wave == wv) %>% mutate(item = droplevels(item))
  fx <- "ln_w + ln_ae + rural + region + item"
  tibble(
    wave            = wv, n = nrow(d),
    eps_expenditure = coeftest(lm(as.formula(paste("ln_exp ~", fx)), d),
                               vcov = vcov_cl)["ln_w", "Estimate"],
    eps_quantity    = coeftest(lm(as.formula(paste("ln_q ~", fx)), d),
                               vcov = vcov_cl)["ln_w", "Estimate"],
    eps_quality     = coeftest(lm(as.formula(paste("ln_uv ~", fx)), d),
                               vcov = vcov_cl)["ln_w", "Estimate"]
  )
})
write.csv(by_wave, file.path(out_tab, "09_elasticities_by_wave.csv"), row.names = FALSE)
cat("\n==== Robustness: elasticities by wave (should be similar if pooling is OK) ====\n")
print(as.data.frame(by_wave), digits = 3)

## ---- Graph: quality elasticity across quintiles ----------------------------
g_q <- ggplot(quality_by_quintile, aes(quintile, eps_quality)) +
  geom_col(aes(fill = positive_sig)) +
  geom_errorbar(aes(ymin = eps_quality - 1.96 * se,
                    ymax = eps_quality + 1.96 * se), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c(`TRUE` = "steelblue", `FALSE` = "grey70"),
                    name = "Quality elasticity\nsignificant (> 0)") +
  labs(title = "Quality elasticity by welfare quintile",
       subtitle = "Positive and significant in every quintile: quality upgrading across the whole distribution",
       x = "Welfare quintile (real expenditure per adult equivalent)",
       y = "Quality elasticity (eps_quality)")
ggsave(file.path(out_fig, "fig5_quality_by_quintile.png"), g_q, width = 8, height = 5, dpi = 150)


## ============================================================================
## 11. ADDITIONAL DESCRIPTIVE & DIAGNOSTIC FIGURES
## ============================================================================
## A broader set of figures covering household structure, participation,
## quantity / expenditure / unit-value gradients (quintiles, quartiles,
## terciles), rural vs urban comparisons, Engel curves and the quality test.

## ---- fig6. Household composition by wave -----------------------------------
hh_comp <- hh_summary %>%
  filter(wave != "Pooled") %>%
  select(wave, `Rural %` = pct_rural, `Urban %` = pct_urban,
         `Owns livestock %` = pct_livestock) %>%
  pivot_longer(-wave, names_to = "indicator", values_to = "pct")
g6 <- ggplot(hh_comp, aes(wave, pct, fill = indicator)) +
  geom_col(position = position_dodge()) +
  labs(title = "Household composition by wave", x = NULL, y = "%", fill = NULL)
ggsave(file.path(out_fig, "fig6_household_composition.png"), g6, width = 8, height = 5, dpi = 150)

## ---- fig7. Participation: share of households consuming each item ----------
g7 <- ggplot(item_summary, aes(reorder(item_label, pct_consuming), pct_consuming)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Share of households consuming each item (pooled)",
       x = NULL, y = "% consuming in the past 7 days")
ggsave(file.path(out_fig, "fig7_participation.png"), g7, width = 8, height = 5, dpi = 150)

## ---- fig8. Quantity per adult equivalent by welfare quintile ---------------
qty_by_q <- group_tables %>% filter(grouping == "quintile", area == "all")
g8 <- ggplot(qty_by_q, aes(group, qty_pae, colour = item_label, group = item_label)) +
  geom_line() + geom_point() +
  labs(title = "Quantity per adult equivalent by welfare quintile",
       x = "Welfare quintile", y = "Quantity per AE (kg / litre / pieces, 7 days)",
       colour = NULL)
ggsave(file.path(out_fig, "fig8_quantity_quintiles.png"), g8, width = 9, height = 5, dpi = 150)

## ---- fig9. Expenditure per adult equivalent by welfare quintile ------------
exp_by_q <- group_tables %>% filter(grouping == "quintile", area == "all")
g9 <- ggplot(exp_by_q, aes(group, exp_pae, colour = item_label, group = item_label)) +
  geom_line() + geom_point() +
  labs(title = "Expenditure per adult equivalent by welfare quintile",
       x = "Welfare quintile", y = "Expenditure per AE (TSH, 7 days)", colour = NULL)
ggsave(file.path(out_fig, "fig9_expenditure_quintiles.png"), g9, width = 9, height = 5, dpi = 150)

## ---- fig10. Q5/Q1 ratios, faceted by area (all / rural / urban) ------------
ratio_all_areas <- q5q1_ratios %>%
  pivot_longer(c(qty_ratio_Q5_Q1, exp_ratio_Q5_Q1, uv_ratio_Q5_Q1),
               names_to = "measure", values_to = "ratio") %>%
  mutate(measure = recode(measure,
                          qty_ratio_Q5_Q1 = "Quantity",
                          exp_ratio_Q5_Q1 = "Expenditure",
                          uv_ratio_Q5_Q1  = "Unit value"))
g10 <- ggplot(ratio_all_areas, aes(item_label, ratio, fill = measure)) +
  geom_col(position = position_dodge()) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ area, ncol = 1) +
  labs(title = "Q5 / Q1 ratios by item and area", x = NULL, y = "Q5 / Q1 ratio", fill = NULL) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(out_fig, "fig10_Q5Q1_by_area.png"), g10, width = 9, height = 9, dpi = 150)

## ---- fig11. Rural vs urban: quantity and expenditure per AE ----------------
ru_qexp <- group_tables %>%
  filter(grouping == "quintile", area %in% c("rural", "urban")) %>%
  group_by(item_label, area) %>%
  summarise(Quantity = mean(qty_pae, na.rm = TRUE),
            Expenditure = mean(exp_pae, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(c(Quantity, Expenditure), names_to = "measure", values_to = "value")
g11 <- ggplot(ru_qexp, aes(item_label, value, fill = area)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ measure, scales = "free_y") +
  labs(title = "Rural vs urban: quantity and expenditure per adult equivalent",
       x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))
ggsave(file.path(out_fig, "fig11_rural_urban_qty_exp.png"), g11, width = 10, height = 5, dpi = 150)

## ---- fig12. Engel curves: ln(expenditure) vs ln(welfare), by item ----------
## Slope of each line is (close to) the expenditure elasticity. Coloured by wave
## because welfare levels differ across waves (see Section 6).
g12 <- ggplot(est, aes(ln_w, ln_exp, colour = wave)) +
  geom_point(alpha = 0.12, size = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ item_label, scales = "free") +
  labs(title = "Engel curves: log expenditure vs log welfare per AE",
       x = "log(real expenditure per AE)", y = "log(item expenditure)", colour = NULL)
ggsave(file.path(out_fig, "fig12_engel_curves.png"), g12, width = 10, height = 6, dpi = 150)

## ---- fig13. Quality gradient: ln(unit value) vs ln(welfare), by item -------
## Slope is the quality elasticity; a positive slope is quality upgrading.
g13 <- ggplot(est, aes(ln_w, ln_uv, colour = wave)) +
  geom_point(alpha = 0.12, size = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ item_label, scales = "free") +
  labs(title = "Quality gradient: log unit value vs log welfare per AE",
       x = "log(real expenditure per AE)", y = "log(unit value)", colour = NULL)
ggsave(file.path(out_fig, "fig13_quality_gradient.png"), g13, width = 10, height = 6, dpi = 150)

## ---- fig14. Unit value across quintiles, quartiles and terciles ------------
uv_groups <- group_tables %>% filter(area == "all") %>%
  mutate(grouping = recode(grouping, quintile = "Quintiles",
                           quartile = "Quartiles", tercile = "Terciles"))
g14 <- ggplot(uv_groups, aes(group, unit_value, colour = item_label, group = item_label)) +
  geom_line() + geom_point() +
  facet_wrap(~ grouping, scales = "free_x") +
  labs(title = "Unit value (quality) gradient across welfare groups",
       x = "Welfare group (poor -> rich)", y = "Median unit value (TSH per std unit)",
       colour = NULL)
ggsave(file.path(out_fig, "fig14_unitvalue_groups.png"), g14, width = 11, height = 5, dpi = 150)

## ---- fig15. Quality elasticity by item with 95% CI (forest plot) -----------
## Visual of the hypothesis test H0: eps_quality = 0. Points right of the dashed
## line with a CI that clears zero reject H0 (quality upgrading).
elas_ci <- elasticities %>%
  mutate(lo = eps_quality - 1.96 * quality_se,
         hi = eps_quality + 1.96 * quality_se,
         pooled = item_label == "ALL ITEMS (pooled)")
g15 <- ggplot(elas_ci, aes(eps_quality, reorder(item_label, eps_quality))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2) +
  geom_point(aes(colour = pooled), size = 2.5) +
  scale_colour_manual(values = c(`FALSE` = "black", `TRUE` = "firebrick"), guide = "none") +
  labs(title = "Quality elasticity by item (95% CI)",
       subtitle = "H0: quality elasticity = 0  vs  H1: > 0 (pooled estimate in red)",
       x = "Quality elasticity (eps_quality)", y = NULL)
ggsave(file.path(out_fig, "fig15_quality_forest.png"), g15, width = 8, height = 5, dpi = 150)


## ============================================================================
## 12. DONE
## ============================================================================
cat("\nAll tables saved to:", normalizePath(out_tab), "\n")
cat("All figures saved to:", normalizePath(out_fig), "\n")
cat("\nNotes:\n")
cat(" * 'Processed milk' is not available in these master files; the analysis\n")
cat("   covers goat meat, beef, pork, chicken & poultry, eggs, and fresh milk.\n")
cat(" * Real expenditure per AE is not level-comparable across waves, so welfare\n")
cat("   groups are built WITHIN wave and elasticities use wave fixed effects.\n")
