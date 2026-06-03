###############################################################################
##  FOOD CONSUMPTION AND QUALITY UPGRADING IN TANZANIA
##  First-stage analysis: descriptive statistics + expenditure-elasticity
##  decomposition (quantity vs. quality) on the Tanzania NPS panel.
##
##  Data : NPS Wave 3, Wave 4, Wave 5 household master files (.xlsx).
##
##  ------------------------------------------------------------------------
##  METHOD (unit-value approach)
##  ------------------------------------------------------------------------
##  For a food item the budget identity is exact:
##        expenditure = quantity * unit value
##   =>   ln(expenditure) = ln(quantity) + ln(unit value)
##  Three log-log regressions on the log household welfare measure W give:
##        eps_expenditure = eps_quantity + eps_quality
##  (Deaton 1988; Cox & Wohlgenant 1986; Gibson & Kim 2019). The quality
##  elasticity is the elasticity of the unit value w.r.t. W. We test
##  H0: eps_quality = 0 vs H1: eps_quality > 0 with a one-sided t-test
##  (region-clustered standard errors).
##
##  ------------------------------------------------------------------------
##  DATA SETTINGS
##  ------------------------------------------------------------------------
##  * Items: goat meat (801), beef (802), pork (803), chicken & poultry (804),
##    eggs (807), fresh milk (901). Processed milk is not present in these files.
##  * Welfare measure = real total monthly expenditure per adult equivalent.
##  * Survey weights: weighted descriptive statistics; unweighted regressions.
##  * Units harmonised: meats in kg (g->kg), eggs in pieces, milk in litres
##    (ml->litre). Non-standard units are dropped.
##  * Unit value = purchase expenditure / purchased quantity (TSH per unit).
##  * Welfare groups: QUARTILES only.
##
##  FILE LAYOUT: each "HH_Data" sheet has two header rows (section, then label);
##  read with skip = 1 so the labels become the column names, then select
##  columns by matching text patterns (labels carry the codes, e.g.
##  "itemcode=801]"). Wave 5's main food module uses the "hh_ja..." questions
##  and splits poultry into 8041 + 8042, summed back to "chicken & poultry".
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

## Read one wave's HH_Data sheet (row 1 = section, row 2 = label, data from row
## 3); skip = 1 makes row 2 the header.
read_wave <- function(path) {
  read_excel(path, sheet = "HH_Data", skip = 1, .name_repair = "minimal")
}

## First column of `df` whose name contains ALL the given text patterns;
## returns a numeric vector (NA if none).
pick_num <- function(df, patterns) {
  hit <- names(df)[map_lgl(names(df),
                           ~ all(str_detect(.x, fixed(patterns))))]
  if (length(hit) == 0) return(rep(NA_real_, nrow(df)))
  suppressWarnings(as.numeric(df[[hit[1]]]))
}

## Same, returning the column as character (used for the household id).
pick_chr <- function(df, patterns) {
  hit <- names(df)[map_lgl(names(df),
                           ~ all(str_detect(.x, fixed(patterns))))]
  if (length(hit) == 0) return(rep(NA_character_, nrow(df)))
  as.character(df[[hit[1]]])
}

## Convert a reported quantity to the item's standard unit.
## Unit codes: 1=kg, 2=g, 3=litre, 4=ml, 5=pieces; anything else -> NA.
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

## Weighted quantile (used for the welfare group cut-points).
wtd_quantile <- function(x, w, probs) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  approx(cw, x, xout = probs, rule = 2, ties = "ordered")$y
}

## Assign group labels (e.g. Q1..Q4) from weighted cut-points.
wtd_group <- function(x, w, n, labels) {
  cuts <- c(-Inf, wtd_quantile(x, w, seq_len(n - 1) / n), Inf)
  cut(x, breaks = cuts, labels = labels, include.lowest = TRUE)
}

## Weighted mean / median that drop rows with NA x, NA weight or weight <= 0.
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
## item x wave to remove data-entry outliers).
trim_to_na <- function(x, lo = 0.01, hi = 0.99) {
  qs <- quantile(x, c(lo, hi), na.rm = TRUE, names = FALSE)
  ifelse(is.finite(x) & x >= qs[1] & x <= qs[2], x, NA_real_)
}


## ============================================================================
## 2. PER-WAVE CONFIGURATION
## ============================================================================
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
  ~item,         ~label,                 ~type,     ~unit,     ~codes_std,    ~codes_y5,
  "goat_meat",   "Goat meat",            "meat",    "kg",      "801",         "801",
  "beef",        "Beef",                 "meat",    "kg",      "802",         "802",
  "pork",        "Pork",                 "meat",    "kg",      "803",         "803",
  "chicken",     "Chicken & poultry",    "meat",    "kg",      "804",         "8041,8042",
  "eggs",        "Eggs",                 "pieces",  "piece",   "807",         "807",
  "fresh_milk",  "Fresh milk",           "litre",   "litre",   "901",         "901"
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
  # welfare = real total monthly expenditure per adult equivalent
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
    livestock   = ifelse(is.na(livestock), 0L, as.integer(livestock == 1)),
    welfare_pae = welfare_pae
  )
}

## One item code, one wave -> consumed flag, total quantity, quantity by source
## (purchased / own production / gifts), and purchase expenditure.
extract_one_code <- function(df, prefix, code, type) {
  tag <- paste0("itemcode=", code, "]")
  yn      <- pick_num(df, c(prefix, tag, "eat/drink any"))
  q_tot   <- pick_num(df, c(prefix, tag, "in total did your household consume", "QUANTITY"))
  u_tot   <- pick_num(df, c(prefix, tag, "in total did your household consume", "UNIT"))
  q_buy   <- pick_num(df, c(prefix, tag, "came from purchases", "QUANTITY"))
  u_buy   <- pick_num(df, c(prefix, tag, "came from purchases", "UNIT"))
  q_own   <- pick_num(df, c(prefix, tag, "came from own production", "QUANTITY"))
  u_own   <- pick_num(df, c(prefix, tag, "came from own production", "UNIT"))
  q_gift  <- pick_num(df, c(prefix, tag, "came from gifts", "QUANTITY"))
  u_gift  <- pick_num(df, c(prefix, tag, "came from gifts", "UNIT"))
  spend   <- pick_num(df, c(prefix, tag, "How much did you spend"))
  tibble(
    consumed   = as.integer(yn == 1),
    qty_total  = normalise_qty(q_tot,  u_tot,  type),
    qty_purch  = normalise_qty(q_buy,  u_buy,  type),
    qty_own    = normalise_qty(q_own,  u_own,  type),
    qty_gift   = normalise_qty(q_gift, u_gift, type),
    exp        = spend
  )
}

## Combine one or more item codes (codes are summed; used for Wave-5 chicken).
extract_item <- function(df, prefix, codes, type) {
  parts <- map(codes, ~ extract_one_code(df, prefix, .x, type))
  sum_na <- function(a, b) ifelse(is.na(a) & is.na(b), NA,
                                  rowSums(cbind(a, b), na.rm = TRUE))
  reduce(parts, function(a, b) tibble(
    consumed  = pmax(a$consumed, b$consumed, na.rm = TRUE),
    qty_total = sum_na(a$qty_total, b$qty_total),
    qty_purch = sum_na(a$qty_purch, b$qty_purch),
    qty_own   = sum_na(a$qty_own,   b$qty_own),
    qty_gift  = sum_na(a$qty_gift,  b$qty_gift),
    exp       = sum_na(a$exp,       b$exp)
  ))
}


## ============================================================================
## 4. BUILD THE POOLED LONG DATASET
## ============================================================================

hh_list   <- list()
item_list <- list()

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
    unit_value = ifelse(qty_purch > 0 & exp > 0, exp / qty_purch, NA_real_)
  )

## ---- Trim outliers within item x wave (1st-99th percentile) -----------------
items <- items %>%
  group_by(item, wave) %>%
  mutate(across(c(unit_value, qty_total, qty_purch, qty_own, qty_gift, exp), trim_to_na)) %>%
  ungroup() %>%
  mutate(
    qty_total_pae = qty_total / adulteq,
    qty_purch_pae = qty_purch / adulteq,
    qty_own_pae   = qty_own   / adulteq,
    qty_gift_pae  = qty_gift  / adulteq,
    exp_pae       = exp / adulteq,
    item_label    = item_cfg$label[match(item, item_cfg$item)],
    unit          = item_cfg$unit[match(item, item_cfg$item)]
  )

item_levels <- item_cfg$label
items$item_label <- factor(items$item_label, levels = item_levels)
items$item_unit <- factor(paste0(items$item_label, " (", items$unit, ")"),
                          levels = paste0(item_cfg$label, " (", item_cfg$unit, ")"))

message("Households pooled: ", nrow(household),
        " | item-rows: ", nrow(items))


## ============================================================================
## 5. DESCRIPTIVE STATISTICS  (weighted)
## ============================================================================

## ---- 5a. Household-level summary, by wave and pooled -----------------------
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

## ---- 5b. Item-level summary (household and per adult equivalent) ------------
item_summary <- items %>%
  group_by(item_label) %>%
  summarise(
    unit               = first(unit),
    pct_consuming      = 100 * wmean(consumed, weight),
    mean_qty_hh        = wmean(ifelse(qty_total > 0, qty_total, NA), weight),
    mean_exp_hh        = wmean(ifelse(exp > 0, exp, NA),             weight),
    mean_qty_pae       = wmean(ifelse(qty_total > 0, qty_total_pae, NA), weight),
    mean_exp_pae       = wmean(ifelse(exp > 0, exp_pae, NA),             weight),
    median_unit_value  = wmedian(unit_value, weight),
    .groups = "drop"
  )
write.csv(item_summary, file.path(out_tab, "02_item_summary.csv"), row.names = FALSE)
print(item_summary)

## ---- 5c. Item summary by livestock ownership -------------------------------
item_by_livestock <- items %>%
  mutate(livestock_owner = ifelse(livestock == 1, "Owns livestock", "No livestock")) %>%
  group_by(item_label, livestock_owner) %>%
  summarise(
    pct_consuming     = 100 * wmean(consumed, weight),
    mean_qty_pae      = wmean(ifelse(qty_total > 0, qty_total_pae, NA), weight),
    mean_exp_pae      = wmean(ifelse(exp > 0, exp_pae, NA),             weight),
    median_unit_value = wmedian(unit_value, weight),
    .groups = "drop"
  )
write.csv(item_by_livestock, file.path(out_tab, "02b_item_by_livestock.csv"), row.names = FALSE)

## ---- 5d. Source of consumed quantity: purchased / own / gifts --------------
src0 <- function(x) coalesce(x, 0)
source_decomp <- items %>%
  filter(qty_total > 0) %>%
  group_by(item_label) %>%
  summarise(
    Purchased        = wmean(src0(qty_purch_pae), weight),
    `Own production` = wmean(src0(qty_own_pae),   weight),
    Gifts            = wmean(src0(qty_gift_pae),  weight),
    .groups = "drop"
  )
write.csv(source_decomp, file.path(out_tab, "02c_quantity_source.csv"), row.names = FALSE)

source_by_livestock <- items %>%
  filter(qty_total > 0) %>%
  mutate(livestock_owner = ifelse(livestock == 1, "Owns livestock", "No livestock")) %>%
  group_by(item_label, livestock_owner) %>%
  summarise(
    Purchased        = wmean(src0(qty_purch_pae), weight),
    `Own production` = wmean(src0(qty_own_pae),   weight),
    Gifts            = wmean(src0(qty_gift_pae),  weight),
    .groups = "drop"
  )
write.csv(source_by_livestock, file.path(out_tab, "02d_quantity_source_by_livestock.csv"), row.names = FALSE)


## ============================================================================
## 6. WELFARE GROUPS: QUARTILES
## ============================================================================
## Quartiles of real expenditure per adult equivalent, weighted, built within
## each wave (per-AE levels are not comparable across waves; the pooled
## elasticity regressions use wave fixed effects).
household <- household %>%
  group_by(wave) %>%
  mutate(quartile = wtd_group(welfare_pae, weight, 4, paste0("Q", 1:4))) %>%
  ungroup()

items <- items %>%
  select(-any_of("quartile")) %>%
  left_join(household %>% select(wave, hhid, quartile), by = c("wave", "hhid"))

## Weighted item means by quartile. For rural / urban the quartiles are
## recomputed within that area (and within wave), so "Q1" for area = "urban" is
## the poorest 25% of urban households.
group_means <- function(data, area = c("all", "rural", "urban")) {
  area <- match.arg(area)
  d <- data
  if (area == "rural") d <- filter(d, rural == 1)
  if (area == "urban") d <- filter(d, urban == 1)
  d %>%
    group_by(wave) %>%
    mutate(group = wtd_group(welfare_pae, weight, 4, paste0("Q", 1:4))) %>%
    ungroup() %>%
    filter(!is.na(group)) %>%
    group_by(item_label, group) %>%
    summarise(
      unit       = first(unit),
      n_obs      = sum(qty_total > 0 & is.finite(weight), na.rm = TRUE),
      qty_pae    = wmean(ifelse(qty_total > 0, qty_total_pae, NA), weight),
      exp_pae    = wmean(ifelse(exp > 0, exp_pae, NA),             weight),
      unit_value = wmedian(unit_value, weight),
      .groups = "drop"
    ) %>%
    mutate(area = area)
}

group_tables <- bind_rows(
  group_means(items, "all"),
  group_means(items, "rural"),
  group_means(items, "urban")
) %>%
  mutate(item_unit = factor(paste0(item_label, " (", unit, ")"),
                            levels = paste0(item_cfg$label, " (", item_cfg$unit, ")")))

write.csv(group_tables, file.path(out_tab, "03_group_means.csv"), row.names = FALSE)


## ============================================================================
## 7. TOP/BOTTOM RATIOS (Q4 / Q1)
## ============================================================================
## Ratio of the richest to the poorest quartile, per adult equivalent. A ratio
## is blanked when either end-group has fewer than `min_cell` consuming
## households.
min_cell <- 30

ratios <- group_tables %>%
  filter(group %in% c("Q1", "Q4")) %>%
  mutate(end = ifelse(group == "Q1", "bot", "top")) %>%
  pivot_wider(id_cols = c(item_label, unit, area), names_from = end,
              values_from = c(n_obs, qty_pae, exp_pae, unit_value)) %>%
  mutate(
    enough            = pmin(n_obs_bot, n_obs_top) >= min_cell,
    n_min             = pmin(n_obs_bot, n_obs_top),
    qty_ratio_Q4_Q1   = ifelse(enough, qty_pae_top    / qty_pae_bot,    NA_real_),
    exp_ratio_Q4_Q1   = ifelse(enough, exp_pae_top    / exp_pae_bot,    NA_real_),
    uv_ratio_Q4_Q1    = ifelse(enough, unit_value_top / unit_value_bot, NA_real_)
  ) %>%
  select(item_label, unit, area, n_min, qty_ratio_Q4_Q1, exp_ratio_Q4_Q1, uv_ratio_Q4_Q1)
write.csv(ratios, file.path(out_tab, "04_Q4_Q1_ratios.csv"), row.names = FALSE)
print(as.data.frame(filter(ratios, area == "all")), digits = 3)


## ============================================================================
## 8. GRAPHS (ratios and gradients)
## ============================================================================
theme_set(theme_minimal(base_size = 11))

## ---- fig1. Q4/Q1 ratios (all households): qty vs exp vs unit value ----------
ratio_long <- ratios %>%
  filter(area == "all") %>%
  pivot_longer(c(qty_ratio_Q4_Q1, exp_ratio_Q4_Q1, uv_ratio_Q4_Q1),
               names_to = "measure", values_to = "ratio") %>%
  mutate(measure = recode(measure,
                          qty_ratio_Q4_Q1 = "Quantity",
                          exp_ratio_Q4_Q1 = "Expenditure",
                          uv_ratio_Q4_Q1  = "Unit value"))

g_ratio <- ggplot(ratio_long, aes(item_label, ratio, fill = measure)) +
  geom_col(position = position_dodge()) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(title = "Q4 / Q1 ratios by item (pooled, all households)",
       subtitle = "Richest-quartile to poorest-quartile averages, per adult equivalent",
       x = NULL, y = "Q4 / Q1 ratio", fill = NULL) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(out_fig, "fig1_Q4Q1_ratios.png"), g_ratio, width = 9, height = 5, dpi = 150)

## ---- fig2. Unit value across quartiles, one panel per item ------------------
uv_by_q <- group_tables %>% filter(area == "all", n_obs >= min_cell)
g_uv <- ggplot(uv_by_q, aes(group, unit_value, group = 1)) +
  geom_line(colour = "steelblue") + geom_point(colour = "steelblue") +
  facet_wrap(~ item_unit, scales = "free_y") +
  labs(title = "Unit value by welfare quartile",
       subtitle = "Median; each panel in its own unit; cells with < 30 consumers dropped",
       x = "Welfare quartile (real expenditure per adult equivalent)",
       y = "Median unit value (TSH per unit shown in panel title)")
ggsave(file.path(out_fig, "fig2_unitvalue_quartiles.png"), g_uv, width = 10, height = 6, dpi = 150)

## ---- fig3. Rural vs urban: average unit value, one panel per item -----------
rural_urban <- group_tables %>%
  filter(area %in% c("rural", "urban"), n_obs >= min_cell) %>%
  group_by(item_unit, area) %>%
  summarise(unit_value = mean(unit_value, na.rm = TRUE), .groups = "drop")

g_ru <- ggplot(rural_urban, aes(area, unit_value, fill = area)) +
  geom_col() +
  facet_wrap(~ item_unit, scales = "free_y") +
  labs(title = "Rural vs urban: average unit value by item",
       subtitle = "Each panel in its own unit (TSH per kg / litre / piece)",
       x = NULL, y = "Median unit value (TSH per unit shown)", fill = NULL)
ggsave(file.path(out_fig, "fig3_rural_urban_unitvalue.png"), g_ru, width = 10, height = 6, dpi = 150)


## ============================================================================
## 9. ELASTICITY ESTIMATION (unit-value decomposition)
## ============================================================================
## Three regressions on log welfare per AE (purchased quantity/value, so the
## identity holds). Controls: log adult equivalents, rural dummy, region, wave.

est <- items %>%
  filter(qty_purch > 0, exp > 0, is.finite(unit_value),
         welfare_pae > 0, adulteq > 0) %>%
  mutate(
    ln_exp = log(exp),
    ln_q   = log(qty_purch),
    ln_uv  = log(unit_value),
    ln_w   = log(welfare_pae),
    ln_ae  = log(adulteq),
    wave   = factor(wave),
    region = factor(region)
  )

vcov_cl <- function(m) vcovCL(m, cluster = model.frame(m)$region)

one_sided_quality_test <- function(model) {
  ct  <- coeftest(model, vcov = vcov_cl(model))
  b   <- ct["ln_w", "Estimate"]
  se  <- ct["ln_w", "Std. Error"]
  tval <- b / se
  p_one <- pt(tval, df = df.residual(model), lower.tail = FALSE)
  c(estimate = b, se = se, t = tval, p_one_sided = p_one)
}

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

elasticities <- est %>%
  group_by(item_label) %>%
  group_modify(~ estimate_item(.x)) %>%
  ungroup()

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

## ---- 9c. Elasticities by subgroup: rural / urban / livestock ---------------
estimate_subgroup <- function(d, label) {
  fx  <- "ln_w + ln_ae + wave + region + item"
  m_e <- lm(as.formula(paste("ln_exp ~", fx)), data = d)
  m_q <- lm(as.formula(paste("ln_q   ~", fx)), data = d)
  m_u <- lm(as.formula(paste("ln_uv  ~", fx)), data = d)
  qt  <- one_sided_quality_test(m_u)
  tibble(subgroup = label, n = nrow(d),
         eps_expenditure = rob_coef(m_e), eps_quantity = rob_coef(m_q),
         eps_quality = qt["estimate"], quality_se = qt["se"],
         quality_p_1side = qt["p_one_sided"])
}
subgroup_elast <- bind_rows(
  estimate_subgroup(filter(est_pooled, rural == 1), "Rural"),
  estimate_subgroup(filter(est_pooled, urban == 1), "Urban"),
  estimate_subgroup(filter(est_pooled, livestock == 1), "Owns livestock"),
  estimate_subgroup(filter(est_pooled, livestock == 0), "No livestock")
)
write.csv(subgroup_elast, file.path(out_tab, "05b_elasticities_by_subgroup.csv"), row.names = FALSE)
cat("\n==== Elasticities by subgroup (rural/urban, livestock) ====\n")
print(as.data.frame(subgroup_elast), digits = 3)

## ---- fig4. Elasticity decomposition by item --------------------------------
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
## 10. QUALITY ELASTICITY ACROSS THE WELFARE DISTRIBUTION (quartiles)
## ============================================================================

## ---- (A) Quality elasticity by welfare quartile ----------------------------
est_q <- est_pooled %>% filter(!is.na(quartile)) %>% mutate(quartile = factor(quartile))
m_uv_byq <- lm(ln_uv ~ quartile + quartile:ln_w + ln_ae + rural + wave + region + item,
               data = est_q)
ctq <- coeftest(m_uv_byq, vcov = vcov_cl(m_uv_byq))

slope_rows <- grep("ln_w", rownames(ctq), value = TRUE)
quality_by_quartile <- tibble(
  quartile    = str_extract(slope_rows, "Q[1-4]"),
  eps_quality = ctq[slope_rows, "Estimate"],
  se          = ctq[slope_rows, "Std. Error"],
  t           = ctq[slope_rows, "Estimate"] / ctq[slope_rows, "Std. Error"]
) %>%
  mutate(p_one_sided = pt(t, df = df.residual(m_uv_byq), lower.tail = FALSE),
         positive_sig = p_one_sided < 0.05) %>%
  arrange(quartile)

write.csv(quality_by_quartile, file.path(out_tab, "06_quality_by_quartile.csv"), row.names = FALSE)
cat("\n==== Quality elasticity by welfare quartile (within-wave quartiles) ====\n")
print(as.data.frame(quality_by_quartile), digits = 3)
n_sig <- sum(quality_by_quartile$positive_sig, na.rm = TRUE)
cat(sprintf("\nQuality elasticity significantly > 0 in %d of 4 quartiles.\n", n_sig))

## Mean real expenditure per AE by quartile and wave (levels differ by wave).
q_income_by_wave <- household %>%
  filter(!is.na(quartile)) %>%
  group_by(wave, quartile) %>%
  summarise(mean_welfare_pae = wmean(welfare_pae, weight),
            .groups = "drop")
write.csv(q_income_by_wave, file.path(out_tab, "07_quartile_income_by_wave.csv"), row.names = FALSE)

## ---- (B) Quality share of the expenditure elasticity, by quartile ----------
m_u <- lm(ln_uv  ~ quartile + quartile:ln_w + ln_ae + rural + wave + region + item, data = est_q)
m_e <- lm(ln_exp ~ quartile + quartile:ln_w + ln_ae + rural + wave + region + item, data = est_q)
us  <- coef(m_u)[grep(":ln_w", names(coef(m_u)))]
es  <- coef(m_e)[grep(":ln_w", names(coef(m_e)))]
quality_share <- tibble(
  group           = factor(str_extract(names(us), "Q[1-4]"), levels = paste0("Q", 1:4)),
  eps_expenditure = as.numeric(es),
  eps_quality     = as.numeric(us)
) %>%
  mutate(quality_share_pct = 100 * eps_quality / eps_expenditure) %>%
  arrange(group)
write.csv(quality_share, file.path(out_tab, "08_quality_share_by_quartile.csv"), row.names = FALSE)
cat("\n==== Quality share of the expenditure elasticity (quartiles) ====\n")
print(as.data.frame(quality_share), digits = 3)

## ---- (C) Elasticities estimated separately by wave -------------------------
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
cat("\n==== Elasticities by wave ====\n")
print(as.data.frame(by_wave), digits = 3)

## ---- fig5. Quality elasticity by quartile with 95% CI ----------------------
g_q <- ggplot(quality_by_quartile, aes(quartile, eps_quality)) +
  geom_col(aes(fill = positive_sig)) +
  geom_errorbar(aes(ymin = eps_quality - 1.96 * se,
                    ymax = eps_quality + 1.96 * se), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c(`TRUE` = "steelblue", `FALSE` = "grey70"),
                    name = "Significant (> 0)") +
  labs(title = "Quality elasticity by welfare quartile",
       subtitle = "Point estimate and 95% confidence interval",
       x = "Welfare quartile (real expenditure per adult equivalent)",
       y = "Quality elasticity (eps_quality)")
ggsave(file.path(out_fig, "fig5_quality_by_quartile.png"), g_q, width = 8, height = 5, dpi = 150)


## ============================================================================
## 11. ADDITIONAL DESCRIPTIVE & DIAGNOSTIC FIGURES
## ============================================================================

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

## ---- fig7. Share of households consuming each item -------------------------
g7 <- ggplot(item_summary, aes(reorder(item_label, pct_consuming), pct_consuming)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Share of households consuming each item (pooled)",
       x = NULL, y = "% consuming in the past 7 days")
ggsave(file.path(out_fig, "fig7_participation.png"), g7, width = 8, height = 5, dpi = 150)

## ---- fig8. Quantity per adult equivalent by welfare quartile ---------------
qty_by_q <- group_tables %>% filter(area == "all", n_obs >= min_cell)
g8 <- ggplot(qty_by_q, aes(group, qty_pae, group = 1)) +
  geom_line(colour = "darkgreen") + geom_point(colour = "darkgreen") +
  facet_wrap(~ item_unit, scales = "free_y") +
  labs(title = "Quantity per adult equivalent by welfare quartile",
       subtitle = "Each panel in its own unit (kg meats / litre milk / pieces eggs), 7-day recall",
       x = "Welfare quartile", y = "Quantity per AE (unit in panel title)")
ggsave(file.path(out_fig, "fig8_quantity_quartiles.png"), g8, width = 10, height = 6, dpi = 150)

## ---- fig9. Expenditure per adult equivalent by welfare quartile ------------
exp_by_q <- group_tables %>% filter(area == "all")
g9 <- ggplot(exp_by_q, aes(group, exp_pae, colour = item_label, group = item_label)) +
  geom_line() + geom_point() +
  labs(title = "Expenditure per adult equivalent by welfare quartile",
       x = "Welfare quartile", y = "Expenditure per AE (TSH, 7 days)", colour = NULL)
ggsave(file.path(out_fig, "fig9_expenditure_quartiles.png"), g9, width = 9, height = 5, dpi = 150)

## ---- fig10. Q4/Q1 ratios, faceted by area ----------------------------------
ratio_all_areas <- ratios %>%
  pivot_longer(c(qty_ratio_Q4_Q1, exp_ratio_Q4_Q1, uv_ratio_Q4_Q1),
               names_to = "measure", values_to = "ratio") %>%
  mutate(measure = recode(measure,
                          qty_ratio_Q4_Q1 = "Quantity",
                          exp_ratio_Q4_Q1 = "Expenditure",
                          uv_ratio_Q4_Q1  = "Unit value"))
g10 <- ggplot(ratio_all_areas, aes(item_label, ratio, fill = measure)) +
  geom_col(position = position_dodge()) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ area, ncol = 1) +
  labs(title = "Q4 / Q1 ratios by item and area", x = NULL, y = "Q4 / Q1 ratio", fill = NULL) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(out_fig, "fig10_Q4Q1_by_area.png"), g10, width = 9, height = 9, dpi = 150)

## ---- fig11. Rural vs urban: expenditure per AE (TSH) -----------------------
ru_exp <- group_tables %>%
  filter(area %in% c("rural", "urban"), n_obs >= min_cell) %>%
  group_by(item_label, area) %>%
  summarise(exp_pae = mean(exp_pae, na.rm = TRUE), .groups = "drop")
g11 <- ggplot(ru_exp, aes(item_label, exp_pae, fill = area)) +
  geom_col(position = position_dodge()) +
  labs(title = "Rural vs urban: expenditure per adult equivalent",
       x = NULL, y = "Expenditure per AE (TSH, 7 days)", fill = NULL) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(out_fig, "fig11_rural_urban_expenditure.png"), g11, width = 9, height = 5, dpi = 150)

## ---- fig11b. Rural vs urban: quantity per AE (one panel per item) ----------
ru_qty <- group_tables %>%
  filter(area %in% c("rural", "urban"), n_obs >= min_cell) %>%
  group_by(item_unit, area) %>%
  summarise(qty_pae = mean(qty_pae, na.rm = TRUE), .groups = "drop")
g11b <- ggplot(ru_qty, aes(area, qty_pae, fill = area)) +
  geom_col() +
  facet_wrap(~ item_unit, scales = "free_y") +
  labs(title = "Rural vs urban: quantity per adult equivalent",
       subtitle = "Each panel in its own unit (kg meats / litre milk / pieces eggs)",
       x = NULL, y = "Quantity per AE (unit in panel title)", fill = NULL)
ggsave(file.path(out_fig, "fig11b_rural_urban_quantity.png"), g11b, width = 10, height = 6, dpi = 150)

## ---- fig12. Engel curves: ln(expenditure) vs ln(welfare), by item ----------
g12 <- ggplot(est, aes(ln_w, ln_exp, colour = wave)) +
  geom_point(alpha = 0.12, size = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ item_label, scales = "free") +
  labs(title = "Engel curves: log expenditure vs log welfare per AE",
       x = "log(real expenditure per AE)", y = "log(item expenditure)", colour = NULL)
ggsave(file.path(out_fig, "fig12_engel_curves.png"), g12, width = 10, height = 6, dpi = 150)

## ---- fig13. ln(unit value) vs ln(welfare), by item -------------------------
g13 <- ggplot(est, aes(ln_w, ln_uv, colour = wave)) +
  geom_point(alpha = 0.12, size = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ item_label, scales = "free") +
  labs(title = "Log unit value vs log welfare per AE",
       x = "log(real expenditure per AE)", y = "log(unit value)", colour = NULL)
ggsave(file.path(out_fig, "fig13_quality_gradient.png"), g13, width = 10, height = 6, dpi = 150)

## ---- fig15. Quality elasticity by item with 95% CI -------------------------
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
       subtitle = "Pooled estimate in red",
       x = "Quality elasticity (eps_quality)", y = NULL)
ggsave(file.path(out_fig, "fig15_quality_forest.png"), g15, width = 8, height = 5, dpi = 150)

## ---- fig16. Source of consumed quantity, as shares -------------------------
src_long <- source_decomp %>%
  pivot_longer(c(Purchased, `Own production`, Gifts),
               names_to = "source", values_to = "qty_pae") %>%
  mutate(source = factor(source, levels = c("Purchased", "Own production", "Gifts")))
g16 <- ggplot(src_long, aes(item_label, qty_pae, fill = source)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Source of consumed quantity (shares)",
       subtitle = "Stacked shares of purchased / own production / gifts within each item",
       x = NULL, y = "Share of consumed quantity", fill = NULL) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(out_fig, "fig16_quantity_source_shares.png"), g16, width = 9, height = 5, dpi = 150)

## ---- fig17. Source shares by livestock ownership ---------------------------
src_liv <- source_by_livestock %>%
  pivot_longer(c(Purchased, `Own production`, Gifts),
               names_to = "source", values_to = "qty_pae") %>%
  mutate(source = factor(source, levels = c("Purchased", "Own production", "Gifts")))
g17 <- ggplot(src_liv, aes(item_label, qty_pae, fill = source)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ livestock_owner) +
  labs(title = "Source of quantity by livestock ownership (shares)",
       x = NULL, y = "Share of consumed quantity", fill = NULL) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))
ggsave(file.path(out_fig, "fig17_source_by_livestock.png"), g17, width = 10, height = 5, dpi = 150)

## ---- fig18. Expenditure per AE by livestock ownership ----------------------
g18 <- ggplot(item_by_livestock, aes(item_label, mean_exp_pae, fill = livestock_owner)) +
  geom_col(position = position_dodge()) +
  labs(title = "Expenditure per AE by livestock ownership",
       x = NULL, y = "Expenditure per AE (TSH, consumers)", fill = NULL) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(out_fig, "fig18_expenditure_by_livestock.png"), g18, width = 9, height = 5, dpi = 150)

## ---- fig19. Quality share of the expenditure elasticity, by quartile -------
g19 <- ggplot(quality_share, aes(group, quality_share_pct)) +
  geom_col(fill = "darkorange") +
  labs(title = "Quality share of the expenditure elasticity, by welfare quartile",
       x = "Welfare quartile (poor -> rich)", y = "Quality share (%)")
ggsave(file.path(out_fig, "fig19_quality_share.png"), g19, width = 8, height = 5, dpi = 150)

## ---- fig20. Unit value gradient, rural vs urban ----------------------------
uv_area <- group_tables %>%
  filter(area %in% c("rural", "urban"), n_obs >= min_cell)
g20 <- ggplot(uv_area, aes(group, unit_value, colour = area, group = area)) +
  geom_line() + geom_point() +
  facet_wrap(~ item_unit, scales = "free_y") +
  labs(title = "Unit value across welfare quartiles: rural vs urban",
       subtitle = "Quartiles within each area; each panel its own unit; cells < 30 consumers dropped",
       x = "Welfare quartile (within area)", y = "Median unit value (TSH per unit shown)",
       colour = NULL)
ggsave(file.path(out_fig, "fig20_unitvalue_rural_urban.png"), g20, width = 10, height = 6, dpi = 150)


## ============================================================================
## 12. DONE
## ============================================================================
cat("\nAll tables saved to:", normalizePath(out_tab), "\n")
cat("All figures saved to:", normalizePath(out_fig), "\n")
cat(" * Processed milk is not in these files; six items are analysed.\n")
cat(" * Welfare per AE is not level-comparable across waves; quartiles are built\n")
cat("   within wave and elasticities use wave fixed effects.\n")
