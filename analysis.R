###############################################################################
##  FOOD CONSUMPTION AND QUALITY UPGRADING IN TANZANIA
##  First-stage analysis: descriptive statistics + expenditure-elasticity
##  decomposition (quantity vs. quality) on the Tanzania NPS panel.
##
##  Data : NPS Wave 3, Wave 4, Wave 5 household master files (.xlsx).
##
##  METHOD: budget identity ln(expenditure) = ln(quantity) + ln(unit value);
##  three log-log regressions on log welfare per AE give
##  eps_expenditure = eps_quantity + eps_quality (Deaton 1988). Test
##  H0: eps_quality = 0 vs H1: eps_quality > 0 (region-clustered SE).
##
##  SETTINGS: six items (goat meat, beef, pork, chicken & poultry, eggs, fresh
##  milk); welfare = real total expenditure per adult equivalent; weighted
##  descriptives, unweighted regressions; units harmonised (kg / piece / litre);
##  outliers trimmed within item x wave; welfare groups = QUARTILES only.
##
##  Figures produced match the 15-point analysis outline (Section 11).
###############################################################################


## ============================================================================
## 0. SETUP
## ============================================================================
# install.packages(c("readxl","dplyr","tidyr","stringr","ggplot2",
#                     "purrr","broom","sandwich","lmtest","scales"))
library(readxl); library(dplyr); library(tidyr); library(stringr)
library(purrr); library(ggplot2); library(broom); library(sandwich)
library(lmtest); library(scales)

y3_path <- "C:/Users/Medei/OneDrive - Food and Agriculture Organization/Desktop/NPS_Y3_Tanzania_HH_Master_UnitValue.xlsx"
y4_path <- "C:/Users/Medei/OneDrive - Food and Agriculture Organization/Desktop/NPS_Y4_Tanzania_HH_Master.xlsx"
y5_path <- "C:/Users/Medei/OneDrive - Food and Agriculture Organization/Desktop/NPS_Y5_Tanzania_HH_Master (1).xlsx"

out_tab <- "output/tables"
out_fig <- "output/figures"
dir.create(out_tab, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig, recursive = TRUE, showWarnings = FALSE)


## ============================================================================
## 1. HELPER FUNCTIONS
## ============================================================================
read_wave <- function(path) read_excel(path, sheet = "HH_Data", skip = 1, .name_repair = "minimal")

pick_num <- function(df, patterns) {
  hit <- names(df)[map_lgl(names(df), ~ all(str_detect(.x, fixed(patterns))))]
  if (length(hit) == 0) return(rep(NA_real_, nrow(df)))
  suppressWarnings(as.numeric(df[[hit[1]]]))
}
pick_chr <- function(df, patterns) {
  hit <- names(df)[map_lgl(names(df), ~ all(str_detect(.x, fixed(patterns))))]
  if (length(hit) == 0) return(rep(NA_character_, nrow(df)))
  as.character(df[[hit[1]]])
}

## Convert a quantity to its standard unit (1=kg,2=g,3=litre,4=ml,5=pieces).
normalise_qty <- function(qty, unit, type) {
  out <- rep(NA_real_, length(qty))
  if (type == "meat") {
    out[unit %in% 1] <- qty[unit %in% 1]
    out[unit %in% 2] <- qty[unit %in% 2] / 1000
  } else if (type == "litre") {
    out[unit %in% 3] <- qty[unit %in% 3]
    out[unit %in% 4] <- qty[unit %in% 4] / 1000
  } else if (type == "pieces") {
    out[unit %in% 5] <- qty[unit %in% 5]
  }
  out
}

wtd_quantile <- function(x, w, probs) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]; o <- order(x); x <- x[o]; w <- w[o]
  approx(cumsum(w) / sum(w), x, xout = probs, rule = 2, ties = "ordered")$y
}
wtd_group <- function(x, w, n, labels) {
  cuts <- c(-Inf, wtd_quantile(x, w, seq_len(n - 1) / n), Inf)
  cut(x, breaks = cuts, labels = labels, include.lowest = TRUE)
}
wmean <- function(x, w) { ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_); sum(x[ok] * w[ok]) / sum(w[ok]) }
wmedian <- function(x, w) { ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_); wtd_quantile(x[ok], w[ok], 0.5) }
trim_to_na <- function(x, lo = 0.01, hi = 0.99) {
  qs <- quantile(x, c(lo, hi), na.rm = TRUE, names = FALSE)
  ifelse(is.finite(x) & x >= qs[1] & x <= qs[2], x, NA_real_)
}


## ============================================================================
## 2. PER-WAVE CONFIGURATION
## ============================================================================
wave_cfg <- list(
  Y3 = list(path = y3_path, food_prefix = "hh_j",  hhid = "y3_hhid", weight = "y3_weight",      welfare_kind = "total"),
  Y4 = list(path = y4_path, food_prefix = "hh_j",  hhid = "y4_hhid", weight = "hhweight",       welfare_kind = "total"),
  Y5 = list(path = y5_path, food_prefix = "hh_ja", hhid = "y5_hhid", weight = "y5_crossweight", welfare_kind = "pae")
)
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
## 3. EXTRACTION
## ============================================================================
extract_household <- function(df, cfg, wave) {
  adulteq <- pick_num(df, c("adulteq", "Adult equivalent"))
  expmR   <- if (cfg$welfare_kind == "pae") pick_num(df, c("expmR_pae", "per adult equivalent"))
             else pick_num(df, c("expmR", "Real total monthly expenditure (TSH, deflated)"))
  welfare_pae <- if (cfg$welfare_kind == "pae") expmR else expmR / adulteq
  urban_raw <- pick_num(df, c("urban", "Urban/Rural (1=Rural, 2=Urban)"))
  livestock <- pick_num(df, c("lf02_any_livestock", "owns at least one livestock"))
  tibble(
    wave = wave, hhid = pick_chr(df, cfg$hhid),
    hhsize = pick_num(df, c("hh_hhsize", "Household size (total members")),
    adulteq = adulteq, weight = pick_num(df, cfg$weight),
    region = pick_num(df, c("region", "Region code")),
    rural = as.integer(urban_raw == 1), urban = as.integer(urban_raw == 2),
    livestock = ifelse(is.na(livestock), 0L, as.integer(livestock == 1)),
    welfare_pae = welfare_pae
  )
}
extract_one_code <- function(df, prefix, code, type) {
  tag <- paste0("itemcode=", code, "]")
  yn     <- pick_num(df, c(prefix, tag, "eat/drink any"))
  q_tot  <- pick_num(df, c(prefix, tag, "in total did your household consume", "QUANTITY"))
  u_tot  <- pick_num(df, c(prefix, tag, "in total did your household consume", "UNIT"))
  q_buy  <- pick_num(df, c(prefix, tag, "came from purchases", "QUANTITY"))
  u_buy  <- pick_num(df, c(prefix, tag, "came from purchases", "UNIT"))
  q_own  <- pick_num(df, c(prefix, tag, "came from own production", "QUANTITY"))
  u_own  <- pick_num(df, c(prefix, tag, "came from own production", "UNIT"))
  q_gift <- pick_num(df, c(prefix, tag, "came from gifts", "QUANTITY"))
  u_gift <- pick_num(df, c(prefix, tag, "came from gifts", "UNIT"))
  spend  <- pick_num(df, c(prefix, tag, "How much did you spend"))
  tibble(consumed = as.integer(yn == 1),
         qty_total = normalise_qty(q_tot, u_tot, type),
         qty_purch = normalise_qty(q_buy, u_buy, type),
         qty_own   = normalise_qty(q_own, u_own, type),
         qty_gift  = normalise_qty(q_gift, u_gift, type),
         exp = spend)
}
extract_item <- function(df, prefix, codes, type) {
  parts <- map(codes, ~ extract_one_code(df, prefix, .x, type))
  sum_na <- function(a, b) ifelse(is.na(a) & is.na(b), NA, rowSums(cbind(a, b), na.rm = TRUE))
  reduce(parts, function(a, b) tibble(
    consumed  = pmax(a$consumed, b$consumed, na.rm = TRUE),
    qty_total = sum_na(a$qty_total, b$qty_total),
    qty_purch = sum_na(a$qty_purch, b$qty_purch),
    qty_own   = sum_na(a$qty_own,   b$qty_own),
    qty_gift  = sum_na(a$qty_gift,  b$qty_gift),
    exp       = sum_na(a$exp,       b$exp)))
}


## ============================================================================
## 4. BUILD THE POOLED DATASET
## ============================================================================
hh_list <- list(); item_list <- list()
for (wave in names(wave_cfg)) {
  cfg <- wave_cfg[[wave]]; message("Reading wave ", wave, " ...")
  df  <- read_wave(cfg$path)
  hh  <- extract_household(df, cfg, wave); hh_list[[wave]] <- hh
  for (i in seq_len(nrow(item_cfg))) {
    it <- item_cfg[i, ]
    codes <- str_split(if (wave == "Y5") it$codes_y5 else it$codes_std, ",")[[1]]
    ext <- extract_item(df, cfg$food_prefix, codes, it$type)
    ext$item <- it$item; ext$hhid <- hh$hhid; ext$wave <- wave
    item_list[[paste(wave, it$item)]] <- ext
  }
}
household <- bind_rows(hh_list)

items <- bind_rows(item_list) %>%
  left_join(household, by = c("wave", "hhid")) %>%
  mutate(unit_value = ifelse(qty_purch > 0 & exp > 0, exp / qty_purch, NA_real_))

## trim outliers within item x wave (1st-99th percentile)
items <- items %>%
  group_by(item, wave) %>%
  mutate(across(c(unit_value, qty_total, qty_purch, qty_own, qty_gift, exp), trim_to_na)) %>%
  ungroup() %>%
  mutate(qty_total_pae = qty_total / adulteq,
         qty_purch_pae = qty_purch / adulteq,
         qty_own_pae   = qty_own   / adulteq,
         qty_gift_pae  = qty_gift  / adulteq,
         exp_pae       = exp / adulteq,
         item_label = item_cfg$label[match(item, item_cfg$item)],
         unit       = item_cfg$unit[match(item, item_cfg$item)])
items$item_label <- factor(items$item_label, levels = item_cfg$label)
items$item_unit  <- factor(paste0(items$item_label, " (", items$unit, ")"),
                           levels = paste0(item_cfg$label, " (", item_cfg$unit, ")"))

## value of consumption (for basket composition only; does not affect elasticities)
items <- items %>%
  group_by(item, wave) %>%
  mutate(price = ifelse(is.finite(unit_value), unit_value, wmedian(unit_value, weight))) %>%
  ungroup() %>%
  mutate(cons_value = qty_total * price) %>%
  group_by(wave, hhid) %>%
  mutate(asf_value = sum(cons_value, na.rm = TRUE),
         budget_share = ifelse(asf_value > 0, cons_value / asf_value, NA_real_)) %>%
  ungroup()


## ============================================================================
## 5. DESCRIPTIVE TABLES
## ============================================================================
## (1) household summary by wave and pooled
hh_summary <- bind_rows(
  household %>% group_by(wave) %>% summarise(
    n_households = n(), mean_hhsize = wmean(hhsize, weight), mean_adulteq = wmean(adulteq, weight),
    pct_rural = 100 * wmean(rural, weight), pct_urban = 100 * wmean(urban, weight),
    pct_livestock = 100 * wmean(livestock, weight), mean_welfare_pae = wmean(welfare_pae, weight),
    .groups = "drop"),
  household %>% summarise(
    wave = "Pooled", n_households = n(), mean_hhsize = wmean(hhsize, weight),
    mean_adulteq = wmean(adulteq, weight), pct_rural = 100 * wmean(rural, weight),
    pct_urban = 100 * wmean(urban, weight), pct_livestock = 100 * wmean(livestock, weight),
    mean_welfare_pae = wmean(welfare_pae, weight)))
write.csv(hh_summary, file.path(out_tab, "01_household_summary.csv"), row.names = FALSE)
print(hh_summary)

## (1b / outline 1) consumption and expenditure by product
item_summary <- items %>% group_by(item_label) %>% summarise(
  unit = first(unit),
  pct_consuming = 100 * wmean(consumed, weight),
  mean_qty_pae  = wmean(ifelse(qty_total > 0, qty_total_pae, NA), weight),
  mean_exp_pae  = wmean(ifelse(exp > 0, exp_pae, NA), weight),
  median_unit_value = wmedian(unit_value, weight),
  .groups = "drop")
write.csv(item_summary, file.path(out_tab, "02_item_summary.csv"), row.names = FALSE)
print(item_summary)

## item summary by livestock ownership
item_by_livestock <- items %>%
  mutate(L = ifelse(livestock == 1, "Owns livestock", "No livestock")) %>%
  group_by(item_label, livestock_owner = L) %>% summarise(
    pct_consuming = 100 * wmean(consumed, weight),
    mean_qty_pae = wmean(ifelse(qty_total > 0, qty_total_pae, NA), weight),
    mean_exp_pae = wmean(ifelse(exp > 0, exp_pae, NA), weight),
    median_unit_value = wmedian(unit_value, weight), .groups = "drop")
write.csv(item_by_livestock, file.path(out_tab, "02b_item_by_livestock.csv"), row.names = FALSE)

## (7) sources of consumption (shares of total quantity)
src0 <- function(x) coalesce(x, 0)
source_decomp <- items %>% filter(qty_total > 0) %>% group_by(item_label) %>% summarise(
  Purchased = wmean(src0(qty_purch_pae), weight),
  `Own production` = wmean(src0(qty_own_pae), weight),
  Gifts = wmean(src0(qty_gift_pae), weight), .groups = "drop")
write.csv(source_decomp, file.path(out_tab, "02c_quantity_source.csv"), row.names = FALSE)

## (5) consumption trends across waves
cons_wave <- items %>% group_by(item_label, wave) %>%
  summarise(qty_pae = wmean(ifelse(qty_total > 0, qty_total_pae, NA), weight), .groups = "drop")
write.csv(cons_wave, file.path(out_tab, "03_consumption_by_wave.csv"), row.names = FALSE)


## ============================================================================
## 6. WELFARE QUARTILES AND GROUP MEANS
## ============================================================================
household <- household %>% group_by(wave) %>%
  mutate(quartile = wtd_group(welfare_pae, weight, 4, paste0("Q", 1:4))) %>% ungroup()
items <- items %>% select(-any_of("quartile")) %>%
  left_join(household %>% select(wave, hhid, quartile), by = c("wave", "hhid"))

group_means <- function(data, area = c("all", "rural", "urban")) {
  area <- match.arg(area); d <- data
  if (area == "rural") d <- filter(d, rural == 1)
  if (area == "urban") d <- filter(d, urban == 1)
  d %>% group_by(wave) %>%
    mutate(group = wtd_group(welfare_pae, weight, 4, paste0("Q", 1:4))) %>%
    ungroup() %>% filter(!is.na(group)) %>%
    group_by(item_label, group) %>% summarise(
      unit = first(unit),
      n_obs = sum(qty_total > 0 & is.finite(weight), na.rm = TRUE),
      qty_pae = wmean(ifelse(qty_total > 0, qty_total_pae, NA), weight),
      exp_pae = wmean(ifelse(exp > 0, exp_pae, NA), weight),
      unit_value = wmedian(unit_value, weight), .groups = "drop") %>%
    mutate(area = area)
}
group_tables <- bind_rows(group_means(items, "all"), group_means(items, "rural"), group_means(items, "urban")) %>%
  mutate(item_unit = factor(paste0(item_label, " (", unit, ")"),
                            levels = paste0(item_cfg$label, " (", item_cfg$unit, ")")))
write.csv(group_tables, file.path(out_tab, "04_group_means.csv"), row.names = FALSE)

## (11) basket composition by quartile
budget_comp <- items %>% filter(!is.na(quartile), qty_total > 0) %>%
  group_by(quartile, item_label) %>%
  summarise(share = 100 * wmean(budget_share, weight), .groups = "drop")
write.csv(budget_comp, file.path(out_tab, "05_budget_composition.csv"), row.names = FALSE)


## ============================================================================
## 7. INEQUALITY RATIOS Q4 / Q1  (outline 12)
## ============================================================================
min_cell <- 30
ratios <- group_tables %>% filter(group %in% c("Q1", "Q4")) %>%
  mutate(end = ifelse(group == "Q1", "bot", "top")) %>%
  pivot_wider(id_cols = c(item_label, unit, area), names_from = end,
              values_from = c(n_obs, qty_pae, exp_pae, unit_value)) %>%
  mutate(enough = pmin(n_obs_bot, n_obs_top) >= min_cell,
         n_min = pmin(n_obs_bot, n_obs_top),
         qty_ratio_Q4_Q1 = ifelse(enough, qty_pae_top / qty_pae_bot, NA_real_),
         exp_ratio_Q4_Q1 = ifelse(enough, exp_pae_top / exp_pae_bot, NA_real_),
         uv_ratio_Q4_Q1  = ifelse(enough, unit_value_top / unit_value_bot, NA_real_)) %>%
  select(item_label, unit, area, n_min, qty_ratio_Q4_Q1, exp_ratio_Q4_Q1, uv_ratio_Q4_Q1)
write.csv(ratios, file.path(out_tab, "06_Q4_Q1_ratios.csv"), row.names = FALSE)
print(as.data.frame(filter(ratios, area == "all")), digits = 3)


## ============================================================================
## 8. ELASTICITIES  (outline 13, 14)
## ============================================================================
est <- items %>%
  filter(qty_purch > 0, exp > 0, is.finite(unit_value), welfare_pae > 0, adulteq > 0) %>%
  mutate(ln_exp = log(exp), ln_q = log(qty_purch), ln_uv = log(unit_value),
         ln_w = log(welfare_pae), ln_ae = log(adulteq),
         wave = factor(wave), region = factor(region))
vcov_cl <- function(m) vcovCL(m, cluster = model.frame(m)$region)
one_sided_quality_test <- function(model) {
  ct <- coeftest(model, vcov = vcov_cl(model))
  b <- ct["ln_w", "Estimate"]; se <- ct["ln_w", "Std. Error"]; tval <- b / se
  c(estimate = b, se = se, t = tval, p_one_sided = pt(tval, df.residual(model), lower.tail = FALSE))
}
estimate_item <- function(d) {
  ctrl <- "ln_w + ln_ae + rural + wave + region"
  m_exp <- lm(as.formula(paste("ln_exp ~", ctrl)), d)
  m_q   <- lm(as.formula(paste("ln_q   ~", ctrl)), d)
  m_uv  <- lm(as.formula(paste("ln_uv  ~", ctrl)), d)
  rob <- function(m) coeftest(m, vcov = vcov_cl(m))["ln_w", "Estimate"]
  qt <- one_sided_quality_test(m_uv)
  tibble(n = nrow(d), eps_expenditure = rob(m_exp), eps_quantity = rob(m_q),
         eps_quality = qt["estimate"], quality_se = qt["se"], quality_t = qt["t"],
         quality_p_1side = qt["p_one_sided"], reject_H0_5pct = qt["p_one_sided"] < 0.05)
}
elasticities <- est %>% group_by(item_label) %>% group_modify(~ estimate_item(.x)) %>% ungroup()

est_pooled <- est %>% mutate(item = factor(item))
rob_coef <- function(m) coeftest(m, vcov = vcov_cl(m))["ln_w", "Estimate"]
m_uv_p <- lm(ln_uv ~ ln_w + ln_ae + rural + wave + region + item, est_pooled)
qt_p <- one_sided_quality_test(m_uv_p)
pooled_row <- tibble(item_label = "ALL ITEMS (pooled)", n = nrow(est_pooled),
  eps_expenditure = rob_coef(lm(ln_exp ~ ln_w + ln_ae + rural + wave + region + item, est_pooled)),
  eps_quantity    = rob_coef(lm(ln_q   ~ ln_w + ln_ae + rural + wave + region + item, est_pooled)),
  eps_quality = qt_p["estimate"], quality_se = qt_p["se"], quality_t = qt_p["t"],
  quality_p_1side = qt_p["p_one_sided"], reject_H0_5pct = qt_p["p_one_sided"] < 0.05)
elasticities <- bind_rows(elasticities, pooled_row)
write.csv(elasticities, file.path(out_tab, "07_elasticities.csv"), row.names = FALSE)
cat("\n==== Elasticity decomposition (eps_expenditure = eps_quantity + eps_quality) ====\n")
print(as.data.frame(elasticities), digits = 3)

## elasticities by subgroup (outline 15)
estimate_subgroup <- function(d, label) {
  fx <- "ln_w + ln_ae + wave + region + item"
  qt <- one_sided_quality_test(lm(as.formula(paste("ln_uv ~", fx)), d))
  tibble(subgroup = label, n = nrow(d),
         eps_expenditure = rob_coef(lm(as.formula(paste("ln_exp ~", fx)), d)),
         eps_quantity    = rob_coef(lm(as.formula(paste("ln_q ~",   fx)), d)),
         eps_quality = qt["estimate"], quality_se = qt["se"], quality_p_1side = qt["p_one_sided"])
}
subgroup_elast <- bind_rows(
  estimate_subgroup(filter(est_pooled, rural == 1), "Rural"),
  estimate_subgroup(filter(est_pooled, urban == 1), "Urban"),
  estimate_subgroup(filter(est_pooled, livestock == 1), "Owns livestock"),
  estimate_subgroup(filter(est_pooled, livestock == 0), "No livestock"))
write.csv(subgroup_elast, file.path(out_tab, "07b_elasticities_by_subgroup.csv"), row.names = FALSE)
print(as.data.frame(subgroup_elast), digits = 3)

## quality elasticity by quartile (table)
est_q <- est_pooled %>% filter(!is.na(quartile)) %>% mutate(quartile = factor(quartile))
m_byq <- lm(ln_uv ~ quartile + quartile:ln_w + ln_ae + rural + wave + region + item, est_q)
ctq <- coeftest(m_byq, vcov = vcov_cl(m_byq))
sr <- grep("ln_w", rownames(ctq), value = TRUE)
quality_by_quartile <- tibble(quartile = str_extract(sr, "Q[1-4]"),
  eps_quality = ctq[sr, "Estimate"], se = ctq[sr, "Std. Error"]) %>%
  mutate(p_one_sided = pt(eps_quality / se, df.residual(m_byq), lower.tail = FALSE)) %>%
  arrange(quartile)
write.csv(quality_by_quartile, file.path(out_tab, "08_quality_by_quartile.csv"), row.names = FALSE)

## mean welfare per AE by quartile and wave (income levels differ by wave)
q_income_by_wave <- household %>% filter(!is.na(quartile)) %>%
  group_by(wave, quartile) %>% summarise(mean_welfare_pae = wmean(welfare_pae, weight), .groups = "drop")
write.csv(q_income_by_wave, file.path(out_tab, "09_quartile_income_by_wave.csv"), row.names = FALSE)

## (15) heterogeneity tests: does the quality elasticity differ by group?
het_test <- function(inter, nm) {
  m <- lm(as.formula(paste0("ln_uv ~ ln_w + ln_w:", inter, " + ln_ae + ", inter,
                            " + wave + region + item")), est_pooled)
  ct <- coeftest(m, vcov = vcov_cl(m)); r <- grep(paste0("ln_w:", inter), rownames(ct), value = TRUE)[1]
  tibble(test = nm, difference = ct[r, 1], se = ct[r, 2], p_value = ct[r, 4])
}
het_tests <- bind_rows(het_test("urban", "Urban vs rural"), het_test("livestock", "Livestock owner vs non-owner"))
write.csv(het_tests, file.path(out_tab, "10_heterogeneity_tests.csv"), row.names = FALSE)
cat("\n==== Heterogeneity tests ====\n"); print(as.data.frame(het_tests), digits = 4)


## ============================================================================
## 9. FIGURES  (only the ones in the analysis outline)
## ============================================================================
theme_set(theme_minimal(base_size = 12))
pal <- c("Goat meat"="#E69F00","Beef"="#D55E00","Pork"="#CC79A7",
         "Chicken & poultry"="#009E73","Eggs"="#F0E442","Fresh milk"="#56B4E9")
gt_all <- group_tables %>% filter(area == "all")
idx_q <- function(d, col) d %>% group_by(item_label) %>% arrange(group) %>%
  mutate(Index = 100 * .data[[col]] / first(na.omit(.data[[col]]))) %>% ungroup()

## fig01 (outline 3) — share of households consuming each item
g <- ggplot(item_summary, aes(reorder(item_label, pct_consuming), pct_consuming, fill = item_label)) +
  geom_col(show.legend = FALSE) + scale_fill_manual(values = pal) + coord_flip() +
  labs(title = "Share of households consuming each item", x = NULL, y = "% (past 7 days)")
ggsave(file.path(out_fig, "fig01_participation.png"), g, width = 8, height = 5, dpi = 150)

## fig02 (outline 4) — product-level descriptives: expenditure per AE by item
g <- ggplot(item_summary, aes(reorder(item_label, mean_exp_pae), mean_exp_pae, fill = item_label)) +
  geom_col(show.legend = FALSE) + scale_fill_manual(values = pal) + coord_flip() +
  labs(title = "Expenditure per adult equivalent by item (consumers)",
       x = NULL, y = "Expenditure per AE (TSH, 7 days)")
ggsave(file.path(out_fig, "fig02_product_descriptives.png"), g, width = 8, height = 5, dpi = 150)

## fig03 (outline 5) — consumption trends across waves (quantity per AE, index)
g <- cons_wave %>% group_by(item_label) %>% arrange(wave) %>%
  mutate(Index = 100 * qty_pae / first(na.omit(qty_pae))) %>% ungroup() %>%
  ggplot(aes(wave, Index, colour = item_label, group = item_label)) +
  geom_line(linewidth = 1) + geom_point() + scale_colour_manual(values = pal) +
  labs(title = "Consumption trends across survey waves",
       subtitle = "Quantity per adult equivalent, index Wave Y3 = 100",
       x = "Survey wave", y = "Quantity per AE (index)", colour = NULL)
ggsave(file.path(out_fig, "fig03_consumption_by_wave.png"), g, width = 9, height = 5, dpi = 150)

## fig04 (outline 6a) — household composition (rural / urban / livestock) by wave
g <- hh_summary %>% filter(wave != "Pooled") %>%
  select(wave, `Rural %` = pct_rural, `Urban %` = pct_urban, `Owns livestock %` = pct_livestock) %>%
  pivot_longer(-wave, names_to = "indicator", values_to = "pct") %>%
  ggplot(aes(wave, pct, fill = indicator)) + geom_col(position = position_dodge()) +
  labs(title = "Household composition by wave (urban / rural / livestock)",
       x = NULL, y = "%", fill = NULL)
ggsave(file.path(out_fig, "fig04_hh_composition_by_wave.png"), g, width = 8, height = 5, dpi = 150)

## fig05 (outline 6b) — urban vs rural consumption (expenditure per AE, TSH)
g <- group_tables %>% filter(area %in% c("rural", "urban"), n_obs >= min_cell) %>%
  group_by(item_label, area) %>% summarise(exp_pae = mean(exp_pae, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(item_label, exp_pae, fill = area)) + geom_col(position = position_dodge()) +
  labs(title = "Urban vs rural consumption", subtitle = "Expenditure per adult equivalent (TSH)",
       x = NULL, y = "Expenditure per AE (TSH, 7 days)", fill = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(file.path(out_fig, "fig05_urban_rural_consumption.png"), g, width = 9, height = 5, dpi = 150)

## fig06 (outline 7) — sources of food consumption (shares)
g <- source_decomp %>%
  pivot_longer(c(Purchased, `Own production`, Gifts), names_to = "Source", values_to = "v") %>%
  mutate(Source = factor(Source, levels = c("Purchased", "Own production", "Gifts"))) %>%
  ggplot(aes(item_label, v, fill = Source)) + geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Sources of food consumption", subtitle = "Shares of purchased / own production / gifts",
       x = NULL, y = "Share of consumed quantity", fill = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(file.path(out_fig, "fig06_sources.png"), g, width = 9, height = 5, dpi = 150)

## fig07 (outline 8) — consumption per AE by income quartile (six items, index)
g <- gt_all %>% filter(n_obs >= min_cell) %>% idx_q("qty_pae") %>%
  ggplot(aes(group, Index, colour = item_label, group = item_label)) +
  geom_line(linewidth = 1) + geom_point() + scale_colour_manual(values = pal) +
  labs(title = "Consumption per adult equivalent by income quartile",
       subtitle = "Pooled sample; index, poorest quartile = 100",
       x = "Income quartile (poor -> rich)", y = "Quantity per AE (index)", colour = NULL)
ggsave(file.path(out_fig, "fig07_consumption_by_quartile.png"), g, width = 9, height = 5, dpi = 150)

## fig08 (outline 9) — expenditure per AE by income quartile (six items, index)
g <- gt_all %>% filter(n_obs >= min_cell) %>% idx_q("exp_pae") %>%
  ggplot(aes(group, Index, colour = item_label, group = item_label)) +
  geom_line(linewidth = 1) + geom_point() + scale_colour_manual(values = pal) +
  labs(title = "Expenditure per adult equivalent by income quartile",
       subtitle = "Pooled sample; index, poorest quartile = 100",
       x = "Income quartile (poor -> rich)", y = "Expenditure per AE (index)", colour = NULL)
ggsave(file.path(out_fig, "fig08_expenditure_by_quartile.png"), g, width = 9, height = 5, dpi = 150)

## fig09 (outline 10) — six panels: quantity, expenditure and unit value (index)
panel <- gt_all %>% filter(n_obs >= min_cell) %>% group_by(item_unit) %>% arrange(group) %>%
  mutate(Quantity = 100 * qty_pae / first(na.omit(qty_pae)),
         Expenditure = 100 * exp_pae / first(na.omit(exp_pae)),
         `Unit value` = 100 * unit_value / first(na.omit(unit_value))) %>% ungroup() %>%
  select(item_unit, group, Quantity, Expenditure, `Unit value`) %>%
  pivot_longer(c(Quantity, Expenditure, `Unit value`), names_to = "Measure", values_to = "Index") %>%
  mutate(Measure = factor(Measure, levels = c("Expenditure", "Quantity", "Unit value")))
g <- ggplot(panel, aes(group, Index, colour = Measure, group = Measure)) +
  geom_line(linewidth = 1) + geom_point() + facet_wrap(~ item_unit, scales = "free_y") +
  scale_colour_manual(values = c(Expenditure = "#D55E00", Quantity = "#0072B2", `Unit value` = "#009E73")) +
  labs(title = "Quantity, expenditure and unit value across income quartiles",
       subtitle = "Index, poorest quartile = 100; expenditure above quantity = quality",
       x = "Income quartile (poor -> rich)", y = "Index (poorest quartile = 100)", colour = NULL)
ggsave(file.path(out_fig, "fig09_qty_exp_unitvalue_panels.png"), g, width = 11, height = 6, dpi = 150)

## fig10 (outline 11) — composition of the animal-food basket by income
g <- ggplot(budget_comp, aes(quartile, share, fill = item_label)) + geom_col() +
  scale_fill_manual(values = pal) +
  labs(title = "Composition of the animal-food basket by income",
       subtitle = "Share of each item in total animal-food value (per household)",
       x = "Income quartile (poor -> rich)", y = "% of animal-food value", fill = NULL)
ggsave(file.path(out_fig, "fig10_basket_composition.png"), g, width = 9, height = 5, dpi = 150)

## fig11 (outline 14) — quality effect: expenditure elasticity = quantity + quality
g <- elasticities %>% filter(item_label != "ALL ITEMS (pooled)") %>%
  select(item_label, Quantity = eps_quantity, Quality = eps_quality) %>%
  pivot_longer(-item_label, names_to = "Component", values_to = "Elasticity") %>%
  ggplot(aes(item_label, Elasticity, fill = Component)) + geom_col() +
  scale_fill_manual(values = c(Quantity = "#0072B2", Quality = "#D55E00")) +
  labs(title = "Expenditure elasticity = quantity + quality",
       subtitle = "The quality part (top) is the quality-upgrading wedge",
       x = NULL, y = "Elasticity w.r.t. real expenditure per AE", fill = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(file.path(out_fig, "fig11_elasticity_decomposition.png"), g, width = 9, height = 5, dpi = 150)

## fig12 (outline 15) — heterogeneity: quality elasticity by subgroup, 95% CI
g <- subgroup_elast %>%
  mutate(lo = eps_quality - 1.96 * quality_se, hi = eps_quality + 1.96 * quality_se,
         subgroup = factor(subgroup, levels = rev(c("Rural", "Urban", "Owns livestock", "No livestock")))) %>%
  ggplot(aes(eps_quality, subgroup)) + geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.25) + geom_point(size = 3, colour = "#0072B2") +
  labs(title = "Quality upgrading by subgroup (95% CI)",
       subtitle = "Quality elasticity: rural vs urban, and livestock ownership",
       x = "Quality elasticity", y = NULL)
ggsave(file.path(out_fig, "fig12_heterogeneity.png"), g, width = 8, height = 5, dpi = 150)


## ============================================================================
## 10. DONE
## ============================================================================
cat("\nTables saved to:", normalizePath(out_tab), "\n")
cat("Figures saved to:", normalizePath(out_fig), "(12 figures, fig01-fig12)\n")
