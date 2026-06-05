###############################################################################
##  FOOD CONSUMPTION AND QUALITY UPGRADING IN TANZANIA
##  Tanzania NPS waves 3-5. Outputs follow the 15-point analysis outline,
##  in order. Only the requested tables and figures are produced.
##
##  Method: ln(expenditure) = ln(quantity) + ln(unit value) =>
##  eps_expenditure = eps_quantity + eps_quality (Deaton 1988). Welfare = real
##  total expenditure per adult equivalent; weighted descriptives; quartiles;
##  region-clustered standard errors.
###############################################################################

library(readxl); library(dplyr); library(tidyr); library(stringr)
library(purrr); library(ggplot2); library(sandwich); library(lmtest); library(scales)

y3_path <- "C:/Users/Medei/OneDrive - Food and Agriculture Organization/Desktop/NPS_Y3_Tanzania_HH_Master_UnitValue.xlsx"
y4_path <- "C:/Users/Medei/OneDrive - Food and Agriculture Organization/Desktop/NPS_Y4_Tanzania_HH_Master.xlsx"
y5_path <- "C:/Users/Medei/OneDrive - Food and Agriculture Organization/Desktop/NPS_Y5_Tanzania_HH_Master (1).xlsx"

out_tab <- "output/tables"; out_fig <- "output/figures"
dir.create(out_tab, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig, recursive = TRUE, showWarnings = FALSE)


## ============================================================================
## HELPERS
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
normalise_qty <- function(qty, unit, type) {
  out <- rep(NA_real_, length(qty))
  if (type == "meat") { out[unit %in% 1] <- qty[unit %in% 1]; out[unit %in% 2] <- qty[unit %in% 2]/1000
  } else if (type == "litre") { out[unit %in% 3] <- qty[unit %in% 3]; out[unit %in% 4] <- qty[unit %in% 4]/1000
  } else if (type == "pieces") { out[unit %in% 5] <- qty[unit %in% 5] }
  out
}
wtd_quantile <- function(x, w, probs) {
  ok <- is.finite(x) & is.finite(w) & w > 0; x <- x[ok]; w <- w[ok]
  o <- order(x); x <- x[o]; w <- w[o]
  approx(cumsum(w)/sum(w), x, xout = probs, rule = 2, ties = "ordered")$y
}
wtd_group <- function(x, w, n, labels) cut(x, c(-Inf, wtd_quantile(x, w, seq_len(n-1)/n), Inf),
                                           labels = labels, include.lowest = TRUE)
wmean <- function(x, w) { ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) NA_real_ else sum(x[ok]*w[ok])/sum(w[ok]) }
wmedian <- function(x, w) { ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) NA_real_ else wtd_quantile(x[ok], w[ok], 0.5) }
trim_to_na <- function(x, lo = 0.01, hi = 0.99) {
  qs <- quantile(x, c(lo, hi), na.rm = TRUE, names = FALSE)
  ifelse(is.finite(x) & x >= qs[1] & x <= qs[2], x, NA_real_)
}


## ============================================================================
## CONFIGURATION
## ============================================================================
wave_cfg <- list(
  Y3 = list(path = y3_path, food_prefix = "hh_j",  hhid = "y3_hhid", weight = "y3_weight",      welfare_kind = "total"),
  Y4 = list(path = y4_path, food_prefix = "hh_j",  hhid = "y4_hhid", weight = "hhweight",       welfare_kind = "total"),
  Y5 = list(path = y5_path, food_prefix = "hh_ja", hhid = "y5_hhid", weight = "y5_crossweight", welfare_kind = "pae")
)
item_cfg <- tibble::tribble(
  ~item,        ~label,               ~type,    ~unit,    ~codes_std, ~codes_y5,
  "goat_meat",  "Goat meat",          "meat",   "kg",     "801",      "801",
  "beef",       "Beef",               "meat",   "kg",     "802",      "802",
  "pork",       "Pork",               "meat",   "kg",     "803",      "803",
  "chicken",    "Chicken & poultry",  "meat",   "kg",     "804",      "8041,8042",
  "eggs",       "Eggs",               "pieces", "piece",  "807",      "807",
  "fresh_milk", "Fresh milk",         "litre",  "litre",  "901",      "901"
)
pal <- c("Goat meat"="#E69F00","Beef"="#D55E00","Pork"="#CC79A7",
         "Chicken & poultry"="#009E73","Eggs"="#F0E442","Fresh milk"="#56B4E9")


## ============================================================================
## EXTRACTION + BUILD
## ============================================================================
extract_household <- function(df, cfg, wave) {
  adulteq <- pick_num(df, c("adulteq", "Adult equivalent"))
  expmR   <- if (cfg$welfare_kind == "pae") pick_num(df, c("expmR_pae", "per adult equivalent"))
             else pick_num(df, c("expmR", "Real total monthly expenditure (TSH, deflated)"))
  welfare_pae <- if (cfg$welfare_kind == "pae") expmR else expmR / adulteq
  urb <- pick_num(df, c("urban", "Urban/Rural (1=Rural, 2=Urban)"))
  liv <- pick_num(df, c("lf02_any_livestock", "owns at least one livestock"))
  tibble(wave = wave, hhid = pick_chr(df, cfg$hhid),
         hhsize = pick_num(df, c("hh_hhsize", "Household size (total members")),
         adulteq = adulteq, weight = pick_num(df, cfg$weight),
         region = pick_num(df, c("region", "Region code")),
         rural = as.integer(urb == 1), urban = as.integer(urb == 2),
         livestock = ifelse(is.na(liv), 0L, as.integer(liv == 1)), welfare_pae = welfare_pae)
}
extract_one_code <- function(df, prefix, code, type) {
  tag <- paste0("itemcode=", code, "]")
  tibble(
    consumed  = as.integer(pick_num(df, c(prefix, tag, "eat/drink any")) == 1),
    qty_total = normalise_qty(pick_num(df, c(prefix, tag, "in total did your household consume", "QUANTITY")),
                              pick_num(df, c(prefix, tag, "in total did your household consume", "UNIT")), type),
    qty_purch = normalise_qty(pick_num(df, c(prefix, tag, "came from purchases", "QUANTITY")),
                              pick_num(df, c(prefix, tag, "came from purchases", "UNIT")), type),
    qty_own   = normalise_qty(pick_num(df, c(prefix, tag, "came from own production", "QUANTITY")),
                              pick_num(df, c(prefix, tag, "came from own production", "UNIT")), type),
    qty_gift  = normalise_qty(pick_num(df, c(prefix, tag, "came from gifts", "QUANTITY")),
                              pick_num(df, c(prefix, tag, "came from gifts", "UNIT")), type),
    exp       = pick_num(df, c(prefix, tag, "How much did you spend")))
}
extract_item <- function(df, prefix, codes, type) {
  parts <- map(codes, ~ extract_one_code(df, prefix, .x, type))
  sum_na <- function(a, b) ifelse(is.na(a) & is.na(b), NA, rowSums(cbind(a, b), na.rm = TRUE))
  reduce(parts, function(a, b) tibble(
    consumed = pmax(a$consumed, b$consumed, na.rm = TRUE),
    qty_total = sum_na(a$qty_total, b$qty_total), qty_purch = sum_na(a$qty_purch, b$qty_purch),
    qty_own = sum_na(a$qty_own, b$qty_own), qty_gift = sum_na(a$qty_gift, b$qty_gift),
    exp = sum_na(a$exp, b$exp)))
}

hh_list <- list(); item_list <- list()
for (wave in names(wave_cfg)) {
  cfg <- wave_cfg[[wave]]; message("Reading wave ", wave, " ...")
  df <- read_wave(cfg$path); hh <- extract_household(df, cfg, wave); hh_list[[wave]] <- hh
  for (i in seq_len(nrow(item_cfg))) {
    it <- item_cfg[i, ]; codes <- str_split(if (wave == "Y5") it$codes_y5 else it$codes_std, ",")[[1]]
    ext <- extract_item(df, cfg$food_prefix, codes, it$type)
    ext$item <- it$item; ext$hhid <- hh$hhid; ext$wave <- wave
    item_list[[paste(wave, it$item)]] <- ext
  }
}
household <- bind_rows(hh_list)

items <- bind_rows(item_list) %>% left_join(household, by = c("wave", "hhid")) %>%
  mutate(unit_value = ifelse(qty_purch > 0 & exp > 0, exp / qty_purch, NA_real_))

## ---- DATA CLEANING / OUTLIER HANDLING --------------------------------------
## We do NOT keep every row blindly. Survey unit values sometimes contain
## implausible numbers (e.g. a beef unit value of millions of TSH/kg from a
## mis-recorded quantity). Within each item x wave we drop the bottom 1% and the
## top 1% of unit value, quantity and expenditure (trim_to_na): those values are
## set to NA and so are excluded from descriptives, ratios and regressions.
## The report below documents, per item x wave, the unit-value range before
## trimming and how many values are flagged, so the cleaning is transparent.
clean_report <- items %>% group_by(item, wave) %>% summarise(
  n_purchasers = sum(is.finite(unit_value)),
  uv_min    = suppressWarnings(min(unit_value, na.rm = TRUE)),
  uv_median = median(unit_value, na.rm = TRUE),
  uv_p99    = quantile(unit_value, 0.99, na.rm = TRUE, names = FALSE),
  uv_max    = suppressWarnings(max(unit_value, na.rm = TRUE)),
  n_trimmed_unit_value = sum(is.finite(unit_value) & is.na(trim_to_na(unit_value))),
  .groups = "drop")
write.csv(clean_report, file.path(out_tab, "00_data_cleaning_report.csv"), row.names = FALSE)

items <- items %>%
  group_by(item, wave) %>%
  mutate(across(c(unit_value, qty_total, qty_purch, qty_own, qty_gift, exp), trim_to_na)) %>%
  ungroup() %>%
  mutate(qty_total_pae = qty_total/adulteq, qty_purch_pae = qty_purch/adulteq,
         qty_own_pae = qty_own/adulteq, qty_gift_pae = qty_gift/adulteq, exp_pae = exp/adulteq,
         item_label = factor(item_cfg$label[match(item, item_cfg$item)], levels = item_cfg$label),
         unit = item_cfg$unit[match(item, item_cfg$item)])
items$item_unit <- factor(paste0(items$item_label, " (", items$unit, ")"),
                          levels = paste0(item_cfg$label, " (", item_cfg$unit, ")"))
items <- items %>% group_by(item, wave) %>%
  mutate(price = ifelse(is.finite(unit_value), unit_value, wmedian(unit_value, weight))) %>%
  ungroup() %>% mutate(cons_value = qty_total * price) %>%
  group_by(wave, hhid) %>%
  mutate(asf_value = sum(cons_value, na.rm = TRUE),
         budget_share = ifelse(asf_value > 0, cons_value/asf_value, NA_real_)) %>% ungroup()

## welfare quartiles within wave
household <- household %>% group_by(wave) %>%
  mutate(quartile = wtd_group(welfare_pae, weight, 4, paste0("Q", 1:4))) %>% ungroup()
items <- items %>% left_join(household %>% select(wave, hhid, quartile), by = c("wave", "hhid"))


## ============================================================================
## COMPUTATIONS (objects used by the outputs below)
## ============================================================================
src0 <- function(x) coalesce(x, 0)

## item descriptives (consumers)
item_summary <- items %>% group_by(Item = item_label) %>% summarise(
  Unit = first(unit), `% consuming` = 100*wmean(consumed, weight),
  `Quantity/AE` = wmean(ifelse(qty_total > 0, qty_total_pae, NA), weight),
  `Expenditure/AE` = wmean(ifelse(exp > 0, exp_pae, NA), weight),
  `Unit value (median)` = wmedian(unit_value, weight), .groups = "drop")

## consumption by wave (quantity per AE)
cons_wave <- items %>% group_by(Item = item_label, wave) %>%
  summarise(qty_pae = wmean(ifelse(qty_total > 0, qty_total_pae, NA), weight), .groups = "drop")

## sources (shares of total quantity)
source_decomp <- items %>% filter(qty_total > 0) %>% group_by(Item = item_label) %>% summarise(
  Purchased = wmean(src0(qty_purch_pae), weight), `Own production` = wmean(src0(qty_own_pae), weight),
  Gifts = wmean(src0(qty_gift_pae), weight), .groups = "drop")

## group means by quartile (all / rural / urban)
group_means <- function(d2, area) {
  if (area == "rural") d2 <- filter(d2, rural == 1)
  if (area == "urban") d2 <- filter(d2, urban == 1)
  d2 %>% group_by(wave) %>% mutate(group = wtd_group(welfare_pae, weight, 4, paste0("Q", 1:4))) %>%
    ungroup() %>% filter(!is.na(group)) %>% group_by(item_label, group) %>% summarise(
      unit = first(unit), n_obs = sum(qty_total > 0 & is.finite(weight), na.rm = TRUE),
      qty_pae = wmean(ifelse(qty_total > 0, qty_total_pae, NA), weight),
      exp_pae = wmean(ifelse(exp > 0, exp_pae, NA), weight),
      unit_value = wmedian(unit_value, weight), .groups = "drop") %>% mutate(area = area)
}
group_tables <- bind_rows(group_means(items, "all"), group_means(items, "rural"), group_means(items, "urban")) %>%
  mutate(item_unit = factor(paste0(item_label, " (", unit, ")"),
                            levels = paste0(item_cfg$label, " (", item_cfg$unit, ")")))
gt_all <- filter(group_tables, area == "all")
min_cell <- 30
idx_q <- function(d, col) d %>% group_by(item_label) %>% arrange(group) %>%
  mutate(Index = 100*.data[[col]]/first(na.omit(.data[[col]]))) %>% ungroup()

## basket composition
budget_comp <- items %>% filter(!is.na(quartile), qty_total > 0) %>%
  group_by(quartile, item_label) %>% summarise(share = 100*wmean(budget_share, weight), .groups = "drop")

## Q4/Q1 ratios (all households)
ratios <- gt_all %>% filter(group %in% c("Q1", "Q4")) %>%
  mutate(end = ifelse(group == "Q1", "bot", "top")) %>%
  pivot_wider(id_cols = item_label, names_from = end, values_from = c(n_obs, qty_pae, exp_pae, unit_value)) %>%
  mutate(enough = pmin(n_obs_bot, n_obs_top) >= min_cell,
         `Quantity ratio (Q4/Q1)`   = ifelse(enough, qty_pae_top/qty_pae_bot, NA_real_),
         `Expenditure ratio (Q4/Q1)`= ifelse(enough, exp_pae_top/exp_pae_bot, NA_real_),
         `Unit value ratio (Q4/Q1)` = ifelse(enough, unit_value_top/unit_value_bot, NA_real_)) %>%
  select(Item = item_label, `Quantity ratio (Q4/Q1)`, `Expenditure ratio (Q4/Q1)`, `Unit value ratio (Q4/Q1)`)

## elasticities (Deaton)
est <- items %>% filter(qty_purch > 0, exp > 0, is.finite(unit_value), welfare_pae > 0, adulteq > 0) %>%
  mutate(ln_exp = log(exp), ln_q = log(qty_purch), ln_uv = log(unit_value),
         ln_w = log(welfare_pae), ln_ae = log(adulteq), wave = factor(wave), region = factor(region))
vcov_cl <- function(m) vcovCL(m, cluster = model.frame(m)$region)
rob <- function(m) coeftest(m, vcov = vcov_cl(m))["ln_w", c("Estimate", "Std. Error")]
elast_item <- function(d) {
  e <- rob(lm(ln_exp ~ ln_w + ln_ae + rural + wave + region, d))
  q <- rob(lm(ln_q   ~ ln_w + ln_ae + rural + wave + region, d))
  v <- rob(lm(ln_uv  ~ ln_w + ln_ae + rural + wave + region, d))
  tibble(`Quantity elasticity` = q[1], `Expenditure elasticity` = e[1],
         `Quality (wedge)` = v[1], quality_se = v[2],
         p_value = pt(v[1]/v[2], nrow(d)-1, lower.tail = FALSE))
}
elasticities <- est %>% group_by(Item = item_label) %>% group_modify(~ elast_item(.x)) %>% ungroup() %>%
  mutate(`Exp > Qty` = `Expenditure elasticity` > `Quantity elasticity`)
est_p <- est %>% mutate(item = factor(item))
ep <- rob(lm(ln_exp ~ ln_w + ln_ae + rural + wave + region + item, est_p))
qp <- rob(lm(ln_q   ~ ln_w + ln_ae + rural + wave + region + item, est_p))
vp <- rob(lm(ln_uv  ~ ln_w + ln_ae + rural + wave + region + item, est_p))
elasticities <- bind_rows(elasticities, tibble(Item = "ALL ITEMS",
  `Quantity elasticity` = qp[1], `Expenditure elasticity` = ep[1], `Quality (wedge)` = vp[1],
  quality_se = vp[2], p_value = pt(vp[1]/vp[2], nrow(est_p)-1, lower.tail = FALSE),
  `Exp > Qty` = ep[1] > qp[1]))

## quality elasticity by subgroup (full decomposition) — rural / urban / livestock
sub_elast <- function(d, label) {
  e <- rob(lm(ln_exp ~ ln_w + ln_ae + wave + region + item, d))
  q <- rob(lm(ln_q   ~ ln_w + ln_ae + wave + region + item, d))
  v <- rob(lm(ln_uv  ~ ln_w + ln_ae + wave + region + item, d))
  tibble(subgroup = label, n = nrow(d),
         eps_expenditure = e[1], eps_quantity = q[1], eps_quality = v[1],
         quality_se = v[2], p_value = pt(v[1]/v[2], nrow(d)-1, lower.tail = FALSE))
}
subgroup_elast <- bind_rows(
  sub_elast(filter(est_p, rural == 1), "Rural"),
  sub_elast(filter(est_p, urban == 1), "Urban"),
  sub_elast(filter(est_p, livestock == 1), "Owns livestock"),
  sub_elast(filter(est_p, livestock == 0), "No livestock"))
## hypothesis tests: does the quality elasticity differ by group?
het_test <- function(inter) { m <- lm(as.formula(paste0("ln_uv ~ ln_w + ln_w:", inter,
  " + ln_ae + ", inter, " + wave + region + item")), est_p)
  ct <- coeftest(m, vcov = vcov_cl(m)); r <- grep(paste0("ln_w:", inter), rownames(ct), value = TRUE)[1]
  c(diff = ct[r, 1], se = ct[r, 2], p = ct[r, 4]) }
het_urban <- het_test("urban"); het_liv <- het_test("livestock")
het_tests <- tibble(
  test = c("Urban vs rural (quality elasticity difference)",
           "Livestock owner vs non-owner (difference)"),
  difference = c(het_urban["diff"], het_liv["diff"]),
  se = c(het_urban["se"], het_liv["se"]),
  p_value = c(het_urban["p"], het_liv["p"]))
## quality elasticity by income quartile (for "from what income does it matter")
est_q <- est_p %>% filter(!is.na(quartile)) %>% mutate(quartile = factor(quartile))
m_byq <- lm(ln_uv ~ quartile + quartile:ln_w + ln_ae + rural + wave + region + item, est_q)
ctq <- coeftest(m_byq, vcov = vcov_cl(m_byq)); sr <- grep("ln_w", rownames(ctq), value = TRUE)
quality_by_quartile <- tibble(quartile = str_extract(sr, "Q[1-4]"),
  eps_quality = ctq[sr, 1], se = ctq[sr, 2]) %>%
  mutate(p_value = pt(eps_quality / se, df.residual(m_byq), lower.tail = FALSE)) %>%
  arrange(quartile)
n_q_sig <- sum(quality_by_quartile$p_value < 0.05)




## ============================================================================
## OUTPUTS — in a sensible order
##   A. Descriptive tables        (01-04)
##   B. Descriptive figures       (fig01-fig06)
##   C. Consumption by income     (05-06 tables, fig07-fig10)
##   D. Elasticities & quality    (07-10 tables, fig11-fig12)
## ============================================================================
theme_set(theme_minimal(base_size = 12))


## ----------------------------------------------------------------------------
## A. DESCRIPTIVE TABLES
## ----------------------------------------------------------------------------

## 01 Sample descriptive statistics: full sample, rural/urban, livestock
samp <- function(d, name) d %>% summarise(Group = name, Households = n(),
  `HH size` = wmean(hhsize, weight), `Adult eq.` = wmean(adulteq, weight),
  `Rural %` = 100*wmean(rural, weight), `Urban %` = 100*wmean(urban, weight),
  `Livestock %` = 100*wmean(livestock, weight), `Welfare per AE` = wmean(welfare_pae, weight))
tab_01 <- bind_rows(samp(household, "Full sample"),
  samp(filter(household, urban == 0), "Rural"), samp(filter(household, urban == 1), "Urban"),
  samp(filter(household, livestock == 1), "Owns livestock"),
  samp(filter(household, livestock == 0), "No livestock"))
write.csv(tab_01, file.path(out_tab, "01_sample_descriptives.csv"), row.names = FALSE)

## 02 Consumption and expenditure by product
write.csv(item_summary, file.path(out_tab, "02_consumption_expenditure_by_product.csv"), row.names = FALSE)

## 03 Consumption levels by product across waves
write.csv(cons_wave %>% pivot_wider(names_from = wave, values_from = qty_pae),
          file.path(out_tab, "03_consumption_by_wave.csv"), row.names = FALSE)

## 04 Sources of consumption (shares of quantity)
src_share <- source_decomp %>%
  mutate(tot = Purchased + `Own production` + Gifts,
         `Purchased %` = 100*Purchased/tot, `Own production %` = 100*`Own production`/tot,
         `Gifts %` = 100*Gifts/tot) %>%
  select(Item, `Purchased %`, `Own production %`, `Gifts %`)
write.csv(src_share, file.path(out_tab, "04_sources_shares.csv"), row.names = FALSE)


## ----------------------------------------------------------------------------
## B. DESCRIPTIVE FIGURES
## ----------------------------------------------------------------------------

## fig01 Share of households consuming each item
g <- ggplot(item_summary, aes(reorder(Item, `% consuming`), `% consuming`, fill = Item)) +
  geom_col(show.legend = FALSE) + scale_fill_manual(values = pal) + coord_flip() +
  labs(title = "Share of households consuming each item", x = NULL, y = "% (past 7 days)")
ggsave(file.path(out_fig, "fig01_share_consumers.png"), g, width = 8, height = 5, dpi = 150)

## fig02 Product-level descriptives: expenditure per AE by item
g <- ggplot(item_summary, aes(reorder(Item, `Expenditure/AE`), `Expenditure/AE`, fill = Item)) +
  geom_col(show.legend = FALSE) + scale_fill_manual(values = pal) + coord_flip() +
  labs(title = "Product-level descriptive statistics", subtitle = "Expenditure per adult equivalent (consumers)",
       x = NULL, y = "Expenditure per AE (TSH, 7 days)")
ggsave(file.path(out_fig, "fig02_product_descriptives.png"), g, width = 8, height = 5, dpi = 150)

## fig03 Consumption trends across survey waves
g <- cons_wave %>% group_by(Item) %>% arrange(wave) %>%
  mutate(Index = 100*qty_pae/first(na.omit(qty_pae))) %>% ungroup() %>%
  ggplot(aes(wave, Index, colour = Item, group = Item)) + geom_line(linewidth = 1) + geom_point() +
  scale_colour_manual(values = pal) +
  labs(title = "Consumption trends across survey waves", subtitle = "Quantity per AE, index Wave Y3 = 100",
       x = "Survey wave", y = "Quantity per AE (index)", colour = NULL)
ggsave(file.path(out_fig, "fig03_consumption_trends_waves.png"), g, width = 9, height = 5, dpi = 150)

## fig04 Household composition (urban/rural) by wave
g <- household %>% group_by(wave) %>%
  summarise(`Rural %` = 100*wmean(rural, weight), `Urban %` = 100*wmean(urban, weight), .groups = "drop") %>%
  pivot_longer(-wave, names_to = "Area", values_to = "pct") %>%
  ggplot(aes(wave, pct, fill = Area)) + geom_col(position = position_dodge()) +
  labs(title = "Household composition by wave (urban / rural)", x = NULL, y = "%", fill = NULL)
ggsave(file.path(out_fig, "fig04_hh_composition_by_wave.png"), g, width = 8, height = 5, dpi = 150)

## fig05 Urban vs rural consumption (expenditure per AE)
g <- group_tables %>% filter(area %in% c("rural", "urban"), n_obs >= min_cell) %>%
  group_by(item_label, area) %>% summarise(exp_pae = mean(exp_pae, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(item_label, exp_pae, fill = area)) + geom_col(position = position_dodge()) +
  labs(title = "Urban vs rural consumption", subtitle = "Expenditure per adult equivalent (TSH)",
       x = NULL, y = "Expenditure per AE (TSH, 7 days)", fill = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(file.path(out_fig, "fig05_urban_rural_consumption.png"), g, width = 9, height = 5, dpi = 150)

## fig06 Sources of food consumption
g <- source_decomp %>% pivot_longer(c(Purchased, `Own production`, Gifts), names_to = "Source", values_to = "v") %>%
  mutate(Source = factor(Source, levels = c("Purchased", "Own production", "Gifts"))) %>%
  ggplot(aes(Item, v, fill = Source)) + geom_col(position = "fill") + scale_y_continuous(labels = scales::percent) +
  labs(title = "Sources of food consumption", subtitle = "Market purchases / own production / gifts",
       x = NULL, y = "Share of consumed quantity", fill = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(file.path(out_fig, "fig06_sources.png"), g, width = 9, height = 5, dpi = 150)


## ----------------------------------------------------------------------------
## C. CONSUMPTION & EXPENDITURE BY INCOME QUARTILE
## ----------------------------------------------------------------------------

## 05 Group means by quartile (all / rural / urban)
write.csv(group_tables %>% select(item_label, group, area, n_obs, qty_pae, exp_pae, unit_value),
          file.path(out_tab, "05_group_means_by_quartile.csv"), row.names = FALSE)

## fig07 Consumption per AE by quartile (six products)
g <- gt_all %>% filter(n_obs >= min_cell) %>% idx_q("qty_pae") %>%
  ggplot(aes(group, Index, colour = item_label, group = item_label)) + geom_line(linewidth = 1) + geom_point() +
  scale_colour_manual(values = pal) +
  labs(title = "Consumption per adult equivalent by income quartile", subtitle = "Pooled; index, Q1 = 100",
       x = "Income quartile (poor -> rich)", y = "Quantity per AE (index)", colour = NULL)
ggsave(file.path(out_fig, "fig07_consumption_by_quartile.png"), g, width = 9, height = 5, dpi = 150)

## fig08 Expenditure per AE by quartile (six products)
g <- gt_all %>% filter(n_obs >= min_cell) %>% idx_q("exp_pae") %>%
  ggplot(aes(group, Index, colour = item_label, group = item_label)) + geom_line(linewidth = 1) + geom_point() +
  scale_colour_manual(values = pal) +
  labs(title = "Expenditure per adult equivalent by income quartile", subtitle = "Pooled; index, Q1 = 100",
       x = "Income quartile (poor -> rich)", y = "Expenditure per AE (index)", colour = NULL)
ggsave(file.path(out_fig, "fig08_expenditure_by_quartile.png"), g, width = 9, height = 5, dpi = 150)

## fig09 Consumption vs expenditure vs unit value (six panels)
panel <- gt_all %>% filter(n_obs >= min_cell) %>% group_by(item_unit) %>% arrange(group) %>%
  mutate(Quantity = 100*qty_pae/first(na.omit(qty_pae)), Expenditure = 100*exp_pae/first(na.omit(exp_pae)),
         `Unit value` = 100*unit_value/first(na.omit(unit_value))) %>% ungroup() %>%
  select(item_unit, group, Quantity, Expenditure, `Unit value`) %>%
  pivot_longer(c(Quantity, Expenditure, `Unit value`), names_to = "Measure", values_to = "Index") %>%
  mutate(Measure = factor(Measure, levels = c("Expenditure", "Quantity", "Unit value")))
g <- ggplot(panel, aes(group, Index, colour = Measure, group = Measure)) + geom_line(linewidth = 1) + geom_point() +
  facet_wrap(~ item_unit, scales = "free_y") +
  scale_colour_manual(values = c(Expenditure = "#D55E00", Quantity = "#0072B2", `Unit value` = "#009E73")) +
  labs(title = "Consumption vs expenditure vs unit value across income quartiles",
       subtitle = "Index, Q1 = 100; expenditure above quantity = quality", x = "Income quartile (poor -> rich)",
       y = "Index (Q1 = 100)", colour = NULL)
ggsave(file.path(out_fig, "fig09_consumption_vs_expenditure_panels.png"), g, width = 11, height = 6, dpi = 150)

## fig10 Composition of the animal-food basket by income
g <- ggplot(budget_comp, aes(quartile, share, fill = item_label)) + geom_col() + scale_fill_manual(values = pal) +
  labs(title = "Composition of the animal-food basket by income",
       subtitle = "Share of each item in total animal-food value (per household)",
       x = "Income quartile (poor -> rich)", y = "% of animal-food value", fill = NULL)
ggsave(file.path(out_fig, "fig10_basket_composition.png"), g, width = 9, height = 5, dpi = 150)

## 06 Inequality ratios Q4/Q1
write.csv(ratios, file.path(out_tab, "06_inequality_ratios_Q4_Q1.csv"), row.names = FALSE)
cat("\n==== Inequality ratios Q4/Q1 ====\n"); print(as.data.frame(ratios), digits = 3)


## ----------------------------------------------------------------------------
## D. ELASTICITIES, QUALITY EFFECT AND HETEROGENEITY (with hypothesis tests)
## ----------------------------------------------------------------------------

## 07 Elasticities by product (quantity, expenditure, quality wedge, test)
write.csv(elasticities, file.path(out_tab, "07_elasticities_by_product.csv"), row.names = FALSE)
cat("\n==== Elasticities by product ====\n"); print(as.data.frame(elasticities), digits = 3)

## fig11 Quality effect: expenditure elasticity = quantity + quality
g <- elasticities %>% filter(Item != "ALL ITEMS") %>%
  select(Item, Quantity = `Quantity elasticity`, Quality = `Quality (wedge)`) %>%
  pivot_longer(-Item, names_to = "Component", values_to = "Elasticity") %>%
  ggplot(aes(Item, Elasticity, fill = Component)) + geom_col() +
  scale_fill_manual(values = c(Quantity = "#0072B2", Quality = "#D55E00")) +
  labs(title = "The quality effect: expenditure elasticity = quantity + quality",
       subtitle = "The quality part (top) is the quality-upgrading wedge",
       x = NULL, y = "Elasticity w.r.t. real expenditure per AE", fill = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(file.path(out_fig, "fig11_quality_effect.png"), g, width = 9, height = 5, dpi = 150)

## 08 Elasticities by subgroup: rural / urban / livestock owner / non-owner
write.csv(subgroup_elast, file.path(out_tab, "08_elasticities_by_subgroup.csv"), row.names = FALSE)
cat("\n==== Elasticities by subgroup (rural/urban, livestock) ====\n")
print(as.data.frame(subgroup_elast), digits = 3)

## 09 Quality elasticity by income quartile (from what income does quality matter)
write.csv(quality_by_quartile, file.path(out_tab, "09_quality_elasticity_by_quartile.csv"), row.names = FALSE)
cat("\n==== Quality elasticity by income quartile ====\n")
print(as.data.frame(quality_by_quartile), digits = 3)
cat(sprintf("Quality elasticity significant in %d of 4 income quartiles.\n", n_q_sig))

## 10 Heterogeneity hypothesis tests (income x group interaction)
write.csv(het_tests, file.path(out_tab, "10_heterogeneity_tests.csv"), row.names = FALSE)
cat("\n==== Heterogeneity tests ====\n"); print(as.data.frame(het_tests), digits = 4)

## fig12 Heterogeneity in quality upgrading (quality elasticity by subgroup, 95% CI)
sub <- sprintf("Urban vs rural: diff %+.3f (p=%.2f).  Livestock: diff %+.3f (p=%.2f).",
               het_urban["diff"], het_urban["p"], het_liv["diff"], het_liv["p"])
g <- subgroup_elast %>% mutate(lo = eps_quality - 1.96*quality_se, hi = eps_quality + 1.96*quality_se,
       subgroup = factor(subgroup, levels = rev(c("Rural", "Urban", "Owns livestock", "No livestock")))) %>%
  ggplot(aes(eps_quality, subgroup)) + geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.25) + geom_point(size = 3, colour = "#0072B2") +
  labs(title = "Heterogeneity in quality upgrading (95% CI)", subtitle = sub,
       x = "Quality elasticity", y = NULL)
ggsave(file.path(out_fig, "fig12_heterogeneity.png"), g, width = 9, height = 5, dpi = 150)


cat("\nDone. Tables 01-10 and figures fig01-fig12 in",
    normalizePath(out_tab), "and", normalizePath(out_fig), "\n")
