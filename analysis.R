###############################################################################
#  Quality upgrading in the demand for animal-source foods — Tanzania (NPS)
#
#  Hypothesis: as household income rises, expenditure on animal products rises
#  FASTER than quantity, because households shift towards higher-quality items.
#  Using unit values (price paid per unit) and the budget identity
#        ln(expenditure) = ln(quantity) + ln(unit value)
#  the expenditure elasticity splits into a quantity and a quality part:
#        eps_expenditure = eps_quantity + eps_quality
#  We test   H0: eps_quality = 0   vs   H1: eps_quality > 0.
#
#  Data: NPS waves 3, 4, 5 (pooled). Items: goat meat, beef, pork,
#  chicken & poultry, eggs, fresh milk. Welfare = real total expenditure per
#  adult equivalent. Descriptives are survey-weighted; regressions are OLS.
#
#  Output:  results.xlsx  (several sheets)  +  4 figures in /figures
###############################################################################

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)   # write multi-sheet Excel
library(sandwich)   # robust standard errors
library(lmtest)

## ---- file paths (edit if needed) -------------------------------------------
paths <- c(
  Y3 = "C:/Users/Medei/OneDrive - Food and Agriculture Organization/Desktop/NPS_Y3_Tanzania_HH_Master_UnitValue.xlsx",
  Y4 = "C:/Users/Medei/OneDrive - Food and Agriculture Organization/Desktop/NPS_Y4_Tanzania_HH_Master.xlsx",
  Y5 = "C:/Users/Medei/OneDrive - Food and Agriculture Organization/Desktop/NPS_Y5_Tanzania_HH_Master (1).xlsx"
)
dir.create("figures", showWarnings = FALSE)


## ===========================================================================
## 1. HELPER FUNCTIONS
## ===========================================================================

# The data sheet has two header rows (section + label); we read with skip = 1
# so the descriptive labels become the column names, then pick columns by text.
read_wave <- function(p) read_excel(p, sheet = "HH_Data", skip = 1, .name_repair = "minimal")

# first numeric column whose name contains all the given pieces of text
pick <- function(df, ...) {
  keys <- c(...)
  hit  <- which(sapply(names(df), function(n) all(sapply(keys, grepl, n, fixed = TRUE))))
  if (length(hit) == 0) return(rep(NA_real_, nrow(df)))
  suppressWarnings(as.numeric(df[[hit[1]]]))
}
pick_id <- function(df, key) {
  hit <- which(grepl(key, names(df), fixed = TRUE))
  if (length(hit) == 0) return(rep(NA_character_, nrow(df)))
  as.character(df[[hit[1]]])
}

# convert a reported quantity to the standard unit (kg, litre or piece)
# unit codes: 1 = kg, 2 = g, 3 = litre, 4 = ml, 5 = pieces
to_std <- function(q, u, unit) {
  if (unit == "kg")    return(ifelse(u == 1, q, ifelse(u == 2, q / 1000, NA)))
  if (unit == "litre") return(ifelse(u == 3, q, ifelse(u == 4, q / 1000, NA)))
  ifelse(u == 5, q, NA)   # pieces
}

# weighted mean and weighted median, ignoring NA / non-positive weights
w_mean <- function(x, w) { ok <- is.finite(x) & is.finite(w) & w > 0
  if (any(ok)) sum(x[ok] * w[ok]) / sum(w[ok]) else NA_real_ }
w_quantile <- function(x, w, p) { ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]; o <- order(x); x <- x[o]; w <- w[o]
  approx(cumsum(w) / sum(w), x, p, rule = 2, ties = "ordered")$y }
w_median <- function(x, w) w_quantile(x, w, 0.5)

# trim values outside the 1st–99th percentile to NA (removes data-entry errors)
trim <- function(x) { q <- quantile(x, c(.01, .99), na.rm = TRUE, names = FALSE)
  ifelse(is.finite(x) & x >= q[1] & x <= q[2], x, NA_real_) }


## ===========================================================================
## 2. ITEM DEFINITIONS
## ===========================================================================
# unit = physical unit (never mixed on one axis); codes per wave (Y5 splits
# poultry into 8041 + 8042, summed back to "chicken & poultry").
items <- tibble::tribble(
  ~item,        ~label,               ~unit,    ~codes,        ~codes_y5,
  "goat",       "Goat meat",          "kg",     "801",         "801",
  "beef",       "Beef",               "kg",     "802",         "802",
  "pork",       "Pork",               "kg",     "803",         "803",
  "chicken",    "Chicken & poultry",  "kg",     "804",         "8041,8042",
  "eggs",       "Eggs",               "piece",  "807",         "807",
  "milk",       "Fresh milk",         "litre",  "901",         "901"
)
item_order <- items$label

# per wave: food-module prefix, household id, weight, welfare variable
wave_cfg <- list(
  Y3 = list(prefix = "hh_j",  id = "y3_hhid", weight = "y3_weight",     welfare = "expmR"),
  Y4 = list(prefix = "hh_j",  id = "y4_hhid", weight = "hhweight",      welfare = "expmR"),
  Y5 = list(prefix = "hh_ja", id = "y5_hhid", weight = "y5_crossweight",welfare = "expmR_pae")
)


## ===========================================================================
## 3. BUILD THE POOLED DATASET (one row per household × item)
## ===========================================================================
all_rows <- list()

for (w in names(paths)) {
  cfg <- wave_cfg[[w]]
  df  <- read_wave(paths[[w]])

  adulteq <- pick(df, "adulteq", "Adult equivalent")
  # welfare = real total monthly expenditure per adult equivalent
  welfare <- if (cfg$welfare == "expmR_pae")
    pick(df, "expmR_pae", "per adult equivalent")
  else pick(df, "expmR", "Real total monthly expenditure (TSH, deflated)") / adulteq

  urban_raw <- pick(df, "urban", "Urban/Rural (1=Rural, 2=Urban)")  # 1 = rural, 2 = urban
  livestock <- pick(df, "lf02_any_livestock", "owns at least one livestock")

  hh <- tibble(
    wave      = w,
    hhid      = pick_id(df, cfg$id),
    hhsize    = pick(df, "hh_hhsize", "Household size (total members"),
    adulteq   = adulteq,
    weight    = pick(df, cfg$weight),
    welfare   = welfare,
    urban     = as.integer(urban_raw == 2),
    livestock = ifelse(is.na(livestock), 0L, as.integer(livestock == 1))
  )

  for (i in seq_len(nrow(items))) {
    it    <- items[i, ]
    codes <- strsplit(if (w == "Y5") it$codes_y5 else it$codes, ",")[[1]]
    # sum across codes (only matters for Y5 chicken); NA -> 0 before summing
    z0  <- function(x) ifelse(is.na(x), 0, x)
    qty <- pur <- own <- gift <- val <- 0
    for (cd in codes) {
      tag <- paste0("itemcode=", cd, "]")
      qty  <- qty  + z0(to_std(pick(df, cfg$prefix, tag, "in total did your household consume", "QUANTITY"),
                               pick(df, cfg$prefix, tag, "in total did your household consume", "UNIT"), it$unit))
      pur  <- pur  + z0(to_std(pick(df, cfg$prefix, tag, "came from purchases", "QUANTITY"),
                               pick(df, cfg$prefix, tag, "came from purchases", "UNIT"), it$unit))
      own  <- own  + z0(to_std(pick(df, cfg$prefix, tag, "came from own production", "QUANTITY"),
                               pick(df, cfg$prefix, tag, "came from own production", "UNIT"), it$unit))
      gift <- gift + z0(to_std(pick(df, cfg$prefix, tag, "came from gifts", "QUANTITY"),
                               pick(df, cfg$prefix, tag, "came from gifts", "UNIT"), it$unit))
      val  <- val  + z0(pick(df, cfg$prefix, tag, "How much did you spend"))
    }
    all_rows[[paste(w, it$item)]] <- hh %>% mutate(
      item = it$item, label = it$label, unit = it$unit,
      qty_total = qty, qty_pur = pur, qty_own = own, qty_gift = gift, exp = val
    )
  }
}

dat <- bind_rows(all_rows) %>%
  mutate(label = factor(label, levels = item_order))

# trim outliers within item × wave; build unit value and per-AE measures
dat <- dat %>%
  group_by(item, wave) %>%
  mutate(across(c(qty_total, qty_pur, qty_own, qty_gift, exp), trim)) %>%
  ungroup() %>%
  mutate(unit_value = ifelse(qty_pur > 0 & exp > 0, exp / qty_pur, NA_real_))  # price per unit

# Value the WHOLE consumption at a price: the household's own unit value if it
# purchased, otherwise the item's median purchase price (so home production is
# valued at local market prices — standard practice). This keeps quantity and
# value on the same basis, unlike raw purchase spending.
dat <- dat %>%
  group_by(item, wave) %>%
  mutate(price = ifelse(is.finite(unit_value), unit_value, w_median(unit_value, weight))) %>%
  ungroup() %>%
  mutate(
    qty_pae   = qty_total / adulteq,                 # quantity consumed per AE
    value_pae = (qty_total * price) / adulteq         # value of consumption per AE
  )

# welfare QUARTILES, computed within wave (levels not comparable across waves)
add_quartiles <- function(d) d %>% group_by(wave) %>%
  mutate(q = cut(welfare,
                 breaks = c(-Inf, w_quantile(welfare, weight, c(.25, .5, .75)), Inf),
                 labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)) %>%
  ungroup()
dat <- add_quartiles(dat)


## ===========================================================================
## 4. DESCRIPTIVE STATISTICS
## ===========================================================================

# (a) household level, by wave + pooled
hh_one <- distinct(dat, wave, hhid, .keep_all = TRUE)
desc_hh <- bind_rows(
  hh_one %>% group_by(Wave = wave) %>% summarise(
    Households = n(),
    `Mean HH size` = w_mean(hhsize, weight),
    `Mean adult eq.` = w_mean(adulteq, weight),
    `Rural %` = 100 * w_mean(1 - urban, weight),
    `Urban %` = 100 * w_mean(urban, weight),
    `Owns livestock %` = 100 * w_mean(livestock, weight), .groups = "drop"),
  hh_one %>% summarise(
    Wave = "Pooled", Households = n(),
    `Mean HH size` = w_mean(hhsize, weight),
    `Mean adult eq.` = w_mean(adulteq, weight),
    `Rural %` = 100 * w_mean(1 - urban, weight),
    `Urban %` = 100 * w_mean(urban, weight),
    `Owns livestock %` = 100 * w_mean(livestock, weight))
)

# small reusable summariser (over consumers of the item)
# N_cons = households consuming (any source); N_buy = households that purchased
# (used to decide whether a unit-value cell is reliable).
item_stats <- function(d) d %>% summarise(
  `% consuming`      = 100 * w_mean(as.numeric(qty_total > 0), weight),
  `Quantity per AE`  = w_mean(ifelse(qty_total > 0, qty_pae, NA), weight),
  `Value per AE (TSH)` = w_mean(ifelse(qty_total > 0, value_pae, NA), weight),
  `Unit value (TSH, median)` = w_median(unit_value, weight),
  N_cons = sum(qty_total > 0, na.rm = TRUE),
  N_buy  = sum(is.finite(unit_value)), .groups = "drop")

# (b) overall, per item
desc_items <- dat %>% group_by(Item = label, Unit = unit) %>% item_stats()

# (c) per item × welfare quartile
desc_quart <- dat %>% filter(!is.na(q)) %>%
  group_by(Item = label, Unit = unit, Quartile = q) %>% item_stats()

# (d) per item × area (rural vs urban), overall
desc_area <- dat %>%
  mutate(Area = ifelse(urban == 1, "Urban", "Rural")) %>%
  group_by(Item = label, Unit = unit, Area) %>% item_stats()

# (e) per item × livestock ownership
desc_live <- dat %>%
  mutate(Livestock = ifelse(livestock == 1, "Owns livestock", "No livestock")) %>%
  group_by(Item = label, Unit = unit, Livestock) %>% item_stats()

# (f) source of consumed quantity, as shares of total (purchased / own / gifts)
desc_source <- dat %>% filter(qty_total > 0) %>%
  group_by(Item = label) %>% summarise(
    `Purchased %`      = 100 * w_mean(qty_pur,  weight) / w_mean(qty_pur + qty_own + qty_gift, weight),
    `Own production %` = 100 * w_mean(qty_own,  weight) / w_mean(qty_pur + qty_own + qty_gift, weight),
    `Gifts %`          = 100 * w_mean(qty_gift, weight) / w_mean(qty_pur + qty_own + qty_gift, weight),
    .groups = "drop")


## ===========================================================================
## 5. Q4 / Q1 RATIOS  (richest vs poorest quartile, per adult equivalent)
## ===========================================================================
# Quartiles give bigger cells than quintiles, so even pork has enough consumers.
# Quantity & value need >=30 consumers; the unit-value ratio needs >=30 buyers.
ratio_tab <- desc_quart %>%
  filter(Quartile %in% c("Q1", "Q4")) %>%
  select(Item, Quartile, q = `Quantity per AE`, v = `Value per AE (TSH)`,
         u = `Unit value (TSH, median)`, nc = N_cons, nb = N_buy) %>%
  pivot_wider(names_from = Quartile, values_from = c(q, v, u, nc, nb)) %>%
  transmute(Item,
            `Quantity Q4/Q1`   = ifelse(pmin(nc_Q1, nc_Q4) >= 30, q_Q4 / q_Q1, NA),
            `Value Q4/Q1`      = ifelse(pmin(nc_Q1, nc_Q4) >= 30, v_Q4 / v_Q1, NA),
            `Unit value Q4/Q1` = ifelse(pmin(nb_Q1, nb_Q4) >= 30, u_Q4 / u_Q1, NA))


## ===========================================================================
## 6. ELASTICITIES — classic unit-value method (Deaton 1988)
## ===========================================================================
# Three OLS regressions on log real expenditure per AE, with standard controls
# (log adult equivalents, urban dummy, wave dummies). Robust (HC1) std. errors.
# The PURCHASED quantity and value are used so that ln(exp)=ln(q)+ln(uv) holds.
reg_data <- dat %>%
  filter(qty_pur > 0, exp > 0, is.finite(unit_value), welfare > 0, adulteq > 0) %>%
  mutate(lx = log(welfare), lae = log(adulteq), wave = factor(wave))

elasticity <- function(d) {
  ctrl  <- "lx + lae + urban + wave"
  b <- function(y) {                 # robust coefficient + se on lx
    m  <- lm(as.formula(paste0("log(", y, ") ~ ", ctrl)), data = d)
    ct <- coeftest(m, vcov = vcovHC(m, type = "HC1"))
    ct["lx", c("Estimate", "Std. Error")]
  }
  be <- b("exp"); bq <- b("qty_pur"); bv <- b("unit_value")
  tibble(
    Expenditure_elast = be[1],
    Quantity_elast    = bq[1],
    Quality_elast     = bv[1],
    Quality_se        = bv[2],
    Quality_t         = bv[1] / bv[2],
    p_value_1sided    = pt(bv[1] / bv[2], df = nrow(d) - 1, lower.tail = FALSE)
  )
}

elast_items <- reg_data %>% group_by(Item = label) %>% group_modify(~ elasticity(.x)) %>% ungroup()

# pooled across items: add item dummies so different price levels do not
# contaminate the quality elasticity
mp <- function(y) { m <- lm(as.formula(paste0("log(", y, ") ~ lx + lae + urban + wave + label")), reg_data)
  coeftest(m, vcov = vcovHC(m, type = "HC1"))["lx", c("Estimate", "Std. Error")] }
be <- mp("exp"); bq <- mp("qty_pur"); bv <- mp("unit_value")
elast_pool <- tibble(Item = "ALL ITEMS (pooled)",
  Expenditure_elast = be[1], Quantity_elast = bq[1], Quality_elast = bv[1],
  Quality_se = bv[2], Quality_t = bv[1]/bv[2],
  p_value_1sided = pt(bv[1]/bv[2], df = nrow(reg_data) - 1, lower.tail = FALSE))

elast <- bind_rows(elast_items, elast_pool) %>%
  mutate(`Reject H0 (quality=0)` = p_value_1sided < 0.05)


## ===========================================================================
## 7. WRITE ONE EXCEL FILE WITH SEVERAL SHEETS
## ===========================================================================
round_df <- function(d, k = 3) mutate(d, across(where(is.numeric), ~ round(.x, k)))

write.xlsx(
  list(
    "Households"      = round_df(desc_hh, 1),
    "Items overall"   = round_df(desc_items),
    "By quartile"     = round_df(desc_quart),
    "Q4 over Q1"      = round_df(ratio_tab),
    "By rural-urban"  = round_df(desc_area),
    "By livestock"    = round_df(desc_live),
    "Quantity source" = round_df(desc_source, 1),
    "Elasticities"    = round_df(elast)
  ),
  file = "results.xlsx", overwrite = TRUE
)


## ===========================================================================
## 8. FOUR CLEAR FIGURES
## ===========================================================================
theme_set(theme_minimal(base_size = 12))

## Figure 1 — the hypothesis in one picture: from Q1 to Q4, the VALUE of
## consumption rises MORE than the quantity, and the unit value (quality) > 1.
f1 <- ratio_tab %>%
  pivot_longer(-Item, names_to = "Measure", values_to = "Ratio") %>%
  mutate(Measure = factor(Measure, levels = c("Quantity Q4/Q1", "Value Q4/Q1", "Unit value Q4/Q1")))
ggplot(f1, aes(Item, Ratio, fill = Measure)) +
  geom_col(position = position_dodge()) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(title = "Richest vs poorest quartile (Q4/Q1), per adult equivalent",
       subtitle = "Value rises more than quantity; the gap is paid as higher unit value (quality)",
       x = NULL, y = "Q4 / Q1 ratio", fill = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave("figures/fig1_Q4_Q1_ratios.png", width = 9, height = 5, dpi = 150)

## Figure 2 — elasticity decomposition: expenditure = quantity + quality
f2 <- elast_items %>%
  select(Item, Quantity = Quantity_elast, Quality = Quality_elast) %>%
  pivot_longer(-Item, names_to = "Component", values_to = "Elasticity")
ggplot(f2, aes(Item, Elasticity, fill = Component)) +
  geom_col() +
  labs(title = "Expenditure elasticity = quantity + quality",
       subtitle = "The quality part (top) is the quality-upgrading effect",
       x = NULL, y = "Elasticity w.r.t. real expenditure per AE", fill = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave("figures/fig2_elasticity_decomposition.png", width = 9, height = 5, dpi = 150)

## Figure 3 — quantity per adult equivalent rises with income (one panel per item)
f3 <- desc_quart %>% filter(N_cons >= 30) %>% mutate(Panel = paste0(Item, " (", Unit, ")"))
ggplot(f3, aes(Quartile, `Quantity per AE`, group = 1)) +
  geom_line(colour = "darkgreen") + geom_point(colour = "darkgreen") +
  facet_wrap(~ Panel, scales = "free_y") +
  labs(title = "Quantity per adult equivalent by welfare quartile",
       subtitle = "Each panel in its own unit (kg meats, litre milk, pieces eggs)",
       x = "Welfare quartile (poor → rich)", y = "Quantity per AE")
ggsave("figures/fig3_quantity_by_quartile.png", width = 9, height = 6, dpi = 150)

## Figure 4 — unit value (quality) rises with income (one panel per item)
f4 <- desc_quart %>% filter(N_buy >= 30) %>% mutate(Panel = paste0(Item, " (", Unit, ")"))
ggplot(f4, aes(Quartile, `Unit value (TSH, median)`, group = 1)) +
  geom_line(colour = "steelblue") + geom_point(colour = "steelblue") +
  facet_wrap(~ Panel, scales = "free_y") +
  labs(title = "Unit value (quality) by welfare quartile",
       subtitle = "Higher unit value at higher income = quality upgrading",
       x = "Welfare quartile (poor → rich)", y = "Median unit value (TSH per unit)")
ggsave("figures/fig4_unitvalue_by_quartile.png", width = 9, height = 6, dpi = 150)

cat("Done. See results.xlsx and the /figures folder.\n")
