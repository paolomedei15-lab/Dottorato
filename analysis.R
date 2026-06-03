###############################################################################
#  Quality upgrading in the demand for animal-source foods — Tanzania (NPS 3-5)
#
#  Hypothesis: as household income rises, the VALUE of animal-food consumption
#  rises FASTER than the quantity, because households trade up to higher-quality
#  items (higher unit value). With the identity
#        ln(value) = ln(quantity) + ln(unit value)
#  the total elasticity splits into a quantity and a quality part:
#        eps_value = eps_quantity + eps_quality ,   H0: eps_quality = 0.
#
#  Output:  results.xlsx  (several sheets)  +  /figures  (combined charts, all
#  items shown together; different units are made comparable with INDICES and
#  LOGS, never mixed on a raw axis).
###############################################################################

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(sandwich)
library(lmtest)

paths <- c(
  Y3 = "C:/Users/Medei/OneDrive - Food and Agriculture Organization/Desktop/NPS_Y3_Tanzania_HH_Master_UnitValue.xlsx",
  Y4 = "C:/Users/Medei/OneDrive - Food and Agriculture Organization/Desktop/NPS_Y4_Tanzania_HH_Master.xlsx",
  Y5 = "C:/Users/Medei/OneDrive - Food and Agriculture Organization/Desktop/NPS_Y5_Tanzania_HH_Master (1).xlsx"
)
dir.create("figures", showWarnings = FALSE)
theme_set(theme_minimal(base_size = 12))
pal <- c("Goat meat"="#E69F00","Beef"="#D55E00","Pork"="#CC79A7",
         "Chicken & poultry"="#009E73","Eggs"="#F0E442","Fresh milk"="#56B4E9")


## ===========================================================================
## 1. HELPERS
## ===========================================================================
read_wave <- function(p) read_excel(p, sheet = "HH_Data", skip = 1, .name_repair = "minimal")

pick <- function(df, ...) {                       # first numeric col matching all text bits
  keys <- c(...); hit <- which(sapply(names(df), function(n) all(sapply(keys, grepl, n, fixed = TRUE))))
  if (length(hit) == 0) return(rep(NA_real_, nrow(df)))
  suppressWarnings(as.numeric(df[[hit[1]]]))
}
pick_id <- function(df, key) { hit <- which(grepl(key, names(df), fixed = TRUE))
  if (length(hit) == 0) rep(NA_character_, nrow(df)) else as.character(df[[hit[1]]]) }

to_std <- function(q, u, unit) {                  # to kg / litre / piece
  if (unit == "kg")    return(ifelse(u == 1, q, ifelse(u == 2, q/1000, NA)))
  if (unit == "litre") return(ifelse(u == 3, q, ifelse(u == 4, q/1000, NA)))
  ifelse(u == 5, q, NA)
}
z0 <- function(x) ifelse(is.na(x), 0, x)

w_mean <- function(x, w) { ok <- is.finite(x) & is.finite(w) & w > 0
  if (any(ok)) sum(x[ok]*w[ok])/sum(w[ok]) else NA_real_ }
w_quantile <- function(x, w, p) { ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]; o <- order(x); x <- x[o]; w <- w[o]
  approx(cumsum(w)/sum(w), x, p, rule = 2, ties = "ordered")$y }
w_median <- function(x, w) w_quantile(x, w, 0.5)
trim <- function(x) { q <- quantile(x, c(.01,.99), na.rm = TRUE, names = FALSE)
  ifelse(is.finite(x) & x >= q[1] & x <= q[2], x, NA_real_) }


## ===========================================================================
## 2. DEFINITIONS
## ===========================================================================
items <- tibble::tribble(
  ~item,    ~label,               ~unit,    ~codes,   ~codes_y5,
  "goat",   "Goat meat",          "kg",     "801",    "801",
  "beef",   "Beef",               "kg",     "802",    "802",
  "pork",   "Pork",               "kg",     "803",    "803",
  "chicken","Chicken & poultry",  "kg",     "804",    "8041,8042",
  "eggs",   "Eggs",               "piece",  "807",    "807",
  "milk",   "Fresh milk",         "litre",  "901",    "901"
)
item_order <- items$label
wave_cfg <- list(
  Y3 = list(prefix="hh_j",  id="y3_hhid", weight="y3_weight",      welfare="expmR"),
  Y4 = list(prefix="hh_j",  id="y4_hhid", weight="hhweight",       welfare="expmR"),
  Y5 = list(prefix="hh_ja", id="y5_hhid", weight="y5_crossweight", welfare="expmR_pae")
)


## ===========================================================================
## 3. BUILD POOLED DATA  (one row per household × item)
## ===========================================================================
rows <- list()
for (w in names(paths)) {
  cfg <- wave_cfg[[w]]; df <- read_wave(paths[[w]])
  adulteq <- pick(df, "adulteq", "Adult equivalent")
  welfare <- if (cfg$welfare == "expmR_pae") pick(df, "expmR_pae", "per adult equivalent")
             else pick(df, "expmR", "Real total monthly expenditure (TSH, deflated)") / adulteq
  urb <- pick(df, "urban", "Urban/Rural (1=Rural, 2=Urban)")
  liv <- pick(df, "lf02_any_livestock", "owns at least one livestock")
  hh <- tibble(wave = w, hhid = pick_id(df, cfg$id),
               hhsize = pick(df, "hh_hhsize", "Household size (total members"),
               adulteq = adulteq, weight = pick(df, cfg$weight), welfare = welfare,
               urban = as.integer(urb == 2),
               livestock = ifelse(is.na(liv), 0L, as.integer(liv == 1)))
  for (i in seq_len(nrow(items))) {
    it <- items[i, ]; codes <- strsplit(if (w=="Y5") it$codes_y5 else it$codes, ",")[[1]]
    qty <- pur <- own <- gift <- val <- 0
    for (cd in codes) { tag <- paste0("itemcode=", cd, "]")
      qty  <- qty  + z0(to_std(pick(df,cfg$prefix,tag,"in total did your household consume","QUANTITY"),
                               pick(df,cfg$prefix,tag,"in total did your household consume","UNIT"), it$unit))
      pur  <- pur  + z0(to_std(pick(df,cfg$prefix,tag,"came from purchases","QUANTITY"),
                               pick(df,cfg$prefix,tag,"came from purchases","UNIT"), it$unit))
      own  <- own  + z0(to_std(pick(df,cfg$prefix,tag,"came from own production","QUANTITY"),
                               pick(df,cfg$prefix,tag,"came from own production","UNIT"), it$unit))
      gift <- gift + z0(to_std(pick(df,cfg$prefix,tag,"came from gifts","QUANTITY"),
                               pick(df,cfg$prefix,tag,"came from gifts","UNIT"), it$unit))
      val  <- val  + z0(pick(df,cfg$prefix,tag,"How much did you spend"))
    }
    rows[[paste(w, it$item)]] <- hh %>% mutate(item=it$item, label=it$label, unit=it$unit,
      qty_total=qty, qty_pur=pur, qty_own=own, qty_gift=gift, exp=val)
  }
}
dat <- bind_rows(rows) %>% mutate(label = factor(label, levels = item_order))

# trim outliers, unit value, price (own production valued at the median price),
# value of consumption, per-AE measures, welfare quartile (within wave)
dat <- dat %>% group_by(item, wave) %>%
  mutate(across(c(qty_total, qty_pur, qty_own, qty_gift, exp), trim),
         unit_value = ifelse(qty_pur > 0 & exp > 0, exp/qty_pur, NA_real_),
         price = ifelse(is.finite(unit_value), unit_value, w_median(unit_value, weight)),
         # value = quantity x price; trim it too (the product of two trimmed
         # variables can still have extreme outliers — this caused the absurd
         # beef Q3 spike) so its mean is well behaved.
         value = trim(qty_total * price)) %>%
  ungroup() %>%
  mutate(qty_pae = qty_total/adulteq, value_pae = value/adulteq,
         purch_share = ifelse(qty_total > 0, qty_pur/qty_total, NA_real_))

add_q <- function(d) d %>% group_by(wave) %>%
  mutate(q = cut(welfare, c(-Inf, w_quantile(welfare, weight, c(.25,.5,.75)), Inf),
                 labels = c("Q1","Q2","Q3","Q4"), include.lowest = TRUE)) %>% ungroup()
dat <- add_q(dat)

# household ASF budget share of each item (value_i / total animal-food value)
dat <- dat %>% group_by(wave, hhid) %>%
  mutate(asf_value = sum(value, na.rm = TRUE),
         budget_share = ifelse(asf_value > 0, value/asf_value, NA_real_)) %>% ungroup()


## ===========================================================================
## 4. DESCRIPTIVE TABLES  (outline points 1-5)
## ===========================================================================
hh1 <- distinct(dat, wave, hhid, .keep_all = TRUE)

## (1) Sample descriptive statistics: full sample + urban/rural + livestock
samp <- function(d, name) d %>% summarise(Group = name, Households = n(),
  `HH size` = w_mean(hhsize, weight), `Adult eq.` = w_mean(adulteq, weight),
  `Rural %` = 100*w_mean(1-urban, weight), `Livestock %` = 100*w_mean(livestock, weight),
  `Welfare per AE` = w_mean(welfare, weight))
tab1_sample <- bind_rows(
  samp(hh1, "Full sample"),
  samp(filter(hh1, urban == 0), "Rural"),
  samp(filter(hh1, urban == 1), "Urban"),
  samp(filter(hh1, livestock == 1), "Owns livestock"),
  samp(filter(hh1, livestock == 0), "No livestock"))

## (2) Product-level descriptive statistics
istats <- function(d) d %>% summarise(
  `% consuming`     = 100*w_mean(as.numeric(qty_total>0), weight),
  `Quantity/AE`     = w_mean(ifelse(qty_total>0, qty_pae, NA), weight),
  `Value/AE (TSH)`  = w_mean(ifelse(qty_total>0, value_pae, NA), weight),
  `Unit value (TSH)`= w_median(unit_value, weight),
  `Purchased %`     = 100*w_mean(purch_share, weight),
  N_cons = sum(qty_total>0, na.rm=TRUE), N_buy = sum(is.finite(unit_value)), .groups="drop")
tab2_products <- dat %>% group_by(Item = label, Unit = unit) %>% istats()

## per item × quartile (used for ratios and several figures)
byq <- dat %>% filter(!is.na(q)) %>% group_by(Item = label, Unit = unit, Q = q) %>% istats()

## (3) Consumption trends across survey waves (quantity per AE)
cons_wave <- dat %>% group_by(Item = label, Wave = wave) %>%
  summarise(`Quantity/AE` = w_mean(ifelse(qty_total>0, qty_pae, NA), weight),
            `Value/AE` = w_mean(ifelse(qty_total>0, value_pae, NA), weight), .groups="drop")
tab3_wave <- cons_wave %>% select(Item, Wave, `Quantity/AE`) %>%
  pivot_wider(names_from = Wave, values_from = `Quantity/AE`)

## (4) Urban-rural consumption patterns
tab4_urban_rural <- dat %>% mutate(Area = ifelse(urban==1,"Urban","Rural")) %>%
  group_by(Item = label, Area) %>% istats()

## (5) Sources of consumption (shares of total quantity).
## NOTE: the survey records only purchases, own production and gifts/transfers;
## there is no separate "other" source, so it is omitted.
tab5_sources <- dat %>% filter(qty_total>0) %>% group_by(Item = label) %>% summarise(
  `Purchased %`      = 100*w_mean(qty_pur, weight)/w_mean(qty_pur+qty_own+qty_gift, weight),
  `Own production %` = 100*w_mean(qty_own, weight)/w_mean(qty_pur+qty_own+qty_gift, weight),
  `Gifts/transfers %`= 100*w_mean(qty_gift, weight)/w_mean(qty_pur+qty_own+qty_gift, weight),
  .groups="drop")

## (9) Inequality ratios Q4/Q1 (quantity, expenditure, unit value)
tab9_ratio <- byq %>% filter(Q %in% c("Q1","Q4")) %>%
  select(Item, Q, q=`Quantity/AE`, v=`Value/AE (TSH)`, u=`Unit value (TSH)`, nc=N_cons, nb=N_buy) %>%
  pivot_wider(names_from = Q, values_from = c(q,v,u,nc,nb)) %>%
  transmute(Item,
    `Quantity ratio (Q4/Q1)`   = ifelse(pmin(nc_Q1,nc_Q4)>=30, q_Q4/q_Q1, NA),
    `Expenditure ratio (Q4/Q1)`= ifelse(pmin(nc_Q1,nc_Q4)>=30, v_Q4/v_Q1, NA),
    `Unit value ratio (Q4/Q1)` = ifelse(pmin(nb_Q1,nb_Q4)>=30, u_Q4/u_Q1, NA))


## ===========================================================================
## 5. ELASTICITIES & QUALITY EFFECT  (outline points 10-13)
## ===========================================================================
# Deaton unit-value method. value = quantity * unit value, so the expenditure
# elasticity = quantity elasticity + quality elasticity. The quality elasticity
# is the WEDGE (expenditure minus quantity). Purchased quantity/value are used so
# the identity holds. Controls: log welfare per AE, log adult eq, urban, wave.
reg <- dat %>% filter(qty_pur>0, exp>0, is.finite(unit_value), welfare>0, adulteq>0) %>%
  mutate(lx = log(welfare), lae = log(adulteq), wave = factor(wave))

coef_lx <- function(form, d) { m <- lm(as.formula(form), d)
  coeftest(m, vcov = vcovHC(m, type="HC1"))["lx", c("Estimate","Std. Error")] }

## (10-11) per item: quantity, expenditure and quality (wedge) elasticities
elast <- reg %>% group_by(Item = label) %>% group_modify(~{
  q <- coef_lx("log(qty_pur) ~ lx+lae+urban+wave", .x)
  e <- coef_lx("log(exp) ~ lx+lae+urban+wave", .x)
  v <- coef_lx("log(unit_value) ~ lx+lae+urban+wave", .x)
  tibble(`Quantity elast`=q[1], `Expenditure elast`=e[1], `Quality (wedge)`=v[1],
         Quality_se=v[2], t=v[1]/v[2], p_1sided=pt(v[1]/v[2], nrow(.x)-1, lower.tail=FALSE))
}) %>% ungroup()
qp <- coef_lx("log(qty_pur) ~ lx+lae+urban+wave+label", reg)
ep <- coef_lx("log(exp) ~ lx+lae+urban+wave+label", reg)
vp <- coef_lx("log(unit_value) ~ lx+lae+urban+wave+label", reg)
elast <- bind_rows(elast, tibble(Item="ALL ITEMS", `Quantity elast`=qp[1],
  `Expenditure elast`=ep[1], `Quality (wedge)`=vp[1], Quality_se=vp[2],
  t=vp[1]/vp[2], p_1sided=pt(vp[1]/vp[2], nrow(reg)-1, lower.tail=FALSE))) %>%
  mutate(`Exp > Qty` = `Expenditure elast` > `Quantity elast`, `Reject H0 (quality=0)` = p_1sided < 0.05)

## (12) heterogeneity: quality elasticity by subgroup (pooled across items)
qonly <- function(d, form) { v <- coef_lx(form, d)
  tibble(Quality=v[1], se=v[2], p_1sided=pt(v[1]/v[2], nrow(d)-1, lower.tail=FALSE)) }
elast_sub <- bind_rows(
  qonly(filter(reg, urban==0), "log(unit_value) ~ lx+lae+wave+label") %>% mutate(Group="Rural"),
  qonly(filter(reg, urban==1), "log(unit_value) ~ lx+lae+wave+label") %>% mutate(Group="Urban"),
  qonly(filter(reg, livestock==1), "log(unit_value) ~ lx+lae+urban+wave+label") %>% mutate(Group="Owns livestock"),
  qonly(filter(reg, livestock==0), "log(unit_value) ~ lx+lae+urban+wave+label") %>% mutate(Group="No livestock"),
  qonly(filter(reg, wave=="Y3"), "log(unit_value) ~ lx+lae+urban+label") %>% mutate(Group="Wave 3"),
  qonly(filter(reg, wave=="Y4"), "log(unit_value) ~ lx+lae+urban+label") %>% mutate(Group="Wave 4"),
  qonly(filter(reg, wave=="Y5"), "log(unit_value) ~ lx+lae+urban+label") %>% mutate(Group="Wave 5")
) %>% select(Group, Quality, se, p_1sided)

## (13) KEY QUESTION — does quality upgrading DIFFER by residence / livestock?
## Interact income with the group dummy; the interaction is the difference in the
## quality elasticity (two-sided p-value tests whether it differs).
het_test <- function(inter, nm) {
  m  <- lm(as.formula(paste0("log(unit_value) ~ lx + lx:", inter, " + lae + ", inter, " + wave + label")), reg)
  ct <- coeftest(m, vcov = vcovHC(m, type="HC1"))
  r  <- grep(paste0("lx:", inter), rownames(ct), value=TRUE)[1]
  tibble(Test = nm, Difference = ct[r,1], se = ct[r,2], p_value = ct[r,4])
}
tab13_het <- bind_rows(
  het_test("urban",     "Urban vs rural (quality elasticity difference)"),
  het_test("livestock", "Livestock owner vs non-owner (difference)"))


## ===========================================================================
## 6. WRITE ONE EXCEL FILE (sheets numbered as in the outline)
## ===========================================================================
rnd <- function(d,k=2) mutate(d, across(where(is.numeric), ~round(.x,k)))
write.xlsx(list(
  "1_Sample"             = rnd(tab1_sample, 1),
  "2_Products"           = rnd(tab2_products),
  "3_Consumption_by_wave"= rnd(tab3_wave),
  "4_Urban_rural"        = rnd(tab4_urban_rural),
  "5_Sources"            = rnd(tab5_sources, 1),
  "9_Inequality_ratios"  = rnd(tab9_ratio),
  "10_11_Elasticities"   = rnd(elast, 3),
  "12_Heterogeneity"     = rnd(elast_sub, 3),
  "13_Het_tests"         = rnd(tab13_het, 4)
), file = "results.xlsx", overwrite = TRUE)


## ===========================================================================
## 7. FIGURES  (numbered to match the outline; all items shown together)
## ===========================================================================
gg  <- function(name, p, w=9, h=5.5) ggsave(file.path("figures", name), p, width=w, height=h, dpi=150)
ord <- function(d) mutate(d, Item = factor(Item, levels = item_order))
idx <- function(d, col) d %>% group_by(Item) %>% arrange(Q) %>%
  mutate(Index = 100*.data[[col]]/first(na.omit(.data[[col]]))) %>% ungroup()

## (1) participation
p <- tab2_products %>% ord() %>%
  ggplot(aes(reorder(Item, `% consuming`), `% consuming`, fill=Item)) +
  geom_col(show.legend=FALSE) + scale_fill_manual(values=pal) + coord_flip() +
  labs(title="Participation: share of households consuming each item", x=NULL, y="% (past 7 days)")
gg("01_participation.png", p, 8, 5)

## (3) consumption trends across waves (quantity per AE, index Wave 3 = 100)
p <- cons_wave %>% group_by(Item) %>% arrange(Wave) %>%
  mutate(Index = 100*`Quantity/AE`/first(na.omit(`Quantity/AE`))) %>% ungroup() %>% ord() %>%
  ggplot(aes(Wave, Index, colour=Item, group=Item)) + geom_line(linewidth=1) + geom_point() +
  scale_colour_manual(values=pal) +
  labs(title="3. Consumption trends across survey waves",
       subtitle="Quantity per adult equivalent, index Wave 3 = 100", x="Survey wave",
       y="Quantity per AE (index)", colour=NULL)
gg("03_consumption_by_wave.png", p)

## (3b) expenditure across waves — to see how spending moves over time.
## NOTE: this is NOMINAL value per AE (prices not deflated), so the rise is
## largely inflation. CONSUMPTION (quantity, fig 03) is flat across waves.
## The household INCOME measure is deflated to a wave-specific base, so its
## LEVEL is ~10x lower in Wave 5 — that is why income quartiles are built within
## wave, and it does NOT mean consumption fell.
p <- cons_wave %>% group_by(Item) %>% arrange(Wave) %>%
  mutate(Index = 100*`Value/AE`/first(na.omit(`Value/AE`))) %>% ungroup() %>% ord() %>%
  ggplot(aes(Wave, Index, colour=Item, group=Item)) + geom_line(linewidth=1) + geom_point() +
  scale_colour_manual(values=pal) +
  labs(title="3b. Expenditure across survey waves",
       subtitle="Nominal value per AE, index Wave 3 = 100 (the rise is mostly inflation)",
       x="Survey wave", y="Expenditure per AE (index)", colour=NULL)
gg("03b_expenditure_by_wave.png", p)

## (4) urban vs rural consumption (value per AE, same TSH unit -> comparable)
p <- tab4_urban_rural %>% ord() %>%
  ggplot(aes(Item, `Value/AE (TSH)`, fill=Area)) + geom_col(position="dodge") +
  labs(title="4. Urban vs rural consumption", subtitle="Value of consumption per adult equivalent",
       x=NULL, y="Value per AE (TSH)", fill=NULL) + theme(axis.text.x=element_text(angle=20,hjust=1))
gg("04_urban_rural.png", p)

## (5) sources of consumption
p <- tab5_sources %>% pivot_longer(-Item, names_to="Source", values_to="Share") %>% ord() %>%
  mutate(Source=factor(Source, levels=c("Purchased %","Own production %","Gifts/transfers %"))) %>%
  ggplot(aes(Item, Share, fill=Source)) + geom_col() +
  labs(title="5. Sources of food consumption", x=NULL, y="% of consumed quantity", fill=NULL) +
  theme(axis.text.x=element_text(angle=20,hjust=1))
gg("05_sources.png", p)

## (6) consumption per AE by income quartile (pooled, all six items, index)
p <- byq %>% filter(N_cons>=30) %>% idx("Quantity/AE") %>% ord() %>%
  ggplot(aes(Q, Index, colour=Item, group=Item)) + geom_line(linewidth=1) + geom_point() +
  scale_colour_manual(values=pal) +
  labs(title="6. Consumption per adult equivalent by income quartile",
       subtitle="Pooled sample; index, poorest quartile = 100",
       x="Income quartile (poor → rich)", y="Quantity per AE (index)", colour=NULL)
gg("06_consumption_by_quartile.png", p)

## (7) expenditure per AE by income quartile (pooled, index)
p <- byq %>% filter(N_cons>=30) %>% idx("Value/AE (TSH)") %>% ord() %>%
  ggplot(aes(Q, Index, colour=Item, group=Item)) + geom_line(linewidth=1) + geom_point() +
  scale_colour_manual(values=pal) +
  labs(title="7. Expenditure per adult equivalent by income quartile",
       subtitle="Pooled sample; index, poorest quartile = 100 (rises more than quantity)",
       x="Income quartile (poor → rich)", y="Expenditure per AE (index)", colour=NULL)
gg("07_expenditure_by_quartile.png", p)

## (8) quantity, expenditure AND unit value, six panels (the quality wedge)
## Three indexed lines per product: expenditure = quantity + quality (unit value).
mk_index <- byq %>% filter(N_cons>=30) %>% group_by(Item) %>% arrange(Q) %>%
  mutate(Quantity     = 100*`Quantity/AE`/first(na.omit(`Quantity/AE`)),
         Expenditure  = 100*`Value/AE (TSH)`/first(na.omit(`Value/AE (TSH)`)),
         `Unit value` = 100*`Unit value (TSH)`/first(na.omit(`Unit value (TSH)`))) %>% ungroup()
p <- mk_index %>%
  select(Item, Q, Quantity, Expenditure, `Unit value`) %>%
  pivot_longer(c(Quantity, Expenditure, `Unit value`), names_to="Measure", values_to="Index") %>%
  mutate(Measure=factor(Measure, levels=c("Expenditure","Quantity","Unit value"))) %>% ord() %>%
  ggplot(aes(Q, Index, colour=Measure, group=Measure)) + geom_line(linewidth=1) + geom_point() +
  facet_wrap(~ Item, scales="free_y") +
  scale_colour_manual(values=c(Expenditure="#D55E00", Quantity="#0072B2", `Unit value`="#009E73")) +
  labs(title="8. Quantity, expenditure and unit value across income quartiles",
       subtitle="Index, poorest quartile = 100. Expenditure (red) = quantity (blue) + unit value/quality (green)",
       x="Income quartile (poor → rich)", y="Index (poorest quartile = 100)", colour=NULL)
gg("08_consumption_vs_expenditure_panels.png", p, 11, 6)

## (8b) FOCUS on fresh milk — quantity, expenditure and unit value
p <- mk_index %>% filter(Item=="Fresh milk") %>%
  select(Q, Quantity, Expenditure, `Unit value`) %>%
  pivot_longer(-Q, names_to="Measure", values_to="Index") %>%
  mutate(Measure=factor(Measure, levels=c("Expenditure","Quantity","Unit value"))) %>%
  ggplot(aes(Q, Index, colour=Measure, group=Measure)) + geom_line(linewidth=1.2) + geom_point(size=2.5) +
  scale_colour_manual(values=c(Expenditure="#D55E00", Quantity="#0072B2", `Unit value`="#009E73")) +
  labs(title="Focus — Fresh milk: quantity, expenditure and quality by income quartile",
       subtitle="Index, poorest quartile = 100. The expenditure–quantity gap is the unit value (quality)",
       x="Income quartile (poor → rich)", y="Index (poorest quartile = 100)", colour=NULL)
gg("08b_focus_fresh_milk.png", p, 8, 5)

## (9) inequality ratios Q4/Q1
p <- tab9_ratio %>% pivot_longer(-Item, names_to="Measure", values_to="Ratio") %>% ord() %>%
  mutate(Measure=factor(Measure, levels=c("Quantity ratio (Q4/Q1)","Expenditure ratio (Q4/Q1)","Unit value ratio (Q4/Q1)"))) %>%
  ggplot(aes(Item, Ratio, fill=Measure)) + geom_col(position="dodge") +
  geom_hline(yintercept=1, linetype="dashed") +
  labs(title="9. Inequality ratios across income quartiles (Q4/Q1)", x=NULL, y="Q4 / Q1 ratio", fill=NULL) +
  theme(axis.text.x=element_text(angle=20,hjust=1))
gg("09_inequality_ratios.png", p)

## (10) quantity vs expenditure elasticities
p <- elast %>% filter(Item!="ALL ITEMS") %>% select(Item, `Quantity elast`, `Expenditure elast`) %>%
  pivot_longer(-Item, names_to="Elasticity", values_to="Value") %>% ord() %>%
  ggplot(aes(Item, Value, fill=Elasticity)) + geom_col(position="dodge") +
  scale_fill_manual(values=c(`Quantity elast`="#0072B2", `Expenditure elast`="#D55E00")) +
  labs(title="10. Quantity vs expenditure elasticities",
       subtitle="Expenditure elasticity exceeds quantity elasticity for every product",
       x=NULL, y="Elasticity w.r.t. income per AE", fill=NULL) +
  theme(axis.text.x=element_text(angle=20,hjust=1))
gg("10_elasticities.png", p)

## (11) the quality effect = wedge (expenditure - quantity), with 95% CI
p <- elast %>% mutate(lo=`Quality (wedge)`-1.96*Quality_se, hi=`Quality (wedge)`+1.96*Quality_se,
                      Item=factor(Item, levels=Item[order(`Quality (wedge)`)])) %>%
  ggplot(aes(`Quality (wedge)`, Item)) + geom_vline(xintercept=0, linetype="dashed") +
  geom_errorbarh(aes(xmin=lo, xmax=hi), height=.25) + geom_point(size=3, colour="#D55E00") +
  labs(title="11. The quality effect (expenditure − quantity elasticity)",
       subtitle="The wedge is positive and significant for all products = quality upgrading",
       x="Quality elasticity (wedge)", y=NULL)
gg("11_quality_effect.png", p, 8, 5)

## (12) heterogeneity in quality upgrading by subgroup
p <- elast_sub %>% mutate(lo=Quality-1.96*se, hi=Quality+1.96*se,
       Group=factor(Group, levels=rev(c("Rural","Urban","Owns livestock","No livestock","Wave 3","Wave 4","Wave 5")))) %>%
  ggplot(aes(Quality, Group)) + geom_vline(xintercept=0, linetype="dashed") +
  geom_errorbarh(aes(xmin=lo, xmax=hi), height=.25) + geom_point(size=3, colour="#009E73") +
  labs(title="12. Heterogeneity in quality upgrading",
       subtitle="Quality elasticity by subgroup (pooled across items, 95% CI)",
       x="Quality elasticity", y=NULL)
gg("12_heterogeneity.png", p, 8, 5)

## (supporting) continuous quality gradient: ln(unit value) vs ln(income)
p <- ggplot(reg, aes(lx, log(unit_value), colour=label)) +
  geom_smooth(method="lm", se=FALSE, linewidth=1) + scale_colour_manual(values=pal) +
  labs(title="Quality gradient: unit value rises with income (all items)",
       subtitle="Fitted log-log lines; slope = quality elasticity",
       x="log(real expenditure per adult equivalent)", y="log(unit value)", colour=NULL)
gg("14_quality_gradient.png", p)


## ===========================================================================
## 8. FRESH MILK — focus figures (milk only)
## ===========================================================================
pct <- function(x) paste0(round(x*100), "%")
milk <- dat %>% filter(item == "milk")

# M1. participation by income quartile, rural vs urban
d <- milk %>% filter(!is.na(q)) %>% mutate(Area = ifelse(urban==1,"Urban","Rural")) %>%
  group_by(Q=q, Area) %>% summarise(p = 100*w_mean(as.numeric(qty_total>0), weight), .groups="drop")
p <- ggplot(d, aes(Q, p, fill=Area)) + geom_col(position="dodge") +
  labs(title="Fresh milk — participation by income quartile",
       x="Income quartile (poor → rich)", y="% of households consuming", fill=NULL)
gg("milk_01_participation.png", p, 8, 5)

# M2. consumption PER HOUSEHOLD vs PER ADULT EQUIVALENT, by quartile
d <- milk %>% filter(!is.na(q)) %>% group_by(Q=q) %>% summarise(
  `Per household` = w_mean(ifelse(qty_total>0, qty_total, NA), weight),
  `Per adult eq.` = w_mean(ifelse(qty_total>0, qty_pae, NA), weight), .groups="drop") %>%
  pivot_longer(-Q, names_to="Basis", values_to="Litres")
p <- ggplot(d, aes(Q, Litres, colour=Basis, group=Basis)) +
  geom_line(linewidth=1.1) + geom_point(size=2.5) +
  labs(title="Fresh milk: consumption per household vs per adult equivalent",
       subtitle="Litres in the past 7 days (among consumers)",
       x="Income quartile (poor → rich)", y="Litres (7 days)", colour=NULL)
gg("milk_02_perHH_vs_perAE.png", p, 8, 5)

# M3. milk share of the animal-food budget, by quartile
d <- milk %>% filter(!is.na(q), qty_total>0) %>% group_by(Q=q) %>%
  summarise(s = 100*w_mean(budget_share, weight), .groups="drop")
p <- ggplot(d, aes(Q, s)) + geom_col(fill="#56B4E9") +
  labs(title="Fresh milk: share of the animal-food budget",
       x="Income quartile (poor → rich)", y="% of animal-food value")
gg("milk_03_budget_share.png", p, 8, 5)

# M4. where milk comes from, by quartile (shares: purchased / own / gifts)
d <- milk %>% filter(!is.na(q), qty_total>0) %>% group_by(Q=q) %>% summarise(
  Purchased = w_mean(qty_pur, weight), `Own production` = w_mean(qty_own, weight),
  Gifts = w_mean(qty_gift, weight), .groups="drop") %>%
  pivot_longer(-Q, names_to="Source", values_to="v") %>%
  mutate(Source = factor(Source, levels=c("Purchased","Own production","Gifts")))
p <- ggplot(d, aes(Q, v, fill=Source)) + geom_col(position="fill") +
  scale_y_continuous(labels = pct) +
  labs(title="Fresh milk: where it comes from, by income",
       subtitle="Richer households buy more from the market (less own production)",
       x="Income quartile (poor → rich)", y="Share of milk quantity", fill=NULL)
gg("milk_04_sources_by_quartile.png", p, 8, 5)

# M5. rural vs urban: quantity per AE, expenditure per AE, unit value
d <- milk %>% mutate(Area = ifelse(urban==1,"Urban","Rural")) %>% group_by(Area) %>% summarise(
  `Quantity per AE (litre)`   = w_mean(ifelse(qty_total>0, qty_pae, NA), weight),
  `Expenditure per AE (TSH)`  = w_mean(ifelse(qty_total>0, value_pae, NA), weight),
  `Unit value (TSH/litre)`    = w_median(unit_value, weight), .groups="drop") %>%
  pivot_longer(-Area, names_to="Measure", values_to="v")
p <- ggplot(d, aes(Area, v, fill=Area)) + geom_col(show.legend=FALSE) +
  facet_wrap(~ Measure, scales="free_y") +
  labs(title="Fresh milk: rural vs urban", x=NULL, y=NULL)
gg("milk_05_rural_urban.png", p, 9, 4)

# M6. milk by livestock ownership (quantity per AE, by source)
d <- milk %>% filter(qty_total>0) %>% mutate(L = ifelse(livestock==1,"Owns livestock","No livestock")) %>%
  group_by(L) %>% summarise(
    Purchased = w_mean(qty_pur/adulteq, weight), `Own production` = w_mean(qty_own/adulteq, weight),
    Gifts = w_mean(qty_gift/adulteq, weight), .groups="drop") %>%
  pivot_longer(-L, names_to="Source", values_to="v") %>%
  mutate(Source = factor(Source, levels=c("Purchased","Own production","Gifts")))
p <- ggplot(d, aes(L, v, fill=Source)) + geom_col() +
  labs(title="Fresh milk by livestock ownership",
       subtitle="Owners self-provision (own production); others rely on the market",
       x=NULL, y="Litres per adult equivalent (7 days)", fill=NULL)
gg("milk_06_livestock.png", p, 8, 5)

# M7. unit value distribution by quartile (quality spread)
d <- milk %>% filter(!is.na(q), is.finite(unit_value))
p <- ggplot(d, aes(q, unit_value)) + geom_boxplot(fill="#56B4E9", outlier.size=0.4) +
  coord_cartesian(ylim = c(0, quantile(d$unit_value, 0.95, na.rm=TRUE))) +
  labs(title="Fresh milk: unit value distribution by income quartile",
       subtitle="The price paid per litre shifts up with income (quality upgrading)",
       x="Income quartile (poor → rich)", y="Unit value (TSH/litre)")
gg("milk_07_unitvalue_box.png", p, 8, 5)

# M8. unit value vs income (household scatter + fit)
d <- reg %>% filter(label == "Fresh milk")
p <- ggplot(d, aes(lx, unit_value)) + geom_point(alpha=0.12, colour="#56B4E9") +
  geom_smooth(method="lm", colour="#D55E00", se=TRUE) +
  coord_cartesian(ylim = c(0, quantile(d$unit_value, 0.97, na.rm=TRUE))) +
  labs(title="Fresh milk: unit value rises with income",
       subtitle="Each point is a household; the line is the fitted quality gradient",
       x="log(real expenditure per adult equivalent)", y="Unit value (TSH/litre)")
gg("milk_08_unitvalue_vs_income.png", p, 8, 5)

cat("Done: results.xlsx (9 sheets) and figures in /figures\n")
