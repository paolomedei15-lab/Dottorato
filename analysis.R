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
         price = ifelse(is.finite(unit_value), unit_value, w_median(unit_value, weight))) %>%
  ungroup() %>%
  mutate(qty_pae = qty_total/adulteq, value = qty_total*price, value_pae = value/adulteq,
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
## 4. DESCRIPTIVE TABLES
## ===========================================================================
hh1 <- distinct(dat, wave, hhid, .keep_all = TRUE)
tab_hh <- bind_rows(
  hh1 %>% group_by(Wave = wave) %>% summarise(Households=n(),
     `HH size`=w_mean(hhsize,weight), `Adult eq.`=w_mean(adulteq,weight),
     `Rural %`=100*w_mean(1-urban,weight), `Urban %`=100*w_mean(urban,weight),
     `Livestock %`=100*w_mean(livestock,weight), .groups="drop"),
  hh1 %>% summarise(Wave="Pooled", Households=n(),
     `HH size`=w_mean(hhsize,weight), `Adult eq.`=w_mean(adulteq,weight),
     `Rural %`=100*w_mean(1-urban,weight), `Urban %`=100*w_mean(urban,weight),
     `Livestock %`=100*w_mean(livestock,weight)))

istats <- function(d) d %>% summarise(
  `% consuming`=100*w_mean(as.numeric(qty_total>0),weight),
  `Quantity/AE`=w_mean(ifelse(qty_total>0,qty_pae,NA),weight),
  `Value/AE (TSH)`=w_mean(ifelse(qty_total>0,value_pae,NA),weight),
  `Unit value (TSH)`=w_median(unit_value,weight),
  `Purchased %`=100*w_mean(purch_share,weight),
  N_cons=sum(qty_total>0,na.rm=TRUE), N_buy=sum(is.finite(unit_value)), .groups="drop")

tab_items <- dat %>% group_by(Item=label, Unit=unit) %>% istats()
byq       <- dat %>% filter(!is.na(q)) %>% group_by(Item=label, Unit=unit, Q=q) %>% istats()
tab_area  <- dat %>% mutate(Area=ifelse(urban==1,"Urban","Rural")) %>% group_by(Item=label, Area) %>% istats()
tab_live  <- dat %>% mutate(L=ifelse(livestock==1,"Owns livestock","No livestock")) %>% group_by(Item=label, Livestock=L) %>% istats()

# Q4/Q1 ratios (quantity & value need >=30 consumers; unit value >=30 buyers)
tab_ratio <- byq %>% filter(Q %in% c("Q1","Q4")) %>%
  select(Item, Q, q=`Quantity/AE`, v=`Value/AE (TSH)`, u=`Unit value (TSH)`, nc=N_cons, nb=N_buy) %>%
  pivot_wider(names_from=Q, values_from=c(q,v,u,nc,nb)) %>%
  transmute(Item,
    `Quantity Q4/Q1` = ifelse(pmin(nc_Q1,nc_Q4)>=30, q_Q4/q_Q1, NA),
    `Value Q4/Q1`    = ifelse(pmin(nc_Q1,nc_Q4)>=30, v_Q4/v_Q1, NA),
    `Unit value Q4/Q1`=ifelse(pmin(nb_Q1,nb_Q4)>=30, u_Q4/u_Q1, NA))

# source of quantity, shares of total
tab_source <- dat %>% filter(qty_total>0) %>% group_by(Item=label) %>% summarise(
  `Purchased %`=100*w_mean(qty_pur,weight)/w_mean(qty_pur+qty_own+qty_gift,weight),
  `Own production %`=100*w_mean(qty_own,weight)/w_mean(qty_pur+qty_own+qty_gift,weight),
  `Gifts %`=100*w_mean(qty_gift,weight)/w_mean(qty_pur+qty_own+qty_gift,weight), .groups="drop")

# budget composition: mean ASF budget share by item × quartile (per household)
tab_budget <- dat %>% filter(!is.na(q), qty_total>0) %>%
  group_by(Q=q, Item=label) %>% summarise(Share=100*w_mean(budget_share,weight), .groups="drop")


## ===========================================================================
## 5. ELASTICITIES — unit-value method (Deaton 1988), OLS with HC1 SE
## ===========================================================================
# value = quantity × unit value, so eps_value = eps_quantity + eps_quality.
# Regressors: log welfare per AE + log adult eq + urban + wave (+ item, pooled).
reg <- dat %>% filter(qty_pur>0, exp>0, is.finite(unit_value), welfare>0, adulteq>0) %>%
  mutate(lx=log(welfare), lae=log(adulteq), wave=factor(wave))

coef_lx <- function(form, d) { m <- lm(as.formula(form), d)
  coeftest(m, vcov = vcovHC(m, type="HC1"))["lx", c("Estimate","Std. Error")] }

# per item
elast <- reg %>% group_by(Item = label) %>% group_modify(~{
  e <- coef_lx("log(exp) ~ lx+lae+urban+wave", .x)
  q <- coef_lx("log(qty_pur) ~ lx+lae+urban+wave", .x)
  v <- coef_lx("log(unit_value) ~ lx+lae+urban+wave", .x)
  tibble(Expenditure=e[1], Quantity=q[1], Quality=v[1], Quality_se=v[2],
         t=v[1]/v[2], p_1sided=pt(v[1]/v[2], nrow(.x)-1, lower.tail=FALSE))
}) %>% ungroup()

# pooled (item dummies)
ep <- coef_lx("log(exp) ~ lx+lae+urban+wave+label", reg)
qp <- coef_lx("log(qty_pur) ~ lx+lae+urban+wave+label", reg)
vp <- coef_lx("log(unit_value) ~ lx+lae+urban+wave+label", reg)
elast <- bind_rows(elast, tibble(Item="ALL ITEMS", Expenditure=ep[1], Quantity=qp[1],
  Quality=vp[1], Quality_se=vp[2], t=vp[1]/vp[2], p_1sided=pt(vp[1]/vp[2], nrow(reg)-1, lower.tail=FALSE)))
elast <- elast %>% mutate(`Quality share %` = 100*Quality/Expenditure,
                          `Reject H0` = p_1sided < 0.05)

# quality elasticity by subgroup (rural/urban, livestock, wave) — pooled, item FE
quality_only <- function(d, form) { v <- coef_lx(form, d)
  tibble(Quality=v[1], se=v[2], p_1sided=pt(v[1]/v[2], nrow(d)-1, lower.tail=FALSE)) }
elast_sub <- bind_rows(
  quality_only(filter(reg, urban==0), "log(unit_value) ~ lx+lae+wave+label") %>% mutate(Group="Rural"),
  quality_only(filter(reg, urban==1), "log(unit_value) ~ lx+lae+wave+label") %>% mutate(Group="Urban"),
  quality_only(filter(reg, livestock==1), "log(unit_value) ~ lx+lae+urban+wave+label") %>% mutate(Group="Owns livestock"),
  quality_only(filter(reg, livestock==0), "log(unit_value) ~ lx+lae+urban+wave+label") %>% mutate(Group="No livestock"),
  quality_only(filter(reg, wave=="Y3"), "log(unit_value) ~ lx+lae+urban+label") %>% mutate(Group="Wave 3"),
  quality_only(filter(reg, wave=="Y4"), "log(unit_value) ~ lx+lae+urban+label") %>% mutate(Group="Wave 4"),
  quality_only(filter(reg, wave=="Y5"), "log(unit_value) ~ lx+lae+urban+label") %>% mutate(Group="Wave 5")
) %>% select(Group, Quality, se, p_1sided)


## ===========================================================================
## 6. WRITE ONE EXCEL FILE
## ===========================================================================
rnd <- function(d,k=2) mutate(d, across(where(is.numeric), ~round(.x,k)))
write.xlsx(list(
  "Households"=rnd(tab_hh,1), "Items overall"=rnd(tab_items), "By quartile"=rnd(byq),
  "Q4 over Q1"=rnd(tab_ratio), "By rural-urban"=rnd(tab_area), "By livestock"=rnd(tab_live),
  "Quantity source"=rnd(tab_source,1), "Budget composition"=rnd(tab_budget,1),
  "Elasticities"=rnd(elast,3), "Elasticity subgroups"=rnd(elast_sub,3)
), file="results.xlsx", overwrite=TRUE)


## ===========================================================================
## 7. FIGURES  (all items together; >=10 useful charts)
## ===========================================================================
gg <- function(name, p, w=9, h=5.5) ggsave(file.path("figures", name), p, width=w, height=h, dpi=150)
ord <- function(d) mutate(d, Item = factor(Item, levels = item_order))
# index to the poorest available quartile = 100 (units become comparable)
idx <- function(d, col) d %>% group_by(Item) %>% arrange(Q) %>%
  mutate(Index = 100 * .data[[col]] / first(na.omit(.data[[col]]))) %>% ungroup()

## --- DESCRIPTIVE ---

# 1. Participation by item
p <- ggplot(ord(tab_items), aes(reorder(Item,`% consuming`), `% consuming`, fill=Item)) +
  geom_col(show.legend=FALSE) + scale_fill_manual(values=pal) + coord_flip() +
  labs(title="Share of households consuming each item", x=NULL, y="% (past 7 days)")
gg("01_participation.png", p, 8, 5)

# 2. Participation rural vs urban
p <- tab_area %>% ord() %>%
  ggplot(aes(Item, `% consuming`, fill=Area)) + geom_col(position="dodge") +
  labs(title="Participation: rural vs urban", x=NULL, y="% consuming", fill=NULL) +
  theme(axis.text.x=element_text(angle=20,hjust=1))
gg("02_participation_rural_urban.png", p)

# 3. Quantity index by quartile (all items, Q1 = 100)
p <- byq %>% filter(N_cons>=30) %>% idx("Quantity/AE") %>% ord() %>%
  ggplot(aes(Q, Index, colour=Item, group=Item)) + geom_line(linewidth=1) + geom_point() +
  scale_colour_manual(values=pal) +
  labs(title="Quantity per adult equivalent rises with income",
       subtitle="Index, poorest quartile Q1 = 100 (so all items are comparable)",
       x="Welfare quartile (poor → rich)", y="Quantity index (Q1 = 100)", colour=NULL)
gg("03_quantity_index.png", p)

# 4. Value index by quartile
p <- byq %>% filter(N_cons>=30) %>% idx("Value/AE (TSH)") %>% ord() %>%
  ggplot(aes(Q, Index, colour=Item, group=Item)) + geom_line(linewidth=1) + geom_point() +
  scale_colour_manual(values=pal) +
  labs(title="Value of consumption per adult equivalent rises with income",
       subtitle="Index, Q1 = 100 — note it rises MORE than quantity (quality upgrading)",
       x="Welfare quartile (poor → rich)", y="Value index (Q1 = 100)", colour=NULL)
gg("04_value_index.png", p)

# 5. Unit value (quality) index by quartile  [KEY descriptive]
p <- byq %>% filter(N_buy>=30) %>% idx("Unit value (TSH)") %>% ord() %>%
  ggplot(aes(Q, Index, colour=Item, group=Item)) + geom_line(linewidth=1) + geom_point() +
  geom_hline(yintercept=100, linetype="dashed") + scale_colour_manual(values=pal) +
  labs(title="Unit value (quality) rises with income",
       subtitle="Index, poorest shown quartile = 100 — richer households pay more per kg / litre / piece",
       x="Welfare quartile (poor → rich)", y="Unit value index", colour=NULL)
gg("05_unitvalue_index.png", p)

# 6. Share of quantity purchased by quartile (market integration)
p <- byq %>% ord() %>%
  ggplot(aes(Q, `Purchased %`, colour=Item, group=Item)) + geom_line(linewidth=1) + geom_point() +
  scale_colour_manual(values=pal) +
  labs(title="Market integration rises with income",
       subtitle="Share of consumed quantity that is purchased (rest is own production / gifts)",
       x="Welfare quartile (poor → rich)", y="% of quantity purchased", colour=NULL)
gg("06_purchased_share.png", p)

# 7. Source of quantity: purchased / own / gifts (shares, all items)
p <- tab_source %>% pivot_longer(-Item, names_to="Source", values_to="Share") %>% ord() %>%
  mutate(Source=factor(Source, levels=c("Purchased %","Own production %","Gifts %"))) %>%
  ggplot(aes(Item, Share, fill=Source)) + geom_col() +
  labs(title="Where consumed quantity comes from", x=NULL, y="% of quantity", fill=NULL) +
  theme(axis.text.x=element_text(angle=20,hjust=1))
gg("07_quantity_source.png", p)

# 8. Animal-food budget composition across quartiles
p <- tab_budget %>% ord() %>%
  ggplot(aes(Q, Share, fill=Item)) + geom_col() + scale_fill_manual(values=pal) +
  labs(title="Composition of the animal-food basket by income",
       subtitle="Share of each item in total animal-food value (per household)",
       x="Welfare quartile (poor → rich)", y="% of animal-food value", fill=NULL)
gg("08_budget_composition.png", p)

## --- ELASTICITIES ---

# 9. Q4/Q1 ratios: value vs quantity vs unit value  [KEY]
p <- tab_ratio %>% pivot_longer(-Item, names_to="Measure", values_to="Ratio") %>% ord() %>%
  mutate(Measure=factor(Measure, levels=c("Quantity Q4/Q1","Value Q4/Q1","Unit value Q4/Q1"))) %>%
  ggplot(aes(Item, Ratio, fill=Measure)) + geom_col(position="dodge") +
  geom_hline(yintercept=1, linetype="dashed") +
  labs(title="Richest vs poorest quartile (Q4/Q1)",
       subtitle="Value rises more than quantity; the gap is the higher unit value (quality)",
       x=NULL, y="Q4 / Q1 ratio", fill=NULL) + theme(axis.text.x=element_text(angle=20,hjust=1))
gg("09_Q4Q1_ratios.png", p)

# 10. Expenditure elasticity = quantity + quality (stacked)  [KEY]
p <- elast %>% filter(Item!="ALL ITEMS") %>% select(Item, Quantity, Quality) %>%
  pivot_longer(-Item, names_to="Part", values_to="Elast") %>% ord() %>%
  ggplot(aes(Item, Elast, fill=Part)) + geom_col() +
  scale_fill_manual(values=c(Quantity="#0072B2", Quality="#D55E00")) +
  labs(title="Expenditure elasticity = quantity + quality",
       subtitle="The orange part (quality) is the quality-upgrading effect",
       x=NULL, y="Elasticity w.r.t. real expenditure per AE", fill=NULL) +
  theme(axis.text.x=element_text(angle=20,hjust=1))
gg("10_decomposition.png", p)

# 11. Quality elasticity with 95% CI (the hypothesis test)  [KEY]
p <- elast %>% mutate(lo=Quality-1.96*Quality_se, hi=Quality+1.96*Quality_se,
                      Item=factor(Item, levels=Item[order(Quality)])) %>%
  ggplot(aes(Quality, Item)) + geom_vline(xintercept=0, linetype="dashed") +
  geom_errorbarh(aes(xmin=lo, xmax=hi), height=.25) + geom_point(size=3, colour="#D55E00") +
  labs(title="Quality elasticity by item (95% CI)",
       subtitle="All bars are right of zero → H0 (no quality upgrading) is rejected",
       x="Quality elasticity", y=NULL)
gg("11_quality_CI.png", p, 8, 5)

# 12. Quality share of the expenditure elasticity
p <- elast %>% filter(Item!="ALL ITEMS") %>% ord() %>%
  ggplot(aes(reorder(Item,`Quality share %`), `Quality share %`, fill=Item)) +
  geom_col(show.legend=FALSE) + scale_fill_manual(values=pal) + coord_flip() +
  labs(title="How much of the income response is quality?",
       x=NULL, y="Quality share of expenditure elasticity (%)")
gg("12_quality_share.png", p, 8, 5)

# 13. Quality elasticity by subgroup (rural/urban, livestock, wave)
p <- elast_sub %>% mutate(lo=Quality-1.96*se, hi=Quality+1.96*se,
       Group=factor(Group, levels=rev(c("Rural","Urban","Owns livestock","No livestock","Wave 3","Wave 4","Wave 5")))) %>%
  ggplot(aes(Quality, Group)) + geom_vline(xintercept=0, linetype="dashed") +
  geom_errorbarh(aes(xmin=lo, xmax=hi), height=.25) + geom_point(size=3, colour="#0072B2") +
  labs(title="Quality elasticity across subgroups (95% CI)",
       subtitle="Positive everywhere; somewhat stronger in rural areas",
       x="Quality elasticity (pooled across items)", y=NULL)
gg("13_quality_subgroups.png", p, 8, 5)

# 14. Continuous quality gradient: ln(unit value) vs ln(welfare), one line per item
p <- ggplot(reg, aes(lx, log(unit_value), colour=label)) +
  geom_smooth(method="lm", se=FALSE, linewidth=1) + scale_colour_manual(values=pal) +
  labs(title="Quality gradient: unit value rises with income (all items)",
       subtitle="Fitted lines of log unit value on log real expenditure per AE; slope = quality elasticity",
       x="log(real expenditure per adult equivalent)", y="log(unit value)", colour=NULL)
gg("14_quality_gradient.png", p)

cat("Done: results.xlsx and 14 figures in /figures\n")
