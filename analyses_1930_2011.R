setwd("~/Google Drive/Academic/Projects/blend_or_resist")
options(stringsAsFactors = FALSE)
library(stargazer)
library(ggplot2)
library(dplyr)
library(tidyr)

## PREPARATION

# Load data
data = read.csv("data/data_1930_2011.csv")

data = subset(data, p1930l != "-Inf" & p2011l != "-Inf")

# Transform variables
data$p1930l = log(data$pop1930)
data$p2011l = log(data$pop2011)
data$lnrepr = log(((data$vict_nac / data$pop1930) * 1000) + 1)
data$repbin = ifelse(data$vict_nac > 1, 1, 0)
data$lnrepr_left = log(((data$vict_rep / data$pop1930) * 1000) + 1)



## ANALYSES

## Wartime violence and VOX support

m_VOXa1 = lm(VOX201904 ~ lnrepr + p1930l + p2011l +
  part201904 + unemp_2016 + izq1936 + dcha1936 + factor(ccaa), data = data)
m_VOXa2 = lm(VOX201904 ~ lnrepr_left + p1930l +
  part201904 + unemp_2016 + izq1936 + dcha1936 + factor(ccaa), data = data)
m_VOXa3 = lm(VOX201904 ~ lnrepr + lnrepr_left + p1930l +
  part201904 + unemp_2016 + izq1936 + dcha1936 + factor(ccaa), data = data)

m_VOXb1 = lm(VOX201911 ~ lnrepr + p1930l + p2011l +
  part201904 + unemp_2016 + izq1936 + dcha1936 + factor(ccaa), data = data)
m_VOXb2 = lm(VOX201911 ~ lnrepr_left + p1930l +
  part201904 + unemp_2016 + izq1936 + dcha1936 + factor(ccaa), data = data)
m_VOXb3 = lm(VOX201911 ~ lnrepr + lnrepr_left + p1930l +
  part201904 + unemp_2016 + izq1936 + dcha1936 + factor(ccaa), data = data)


stargazer(m_VOXa1, m_VOXa2, m_VOXa3,
  title = "Wartime Francoist violence and VOX vote in 2019",
  label = "tab:main_hist",
  omit = "ccaa",
  omit.stat = c("f", "ser"), intercept.bottom = F,
  covariate.labels = c("(Intercept)",
  "Log. Francoist wartime killings",
  "Log. Population 1930",
  "Log. Population 2011",
  "Turnout April 2019",
  "Unemployment 2016",
  "Leftist support 1936",
  "Rightist support 1936"),
  multicolumn = FALSE,
  column.sep.width = "-20pt",
  dep.var.caption = "",
  dep.var.labels = c("VOX Apr-2019", "VOX Change Apr-Nov 19"),
  font.size = "small",
  digits = 3, digits.extra = 0,
  star.char = c("+", "*", "**", "***"),
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  notes.align = "c", align = T, no.space = TRUE,
  notes.label = "", notes.append = F,
  add.lines=list(c("CCAA Fixed Effects", rep("\\multicolumn{1}{c}{Yes}", 4))),
  notes = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$.}")

















# m_a2_left = lm(VOX_a ~ lnrepr_left + p1930l + p2011l +
#   turnout_a + renta_media_h16 + factor(ccaa), data = data)
# m_a2_left2 = lm(VOX_a ~ lnrepr_left + lnrepr + p1930l + p2011l +
#   turnout_a + renta_media_h16 + factor(ccaa), data = data)
# stargazer(m_a2, m_a2_left, m_a2_left2, type = "text")

m_n1 = lm(VOX_2019_11 ~ lnrepr + p1930l + p2011l +
  turnout_2019_11 + factor(ccaa), data = data)
m_n2 = lm(VOX_2019_11 ~ lnrepr + p1930l + p2011l +
  turnout_2019_11 + renta_media_h16 + factor(ccaa), data = data)
m_n3 = lm(VOX_2019_11 ~ lnrepr + p1930l + p2011l +
  turnout_2019_11 + renta_media_h16 + izq1936 + dcha1936 + factor(ccaa), data = data)

stargazer(m_a1, m_a2, m_a3, m_n1, m_n2, m_n3,
  type = "text", omit = c("ccaa", "prov_name"),
  intercept.bottom = FALSE,
  omit.stat = c("f","ser"))

m_ch1 = lm(VOX_chg ~ lnrepr + p1930l + p2011l +
  turnout_2019_11 + factor(ccaa), data = data)
m_ch2 = lm(VOX_chg ~ lnrepr + p1930l + p2011l +
  turnout_2019_11 + renta_media_h16 + factor(ccaa), data = data)
m_ch3 = lm(VOX_chg ~ lnrepr + p1930l + p2011l +
  turnout_2019_11 + renta_media_h16 + izq1936 + dcha1936 + factor(ccaa), data = data)

stargazer(m_a1, m_n1, m_ch1, m_ch2, m_ch3,
  type = "text", omit = c("ccaa", "prov_name"),
  intercept.bottom = FALSE,
  omit.stat = c("f","ser"))

## Plotting predicted effects
# Newdata
nd = data.frame(lnrepr = seq(0, 4, 1),
  p1930l = mean(data$p1930l, na.rm=T),
  p2011l = mean(data$p2011l, na.rm=T),
  turnout_a = mean(data$turnout_a, na.rm=T),
  turnout_b = mean(data$turnout_b, na.rm=T),
  renta_media_h16 = mean(data$renta_media_h16, na.rm=T),
  ccaa = "catalunya")
# Estimates
pp = nd[, "lnrepr", drop = FALSE]
pp$y_a = predict(m_a2, newdata = nd)
pp$y_b = predict(m_n2, newdata = nd)
pp$y_ch = predict(m_ch2, newdata = nd)
pp$se_a = predict(m_a2, newdata = nd, se.fit = TRUE)$se.fit
pp$se_b = predict(m_n2, newdata = nd, se.fit = TRUE)$se.fit
pp$se_ch = predict(m_ch2, newdata = nd, se.fit = TRUE)$se.fit
pp$upr_a = pp$y_a + 1.96 * pp$se_a
pp$upr_b = pp$y_b + 1.96 * pp$se_b
pp$upr_ch = pp$y_ch + 1.96 * pp$se_ch
pp$lwr_a = pp$y_a - 1.96 * pp$se_a
pp$lwr_b = pp$y_b - 1.96 * pp$se_b
pp$lwr_ch = pp$y_ch - 1.96 * pp$se_ch
# To long format
pp = pp %>%
  select(starts_with(c("lnrepr", "y_", "lwr_", "upr_"))) %>%
  pivot_longer(-lnrepr) %>%
  separate(name, into = c("stat", "model"), sep = "_") %>%
  pivot_wider(names_from = "stat", values_from = "value") %>%
  mutate(model = recode(model,
    'a' = "April", 'b' = "November", 'ch' = "Change")) %>%
  as.data.frame()
# Plotting
pdf("plots/VOX_wartime_vio.pdf", height = 4, width = 6)
ggplot(subset(pp, model != "Change"),
  aes(x = lnrepr, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  facet_wrap(~model, ncol = 2) +
  labs(x = "Log. Wartime killings / 1000hab",
    y = "VOX vote share") +
  theme_bw() + theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12),
    strip.background = element_blank())
dev.off()
pdf("plots/VOX_wartime_vio_change.pdf", height = 4, width = 4)
ggplot(subset(pp, model == "Change"),
  aes(x = lnrepr, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  labs(x = "Log. Wartime killings / 1000hab",
    y = "Change in VOX vote share, Apr-Nov") +
  # scale_y_continuous(limits = c(1.5,2)) +
  theme_bw() + theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12),
    strip.background = element_blank())
dev.off()

## Wartime violence and VOX support, % of total right

m_a1_dcha = lm(VOX_dcha_a ~ lnrepr + p1930l + p2011l +
  turnout_a + factor(ccaa), data = data)
m_a2_dcha = lm(VOX_dcha_a ~ lnrepr + p1930l + p2011l +
  turnout_a + renta_media_h16 + factor(ccaa), data = data)
m_a3_dcha = lm(VOX_dcha_a ~ lnrepr + p1930l + p2011l +
  turnout_a + renta_media_h16 + izq1936 + dcha1936 + factor(ccaa), data = data)

m_n1_dcha = lm(VOX_dcha_b ~ lnrepr + p1930l + p2011l +
  turnout_b + factor(ccaa), data = data)
m_n2_dcha = lm(VOX_dcha_b ~ lnrepr + p1930l + p2011l +
  turnout_b + renta_media_h16 + factor(ccaa), data = data)
m_n3_dcha = lm(VOX_dcha_b ~ lnrepr + p1930l + p2011l +
  turnout_b + renta_media_h16 + izq1936 + dcha1936 + factor(ccaa), data = data)

m_ch1_dcha = lm(VOX_dcha_chg ~ lnrepr + p1930l + p2011l +
  turnout_a + turnout_b + factor(ccaa), data = data)
m_ch2_dcha = lm(VOX_dcha_chg ~ lnrepr + p1930l + p2011l +
  turnout_a + turnout_b + renta_media_h16 + factor(ccaa), data = data)
m_ch3_dcha = lm(VOX_dcha_chg ~ lnrepr + p1930l + p2011l +
  turnout_a + turnout_b + renta_media_h16 + izq1936 + dcha1936 + factor(ccaa), data = data)

## Plotting predicted effects
# Estimates
pp_dcha = nd[, "lnrepr", drop = FALSE]
pp_dcha$y_a = predict(m_a2_dcha, newdata = nd)
pp_dcha$y_b = predict(m_n2_dcha, newdata = nd)
pp_dcha$y_ch = predict(m_ch2_dcha, newdata = nd)
pp_dcha$se_a = predict(m_a2_dcha, newdata = nd, se.fit = TRUE)$se.fit
pp_dcha$se_b = predict(m_n2_dcha, newdata = nd, se.fit = TRUE)$se.fit
pp_dcha$se_ch = predict(m_ch2_dcha, newdata = nd, se.fit = TRUE)$se.fit
pp_dcha$upr_a = pp_dcha$y_a + 1.96 * pp_dcha$se_a
pp_dcha$upr_b = pp_dcha$y_b + 1.96 * pp_dcha$se_b
pp_dcha$upr_ch = pp_dcha$y_ch + 1.96 * pp_dcha$se_ch
pp_dcha$lwr_a = pp_dcha$y_a - 1.96 * pp_dcha$se_a
pp_dcha$lwr_b = pp_dcha$y_b - 1.96 * pp_dcha$se_b
pp_dcha$lwr_ch = pp_dcha$y_ch - 1.96 * pp_dcha$se_ch
# To long format
pp_dcha = pp_dcha %>%
  select(starts_with(c("lnrepr", "y_", "lwr_", "upr_"))) %>%
  pivot_longer(-lnrepr) %>%
  separate(name, into = c("stat", "model"), sep = "_") %>%
  pivot_wider(names_from = "stat", values_from = "value") %>%
  mutate(model = recode(model,
    'a' = "April", 'b' = "November", 'ch' = "Change")) %>%
  as.data.frame()
# Plotting
pdf("plots/VOX_dcha_wartime_vio.pdf", height = 4, width = 6)
ggplot(subset(pp_dcha, model != "Change"),
  aes(x = lnrepr, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  facet_wrap(~model, ncol = 2) +
  labs(x = "Log. Wartime killings / 1000hab",
    y = "VOX vote share / total Right") +
  theme_bw() + theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12),
    strip.background = element_blank())
dev.off()
pdf("plots/VOX_dcha_wartime_vio_change.pdf", height = 4, width = 4)
ggplot(subset(pp_dcha, model == "Change"),
  aes(x = lnrepr, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  labs(x = "Log. Wartime killings / 1000hab",
    y = "Change in VOX vote share / total Right, Apr-Nov") +
  # scale_y_continuous(limits = c(1.5,2)) +
  theme_bw() + theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12),
    strip.background = element_blank())
dev.off()

## Removal of Francoist street names

m_str_a1 = lm(VOX_2019_04 ~ fs_changes_2001_2019_bin +lnrepr + p1930l + p2011l +
  turnout_2019_04 + factor(ccaa), data = data)
m_str_a2 = lm(VOX_2019_04 ~ fs_changes_2001_2019_bin + lnrepr + p1930l + p2011l +
  turnout_2019_04 + renta_media_h16 + factor(ccaa), data = data)
m_str_a3 = lm(VOX_2019_04 ~ fs_changes_2001_2019_bin + lnrepr + p1930l + p2011l +
  turnout_2019_04 + renta_media_h16 + izq1936 + dcha1936 + factor(ccaa), data = data)

m_str_n1 = lm(VOX_2019_11 ~ fs_changes_2001_2019_bin + lnrepr + p1930l + p2011l +
  turnout_2019_11 + factor(ccaa), data = data)
m_str_n2 = lm(VOX_2019_11 ~ fs_changes_2001_2019_bin + lnrepr + p1930l + p2011l +
  turnout_2019_11 + renta_media_h16 + factor(ccaa), data = data)
m_str_n3 = lm(VOX_2019_11 ~ fs_changes_2001_2019_bin + lnrepr + p1930l + p2011l +
  turnout_2019_11 + renta_media_h16 + izq1936 + dcha1936 + factor(ccaa), data = data)

m_str_ch1 = lm(VOX_chg ~ fs_changes_2001_2019_bin + lnrepr + p1930l + p2011l +
  turnout_2019_11 + factor(ccaa), data = data)
m_str_ch2 = lm(VOX_chg ~ fs_changes_2001_2019_bin + lnrepr + p1930l + p2011l +
  turnout_2019_11 + renta_media_h16 + factor(ccaa), data = data)
m_str_ch3 = lm(VOX_chg ~ fs_changes_2001_2019_bin + lnrepr + p1930l + p2011l +
  turnout_2019_11 + renta_media_h16 + izq1936 + dcha1936 + factor(ccaa), data = data)

stargazer(m_str_a1, m_str_a2, m_str_a3,
  type = "text", omit = c("ccaa", "prov_name"),
  intercept.bottom = FALSE,
  omit.stat = c("f","ser"))

stargazer(m_str_n1, m_str_n2, m_str_n3,
  type = "text", omit = c("ccaa", "prov_name"),
  intercept.bottom = FALSE,
  omit.stat = c("f","ser"))

stargazer(m_str_ch1, m_str_ch1, m_str_ch1,
  type = "text", omit = c("ccaa", "prov_name"),
  intercept.bottom = FALSE,
  omit.stat = c("f","ser"))


## Plotting predicted effects
# Newdata
nd_str = data.frame(fra_str_drop01_19 = 0:1,
  lnrepr = mean(data$lnrepr, na.rm=T),
  p1930l = mean(data$p1930l, na.rm=T),
  p2011l = mean(data$p2011l, na.rm=T),
  turnout_a = mean(data$turnout_a, na.rm=T),
  turnout_b = mean(data$turnout_b, na.rm=T),
  renta_media_h16 = mean(data$renta_media_h16, na.rm=T),
  ccaa = "catalunya")
# Estimates
pp_str = nd_str[, "fra_str_drop01_19", drop = FALSE]
pp_str$y_a = predict(m_str_a2, newdata = nd_str)
pp_str$y_b = predict(m_str_n2, newdata = nd_str)
pp_str$y_ch = predict(m_str_ch2, newdata = nd_str)
pp_str$se_a = predict(m_str_a2, newdata = nd_str, se.fit = TRUE)$se.fit
pp_str$se_b = predict(m_str_n2, newdata = nd_str, se.fit = TRUE)$se.fit
pp_str$se_ch = predict(m_str_ch2, newdata = nd_str, se.fit = TRUE)$se.fit
pp_str$upr_a = pp_str$y_a + 1.96 * pp_str$se_a
pp_str$upr_b = pp_str$y_b + 1.96 * pp_str$se_b
pp_str$upr_ch = pp_str$y_ch + 1.96 * pp_str$se_ch
pp_str$lwr_a = pp_str$y_a - 1.96 * pp_str$se_a
pp_str$lwr_b = pp_str$y_b - 1.96 * pp_str$se_b
pp_str$lwr_ch = pp_str$y_ch - 1.96 * pp_str$se_ch
# To long format
pp_str = pp_str %>%
  select(starts_with(c("fra", "y_", "lwr_", "upr_"))) %>%
  pivot_longer(-fra_str_drop01_19) %>%
  separate(name, into = c("stat", "model"), sep = "_") %>%
  pivot_wider(names_from = "stat", values_from = "value") %>%
  mutate(model = recode(model,
    'a' = "April", 'b' = "November", 'ch' = "Change")) %>%
  as.data.frame()
# Plotting
pdf("plots/VOX_franc_street.pdf", height = 4, width = 6)
ggplot(subset(pp_str, model != "Change"),
  aes(x = factor(fra_str_drop01_19), y = y)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.05) +
  facet_wrap(~model, ncol = 2) +
  labs(x = "Francoist street name removal 2001-2019",
    y = "VOX vote share") +
  theme_bw() + theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12),
    strip.background = element_blank())
dev.off()
pdf("plots/VOX_franc_street_change.pdf", height = 4, width = 4)
ggplot(subset(pp_str, model == "Change"),
  aes(x = factor(fra_str_drop01_19), y = y)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.05) +
  # scale_y_continuous(limits = c(1.5,2)) +
  labs(x = "Francoist street name removal 2001-2019",
    y = "Change in VOX vote share, Apr-Nov") +
  theme_bw() + theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12),
    strip.background = element_blank())
dev.off()

## Mass graves

m_f_a2 = lm(VOX_a ~ fosas_vict_l + lnrepr + p1930l + p2011l +
  turnout_a + renta_media_h16, data = data)
m_f_n2 = lm(VOX_b ~ fosas_vict_l + lnrepr + p1930l + p2011l +
  turnout_b + renta_media_h16, data = data)
m_f_ch2 = lm(VOX_chg ~ fosas_vict_l + lnrepr + p1930l + p2011l +
  turnout_a + turnout_b + renta_media_h16, data = data)

stargazer(m_f_a2, m_f_n2, m_f_ch2, type="text")

## Plotting predicted effects
# Newdata
nd_f = data.frame(fosas_vict_l = 0:6,
  lnrepr = mean(data$lnrepr, na.rm=T),
  p1930l = mean(data$p1930l, na.rm=T),
  p2011l = mean(data$p2011l, na.rm=T),
  turnout_a = mean(data$turnout_a, na.rm=T),
  turnout_b = mean(data$turnout_b, na.rm=T),
  renta_media_h16 = mean(data$renta_media_h16, na.rm=T))
# Estimates
pp_f = nd_f[, "fosas_vict_l", drop = FALSE]
pp_f$y_a = predict(m_f_a2, newdata = nd_f)
pp_f$y_b = predict(m_f_n2, newdata = nd_f)
pp_f$y_ch = predict(m_f_ch2, newdata = nd_f)
pp_f$se_a = predict(m_f_a2, newdata = nd_f, se.fit = TRUE)$se.fit
pp_f$se_b = predict(m_f_n2, newdata = nd_f, se.fit = TRUE)$se.fit
pp_f$se_ch = predict(m_f_ch2, newdata = nd_f, se.fit = TRUE)$se.fit
pp_f$upr_a = pp_f$y_a + 1.96 * pp_f$se_a
pp_f$upr_b = pp_f$y_b + 1.96 * pp_f$se_b
pp_f$upr_ch = pp_f$y_ch + 1.96 * pp_f$se_ch
pp_f$lwr_a = pp_f$y_a - 1.96 * pp_f$se_a
pp_f$lwr_b = pp_f$y_b - 1.96 * pp_f$se_b
pp_f$lwr_ch = pp_f$y_ch - 1.96 * pp_f$se_ch
# To long format
pp_f = pp_f %>%
  select(starts_with(c("fosas", "y_", "lwr_", "upr_"))) %>%
  pivot_longer(-fosas_vict_l) %>%
  separate(name, into = c("stat", "model"), sep = "_") %>%
  pivot_wider(names_from = "stat", values_from = "value") %>%
  mutate(model = recode(model,
    'a' = "April", 'b' = "November", 'ch' = "Change")) %>%
  as.data.frame()
# Plotting
pdf("plots/VOX_fosas.pdf", height = 4, width = 6)
ggplot(subset(pp_f, model != "Change"),
  aes(x = fosas_vict_l, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  facet_wrap(~model, ncol = 2) +
  labs(x = "Log. Victims in mass graves / 1000 hab.",
    y = "VOX vote share") +
  theme_bw() + theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12),
    strip.background = element_blank())
dev.off()
pdf("plots/VOX_fosas_change.pdf", height = 4, width = 4)
ggplot(subset(pp_f, model == "Change"),
  aes(x = fosas_vict_l, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  # scale_y_continuous(limits = c(1.5,2)) +
  labs(x = "Log. Victims in mass graves / 1000 hab.",
    y = "VOX vote share") +
  theme_bw() + theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12),
    strip.background = element_blank())
dev.off()
