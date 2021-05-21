## Replication "Do TJ policies cause backlash? Evidence from street name changes in Spain"
## January 2021
## File: MAIN ANALYSES
## ---------------------------------------

if(!grepl("replication$", getwd())){
  print("Choose any file in the replication folder (e.g. analyses.R)")
  dir = file.choose()
  setwd(gsub("replication(/.*)$", "replication", dir))
}

options(stringsAsFactors = FALSE)
library(dplyr)
library(tidyr)
library(stargazer)
library(ggplot2)
library(stringr)
library(MASS)
source("extra/functions_did.R")

## PREPARE DATA ##

# Load
data = read.csv("data.csv")

# Log FS count variables
data = data %>% mutate(
  l_fs_2001_06 = log(fs_2001_06 + 1),
  l_fs_2016_06 = log(fs_2016_06 + 1),
  l_fs_2018_12 = log(fs_2018_12 + 1),
  l_fs_2019_06 = log(fs_2019_06 + 1),
  l_fs_rm_2016s2_2018s2 = log(fs_rm_2016s2_2018s2 + 1),
  change_2019_VOX = VOX201911 / VOX201904)
data$change_2019_VOX[data$change_2019_VOX == "Inf"] = NA

## CROSS-SECTIONAL ANALYSES ##

m_cs1 = lm(VOX201904 ~ fs_rm_2001s2_2018s2_bin +
  unemp_2019 + part201904 + lpop2011 + factor(ccaa), data = data)
m_cs2 = lm(VOX201911 ~ fs_rm_2001s2_2018s2_bin +
  unemp_2019 + part201911 + lpop2011 + factor(ccaa), data = data)

m_cs3 = lm(VOX201904 ~ fs_rm_2001s2_2018s2_bin +
  unemp_2019 + part201904 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2001_06 > 0))
m_cs4 = lm(VOX201911 ~ fs_rm_2001s2_2018s2_bin +
  unemp_2019 + part201911 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2001_06 > 0))

## Table 1 (main text)

stargazer(m_cs1, m_cs2, m_cs3, m_cs4,
  title = "Francoist street name removal and electoral support for Vox",
  label = "tab:cs",
  omit = "ccaa",
  omit.stat = c("f", "ser"), intercept.bottom = F,
  covariate.labels = c("(Intercept)",
  "Francoist street name removal",
  "Unemployment 2019",
  "Turnout April 2019",
  "Turnout Nov 2019",
  "Log. Population"),
  multicolumn = FALSE,
  column.sep.width = "-20pt",
  dep.var.caption = "",
  dep.var.labels = c("\\footnotesize Apr 2019", "\\footnotesize Nov 2019",
    "\\footnotesize Apr 2019", "\\footnotesize Nov 2019"),
  font.size = "small",
  digits = 3, digits.extra = 0,
  star.char = c("+", "*", "**", "***"),
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  notes.align = "c", align = T, no.space = TRUE,
  notes.label = "", notes.append = F,
  add.lines=list(c("CCAA Fixed Effects", rep("\\multicolumn{1}{c}{Yes}", 4))),
  notes = "\\parbox[t]{0.8\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. The main independent variable refers to the removal of Francoist street names between June 2001 and December 2018. Models 3 and 4 only include municipalities that had Francoist street names in June 2001.}")


## DIFFERENCE-IN-DIFFERENCES ##

# Get long-form data for each party

# VOX
dl_VOX = data %>%
  filter(!is.na(VOX201606) & fs_2016_06 > 0) %>%
  dplyr::select(-ends_with(c("20111", "201911"))) %>%
  pivot_longer(
    cols = starts_with("VOX"),
    names_to = "election",
    names_prefix = "VOX",
    values_to = c("VOX_share")) %>%
  mutate(VOX_share = VOX_share * 100) %>%
  as.data.frame()

# PP
dl_PP = data %>%
  filter(fs_2016_06 > 0) %>%
  dplyr::select(-ends_with(c("201911"))) %>%
  pivot_longer(
    cols = starts_with("PP"),
    names_to = "election",
    names_prefix = "PP",
    values_to = c("PP_share")) %>%
  mutate(PP_share = PP_share * 100) %>%
  as.data.frame()

# PSOE
dl_PSOE = data %>%
  filter(fs_2016_06 > 0) %>%
  dplyr::select(-ends_with(c("201911"))) %>%
  pivot_longer(
    cols = starts_with("PSOE"),
    names_to = "election",
    names_prefix = "PSOE",
    values_to = c("PSOE_share")) %>%
  mutate(PSOE_share = PSOE_share * 100) %>%
  as.data.frame()

# MAIN MODELS

did_VOX1 = lm(VOX_share ~ fs_rm_2016s2_2018s2_bin * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = subset(dl_VOX, election %in% c("201606", "201904")))

did_PP1 = lm(PP_share ~ fs_rm_2016s2_2018s2_bin * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = subset(dl_PP, election %in% c("201606", "201904")))

did_PSOE1 = lm(PSOE_share ~ fs_rm_2016s2_2018s2_bin * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = subset(dl_PSOE, election %in% c("201606", "201904")))

## Table 2 (main text)

stargazer(did_VOX1, did_PP1, did_PSOE1,
  title = "Francoist street name removal and increase in electoral support for parties",
  label = "tab:main_did",
  omit = "ccaa",
  omit.stat = c("f", "ser"), intercept.bottom = F,
  order = c("Constant", "2018s2_bin$", "201904$"),
  covariate.labels = c("(Intercept)",
  "Francoist street name removal",
  "Election April 2019",
  "Francoist removal $\\times$ April 2019",
  "Leftist major 2015", "Log. Population",
  "Log. No. Francoist streets $t_{0}$",
  "Unemployment"),
  multicolumn = FALSE,
  column.sep.width = "-20pt",
  dep.var.caption = "",
  dep.var.labels = c("VOX", "PP", "PSOE"),
  font.size = "small",
  digits = 3, digits.extra = 0,
  star.char = c("+", "*", "**", "***"),
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  notes.align = "c", align = T, no.space = TRUE,
  notes.label = "", notes.append = F,
  add.lines=list(c("CCAA Fixed Effects", rep("\\multicolumn{1}{c}{Yes}", 3))),
  notes = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Only municipalities that had at least one street with a Francoist name in $t_{0}$ were included in the sample.}")


## GETTING DID ESTIMATES (SIMULATION) ##

# Extra variables for simulation
extra_vars = data.frame(major_2015_izq = 0,
  lpop2011 = mean(data$lpop2011, na.rm = T),
  l_fs_2016_06 = mean(data$l_fs_2016_06, na.rm = T),
  unemp_2016 = mean(data$unemp_2016, na.rm = T))

# Simulating each model and obtaining estimates

sim_VOX = did_sim(m = did_VOX1,
  fs_chg_var = "fs_rm_2016s2_2018s2_bin",
  other_vars = extra_vars, FE = "ccaa")
sim_VOX_est = calculate_did_estimate(depvar_label = "VOX",
  simulations = sim_VOX, elec_t0 = "201606", elec_t1 = "201904")

sim_PP = did_sim(m = did_PP1,
  fs_chg_var = "fs_rm_2016s2_2018s2_bin",
  other_vars = extra_vars, FE = "ccaa")
sim_PP_est = calculate_did_estimate(depvar_label = "PP",
  simulations = sim_PP, elec_t0 = "201606", elec_t1 = "201904")

sim_PSOE = did_sim(m = did_PSOE1,
  fs_chg_var = "fs_rm_2016s2_2018s2_bin",
  other_vars = extra_vars, FE = "ccaa")
sim_PSOE_est = calculate_did_estimate(depvar_label = "PSOE",
  simulations = sim_PSOE, elec_t0 = "201606", elec_t1 = "201904")

# Putting all together

sim = rbind(sim_VOX_est, sim_PP_est, sim_PSOE_est)

## Figure 3 (main text)

pdf("DiD_estimates.pdf", width = 5.5, height = 3)
ggplot(sim, aes(x = depvar, y = mean)) +
  geom_point(shape = 1, size = 2) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0) +
  geom_errorbar(aes(ymin = lwr90, ymax = upr90), size = 1.1, width = 0) +
  geom_text(aes(label = round(mean, 2)), nudge_x = 0.15, size = 3) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    # panel.border = element_blank(),
    axis.ticks.y = element_blank()) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "",
    y = "\nIncrease in (%) vote share 2016-2019 due to\nFrancoist street name removal") +
  coord_flip()
dev.off()
