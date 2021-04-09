setwd("~/Google Drive/Academic/Projects/streets_vox")
options(stringsAsFactors = FALSE, scipen = 999)
library(dplyr)
library(tidyr)
library(stargazer)
library(ggplot2)
library(stringr)
library(MASS)
source("functions_did.R")

# Load
data = read.csv("data/data.csv")

# Log FS count variables
data = data %>% mutate(
  l_fs_2001_06 = log(fs_2001_06 + 1),
  l_fs_2016_06 = log(fs_2016_06 + 1),
  l_fs_2018_12 = log(fs_2018_12 + 1),
  l_fs_2019_06 = log(fs_2019_06 + 1),
  l_fs_rm_2016s2_2018s2 = log(fs_rm_2016s2_2018s2 + 1),
  change_2019_VOX = VOX201911 / VOX201904)
data$change_2019_VOX[data$change_2019_VOX == "Inf"] = NA

### ----------------------------------------------------------------------------
### Cross-sectional analyses ---------------------------------------------------

m_cs1 = lm(VOX201904 ~ fs_rm_2001s2_2018s2_bin +
  unemp_2019 + part201904 + lpop2011 + factor(ccaa), data = data)
m_cs2 = lm(VOX201911 ~ fs_rm_2001s2_2018s2_bin +
  unemp_2019 + part201911 + lpop2011 + factor(ccaa), data = data)
m_cs3 = lm(change_2019_VOX ~ fs_rm_2001s2_2018s2_bin +
  unemp_2019 + part201904 + lpop2011 + factor(ccaa), data = data)

m_cs4 = lm(VOX201904 ~ fs_rm_2001s2_2018s2_bin +
  unemp_2019 + part201904 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2001_06 > 0))
m_cs5 = lm(VOX201911 ~ fs_rm_2001s2_2018s2_bin +
  unemp_2019 + part201911 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2001_06 > 0))
m_cs6 = lm(change_2019_VOX ~ fs_rm_2001s2_2018s2_bin +
  unemp_2019 + part201904 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2001_06 > 0))

filecon = file("output/table_cs.tex")
writeLines(
  stargazer(m_cs1, m_cs2, m_cs4, m_cs5,
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
    notes = "\\parbox[t]{0.8\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. The main independent variable refers to the removal of Francoist street names between June 2001 and December 2018. Models 3 and 4 only include municipalities that had Francoist street names in June 2001.}"),
  filecon)
close(filecon)

filecon = file("output/table_cs_change.tex")
writeLines(
  stargazer(m_cs3, m_cs6,
    title = "Francoist street name removal and change in electoral support for Vox during 2019",
    label = "tab:cs_change",
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
    dep.var.labels = c("\\footnotesize Full sample", "\\footnotesize Limited sample"),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("CCAA Fixed Effects", rep("\\multicolumn{1}{c}{Yes}", 2))),
    notes = "\\parbox[t]{0.6\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. The main independent variable refers to the removal of Francoist street names between June 2001 and December 2018. The limited sample corresponds to municipalities that had Francoist street names in June 2001.}"),
  filecon)
close(filecon)

filecon = file("output/table_cs_all.tex")
writeLines(
  stargazer(m_cs1, m_cs2, m_cs3,
    title = "Francoist street name removal and electoral support for Vox",
    label = "tab:cs_all",
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
      "\\footnotesize Change"),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("CCAA Fixed Effects", rep("\\multicolumn{1}{c}{Yes}", 3))),
    notes = "\\parbox[t]{0.7\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. The main independent variable refers to the removal of Francoist street names between June 2001 and December 2018.}"),
  filecon)
close(filecon)

filecon = file("output/table_cs_limited.tex")
writeLines(
  stargazer(m_cs4, m_cs5, m_cs6,
    title = "Francoist street name removal and electoral support for Vox",
    label = "tab:cs_limited",
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
      "\\footnotesize Change"),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("CCAA Fixed Effects", rep("\\multicolumn{1}{c}{Yes}", 3))),
    notes = "\\parbox[t]{0.7\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. The main independent variable refers to the removal of Francoist street names between June 2001 and December 2018. Only municipalities that had Francoist street names in June 2001 were included.}"),
  filecon)
close(filecon)

m_cs7 = lm(VOX201904 ~ fs_rm_2011s1_2018s2_bin +
  unemp_2019 + part201904 + lpop2011 + factor(ccaa), data = data)
m_cs8 = lm(VOX201911 ~ fs_rm_2011s1_2018s2_bin +
  unemp_2019 + part201911 + lpop2011 + factor(ccaa), data = data)
m_cs9 = lm(change_2019_VOX ~ fs_rm_2011s1_2018s2_bin +
  unemp_2019 + part201904 + lpop2011 + factor(ccaa), data = data)

m_cs10 = lm(VOX201904 ~ fs_rm_2011s1_2018s2_bin +
  unemp_2019 + part201904 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2010_12 > 0))
m_cs11 = lm(VOX201911 ~ fs_rm_2011s1_2018s2_bin +
  unemp_2019 + part201911 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2010_12 > 0))
m_cs12 = lm(change_2019_VOX ~ fs_rm_2011s1_2018s2_bin +
  unemp_2019 + part201904 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2010_12 > 0))

filecon = file("output/table_cs_all_2011.tex")
writeLines(
  stargazer(m_cs7, m_cs8, m_cs9,
    title = "Electoral support for Vox and Francoist street name removal",
    label = "tab:cs_all_2011",
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
      "\\footnotesize Change"),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("CCAA Fixed Effects", rep("\\multicolumn{1}{c}{Yes}", 3))),
    notes = "\\parbox[t]{0.7\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. The main independent variable refers to the removal of Francoist street names between December 2010 and December 2018.}"),
  filecon)
close(filecon)

filecon = file("output/table_cs_limited_2011.tex")
writeLines(
  stargazer(m_cs10, m_cs11, m_cs12,
    title = "Electoral support for Vox and Francoist street name removal",
    label = "tab:cs_limited_2011",
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
      "\\footnotesize Change"),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("CCAA Fixed Effects", rep("\\multicolumn{1}{c}{Yes}", 3))),
    notes = "\\parbox[t]{0.7\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. The main independent variable refers to the removal of Francoist street names between December 2010 and December 2018. Only municipalities that had Francoist street names in June 2011 were included.}"),
  filecon)
close(filecon)

### ----------------------------------------------------------------------------
### Differences-in-Differences -------------------------------------------------

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

# MAIN MODELS -------------------------------------------------------

did_VOX1 = lm(VOX_share ~ fs_rm_2016s2_2018s2_bin * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = subset(dl_VOX, election %in% c("201606", "201904")))

did_PP1 = lm(PP_share ~ fs_rm_2016s2_2018s2_bin * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = subset(dl_PP, election %in% c("201606", "201904")))

did_PSOE1 = lm(PSOE_share ~ fs_rm_2016s2_2018s2_bin * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = subset(dl_PSOE, election %in% c("201606", "201904")))

filecon = file("output/table_main_did.tex")
writeLines(
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
    notes = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Only municipalities that had at least one street with a Francoist name in $t_{0}$ were included in the sample.}"),
  filecon)
close(filecon)


# SIMULATIONS

# Extra variables for simulation
extra_vars = data.frame(major_2015_izq = 0,
  lpop2011 = mean(data$lpop2011, na.rm = T),
  l_fs_2016_06 = mean(data$l_fs_2016_06, na.rm = T),
  unemp_2016 = mean(data$unemp_2016, na.rm = T))

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

sim = rbind(sim_VOX_est, sim_PP_est, sim_PSOE_est)

pdf("output/DiD_estimates.pdf", width = 5.5, height = 3)
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

## Robustness for VOX and PP ----------------------------------------

VOX2016_above0 = subset(data, VOX201606 > 0)$muni_code
dl_VOX$election = relevel(factor(dl_VOX$election), ref = "201606")
dl_PP$election = relevel(factor(dl_PP$election), ref = "201606")
dl_PSOE$election = relevel(factor(dl_PSOE$election), ref = "201606")

# Also including elections before 2016
did_VOX_alt1 = lm(VOX_share ~ fs_rm_2016s2_2018s2_bin * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = dl_VOX)
# Extending DV to first half of 2019
did_VOX_alt2 = lm(VOX_share ~ fs_rm_2016s2_2019s1_bin * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = dl_VOX)
# Using DV as continuous (log n changes)
did_VOX_alt3 = lm(VOX_share ~ l_fs_rm_2016s2_2018s2 * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = dl_VOX)
# Also municipalities where VOX got votes in 2016
did_VOX_alt4 = lm(VOX_share ~ fs_rm_2016s2_2018s2_bin * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = subset(dl_VOX, muni_code %in% VOX2016_above0))

did_VOX_alt1_m = did_VOX_alt1
did_VOX_alt2_m = did_VOX_alt2
did_VOX_alt3_m = did_VOX_alt3
did_VOX_alt4_m = did_VOX_alt4
names(did_VOX_alt1_m$coefficients) = gsub("fs_rm_2016s2_2018s2_bin", "fsn_removal",
  names(did_VOX_alt1_m$coefficients))
names(did_VOX_alt2_m$coefficients) = gsub("fs_rm_2016s2_2019s1_bin", "fsn_removal",
  names(did_VOX_alt2_m$coefficients))
names(did_VOX_alt3_m$coefficients) = gsub("l_fs_rm_2016s2_2018s2", "fsn_removal",
  names(did_VOX_alt3_m$coefficients))
names(did_VOX_alt4_m$coefficients) = gsub("fs_rm_2016s2_2018s2_bin", "fsn_removal",
  names(did_VOX_alt4_m$coefficients))

stargazer(did_VOX_alt1_m, did_VOX_alt2_m, did_VOX_alt3_m, did_VOX_alt4_m,
  type = "text", #omit = "ccaa",
  omit = c("ccaa", "major_2015_izq", "lpop2011", "l_fs_2016_06", "unemp_2016"),
  intercept.bottom = FALSE, omit.stat = c("f","ser"))

filecon = file("output/table_VOX_robustness.tex")
writeLines(
  stargazer(did_VOX_alt1_m, did_VOX_alt2_m, did_VOX_alt3_m, did_VOX_alt4_m,
    title = "Francoist street name removal and increase in electoral support for Vox",
    label = "tab:vox_robustness",
    omit = c("ccaa", "major_2015_izq", "lpop2011", "l_fs_2016_06", "unemp_2016"),
    omit.stat = c("f", "ser"), intercept.bottom = F,
    covariate.labels = c("(Intercept)",
    "Francoist street name removal",
    "Election December 2015",
    "Election April 2019",
    "Francoist removal $\\times$ Dec 2015",
    "Francoist removal $\\times$ April 2019"),
    multicolumn = FALSE,
    column.sep.width = "-20pt",
    dep.var.caption = "",
    dep.var.labels.include = FALSE,
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("CCAA Fixed Effects", rep("\\multicolumn{1}{c}{Yes}", 4)),
      c("Controls", rep("\\multicolumn{1}{c}{Yes}", 4))),
    notes = "\\parbox[t]{0.85\\textwidth}{\\textit{Note:} + $p<0.1$; * $p<0.05$; ** $p<0.01$; *** $p<0.001$. All models also include elections before June 2016 (December 2015). Model 2 extends the DV (name removal) to the first half of 2019. Model 3 uses the IV in continuous form (logged number of changes). Model 4 restricts the sample to municipalities where Vox got more than 0 votes. Controls include a dummy for a leftist major elected in 2015 local elections, logged population in 2011, logged number of Francoist streets in $t_{0}$, and the unemployment rate in January 2016. Only municipalities that had at least one street with a Francoist name in $t_{0}$ (June 2016) were included in the sample.}"),
  filecon)
close(filecon)

did_PP_alt1 = lm(PP_share ~ fs_rm_2016s2_2018s2_bin * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = dl_PP)
did_PP_alt2 = lm(PP_share ~ fs_rm_2016s2_2019s1_bin * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = dl_PP)
did_PP_alt3 = lm(PP_share ~ l_fs_rm_2016s2_2018s2 * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = dl_PP)
did_PP_alt4 = lm(PP_share ~ fs_rm_2016s2_2018s2_bin * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = subset(dl_PP, muni_code %in% VOX2016_above0))

did_PP_alt1_m = did_PP_alt1
did_PP_alt2_m = did_PP_alt2
did_PP_alt3_m = did_PP_alt3
did_PP_alt4_m = did_PP_alt4
names(did_PP_alt1_m$coefficients) = gsub("fs_rm_2016s2_2018s2_bin", "fsn_removal",
  names(did_PP_alt1_m$coefficients))
names(did_PP_alt2_m$coefficients) = gsub("fs_rm_2016s2_2019s1_bin", "fsn_removal",
  names(did_PP_alt2_m$coefficients))
names(did_PP_alt3_m$coefficients) = gsub("l_fs_rm_2016s2_2018s2", "fsn_removal",
  names(did_PP_alt3_m$coefficients))
names(did_PP_alt4_m$coefficients) = gsub("fs_rm_2016s2_2018s2_bin", "fsn_removal",
  names(did_PP_alt4_m$coefficients))

filecon = file("output/table_PP_robustness.tex")
writeLines(
  stargazer(did_PP_alt1_m, did_PP_alt2_m, did_PP_alt3_m, did_PP_alt4_m,
    title = "Francoist street name removal and increase in electoral support for PP",
    label = "tab:PP_robustness",
    omit = c("ccaa", "major_2015_izq", "lpop2011", "l_fs_2016_06", "unemp_2016"),
    omit.stat = c("f", "ser"), intercept.bottom = F,
    covariate.labels = c("(Intercept)",
    "Francoist street name removal",
    "Election November 2011",
    "Election December 2015",
    "Election April 2019",
    "Francoist removal $\\times$ Nov 2011",
    "Francoist removal $\\times$ Dec 2015",
    "Francoist removal $\\times$ April 2019"),
    multicolumn = FALSE,
    column.sep.width = "-20pt",
    dep.var.caption = "",
    dep.var.labels.include = FALSE,
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("CCAA Fixed Effects", rep("\\multicolumn{1}{c}{Yes}", 4)),
      c("Controls", rep("\\multicolumn{1}{c}{Yes}", 4))),
    notes = "\\parbox[t]{0.85\\textwidth}{\\textit{Note:} + $p<0.1$; * $p<0.05$; ** $p<0.01$; *** $p<0.001$. All models also include elections before June 2016 (December 2015). Model 2 extends the DV (name removal) to the first half of 2019. Model 3 uses the IV in continuous form (logged number of changes). Model 4 restricts the sample to municipalities where Vox got more than 0 votes. Controls include a dummy for a leftist major elected in 2015 local elections, logged population in 2011, logged number of Francoist streets in $t_{0}$, and the unemployment rate in January 2016. Only municipalities that had at least one street with a Francoist name in $t_{0}$ (June 2016) were included in the sample.}"),
  filecon)
close(filecon)


## For the Appendix, logit on Francoist street name removal

fsrm1 = glm(fs_rm_2016s2_2018s2_bin ~
  major_2015_izq + lpop2011 + l_fs_2016_06,
  data = subset(data, fs_2016_06 > 0))
fsrm2 = glm(fs_rm_2016s2_2018s2_bin ~
  major_2015_izq + lpop2011 + l_fs_2016_06 + factor(ccaa),
  data = subset(data, fs_2016_06 > 0))
fsrm3 = glm(fs_rm_2016s2_2018s2_bin ~
  major_2015_izq + lpop2011 + l_fs_2016_06 + PP201606 + VOX201606 + part201606 + factor(ccaa),
  data = subset(data, fs_2016_06 > 0))

filecon = file("output/table_logit_fs_removal.tex")
writeLines(
  stargazer(fsrm1, fsrm2, fsrm3,
    title = "Logit regression on Francoist street name removal (2016--2018)",
    label = "tab:logit_fs_rm",
    omit = "ccaa",
    omit.stat = c("f", "ser"), intercept.bottom = F,
    covariate.labels = c("(Intercept)",
    "Leftist mayor 2015",
    "Log. Population 2011",
    "Log. No. Francoist streets June 2016",
    "PP support, June 2016",
    "Vox support, June 2016",
    "Turnout, June 2016"),
    multicolumn = FALSE,
    column.sep.width = "-20pt",
    dep.var.caption = "",
    dep.var.labels.include = FALSE,
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("CCAA Fixed Effects", "\\multicolumn{1}{c}{No}", rep("\\multicolumn{1}{c}{Yes}", 2))),
    notes = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} + $p<0.1$; * $p<0.05$; ** $p<0.01$; *** $p<0.001$. Only including municipalities that had at least one street with Francoist names in June 2016.}"),
  filecon)
close(filecon)
