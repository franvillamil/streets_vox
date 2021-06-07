# setwd("~/Documents/Projects/streets_vox")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "tidyr", "stargazer", "ggplot2", "stringr", "MASS")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
lapply(pkg, library, character.only = TRUE)

# ------------------------------

# Load functions
source("func/my_stargazer.R")

# ------------------------------

# Load data
data = read.csv("dataset/output/data.csv")
dl_VOX = read.csv("dataset/output/dl_VOX.csv")
dl_PP = read.csv("dataset/output/dl_PP.csv")
dl_PSOE = read.csv("dataset/output/dl_PSOE.csv")

# Sample variables and relevel
VOX2016_above0 = subset(data, VOX2016_06 > 0)$muni_code
dl_VOX$election = relevel(factor(dl_VOX$election), ref = "2016_06")
dl_PP$election = relevel(factor(dl_PP$election), ref = "2016_06")

# First diff models, new variables
data = data %>%
  mutate(
    d_VOX_2015_2016 = (VOX2016_06 - VOX2015_12),
    d_VOX_2016_2019 = (VOX2019_04 - VOX2016_06),
    d_VOX_2019_2019 = (VOX2019_11 - VOX2019_04),
    d_PP_2008_2011 = (PP2011_11 - PP2008_03),
    d_PP_2011_2015 = (PP2015_12 - PP2011_11),
    d_PP_2015_2016 = (PP2016_06 - PP2015_12),
    d_PP_2016_2019 = (PP2019_04 - PP2016_06),
    d_PP_2019_2019 = (PP2019_11 - PP2019_04))

# ------------------------------
# Cross-sectional models

# Change as DV
m_cs_chg1 = lm(change_2019_VOX ~ fs_rm_2001s2_2018s2_bin +
  unemp_2019 + part2019_04 + lpop2011 + factor(ccaa), data = data)
m_cs_chg2 = lm(change_2019_VOX ~ fs_rm_2001s2_2018s2_bin +
  unemp_2019 + part2019_04 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2001_06 > 0))

# Alternative period (2011-2018), full sample & limited sample
m_cs_1118_full1 = lm(VOX2019_04 ~ fs_rm_2011s1_2018s2_bin +
  unemp_2019 + part2019_04 + lpop2011 + factor(ccaa), data = data)
m_cs_1118_full2 = lm(VOX2019_11 ~ fs_rm_2011s1_2018s2_bin +
  unemp_2019 + part2019_11 + lpop2011 + factor(ccaa), data = data)
m_cs_1118_full3 = lm(change_2019_VOX ~ fs_rm_2011s1_2018s2_bin +
  unemp_2019 + part2019_04 + lpop2011 + factor(ccaa), data = data)

m_cs_1118_lim1 = lm(VOX2019_04 ~ fs_rm_2011s1_2018s2_bin +
  unemp_2019 + part2019_04 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2010_12 > 0))
m_cs_1118_lim2 = lm(VOX2019_11 ~ fs_rm_2011s1_2018s2_bin +
  unemp_2019 + part2019_11 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2010_12 > 0))
m_cs_1118_lim3 = lm(change_2019_VOX ~ fs_rm_2011s1_2018s2_bin +
  unemp_2019 + part2019_04 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2010_12 > 0))

my_stargazer(dest_file = "robust/output/tab_cs_change.tex",
  model_list = list(m_cs_chg1, m_cs_chg2),
  omit = "ccaa",
  label = "tab:cs_change",
  title = "Francoist street name removal and change in electoral support for Vox during 2019",
  order = c("Constant"),
  dep.var.labels = c("\\footnotesize Full sample", "\\footnotesize Limited sample"),
  covariate.labels = c("(Intercept)",
    "Francoist street name removal",
    "Unemployment 2019",
    "Turnout April 2019",
    "Turnout Nov 2019",
    "Log. Population"),
  notes_table = "\\parbox[t]{0.6\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. The main independent variable refers to the removal of Francoist street names between June 2001 and December 2018. The limited sample corresponds to municipalities that had Francoist street names in June 2001.}")

my_stargazer(dest_file = "robust/output/tab_cs_all_2011.tex",
  model_list = list(m_cs_1118_full1, m_cs_1118_full2, m_cs_1118_full3),
  omit = "ccaa",
  label = "tab:cs_all_2011",
  title = "Electoral support for Vox and Francoist street name removal (2011--2018)",
  order = c("Constant"),
  dep.var.labels = c("\\footnotesize Apr 2019", "\\footnotesize Nov 2019",
    "\\footnotesize Change"),
  covariate.labels = c("(Intercept)",
    "Francoist street name removal",
    "Unemployment 2019",
    "Turnout April 2019",
    "Turnout Nov 2019",
    "Log. Population"),
  notes_table = "\\parbox[t]{0.7\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. The main independent variable refers to the removal of Francoist street names between December 2010 and December 2018.}")

my_stargazer(dest_file = "robust/output/tab_cs_limited_2011.tex",
  model_list = list(m_cs_1118_lim1, m_cs_1118_lim2, m_cs_1118_lim3),
  omit = "ccaa",
  label = "tab:cs_limited_2011",
  title = "Electoral support for Vox and Francoist street name removal (2011--2018), limited sample",
  order = c("Constant"),
  dep.var.labels = c("\\footnotesize Apr 2019", "\\footnotesize Nov 2019",
    "\\footnotesize Change"),
  covariate.labels = c("(Intercept)",
    "Francoist street name removal",
    "Unemployment 2019",
    "Turnout April 2019",
    "Turnout Nov 2019",
    "Log. Population"),
  notes_table = "\\parbox[t]{0.7\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. The main independent variable refers to the removal of Francoist street names between December 2010 and December 2018. Only municipalities that had Francoist street names in January 2011 were included.}")

# ------------------------------
# Tracking street name changes in different periods

m_csperiods1 = lm(VOX2019_04 ~ fs_rm_2001s2_2015s2_bin +
  unemp_2019 + part2019_04 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2001_06 > 0))
m_csperiods2 = lm(VOX2019_04 ~ fs_rm_2001s2_2018s2_bin +
  unemp_2019 + part2019_04 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2001_06 > 0))
m_csperiods3 = lm(VOX2019_04 ~ fs_rm_2011s1_2018s2_bin +
  unemp_2019 + part2019_04 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2010_12 > 0))
m_csperiods4 = lm(VOX2019_04 ~ fs_rm_2016s2_2018s2_bin +
  unemp_2019 + part2019_04 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2016_06 > 0))

# Change coefficient names to plot together in stargazer
m_csperiods1_m = m_csperiods1
m_csperiods2_m = m_csperiods2
m_csperiods3_m = m_csperiods3
m_csperiods4_m = m_csperiods4
names(m_csperiods1_m$coefficients) = gsub("fs_rm_2001s2_2015s2_bin", "fsn_removal",
  names(m_csperiods1_m$coefficients))
names(m_csperiods2_m$coefficients) = gsub("fs_rm_2001s2_2018s2_bin", "fsn_removal",
  names(m_csperiods2_m$coefficients))
names(m_csperiods3_m$coefficients) = gsub("fs_rm_2011s1_2018s2_bin", "fsn_removal",
  names(m_csperiods3_m$coefficients))
names(m_csperiods4_m$coefficients) = gsub("fs_rm_2016s2_2018s2_bin", "fsn_removal",
  names(m_csperiods4_m$coefficients))

my_stargazer(dest_file = "robust/output/tab_cs_periods.tex",
  model_list = list(m_csperiods1_m, m_csperiods2_m, m_csperiods3_m, m_csperiods4_m),
  omit = "ccaa",
  label = "tab:cs_periods",
  title = "Electoral support for Vox in 2019 and Francoist street name removal across different periods",
  order = c("Constant"),
  dep.var.labels = c("2001-2015", "2001-2018", "2011-2018", "2016-2018"),
  covariate.labels = c("(Intercept)",
    "Francoist street name removal",
    "Unemployment 2019",
    "Turnout April 2019",
    "Log. Population"),
  notes_table = "\\parbox[t]{0.7\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. The main independent variable refers to the removal of Francoist street names in different periods: 1) June 2001 - December 2015, 2) June 2001 - December 2018, 3) December 2010 - December 2018, and 4) June 2016 - December 2018. Only municipalities that had Francoist street names at the beginning of each period were included.}")

# ------------------------------
# Tracking street name changes in different periods (CONTINUOUS form)

m_csperiods_c1 = lm(VOX2019_04 ~ l_fs_rm_2001s2_2015s2 +
  unemp_2019 + part2019_04 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2001_06 > 0))
m_csperiods_c2 = lm(VOX2019_04 ~ l_fs_rm_2001s2_2018s2 +
  unemp_2019 + part2019_04 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2001_06 > 0))
m_csperiods_c3 = lm(VOX2019_04 ~ l_fs_rm_2011s1_2018s2 +
  unemp_2019 + part2019_04 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2010_12 > 0))
m_csperiods_c4 = lm(VOX2019_04 ~ l_fs_rm_2016s2_2018s2 +
  unemp_2019 + part2019_04 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2016_06 > 0))

# Change coefficient names to plot together in stargazer
m_csperiods_c1_m = m_csperiods_c1
m_csperiods_c2_m = m_csperiods_c2
m_csperiods_c3_m = m_csperiods_c3
m_csperiods_c4_m = m_csperiods_c4
names(m_csperiods_c1_m$coefficients) = gsub("l_fs_rm_2001s2_2015s2", "fsn_removal",
  names(m_csperiods_c1_m$coefficients))
names(m_csperiods_c2_m$coefficients) = gsub("l_fs_rm_2001s2_2018s2", "fsn_removal",
  names(m_csperiods_c2_m$coefficients))
names(m_csperiods_c3_m$coefficients) = gsub("l_fs_rm_2011s1_2018s2", "fsn_removal",
  names(m_csperiods_c3_m$coefficients))
names(m_csperiods_c4_m$coefficients) = gsub("l_fs_rm_2016s2_2018s2", "fsn_removal",
  names(m_csperiods_c4_m$coefficients))

my_stargazer(dest_file = "robust/output/tab_cs_periods_cont.tex",
  model_list = list(m_csperiods_c1_m, m_csperiods_c2_m, m_csperiods_c3_m, m_csperiods_c4_m),
  omit = "ccaa",
  label = "tab:cs_periods_cont",
  title = "Electoral support for Vox in 2019 and Francoist street name removal across different periods (in continuous form)",
  order = c("Constant"),
  dep.var.labels = c("2001-2015", "2001-2018", "2011-2018", "2016-2018"),
  covariate.labels = c("(Intercept)",
    "Francoist street name rm (log no)",
    "Unemployment 2019",
    "Turnout April 2019",
    "Log. Population"),
  notes_table = "\\parbox[t]{0.7\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. The main independent variable refers to the (logged) number of Francoist street name removals in different periods: 1) June 2001 - December 2015, 2) June 2001 - December 2018, 3) December 2010 - December 2018, and 4) June 2016 - December 2018. Only municipalities that had Francoist street names at the beginning of each period were included.}")

# ------------------------------
# First differences (VOX and PP)
# from: https://rpubs.com/eliascis/fd-plm-lfe

# Right side formula
rs = "~ fs_rm_2016s2_2018s2_bin + factor(ccaa)"

# FD data
fd_data = subset(data, fs_2016_06 > 0)

# VOX
m_fd_VOX_15 = lm(as.formula(paste0("d_VOX_2015_2016", rs)), data = fd_data)
m_fd_VOX_16 = lm(as.formula(paste0("d_VOX_2016_2019", rs)), data = fd_data)
m_fd_VOX_19 = lm(as.formula(paste0("d_VOX_2019_2019", rs)), data = fd_data)
# PP
m_fd_PP_15 = lm(as.formula(paste0("d_PP_2015_2016", rs)), data = fd_data)
m_fd_PP_16 = lm(as.formula(paste0("d_PP_2016_2019", rs)), data = fd_data)
m_fd_PP_19 = lm(as.formula(paste0("d_PP_2019_2019", rs)), data = fd_data)

my_stargazer(dest_file = "robust/output/tab_firstdiff_vox.tex",
  model_list = list(m_fd_VOX_15, m_fd_VOX_16, m_fd_VOX_19),
  omit = "ccaa",
  label = "tab:firstdiff_vox",
  title = "First differences model on change in support for Vox",
  order = c("Constant"),
  dep.var.labels = c("2015-2016", "2016-2019", "2019-2019"),
  covariate.labels = c("(Intercept)",
    "Francoist street name removal"),
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} + $p<0.1$; * $p<0.05$; ** $p<0.01$; *** $p<0.001$. The dependent variable refers to the change in support for Vox ($t_{1} - t_{0}$) in each of the three periods. Only municipalities that had at least one street with a Francoist name in $t_{0}$ (June 2016) were included in the sample.}")

my_stargazer(dest_file = "robust/output/tab_firstdiff_pp.tex",
  model_list = list(m_fd_PP_15, m_fd_PP_16, m_fd_PP_19),
  omit = "ccaa",
  label = "tab:firstdiff_pp",
  title = "First differences model on change in support for PP",
  order = c("Constant"),
  dep.var.labels = c("2015-2016", "2016-2019", "2019-2019"),
  covariate.labels = c("(Intercept)",
    "Francoist street name removal"),
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} + $p<0.1$; * $p<0.05$; ** $p<0.01$; *** $p<0.001$. The dependent variable refers to the change in support for PP ($t_{1} - t_{0}$) in each of the three periods. Only municipalities that had at least one street with a Francoist name in $t_{0}$ (June 2016) were included in the sample.}")

# ------------------------------
# Alternative DiD models (VOX)

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

# Change coefficient names to plot together in stargazer
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

my_stargazer(dest_file = "robust/output/tab_vox_robustness.tex",
  model_list = list(did_VOX_alt1_m, did_VOX_alt2_m, did_VOX_alt3_m, did_VOX_alt4_m),
  omit = c("ccaa", "major_2015_izq", "lpop2011", "l_fs_2016_06", "unemp_2016"),
  label = "tab:vox_robustness",
  title = "Francoist street name removal and increase in electoral support for Vox",
  order = c("Constant"),
  dep.var.labels = NULL,
  dep.var.labels.include = FALSE,
  covariate.labels = c("(Intercept)",
    "Francoist street name removal",
    "Election December 2015",
    "Election April 2019",
    "Francoist removal $\\times$ Dec 2015",
    "Francoist removal $\\times$ April 2019"),
  add.lines=list(
    c("Controls", rep("\\multicolumn{1}{c}{Yes}", 4)),
    c("CCAA Fixed Effects", rep("\\multicolumn{1}{c}{Yes}", 4))
    ),
  notes_table = "\\parbox[t]{0.85\\textwidth}{\\textit{Note:} + $p<0.1$; * $p<0.05$; ** $p<0.01$; *** $p<0.001$. All models also include elections before June 2016 (December 2015). Model 2 extends the DV (name removal) to the first half of 2019. Model 3 uses the IV in continuous form (logged number of changes). Model 4 restricts the sample to municipalities where Vox got more than 0 votes. Controls include a dummy for a leftist major elected in 2015 local elections, logged population in 2011, logged number of Francoist streets in $t_{0}$, and the unemployment rate in January 2016. Only municipalities that had at least one street with a Francoist name in $t_{0}$ (June 2016) were included in the sample.}")

# ------------------------------
# DiD models (PP)

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

# Change coefficient names to plot together in stargazer
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

my_stargazer(dest_file = "robust/output/tab_pp_robustness.tex",
  model_list = list(did_PP_alt1_m, did_PP_alt2_m, did_PP_alt3_m, did_PP_alt4_m),
  omit = c("ccaa", "major_2015_izq", "lpop2011", "l_fs_2016_06", "unemp_2016"),
  label = "tab:pp_robustness",
  covariate.labels = c("(Intercept)",
    "Francoist street name removal",
    "Election March 2000",
    "Election March 2004",
    "Election March 2008",
    "Election November 2011",
    "Election December 2015",
    "Election April 2019",
    "Francoist removal $\\times$ March 2000",
    "Francoist removal $\\times$ March 2004",
    "Francoist removal $\\times$ March 2008",
    "Francoist removal $\\times$ Nov 2011",
    "Francoist removal $\\times$ Dec 2015",
    "Francoist removal $\\times$ April 2019"),
  title = "Francoist street name removal and increase in electoral support for PP",
  order = c("Constant"),
  dep.var.labels = NULL,
  dep.var.labels.include = FALSE,
  add.lines=list(
    c("Controls", rep("\\multicolumn{1}{c}{Yes}", 4)),
    c("CCAA Fixed Effects", rep("\\multicolumn{1}{c}{Yes}", 4))
    ),
  notes_table = "\\parbox[t]{0.85\\textwidth}{\\textit{Note:} + $p<0.1$; * $p<0.05$; ** $p<0.01$; *** $p<0.001$. All models also include elections before June 2016 (2000--2015). Model 2 extends the DV (name removal) to the first half of 2019. Model 3 uses the IV in continuous form (logged number of changes). Model 4 restricts the sample to municipalities where Vox got more than 0 votes. Controls include a dummy for a leftist major elected in 2015 local elections, logged population in 2011, logged number of Francoist streets in $t_{0}$, and the unemployment rate in January 2016. Only municipalities that had at least one street with a Francoist name in $t_{0}$ (June 2016) were included in the sample.}")

# ------------------------------
# DiD models (PSOE)

did_PSOE_alt1 = lm(PSOE_share ~ fs_rm_2016s2_2018s2_bin * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = dl_PSOE)
did_PSOE_alt2 = lm(PSOE_share ~ fs_rm_2016s2_2019s1_bin * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = dl_PSOE)
did_PSOE_alt3 = lm(PSOE_share ~ l_fs_rm_2016s2_2018s2 * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = dl_PSOE)
did_PSOE_alt4 = lm(PSOE_share ~ fs_rm_2016s2_2018s2_bin * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = subset(dl_PSOE, muni_code %in% VOX2016_above0))

# Change coefficient names to plot together in stargazer
did_PSOE_alt1_m = did_PSOE_alt1
did_PSOE_alt2_m = did_PSOE_alt2
did_PSOE_alt3_m = did_PSOE_alt3
did_PSOE_alt4_m = did_PSOE_alt4
names(did_PSOE_alt1_m$coefficients) = gsub("fs_rm_2016s2_2018s2_bin", "fsn_removal",
names(did_PSOE_alt1_m$coefficients))
names(did_PSOE_alt2_m$coefficients) = gsub("fs_rm_2016s2_2019s1_bin", "fsn_removal",
names(did_PSOE_alt2_m$coefficients))
names(did_PSOE_alt3_m$coefficients) = gsub("l_fs_rm_2016s2_2018s2", "fsn_removal",
names(did_PSOE_alt3_m$coefficients))
names(did_PSOE_alt4_m$coefficients) = gsub("fs_rm_2016s2_2018s2_bin", "fsn_removal",
names(did_PSOE_alt4_m$coefficients))

my_stargazer(dest_file = "robust/output/tab_psoe_robustness.tex",
model_list = list(did_PSOE_alt1_m, did_PSOE_alt2_m, did_PSOE_alt3_m, did_PSOE_alt4_m),
omit = c("ccaa", "major_2015_izq", "lpop2011", "l_fs_2016_06", "unemp_2016"),
label = "tab:PSOE_robustness",
covariate.labels = c("(Intercept)",
  "Francoist street name removal",
  "Election March 2000",
  "Election March 2004",
  "Election March 2008",
  "Election November 2011",
  "Election December 2015",
  "Election April 2019",
  "Francoist removal $\\times$ March 2000",
  "Francoist removal $\\times$ March 2004",
  "Francoist removal $\\times$ March 2008",
  "Francoist removal $\\times$ Nov 2011",
  "Francoist removal $\\times$ Dec 2015",
  "Francoist removal $\\times$ April 2019"),
title = "Francoist street name removal and increase in electoral support for PSOE",
order = c("Constant"),
dep.var.labels = NULL,
dep.var.labels.include = FALSE,
add.lines=list(
  c("Controls", rep("\\multicolumn{1}{c}{Yes}", 4)),
  c("CCAA Fixed Effects", rep("\\multicolumn{1}{c}{Yes}", 4))
  ),
notes_table = "\\parbox[t]{0.85\\textwidth}{\\textit{Note:} + $p<0.1$; * $p<0.05$; ** $p<0.01$; *** $p<0.001$. All models also include elections before June 2016 (2000--2015). Model 2 extends the DV (name removal) to the first half of 2019. Model 3 uses the IV in continuous form (logged number of changes). Model 4 restricts the sample to municipalities where Vox got more than 0 votes. Controls include a dummy for a leftist major elected in 2015 local elections, logged population in 2011, logged number of Francoist streets in $t_{0}$, and the unemployment rate in January 2016. Only municipalities that had at least one street with a Francoist name in $t_{0}$ (June 2016) were included in the sample.}")

# ------------------------------
# Removal of Francoist streets as DV

fsrm1 = glm(fs_rm_2016s2_2018s2_bin ~
  major_2015_izq + lpop2011 + l_fs_2016_06,
  data = subset(data, fs_2016_06 > 0))

fsrm2 = glm(fs_rm_2016s2_2018s2_bin ~
  major_2015_izq + lpop2011 + l_fs_2016_06 + factor(ccaa),
  data = subset(data, fs_2016_06 > 0))

fsrm3 = glm(fs_rm_2016s2_2018s2_bin ~
  major_2015_izq + lpop2011 + l_fs_2016_06 + PP2016_06 + VOX2016_06 + part2016_06 + factor(ccaa),
  data = subset(data, fs_2016_06 > 0))

my_stargazer(dest_file = "robust/output/tab_logit_fs_rm.tex",
  model_list = list(fsrm1, fsrm2, fsrm3),
  omit = "ccaa",
  label = "tab:logit_fs_rm",
  title = "Logit regression on Francoist street name removal (2016--2018)",
  order = c("Constant"),
  dep.var.labels = NULL,
  dep.var.labels.include = FALSE,
  covariate.labels = c("(Intercept)",
    "Leftist mayor 2015",
    "Log. Population 2011",
    "Log. No. Francoist streets June 2016",
    "PP support, June 2016",
    "Vox support, June 2016",
    "Turnout, June 2016"),
  add.lines=list(
    c("CCAA Fixed Effects", "\\multicolumn{1}{c}{No}", rep("\\multicolumn{1}{c}{Yes}", 2))),
  notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} + $p<0.1$; * $p<0.05$; ** $p<0.01$; *** $p<0.001$. Only including municipalities that had at least one street with Francoist names in June 2016.}")
