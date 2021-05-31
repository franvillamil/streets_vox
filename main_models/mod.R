# setwd("~/Documents/Projects/streets_vox")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "tidyr", "stargazer", "ggplot2", "stringr",
  "MASS", "miceadds", "sandwich")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
lapply(pkg, library, character.only = TRUE)

# ------------------------------

# Load functions
source("func/functions_did.R")
source("func/my_stargazer.R")
source("func/cluster_se.R")

# ------------------------------

# Load data
data = read.csv("dataset/output/data.csv")
dl_VOX = read.csv("dataset/output/dl_VOX.csv")
dl_PP = read.csv("dataset/output/dl_PP.csv")
dl_PSOE = read.csv("dataset/output/dl_PSOE.csv")

# ------------------------------
# Cross-sectional models


m_cs1 = lm(VOX2019_04 ~ fs_rm_2001s2_2018s2_bin +
  unemp_2019 + part2019_04 + lpop2011 + factor(ccaa),
  data = data)

m_cs2 = lm(VOX2019_11 ~ fs_rm_2001s2_2018s2_bin +
  unemp_2019 + part2019_11 + lpop2011 + factor(ccaa),
  data = data)

m_cs3 = lm(VOX2019_04 ~ fs_rm_2001s2_2018s2_bin +
  unemp_2019 + part2019_04 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2001_06 > 0))

m_cs4 = lm(VOX2019_11 ~ fs_rm_2001s2_2018s2_bin +
  unemp_2019 + part2019_11 + lpop2011 + factor(ccaa),
  data = subset(data, fs_2001_06 > 0))

my_stargazer(dest_file = "main_models/output/tab_cs.tex",
  model_list = list(m_cs1, m_cs2, m_cs3, m_cs4),
  se = lapply(list(m_cs1, m_cs2, m_cs3, m_cs4), function(x) cluster_se(x)),
  omit = "ccaa",
  label = "tab:cs",
  title = "Francoist street name removal and electoral support for Vox",
  order = c("Constant"),
  dep.var.labels = c("\\footnotesize Apr 2019", "\\footnotesize Nov 2019",
    "\\footnotesize Apr 2019", "\\footnotesize Nov 2019"),
  covariate.labels = c("(Intercept)",
    "Francoist street name removal",
    "Unemployment 2019",
    "Turnout April 2019",
    "Turnout Nov 2019",
    "Log. Population"),
  notes_table = "\\parbox[t]{0.8\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. The main independent variable refers to the removal of Francoist street names between June 2001 and December 2018. Models 3 and 4 only include municipalities that had Francoist street names in June 2001.}")

# ------------------------------
# Difference-in-Differences

did_VOX1 = lm(VOX_share ~ fs_rm_2016s2_2018s2_bin * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = subset(dl_VOX, election %in% c("2016_06", "2019_04")))

did_PP1 = lm(PP_share ~ fs_rm_2016s2_2018s2_bin * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = subset(dl_PP, election %in% c("2016_06", "2019_04")))

did_PSOE1 = lm(PSOE_share ~ fs_rm_2016s2_2018s2_bin * factor(election) +
  major_2015_izq + lpop2011 + l_fs_2016_06 + unemp_2016 + factor(ccaa),
  data = subset(dl_PSOE, election %in% c("2016_06", "2019_04")))

main_did = list(did_VOX1, did_PP1, did_PSOE1)

my_stargazer(dest_file = "main_models/output/tab_main_did.tex",
  model_list = main_did,
  se = lapply(main_did, function(x) cluster_se(x)),
  omit = c("ccaa", "major_2015_izq", "lpop2011", "l_fs_2016_06", "unemp_2016"),
  label = "tab:main_did",
  title = "Francoist street name removal and increase in electoral support for parties",
  dep.var.labels = c("VOX", "PP", "PSOE"),
  order = c("Constant"),
  covariate.labels = c("(Intercept)",
    "Francoist street name removal",
    "Election April 2019",
    "Francoist removal $\\times$ April 2019"),
  add.lines=list(
    c("Controls", rep("\\multicolumn{1}{c}{Yes}", 3)),
    c("CCAA Fixed Effects", rep("\\multicolumn{1}{c}{Yes}", 3))
    ),
  notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Only municipalities that had at least one street with a Francoist name in $t_{0}$ were included in the sample.}")


# ------------------------------
# Simulation (DiD)

# Extra variables for simulation
extra_vars = data.frame(major_2015_izq = 0,
  lpop2011 = mean(data$lpop2011, na.rm = T),
  l_fs_2016_06 = mean(data$l_fs_2016_06, na.rm = T),
  unemp_2016 = mean(data$unemp_2016, na.rm = T))

# Setting a seed
set.seed(304261)

# Simulating each model and obtaining estimates
sim_VOX = did_sim(m = did_VOX1,
  fs_chg_var = "fs_rm_2016s2_2018s2_bin",
  other_vars = extra_vars, FE = "ccaa")
sim_VOX_est = calculate_did_estimate(depvar_label = "VOX",
  simulations = sim_VOX, elec_t0 = "2016_06", elec_t1 = "2019_04")

sim_PP = did_sim(m = did_PP1,
  fs_chg_var = "fs_rm_2016s2_2018s2_bin",
  other_vars = extra_vars, FE = "ccaa")
sim_PP_est = calculate_did_estimate(depvar_label = "PP",
  simulations = sim_PP, elec_t0 = "2016_06", elec_t1 = "2019_04")

sim_PSOE = did_sim(m = did_PSOE1,
  fs_chg_var = "fs_rm_2016s2_2018s2_bin",
  other_vars = extra_vars, FE = "ccaa")
sim_PSOE_est = calculate_did_estimate(depvar_label = "PSOE",
  simulations = sim_PSOE, elec_t0 = "2016_06", elec_t1 = "2019_04")

# Putting all together
sim = rbind(sim_VOX_est, sim_PP_est, sim_PSOE_est)

# DiD estimates plot
pdf("main_models/output/DiD_estimates.pdf", width = 5.5, height = 3)
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
