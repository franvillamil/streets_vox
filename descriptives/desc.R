# setwd("~/Documents/Projects/streets_vox")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "tidyr", "stargazer", "ggplot2", "stringr",
  "MASS", "forcats", "scales", "rgdal", "rgeos", "maptools")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# muniSpain
if(!"muniSpain" %in% rownames(installed.packages())){
  if(!"devtools" %in% rownames(installed.packages())){install.packages("devtools")}
  library(devtools)
  install_github("franvillamil/muniSpain")
}
# Load
lapply(c(pkg, "muniSpain"), library, character.only = TRUE)

# ------------------------------

# Functions
source("func/capitalize.R")
source("func/ttest_tex.R")
source("func/my_stargazer_glm.R")
source("func/fix_map_ci.R")

# ------------------------------

# Load data
franc_names = readLines("input/calles_franquistas.txt")
changes = read.csv("str_changes/output/changes.csv")
fs_prov_all  = read.csv("str_agg/output/fs_prov.csv")
fs_all = read.csv("str_agg/output/fs_all.csv")
data = read.csv("dataset/output/data.csv")

# ------------------------------
# Descriptives table

covs = data[, c("muni_code", "VOX2019_04", "VOX2016_06",
  "PP2019_04", "PP2016_06", "PSOE2019_04", "PSOE2016_06",
  "fs_rm_2001s2_2018s2_bin", "l_fs_2016_06",
  "part2019_04", "part2016_06", "lpop2011", "major_2015_izq", "unemp_2016")] %>%
  filter(!is.na(VOX2016_06) & l_fs_2016_06 > 0) %>%
  rename(
    "Vox April 2019" = `VOX2019_04`,
    "Vox June 2016" = `VOX2016_06`,
    "PP April 2019" = `PP2019_04`,
    "PP June 2016" = `PP2016_06`,
    "PSOE April 2019" = `PSOE2019_04`,
    "PSOE June 2016" = `PSOE2016_06`,
    "Francoist st name removal, 2016-2018" = `fs_rm_2001s2_2018s2_bin`,
    "Log. Francoist streets, June 2016" = `l_fs_2016_06`,
    "Turnout April 2019" = `part2019_04`,
    "Turnout June 2016" = `part2016_06`,
    "Log. Population 2011" = `lpop2011`,
    "Leftist mayor 2015" = `major_2015_izq`,
    "Unemployment 2016" = `unemp_2016`)

muni_inc = covs$muni_code
covs = covs[, -which(names(covs) == "muni_code")]

# Create table df
descs = vector()
for(i in 1:ncol(covs)){descs = rbind(descs, round(summary(covs[, i]), 2)[1:6])}
descs = as.data.frame(descs)
descs = cbind(names(covs), descs)
names(descs) = c("Variable", "Min", "Q1", "Median", "Mean", "Q3", "Max")

# Write tex table
fileconnection = file("descriptives/output/tab_descriptives.tex")
writeLines(
  paste0(
    "\\begin{table}[!htbp] \\centering", "\n",
    "\\caption{Summary statistics for the covariates}", "\n",
    "\\label{tab:descriptives}", "\n",
    "\\small", "\n",
    paste0("\\begin{tabular}{l", strrep("c", ncol(descs)-1), "}"), "\n",
    "\\\\[-1.8ex]\\hline", "\n",
    "\\hline \\\\[-1.8ex]", "\n",
    "\\\\[-1.8ex]", "\n",
    paste(names(descs), collapse = " & "), " \\\\", "\n",
    "\\hline \\\\[-1.8ex]", "\n",
    paste(descs[1,], collapse = " & "), " \\\\", "\n",
    paste(descs[2,], collapse = " & "), " \\\\", "\n",
    paste(descs[3,], collapse = " & "), " \\\\", "\n",
    paste(descs[4,], collapse = " & "), " \\\\", "\n",
    paste(descs[5,], collapse = " & "), " \\\\", "\n",
    paste(descs[6,], collapse = " & "), " \\\\", "\n",
    paste(descs[7,], collapse = " & "), " \\\\", "\n",
    paste(descs[8,], collapse = " & "), " \\\\", "\n",
    paste(descs[9,], collapse = " & "), " \\\\", "\n",
    paste(descs[10,], collapse = " & "), " \\\\", "\n",
    paste(descs[11,], collapse = " & "), " \\\\", "\n",
    paste(descs[12,], collapse = " & "), " \\\\", "\n",
    paste(descs[13,], collapse = " & "), " \\\\", "\n",
    "\\hline", "\n",
    "\\hline \\\\[-1.8ex]", "\n",
    "\\end{tabular}", "\n",
    "\\end{table}", "\n"
  ), fileconnection)
close(fileconnection)

# ------------------------------
# Municipalities map

# Load shapefile
shp = readOGR("input/ESP_adm4_codes.shp", layer = "ESP_adm4_codes")

# Changing Canary Islands
shpEA = spTransform(shp,CRS("+init=epsg:2163"))
ci = shpEA[shpEA$NAME_2 %in% c("santa cruz de tenerife", "las palmas"),]
ci = fix1(ci, c(-20.5,1,-2.5e05,2e06))
proj4string(ci) = proj4string(shpEA)
shpEA = shpEA[!shpEA$NAME_2 %in% c("santa cruz de tenerife", "las palmas"),]
shpEA = rbind(shpEA, ci)
shp2 = spTransform(shpEA, CRS("+init=epsg:4326"))

# Get whole country
c = gUnaryUnion(shp2)

# Mark municipalities in the sample
shp2$sample = ifelse(shp2@data$muni_code %in% muni_inc, TRUE, FALSE)

# Plot
pdf("descriptives/output/map.pdf", width = 10, height = 8)
# Base map, grey borders
plot(shp2, col = "white", border = "grey", lwd = 0.25)
# Municipalities in the sample
plot(shp2[shp2$sample,], col = gray(0.5), border = "grey", lwd = 0.25, add = TRUE)
# Country profile
plot(c, border = grey(0.5), lwd = 0.5, add = TRUE)
# Canary Islands box
segments(x0 = -1.85, y0 = 34.1, y1 = 36, col = grey(0.5), lwd = 2, lty = "dashed")
segments(x0 = -0.5, x1 = 4, y0 = 36.75, col = grey(0.5), lwd = 2, lty = "dashed")
segments(x0 = -1.85, y0 = 36, x1 = -0.5, y1 = 36.75, col = grey(0.5), lwd = 2, lty = "dashed")
dev.off()

# ------------------------------

# Create list of Francoist street names (for tex)
fn_tex = franc_names[order(franc_names)]
fn_tex = as.character(sapply(fn_tex, function(x) capitalize(x)))
fn_tex = gsub(" De ", " de ", fn_tex)
fn_tex = gsub(" Del ", " del ", fn_tex)
fn_tex = gsub(" La ", " la ", fn_tex)
fn_tex = gsub(" Las ", " las ", fn_tex)
fn_tex = gsub(" Los ", " los ", fn_tex)
fn_tex = gsub(" Y ", " y ", fn_tex)
fn_tex = gsub("Vallejo-nagera", "Vallejo-Nagera", fn_tex)
fn_tex = paste(fn_tex, collapse = "; ")
fc = file("descriptives/output/francoist_name_list.tex")
writeLines(fn_tex, fc)
close(fc)

# ------------------------------

# Frequency table in_sample & treatment

ft = table(data$fs_2016_06 > 0, data$fs_rm_2016s2_2018s2_bin)
pft = round(prop.table(ft, 1)*100,0)

sample_trt = c(
  "\\begin{table}[!htbp] \\centering",
  paste0("\\caption{DiD sample classification}"),
  paste0("\\label{tab:sample_trt}"),
  "\\small",
  "\\begin{tabular}{lcc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  "\\multicolumn{1}{p{3cm}}{\\hspace{3cm} Francoist names} & \\multicolumn{2}{p{3.5cm}}{Removed Francoist names, 2016--2018?}\\\\",
  "in June 2016? & No & Yes \\\\",
  "\\cline{2-3} \\\\[-1.8ex]",
  paste0("No & ", paste(ft[1,], collapse = " & "), " \\\\"),
  paste0(" & ", paste(paste0("(", pft[1,], "\\%)"), collapse = " & "), " \\\\"),
  paste0("Yes (DiD sample) \\hspace{2cm} & ", paste(ft[2,], collapse = " & "), " \\\\"),
  paste0(" & ", paste(paste0("(", pft[2,], "\\%)"), collapse = " & "), " \\\\"),
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  "\\multicolumn{3}{c}{\\parbox[t]{0.55\\textwidth}{\\textit{Note:} Row percentages. Changes in 2016--2018 refer to the period between 01/07/2016 and 31/12/2018.}}\\\\",
  "\\end{tabular}",
  "\\end{table}")

fcon = file("descriptives/output/tab_sample_trt.tex")
writeLines(paste0(sample_trt), fcon)
close(fcon)

# ------------------------------

# Prepare changes dataframe
changes = changes %>%
  mutate(old = tolower(old), new = tolower(new)) %>%
  filter(old %in% franc_names & !new %in% franc_names) %>%
  mutate(date = as.Date(as.character(new_date), format = "%Y%m%d")) %>%
  mutate(year = as.integer(format(date, "%Y")))

# Province-level changes
changes_prov = changes %>%
  group_by(prov) %>%
  summarize(
    changes_2001_2020 = length(cod_via),
    changes_2016_2018 = length(cod_via[date >= "2016-06-30" & date <= "2018-12-31"]),
    changes_2011_2016 = length(cod_via[date > "2010-12-31" & date < "2016-06-30"])) %>%
  pivot_longer(
    cols = starts_with("changes"),
    names_to = "period",
    values_to = "changes") %>%
  mutate(period = gsub("changes_", "", period)) %>%
  mutate(period = gsub("_", " - ", period)) %>%
  as.data.frame()
# Add provinces that did not have any changes
prov0 = code_to_prov(1:52)[!code_to_prov(1:52) %in% changes$prov]
changes_prov = rbind(changes_prov,
  data.frame(
    prov = rep(prov0, each = length(unique(changes_prov$period))),
    period = rep(unique(changes_prov$period), length(prov0)),
    changes = 0))
changes_prov = changes_prov[order(changes_prov$prov),]
# Capitalize province names
changes_prov$prov = sapply(changes_prov$prov, function(x) capitalize(x))
changes_prov$prov = factor(changes_prov$prov)
changes_prov$prov = fct_relevel(changes_prov$prov,
  levels(changes_prov$prov)[
    order(changes_prov$changes[changes_prov$period == "2001 - 2020"])])

# Create share var, limit to periods and change time var
fs_prov = fs_prov_all %>%
  mutate(fs_sh = f_streets/total_streets) %>%
  filter(fecha %in% c("2001_06", "2010_12", "2016_06")) %>%
  mutate(fecha = as.Date(paste0(fecha, "_30"), "%Y_%m_%d"))
# Reorder fecha variable
fs_prov$fecha = factor(format(fs_prov$fecha, "%B %Y"))
fs_prov$fecha = fct_relevel(fs_prov$fecha, levels(fs_prov$fecha)[
  order(as.integer(gsub(".*(\\d{4})$", "\\1", levels(fs_prov$fecha))))])
# Capitalize and reorder prov variable
fs_prov$prov = factor(sapply(fs_prov$prov, function(x) capitalize(x)))
fs_prov$prov = fct_relevel(fs_prov$prov,
  levels(fs_prov$prov)[order(fs_prov$fs_sh[fs_prov$fecha == "June 2001"])])

# Create year-based changes data
fs_year = fs_all %>%
  filter(grepl("_06", fecha)) %>%
  mutate(year = as.integer(gsub("_06", "", fecha))) %>%
  mutate(fs_sh = f_streets/total_streets)

# ------------------------------

pdf("descriptives/output/changes_by_year.pdf", width = 7, height = 2.5)
  ggplot(subset(changes, date != "2010-12-31"), aes(x = date)) +
    geom_histogram(binwidth = 30*6, color = "white") +
    scale_x_date(labels = date_format("%Y"), breaks = "1 year") +
    theme_classic() +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(hjust = -0.25),
          panel.border = element_blank(),
          strip.text = element_text(size = 12),
          plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
          strip.background = element_blank()) +
    labs(x = "", y = "Francoist street name removals")
dev.off()

pdf("descriptives/output/fs_by_year.pdf", width = 6, height = 3)
  ggplot(fs_year, aes(x = factor(year), y = fs_sh)) +
    geom_bar(stat = "identity") +
    theme_classic() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      strip.text = element_text(colour = "grey30", size = 10),
      strip.background = element_blank()) +
    labs(y = "Share of Francoist streets", x = "")
dev.off()


pdf("descriptives/output/changes_by_prov.pdf", width = 8, height = 8)
ggplot(changes_prov, aes(x = prov, y = changes)) +
  geom_bar(stat = "identity") +
  facet_wrap(~period, ncol = 3, scales = "free_x") +
  theme_bw() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.text = element_text(colour = "grey30", size = 10),
    strip.background = element_blank()) +
  labs(y = "\nFrancoist street name removals", x = "")
dev.off()

pdf("descriptives/output/fs_by_prov.pdf", width = 8, height = 8)
ggplot(fs_prov, aes(x = prov, y = fs_sh)) +
  geom_bar(stat = "identity") +
  facet_wrap(~fecha, ncol = 3, scales = "free_x") +
  theme_bw() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.text = element_text(colour = "grey30", size = 10),
    strip.background = element_blank()) +
  labs(y = "\nShare of Francoist streets", x = "")
dev.off()

# ------------------------------

# In-sample vs out-sample tables & glm

# Get rid of observations with missing data on covariates
vars = c("fs_rm_2016s2_2018s2_bin", "fs_2016_06",
  "major_2015_izq", "lpop2011", "l_fs_2016_06", "unemp_2016")
sample_did = data[!(apply(data[, vars], 1, function(x) sum(is.na(x))) > 0), ]

# Create in/out sample variable, treatment/control
sample_did$insample = ifelse(sample_did$fs_2016_06 >= 1, TRUE, FALSE)
sample_did$insample2001 = ifelse(sample_did$fs_2001_06 >= 1, TRUE, FALSE)
sample_did$treatment = ifelse(sample_did$fs_rm_2016s2_2018s2_bin == 1, TRUE, FALSE)
sample_did$treatment = ifelse(!sample_did$insample, NA, sample_did$treatment)

# Table with t-tests
ttest_table = tex_append(
  title = "Mean comparison municipalities in/out of sample (with/without Francoist street names in June 2016)",
  label = "tab:ttest_sample",
  midlines = c("& \\multicolumn{4}{c}{April 2019}\\\\",
  ttest_to_tex(with(sample_did, t.test(PP2019_04[insample], PP2019_04[!insample]))),
  ttest_to_tex(with(sample_did, t.test(PSOE2019_04[insample], PSOE2019_04[!insample]))),
  ttest_to_tex(with(sample_did, t.test(VOX2019_04[insample], VOX2019_04[!insample]))),
  "\\hline \\\\[-1.8ex]",
  "& \\multicolumn{4}{c}{June 2016}\\\\",
  ttest_to_tex(with(sample_did, t.test(PP2016_06[insample], PP2016_06[!insample]))),
  ttest_to_tex(with(sample_did, t.test(PSOE2016_06[insample], PSOE2016_06[!insample]))),
  ttest_to_tex(with(sample_did, t.test(VOX2016_06[insample], VOX2016_06[!insample]))),
  "\\hline \\\\[-1.8ex]",
  "& \\multicolumn{4}{c}{December 2015}\\\\",
  ttest_to_tex(with(sample_did, t.test(PP2015_12[insample], PP2015_12[!insample]))),
  ttest_to_tex(with(sample_did, t.test(PSOE2015_12[insample], PSOE2015_12[!insample]))),
  ttest_to_tex(with(sample_did, t.test(VOX2015_12[insample], VOX2015_12[!insample]))),
  "\\hline \\\\[-1.8ex]",
  "& \\multicolumn{4}{c}{November 2011}\\\\",
  ttest_to_tex(with(sample_did, t.test(PP2011_11[insample], PP2011_11[!insample]))),
  ttest_to_tex(with(sample_did, t.test(PSOE2011_11[insample], PSOE2011_11[!insample])))))

fcon = file("descriptives/output/ttest_sample_fs2016.tex")
writeLines(paste0(ttest_table), fcon)
close(fcon)

# Table with t-tests
ttest_table2001 = tex_append(
  title = "Mean comparison municipalities with/without Francoist street names in June 2001",
  label = "tab:ttest_sample2001",
  midlines = c("& \\multicolumn{4}{c}{April 2019}\\\\",
  ttest_to_tex(with(sample_did, t.test(PP2019_04[insample2001], PP2019_04[!insample2001]))),
  ttest_to_tex(with(sample_did, t.test(PSOE2019_04[insample2001], PSOE2019_04[!insample2001]))),
  ttest_to_tex(with(sample_did, t.test(VOX2019_04[insample2001], VOX2019_04[!insample2001]))),
  "\\hline \\\\[-1.8ex]",
  "& \\multicolumn{4}{c}{June 2016}\\\\",
  ttest_to_tex(with(sample_did, t.test(PP2016_06[insample2001], PP2016_06[!insample2001]))),
  ttest_to_tex(with(sample_did, t.test(PSOE2016_06[insample2001], PSOE2016_06[!insample2001]))),
  ttest_to_tex(with(sample_did, t.test(VOX2016_06[insample2001], VOX2016_06[!insample2001]))),
  "\\hline \\\\[-1.8ex]",
  "& \\multicolumn{4}{c}{December 2015}\\\\",
  ttest_to_tex(with(sample_did, t.test(PP2015_12[insample2001], PP2015_12[!insample2001]))),
  ttest_to_tex(with(sample_did, t.test(PSOE2015_12[insample2001], PSOE2015_12[!insample2001]))),
  ttest_to_tex(with(sample_did, t.test(VOX2015_12[insample2001], VOX2015_12[!insample2001]))),
  "\\hline \\\\[-1.8ex]",
  "& \\multicolumn{4}{c}{November 2011}\\\\",
  ttest_to_tex(with(sample_did, t.test(PP2011_11[insample2001], PP2011_11[!insample2001]))),
  ttest_to_tex(with(sample_did, t.test(PSOE2011_11[insample2001], PSOE2011_11[!insample2001]))),
  "\\hline \\\\[-1.8ex]",
  "& \\multicolumn{4}{c}{March 2008}\\\\",
  ttest_to_tex(with(sample_did, t.test(PP2008_03[insample2001], PP2008_03[!insample2001]))),
  ttest_to_tex(with(sample_did, t.test(PSOE2008_03[insample2001], PSOE2008_03[!insample2001]))),
  "\\hline \\\\[-1.8ex]",
  "& \\multicolumn{4}{c}{March 2004}\\\\",
  ttest_to_tex(with(sample_did, t.test(PP2004_03[insample2001], PP2004_03[!insample2001]))),
  ttest_to_tex(with(sample_did, t.test(PSOE2004_03[insample2001], PSOE2004_03[!insample2001]))),
  "\\hline \\\\[-1.8ex]",
  "& \\multicolumn{4}{c}{March 2000}\\\\",
  ttest_to_tex(with(sample_did, t.test(PP2000_03[insample2001], PP2000_03[!insample2001]))),
  ttest_to_tex(with(sample_did, t.test(PSOE2000_03[insample2001], PSOE2000_03[!insample2001])))
))

fcon = file("descriptives/output/ttest_sample_fs2001.tex")
writeLines(paste0(ttest_table2001), fcon)
close(fcon)

# Logit regression
sm00 = glm(insample ~ PP2000_03 + PSOE2000_03 + lpop2011 +
  factor(ccaa), data = sample_did)
sm04 = glm(insample ~ PP2004_03 + PSOE2004_03 + lpop2011 +
  factor(ccaa), data = sample_did)
sm08 = glm(insample ~ PP2008_03 + PSOE2008_03 + lpop2011 +
  factor(ccaa), data = sample_did)
sm11 = glm(insample ~ PP2011_11 + PSOE2011_11 + lpop2011 +
  factor(ccaa), data = sample_did)
sm15 = glm(insample ~ PP2015_12 + PSOE2015_12 + lpop2011 +
  factor(ccaa), data = sample_did)
sm16 = glm(insample ~ PP2016_06 + PSOE2016_06 + lpop2011 +
  factor(ccaa), data = sample_did)

sm00_2001 = glm(insample2001 ~ PP2000_03 + PSOE2000_03 + lpop2011 +
  factor(ccaa), data = sample_did)
sm04_2001 = glm(insample2001 ~ PP2004_03 + PSOE2004_03 + lpop2011 +
  factor(ccaa), data = sample_did)
sm08_2001 = glm(insample2001 ~ PP2008_03 + PSOE2008_03 + lpop2011 +
  factor(ccaa), data = sample_did)
sm11_2001 = glm(insample2001 ~ PP2011_11 + PSOE2011_11 + lpop2011 +
  factor(ccaa), data = sample_did)
sm15_2001 = glm(insample2001 ~ PP2015_12 + PSOE2015_12 + lpop2011 +
  factor(ccaa), data = sample_did)
sm16_2001 = glm(insample2001 ~ PP2016_06 + PSOE2016_06 + lpop2011 +
  factor(ccaa), data = sample_did)

my_stargazer_glm(dest_file = "descriptives/output/tab_insample.tex",
  model_list = list(sm00, sm04, sm08, sm11, sm15, sm16),
  title = "Voting for PP/PSOE and having a Francoist street name in June 2016",
  label = "tab:insample",
  order = "Constant",
  covariate.labels = c("(Intercept)",
    "PP (2000/03)", "PSOE (2000/03)",
    "PP (2004/03)", "PSOE (2004/03)",
    "PP (2008/03)", "PSOE (2008/03)",
    "PP (2011/11)", "PSOE (2011/11)",
    "PP (2015/12)", "PSOE (2015/12)",
    "PP (2016/06)", "PSOE (2016/06)",
    "Log. Pop 2011"),
  notes_table = "\\parbox[t]{0.6\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$.}")

my_stargazer_glm(dest_file = "descriptives/output/tab_insample2001.tex",
  model_list = list(sm00_2001, sm04_2001, sm08_2001,
    sm11_2001, sm15_2001, sm16_2001),
  title = "Voting for PP/PSOE and having a Francoist street name in June 2001",
  label = "tab:insample2001",
  order = "Constant",
  covariate.labels = c("(Intercept)",
    "PP (2000/03)", "PSOE (2000/03)",
    "PP (2004/03)", "PSOE (2004/03)",
    "PP (2008/03)", "PSOE (2008/03)",
    "PP (2011/11)", "PSOE (2011/11)",
    "PP (2015/12)", "PSOE (2015/12)",
    "PP (2016/06)", "PSOE (2016/06)",
    "Log. Pop 2011"),
  notes_table = "\\parbox[t]{0.6\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$.}")

# ------------------------------

# Treatment strength among treated

pdf("descriptives/output/trt_strength.pdf", width = 5, height = 5)
  ggplot(subset(sample_did, treatment),
      aes(x = fs_rm_2016s2_2018s2)) +
    geom_histogram(binwidth = 2, color = "white") +
    theme_classic() +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(hjust = -0.25),
          panel.border = element_blank(),
          strip.text = element_text(size = 12),
          plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
          strip.background = element_blank()) +
    labs(y = "",
      x = "Francoist street names removed, Jun 2016 - Dec 2018")
dev.off()

pdf("descriptives/output/trt_remaining.pdf", width = 5, height = 5)
  ggplot(subset(sample_did, treatment),
      aes(x = fs_2016_06 - fs_rm_2016s2_2018s2)) +
    geom_histogram(binwidth = 1, color = "white") +
    theme_classic() +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(hjust = -0.25),
          panel.border = element_blank(),
          strip.text = element_text(size = 12),
          plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
          strip.background = element_blank()) +
    labs(y = "",
      x = "Remaining Francoist street names, Dec 2018")
dev.off()

pdf("descriptives/output/trt_strength_st2016.pdf", width = 7.5, height = 5)
  ggplot(subset(sample_did, treatment & muni_code != 28079),
      aes(x = fs_2016_06, y = fs_rm_2016s2_2018s2)) +
    # geom_point(alpha = 0.25, size = 3, colour = "black") +
    geom_bin2d(binwidth = 1) +
    scale_fill_gradient(low = gray(0.9), high = gray(0)) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      strip.text = element_text(colour = "grey30", size = 10),
      strip.background = element_blank()) +
    labs(x = "Number of Francoist street names, June 2016\n(excl. city of Madrid)",
      y = "Francoist street names removed\n(June 2016 - December 2018)",
      fill = "")
dev.off()


# Print mean number of streets removed
trt_mean = mean(sample_did$fs_rm_2016s2_2018s2[sample_did$fs_rm_2016s2_2018s2>0])
trt_sd = sd(sample_did$fs_rm_2016s2_2018s2[sample_did$fs_rm_2016s2_2018s2>0])
fc = file("descriptives/output/mean_trt_treated.tex")
writeLines(paste0(round(trt_mean, 2), " (SD = ", round(trt_sd, 2), ")"), fc)
close(fc)

# ------------------------------

# Create long-form data to plot
sample_did_l = sample_did
names(sample_did_l) = gsub("(PP|PSOE|VOX)(\\d+)_(\\d+)", "\\1_\\2\\3", names(sample_did_l))

sample_did_l = sample_did_l %>%
  pivot_longer(cols = matches("(PP|PSOE|VOX)_(\\d+)"),
    names_to = c("party", "election"), names_sep= "[_]") %>%
# Tip: try puting ".value" instead of "election"
# see https://stackoverflow.com/questions/60083062
  mutate(election_date = as.Date(gsub("(\\d{4})(\\d{2})", "\\1-\\2-01", election))) %>%
  mutate(election_date = format(election_date, "%Y/%m")) %>%
  as.data.frame

# National means
trends = sample_did_l %>%
  filter(insample) %>%
  group_by(party, election_date, treatment) %>%
  summarize(value = mean(value, na.rm = TRUE)) %>%
  group_by(party, treatment) %>%
  mutate(n_value = value - value[election_date == "2016/06"]) %>%
  as.data.frame()

trends$election_date = factor(trends$election_date)
trends$treatment = factor(trends$treatment)
levels(trends$treatment) = c("Control", "Treatment (rm Francoist street names)")

# # Plots of parallel trends
# pdf("descriptives/output/par_trends.pdf", width = 10, height = 4)
# ggplot(trends, aes(x = election_date, y = value, group = treatment)) +
#   geom_line(aes(linetype = treatment)) +
#   facet_wrap(~party, scales = "free_y", ncol = 2) +
#   theme_bw() +
#   theme(
#     legend.position = c(0.85, 0),
#     legend.justification = c(1, 0),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.background = element_blank(),
#     panel.border = element_rect(colour = "black", fill = NA),
#     strip.text = element_text(colour = "grey30", size = 12),
#     strip.background = element_blank()) +
#   labs(x = "", y = "Mean electoral share", linetype = "Group in DiD models")
# dev.off()

pdf("descriptives/output/par_trends_norm.pdf", width = 8.5, height = 3)
ggplot(subset(trends, party != "PSOE" &
      as.integer(str_sub(election_date, 1, 4)) > 2010),
    aes(x = election_date, y = n_value, group = treatment)) +
  geom_line(aes(linetype = treatment)) +
  facet_wrap(~party, scales = "free", ncol = 2) +
  geom_vline(xintercept = "2016/06", linetype = "dotted") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_blank(),
    # panel.border = element_rect(colour = "black", fill = NA),
    strip.text = element_text(colour = "grey30", size = 12),
    strip.background = element_blank()) +
  labs(x = "", y = "Mean electoral share\n(Group-normalized, June 2016)",
    linetype = "Group in DiD models")
dev.off()
