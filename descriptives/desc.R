# setwd("~/Documents/Projects/streets_vox")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "tidyr", "stargazer", "ggplot2", "stringr",
  "MASS", "forcats", "scales", "ggpubr")
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

# Function to make First Letter capital
capitalize = function(str){
  c = strsplit(str, " ")[[1]]
  out = paste(toupper(substring(c, 1,1)), substring(c, 2), sep="", collapse=" ")
  return(out)
  }

ttest_to_tex = function(ttout){
  ins = paste0(round(ttout$estimate[1] * 100, 2), "\\%")
  outs = paste0(round(ttout$estimate[2] * 100, 2), "\\%")
  pv = sprintf("%0.3f", ttout$p.value)
  if(ttout$p.value < 0.05){pv = paste0(pv, "*")}
  if(ttout$p.value < 0.01){pv = paste0(pv, "*")}
  if(ttout$p.value < 0.001){pv = paste0(pv, "*")}
  diff = round(ttout$estimate[1] * 100 - ttout$estimate[2] * 100, 2)
  party = gsub("^([A-Z]*)\\d+_\\d+.*", "\\1", ttout$data.name)
  row = paste0(party, " & ", ins, " & ", outs, " & ", diff, " & ", pv, " \\\\")
  return(row)
}

tex_append = function(title, label, midlines){

  preamble = c(
    "\\begin{table}[!htbp] \\centering",
    paste0("\\caption{", title, "}"),
    paste0("\\label{", label, "}"),
    "\\small",
    "\\begin{tabular}{lcccc}",
    "\\\\[-1.8ex]\\hline",
    "\\hline \\\\[-1.8ex]",
    "\\\\[-1.8ex]",
    "Party & In sample & Out of sample & Diff In-Out (\\%) & P-value \\\\",
    "\\hline \\\\[-1.8ex]")

  end = c("\\hline",
    "\\hline \\\\[-1.8ex]",
    "\\multicolumn{5}{c}{\\parbox[t]{0.65\\textwidth}{\\textit{Note:} + $p<0.1$; * $p<0.05$; ** $p<0.01$; *** $p<0.001$.}}\\\\",
    "\\end{tabular}",
    "\\end{table}")

  return(c(preamble, midlines, end))

}


# ------------------------------

# Load data
franc_names = readLines("input/calles_franquistas.txt")
changes = read.csv("str_changes/output/changes.csv")
fs_prov_all  = read.csv("str_agg/output/fs_prov.csv")
fs_all = read.csv("str_agg/output/fs_all.csv")
data = read.csv("dataset/output/data.csv")

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
writeLines(fn_tex, con = file("descriptives/output/francoist_name_list.txt"))

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

# In-sample vs out-sample

# Get rid of observations with missing data on covariates
vars = c("fs_rm_2016s2_2018s2_bin", "fs_2016_06",
  "major_2015_izq", "lpop2011", "l_fs_2016_06", "unemp_2016")
sample_did = data[!(apply(data[, vars], 1, function(x) sum(is.na(x))) > 0), ]

# Create in/out sample variable, treatment/control
sample_did$insample = ifelse(sample_did$fs_2016_06 >= 1, TRUE, FALSE)
sample_did$treatment = ifelse(sample_did$fs_rm_2016s2_2018s2_bin == 1, TRUE, FALSE)
sample_did$treatment = ifelse(!sample_did$insample, NA, sample_did$treatment)

# Table with t-tests
ttest_table = tex_append(
  title = "Mean comparison municipalities in/out of sample",
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

# Logit regression
sm1 = glm(insample ~ PP2011_11 + PSOE2011_11 + lpop2011 +
  factor(ccaa), data = sample_did)
sm2 = glm(insample ~ PP2015_12 + PSOE2015_12 + lpop2011 +
  factor(ccaa), data = sample_did)
sm3 = glm(insample ~ PP2016_06 + PSOE2016_06 + lpop2011 +
  factor(ccaa), data = sample_did)

stargazer(sm1,sm2,sm3, type="text",omit="ccaa")

# # Create long-form data to plot
# sample_did_l = sample_did
# names(sample_did_l) = gsub("(PP|PSOE|VOX)(\\d+)_(\\d+)", "\\1_\\2\\3", names(sample_did_l))
#
# sample_did_l = sample_did_l %>%
#   pivot_longer(cols = matches("(PP|PSOE|VOX)_(\\d+)"),
#     names_to = c("party", "election"), names_sep= "[_]") %>%
# # Tip: try puting ".value" instead of "election"
# # see https://stackoverflow.com/questions/60083062
#   mutate(election2 = as.Date(gsub("(\\d{4})(\\d{2})", "\\1-\\2-01", election))) %>%
#   mutate(election2 = format(election2, "%b %Y")) %>%
#   as.data.frame
