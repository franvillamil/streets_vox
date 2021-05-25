# setwd("~/Documents/Projects/streets_vox")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "tidyr", "stargazer", "ggplot2", "stringr",
  "MASS", "forcats", "scales")
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

# ------------------------------

# Load data
franc_names = readLines("input/calles_franquistas.txt")
changes = read.csv("str_changes/output/changes.csv")
fs_prov_all  = read.csv("str_agg/output/fs_prov.csv")
fs_all = read.csv("str_agg/output/fs_all.csv")

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
