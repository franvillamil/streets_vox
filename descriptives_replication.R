## Replication "Do TJ policies cause backlash? Evidence from street name changes in Spain"
## January 2021
## File: DESCRIPTIVE GRAPHS
## ---------------------------------------

if(!grepl("replication$", getwd())){
  print("Choose any file in the replication folder (e.g. analyses.R)")
  dir = file.choose()
  setwd(gsub("replication(/.*)$", "replication", dir))
}

options(stringsAsFactors = FALSE)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)

# Function to make First Letter capital
capitalize = function(str){
  c = strsplit(str, " ")[[1]]
  out = paste(toupper(substring(c, 1,1)), substring(c, 2), sep="", collapse=" ")
  return(out)
  }

## LOAD ##

franc_names = readLines("extra/calles_franquistas.txt")
changes = read.csv("extra/changes.csv")
fs_prov = read.csv("extra/fstreets_prov.csv")
fs_year  = read.csv("extra/fstreets_share_prov.csv")

## PREPARE ##

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
changes_prov$prov = sapply(changes_prov$prov, function(x) capitalize(x))
changes_prov$prov = factor(changes_prov$prov)
changes_prov$prov = fct_relevel(changes_prov$prov,
  levels(changes_prov$prov)[
    order(changes_prov$changes[changes_prov$period == "2001 - 2020"])])

# Prepare prov-level data on streets for plotting
fs_prov$prov = sapply(fs_prov$prov, function(x) capitalize(x))
# Reorder prov variable
fs_prov$prov = factor(fs_prov$prov)
fs_prov$prov = fct_relevel(fs_prov$prov,
  levels(fs_prov$prov)[
    order(fs_prov$fs_sh[fs_prov$fecha == "June 2001"])])

## GRAPHS ##

pdf("figure1.pdf", width = 7, height = 2.5)
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

pdf("figure2.pdf", width = 6, height = 3)
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


pdf("figureA1.pdf", width = 8, height = 8)
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

pdf("figureA2.pdf", width = 8, height = 8)
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
