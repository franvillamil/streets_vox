## NOTE TODO: REHACER ESTO, Y DE PASO THE WHOLE FOLDER!!!

setwd("~/Google Drive/Academic/DATA/Spain/street_names")
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

## PREPARE DATA ## --------------------------------------------------

# List of Francoist street names
franc_names = readLines("output/calles_franquistas.txt")

# Changes
changes = rbind(read.csv("output/changes_6month.csv"),
  read.csv("output/changes_2001_2011.csv"))
changes$old = tolower(changes$old)
changes$new = tolower(changes$new)
changes_f = subset(changes, old %in% franc_names & !new %in% franc_names)

# Date variable
changes_f$date = as.Date(as.character(changes_f$new_date), format = "%Y%m%d")
changes_f$year = as.integer(format(changes_f$date, "%Y"))

# Province-level data
changes_f_prov = changes_f %>%
  group_by(prov) %>%
  summarize(
    changes_2001_2020 = length(cod_via),
    # changes_2011_2020 = length(cod_via[date > "2010-12-31"]),
    changes_2016_2018 = length(cod_via[date >= "2016-06-30" & date <= "2018-12-31"]),
    changes_2011_2016 = length(cod_via[date > "2010-12-31" & date < "2016-06-30"])) %>%
  pivot_longer(
    cols = starts_with("changes"),
    names_to = "period",
    values_to = "changes") %>%
  mutate(period = gsub("changes_", "", period)) %>%
  mutate(period = gsub("_", " - ", period)) %>%
  as.data.frame()
# Prov name
changes_f_prov$prov = sapply(changes_f_prov$prov, function(x) capitalize(x))
# Reorder prov variable
changes_f_prov$prov = factor(changes_f_prov$prov)
changes_f_prov$prov = fct_relevel(changes_f_prov$prov,
  levels(changes_f_prov$prov)[
    order(changes_f_prov$changes[changes_f_prov$period == "2001 - 2020"])])

# Full data on streets
f = list.files("callejero_clean")
f = paste0("callejero_clean/", f)
df_list = lapply(f, function(x) read.csv(x))
if( any(sapply(df_list, function(x) ncol(x)) != 8) ){warning("problem with ncol!")}
# Merge, clean and prepare
calles = as.data.frame(do.call("rbind", df_list))
calles = calles[, c("prov", "muni", "fecha", "nombre_via")]
calles$nombre_via = tolower(calles$nombre_via)
# By province
fs_prov = calles %>%
  filter(fecha %in% c("20010630", "20101231", "20160630")) %>%
  mutate(fecha = recode(factor(fecha),
    `20010630` = "June 2001",
    `20101231` = "January 2010",
    `20160630` = "June 2016")) %>%
  group_by(prov, fecha) %>%
  summarize(
    total_streets = length(nombre_via),
    fs = length(nombre_via[nombre_via %in% franc_names])) %>%
  mutate(fs_sh = fs / total_streets) %>%
  as.data.frame()
fs_prov$prov = sapply(fs_prov$prov, function(x) capitalize(x))
# Reorder prov variable
fs_prov$prov = factor(fs_prov$prov)
fs_prov$prov = fct_relevel(fs_prov$prov,
  levels(fs_prov$prov)[
    order(fs_prov$fs_sh[fs_prov$fecha == "June 2001"])])

# Overall
fs_year = calles %>%
  filter(grepl("0630", fecha)) %>%
  group_by(fecha) %>%
  summarize(
    total_streets = length(nombre_via),
    fs = length(nombre_via[nombre_via %in% franc_names])) %>%
  mutate(fs_sh = fs / total_streets) %>%
  mutate(year = str_sub(fecha, 1, 4)) %>%
  as.data.frame()


## GRAPHS ## --------------------------------------------------------

pdf("changes_over_time.pdf", width = 7, height = 2.5)
ggplot(subset(changes_f, date != "2010-12-31"), aes(x = date)) +
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

pdf("changes_by_prov.pdf", width = 8, height = 8)
ggplot(changes_f_prov, aes(x = prov, y = changes)) +
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

pdf("fs_by_prov.pdf", width = 8, height = 8)
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

pdf("fs_year.pdf", width = 6, height = 3)
ggplot(fs_year, aes(x = year, y = fs_sh)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    # panel.border = element_rect(colour = "black", fill = NA),
    strip.text = element_text(colour = "grey30", size = 10),
    strip.background = element_blank()) +
  labs(y = "Share of Francoist streets", x = "")
dev.off()














## Callejeros ----------------

# List and load
f = list.files("callejero_clean")
f = paste0("callejero_clean/", f)
df_list = lapply(f, function(x) read.csv(x))
if( any(sapply(df_list, function(x) ncol(x)) != 8) ){warning("problem with ncol!")}
# Merge, clean and prepare
calles = as.data.frame(do.call("rbind", df_list))
calles = calles[, c("prov", "muni", "fecha", "nombre_via")]
calles$nombre_via = tolower(calles$nombre_via)

## Get number of streets and Francoist streets by muni and date
calles$fecha = paste(
  str_sub(as.character(calles$fecha), 1, 4),
  str_sub(as.character(calles$fecha), 5, 6), sep = "_")
fc_muni = calles %>%
  group_by(muni, fecha) %>%
  summarize(
    total_streets = length(nombre_via),
    f_streets = length(nombre_via[nombre_via %in% franc_names])) %>%
  # To wide form
  pivot_wider(-total_streets,
    names_from = "fecha",
    values_from = "f_streets",
    names_prefix = "fs_") %>%
  as.data.frame()
head(fc_muni)


## (viene de street number)
## GRAPHS / DESCRIPTIVES ------------------------------

fc_muni_sum = fc_muni %>%
  group_by(fecha) %>%
  summarize(total_fc = sum(f_streets), total_fc_sh = sum(f_streets)/sum(total_streets)*100)
fc_muni_sum$date = as.Date(as.character(fc_muni_sum$fecha), "%Y%m%d")
# fc_muni_sum$date = format(fc_muni_sum$date, "%Y/%m")
pdf("fstreets.pdf", width = 10, height = 3)
ggplot(fc_muni_sum, aes(x = factor(date), y = total_fc_sh)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(panel.background = element_blank(),
        legend.position = c(0, 1.03), legend.justification = c(0,1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  labs(x = "", y = "Street with Francoist names (%)\n") +
  scale_x_discrete(labels = c(
    "2001-06-30" = "June\n2001",
    "2010-12-31" = "2011",
    "2011-06-30" = "",
    "2011-12-31" = "2012",
    "2012-06-30" = "",
    "2012-12-31" = "2013",
    "2013-06-30" = "",
    "2013-12-31" = "2014",
    "2014-06-30" = "",
    "2014-12-31" = "2015",
    "2015-06-30" = "",
    "2015-12-31" = "2016",
    "2016-06-30" = "",
    "2016-12-31" = "2017",
    "2017-06-30" = "",
    "2017-12-31" = "2018",
    "2018-06-30" = "",
    "2018-12-31" = "2019",
    "2019-06-30" = "",
    "2019-12-31" = "2020",
    "2020-06-30" = ""))
dev.off()


## (viene de changes)
## GRAPHS / DESCRIPTIVES ------------------------------



# # Add population data
# census = read.csv("../census/INE_census.csv")
# census = subset(census, !is.na(c2011), select = c("muni_code", "prov_name", "c2011"))
# names(census)[names(census) == "muni_code"] = "muni"
# names(census)[names(census) == "prov_name"] = "prov"
# c_chg_m = merge(c_chg_m, census, all.x = TRUE)
# c_chg_m$l_pop2011 = log(c_chg_m$c2011)
#
# ggplot(c_chg_m, aes(x = l_pop2011, y = changes)) + geom_point()
# ggplot(subset(c_chg_m, muni != 28079), aes(x = l_pop2011, y = changes)) + geom_point()










# List of Francoist street names
cf = readLines("output/calles_franquistas.txt")

# Changes - subset of calles franquistas
c = rbind(read.csv("output/changes_6month.csv"),
  read.csv("output/changes_2001_2011.csv"))
c$old = tolower(c$old)
c$new = tolower(c$new)
c_chg = subset(c, old %in% cf & !new %in% cf)
c_reversal = subset(c, new %in% cf & !old %in% cf)

# Date variable
c_chg$date = as.Date(as.character(c_chg$new_date), format = "%Y%m%d")

# At the level of municipalities
c_chg_m = c_chg %>%
  group_by(muni) %>%
  summarize(
    # Long-term trends
    franame_rm_2001_2010 = length(cod_via[date == "2010-12-31"]),
    # DiD 2015-2019 (and extra coding for delays in INE inclusion)
    franame_rm_2015s2_2018_s2 = length(cod_via[date >= "2015-12-31" & date <= "2018-12-31"]),
    franame_rm_2015s2_2019_s1 = length(cod_via[date >= "2015-12-31" & date <= "2019-06-30"]),
    # DiD Apr-Nov2019 (and extra coding for delays in INE inclusion)
    franame_rm_2019 = length(cod_via[date %in% as.Date(c("2019-06-30", "2019-12-31"))]),
    franame_rm_2019s2 = length(cod_via[date == "2019-12-31"]),
    franame_rm_2019s2_2020s1 = length(cod_via[date %in% as.Date(c("2020-06-30", "2019-12-31"))])) %>%
  as.data.frame()

write.csv(c_chg_m, "output/changes.csv", row.names = FALSE)
