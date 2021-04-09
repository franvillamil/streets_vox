setwd("~/Google Drive/Academic")
options(stringsAsFactors = FALSE)
library(muniSpain)
library(dplyr)
library(tidyr)
library(stringr)

## BASE DATASET -----------------------------------------------------

# Provinces included
prov_incl = c("albacete", "badajoz", "asturias",
  "lugo", "ourense", "a coruna", "pontevedra",
  "bizkaia", "gipuzkoa", "alava",
  "girona", "tarragona", "lleida", "barcelona",
  "huesca", "zaragoza", "teruel")

# Load
census = subset(read.csv("DATA/Spain/census/INE_census.csv"),
  prov_name %in% prov_incl,
  select = c("prov_name", "muni_code", "muni_name", "c1930", "c2011"))
# Adapt muni codes to 1930-2011 changes
census$muni_code_old = census$muni_code
census$muni_code = as.integer(changes_newcode(census$muni_code_old, 1930, 2011))
# Create dataset
data = census %>%
  filter(!is.na(muni_code)) %>%
  group_by(muni_code) %>%
  summarize(
    pop1930 = sum(c1930, na.rm = TRUE),
    pop2011 = sum(c2011, na.rm = TRUE),
    prov_name = unique(prov_name),
    muni_name = paste(unique(muni_name), collapse=";")) %>%
  as.data.frame()
# Add region
data$ccaa = prov_to_ccaa(data$prov_name)

## ELECTORAL DATA ---------------------------------------------------

e19a = read.csv("DATA/Spain/elections/clean/2019_04.csv")
# unique(e19a$siglas)[grepl("PP", unique(e19a$siglas))]
e19a$siglas[e19a$siglas %in% c("PP", "PP-FORO")] = "PP"
e19a = subset(e19a, siglas %in% c("PP", "VOX"),
  select = c("muni_code", "votantes", "censo", "validos", "siglas", "votos"))
e19a = e19a %>%
  pivot_wider(names_from = "siglas", values_from = "votos") %>%
  as.data.frame()
# Municipality code changes
e19a$muni_code = as.integer(changes_newcode(e19a$muni_code, 1930, 2011))
# Re-add with updated municipality codes
e19a = e19a %>% group_by(muni_code) %>%
  summarize_if(is.integer, sum) %>%
  filter(!is.na(muni_code)) %>%
  as.data.frame()
# Calculating
e19a$PP = e19a$PP / e19a$validos
e19a$VOX = e19a$VOX / e19a$validos
names(e19a)[names(e19a) == "PP"] = "PP201904"
names(e19a)[names(e19a) == "VOX"] = "VOX201904"
e19a$part201904 = e19a$votantes / e19a$censo

e19n = read.csv("DATA/Spain/elections/clean/2019_11.csv")
# unique(e19n$siglas)[grepl("PP", unique(e19n$siglas))]
e19n$siglas[e19n$siglas %in% c("PP", "PP-FORO")] = "PP"
e19n = subset(e19n, siglas %in% c("PP", "VOX"),
  select = c("muni_code", "votantes", "censo", "validos", "siglas", "votos"))
e19n = e19n %>%
  pivot_wider(names_from = "siglas", values_from = "votos") %>%
  as.data.frame()
# Municipality code changes
e19n$muni_code = as.integer(changes_newcode(e19n$muni_code, 1930, 2011))
# Re-add with updated municipality codes
e19n = e19n %>% group_by(muni_code) %>%
  summarize_if(is.integer, sum) %>%
  filter(!is.na(muni_code)) %>%
  as.data.frame()
  # Calculating
e19n$PP = e19n$PP / e19n$validos
e19n$VOX = e19n$VOX / e19n$validos
names(e19n)[names(e19n) == "PP"] = "PP201911"
names(e19n)[names(e19n) == "VOX"] = "VOX201911"
e19n$part201911 = e19n$votantes / e19n$censo

e16 = read.csv("DATA/Spain/elections/clean/2016_06.csv")
e16$siglas[e16$siglas %in% c("PP", "PP-FORO", "PP-PAR", "UPN-PP")] = "PP"
e16 = subset(e16, siglas %in% c("PP", "VOX"),
  select = c("muni_code", "votantes", "censo", "validos", "siglas", "votos"))
e16 = e16 %>%
  pivot_wider(names_from = "siglas", values_from = "votos") %>%
  as.data.frame()
# Municipality code changes
e16$muni_code = as.integer(changes_newcode(e16$muni_code, 1930, 2011))
# Re-add with updated municipality codes
e16 = e16 %>% group_by(muni_code) %>%
  summarize_if(is.integer, sum) %>%
  filter(!is.na(muni_code)) %>%
  as.data.frame()
# Calculating
e16$PP = e16$PP / e16$validos
e16$VOX = e16$VOX / e16$validos
names(e16)[names(e16) == "PP"] = "PP201606"
names(e16)[names(e16) == "VOX"] = "VOX201606"
e16$part201606 = e16$votantes / e16$censo

elec = merge(
  e19a[, c("muni_code", "PP201904", "VOX201904", "part201904")],
  e19n[, c("muni_code", "PP201911", "VOX201911", "part201911")])
elec = merge(elec, e16[, c("muni_code", "PP201606", "VOX201606", "part201606")])

# Extra variable: Change VOX April-November 2019
elec$VOX_chg_2019 = elec$VOX201911 / elec$VOX201904
elec$VOX_chg_2019[elec$VOX_chg_2019 == "Inf"] = NA

table(elec$muni_code %in% data$muni_code)
table(data$muni_code %in% elec$muni_code)
data = merge(data, elec, all.x = TRUE)

## WARTIME VICTIMIZATION --------------------------------------------

# Victims by municipality up to 1942
vict_36_42 = read.csv("DATA/Spain/violence/victims_muni_36_42.csv")
# Merge
if(any(!vict_36_42$muni_code %in% data$muni_code)){warning("check")}
data = merge(data, vict_36_42, all.x = TRUE)
data$vict_nac[is.na(data$vict_nac)] = 0
data$vict_rep[is.na(data$vict_rep)] = 0
# But data for leftist violence not available for all provinces
data$vict_rep[data$prov_name %in% c("albacete", "badajoz",
  "a coruna", "lugo", "ourense", "pontevedra",
  "alava", "bizkaia", "gipuzkoa")] = NA

## UNEMPLOYMENT -----------------------------------------------------

u16 = read.csv("DATA/Spain/unemployment/output/unemployment_01_2016.csv")
u16 = subset(u16, prov %in% prov_incl)
# Change municipalities
u16$muni_code = as.integer(changes_newcode(u16$muni_code, 1930, 2011))
# Reaggregate
u16 = u16 %>%
  filter(!is.na(muni_code)) %>%
  group_by(muni_code) %>%
  summarize(n_unemploy_2016 = sum(total)) %>%
  as.data.frame()

any(!u16$muni_code %in% data$muni_code)
# subset(data, !muni_code %in% u16$muni_code)
data = merge(data, u16, all.x = TRUE)

u19 = read.csv("DATA/Spain/unemployment/output/unemployment_01_2019.csv")
u19 = subset(u19, prov %in% prov_incl)
# Change municipalities
u19$muni_code = as.integer(changes_newcode(u19$muni_code, 1930, 2011))
# Reaggregate
u19 = u19 %>%
  filter(!is.na(muni_code)) %>%
  group_by(muni_code) %>%
  summarize(n_unemploy_2019 = sum(total)) %>%
  as.data.frame()

any(!u19$muni_code %in% data$muni_code)
# subset(data, !muni_code %in% u19$muni_code)
data = merge(data, u19, all.x = TRUE)

# Get pop-weighted variables
data$unemp_2016 = data$n_unemploy_2016 / data$pop2011
data$unemp_2019 = data$n_unemploy_2019 / data$pop2011

## PREWAR ELECTIONS -------------------------------------------------

# Load data
results1936 = read.csv("DATA/Spain/elections_prewar/results1936.csv")
# Merge
if(any(!results1936$muni_code %in% results1936$muni_code)){warning("check")}
data = merge(data, results1936, all.x = TRUE)


## SAVE DATASET -----------------------------------------------------
write.csv(data, "Projects/blend_or_resist/data/data_1930_2011.csv", row.names = FALSE)
