setwd("~/Google Drive/Academic")
options(stringsAsFactors = FALSE)
library(muniSpain)
library(dplyr)
library(tidyr)
library(stringr)

## BASE DATA (census + streets) -------------------------------------

# Census
census = subset(read.csv("DATA/Spain/census/INE_census.csv"),
  !is.na(c2011),
  select = c("prov_name", "muni_code", "muni_name", "c2011"))
census$lpop2011 = log(census$c2011)
census = census[, -which(names(census) == "c2011")]
# Add CCAA
census$ccaa = prov_to_ccaa(census$prov_name)

# Francoist streets
fs = read.csv("DATA/Spain/street_names/output/francoist_streets.csv")

# Create change variables
fs$fs_rm_2001s2_2015s2 = with(fs, fs_rm_2001_2010 + fs_rm_2011s1 +
  fs_rm_2011s2 + fs_rm_2012s1 + fs_rm_2012s2 + fs_rm_2013s1 + fs_rm_2013s2 +
  fs_rm_2014s1 + fs_rm_2014s2 + fs_rm_2015s1 + fs_rm_2015s2)
fs$fs_rm_2001s2_2018s2 = with(fs, fs_rm_2001_2010 + fs_rm_2011s1 +
  fs_rm_2011s2 + fs_rm_2012s1 + fs_rm_2012s2 + fs_rm_2013s1 + fs_rm_2013s2 +
  fs_rm_2014s1 + fs_rm_2014s2 + fs_rm_2015s1 + fs_rm_2015s2 + fs_rm_2016s1 +
  fs_rm_2016s2 + fs_rm_2017s1 + fs_rm_2017s2 + fs_rm_2018s1 + fs_rm_2018s2)
fs$fs_rm_2011s1_2018s2 = with(fs, fs_rm_2011s1 +
  fs_rm_2011s2 + fs_rm_2012s1 + fs_rm_2012s2 + fs_rm_2013s1 + fs_rm_2013s2 +
  fs_rm_2014s1 + fs_rm_2014s2 + fs_rm_2015s1 + fs_rm_2015s2 + fs_rm_2016s1 +
  fs_rm_2016s2 + fs_rm_2017s1 + fs_rm_2017s2 + fs_rm_2018s1 + fs_rm_2018s2)
fs$fs_rm_2016s2_2018s2 = with(fs, fs_rm_2016s2 + fs_rm_2017s1 +
  fs_rm_2017s2 + fs_rm_2018s1 + fs_rm_2018s2)
fs$fs_rm_2016s2_2019s1 = with(fs, fs_rm_2016s2 + fs_rm_2017s1 +
  fs_rm_2017s2 + fs_rm_2018s1 + fs_rm_2018s2 + fs_rm_2019s1)
fs$fs_rm_2019s1_2019s2 = with(fs, fs_rm_2019s1 + fs_rm_2019s2)
# Turn to binary
fs$fs_rm_2001s2_2015s2_bin = ifelse(fs$fs_rm_2001s2_2015s2 > 0, 1, 0)
fs$fs_rm_2001s2_2018s2_bin = ifelse(fs$fs_rm_2001s2_2018s2 > 0, 1, 0)
fs$fs_rm_2011s1_2018s2_bin = ifelse(fs$fs_rm_2011s1_2018s2 > 0, 1, 0)
fs$fs_rm_2016s2_2018s2_bin = ifelse(fs$fs_rm_2016s2_2018s2 > 0, 1, 0)
fs$fs_rm_2016s2_2019s1_bin = ifelse(fs$fs_rm_2016s2_2019s1 > 0, 1, 0)
fs$fs_rm_2019s1_2019s2_bin = ifelse(fs$fs_rm_2019s1_2019s2 > 0, 1, 0)
fs$fs_rm_2019s2_bin = ifelse(fs$fs_rm_2019s2 > 0, 1, 0)
# Create alternative (binary) change variables
fs$fs_rm_2001s2_2015s2_bin2 = with(fs, ifelse((fs_2001_06 - fs_2015_12) > 0, 1, 0))
fs$fs_rm_2001s2_2018s2_bin2 = with(fs, ifelse((fs_2001_06 - fs_2018_12) > 0, 1, 0))
fs$fs_rm_2016s2_2018s2_bin2 = with(fs, ifelse((fs_2016_06 - fs_2018_12) > 0, 1, 0))
fs$fs_rm_2016s2_2019s1_bin2 = with(fs, ifelse((fs_2016_06 - fs_2019_06) > 0, 1, 0))
fs$fs_rm_2019s1_2019s2_bin2 = with(fs, ifelse((fs_2018_12 - fs_2019_12) > 0, 1, 0))
fs$fs_rm_2019s2_bin2 = with(fs, ifelse((fs_2019_06 - fs_2019_12) > 0, 1, 0))
# Only keep certain variables
fs = fs[, c("muni_code", "fs_rm_2016s2_2018s2",
  "fs_rm_2001s2_2015s2_bin", "fs_rm_2001s2_2018s2_bin", "fs_rm_2011s1_2018s2_bin",
  "fs_rm_2016s2_2018s2_bin", "fs_rm_2016s2_2019s1_bin", "fs_rm_2019s1_2019s2_bin",
  "fs_rm_2019s2_bin", "fs_rm_2001s2_2015s2_bin2", "fs_rm_2001s2_2018s2_bin2",
  "fs_rm_2016s2_2018s2_bin2", "fs_rm_2016s2_2019s1_bin2", "fs_rm_2019s1_2019s2_bin2",
  "fs_rm_2019s2_bin2", "fs_2001_06", "fs_2010_12", "fs_2016_06", "fs_2018_12", "fs_2019_06")]

# Merge
subset(census, !muni_code %in% fs$muni_code) # (NOTE: !? e.g. Alfoz)
# subset(fs, !muni_code %in% census$muni_code) # All not un 2011 census
data = merge(census, fs)

## ALCALDES ---------------------------------------------------------

alc = read.csv("DATA/Spain/alcaldes/major_izq_muni.csv")
subset(data, !muni_code %in% alc$muni_code)[,1:4] # ?
data = merge(data, alc, all.x = TRUE)

## ELECTORAL DATA ---------------------------------------------------

e19a = read.csv("DATA/Spain/elections/clean/2019_04.csv")
# unique(e19a$siglas)[grepl("PP", unique(e19a$siglas))]
e19a$siglas[e19a$siglas %in% c("PP", "PP-FORO")] = "PP"
e19a$siglas[e19a$siglas %in% c("PSC", "PSdeG-PSOE", "PSE-EE (PSO")] = "PSOE"
e19a = e19a %>%
  filter(siglas %in% c("PP", "VOX", "PSOE")) %>%
  select(c("muni_code", "votantes", "censo", "validos", "siglas", "votos")) %>%
  pivot_wider(names_from = "siglas", values_from = "votos") %>%
  mutate(
    PP = PP / validos,
    VOX = VOX / validos,
    PSOE = PSOE / validos,
    part = votantes / censo) %>%
  select(-c("votantes", "censo", "validos")) %>%
  rename_at(vars(-muni_code), paste0, "201904") %>%
  as.data.frame()

e19n = read.csv("DATA/Spain/elections/clean/2019_11.csv")
# unique(e19n$siglas)[grepl("PP", unique(e19n$siglas))]
e19n$siglas[e19n$siglas %in% c("PP", "PP-FORO")] = "PP"
e19n$siglas[e19n$siglas %in% c("PSC-PSOE", "PSdeG-PSOE", "PSE-EE (PSOE)")] = "PSOE"
e19n = e19n %>%
  filter(siglas %in% c("PP", "VOX", "PSOE")) %>%
  select(c("muni_code", "votantes", "censo", "validos", "siglas", "votos")) %>%
  pivot_wider(names_from = "siglas", values_from = "votos") %>%
  mutate(
    PP = PP / validos,
    VOX = VOX / validos,
    PSOE = PSOE / validos,
    part = votantes / censo) %>%
  select(-c("votantes", "censo", "validos")) %>%
  rename_at(vars(-muni_code), paste0, "201911") %>%
  as.data.frame()

e16 = read.csv("DATA/Spain/elections/clean/2016_06.csv")
e16$siglas[e16$siglas %in% c("PP", "PP-FORO", "PP-PAR", "UPN-PP")] = "PP"
e16$siglas[e16$siglas %in% c("PSC-PSOE", "PSdeG-PSOE", "PSE-EE (PSOE)", "PSOE-NCa")] = "PSOE"
e16 = e16 %>%
  filter(siglas %in% c("PP", "VOX", "PSOE")) %>%
  select(c("muni_code", "votantes", "censo", "validos", "siglas", "votos")) %>%
  pivot_wider(names_from = "siglas", values_from = "votos") %>%
  mutate(
    PP = PP / validos,
    VOX = VOX / validos,
    PSOE = PSOE / validos,
    part = votantes / censo) %>%
  select(-c("votantes", "censo", "validos")) %>%
  rename_at(vars(-muni_code), paste0, "201606") %>%
  as.data.frame()

e15 = read.csv("DATA/Spain/elections/clean/2015_12.csv")
e15$siglas[e15$siglas %in% c("PP", "PP-FORO", "PP-PAR", "UPN-PP")] = "PP"
e15$siglas[e15$siglas %in% c("PSC-PSOE", "PSC", "PSdeG-PSOE", "PSE-EE (PSO",
  "PSE-EE", "PSOE-NCa")] = "PSOE"
e15 = e15 %>%
  filter(siglas %in% c("PP", "VOX", "PSOE")) %>%
  select(c("muni_code", "votantes", "censo", "validos", "siglas", "votos")) %>%
  pivot_wider(names_from = "siglas", values_from = "votos") %>%
  mutate(
    PP = PP / validos,
    VOX = VOX / validos,
    PSOE = PSOE / validos,
    part = votantes / censo) %>%
  select(-c("votantes", "censo", "validos")) %>%
  rename_at(vars(-muni_code), paste0, "201512") %>%
  as.data.frame()

e11 = read.csv("DATA/Spain/elections/clean/2011_11.csv")
e11$siglas[e11$siglas %in% c("PP-PAR", "PP-EU", "UPN-PP", "P.P.", "P.P-E.U.")] = "PP"
e11$siglas[e11$siglas %in% c("PSdeG-PSOE", "PSE-EE (PSOE)", "PSOE",
  "PSC-PSOE", "P.S.O.E.")] = "PSOE"
e11 = e11 %>%
  filter(siglas %in% c("PP", "PSOE")) %>%
  select(c("muni_code", "votantes", "censo", "validos", "siglas", "votos")) %>%
  pivot_wider(names_from = "siglas", values_from = "votos") %>%
  mutate(
    PP = PP / validos,
    PSOE = PSOE / validos,
    part = votantes / censo) %>%
  select(-c("votantes", "censo", "validos")) %>%
  rename_at(vars(-muni_code), paste0, "201111") %>%
  as.data.frame()

elec = merge(e19a, e19n)
elec = merge(elec, e16)
elec = merge(elec, e15, all.x = TRUE)
elec = merge(elec, e11, all.x = TRUE)
nrow(elec)

table(elec$muni_code %in% data$muni_code)
table(data$muni_code %in% elec$muni_code)
data = merge(data, elec)

## UNEMPLOYMENT -----------------------------------------------------

u16 = read.csv("DATA/Spain/unemployment/output/unemployment_01_2016.csv")
# subset(u16, !muni_code %in% data$muni_code)
any(!data$muni_code %in% u16$muni_code)
names(u16)[names(u16) == "total"] = "n_unemploy_2016"
data = merge(data, u16[, c("muni_code", "n_unemploy_2016")])

u19 = read.csv("DATA/Spain/unemployment/output/unemployment_01_2019.csv")
# subset(u19, !muni_code %in% data$muni_code)
any(!data$muni_code %in% u19$muni_code)
names(u19)[names(u19) == "total"] = "n_unemploy_2019"
data = merge(data, u19[, c("muni_code", "n_unemploy_2019")])

# Get pop-weighted variables
data$unemp_2016 = data$n_unemploy_2016 / exp(data$lpop2011)
data$unemp_2019 = data$n_unemploy_2019 / exp(data$lpop2011)

## ------------------------------------------------------------------
## SAVE DATASET
write.csv(data, "Projects/streets_vox/data/data.csv", row.names = FALSE)
