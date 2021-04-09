setwd("~/Google Drive/Academic")
options(stringsAsFactors = FALSE)
library(muniSpain)
library(dplyr)
library(tidyr)
library(stringr)
library(electoral)

## BASE DATA (census + streets) -------------------------------------

# Census
census = subset(read.csv("DATA/Spain/census/INE_census.csv"),
  !is.na(c2011), select = c("prov_name", "muni_code", "muni_name"))
# Add CCAA
census$ccaa = prov_to_ccaa(census$prov_name)

# Francoist streets
fs = read.csv("DATA/Spain/street_names/output/francoist_streets.csv")
# Create change variables
fs$fs_rm_2016s2_2018s2 = with(fs, fs_rm_2016s2 + fs_rm_2017s1 +
  fs_rm_2017s2 + fs_rm_2018s1 + fs_rm_2018s2)
fs$fs_rm_2016s2_2019s1 = with(fs, fs_rm_2016s2 + fs_rm_2017s1 +
  fs_rm_2017s2 + fs_rm_2018s1 + fs_rm_2018s2 + fs_rm_2019s1)
# Turn to binary
fs$fs_rm_2016s2_2018s2_bin = ifelse(fs$fs_rm_2016s2_2018s2 > 0, 1, 0)
fs$fs_rm_2016s2_2019s1_bin = ifelse(fs$fs_rm_2016s2_2019s1 > 0, 1, 0)
# Only keep certain variables
fs = fs[, c("muni_code", "fs_rm_2016s2_2018s2_bin", "fs_rm_2016s2_2019s1_bin")]

# Merge
subset(census, !muni_code %in% fs$muni_code) # (NOTE: !? e.g. Alfoz)
# subset(fs, !muni_code %in% census$muni_code) # All not un 2011 census
data = merge(census, fs)

## ELECTION 2019 ----------------------------------------------------

e19a = read.csv("DATA/Spain/elections/clean/2019_04.csv")
e19a$siglas[e19a$siglas %in% c("PP", "PP-FORO")] = "PP"
e19a$siglas[e19a$siglas %in% c("PSC", "PSdeG-PSOE", "PSE-EE (PSO")] = "PSOE"

# Merge with data on streets
e19a = merge(e19a, fs)



## Get provincial summaries
prov1 = e19a %>%
  select(prov_code, siglas, validos, votos) %>%
  group_by(prov_code, siglas) %>%
  summarize_all(sum) %>%
  mutate(prov = code_to_prov(prov_code)) %>% as.data.frame

# Reduce VOX share / increase PP
effect = 0.0072
vox_votes = e19a$votos[e19a$siglas == "VOX" & e19a$fs_rm_2016s2_2018s2_bin == 1]
vox_votes_decrease = round(vox_votes * 0.0072, 0)
e19a$votos[e19a$siglas == "VOX" & e19a$fs_rm_2016s2_2018s2_bin == 1] =
  vox_votes - vox_votes_decrease

## Get counter-factual prov summaries
prov2 = e19a %>%
  select(prov_code, siglas, validos, votos) %>%
  group_by(prov_code, siglas) %>%
  summarize_all(sum) %>%
  mutate(prov = code_to_prov(prov_code)) %>% as.data.frame

## Vote decrease
prov1$v_actual = prov1$votos
prov2$v_counterfac = prov2$votos
sum = merge(prov1[, c("prov_code", "siglas", "v_actual")],
  prov2[, c("prov_code", "siglas", "v_counterfac")]) %>%
  filter(siglas == "VOX") %>%
  mutate(decrease = v_counterfac / v_actual, prov = code_to_prov(prov_code))
sum = sum[order(sum$decrease),]

ggplot(sum, aes(x = decrease, y = reorder(prov, decrease))) +
  geom_bar(stat = "identity") +
  scale_x_continuous(limits = c(0.9, 1.01))


get_VOX_seats = function(prov_df, n_seats){
  result = seats_ha(
    parties = prov_df[, "siglas"],
    votes = prov_df[, "votos"],
    n_seats = n_seats, method = "dhondt")
  if("VOX" %in% names(result)){return(result["VOX"])}else{return(0)}
}

c(get_VOX_seats(subset(prov1, prov=="valladolid"), 5), get_VOX_seats(subset(prov2, prov=="valladolid"), 5))

c(get_VOX_seats(subset(prov1, prov=="madrid"), 37),
  get_VOX_seats(subset(prov2, prov=="madrid"), 37))

c(get_VOX_seats(subset(prov1, prov=="palencia"), 4),
  get_VOX_seats(subset(prov2, prov=="palencia"), 4))

c(get_VOX_seats(subset(prov1, prov=="la rioja"), 4),
  get_VOX_seats(subset(prov2, prov=="la rioja"), 4))

c(get_VOX_seats(subset(prov1, prov=="baleares"), 8),
  get_VOX_seats(subset(prov2, prov=="baleares"), 8))

c(get_VOX_seats(subset(prov1, prov=="avila"), 3),
  get_VOX_seats(subset(prov2, prov=="avila"), 3))

c(get_VOX_seats(subset(prov1, prov=="sevilla"), 12),
  get_VOX_seats(subset(prov2, prov=="sevilla"), 12))
