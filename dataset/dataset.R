# setwd("~/Documents/Projects/streets_vox")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "tidyr", "stringr")
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

# Load data
census = read.csv("input/INE_census.csv")
mayor = read.csv("input/major_izq_muni.csv")
unemp16 = read.csv("input/unemployment_01_2016.csv")
unemp19 = read.csv("input/unemployment_01_2019.csv")
elec = read.csv("download_elec/output/elec.csv")
francst = read.csv("str_agg/output/fs.csv")

# ------------------------------

# Data structure
data = census %>%
  filter(!is.na(c2011)) %>%
  select(prov_name, muni_code, muni_name, c2011) %>%
  mutate(lpop2011 = log(c2011)) %>%
  mutate(ccaa = prov_to_ccaa(prov_name)) %>%
  select(-c2011)

# ------------------------------

# Francoist streets
fs_sum = francst %>%
  # Create period variables
  mutate(
    fs_rm_2001s2_2015s2 = fs_rm_2001_2010 + fs_rm_2011s1 +
      fs_rm_2011s2 + fs_rm_2012s1 + fs_rm_2012s2 + fs_rm_2013s1 +
      fs_rm_2013s2 + fs_rm_2014s1 + fs_rm_2014s2 + fs_rm_2015s1 +
      fs_rm_2015s2,
    fs_rm_2001s2_2018s2 = fs_rm_2001_2010 + fs_rm_2011s1 +
      fs_rm_2011s2 + fs_rm_2012s1 + fs_rm_2012s2 + fs_rm_2013s1 +
      fs_rm_2013s2 + fs_rm_2014s1 + fs_rm_2014s2 + fs_rm_2015s1 +
      fs_rm_2015s2 + fs_rm_2016s1 + fs_rm_2016s2 + fs_rm_2017s1 +
      fs_rm_2017s2 + fs_rm_2018s1 + fs_rm_2018s2,
    fs_rm_2011s1_2018s2 = fs_rm_2011s1 + fs_rm_2011s2 +
      fs_rm_2012s1 + fs_rm_2012s2 + fs_rm_2013s1 + fs_rm_2013s2 +
      fs_rm_2014s1 + fs_rm_2014s2 + fs_rm_2015s1 + fs_rm_2015s2 +
      fs_rm_2016s1 + fs_rm_2016s2 + fs_rm_2017s1 + fs_rm_2017s2 +
      fs_rm_2018s1 + fs_rm_2018s2,
    fs_rm_2016s2_2018s2 = fs_rm_2016s2 + fs_rm_2017s1 +
      fs_rm_2017s2 + fs_rm_2018s1 + fs_rm_2018s2,
    fs_rm_2016s2_2019s1 = fs_rm_2016s2 + fs_rm_2017s1 +
      fs_rm_2017s2 + fs_rm_2018s1 + fs_rm_2018s2 + fs_rm_2019s1,
    fs_rm_2019s1_2019s2 = fs_rm_2019s1 + fs_rm_2019s2) %>%
    # Turn to binary
  mutate(
    fs_rm_2001s2_2015s2_bin = ifelse(fs_rm_2001s2_2015s2 > 0, 1, 0),
    fs_rm_2001s2_2018s2_bin = ifelse(fs_rm_2001s2_2018s2 > 0, 1, 0),
    fs_rm_2011s1_2018s2_bin = ifelse(fs_rm_2011s1_2018s2 > 0, 1, 0),
    fs_rm_2016s2_2018s2_bin = ifelse(fs_rm_2016s2_2018s2 > 0, 1, 0),
    fs_rm_2016s2_2019s1_bin = ifelse(fs_rm_2016s2_2019s1 > 0, 1, 0),
    fs_rm_2019s1_2019s2_bin = ifelse(fs_rm_2019s1_2019s2 > 0, 1, 0),
    fs_rm_2019s2_bin = ifelse(fs_rm_2019s2 > 0, 1, 0)) %>%
  select(muni_code, fs_rm_2016s2_2018s2, fs_rm_2001s2_2015s2_bin,
    fs_rm_2001s2_2018s2_bin, fs_rm_2011s1_2018s2_bin, fs_rm_2016s2_2018s2_bin,
    fs_rm_2016s2_2019s1_bin, fs_rm_2019s1_2019s2_bin, fs_rm_2019s2_bin,
    fs_2001_06, fs_2010_12, fs_2016_06, fs_2018_12, fs_2019_06)


# Merge with census
data = merge(data, fs_sum, all.x = TRUE)

# ------------------------------

# Unemployment data
names(unemp16)[names(unemp16) == "total"] = "n_unemploy_2016"
data = merge(data, unemp16[, c("muni_code", "n_unemploy_2016")])
names(unemp19)[names(unemp19) == "total"] = "n_unemploy_2019"
data = merge(data, unemp19[, c("muni_code", "n_unemploy_2019")])

# Get pop-weighted variables
data = data %>%
  mutate(
    unemp_2016 = n_unemploy_2016 / exp(lpop2011),
    unemp_2019 = n_unemploy_2019 / exp(lpop2011))

# ------------------------------

# Merge electoral data and mayor data
data = merge(data, elec, all.x = TRUE)
data = merge(data, mayor, all.x = TRUE)

# ------------------------------

# Change a few variables (log, etc)
data = data %>% mutate(
  l_fs_2001_06 = log(fs_2001_06 + 1),
  l_fs_2016_06 = log(fs_2016_06 + 1),
  l_fs_2018_12 = log(fs_2018_12 + 1),
  l_fs_2019_06 = log(fs_2019_06 + 1),
  l_fs_rm_2016s2_2018s2 = log(fs_rm_2016s2_2018s2 + 1),
  change_2019_VOX = VOX2019_11 / VOX2019_04)
data$change_2019_VOX[data$change_2019_VOX == "Inf"] = NA

# ------------------------------

# Create long form datasets

# VOX
dl_VOX = data %>%
  filter(!is.na(VOX2016_06) & fs_2016_06 > 0) %>%
  dplyr::select(-ends_with(c("2011_11", "2019_11"))) %>%
  pivot_longer(
    cols = starts_with("VOX"),
    names_to = "election",
    names_prefix = "VOX",
    values_to = c("VOX_share")) %>%
  mutate(VOX_share = VOX_share * 100) %>%
  as.data.frame()

# PP
dl_PP = data %>%
  filter(fs_2016_06 > 0) %>%
  dplyr::select(-ends_with(c("2019_11"))) %>%
  pivot_longer(
    cols = starts_with("PP"),
    names_to = "election",
    names_prefix = "PP",
    values_to = c("PP_share")) %>%
  mutate(PP_share = PP_share * 100) %>%
  as.data.frame()

# PSOE
dl_PSOE = data %>%
  filter(fs_2016_06 > 0) %>%
  dplyr::select(-ends_with(c("2019_11"))) %>%
  pivot_longer(
    cols = starts_with("PSOE"),
    names_to = "election",
    names_prefix = "PSOE",
    values_to = c("PSOE_share")) %>%
  mutate(PSOE_share = PSOE_share * 100) %>%
  as.data.frame()

# ------------------------------

# Save everything
write.csv(data, "dataset/output/data.csv", row.names = FALSE)
write.csv(dl_VOX, "dataset/output/dl_VOX.csv", row.names = FALSE)
write.csv(dl_PP, "dataset/output/dl_PP.csv", row.names = FALSE)
write.csv(dl_PSOE, "dataset/output/dl_PSOE.csv", row.names = FALSE)
