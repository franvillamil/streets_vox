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

# -------------------------

# List of Francoist street names
franc_names = readLines("input/calles_franquistas.txt")

# -------------------------
# Changes in Francoist streets

# Subset of Francoist streets
changes = read.csv("str_changes/output/changes.csv")
changes$old = tolower(changes$old)
changes$new = tolower(changes$new)
changes_f = subset(changes, old %in% franc_names & !new %in% franc_names)

# Date variable
changes_f$date = as.Date(as.character(changes_f$new_date), format = "%Y%m%d")

# Changes by period
changes_f_muni = changes_f %>%
  group_by(muni, date) %>%
  summarize(changes = length(cod_via)) %>%
  mutate(date = as.character(format(date, "%Y-%m"))) %>%
  mutate(date = gsub("-06", "s1", date)) %>%
  mutate(date = gsub("-12", "s2", date)) %>%
  pivot_wider(names_from = "date", names_prefix = "fs_rm_", values_from = "changes") %>%
  as.data.frame()

# Re order
fs_vars = names(changes_f_muni)[2:length(names(changes_f_muni))]
changes_f_muni = changes_f_muni[, c("muni", fs_vars[order(fs_vars)])]
names(changes_f_muni)[names(changes_f_muni) == "fs_rm_2010s2"] = "fs_rm_2001_2010"

# -------------------------
# Total number of Francoist streets at each time period

# List and load
rawdir = "download_str/output"
f = paste0(rawdir, "/", list.files(rawdir))
df_list = lapply(f, function(x) read.csv(x))
if( any(sapply(df_list, function(x) ncol(x)) != 4) ){warning("problem with ncol!")}

# Merge, clean and prepare
calles = as.data.frame(do.call("rbind", df_list))
calles = calles[, c("muni", "fecha", "nombre_via")]
calles$nombre_via = tolower(calles$nombre_via)

# Fix date
calles$fecha = paste(str_sub(as.character(calles$fecha), 1, 4),
  str_sub(as.character(calles$fecha), 5, 6), sep = "_")

# Get number of streets and Francoist streets by muni and date
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

# Get province and total aggregates (for descriptives)
fc_prov = calles %>%
  mutate(prov = code_to_prov(str_sub(muni, -5L, -4L))) %>%
  group_by(prov, fecha) %>%
  summarize(
    total_streets = length(nombre_via),
    f_streets = length(nombre_via[nombre_via %in% franc_names]))

fc_all = calles %>%
  group_by(fecha) %>%
  summarize(
    total_streets = length(nombre_via),
    f_streets = length(nombre_via[nombre_via %in% franc_names]))

# -------------------------
# Put together

# Merge changes with total number of streets
data = merge(fc_muni, changes_f_muni, all.x = TRUE, all.y = TRUE)

# Turn NAs to 0
for(i in names(data)[names(data) != "muni"]){data[ is.na(data[, i]) , i] = 0}

# Change variable name
names(data)[names(data) == "muni"] = "muni_code"

# -------------------------
# Save

write.csv(data, "str_agg/output/fs.csv", row.names = FALSE)
write.csv(fc_prov, "str_agg/output/fs_prov.csv", row.names = FALSE)
write.csv(fc_all, "str_agg/output/fs_all.csv", row.names = FALSE)
