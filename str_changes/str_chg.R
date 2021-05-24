# setwd("~/Documents/Projects/streets_vox")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "stringr")
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
# Functions

changes = function(old, new, dir){
  # Get full file path
  oldf = paste0(dir, "/", old, ".csv")
  newf = paste0(dir, "/", new, ".csv")
  # Read files
  c1 = read.csv(oldf)
  c2 = read.csv(newf)
  # Assign unique code to streets
  c1$cod_full = paste0(c1$muni, "_", c1$cod_via)
  c2$cod_full = paste0(c2$muni, "_", c2$cod_via)
  # Join
  c = rbind(c1, c2)
  # Detect changes
  changes = c %>%
    group_by(cod_full) %>%
    summarize(changes = length(unique(nombre_via)) > 1) %>%
    filter(changes) %>%
    as.data.frame()
  # Merge with other data
  changes$old = c1$nombre_via[match(changes$cod_full, c1$cod_full)]
  changes$new = c2$nombre_via[match(changes$cod_full, c2$cod_full)]
  changes$old_date = c1$fecha[match(changes$cod_full, c1$cod_full)]
  changes$new_date = c2$fecha[match(changes$cod_full, c2$cod_full)]
  # Clean data on municipalities
  changes$muni = gsub("_.*", "", changes$cod_full)
  changes$cod_via = gsub(".*_", "", changes$cod_full)
  changes$prov = code_to_prov(str_sub(changes$muni, -5L, -4L))
  # Select and reorder
  changes = changes[, c("muni", "cod_via", "prov", "old", "new", "old_date", "new_date")]
  # Return
  return(changes)
}

# -------------------------


dir = "download_str/output"

changes = rbind(
  changes(old = "V_072001", new = "V_012011", dir = dir),
  changes(old = "V_012011", new = "V_072011", dir = dir),
  changes(old = "V_072011", new = "V_012012", dir = dir),
  changes(old = "V_012012", new = "V_072012", dir = dir),
  changes(old = "V_072012", new = "V_012013", dir = dir),
  changes(old = "V_012013", new = "V_072013", dir = dir),
  changes(old = "V_072013", new = "V_012014", dir = dir),
  changes(old = "V_012014", new = "V_072014", dir = dir),
  changes(old = "V_072014", new = "V_012015", dir = dir),
  changes(old = "V_012015", new = "V_072015", dir = dir),
  changes(old = "V_072015", new = "V_012016", dir = dir),
  changes(old = "V_012016", new = "V_072016", dir = dir),
  changes(old = "V_072016", new = "V_012017", dir = dir),
  changes(old = "V_012017", new = "V_072017", dir = dir),
  changes(old = "V_072017", new = "V_012018", dir = dir),
  changes(old = "V_012018", new = "V_072018", dir = dir),
  changes(old = "V_072018", new = "V_012019", dir = dir),
  changes(old = "V_012019", new = "V_072019", dir = dir),
  changes(old = "V_072019", new = "V_012020", dir = dir),
  changes(old = "V_012020", new = "V_072020", dir = dir))

# Save
write.csv(changes, "str_changes/output/changes.csv", row.names = FALSE)
