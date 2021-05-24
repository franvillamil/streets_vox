# setwd("~/Documents/Projects/streets_vox")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("zip", "stringr")
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
# Encoding
Sys.setlocale("LC_ALL", "C")

# -------------------------

# Functions

# Download, unzip, return file path (in tempdir)
download = function(url){

  # Download and unzip
  tmpf = tempfile()
  tmpd = tempdir()
  download.file(paste0("http://www.ine.es/prodyser/callejero/caj_esp/", url, ".zip"), tmpf)
  unzip(tmpf, exdir = tmpd)

  # If folder, mv out of folder
  if(any(list.files(tmpd) == url)){
    system(paste0("mv ", tmpd, "/", url, "/* ", tmpd))
  }

  # If ZIP subfiles, unzip
  if(any(grepl(".zip", list.files(tmpd)))){
    for(j in list.files(tmpd)[grepl("zip$", list.files(tmpd))]){
      jpath = paste0(tmpd, "/", j)
      unzip(jpath, exdir = tmpd)
      file.remove(jpath)
    }
  }

  filepath = paste(tmpd, list.files(tmpd)[grepl("^VIAS", list.files(tmpd))], sep = "/")
  print(filepath)
  return(filepath)

}

# fwf structure
str_fwf = matrix(
  c("muni", 5, "integer",
  "cod_via", 5, "integer",
  "unknown1", 3, "integer",
  "fecha", 8, "integer",
  "unknown2", 1, "integer",
  "cod_via2", 5, "integer",
  "tipo_via", 6, "character",
  "nombre_via", 50, "character",
  "nombre_via_corto", 25, "character"),
  ncol = 3, byrow = TRUE)
str_fwf = data.frame(
  varname = str_fwf[,1],
  width = as.integer(str_fwf[,2]),
  class = str_fwf[,3])


# Read fixed with files
read_fixedwidth = function(file){
  # Read
  print("reading...")
  output = read.fwf(file,
    widths = str_fwf$width,
    colClasses = str_fwf$class)
  # Assign names
  names(output) = str_fwf$varname
  # Clean up temporary folder for next iteration
  dir = gsub("/VIAS.*$", "", file)
  files_to_rm = paste(dir, list.files(dir), sep = "/")
  for(f in files_to_rm){system(paste0("rm -rfv ", f))}
  # Limit columns
  output = output[, c("muni", "cod_via", "fecha", "nombre_via")]
  # Encoding
  print("adapting encoding...")
  output = adapt(output)
  print("cleaning...")
  # Muni code
  output$muni = sprintf("%0.5d", output$muni)
  # Cleaning
  output$nombre_via = str_trim(output$nombre_via)
  # Return
  return(output)
  }


# -------------------------

urls = c("caj_esp_072001",
  "caj_esp_012011", "caj_esp_072011",
  "caj_esp_012012", "caj_esp_072012",
  "caj_esp_012013", "caj_esp_072013",
  "caj_esp_012014", "caj_esp_072014",
  "caj_esp_012015", "caj_esp_072015",
  "caj_esp_012016", "caj_esp_072016",
  "caj_esp_012017", "caj_esp_072017",
  "caj_esp_012018", "caj_esp_072018",
  "caj_esp_012019", "caj_esp_072019",
  "caj_esp_012020", "caj_esp_072020")

# -------------------------


for(u in urls){

  filepath = download(u)
  df = read_fixedwidth(filepath)
  out = paste0("download_str/output/V_", str_sub(u, -6L, -1L), ".csv")
  write.csv(df, out, row.names = FALSE)

}
