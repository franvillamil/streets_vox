.PHONY: all clean taskflow wsubdir
.DELETE_ON_ERROR:

# ------------------------
# Variables

raw_streets = download_str/output/V_072001.csv download_str/output/V_012011.csv download_str/output/V_072011.csv download_str/output/V_012012.csv download_str/output/V_072012.csv download_str/output/V_012013.csv download_str/output/V_072013.csv download_str/output/V_012014.csv download_str/output/V_072014.csv download_str/output/V_012015.csv download_str/output/V_072015.csv download_str/output/V_012016.csv download_str/output/V_072016.csv download_str/output/V_012017.csv download_str/output/V_072017.csv download_str/output/V_012018.csv download_str/output/V_072018.csv download_str/output/V_012019.csv download_str/output/V_072019.csv download_str/output/V_012020.csv download_str/output/V_072020.csv
dataset = dataset/output/data.csv
out_main_mod = main_models/output/DiD_estimates.pdf main_models/output/tab_did.tex main_models/output/tab_cs.tex

# ------------------------
# Main recipes

all: download_elec/output/elec.csv $(dataset) $(out_main_mod)

clean:
	rm -rvf $(dataset)# $(out_desc) $(out_lm) $(out_robust) $(out_alt)
	rm -rvf */output/*

taskflow:
	Rscript --no-save --verbose taskflow/create_dependency_graph.R
	dot -Grankdir=LR -Tpdf taskflow/dependency_list.txt -o taskflow/workflow.pdf
	sips -s format jpeg taskflow/workflow.pdf --out taskflow/workflow.jpeg

wsubdir:
	mkdir -p writing/tab writing/img

# ------------------------
# Data

$(raw_streets): download_str/download.R
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

str_changes/output/changes.csv: str_changes/str_chg.R $(raw_streets)
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

str_local_vars/output/fs.csv: str_local_vars/local.R input/calles_franquistas.txt str_changes/output/changes.csv $(raw_streets)
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

download_elec/output/elec.csv: download_elec/elec.R
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

$(dataset): dataset/dataset.R input/unemployment_01_2019.csv input/unemployment_01_2016.csv input/major_izq_muni.csv input/INE_census.csv str_local_vars/output/fs.csv download_elec/output/elec.csv
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

$(out_main_mod): main_models/mod.R main_models/functions_did.R $(dataset)
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out
