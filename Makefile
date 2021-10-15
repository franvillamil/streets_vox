.PHONY: all clean taskflow
.DELETE_ON_ERROR:

# ------------------------
# Variables

raw_streets = download_str/output/V_072001.csv download_str/output/V_012011.csv download_str/output/V_072011.csv download_str/output/V_012012.csv download_str/output/V_072012.csv download_str/output/V_012013.csv download_str/output/V_072013.csv download_str/output/V_012014.csv download_str/output/V_072014.csv download_str/output/V_012015.csv download_str/output/V_072015.csv download_str/output/V_012016.csv download_str/output/V_072016.csv download_str/output/V_012017.csv download_str/output/V_072017.csv download_str/output/V_012018.csv download_str/output/V_072018.csv download_str/output/V_012019.csv download_str/output/V_072019.csv download_str/output/V_012020.csv download_str/output/V_072020.csv
agg_streets = str_agg/output/fs.csv str_agg/output/fs_prov.csv str_agg/output/fs_all.csv
dataset = dataset/dataset.Rout
out_main_mod = main_models/mod.Rout
out_robust = robust/rob.Rout
out_desc = descriptives/desc.Rout

# ------------------------
# Main recipes

all: empirics taskflow latex
empirics: $(dataset) $(out_desc) $(out_main_mod) $(out_robust)

clean:
	rm -rvf */output/*
	rm -rvf */*.Rout

taskflow:
	Rscript --no-save --verbose taskflow/create_dependency_graph.R
	dot -Grankdir=LR -Tpdf taskflow/dependency_list.txt -o taskflow/workflow.pdf
	sips -s format jpeg taskflow/workflow.pdf --out taskflow/workflow.jpeg

latex:
	mkdir -p writing/img writing/tab
	cp */output/*.pdf writing/img
	cp */output/*.tex writing/tab

# ------------------------
# Data

$(raw_streets): download_str/download.R
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

str_changes/output/changes.csv: str_changes/str_chg.R $(raw_streets)
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

$(agg_streets): str_agg/agg.R input/calles_franquistas.txt str_changes/output/changes.csv $(raw_streets)
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

download_elec/output/elec.csv: download_elec/elec.R
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

$(dataset): dataset/dataset.R input/unemployment_01_2019.csv input/unemployment_01_2016.csv input/major_izq_muni.csv input/INE_census.csv str_agg/output/fs.csv download_elec/output/elec.csv
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

# ------------------------
# Analyses and descriptives

$(out_main_mod): main_models/mod.R func/functions_did.R func/my_stargazer.R $(dataset)
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

$(out_robust): robust/rob.R $(dataset) func/my_stargazer.R
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

$(out_desc): descriptives/desc.R input/calles_franquistas.txt str_changes/output/changes.csv $(agg_streets)
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out
	pdfcrop descriptives/output/map.pdf descriptives/output/map.pdf
	pdfcrop descriptives/output/map_full.pdf descriptives/output/map_full.pdf
