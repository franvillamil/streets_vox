.PHONY: all clean taskflow wsubdir
.DELETE_ON_ERROR:

# ------------------------
# Variables

raw_streets = download_str/output/V_072001.csv download_str/output/V_012011.csv download_str/output/V_072011.csv download_str/output/V_012012.csv download_str/output/V_072012.csv download_str/output/V_012013.csv download_str/output/V_072013.csv download_str/output/V_012014.csv download_str/output/V_072014.csv download_str/output/V_012015.csv download_str/output/V_072015.csv download_str/output/V_012016.csv download_str/output/V_072016.csv download_str/output/V_012017.csv download_str/output/V_072017.csv download_str/output/V_012018.csv download_str/output/V_072018.csv download_str/output/V_012019.csv download_str/output/V_072019.csv download_str/output/V_012020.csv download_str/output/V_072020.csv
agg_streets = str_agg/output/fs.csv str_agg/output/fs_prov.csv str_agg/output/fs_all.csv
dataset = dataset/output/data.csv dataset/output/dl_VOX.csv dataset/output/dl_PP.csv dataset/output/dl_PSOE.csv
out_main_mod = main_models/output/DiD_estimates.pdf main_models/output/tab_main_did.tex main_models/output/tab_cs.tex
out_robust = robust/output/tab_vox_robustness.tex robust/output/tab_cs_limited_2011.tex robust/output/tab_cs_all_2011.tex robust/output/tab_cs_change.tex robust/output/tab_vox_robustness robust/output/tab_pp_robustness robust/output/tab_logit_fs_rm
out_desc = descriptives/output/fs_by_prov.pdf descriptives/output/changes_by_prov.pdf descriptives/output/fs_by_year.pdf descriptives/output/changes_by_year.pdf descriptives/output/francoist_name_list.tex
tex = writing/main.pdf writing/appendix.pdf

# ------------------------
# Main recipes

all: wsubdir $(dataset) $(out_main_mod) $(out_desc) $(out_robust) taskflow $(tex)

clean:
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
	cp $(<D)/output/*.pdf writing/img
	cp $(<D)/output/*.tex writing/tab

$(out_robust): robust/rob.R $(dataset) func/my_stargazer.R
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out
	cp $(<D)/output/*.tex writing/tab

$(out_desc): descriptives/desc.R input/calles_franquistas.txt str_changes/output/changes.csv $(agg_streets)
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out
	cp $(<D)/output/*.pdf writing/img
	cp $(<D)/output/*.tex writing/tab

# ------------------------
# Latex files

texb = pdflatex -interaction=batchmode

$(tex): writing/main.tex writing/appendix.tex $(out_desc) $(out_robust) $(out_main_mod)
	$(texb) appendix.tex
	bibtex appendix.aux
	$(texb) appendix.tex
	$(texb) appendix.tex
	$(texb) main.tex
	bibtex main.aux
	$(texb) main.tex
	$(texb) main.tex
	find writing | egrep "(aux|log|blg|bbl|out|DS_Store|ent)$" | xargs rm
