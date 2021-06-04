.PHONY: all clean taskflow wsubdir
.DELETE_ON_ERROR:

# ------------------------
# Variables

raw_streets = download_str/output/V_072001.csv download_str/output/V_012011.csv download_str/output/V_072011.csv download_str/output/V_012012.csv download_str/output/V_072012.csv download_str/output/V_012013.csv download_str/output/V_072013.csv download_str/output/V_012014.csv download_str/output/V_072014.csv download_str/output/V_012015.csv download_str/output/V_072015.csv download_str/output/V_012016.csv download_str/output/V_072016.csv download_str/output/V_012017.csv download_str/output/V_072017.csv download_str/output/V_012018.csv download_str/output/V_072018.csv download_str/output/V_012019.csv download_str/output/V_072019.csv download_str/output/V_012020.csv download_str/output/V_072020.csv
agg_streets = str_agg/output/fs.csv str_agg/output/fs_prov.csv str_agg/output/fs_all.csv
dataset = dataset/output/data.csv dataset/output/dl_VOX.csv dataset/output/dl_PP.csv dataset/output/dl_PSOE.csv
out_main_mod = main_models/output/DiD_estimates.pdf main_models/output/tab_main_did.tex main_models/output/tab_cs.tex
out_robust = robust/output/tab_cs_limited_2011.tex robust/output/tab_cs_all_2011.tex robust/output/tab_cs_change.tex robust/output/tab_vox_robustness.tex robust/output/tab_psoe_robustness.tex robust/output/tab_pp_robustness.tex robust/output/tab_logit_fs_rm.tex
out_desc = descriptives/output/francoist_name_list.tex descriptives/output/changes_by_year.pdf descriptives/output/fs_by_year.pdf descriptives/output/changes_by_prov.pdf descriptives/output/fs_by_prov.pdf descriptives/output/ttest_sample_fs2016.tex descriptives/output/ttest_sample_fs2001.tex descriptives/output/tab_insample.tex descriptives/output/tab_insample2001.tex descriptives/output/trt_strength.pdf descriptives/output/trt_remaining.pdf descriptives/output/trt_strength_st2016.pdf descriptives/output/mean_trt_treated.tex descriptives/output/par_trends.pdf
tex = writing/main.pdf writing/appendix.pdf


# ------------------------
# Main recipes

all: empirics paper taskflow
paper: $(tex)
empirics: $(dataset) $(out_desc) $(out_main_mod) $(out_robust)

clean:
	rm -rvf */output/*
	rm -rvf *.Rout

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
	Rscript --no-save --verbose $< 2>&1 | tee $(<F)out

str_changes/output/changes.csv: str_changes/str_chg.R $(raw_streets)
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $(<F)out

$(agg_streets): str_agg/agg.R input/calles_franquistas.txt str_changes/output/changes.csv $(raw_streets)
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $(<F)out

download_elec/output/elec.csv: download_elec/elec.R
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $(<F)out

$(dataset): dataset/dataset.R input/unemployment_01_2019.csv input/unemployment_01_2016.csv input/major_izq_muni.csv input/INE_census.csv str_agg/output/fs.csv download_elec/output/elec.csv
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $(<F)out

# ------------------------
# Analyses and descriptives

$(out_main_mod): main_models/mod.R func/functions_did.R func/my_stargazer.R $(dataset)
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $(<F)out

$(out_robust): robust/rob.R $(dataset) func/my_stargazer.R
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $(<F)out

$(out_desc): descriptives/desc.R input/calles_franquistas.txt str_changes/output/changes.csv $(agg_streets)
	mkdir -p $(<D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $(<F)out


# ------------------------
# Latex files

texb = pdflatex -interaction=batchmode

$(tex): writing/main.tex writing/appendix.tex $(out_desc) $(out_robust) $(out_main_mod) | wsubdir
	cp */output/*.pdf writing/img;cp */output/*.tex writing/tab
	$(texb) appendix.tex
	bibtex appendix.aux
	$(texb) appendix.tex
	$(texb) appendix.tex
	$(texb) main.tex
	bibtex main.aux
	$(texb) main.tex
	$(texb) main.tex
	find writing | egrep "(aux|log|blg|bbl|out|DS_Store|ent)$" | xargs rm
