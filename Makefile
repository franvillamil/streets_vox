.PHONY: all clean taskflow wsubdir
.DELETE_ON_ERROR:

# ------------------------
# Variables

out_data = dataset/output/data.csv dataset/dataset.Rout
# out_desc = descriptives/desc.Rout
# out_lm = lm/lm.Rout
# out_robust = lm_robust/robust.Rout
# out_alt = alt_exp/alt.Rout

# ------------------------
# Main recipes

all: download_elec/output/elec.csv $(out_data) #$(out_desc) $(out_lm) $(out_robust) $(out_alt) taskflow

clean:
	rm -rvf $(out_data) $(out_desc) $(out_lm) $(out_robust) $(out_alt)
	rm -rvf */output/*

taskflow:
	Rscript --no-save --verbose taskflow/create_dependency_graph.R
	dot -Grankdir=LR -Tpdf taskflow/dependency_list.txt -o taskflow/workflow.pdf
	sips -s format jpeg taskflow/workflow.pdf --out taskflow/workflow.jpeg

wsubdir:
	mkdir -p writing/tab writing/img

# ------------------------
# Data

download_elec/output/elec.csv: download_elec/elec.R
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

$(out_data): dataset/dataset.R input/unemployment_01_2019.csv input/unemployment_01_2016.csv input/major_izq_muni.csv input/INE_census.csv input/francoist_streets.csv download_elec/output/elec.csv
	mkdir -p $(@D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out


Rscript --no-save --verbose dataset/dataset.R 2>&1 | tee dataset/dataset.Rout
