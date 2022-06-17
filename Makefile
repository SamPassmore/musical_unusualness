GJB_REPO=https://github.com/theglobaljukebox/cantometrics
DPLACE_REPO=https://github.com/D-PLACE/dplace-data.git
GJB=./raw/gjb
DPLACE=./raw/dplace

# Install downloads the necessary data from Github to run the models
install:
	git submodule update --init

clean:
	rm -rf raw figures processed_data

process_data: install
	mkdir -p processed_data/
	RScript processing/prepare_data.R
	RScript processing/prune_tree.R
	Rscript processing/unusualness.R
	RScript processing/dplace_unusualness.R
	RScript processing/standardize_data.R
	Rscript processing/makemodel_data.R

models:
	mkdir -p results/models
	mkdir -p results/networks
	RScript analysis/fake_cantometrics.R
	RScript analysis/model_unusualness.R 
	RScript figure_code/networks.R

plots:
	mkdir -p figures/
	RScript figure_code/society_map.R