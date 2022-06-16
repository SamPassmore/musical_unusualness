GJB_REPO=https://github.com/theglobaljukebox/cantometrics
DPLACE_REPO=https://github.com/D-PLACE/dplace-data.git
GJB=./raw/gjb
DPLACE=./raw/dplace

$(GJB):
	mkdir -p $(GJB)
	git clone $(GJB_REPO) $(GJB)

$(DPLACE):
	mkdir -p $(DPLACE)
	git clone $(DPLACE_REPO) $(DPLACE)
	cd $(DPLACE) && git checkout ab2a00221c349008a30c11d82440697fd52c0a75

# Install downloads the necessary data from Github to run the models
install: $(GJB) $(DPLACE)

clean:
	rm -rf raw figures processed_data

process_data: install
	mkdir -p processed_data/
	RScript processing/prepare_data.R
	RScript processing/prune_tree.R
	Rscript unusualness.R
	RScript processing/dplace_unusualness.R
	RScript processing/standardize_data.R
	Rscript processing/makemodel_data.R

models:
	RScript musical_morphospace.R
	RScript model_unusualness.R 
	RScript figure_code/networks.R

plots:
	mkdir -p figures/
	RScript figure_code/society_map.R