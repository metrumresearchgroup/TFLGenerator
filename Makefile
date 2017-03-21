TFL_VERSION=$(shell grep -i ^[Vv]ersion TFL/DESCRIPTION | cut -d : -d \  -f 2)
TFL_NAME=$(shell grep -i ^[Pp]ackage: TFL/DESCRIPTION | cut -d : -d \  -f 2)

TFL_SHINYDIR := /data/shiny-server/TFL\ generator

TFL_R_FILES := $(wildcard TFL/R/*.R)
TFL_FILES := TFL/DESCRIPTION TFL/NAMESPACE $(R_FILES) $(SRC_FILES)
TFL_DATA_FILES := $(wildcard TFL/data/*.rda)

.PHONY: install check clean update_deliv build

check: script/pkg/$(TFL_NAME)_$(TFL_VERSION).tar.gz
	R CMD check script/pkg/$(TFL_NAME)_$(TFL_VERSION).tar.gz

data: TFL/data/cleanScales.rda
	R CMD BATCH --no-save script/createScales.R

update_documentation:
	R --vanilla -e '.libPaths("script/lib");library(devtools);document("TFL")'

build: script/pkg/$(TFL_NAME)_$(TFL_VERSION).tar.gz
	R CMD build $(TFL_NAME)
	mv $(TFL_NAME)_$(TFL_VERSION).tar.gz script/pkg/$(TFL_NAME)_$(TFL_VERSION).tar.gz

install: script/pkg/$(TFL_NAME)_$(TFL_VERSION).tar.gz
	R CMD INSTALL script/pkg/$(TFL_NAME)_$(TFL_VERSION).tar.gz -l script/lib
	
update_server: $(TFL_SHINYDIR)/server.R $(TFL_SHINYDIR)/ui.R
	cp script/server.R script/ui.R $(TFL_SHINYDIR)

signal_restart: $(TFL_SHINYDIR)/restart.txt
	touch $(TFL_SHINYDIR)/restart.txt

clean:
	-rm -f script/pkg/$(TFL_NAME)_*.tar.gz
	-rm -fr script/lib/$(TFL_NAME)
	
update_deliv:
	rm deliv/script/pkg/*
	cp script/pkg/* deliv/script/pkg/
	cp script/server.R script/ui.R deliv/script/
	cp script/shiny-server.conf deliv/script/shiny-server.conf
	cp -R script/nginx deliv/nginx
	cp script/nginx_update.sh deliv/nginx_update.sh
	cp -R deliv tflgenerator 
	rm -fr tflgenerator
	

