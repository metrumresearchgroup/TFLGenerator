GUI_VERSION=$(shell grep -i ^version GUI/DESCRIPTION | cut -d : -d \  -f 2)
GUI_NAME=$(shell grep -i ^package: GUI/DESCRIPTION | cut -d : -d \  -f 2)

TFL_VERSION=$(shell grep -i ^version TFL/DESCRIPTION | cut -d : -d \  -f 2)
TFL_NAME=$(shell grep -i ^package: TFL/DESCRIPTION | cut -d : -d \  -f 2)

GUI_R_FILES := $(wildcard GUI/R/*.R)
GUI_FILES := GUI/DESCRIPTION GUI/NAMESPACE $(R_FILES) $(SRC_FILES)
GUI_DATA_FILES := $(wildcard GUI/data/*.rda)
TFL_SHINYDIR := /data/shiny-server/TFL\ Generator

TFL_R_FILES := $(wildcard TFL/R/*.R)
TFL_FILES := TFL/DESCRIPTION TFL/NAMESPACE $(R_FILES) $(SRC_FILES)
TFL_DATA_FILES := $(wildcard TFL/data/*.rda)

.PHONY: install check clean

check: script/pkg/$(GUI_NAME)_$(GUI_VERSION).tar.gz script/pkg/$(TFL_NAME)_$(TFL_VERSION).tar.gz
	R CMD check script/pkg/$(GUI_NAME)_$(GUI_VERSION).tar.gz
	R CMD check script/pkg/$(TFL_NAME)_$(TFL_VERSION).tar.gz

data: GUI/data/DefaultsFirst.rda GUI/data/plotList.rda
	R CMD BATCH --no-save script/DefaultsFirst.R
	R CMD BATCH --no-save script/plotList.R

build: script/pkg/$(GUI_NAME)_$(GUI_VERSION).tar.gz script/pkg/$(TFL_NAME)_$(TFL_VERSION).tar.gz
	R CMD build $(GUI_NAME) 
	R CMD build $(TFL_NAME)
	mv $(GUI_NAME)_$(GUI_VERSION).tar.gz script/pkg/$(GUI_NAME)_$(GUI_VERSION).tar.gz
	mv $(TFL_NAME)_$(TFL_VERSION).tar.gz script/pkg/$(TFL_NAME)_$(TFL_VERSION).tar.gz

install: script/pkg/$(GUI_NAME)_$(GUI_VERSION).tar.gz script/pkg/$(TFL_NAME)_$(TFL_VERSION).tar.gz
	R CMD INSTALL script/pkg/$(GUI_NAME)_$(GUI_VERSION).tar.gz -l script/lib
	R CMD INSTALL script/pkg/$(TFL_NAME)_$(TFL_VERSION).tar.gz -l script/lib
	
update_server: $(TFL_SHINYDIR)/server.R $(TFL_SHINYDIR)/ui.R
	cp script/server.R script/ui.R $(TFL_SHINYDIR)

signal_restart: $(TFL_SHINYDIR)/restart.txt
  touch $(TFL_SHINYDIR)/restart.txt

clean:
	-rm -f script/pkg/$(GUI_NAME)_*.tar.gz
	-rm -f script/pkg/$(TFL_NAME)_*.tar.gz
	-rm -fr script/lib/$(GUI_NAME)
	-rm -fr script/lib/$(TFL_NAME)
