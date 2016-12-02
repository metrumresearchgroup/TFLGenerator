GUI_VERSION=$(shell grep -i ^[Vv]ersion GUI/DESCRIPTION | cut -d : -d \  -f 2)
GUI_NAME=$(shell grep -i ^[Pp]ackage: GUI/DESCRIPTION | cut -d : -d \  -f 2)

TFL_VERSION=$(shell grep -i ^[Vv]ersion TFL/DESCRIPTION | cut -d : -d \  -f 2)
TFL_NAME=$(shell grep -i ^[Pp]ackage: TFL/DESCRIPTION | cut -d : -d \  -f 2)

GUI_R_FILES := $(wildcard GUI/R/*.R)
GUI_FILES := GUI/DESCRIPTION GUI/NAMESPACE $(R_FILES) $(SRC_FILES)
GUI_DATA_FILES := $(wildcard GUI/data/*.rda)
TFL_SHINYDIR := /data/shiny-server/TFL\ generator

TFL_R_FILES := $(wildcard TFL/R/*.R)
TFL_FILES := TFL/DESCRIPTION TFL/NAMESPACE $(R_FILES) $(SRC_FILES)
TFL_DATA_FILES := $(wildcard TFL/data/*.rda)

.PHONY: install check clean update_deliv build

check: script/pkg/$(GUI_NAME)_$(GUI_VERSION).tar.gz script/pkg/$(TFL_NAME)_$(TFL_VERSION).tar.gz
	R CMD check script/pkg/$(GUI_NAME)_$(GUI_VERSION).tar.gz
	R CMD check script/pkg/$(TFL_NAME)_$(TFL_VERSION).tar.gz

data: GUI/data/DefaultsFirst.rda GUI/data/plotList.rda GUI/data/tabList.rda
	R CMD BATCH --no-save script/DefaultsFirst.R
	R CMD BATCH --no-save script/plotList.R
	R CMD BATCH --no-save script/tabList.R

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
	
update_deliv:
	cp script/pkg/* deliv/script/pkg/
	cp script/server.R script/ui.R deliv/script/
	cp script/shiny-server.conf deliv/script/shiny-server.conf
	cp -R script/nginx deliv/nginx
	cp script/nginx_update.sh deliv/nginx_update.sh
	cp -R deliv tflgenerator 
	zip -r tflgenerator-$(GUI_VERSION).zip tflgenerator
	rm -fr tflgenerator
	

