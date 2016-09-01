TFL_SHINYDIR := /data/shiny-server/TFL\ generator

update_server: $(TFL_SHINYDIR)/server.R $(TFL_SHINYDIR)/ui.R
	cp script/server.R script/ui.R $(TFL_SHINYDIR)

signal_restart: $(TFL_SHINYDIR)/restart.txt
	touch $(TFL_SHINYDIR)/restart.txt

