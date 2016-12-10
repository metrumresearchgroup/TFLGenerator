load(file.path(getwd(),"TFL","data","cleanTheme.rda"))

themeEditorDefaults=list(
  themeTextSize=cleanTheme$text$size[[1]],
  themePlotTitleSize=cleanTheme$plot.title$size[[1]],
  themeAxisTxtSize=cleanTheme$axis.text$size[[1]],
  themeAxisTxtColour='black',
  themeAxisTitleTxtSize=cleanTheme$axis.title$size[[1]],
  themeAxisTitleColour='black',
  themePanelBackgroundFill=cleanTheme$panel.background$fill,
  themePanelGridSize=NULL,
  themePanelGridColour='white',
  themePanelLineType=1,
  themePanelTitleSize=NULL,
  themePlotTitleColour='black',
  themePlotLegendPosition="right"
)

save(themeEditorDefaults,file=file.path(getwd(),"GUI","data","themeEditorDefaults.rda")) # Expects to run from top of repo
