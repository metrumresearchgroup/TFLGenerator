TOC=queryChapter(concept = c('table','figure'))
createChapter('Tables','01',funs = list(name=TOC$Name[TOC$output=='table'][4:1],pkg='TFL',type='table'))
createChapter('Figures','02',funs = list(name=TOC$Name[TOC$output=='figure'],pkg='TFL',type='figure'))
