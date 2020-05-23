library(pacman)
p_load(kokudosuuchi,rvest,tidyverse,sf)


#Layers https://libguides.tru.ca/censuscanada/censustract

#Census tract + Dessemination 
setwd("~/Desktop/Datasets")
url <- "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lda_000b16a_e.zip"
temp <- tempfile()
download.file(url, temp)
unzip(temp)

can_tracts <- st_read("lct_000b16a_e.shp")
can <- st_read("lda_000b16a_e.shp")
can_all <- read_csv("https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/download-telecharger/comp/GetFile.cfm?Lang=E&FILETYPE=CSV&GEONO=044")
#from https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/page_Download-Telecharger.cfm?Lang=E&Tab=1&Geo1=CT&Code1=5050200.01&Geo2=PR&Code2=01&SearchText=5050200.01&SearchType=Begins&SearchPR=01&B1=All&TABID=3&type=0

setwd("~/Dropbox/Projects/maps")


#Mapping
filter(can, CMANAME=="Montréal") %>%
  ggplot() +
  geom_sf()

