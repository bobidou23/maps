library(pacman)
p_load(rvest,tidyverse,sf)

remove.packages("rgdal")
install.packages("rgdal", type = "source")
library(rgdal)

#Layers https://libguides.tru.ca/censuscanada/censustract

#Census Tracts + Dissemination Areas (smaller)
setwd("~/Desktop/Datasets")
url <- "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lda_000b16a_e.zip"
temp <- tempfile()
download.file(url, temp)
unzip(temp)

can_tracts <- st_read("lct_000b16a_e.shp")
can_da <- st_read("lda_000b16a_e.shp")
can_da2 <- read_csv("https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/pd-pl/Tables/CompFile.cfm?Lang=Eng&T=1901&OFT=FULLCSV")
#from https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/page_Download-Telecharger.cfm?Lang=E&Tab=1&Geo1=CT&Code1=5050200.01&Geo2=PR&Code2=01&SearchText=5050200.01&SearchType=Begins&SearchPR=01&B1=All&TABID=3&type=0

setwd("~/Dropbox/Projects/maps")

#Mapping
mtl <- mutate(can_da, "DAUID"=as.double(DAUID)) %>%
  left_join(can_da2, by=c("DAUID"="Geographic code")) %>%
  filter(CMANAME=="Montréal") %>%
  mutate(density = `Population density per square kilometre, 2016`/100,
         density = case_when(density > 900 ~ 900,
                             TRUE ~ density))

toronto <- mutate(can_da, "DAUID"=as.double(DAUID)) %>%
  left_join(can_da2, by=c("DAUID"="Geographic code")) %>%
  filter(CMANAME=="Toronto") %>%
  mutate(density = `Population density per square kilometre, 2016`/100,
         density = case_when(density > 900 ~ 900,
                             TRUE ~ density))

vancouver <- mutate(can_da, "DAUID"=as.double(DAUID)) %>%
  left_join(can_da2, by=c("DAUID"="Geographic code")) %>%
  filter(CMANAME=="Vancouver") %>%
  mutate(density = `Population density per square kilometre, 2016`/100,
         density = case_when(density > 900 ~ 900,
                             TRUE ~ density))

ggplot(vancouver) +
  geom_sf(aes(fill=density),size=0) +
  scale_fill_gradientn(trans = "sqrt", limits=c(0,900), colours = inlmisc::GetColors(256,start=0.2,end=1))



