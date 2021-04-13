library(pacman)
p_load(rvest,tidyverse,sf)

remove.packages("rgdal")
install.packages("rgdal", type = "source")
library(rgdal)

#Layers https://libguides.tru.ca/censuscanada/censustract

# Census Tracts + Dissemination Areas (smaller)
# Boundary files https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm
setwd("~/Desktop/Datasets")
url <- "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lda_000b16a_e.zip"
temp <- tempfile()
download.file(url, temp)
unzip(temp)

can_tracts <- st_read("lct_000b16a_e.shp")
can_da <- st_read("lda_000b16a_e.shp")
can_da2 <- read_csv("https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/pd-pl/Tables/CompFile.cfm?Lang=Eng&T=1901&OFT=FULLCSV")
#from https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/page_Download-Telecharger.cfm?Lang=E&Tab=1&Geo1=CT&Code1=5050200.01&Geo2=PR&Code2=01&SearchText=5050200.01&SearchType=Begins&SearchPR=01&B1=All&TABID=3&type=0

can_all <- mutate(can_da, "DAUID"=as.double(DAUID)) %>%
  left_join(can_da2, by=c("DAUID"="Geographic code")) %>%
  mutate(density = `Population density per square kilometre, 2016`/1000)

setwd("~/Dropbox/Projects/maps")

#Mapping
mtl <- filter(can_all, CMANAME=="Montréal") %>%
  mutate(density = case_when(density > 50 ~ 50,
                             TRUE ~ density),
         category = case_when(CDNAME == "Montréal" ~ "Montréal",
                              TRUE ~ "Surrounding"),
         radius = as.numeric(st_distance(st_centroid(geometry),
                                         st_transform(st_sfc(st_point(x=c(-73.567, 45.5), dim="XY"), crs=4326),
                                                      crs=st_crs(can_da))))/1000)

toronto <- filter(can_all, CMANAME=="Toronto" | CMANAME=="Hamilton" | CMANAME=="Oshawa") %>%
  mutate(density = case_when(density > 50 ~ 50,
                             TRUE ~ density),
         category = case_when(CDNAME == "Toronto" ~ "Toronto",
                              TRUE ~ "Surrounding"),
         radius = as.numeric(st_distance(st_centroid(geometry),
                                         st_transform(st_sfc(st_point(x=c(-79.379, 43.645), dim="XY"), crs=4326),
                                                      crs=st_crs(can_da))))/1000)

mtl %>%
  ggplot()+
  geom_point(aes(x=radius,y=density,size=`Population, 2016`,color=category),alpha=0.05) +
  scale_y_continuous(trans="sqrt",limits=c(0,50)) +
  scale_x_continuous(trans="sqrt")



vancouver <- mutate(can_da, "DAUID"=as.double(DAUID)) %>%
  left_join(can_da2, by=c("DAUID"="Geographic code")) %>%
  filter(CMANAME=="Vancouver") %>%
  mutate(density = `Population density per square kilometre, 2016`/1000,
         density = case_when(density > 50 ~ 50,
                             TRUE ~ density))

ggplot(mtl) +
  geom_sf(aes(fill=density),size=0,alpha=0.8) +
  scale_fill_gradientn(trans = "sqrt", limits=c(0,50), colours = inlmisc::GetColors(256,start=0.2,end=1))



