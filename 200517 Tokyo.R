library(pacman)
p_load(kokudosuuchi,rvest,tidyverse,sf)

#Only town/ward level
# url <- "https://www.esrij.com/cgi-bin/wp/wp-content/uploads/2020/05/japan_ver82.zip"
# temp <- tempfile()
# download.file(url, temp)
# unzip(temp)
# read_sf("japan_ver82/japan_ver82.shp")

url <- "https://www.esrij.com/cgi-bin/wp/wp-content/uploads/2020/05/japan_ver81.zip"
temp <- tempfile()
download.file(url, temp)
unzip(temp)

setwd("~/Desktop/Datasets/Japan")
for (i in 1:47) {
  url <- paste0("https://www.e-stat.go.jp/gis/statmap-search/data?dlserveyId=A002005212015&code=",sprintf("%02d",i),"&coordSys=1&format=shape&downloadType=5")
  temp <- tempfile()
  download.file(url, temp)
  unzip(temp)
}
setwd("~/Dropbox/Projects/maps")

kanto <- rbind(read_sf("~/Desktop/Datasets/Japan/h27ka08.shp"),
               read_sf("~/Desktop/Datasets/Japan/h27ka09.shp"),
               read_sf("~/Desktop/Datasets/Japan/h27ka10.shp"),
               read_sf("~/Desktop/Datasets/Japan/h27ka11.shp"),
               read_sf("~/Desktop/Datasets/Japan/h27ka12.shp"),
               read_sf("~/Desktop/Datasets/Japan/h27ka13.shp"),
               read_sf("~/Desktop/Datasets/Japan/h27ka14.shp"))%>%
  mutate(density = JINKO/AREA*10000)
kanto %>%
  filter(is.na(SITYO_NAME),
         JINKO > 0) %>%
  mutate(density = case_when(density > 900 ~ 900, TRUE ~ density)) %>%
  ggplot() +
  geom_sf(aes(fill=density), size=0, alpha=0.6) +
  scale_fill_gradientn(trans = "sqrt", limits=c(0,900), colours = inlmisc::GetColors(256,start=0.2,end=1))

#Testing out the st_area function
# tokyo %>%
#   mutate(area = as.numeric(st_area(geometry)),
#          perc = area/ALAND) %>%
#   # quantile(.$perc, 0.99, na.rm=TRUE)
#   ggplot() +
#   geom_density(aes(x=perc)) +
#     xlim(.99998,1.00002)

#documentation: https://www.e-stat.go.jp/gis/statmap-search/data?datatype=2&serveyId=A002005212015&downloadType=1
#reference https://www.e-stat.go.jp/gis/statmap-search?page=1&type=2&aggregateUnitForBoundary=A&toukeiCode=00200521&toukeiYear=2015&serveyId=A002005212015&coordsys=1&format=shape
p_load(reldist, magrittr)
wtd.quantile(mutate(kanto, density = JINKO/AREA*10000) %$% density, c(0.25, 0.5, 0.75, 0.99), na.rm=TRUE, kanto$JINKO)

{ggplot(kanto) + geom_density(aes(x=density,weight=JINKO),alpha=0.4) +
    xlim(0,500)}



