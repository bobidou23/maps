library(pacman)
p_load(kokudosuuchi,rvest,tidyverse,sf,lwgeom)

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

japan_geo <- data.frame()
for(i in 1:47) {
  japan_geo <- rbind(japan_geo, read_sf(paste0("~/Desktop/Datasets/Japan/h27ka",sprintf("%02s", i),".shp")))
} %>%
  mutate(density = JINKO/AREA*1000)
setwd("~/Dropbox/Projects/maps")

kanto <- filter(japan_geo, PREF_NAME %in% c("群馬県","栃木県","茨城県","埼玉県","千葉県","東京都","神奈川県")) %>%
  mutate(centroid = st_centroid(geometry),
         radius = as.numeric(st_distance(centroid,
                                         st_sfc(st_point(x = c(139.767, 35.682), dim = "XY"),crs=4612)))/1000,
         areaclass = case_when(density == 0 ~ "Zero",
                               density < 1.5 & density > 0 & radius < 50 ~ "Near low",
                               density < 1.5 & density > 0 & radius > 50 ~ "Far low",
                               density > 1.5 & radius > 50 ~ "Far medium",
                               TRUE ~ "Near"))

kanto %>% #map
  filter(is.na(SITYO_NAME), #filters out distant islands in Tokyo Prefecture
         JINKO > 0) %>% #filters out lakes, etc
  mutate(density = case_when(density > 50 ~ 50, TRUE ~ density)) %>%
  ggplot() +
  geom_sf(aes(fill=density), size=0, alpha=0.8) +
  # xlim(139,140.5) +
  # ylim(35,36.25) +
  scale_fill_gradientn(trans = "sqrt", limits=c(0,50), colours = inlmisc::GetColors(256,start=0.2,end=1))

p_load(ggExtra)
kanto %>% #dotplot
  filter(is.na(SITYO_NAME)) %>%
  ggplot() +
  geom_point(aes(x=radius,y=density,size=JINKO,color=PREF),alpha=0.05) +
  # geom_hline(yintercept=1) +
  # geom_segment(aes(x=50,xend=50,y=1,yend=50)) +
  # scale_x_continuous(trans="sqrt") +
  scale_y_continuous(trans="sqrt",limits=c(0,50))

kanto %>% #dotplot
  filter(is.na(SITYO_NAME)) %>%
  ggplot() +
  geom_density(aes(y=density,weight=JINKO))

kanto %>% #map
  filter(is.na(SITYO_NAME)) %>% #filters out distant islands in Tokyo Prefecture
  mutate(density = case_when(density > 50 ~ 50, TRUE ~ density)) %>%
  ggplot() +
  geom_sf(aes(fill=areaclass), size=0, alpha=0.6) +
  # geom_circle(x0=139.767,y0=35.682,r=0.451) +
  scale_fill_manual(values=c("Near"="orange","Far medium"="orange3",
                             "Near low"="lightskyblue1","Far low"="lightskyblue3","Zero"="grey50"))

geom_circle <- function(x0, y0, r, npoints=100, ...) {
  angles <- seq(-pi, pi, length.out=npoints)
  x <- sin(angles) * r
  y <- cos(angles) * r
  circdat <- data.frame(x=x+x0, y=y+y0)
  geom_polygon(data=circdat, 
               mapping=aes(x, y),
               alpha=0, 
               colour="black",
               ...)
}

##KANSAI
kansai <- filter(japan_geo, PREF_NAME %in% c("兵庫県","京都府","大阪府","滋賀県","和歌山県","奈良県")) %>%
  mutate(centroid = st_centroid(geometry),
         radius = as.numeric(st_distance(centroid,
                                         st_sfc(st_point(x = c(135.495, 34.702), dim = "XY"),crs=4612)))/1000)

kansai %>% #map
  filter(is.na(SITYO_NAME), #filters out distant islands in Tokyo Prefecture
         JINKO > 0) %>% #filters out lakes, etc
  mutate(density = case_when(density > 50 ~ 50, TRUE ~ density)) %>%
  ggplot() +
  geom_sf(aes(fill=density), size=0, alpha=0.8) +
  scale_fill_gradientn(trans = "sqrt", limits=c(0,50), colours = inlmisc::GetColors(256,start=0.2,end=1))

kansai %>% #dotplot
  filter(is.na(SITYO_NAME)) %>%
  ggplot() +
  geom_point(aes(x=radius,y=density,size=JINKO,color=PREF),alpha=0.05) +
  # geom_hline(yintercept=1) +
  # geom_segment(aes(x=50,xend=50,y=1,yend=50)) +
  # scale_x_continuous(trans="sqrt") +
  scale_y_continuous(trans="sqrt",limits=c(0,50))

##NAGOYA
nagoya <- filter(japan_geo, PREF_NAME %in% c("愛知県","岐阜県","三重県")) %>%
  mutate(centroid = st_centroid(geometry),
         radius = as.numeric(st_distance(centroid,
                                         st_sfc(st_point(x = c(136.908,35.170), dim = "XY"),crs=4612)))/1000)

nagoya %>% #map
  filter(is.na(SITYO_NAME), #filters out distant islands in Tokyo Prefecture
         JINKO > 0) %>% #filters out lakes, etc
  mutate(density = case_when(density > 50 ~ 50, TRUE ~ density)) %>%
  ggplot() +
  geom_sf(aes(fill=density), size=0, alpha=0.8) +
  scale_fill_gradientn(trans = "sqrt", limits=c(0,50), colours = inlmisc::GetColors(256,start=0.2,end=1))

nagoya %>% #dotplot
  filter(is.na(SITYO_NAME)) %>%
  ggplot() +
  geom_point(aes(x=radius,y=density,size=JINKO,color=PREF),alpha=0.05) +
  # geom_hline(yintercept=1) +
  # geom_segment(aes(x=50,xend=50,y=1,yend=50)) +
  # scale_x_continuous(trans="sqrt") +
  scale_y_continuous(trans="sqrt",limits=c(0,50))

##FUKUOKA
fukuoka <- filter(japan_geo, PREF_NAME == "福岡県" | PREF_NAME == "佐賀県") %>%
  mutate(centroid = st_centroid(geometry),
         radius = as.numeric(st_distance(centroid,
                                         st_sfc(st_point(x = c(130.4,33.589), dim = "XY"),crs=4612)))/1000)

fukuoka %>% #map
  filter(is.na(SITYO_NAME), #filters out distant islands in Tokyo Prefecture
         JINKO > 0) %>% #filters out lakes, etc
  mutate(density = case_when(density > 50 ~ 50, TRUE ~ density)) %>%
  ggplot() +
  geom_sf(aes(fill=density), size=0, alpha=0.8) +
  scale_fill_gradientn(trans = "sqrt", limits=c(0,50), colours = inlmisc::GetColors(256,start=0.2,end=1))

fukuoka %>% #dotplot
  filter(is.na(SITYO_NAME)) %>%
  ggplot() +
  geom_point(aes(x=radius,y=density,size=JINKO,color=PREF),alpha=0.05) +
  # geom_hline(yintercept=1) +
  # geom_segment(aes(x=50,xend=50,y=1,yend=50)) +
  # scale_x_continuous(trans="sqrt") +
  scale_y_continuous(trans="sqrt",limits=c(0,50))


ja_lines <- st_read("~/Desktop/Datasets/ja_lines.shp")

#Testing out the st_area function
kanto %>%
  mutate(area2 = as.numeric(st_area(geometry)),
         perc = area2/AREA) %>%
  # quantile(.$perc, 0.99, na.rm=TRUE)
  ggplot() +
  geom_density(aes(x=perc)) +
    xlim(1,1.0004)

#documentation: https://www.e-stat.go.jp/gis/statmap-search/data?datatype=2&serveyId=A002005212015&downloadType=1
#reference https://www.e-stat.go.jp/gis/statmap-search?page=1&type=2&aggregateUnitForBoundary=A&toukeiCode=00200521&toukeiYear=2015&serveyId=A002005212015&coordsys=1&format=shape
p_load(reldist, magrittr)
wtd.quantile(mutate(kanto, density = JINKO/AREA*10000) %$% density, c(0.25, 0.5, 0.75, 0.99), na.rm=TRUE, kanto$JINKO)

{ggplot(kanto) + geom_density(aes(x=density,weight=JINKO),alpha=0.4) +
    xlim(0,500)}

p_load(usethis)
usethis::create_github_token()

