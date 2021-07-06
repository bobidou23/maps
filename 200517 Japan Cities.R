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
setwd("~/Desktop/Datasets/maps")

kanto <- filter(japan_geo, PREF_NAME %in% c("群馬県","栃木県","茨城県","埼玉県","千葉県","東京都","神奈川県","山梨県","静岡県")) %>%
  mutate(centroid = st_centroid(geometry),
         radius = as.numeric(st_distance(centroid,
                                         st_sfc(st_point(x = c(139.767, 35.682), dim = "XY"),crs=4612)))/1000,
         category = case_when(PREF_NAME=="東京都"&str_sub(CITY_NAME,-1)=="区" ~ "Central",
                              GST_NAME %in% c("川崎市","横浜市","相模原市","千葉市","さいたま市") ~ "Large city",
                              CITY_NAME %in% c("水戸市","宇都宮市","前橋市","高崎市","川越市","川口市","越谷市",
                                               "船橋市","柏市","八王子市","横須賀市") ~ "Secondary",
                              CITY_NAME %in% c("つくば市","伊勢崎市","太田市","所沢市","草加市","春日部市","熊谷市",
                                               "小田原市","大和市","平塚市","厚木市","茅ヶ崎市","沼津市") ~ "Tertiary",
                              TRUE ~ "Small")) %>%
  filter(radius < 120)

kanto %>% #map
  filter(is.na(SITYO_NAME), #filters out distant islands in Tokyo Prefecture
         JINKO > 0) %>% #filters out lakes, etc
  mutate(density = case_when(density > 50 ~ 50, TRUE ~ density)) %>%
  ggplot() +
  geom_sf(aes(fill=density), size=0, alpha=0.8) +
  # xlim(139,140.5) +
  # ylim(35,36.25) +
  scale_fill_gradientn(trans = "sqrt", limits=c(0,50), colours = inlmisc::GetColors(256,start=0.2,end=1))

kanto %>% #dotplot
  mutate(density = case_when(density > 50 ~ 50, TRUE ~ density)) %>%
  ggplot() +
  geom_point(aes(x=radius,y=density,size=JINKO,color=category),alpha=0.08) +
  # geom_hline(yintercept=1) +
  # geom_segment(aes(x=50,xend=50,y=1,yend=50)) +
  # scale_x_continuous(trans="sqrt") +
  scale_y_continuous(trans="sqrt",limits=c(0,50)) +
  scale_color_manual(values=c("Central"="hotpink","Large city"="pink","Secondary"="orange",
                              "Tertiary"="gold3","Small"="lightskyblue"))

kanto %>% #density
  filter(is.na(SITYO_NAME)) %>%
  ggplot() +
  geom_density(aes(y=density,weight=JINKO))

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
kansai <- filter(japan_geo, PREF_NAME %in% c("兵庫県","京都府","大阪府","滋賀県","和歌山県","奈良県","福井県","三重県")) %>%
  mutate(centroid = st_centroid(geometry),
         radius = as.numeric(st_distance(centroid,
                                         st_sfc(st_point(x = c(135.495, 34.702), dim = "XY"),crs=4612)))/1000,
         category = case_when(GST_NAME=="大阪市" ~ "Central",
                              GST_NAME %in% c("神戸市","堺市","京都市") ~ "Large city",
                              CITY_NAME %in% c("大津市","高槻市","東大阪市","豊中市","枚方市","八尾市","寝屋川市","吹田市",
                                               "和歌山市","奈良市","尼崎市","西宮市","明石市","姫路市") ~ "Secondary",
                              CITY_NAME %in% c("茨木市","岸和田市","加古川市","宝塚市") ~ "Tertiary",
                              TRUE ~ "Small")) %>%
  filter(radius < 100)

kansai %>% #map
  filter(JINKO > 0) %>% #filters out lakes, etc
  mutate(density = case_when(density > 50 ~ 50, TRUE ~ density)) %>%
  ggplot() +
  geom_sf(aes(fill=density), size=0, alpha=0.8) +
  scale_fill_gradientn(trans = "sqrt", limits=c(0,50), colours = inlmisc::GetColors(256,start=0.2,end=1))

kansai %>% #dotplot
  mutate(density = case_when(density > 50 ~ 50, TRUE ~ density)) %>%
  ggplot() +
  geom_point(aes(x=radius,y=density,size=JINKO,color=category),alpha=0.08) +
  # geom_hline(yintercept=1) +
  # geom_segment(aes(x=50,xend=50,y=1,yend=50)) +
  # scale_x_continuous(trans="sqrt") +
  scale_y_continuous(trans="sqrt",limits=c(0,50)) +
  scale_color_manual(values=c("Central"="hotpink","Large city"="pink","Secondary"="orange",
                              "Tertiary"="gold3","Small"="lightskyblue"))

##NAGOYA
nagoya <- filter(japan_geo, PREF_NAME %in% c("愛知県","岐阜県","三重県","滋賀県")) %>%
  mutate(centroid = st_centroid(geometry),
         radius = as.numeric(st_distance(centroid,
                                         st_sfc(st_point(x = c(136.908,35.170), dim = "XY"),crs=4612)))/1000,
         category = case_when(GST_NAME=="名古屋市" ~ "Central",
                              CITY_NAME %in% c("岐阜市","豊田市","豊橋市","岡崎市","一宮市") ~ "Secondary",
                              CITY_NAME %in% c("春日井市","四日市市") ~ "Tertiary",
                              TRUE ~ "Small")) %>%
  filter(radius < 50)

nagoya %>% #map
  filter(JINKO > 0) %>% #filters out lakes, etc
  mutate(density = case_when(density > 50 ~ 50, TRUE ~ density)) %>%
  ggplot() +
  geom_sf(aes(fill=density), size=0, alpha=0.8) +
  scale_fill_gradientn(trans = "sqrt", limits=c(0,50), colours = inlmisc::GetColors(256,start=0.2,end=1))

nagoya %>% #dotplot
  mutate(density = case_when(density > 50 ~ 50, TRUE ~ density)) %>%
  ggplot() +
  geom_point(aes(x=radius,y=density,size=JINKO,color=category),alpha=0.08) +
  # geom_hline(yintercept=1) +
  # geom_segment(aes(x=50,xend=50,y=1,yend=50)) +
  # scale_x_continuous(trans="sqrt") +
  scale_y_continuous(trans="sqrt",limits=c(0,50)) +
  scale_color_manual(values=c("Central"="hotpink","Secondary"="orange",
                              "Tertiary"="gold3","Small"="lightskyblue"))

##FUKUOKA
fukuoka <- filter(japan_geo, PREF_NAME %in% c("福岡県","佐賀県","山口県")) %>%
  mutate(centroid = st_centroid(geometry),
         radius = as.numeric(st_distance(centroid,
                                         st_sfc(st_point(x = c(130.4,33.589), dim = "XY"),crs=4612)))/1000,
         radius2 = as.numeric(st_distance(centroid,
                                          st_sfc(st_point(x = c(130.882,33.886), dim = "XY"),crs=4612)))/1000,
         category = case_when(GST_NAME=="福岡市" ~ "Central",
                              GST_NAME=="北九州市" ~ "Large city",
                              CITY_NAME %in% c("下関市","久留米市") ~ "Secondary",
                              CITY_NAME %in% c("佐賀市") ~ "Tertiary",
                              TRUE ~ "Small")) %>%
  filter(radius < 50 | radius2 < 25)

fukuoka %>% #map
  filter(JINKO > 0) %>% #filters out lakes, etc
  mutate(density = case_when(density > 50 ~ 50, TRUE ~ density)) %>%
  ggplot() +
  geom_sf(aes(fill=density), size=0, alpha=0.8) +
  scale_fill_gradientn(trans = "sqrt", limits=c(0,50), colours = inlmisc::GetColors(256,start=0.2,end=1))

fukuoka %>% #dotplot
  mutate(density = case_when(density > 50 ~ 50, TRUE ~ density)) %>%
  ggplot() +
  geom_point(aes(x=radius,y=density,size=JINKO,color=category),alpha=0.08) +
  # geom_hline(yintercept=1) +
  # geom_segment(aes(x=50,xend=50,y=1,yend=50)) +
  # scale_x_continuous(trans="sqrt") +
  scale_y_continuous(trans="sqrt",limits=c(0,50)) +
  scale_color_manual(values=c("Central"="hotpink","Large city"="pink","Secondary"="orange",
                              "Tertiary"="gold3","Small"="lightskyblue"))


##SENDAI
sendai <- filter(japan_geo, PREF_NAME %in% c("宮城県")) %>%
  mutate(centroid = st_centroid(geometry),
         radius = as.numeric(st_distance(centroid,
                                         st_sfc(st_point(x = c(140.88,38.259), dim = "XY"),crs=4612)))/1000,
         category = case_when(GST_NAME=="仙台市" ~ "Central",
                              TRUE ~ "Small")) %>%
  filter(radius < 25)

sendai %>% #map
  filter(JINKO > 0) %>% #filters out lakes, etc
  mutate(density = case_when(density > 50 ~ 50, TRUE ~ density)) %>%
  ggplot() +
  geom_sf(aes(fill=density), size=0, alpha=0.8) +
  scale_fill_gradientn(trans = "sqrt", limits=c(0,50), colours = inlmisc::GetColors(256,start=0.2,end=1))

#SAPPORO
sapporo <- filter(japan_geo, PREF_NAME %in% c("北海道")) %>%
  mutate(centroid = st_centroid(geometry),
         radius = as.numeric(st_distance(centroid,
                                         st_sfc(st_point(x = c(141.346,43.069), dim = "XY"),crs=4612)))/1000,
         category = case_when(GST_NAME=="札幌市" ~ "Central",
                              TRUE ~ "Small")) %>%
  filter(radius < 50)

sapporo %>% #map
  filter(JINKO > 0) %>% #filters out lakes, etc
  mutate(density = case_when(density > 50 ~ 50, TRUE ~ density)) %>%
  ggplot() +
  geom_sf(aes(fill=density), size=0, alpha=0.8) +
  scale_fill_gradientn(trans = "sqrt", limits=c(0,50), colours = inlmisc::GetColors(256,start=0.2,end=1))

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

