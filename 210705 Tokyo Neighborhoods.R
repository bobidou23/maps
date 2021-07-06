library(pacman)
p_load(stringr,magrittr,rvest,tidyverse,sf,lwgeom)
theme_set(theme_gray(base_size=12, base_family="HiraKakuProN-W3"))

tokyo23_lines <- filter(japan_geo,PREF_NAME=="東京都",str_sub(CITY_NAME,-1)=="区") %>% 
  group_by(CITY_NAME) %>%
  summarise(geometry = st_union(geometry))

tokyo23 <- filter(japan_geo,PREF_NAME=="東京都",str_sub(CITY_NAME,-1)=="区",
                    str_sub(S_NAME,end=6)!="多摩川河川敷",!(S_NAME %in% c("有明","羽田沖水面","水面調査区","宮城２丁目","鹿浜２丁目")),KBSUM>0) %>% 
  mutate(chomei = sub("[１２３４５６７８９].*","",S_NAME),
         chomei2 = case_when((str_sub(paste0(CITY_NAME,chomei),end=6)%in%c("中央区日本橋","新宿区早稲田")) ~ str_sub(chomei,end=3),
                             str_sub(paste0(CITY_NAME,chomei),end=5)=="板橋区大山" ~ "大山町",
                            str_sub(paste0(CITY_NAME,chomei),end=6)=="千代田区神田" ~ "神田",
                            (str_sub(paste0(CITY_NAME,chomei),end=5) %in% c("足立区千住","新宿区市谷")) ~ str_sub(chomei,end=2),
                            (nchar(chomei)>2 & str_sub(chomei,start=-2) %in% c("本町","公園")) ~ str_sub(chomei,end=-3),
                            (nchar(chomei)>2 & str_sub(chomei,start=-1) %in% c("東","西","北","南","中") & str_sub(chomei,start=-2)!="葛西") ~ str_sub(chomei,end=-2),
                            ((nchar(chomei)>2 | str_sub(chomei,start=-1)%in%c("砂","荻")) & str_sub(chomei,end=1) %in% c("東","西","北","南","中","上","下","本","新","仲","外","内","元")) ~ str_sub(chomei,start=2),
                            TRUE ~ chomei),
         prefix2 = case_when((str_sub(paste0(CITY_NAME,chomei),end=6)%in%c("中央区日本橋","新宿区早稲田")) ~ str_sub(chomei,start=4),
                             str_sub(paste0(CITY_NAME,chomei),end=5)=="板橋区大山" ~ str_sub(chomei,start=3,end=-2),
                             str_sub(paste0(CITY_NAME,chomei),end=6)=="千代田区神田" ~ str_sub(chomei,start=3),
                             (str_sub(paste0(CITY_NAME,chomei),end=5)%in%c("足立区千住","新宿区市谷"))~ str_sub(chomei,start=3), #keep these 3 first
                             (nchar(chomei)>2 & str_sub(chomei,start=-2) %in% c("本町","公園")) ~ str_sub(chomei,start=-2),
                             (nchar(chomei)>2 & str_sub(chomei,start=-1) %in% c("東","西","北","南","中") & str_sub(chomei,start=-2)!="葛西") ~ str_sub(chomei,start=-1),
                             ((nchar(chomei)>2|str_sub(chomei,start=-1)%in%c("砂","荻")) & str_sub(chomei,end=1) %in% c("東","西","北","南","中","上","下","本","新","仲","外","内","元")) ~ str_sub(chomei,end=1),
                             TRUE ~ "-")) %>% #a definition that doesn't include 町 at all
  group_by(CITY_NAME, chomei2) %>%
  mutate(nprefix = length(unique(prefix2)), #keeping only the ones with multiple 
         chomei3 = case_when(nprefix>1~chomei2, TRUE~ chomei),
         prefix3 = case_when(nprefix>1~prefix2, TRUE~"")) %>%
  group_by(CITY_NAME) %>% #now we account for all neighborhoods that are [name already recognized here] + something else
  mutate(chomei4 = case_when(str_sub(chomei3,end=2) %in% unique(chomei3) ~ str_sub(chomei3,end=2),
                             str_sub(chomei3,end=3) %in% unique(chomei3) ~ str_sub(chomei3,end=3),
                             str_sub(chomei3,start=-2) %in% unique(chomei3) ~ str_sub(chomei3,start=-2),
                             str_sub(chomei3,start=-3) %in% unique(chomei3) ~ str_sub(chomei3,start=-3),
                             str_sub(chomei3,start=2,end=3) %in% unique(chomei3) ~ str_sub(chomei3,start=2,end=3),
                             str_sub(chomei3,start=2,end=4) %in% unique(chomei3) ~ str_sub(chomei3,start=2,end=4),
                             str_sub(chomei3,start=-3,end=-1) %in% unique(chomei3) ~ str_sub(chomei3,start=-3,end=-1),
                             str_sub(chomei3,start=-4,end=-1) %in% unique(chomei3) ~ str_sub(chomei3,start=-4,end=-1),
                             TRUE~chomei3),
         prefix4 = case_when((nchar(chomei3)>2 & str_sub(chomei3,end=2)%in%unique(chomei3)) ~ str_sub(chomei3,start=3),
                             (nchar(chomei3)>3 & str_sub(chomei3,end=3) %in% unique(chomei3)) ~ str_sub(chomei3,start=4),
                             (nchar(chomei3)>2 & str_sub(chomei3,start=-2)%in%unique(chomei3)) ~ str_sub(chomei3,end=-3),
                             (nchar(chomei3)>3 & str_sub(chomei3,start=-3)%in%unique(chomei3)) ~ str_sub(chomei3,end=-4),
                             str_sub(chomei3,start=2,end=3) %in% unique(chomei3) ~ paste0(str_sub(chomei3,end=1),"・",str_sub(chomei3,start=4)),
                             str_sub(chomei3,start=2,end=4) %in% unique(chomei3) ~ paste0(str_sub(chomei3,end=1),"・",str_sub(chomei3,start=5)),
                             str_sub(chomei3,start=-3,end=-2) %in% unique(chomei3) ~ paste0(str_sub(chomei3,end=-4),"・",str_sub(chomei3,start=-1)),
                             str_sub(chomei3,start=-4,end=-2) %in% unique(chomei3) ~ paste0(str_sub(chomei3,end=-5),"・",str_sub(chomei3,start=-1)),
                             TRUE~prefix3),
         prefix4 = case_when(prefix3==prefix4 ~ prefix3,
                             prefix3=="-" | prefix3=="" ~ prefix4,
                             TRUE ~ paste0(prefix3,"・",prefix4)),
         chomei4 = case_when(chomei%in%c("上北沢","新木場","玉川田園調布","東玉川","浅草橋")~chomei,
                             chomei=="芝大門"~"芝", TRUE~chomei4),
         prefix4 = case_when(chomei%in%c("上北沢","新木場","玉川田園調布","東玉川","浅草橋")~"",
                             chomei=="芝大門"~"大門", TRUE~prefix4))
  
tokyo23_fine <- group_by(tokyo23, CITY_NAME, chomei4, prefix4) %>%
  filter(prefix4 != "") %>%
  group_by(CITY_NAME, chomei4, prefix4) %>%
  summarise(geometry = st_union(geometry),
            chomei4 = head(chomei4,1),
            prefix4 = head(prefix4,1))

############
tokyotama <- filter(japan_geo,PREF_NAME=="東京都",(str_sub(CITY_NAME,-1)=="町"|str_sub(CITY_NAME,-1)=="市"),
                    CITY_NAME!="大島町",CITY_NAME!="八丈町",CITY_NAME!="奥多摩町") %>% 
  

tokyotama_lines <- filter(japan_geo,PREF_NAME=="東京都",(str_sub(CITY_NAME,-1)=="町"|str_sub(CITY_NAME,-1)=="市"),
                  CITY_NAME!="大島町",CITY_NAME!="八丈町",CITY_NAME!="奥多摩町") %>% 
  group_by(CITY_NAME) %>%
  summarise(geometry = st_union(geometry))

tokyo23 %>%
  group_by(CITY_NAME, chomei4) %>%
  summarise(geometry = st_union(geometry),
            chomei4 = head(chomei4,1),
            JINKO = sum(JINKO),
            AREA = sum(AREA)) %>%
  ggplot() +
  geom_sf(aes(fill=JINKO),alpha=0.1,size=0.1) +
  geom_sf_text(aes(label=chomei4,size=AREA),family="HiraKakuProN-W3", show.legend = F) +
  geom_sf_text(data=tokyo23_fine,aes(label=prefix4),size=1.3,alpha=0.5,family="HiraKakuProN-W3") +
  geom_sf(data=tokyo23_lines,size=0.5,fill=NA) +
  scale_size(range=c(0.2,7)) +
  scale_fill_distiller(palette="YlGnBu",direction=1,trans="sqrt") +
  labs(title="The Neighborhoods of Tokyo",
       fill="Population") +
  theme_void() +
  theme(text = element_text(family="Fira Sans"),
        plot.title = element_text(size=36))


