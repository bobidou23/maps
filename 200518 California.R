library(pacman)
p_load(tigris, tidycensus, tidyverse, sf, plotly)
#Documentation https://cran.r-project.org/web/packages/tidycensus/tidycensus.pdf

census_api_key("blahblahblah", install=TRUE, overwrite=TRUE)
ca1 <- tracts("CA") %>% st_as_sf()
ca2 <- get_decennial(geography = "tract", variable="P001001", state="CA")

sf_counties <- data.frame(county = c("06001", "06013", "06041", "06075", "06081", #MSA
                                      "06055", "06085", "06095", "06097", #Metro division
                                      "06047", "06069", "06077", "06087", "06099"),
                           name = c("Alameda", "Contra Costa", "Marin", "San Francisco", "San Mateo",
                                    "Napa", "Santa Clara", "Solano", "Sonoma",
                                    "Merced", "San Benito", "San Joaquin", "Santa Cruz", "Stanislaus"),
                           region = c("MSA", "MSA", "MSA", "MSA", "MSA",
                                      "Bay Area", "Bay Area", "Bay Area", "Bay Area",
                                      "CSA", "CSA", "CSA", "CSA", "CSA"),
                           stringsAsFactors = FALSE) %>%
  mutate(region = factor(region, levels=c("CSA","Bay Area","MSA")))

la_counties <- data.frame(county = c("06037", "06059",
                                     "06065", "06071", "06111"),
                          name = c("Los Angeles", "Orange",
                                   "Riverside", "San Bernardino", "Ventura"),
                          region = c("MSA", "MSA",
                                     "CSA", "CSA", "CSA"),
                          stringsAsFactors = FALSE) %>%
  mutate(region = factor(region, levels=c("CSA","MSA")))

la <- left_join(ca1, ca2, by="GEOID") %>%
  mutate(county = paste0(STATEFP, COUNTYFP)) %>%
  filter(county %in% la_counties$county) %>%
  left_join(la_counties, by="county") %>%
  filter(value > 10) %>%
  mutate(ALAND = as.numeric(ALAND),
         AWATER = as.numeric(AWATER),
         density = value/ALAND*10000)

p_load(inlmisc)
la %>%
  # mutate(density = case_when(density < 1 ~ 1, TRUE ~ density)) %>%
  ggplot() +
  geom_sf(aes(fill=density), size=0, alpha=0.6) +
  scale_fill_gradientn(trans = "sqrt", limits=c(0,900), colours = inlmisc::GetColors(256,start=0.2,end=1))
# rainbow(7, rev = TRUE, start=0, end=0.7))
# wes_palette("Zissou1", 7, type = "continuous"))

{ggplot(la) + geom_density(aes(fill=region,x=density,weight=value),position="stack",color=NA,alpha=0.4) +
    scale_x_log10()} #%>% ggplotly()

