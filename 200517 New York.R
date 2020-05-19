library(pacman)
p_load(tigris, tidycensus, tidyverse, sf, plotly)
#Documentation https://cran.r-project.org/web/packages/tidycensus/tidycensus.pdf

ny <- tracts("NY") %>% st_as_sf()
nj <- tracts("NJ") %>% st_as_sf()
ct <- tracts("CT") %>% st_as_sf()
# california <- tracts("CA")
# texas <- tracts("TX")
# florida <- tracts("FL")

census_api_key("blahblahblah", install=TRUE, overwrite=TRUE)
ny2 <- get_decennial(geography = "tract", variable="P001001", state="NY")
nj2 <- get_decennial(geography = "tract", variable="P001001", state="NJ")
ct2 <- get_decennial(geography = "tract", variable="P001001", state="CT")

#County codes: https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/home/?cid=nrcs143_013697
nyc_counties <- data.frame(county = c("36005", "36047", "36061", "36081", #Central city
                                      "36079", "36085", "36087", "36119", "34003", "34017", "34031", #Metro division
                                      "36059", "36103", 
                                      "34023", "34025", "34029", "34035", 
                                      "34013", "34019", "34027", "34037", "34039", "42103", #MSA
                                      "09001", "09005", "09009", "34021", "36027", "36071", "36111", "42089"),
                           name = c("Bronx", "Kings", "New York", "Queens",
                                    "Putnam", "Richmond", "Rockland", "Westchester", "Bergen", "Hudson", "Passaic",
                                    "Nassau", "Suffolk",
                                    "Middlesex", "Monmouth", "Ocean", "Somerset",
                                    "Essex", "Hunterdon", "Morris", "Sussex", "Union", "Pike",
                                    "Fairfield", "Litchfield", "New Haven", "Mercer", "Dutchess", "Orange", "Ulster", "Monroe"),
                           region = c("NYC", "NYC", "NYC", "NYC",
                                      "Division", "Division", "Division", "Division", "Division", "Division", "Division",
                                      "MSA", "MSA",
                                      "MSA", "MSA", "MSA", "MSA",
                                      "MSA", "MSA", "MSA", "MSA", "MSA", "MSA",
                                      "CSA", "CSA", "CSA", "CSA", "CSA", "CSA", "CSA", "CSA"),
                           stringsAsFactors = FALSE) %>%
  mutate(region = factor(region, levels=c("CSA","MSA","Division","NYC")))

#Rail
url <- "http://www2.census.gov/geo/tiger/TIGER2015/RAILS/tl_2015_us_rails.zip"
temp <- tempfile()
download.file(url, temp)
unzip(temp)

#Geography
nyc <- rbind(left_join(ny1, ny2, by="GEOID"),
                 left_join(nj1, nj2, by="GEOID"),
                 left_join(ct1, ct2, by="GEOID")) %>%
  mutate(county = paste0(STATEFP, COUNTYFP)) %>%
  filter(county %in% nyc_counties$county) %>%
  left_join(nyc_counties, by="county") %>%
  filter(value > 10) %>%
  mutate(ALAND = as.numeric(ALAND),
         AWATER = as.numeric(AWATER),
         density = value/ALAND*10000)

p_load(wesanderson, inlmisc)
nyc %>%
  # mutate(density = case_when(density < 1 ~ 1,
  #                            TRUE ~ density)) %>%
  ggplot() +
  geom_sf(aes(fill=density), size=0, alpha=0.6) +
  scale_fill_gradientn(trans = "sqrt", limits=c(0,900), colours = inlmisc::GetColors(256,start=0.2,end=1))
                         # rainbow(7, rev = TRUE, start=0, end=0.7))
                         # wes_palette("Zissou1", 7, type = "continuous"))

{ggplot(nyc) + geom_density(aes(fill=region,x=density,weight=value),position="stack",color=NA,alpha=0.4) +
    scale_x_log10()} #%>% ggplotly()
#1. Fix scale so that plot_ly shows original value. Also, stop scientific notation


