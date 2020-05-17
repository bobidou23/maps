library(pacman)
p_load(tigris, tidycensus)

newyork <-  tracts("NY")
newjersey <-  tracts("NJ")
connecticut <- tracts("CT")
# california <- tracts("CA")
# texas <- tracts("TX")
# florida <- tracts("FL")

census_api_key("e8a1ede4f588fcd321a6f9216794826faaf07bb5", install=TRUE)
newyork2 <- get_decennial(geography = "tract")

