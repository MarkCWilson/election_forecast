## graphics.R
## Mark C. Wilson <wilson.mark.c@gmail.com>
##
## Purpose: produce cool and relevant pictures
##
#install.packages("sf")
#install.packages("gganimate")
#install.packages("gapminder")
#install.packages("gifski")

library(gganimate)
library(gapminder)
library(gifski)
library(sf)
library(ggplot2)

# do after other analyses or move fips back to factor


vote_data_clean <- vote_data_clean %>% mutate(fips = as.integer(levels(fips)[fips]))
counties_sf <- read_sf("data/counties.geojson")
counties_sf <- counties_sf %>% mutate(GEOID = as.integer(GEOID))
counties_sf_merged <- counties_sf %>%
  inner_join(vote_data_clean, by = c("GEOID" = "fips")) %>%
  mutate(demfrac=DEMOCRAT/(DEMOCRAT+REPUBLICAN+OTHER),
         repfrac=REPUBLICAN/(DEMOCRAT+REPUBLICAN+OTHER),
         othfrac=OTHER/(DEMOCRAT+REPUBLICAN+OTHER))

lower_48 <- counties_sf_merged %>% 
  filter(!(STATEFP == "15" | STATEFP == "02" ))

alaska_merged <- counties_sf_merged %>% 
  filter(STATEFP=="02")

hawaii_merged <- counties_sf_merged %>% 
  filter(STATEFP=="15")

lower_48_merged_2020 <- lower_48 %>% 
  filter(year==2020) 

alaska_merged_2020 <- alaska_merged %>% 
  filter(year==2020)

hawaii_merged_2020 <- hawaii_merged %>% 
  filter(year==2020)

qq<- ggplot(lower_48_merged_2020) + geom_sf(aes(fill = demfrac/(demfrac+repfrac)), alpha = 0.7) +
  theme_void() + guides(fill="none") +scale_fill_gradient(low = "#FF0000", high="#0000FF", limits = c(0,1))

s <- ggsave("output/figures/votemap_lower48_2020.png", width = 10, height = 10) 

qq<- ggplot(alaska_merged_2020) + geom_sf(aes(fill = demfrac/(demfrac+repfrac)), alpha = 0.7) +
  theme_void() + guides(fill="none") +scale_fill_gradient(low = "#FF0000", high="#0000FF", limits = c(0,1))

s <- ggsave("output/figures/votemap_alaska_2020.png", width = 10, height = 10) 

qq<- ggplot(hawaii_merged_2020) + geom_sf(aes(fill = demfrac/(demfrac+repfrac)), alpha = 0.7) +
  theme_void() + guides(fill="none") +scale_fill_gradient(low = "#FF0000", high="#0000FF", limits = c(0,1))

s <- ggsave("output/figures/votemap_hawaii_2020.png", width = 10, height = 10) 


