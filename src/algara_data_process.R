#####################################################
##### Load libraries and helper functions

library(tidyverse)

#####################################################
##### Import data
setwd("/Users/mwilson/Documents/GitHub/election_forecast/")
load("data/County_Level_US_Elections_Data/dataverse_shareable_presidential_county_returns_1868_2020.Rdata")

df<- pres_elections_release %>% 
     filter(election_year > 1944) %>%
     select(election_year:fips,state,democratic_raw_votes,republican_raw_votes,
            raw_county_vote_totals) %>%
     rename(year = election_year, state = state, fips = fips,  totalvotes = raw_county_vote_totals, 
            DEMOCRAT = democratic_raw_votes, REPUBLICAN = republican_raw_votes) %>%
     mutate(OTHER = totalvotes - DEMOCRAT - REPUBLICAN) %>%
     group_by(fips) %>%
     mutate(lagDEM = lag(DEMOCRAT, n=1, order_by=fips), 
         lagREP = lag(REPUBLICAN, n=1, order_by=fips),
         lagOTH = lag(OTHER, n=1, order_by=fips)) %>%
  select(year,state,fips,totalvotes,DEMOCRAT, REPUBLICAN,OTHER, 
         lagDEM, lagREP, lagOTH)

write.csv(df, "data/temp/vote_data_clean_algara.csv", row.names=FALSE)
vote_data_clean_algara <- df