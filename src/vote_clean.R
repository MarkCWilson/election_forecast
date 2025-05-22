## vote_clean.R
## Mark C. Wilson <wilson.mark.c@gmail.com>
##
## Purpose: import and clean vote data, add and remove variables
##

#####################################################
##### Load libraries and helper functions

library(tidyverse)
library(tidymodels)

#####################################################
##### Output directory for figures

if (!dir.exists('output/figures')){
  dir.create('output/figures')
} else {
  
}

#####################################################
##### Import data

vote_data_orig <- as_tibble(read.table('data/pres_county_1976_2020/countypres_2000-2020.tab', header=FALSE,sep="\t",strip.white=TRUE, quote="\""))

#####################################################
##### Explore data

dim(vote_data_orig)
vote_g <- vote_data_orig %>% glimpse()

# proper column names
cnames = c("year", "state", "stabb", "county", "fips", "office", 
           "candidate", "party", "votes", "totalvotes", "datadate","dataform")
vote_data_clean <- vote_data_orig
colnames(vote_data_clean) = cnames

# some states report different categories, e.g. absentee, provisional, for each candidate
vote_data_clean <- vote_data_clean %>% group_by (across(c(-votes,-dataform))) %>%
  summarize(votes = sum(votes)) %>% ungroup()


# fips code changes since 2000 - remove cities merging, not quite satisfactory, not done
# https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf

vote_data_clean <- vote_data_clean %>% mutate(fips = ifelse(fips==46113,46102,fips)) %>% 
  filter (!fips == 51515)

# Kansas City problem still - just delete, not ideal but it spans counties

vote_data_clean <-  vote_data_clean %>% filter(!fips == 2938000)

#####################################################
##### Tidy data format

# messes things up otherwise, not sure why
vote_data_clean <- vote_data_clean %>% select(-candidate)
vote_data_clean <- vote_data_clean %>% 
  pivot_wider(names_from = party, values_from = votes)

#####################################################
##### Restrict to relevant data - major choices needed

vote_data_clean <- vote_data_clean %>% 
  select(!office & !state:county & !datadate) 

#####################################################
##### Fix remaining data format problems, improve variable names

vote_data_clean <- vote_data_clean %>% 
  replace(is.na(.), 0) %>%
  mutate(fips = as.factor(fips))

#####################################################
##### Add useful derived variables


# is it useful to have a 2-lag, etc?

vote_data_clean <- vote_data_clean %>%
  group_by(fips) %>%
  mutate(lagDEM = lag(DEMOCRAT, n=1, order_by=fips), 
         lagREP = lag(REPUBLICAN, n=1, order_by=fips),
         OTHER = totalvotes - DEMOCRAT - REPUBLICAN,
         lagOTH = lag(OTHER, n=1, order_by=fips)) %>%
  select(year,fips,totalvotes,DEMOCRAT, REPUBLICAN,OTHER, 
         lagDEM, lagREP, lagOTH)

