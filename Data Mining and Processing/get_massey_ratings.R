# use selenium running with Docker on port 4445 to load page then extract HTML

# current season link
url_data <- "https://masseyratings.com/cb/ncaa-d1/ratings"
# past seasons link
url_data <- "https://masseyratings.com/cb2019/ncaa-d1/ratings"

library(tidyverse)
library(magrittr)
library(httr)
library(Rcpp)
library(magrittr)
library(rvest)
library(RSelenium)
remDr <- remoteDriver(
  remoteServerAddr = '###.###.##.##',
  port = 4445L,
  browserName = "chrome"
)
remDr$open()

remDr$navigate(url_data)
html <- remDr$getPageSource()[[1]]

# might be necessary if getting length of url error
#rm(remDr)

# Extract table from source
DF <- read_html(html) %>% 
  html_nodes("table") %>% 
  `[[`(2) %>% 
  html_table()

# Close connection
remDr$close()

######################
## PREVIOUS SEASONS ##
######################

data = DF %>%
  select(-c(2,9)) %>% 
  slice(-1) %>% # remove first row of correlations
  janitor::clean_names() %>%
  mutate(team = str_replace(team, 
                            'Ivy League|West Coast|Big 12|Big 10|Pac 12|American Athletic|Southeastern|Missouri Val|Big East|Atlantic Coast|Conference USA|Patriot League|Summit Lg|OH Valley|Big West|Mid-American|Mountain West|Atlantic 10|Southland|Big Sky|Western Athletic|Big South|Horizon|Atlantic Sun|Sun Belt|Colonial|Metro Atlantic|America East|Southwestern AC|Mid-Eastern AC','')) %>%
  mutate(rat = as.numeric(str_sub(rat, -4, -1)), # rating never goes above 10
         pwr = as.numeric(str_sub(pwr, -5, -1)),
         so_s = as.numeric(str_sub(so_s, -5, -1)),
         off = as.numeric(str_sub(off, -6, -1)),
         def = as.numeric(str_sub(def, -5, -1)),
         # remove rank in front for those ranked > 1-9 (removes first character)
         off = ifelse(off > 130, gsub('^.', '', off), off), 
         def = ifelse(def > 35, gsub('^.', '', def), def),
         pwr = ifelse(pwr > 60, gsub('^.', '', pwr), pwr),
         # remove conference that is also in real name
         team = gsub('Southern$', '', team),
         team = gsub('Northeast$', '', team)) %>%
  mutate(across(c(pwr, off, def, hfa), ~as.numeric(.))) %>%
  drop_na()

write.csv(data, 'masseyratings_19.csv', row.names = F)

######################
## CURRENT SEASON  ##
######################

# remove messy columns not needed (record, change/delta, unknown 13)
# drop first row ("correlation")
data = DF %>%
  select(-c(2,3,13)) %>% 
  slice(-1) %>% # remove first row of correlations
  janitor::clean_names() # make column names all lowercase

# remove conference names from teams column
# need to find a few more to pull out
data %<>%
  mutate(team = str_replace(team, 
  'West Coast|Big 12|Big 10|Pac 12|American Athletic|Southeastern|Missouri Val|Big East|Atlantic Coast|Conference USA|Patriot League|Summit Lg|OH Valley|Big West|Mid-American|Mountain West|Atlantic 10|Southland|Big Sky|Western Athletic|Big South|Horizon|Atlantic Sun|Sun Belt|Colonial|Metro Atlantic|America East|Southwestern AC|Mid-Eastern AC',''))

# remove extra data in front of statistics (removing the rankings to get ratings only)
data %<>%
  mutate(rat = as.numeric(str_sub(rat, -4, -1)), # rating never goes above 10
         pwr = as.numeric(str_sub(pwr, -5, -1)),
         so_s = as.numeric(str_sub(so_s, -5, -1)),
         ssf = as.numeric(str_sub(ssf, -5, -1)),
         off = as.numeric(str_sub(off, -6, -1)),
         def = as.numeric(str_sub(def, -5, -1)),
         # remove rank in front for those ranked > 1-9 (removes first character)
         off = ifelse(off > 130, gsub('^.', '', off), off), 
         def = ifelse(def > 35, gsub('^.', '', def), def),
         pwr = ifelse(pwr > 60, gsub('^.', '', pwr), pwr),
         # remove conference that is also in real name
         team = gsub('Southern$', '', team),
         team = gsub('Northeast$', '', team)) %>%
  mutate(across(c(pwr, off, def, hfa, ew, el), ~as.numeric(.))) %>%
  drop_na()

write.csv(data, 'masseyratings_21.csv', row.names = F)






