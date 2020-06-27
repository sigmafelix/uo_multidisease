library(pacman)
p_load(tidyverse, dtplyr, fuzzyjoin, forcats)


hdir <- '/home/felix/OneDrive/Data/HIV/'

covid_jhph <- read_csv(str_c(hdir, 'covid_JH_06192020.csv')) %>% 
  mutate(FIPS = sprintf('%05d', FIPS))
covid_fact <- read_csv(str_c(hdir, 'covid_deaths_usafacts_061920.csv')) %>% 
  mutate(FIPS = sprintf('%05d', countyFIPS))
covid_nyt <- read_csv(str_c(hdir, 'covid_nyt_06192020.csv')) %>% 
  rename(FIPS = fips)


covid_facts <- covid_fact %>% 
  pivot_longer(cols = 5:154) %>% 
  filter(name == '6/19/20') %>% 
  rename(Deaths.USAFacts = value) %>% 
  dplyr::select(FIPS, Deaths.USAFacts)
covid_jhphs <- covid_jhph %>% 
  dplyr::select(FIPS, Deaths) %>% 
  #rename(Deaths.JH = Deaths) %>% 
  group_by(FIPS) %>% 
  summarize(Deaths.JH = sum(Deaths, na.rm = T)) %>% 
  ungroup
covid_nyts <- covid_nyt %>% 
  filter(date == '2020-06-19') %>% 
  dplyr::select(FIPS, deaths) %>% 
  group_by(FIPS) %>% 
  summarize(Deaths.NYT = sum(deaths)) %>% 
  ungroup


covid_joint <- covid_jhphs %>% 
  full_join(covid_facts, 'FIPS') %>% 
  full_join(covid_nyts, 'FIPS')
covid_joint_full <- covid_joint %>% 
  filter(complete.cases(.)) %>% 
  arrange(FIPS)





nrow(covid_joint_full %>% filter(Deaths.JH == Deaths.USAFacts & Deaths.USAFacts & Deaths.NYT))
nrow(covid_joint %>% filter(Deaths.JH < Deaths.USAFacts))
nrow(covid_joint %>% filter(Deaths.JH > Deaths.USAFacts))
nrow(covid_joint %>% filter(Deaths.NYT < Deaths.USAFacts))
nrow(covid_joint %>% filter(Deaths.NYT > Deaths.USAFacts))
nrow(covid_joint %>% filter(Deaths.JH < Deaths.NYT))
nrow(covid_joint %>% filter(Deaths.JH > Deaths.NYT))


sum(!covid_jhphs$FIPS %in% covid_facts$FIPS)
sum(!covid_jhphs$FIPS %in% covid_nyts$FIPS)
sum(!covid_nyts$FIPS %in% covid_facts$FIPS)
sum(!covid_nyts$FIPS %in% covid_jhphs$FIPS)
sum(!covid_facts$FIPS %in% covid_nyts$FIPS)
sum(!covid_facts$FIPS %in% covid_jhphs$FIPS)


# 2018 FIPS
gdir <- '/home/felix/OneDrive/Data/Geo/'
cty18 <- st_read(str_c(gdir, 'tl_2018_us_county.shp'))

sum(!covid_jhphs$FIPS %in% cty18$GEOID)
sum(!covid_nyts$FIPS %in% cty18$GEOID)
sum(!covid_facts$FIPS %in% cty18$GEOID)

sum(!cty18$GEOID %in% covid_jhphs$FIPS)
sum(!cty18$GEOID %in% covid_nyts$FIPS)
sum(!cty18$GEOID %in% covid_facts$FIPS)

