### ACS data download
### Version 062420
library(pacman)
p_load(tidycensus, tidyverse, sf)
capi <- ''

census_api_key(capi)

?tidycensus

lvars <- load_variables(2018, 'acs1')
acs_income <- lvars %>% filter(grepl('*Household*', label))
acs_poverty <- lvars %>% filter(grepl('*Poverty*', label))

acs_variables <- lvars %>% filter(grepl('B01001(|[A-I])_[0-9]{3}', name)) %>% data.frame

codefilter <- '^(B01001|B17001|B19013|B19083|B23025|B25003|B25004|B15001|B15003)(|[A-Z])_.*'
acs_variables <- lvars %>% filter(grepl(codefilter, name)) %>% data.frame
# B01001[A-I]_001-099: total population (sex, age, ethnicity)
# B19013 Median household income (past 12 months)
# B19083 Gini Index
# B15003 Educational attainment (25+ yo)
# B23025 Employment status (16+ yo)
# B25003 Tenure
# B25004 Vacancy Status
# B15001 Sex by age by Educational Attainment for the population 18+yo
# B17001 Poverty status in the past 12 months sex by age



acs18_5yr <- get_acs('county',year = 2018, survey = 'acs5', variables = acs_variables$name)
acs17_5yr <- get_acs('county',year = 2017, survey = 'acs5', variables = acs_variables$name)
acs16_5yr <- get_acs('county',year = 2016, survey = 'acs5', variables = acs_variables$name)
acs15_5yr <- get_acs('county',year = 2015, survey = 'acs5', variables = acs_variables$name)
#acs18_5yr <- get_acs('county',year = 2018, survey = 'acs5', variables = lvars0$name)
