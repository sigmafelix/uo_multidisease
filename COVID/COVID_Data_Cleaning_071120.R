## data munging

library(pacman)
p_load(tidyverse, sf, dtplyr)

pdir <- 'C:/Users/sigma/OneDrive/Data/HIV/Multidisease_data/'

# files
covid_oh <- read_csv(str_c(pdir, 'COVID_Ohio_071120.csv'), col_types = 'ccccccddd')
covid_ms <- read_csv(str_c(pdir, 'COVID_Mississippi_063020.csv'))
covid_us <- read_csv(str_c(pdir, 'COVID_County_063020.csv'))

# filtering
covid_oh <- covid_oh %>% 
  mutate(death_date = lubridate::mdy(`Date Of Death`),
         State = 'Ohio') %>% 
  filter(death_date <= as.Date('2020-06-30') | (`Death Count` != 0)) %>% 
  rename(Age_range = `Age Range`,
         NDeath = `Death Count`) %>% 
  dplyr::select(State, County, Sex, Age_range, NDeath, death_date) %>% 
  group_by(State, County, Sex, Age_range) %>% 
  summarize(NDeath = sum(NDeath)) %>% 
  ungroup

covid_ms <- covid_ms %>% 
  pivot_longer(cols = 3:21, names_to = c('Hispanic', 'Ethnicity'), names_sep = '_') %>% 
  mutate(Hispanic = plyr::mapvalues(Hispanic, c('NH', 'H', 'UN'), c('Non-Hispanic', 'Hispanic', 'Unknown')),
         Ethnicity = plyr::mapvalues(Ethnicity, c('Afr', 'W', 'AIAN', 'As', 'other', 'unk'),
                                     c('African-American', 'White', 'American-Indian-Alaska-Native', 'Asian', 'Other', 'Unknown')),
         County = str_c(str_sub(County, 1, 1), tolower(str_sub(County, 2, -1))))

covid_us <- covid_us %>% 
  filter(Country_Region == 'US') %>% 
  rename(State = Province_State,
         County = Admin2,
         NDeath = Deaths) %>% 
  mutate(FIPS = sprintf('%05d', FIPS)) %>% 
  dplyr::select(FIPS, State, County, NDeath)

# export data
write_csv(covid_oh, str_c(pdir, 'COVID_Ohio_Cleaned_063020.csv'))
write_csv(covid_ms, str_c(pdir, 'COVID_Mississippi_Cleaned_063020.csv'))
write_csv(covid_us, str_c(pdir, 'COVID_County_Cleaned_063020.csv'))



## Automated COVID data acquisition
covid0915 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/09-14-2020.csv')
dim(covid0915)
colnames(covid0915)

covid0915 <- covid0915 %>% 
       filter(Country_Region == 'US') %>% 
       dplyr::select(-5:-7, -12:-14) %>% 
       mutate(FIPS = sprintf('%05d', FIPS))

covid0915