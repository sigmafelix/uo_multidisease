## Last revised 063021
## data munging

library(pacman)
p_load(tidyverse, sf, dtplyr)

pdir <- '/mnt/c/Users/sigma/OneDrive/Data/HIV/Multidisease_data/'

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


## COVID difference
covid0410 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-10-2020.csv')
covid0411 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-11-2020.csv')
covid0724 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/07-24-2020.csv')
covid0725 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/07-25-2020.csv')
covid1009 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/10-09-2020.csv')


covid_filter <- function(dat){
  dat_filt <- dat %>% 
    filter(Country_Region == 'US') %>% 
    dplyr::select(-5:-7, -12:-(ncol(.))) %>% 
    mutate(FIPS = sprintf('%05d', FIPS))
  return(dat_filt)
}

covid0410s <- covid0410 %>% covid_filter
covid0411s <- covid0411 %>% covid_filter
covid0724s <- covid0724 %>% covid_filter
covid0725s <- covid0725 %>% covid_filter
covid1009s <- covid1009 %>% covid_filter

covid_period1 <- covid0410s %>% 
  transmute(FIPS = FIPS,
            Confirmed_p1 = ifelse(FIPS == "53061", Confirmed - 1, Confirmed)) %>% 
  filter(!grepl('*.(NA)$', FIPS)) %>% 
  filter(!grepl('^90', FIPS))
covid_period2 <- covid0724s %>% 
  dplyr::select(FIPS, Confirmed) %>% 
  mutate(c0724 = ifelse(is.na(Confirmed), 0, Confirmed)) %>% 
  dplyr::select(-Confirmed) %>% 
  full_join(covid0411s %>% dplyr::select(FIPS, Confirmed)) %>% 
  transmute(FIPS = FIPS,
            Confirmed_p2 = ifelse(is.na(c0724), 0, c0724) - ifelse(is.na(Confirmed), 0, Confirmed)) %>% 
  filter(!grepl('*.(NA)$', FIPS)) %>% 
  filter(!grepl('^(00|90)', FIPS))
covid_period3 <- covid1009s %>% 
  dplyr::select(FIPS, Confirmed) %>% 
  #mutate(FIPS = sprintf('%05d', FIPS)) %>% 
  mutate(c1009 = ifelse(is.na(Confirmed), 0, Confirmed)) %>%
  dplyr::select(-Confirmed) %>% 
  full_join(covid0725s %>% dplyr::select(FIPS, Confirmed)) %>% 
  transmute(FIPS = FIPS,
            Confirmed_p3 = ifelse(is.na(c1009), 0, c1009) - ifelse(is.na(Confirmed), 0, Confirmed)) %>% 
  filter(!grepl('*.(NA)$', FIPS)) %>% 
  filter(!grepl('^(00|90)', FIPS))

summary(covid_period1)
summary(covid_period2)
summary(covid_period3)

# sensitivity
# 0401/0402/0531/0601/1231
covid0401 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-01-2020.csv')
covid0402 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-02-2020.csv')
covid0531 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/05-31-2020.csv')
covid0601 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/06-01-2020.csv')
covid1231 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/12-31-2020.csv')


covid0401s <- covid0401 %>% covid_filter
covid0402s <- covid0402 %>% covid_filter
covid0531s <- covid0531 %>% covid_filter
covid0601s <- covid0601 %>% covid_filter
covid1231s <- covid1231 %>% covid_filter

covid_period1s <- covid0401s %>% 
  transmute(FIPS = FIPS,
            Confirmed_p1s = ifelse(FIPS == "53061", Confirmed - 1, Confirmed)) %>% 
  filter(!grepl('*.(NA)$', FIPS)) %>% 
  filter(!grepl('^90', FIPS))
covid_period2s <- covid0531s %>% 
  dplyr::select(FIPS, Confirmed) %>% 
  mutate(c0531 = ifelse(is.na(Confirmed), 0, Confirmed)) %>% 
  dplyr::select(-Confirmed) %>% 
  full_join(covid0411s %>% dplyr::select(FIPS, Confirmed)) %>% 
  transmute(FIPS = FIPS,
            Confirmed_p2s = ifelse(is.na(c0531), 0, c0531) - ifelse(is.na(Confirmed), 0, Confirmed)) %>% 
  filter(!grepl('*.(NA)$', FIPS)) %>% 
  filter(!grepl('^(00|90)', FIPS))
covid_period3s <- covid1231s %>% 
  dplyr::select(FIPS, Confirmed) %>% 
  #mutate(FIPS = sprintf('%05d', FIPS)) %>% 
  mutate(c1231 = ifelse(is.na(Confirmed), 0, Confirmed)) %>%
  dplyr::select(-Confirmed) %>% 
  full_join(covid0601s %>% dplyr::select(FIPS, Confirmed)) %>% 
  transmute(FIPS = FIPS,
            Confirmed_p3s = ifelse(is.na(c1231), 0, c1231) - ifelse(is.na(Confirmed), 0, Confirmed)) %>% 
  filter(!grepl('*.(NA)$', FIPS)) %>% 
  filter(!grepl('^(00|90)', FIPS))

summary(covid_period1s)
summary(covid_period2s)
summary(covid_period3s)


# Merge all
covid_periods_all <- covid_period1 %>% 
  full_join(covid_period2) %>% 
  full_join(covid_period3) %>% 
  full_join(covid_period1s) %>% 
  full_join(covid_period2s) %>% 
  full_join(covid_period3s) 

###  
# 0725/1007/1231
covid0725 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/07-25-2020.csv')
covid1007 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/10-07-2020.csv')
covid1231 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/12-31-2020.csv')

covid0725s <- covid0725 %>% covid_filter
covid1007s <- covid1007 %>% covid_filter
covid1231s <- covid1231 %>% covid_filter


covid_period2a <- covid1007s %>% 
  dplyr::select(FIPS, Confirmed) %>% 
  mutate(c1007 = ifelse(is.na(Confirmed), 0, Confirmed)) %>% 
  dplyr::select(-Confirmed) %>% 
  full_join(covid0725s %>% dplyr::select(FIPS, Confirmed)) %>% 
  transmute(FIPS = FIPS,
            Confirmed_p2a = ifelse(is.na(c1007), 0, c1007) - ifelse(is.na(Confirmed), 0, Confirmed)) %>% 
  filter(!grepl('*.(NA)$', FIPS)) %>% 
  filter(!grepl('^(00|90)', FIPS))
covid_period3a <- covid1231s %>% 
  dplyr::select(FIPS, Confirmed) %>% 
  #mutate(FIPS = sprintf('%05d', FIPS)) %>% 
  mutate(c1231 = ifelse(is.na(Confirmed), 0, Confirmed)) %>%
  dplyr::select(-Confirmed) %>% 
  full_join(covid0725s %>% dplyr::select(FIPS, Confirmed)) %>% 
  transmute(FIPS = FIPS,
            Confirmed_p3a = ifelse(is.na(c1231), 0, c1231) - ifelse(is.na(Confirmed), 0, Confirmed)) %>% 
  filter(!grepl('*.(NA)$', FIPS)) %>% 
  filter(!grepl('^(00|90)', FIPS))
covid_periodas <- covid_period2a %>% 
  full_join(covid_period3a)

covars <- read_csv('C:/Users/sigma/OneDrive/Data/HIV/COVID-Mental/Data/County_Data_Mental_2018_011921.csv')
covars_ext <- covars %>% 
  mutate(GEOID = sprintf('%05d', GEOID)) %>% 
  left_join(covid_periodas, by = c('GEOID' = 'FIPS'))

airp <- read_csv('C:/Users/sigma/OneDrive/Data/HIV/County_Data/county_pm25.csv')
airp_e <- airp %>% 
  pivot_wider(names_from = year, values_from = pm25) %>% 
  filter(as.integer(substr(sprintf('%05d',fips),1,2)) <= 70) %>% 
  mutate(fips = sprintf('%05d', fips),
         fips = ifelse(fips == '46113', '46102', fips)) %>% 
  arrange(fips)
colnames(airp_e)[-1] <- str_c('pm25_', colnames(airp_e)[-1])
airp_em <- airp_e[,-1] %>% apply(1, function(x) mean(x, na.rm = T))
airp_e <- airp_e %>% 
  mutate(pm25_2000_2016_mean = airp_em)
covars_ext <- covars_ext %>% 
  left_join(airp_e, by = c('GEOID' = 'fips'))

write_csv(covars_ext, 'C:/Users/sigma/OneDrive/Data/HIV/COVID-Mental/Data/County_Data_Mental_2018_012121.csv')



## Rework @063021
#(1) Jan 22 - April 30, 2020;
#(2) May 1 - Oct 7, 2020. 
covid0122 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/01-22-2020.csv')
covid0430 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-30-2020.csv')
covid0501 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/05-01-2020.csv')
covid1007 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/10-07-2020.csv')


covid_filter <- function(dat){
  dat_filt <- dat %>% 
    filter(Country_Region == 'US') %>% 
    dplyr::select(-5:-7, -12:-(ncol(.))) %>% 
    mutate(FIPS = sprintf('%05d', FIPS))
  return(dat_filt)
}

covid0122s <- covid0122 %>% covid_filter
covid0430s <- covid0430 %>% covid_filter
covid0501s <- covid0501 %>% covid_filter
covid1007s <- covid1007 %>% covid_filter

covid_period1 <- covid0430s %>% 
  transmute(FIPS = FIPS,
            Confirmed_n1 = ifelse(FIPS == "53061", Confirmed - 1, Confirmed)) %>% 
  filter(!grepl('*.(NA)$', FIPS)) %>% 
  filter(!grepl('^90', FIPS))
covid_period2 <- covid1007s %>% 
  dplyr::select(FIPS, Confirmed) %>% 
  mutate(c1007 = ifelse(is.na(Confirmed), 0, Confirmed)) %>% 
  dplyr::select(-Confirmed) %>% 
  full_join(covid0501s %>% dplyr::select(FIPS, Confirmed)) %>% 
  transmute(FIPS = FIPS,
            Confirmed_n2 = ifelse(is.na(c1007), 0, c1007) - ifelse(is.na(Confirmed), 0, Confirmed)) %>% 
  filter(!grepl('*.(NA)$', FIPS)) %>% 
  filter(!grepl('^(00|90)', FIPS))


#For sensitivity analysis, download the data for:
#(1) Jan 22 - March 31, 2020;
#(2) April 1 - Oct 7, 2020. 
covid0331 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-31-2020.csv')
covid0401 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-01-2020.csv')
covid1007 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/10-07-2020.csv')


covid0331s <- covid0331 %>% covid_filter
covid0401s <- covid0401 %>% covid_filter
covid1007s <- covid1007 %>% covid_filter

covid_period1s <- covid0331s %>% 
  transmute(FIPS = FIPS,
            Confirmed_n1s = ifelse(FIPS == "53061", Confirmed - 1, Confirmed)) %>% 
  filter(!grepl('*.(NA)$', FIPS)) %>% 
  filter(!grepl('^90', FIPS))
covid_period2s <- covid1007s %>% 
  dplyr::select(FIPS, Confirmed) %>% 
  mutate(c1007 = ifelse(is.na(Confirmed), 0, Confirmed)) %>% 
  dplyr::select(-Confirmed) %>% 
  full_join(covid0401s %>% dplyr::select(FIPS, Confirmed)) %>% 
  transmute(FIPS = FIPS,
            Confirmed_n2s = ifelse(is.na(c1007), 0, c1007) - ifelse(is.na(Confirmed), 0, Confirmed)) %>% 
  filter(!grepl('*.(NA)$', FIPS)) %>% 
  filter(!grepl('^(00|90)', FIPS))


covid_periods_all <- covid_period1 %>% 
  full_join(covid_period2) %>% 
  full_join(covid_period1s) %>% 
  full_join(covid_period2s) 

# previous dataset
covars_ext = read_csv('/mnt/c/Users/sigma/OneDrive/Data/HIV/COVID-Mental/Data/County_Data_Mental_2018_012121.csv')
covars_extd = covars_ext %>% 
  mutate(GEOID = sprintf('%05d', GEOID)) %>%
  full_join(covid_periods_all, by = c('GEOID' = 'FIPS')) %>%
  filter(GEOID %in% sprintf('%05d', covars_ext$GEOID))
dim(covars_extd)
write_csv(covars_extd, '/mnt/c/Users/sigma/OneDrive/Data/HIV/COVID-Mental/Data/County_Data_Mental_2018_063021.csv')


covars_ext = read_csv('/mnt/c/Users/sigma/OneDrive/Data/HIV/COVID-Mental/Data/County_Data_Mental_imputed_2019_013121.csv')
covars_extd = covars_ext %>% 
  mutate(GEOID = sprintf('%05d', GEOID)) %>%
  full_join(covid_periods_all, by = c('GEOID' = 'FIPS')) %>%
  filter(GEOID %in% sprintf('%05d', covars_ext$GEOID))
dim(covars_extd)
write_csv(covars_extd, '/mnt/c/Users/sigma/OneDrive/Data/HIV/COVID-Mental/Data/County_Data_Mental_imputed_2019_063021.csv')
