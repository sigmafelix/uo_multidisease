## Death count data from CDC WONDER
library(pacman)
p_load(tidyverse, dtplyr, sf, tmap)
pdir <- 'C:/Users/sigma/OneDrive/Data/HIV/Multidisease_data/'

# Data load
# HIV
d_hiv <- read_tsv(str_c(pdir, 'HIV_County_Year_2001-2018_Cut.txt'))
  
# Viral Hepatitis
d_vhp <- read_tsv(str_c(pdir, 'ViralHepatitis_County_Year_2001-2018_Cut.txt'))

# Drug-induced
d_drg <- read_tsv(str_c(pdir, 'DrugInduced_County_Year_2001-2018_Cut.txt'))

# Restructure fields
clean_data <- function(dat, suffix){
  dat <- dat %>% 
    dplyr::select(2:4, 6:7) %>% 
    rename(FIPS = `County Code`) %>% 
    mutate(State = str_split_fixed(County, ', ', 2)[,2],
           County = str_split_fixed(County, ', ', 2)[,1],
           Deaths = plyr::mapvalues(Deaths, c('Suppressed', 'Missing'), c(-999, NA)) %>% as.integer) %>% 
    dplyr::select(FIPS, State, County, Year, Deaths, Population)
  colnames(dat)[length(colnames(dat))-1] <- str_c('D_', suffix)
  return(dat)
}

d_hiv0 <- clean_data(d_hiv, 'HIV')
d_vhp0 <- clean_data(d_vhp, 'HXV') %>% dplyr::select(1, 4, 5)
d_drg0 <- clean_data(d_drg, 'DRUG') %>% dplyr::select(1, 4, 5)

# Combine all data
d_combined <- d_hiv0 %>% 
  left_join(d_vhp0) %>% 
  left_join(d_drg0)

# export
write_csv(d_combined, str_c(pdir, 'HIV_HXV_DRUG_County_Year_2001-2018.csv'))

#
county <- st_read('C:/Users/sigma/OneDrive/Data/Geo/tl_2018_us_county.shp')

county_s <- county %>% 
  st_transform(2163) %>% 
  st_simplify(dTolerance = 500, preserveTopology = TRUE) %>% 
  st_buffer(0)
county_s = county_s %>% 
  filter(!STATEFP %in% c("72", "78", "66", "69", "60", "02", "15", "74"))

d_comb <- d_combined %>% 
  filter(Year == 2018)
county_sd <- county_s %>% 
  left_join(d_comb, by = c('GEOID' = 'FIPS')) %>% 
  mutate_at(.vars = vars(21, 23:24),
            .funs = list(~ifelse(.<0, NA, .)))

tm_shape(county_sd) +
  tm_fill('D_DRUG', style = 'jenks')
