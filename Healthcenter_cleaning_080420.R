## 

library(pacman)
p_load(tidyverse, sf, dtplyr, readxl, rvest, xml2, fuzzyjoin)

pdir <- '/mnt/c/Users/sigma/OneDrive/Data/HIV/Healthcenter/'

cities <- st_read(str_c(pdir, 'tufts-uscities11-geojson.json'))
county <- st_read('/mnt/c/Users/sigma/OneDrive/Data/Geo/tl_2018_us_county.shp')

hc <- read_excel(str_c(pdir, 'national.xlsx'), sheet = 1)

cities.s <- cities %>% 
    mutate(CITY = toupper(NAME)) %>% 
    bind_cols(as.data.frame(st_coordinates(.)))
hc.s <- hc %>% 
    dplyr::select(1:4,8,10) %>% 
    mutate(DID = 1:1362)
colnames(hc.s) <- c('Name_HC', 'City', 'State', 'NPatients', 'r_minority', 'r_afr', 'DID')
hc.s <- hc.s %>% 

    left_join(cities.s, by = c('City' = 'CITY', 'State' = 'STATE'))
hc.s %>% dplyr::select(-bbox) %>%
    write_csv('/mnt/c/Users/sigma/OneDrive/Data/HIV/Healthcenter/HC_centers.csv')

hc.s %>% 
    filter(duplicated(DID))
hc.s %>% 
    filter(DID %in% c(80, 307, 629, 910)) %>% 
    dplyr::select(1:3, 21:22)


hlink <- 'https://www.cdc.gov/drugoverdose/maps/rxcounty2017.html'
read_html(hlink) %>% html_table(header = T) %>% .[[1]] -> hlink.df
colnames(hlink.df) <- c('County', 'State', 'FIPS', 'opioid_rate_100')
hlink.df <- hlink.df[-1,]

hlink.df <- hlink.df %>% mutate(FIPS = sprintf('%05d', as.integer(FIPS)))




## County level data
pdir <- '/mnt/c/Users/sigma/OneDrive/Data/HIV/Yusuf/County/'
`%s%` <- function(x, y) str_c(x, y)

county <- st_read('/mnt/c/Users/sigma/OneDrive/Data/Geo/tl_2018_us_county.shp')
stateab <- read_csv('/mnt/c/Users/sigma/OneDrive/Data/Geo/State_Abbreviation.csv')
county_attr <- read_csv(pdir %s% 'USA_county_variables.csv')
county_sf <- st_read(pdir %s% 'USA_county_variables/USA_county_variables.shp')
alcdrug <- read_csv(pdir %s% 'alcoholanddrug_mortality.csv')
alcdrug_treat <- read_csv(pdir %s% 'drugalco_treatfacilities.csv')
commhc <- read_csv(pdir %s% 'community_health_center.csv')
hospital <- read_csv(pdir %s% 'hospitals.csv')
mental <- read_csv(pdir %s% 'mentalhealth_facilities.csv')
phys <- read_csv(pdir %s% 'physician_locations.csv')
simpson <- read_csv(pdir %s% 'simpsonindex.csv')

# zcta5
zcta5 <- st_read('/mnt/c/Users/sigma/OneDrive/Data/Geo/ZCTA5_Centroid_2019.gpkg') %>%
    st_transform(2163)
county <- county %>%
    st_transform(2163)

zcta5.c <- zcta5 %>% 
    st_join(county %>% dplyr::select(GEOID)) %>% 
    bind_cols(st_coordinates(.) %>% as.data.frame)


# defeat one by one
# Alcohol-drug
alcdrug.p <- alcdrug %>%
    mutate(Race = plyr::mapvalues(Race, unique(Race), c('white', 'aian', 'african', 'asianpac'))) %>% 
    dplyr::select(-Race.Code) %>% 
    pivot_wider(id_cols = c(County, State, County.Code), names_from = Race, values_from = 5:29) %>% 
    mutate(GEOID = sprintf('%05d', County.Code))

# Alcohol-drug treatment center (proportion of Opioid treatment to the total number of centers)
alcdrug_treat.p <- alcdrug_treat %>% 
    mutate(ZCTA = sprintf('%05d', Zip)) %>% 
    left_join(st_set_geometry(zcta5.c, NULL) %>% mutate(ZCTA = sprintf('%05d', as.integer(ZCTA5CE10))), by = c('ZCTA')) %>% 
    group_by(GEOID) %>% 
    summarize(r_opioid = 100*sum(Opioid == 'Yes')/n()) %>% 
    ungroup

# community health center
# proportion of administrative sites
commhc %>% head
# obsolete
commhc.p <- commhc %>% 
    mutate(ZCTA = str_sub(Zip, 1, 5)) %>% 
    left_join(st_set_geometry(zcta5.c, NULL) %>% mutate(ZCTA = sprintf('%05d', as.integer(ZCTA5CE10))), by = c('ZCTA')) %>% 
    group_by(GEOID) %>% 
    summarize(n_patients = sum(Patients, na.rm = T),
              n_african = floor(sum((`African-American`/100)*Patients, na.rm = T)),
              n_white = floor(sum((white/100)*Patients, na.rm = T)), # needs definition)
              n_hiv = floor(sum((HIV/100) * Patients, na.rm = T)),
              n_center = n(),
              n_administrative = sum(grepl('Administrative.*', Center.type))) %>% 
    ungroup %>% 
    mutate(p_african = 100* n_african / n_patients,
           p_nonwhite = 100* (n_patients - n_white)/n_patients,
           p_hiv = 100* n_hiv/n_patients,
           p_administrative = 100*n_administrative/n_center)

# effective
commhc.p <- commhc %>% 
    #mutate(State = factor(State), County = factor(County)) %>% 
    group_by(State, County, .add = TRUE) %>% 
    summarise(n_patients = sum(Patients, na.rm = T),
              n_african = floor(sum((`African-American`/100)*Patients, na.rm = T)),
              n_white = floor(sum((white/100)*Patients, na.rm = T)), # needs definition)
              n_hiv = floor(sum((HIV/100) * Patients, na.rm = T)),
              n_center = n(),
              n_administrative = sum(grepl('Administrative.*', Center.type))) %>% 
    ungroup %>% 
    arrange(State, County) %>% 
    mutate(p_african = 100* n_african / n_patients,
           p_nonwhite = 100* (n_patients - n_white)/n_patients,
           p_hiv = 100* n_hiv/n_patients,
           p_administrative = 100*n_administrative/n_center)
write_csv(commhc.p, '/mnt/d/commhc_p.csv')

#county_sfa <- county %>% 
#    left_join(commhc.p, by = c('ab' = 'State', 'County' = 'County'))
#st_write(county_sfa, '/mnt/d/County_join_commhc.gpkg')



# hospital - what to summarize?


# mental - percentage of telemedicine facilities
mental.p <- mental %>% 
    mutate(ZCTA = sprintf('%05d', Zipcode)) %>% 
    left_join(st_set_geometry(zcta5.c, NULL) %>% mutate(ZCTA = sprintf('%05d', as.integer(ZCTA5CE10))), by = c('ZCTA')) %>% 
    group_by(GEOID) %>% 
    summarize(p_telemedicine = 100*sum(grepl('Telemedicine.*', Service), na.rm = T)/n()) %>% 
    ungroup

alcdrug_treat.p %>% filter(duplicated(GEOID))

# Make county file
county_attr.s <- county_attr %>% 
    mutate(GEOID = sprintf('%05d', FIPS)) %>% 
    filter(!duplicated(GEOID))

county.ab <- county %>% 
    left_join(stateab, by= c('STATEFP' = 'FIPS_State')) %>% 
    left_join(county_attr.s %>% filter(!duplicated(FIPS)), by = c('GEOID')) %>% 
    left_join(commhc.p, c('ab' = 'State', 'County' = 'County')) %>% 
    left_join(alcdrug.p, by = c('GEOID')) %>% 
    left_join(alcdrug_treat.p, by = c('GEOID')) %>% 
    left_join(mental.p, by = c('GEOID'))

county.ab %>% 
    filter(duplicated())
county_attr %>% 
    filter(duplicated(state.ab) & duplicated(County))
county_attr %>% 
    filter(duplicated(FIPS)) %>% 
    tail

st_write(county.ab, '/mnt/c/Users/sigma/OneDrive/Data/HIV/Healthcenter/County_Attr_080520.geojson')