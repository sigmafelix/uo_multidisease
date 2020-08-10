library(pacman)
p_load(tidyverse, sf, dtplyr)

pdir <- '/mnt/c/Users/sigma/OneDrive/Data/HIV/'
# Remove duplicates
alc <- st_read(str_c(pdir, 'Geocoding/Geocoding_Premise.shp'))

alc.l <- alc %>%
    bind_cols(st_coordinates(.) %>% as.data.frame) %>%
    st_set_geometry(NULL) %>%
    arrange(State, LID, Status, Name, Addr, Type) %>%
    group_by(State, Name, Addr, X, Y) %>%
    dplyr::summarize(Lic_Type = Lic_Type[1],
              Status = Status[which(!is.na(Type))],
              Geocoder = Geocoder[which(!is.na(Type))],
              Type = Type[which(!is.na(Type))],
              D_orig = D_orig[which(!is.na(Type))],
              D_eff = D_eff[which(!is.na(Type))],
              D_expr = D_expr[which(!is.na(Type))],
              X = unique(X)[1], Y = unique(Y)[1],
              Xcoord = unique(X)[1], Ycoord = unique(Y)[1]
              ) %>%
    ungroup %>%
    st_as_sf(coords = c('X', 'Y'), crs = 4326)

st_write(alc.ll, str_c(pdir, 'Geocoding/Geocoding_Premise_Cleaned.gpkg'))
st_write(alc.l, str_c(pdir, 'Geocoding/Geocoding_Premise_Cleaned.shp'))
table(alc.l$Status)


alc.ll <- alc %>%
    bind_cols(st_coordinates(.) %>% as.data.frame) %>%
    st_set_geometry(NULL) %>%
    mutate(Status = ifelse(is.na(Status), 'Active', Status)) %>%
    filter(!is.na(Type)) %>%
    arrange(State, LID, Lic_Type, Status, Name, Addr, Type) %>%
    group_by(State, Name, Addr) %>%
    dplyr::summarize(Lic_Type = Lic_Type[(grepl('^(On|Off|Both)', Type)*grepl('^(ACTIVE|Active).*', Status))[which(grepl('^(On|Off|Both)', Type)*grepl('^(ACTIVE|Active).*', Status) == 1)]][1],
              Status = Status[(grepl('^(On|Off|Both)', Type)*grepl('^(ACTIVE|Active).*', Status))[which(grepl('^(On|Off|Both)', Type)*grepl('^(ACTIVE|Active).*', Status) == 1)]][1],
              Geocoder = Geocoder[(grepl('^(On|Off|Both)', Type)*grepl('^(ACTIVE|Active).*', Status))[which(grepl('^(On|Off|Both)', Type)*grepl('^(ACTIVE|Active).*', Status) == 1)]][1],
              Type = Type[(grepl('^(On|Off|Both)', Type)*grepl('^(ACTIVE|Active).*', Status))[which(grepl('^(On|Off|Both)', Type)*grepl('^(ACTIVE|Active).*', Status) == 1)]][1],
              D_orig = D_orig[(grepl('^(On|Off|Both)', Type)*grepl('^(ACTIVE|Active).*', Status))[which(grepl('^(On|Off|Both)', Type)*grepl('^(ACTIVE|Active).*', Status) == 1)]][1],
              D_eff = D_eff[(grepl('^(On|Off|Both)', Type)*grepl('^(ACTIVE|Active).*', Status))[which(grepl('^(On|Off|Both)', Type)*grepl('^(ACTIVE|Active).*', Status) == 1)]][1],
              D_expr = D_expr[(grepl('^(On|Off|Both)', Type)*grepl('^(ACTIVE|Active).*', Status))[which(grepl('^(On|Off|Both)', Type)*grepl('^(ACTIVE|Active).*', Status) == 1)]][1],
              X = unique(X), 
              Y = unique(Y),
              Xcoord = unique(X), 
              Ycoord = unique(Y)
              ) %>%
    ungroup %>%
    filter((!is.na(X) & !is.na(Y)) & Status %in% c('Active', 'ACTIVE') & !is.na(Status)) %>%
    st_as_sf(coords = c('X', 'Y'), crs = 4326)
st_write(alc.ll, str_c(pdir, 'Geocoding/Geocoding_Premise_Cleaned2.shp'))



# Task 2: COVID data
mscv <- read_csv(str_c(pdir, 'Multidisease_data/COVID_Mississippi_Cleaned_063020.csv'))
ohcv <- read_csv(str_c(pdir, 'Multidisease_data/COVID_Ohio_Cleaned_063020.csv'))

mscv.wide <- mscv %>%
    mutate(Hispanic = plyr::mapvalues(Hispanic, unique(Hispanic), c('T', 'NH', 'H', 'U')),
           Ethnicity = plyr::mapvalues(Ethnicity, unique(Ethnicity), c('T', 'AFR', 'W', 'AIAN', 'AS', 'O', 'U'))) %>%
    dplyr::select(-State) %>%
    pivot_wider(id_cols = County, names_from = c('Hispanic', 'Ethnicity')) %>%
    mutate(County = plyr::mapvalues(County, c('Desoto', 'Jefferson davis', 'Pearl river'), c('DeSoto', 'Jefferson Davis', 'Pearl River')))

ohcv.wide <- ohcv %>%
    mutate(Sex = plyr::mapvalues(Sex, unique(Sex), c('M', 'F')),
           Age_range = plyr::mapvalues(Age_range, unique(Age_range), c('80p', '50s', '70s', '40s', '60s', '20s', '30s', '19b', 'UNK'))) %>%
    dplyr::select(-State) %>%
    pivot_wider(id_cols = County, names_from = c('Sex', 'Age_range'), values_from = NDeath) %>%
    mutate_all(list(~ifelse(is.na(.), 0, .))) %>%
    mutate(Total = .[,-1] %>% apply(., 1, sum))
    


county <- st_read('/mnt/c/Users/sigma/OneDrive/Data/Geo/tl_2018_us_county.shp')
cty.oh <- county %>% filter(STATEFP == 39) %>%
    dplyr::select(NAME, GEOID) %>%
    left_join(ohcv.wide, by = c('NAME' = 'County')) %>%
    st_transform(2163) %>%
    mutate_at(vars(-1, -ncol(.)), list(~as.integer(.)))
cty.ms <- county %>% filter(STATEFP == 28) %>%
    dplyr::select(NAME, GEOID) %>%
    left_join(mscv.wide, by = c('NAME' = 'County')) %>%
    st_transform(2163) %>%
    mutate_at(vars(-1, -ncol(.)), list(~as.integer(.)))



# Task 3
#rm(list=ls())
pdir <- '/mnt/c/Users/sigma/OneDrive/Data/HIV/'
load(str_c(pdir, 'ACS_2015-2018.RData'))

acs18_sub <- 
acs18_5yr %>%
    filter(grepl('^B01001(A|B)_(001)$', variable)) %>%
    filter(grepl('^(28|39).*', GEOID)) %>%
    mutate(variable = plyr::mapvalues(variable, c('B01001A_001', 'B01001B_001'), c('Pop_W', 'Pop_AFR'))) %>%
    dplyr::select(-NAME, -moe) %>%
    pivot_wider(id_cols = GEOID, values_from = estimate, names_from = variable) 


cty.oh <- cty.oh %>%
    mutate(GEOID = as.character(GEOID)) %>% 
    left_join(acs18_sub)
cty.ms <- cty.ms %>%
    mutate(GEOID = as.character(GEOID)) %>% 
    left_join(acs18_sub)

# Export
st_write(cty.oh, str_c(pdir, 'Multidisease_data/Ohio_COVID_County.shp'))
st_write(cty.ms, str_c(pdir, 'Multidisease_data/Mississippi_COVID_County.shp'))
