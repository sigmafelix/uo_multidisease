### Areal summary
### last edited 082020
### Insang Song (isong@uoregon.edu)

library(pacman)
p_load(tidyverse, sf, rmapshaper)

osv <- sessionInfo()[[2]]
if (grepl('*.(linux).*', osv)){
    pdir <- '/mnt/c/Users/sigma/OneDrive/Data/HIV/'
} else {
    pdir <- 'C:/Users/sigma/OneDrive/Data/HIV/'
}

## Data
# Geocoding
alc <- st_read(str_c(pdir, 'Geocoding/Geocoding_NYNJCT.geojson')) %>%
    st_transform(2163)
# County
cnty <- st_read(str_c(pdir, 'Yusuf/County/Result/County_Merged_081020.geojson'))
# PREP
prep <- st_read(str_c(pdir, 'PREP_CDC.gpkg')) %>%
    st_transform(2163)

## Summary
alc.on <- alc %>% filter(Type %in% c('On', 'Both'))
alc.off <- alc %>% filter(Type %in% c('Off', 'Both'))


cnty.3s <- cnty %>% 
    filter(state.ab %in% c('NY', 'NJ', 'CT')) %>%
    mutate(N_alc = lengths(st_intersects(geometry, alc)),
           N_alc_on = lengths(st_intersects(geometry, alc.on)),
           N_alc_off = lengths(st_intersects(geometry, alc.off)),
           N_prep = lengths(st_intersects(geometry, prep))) %>%
    mutate(P_alc = 1e5 * (N_alc/Population),
           P_alc_on = 1e5 * (N_alc_on/Population),
           P_alc_off = 1e5 * (N_alc_off/Population),
           P_prep = 1e5 * (N_prep/Population))

cor(cnty.3s$P_alc, cnty.3s$P_prep) # 0.145726
cor(cnty.3s$P_alc_on, cnty.3s$P_prep) # 0.149365
cor(cnty.3s$P_alc_off, cnty.3s$P_prep) # -0.011989

cnty.3s %>% st_write(str_c(pdir, 'Geocoding/Geocoding_Summarized_County_082020.geojson'))