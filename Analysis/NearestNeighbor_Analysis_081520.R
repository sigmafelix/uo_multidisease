### last revision: 090220
library(pacman)
p_load(tidyverse, sf, spdep, dtplyr, tmap, rmapshaper, classInt, nngeo)

## Get Data
# county with attributes
county_a <- st_read('/mnt/c/Users/sigma/OneDrive/Data/HIV/Yusuf/County/Result/County_Merged_081020.geojson')
# geocoded alcohol outlets
alc <- st_read('/mnt/c/Users/sigma/OneDrive/Data/HIV/Geocoding/Geocoding_Premise_Cleaned_Active.shp')
# PrEP centers
prep <- st_read('/mnt/c/Users/sigma/OneDrive/Data/HIV/PREP_CDC.gpkg')

alc <- alc %>% st_transform(2163)
prep <- prep %>% st_transform(2163)

## Classify counties after calculating the rates per 100000
county_a <- county_a %>% 
    mutate(r_hiv_c10 = ifelse(r_hiv_new_pop >= quantile(r_hiv_new_pop, 0.9, na.rm = TRUE), 1, 0),
           r_hiv_c20 = ifelse(r_hiv_new_pop >= quantile(r_hiv_new_pop, 0.8, na.rm = TRUE), 1, 0),
           r_hiv_c30 = ifelse(r_hiv_new_pop >= quantile(r_hiv_new_pop, 0.7, na.rm = TRUE), 1, 0),
           r_hiv_c40 = ifelse(r_hiv_new_pop >= quantile(r_hiv_new_pop, 0.6, na.rm = TRUE), 1, 0),
           r_hiv_c50 = ifelse(r_hiv_new_pop >= quantile(r_hiv_new_pop, 0.5, na.rm = TRUE), 1, 0))


county_t <- county_a %>% 
    filter(state.ab %in% c('NY', 'CT', 'NJ'))
alc_s <- alc %>% .[county_t %>% st_geometry %>% st_buffer(5000),]
prep_s <- prep %>% .[county_t %>% st_geometry,]

test_nn <- st_nn(prep[3,], alc[1:5,], k = 3, returnDist = TRUE)

calc_nndist <- function(pnt1, pnt2, ref, field, k = 1){
    ref_above <- ref %>% 
        filter(!!rlang::sym(field) == 1 & !is.na(!!rlang::sym(field)))
    ref_below <- ref %>% 
        filter(!!rlang::sym(field) == 0 & !is.na(!!rlang::sym(field)))

    pnt1_inabove <- pnt1[ref_above %>% st_geometry,]
    pnt2_inabove <- pnt2[ref_above %>% st_geometry,]
    pnt1_inabove_ext <- pnt1[ref_above %>% st_geometry %>% st_buffer(10000),]
    pnt2_inabove_ext <- pnt2[ref_above %>% st_geometry %>% st_buffer(10000),]

    pnt1_inbelow <- pnt1[ref_below %>% st_geometry,]
    pnt2_inbelow <- pnt2[ref_below %>% st_geometry,]
    pnt1_inbelow_ext <- pnt1[ref_below %>% st_geometry %>% st_buffer(10000),]
    pnt2_inbelow_ext <- pnt2[ref_below %>% st_geometry %>% st_buffer(10000),]


    if (k > 1){
        dist12_above <- st_nn(pnt1_inabove, pnt2_inabove_ext, k = k, returnDist = TRUE) %>% 
            .$dist %>% 
            lapply(mean) %>% 
            do.call(c,.) %>% 
            mean
        dist12_below <- st_nn(pnt1_inbelow, pnt2_inbelow_ext, k = k, returnDist = TRUE) %>% 
            .$dist %>% 
            lapply(mean) %>% 
            do.call(c,.) %>% 
            mean
        dist21_above <- st_nn(pnt2_inabove, pnt1_inabove_ext, k = k, returnDist = TRUE) %>% 
            .$dist %>% 
            lapply(mean) %>% 
            do.call(c,.) %>% 
            mean
        dist21_below <- st_nn(pnt2_inbelow, pnt1_inbelow, k = k, returnDist = TRUE) %>% 
            .$dist %>% 
            lapply(mean) %>% 
            do.call(c,.) %>% 
            mean
    } else {
        dist12_above <- st_nn(pnt1_inabove, pnt2_inabove_ext, k = k, returnDist = TRUE) %>% 
            .$dist %>% 
            do.call(c,.) %>% 
            mean
        dist12_below <- st_nn(pnt1_inbelow, pnt2_inbelow_ext, k = k, returnDist = TRUE) %>% 
            .$dist %>% 
            do.call(c,.) %>% 
            mean
        dist21_above <- st_nn(pnt2_inabove, pnt1_inabove_ext, k = k, returnDist = TRUE) %>% 
            .$dist %>% 
            do.call(c,.) %>% 
            mean
        dist21_below <- st_nn(pnt2_inbelow, pnt1_inbelow, k = k, returnDist = TRUE) %>% 
            .$dist %>% 
            do.call(c,.) %>% 
            mean
    }
    
    dist.check <- data.frame(D12_Above = dist12_above,
                             D21_Above = dist21_above,
                             D12_Below = dist12_below,
                             D21_Below = dist21_below)
    #dist.check <- list(dist12_above, dist12_below, dist21_above, dist21_below)
    return(dist.check)
}

nndist_c10 <- calc_nndist(alc_s, prep_s, county_t, 'r_hiv_c10')
nndist_c20 <- calc_nndist(alc_s, prep_s, county_t, 'r_hiv_c20')
nndist_c30 <- calc_nndist(alc_s, prep_s, county_t, 'r_hiv_c30')
nndist_c40 <- calc_nndist(alc_s, prep_s, county_t, 'r_hiv_c40')
nndist_c50 <- calc_nndist(alc_s, prep_s, county_t, 'r_hiv_c50')

nndists <- bind_rows(
    nndist_c10,
    nndist_c20,
    nndist_c30,
    nndist_c40,
    nndist_c50
) %>% 
    mutate(percentage = str_c(seq(10,50,10), '%'), Var1 = 'Alcohol', Var2 = 'PrEP')

nndists %>% write_csv('/mnt/c/Users/sigma/OneDrive/Data/HIV/NearestNeighborDistance_081520.csv')

### k = 3
nndist_c10 <- calc_nndist(alc_s, prep_s, county_t, k = 3, 'r_hiv_c10')
nndist_c20 <- calc_nndist(alc_s, prep_s, county_t, k = 3, 'r_hiv_c20')
nndist_c30 <- calc_nndist(alc_s, prep_s, county_t, k = 3, 'r_hiv_c30')
nndist_c40 <- calc_nndist(alc_s, prep_s, county_t, k = 3, 'r_hiv_c40')
nndist_c50 <- calc_nndist(alc_s, prep_s, county_t, k = 3, 'r_hiv_c50')

nndists <- bind_rows(
    nndist_c10,
    nndist_c20,
    nndist_c30,
    nndist_c40,
    nndist_c50
) %>% 
    mutate(percentage = str_c(seq(10,50,10), '%'), Var1 = 'Alcohol', Var2 = 'PrEP')
nndists %>% write_csv('/mnt/c/Users/sigma/OneDrive/Data/HIV/NearestNeighborDistance_090220.csv')
