# last revised 100720
# Insang Song

library(pacman)
p_load(tidyverse, sf, dtplyr)


## Files
hdir <- '/mnt/c/Users/sigma/OneDrive/Data/HIV/Geocoding/'

msas <- str_c(hdir, 'Metropolitan_2018.gpkg')
msas <- st_read(msas)

pnts <- str_c(hdir, 'Geocoding_Comparison_092820.gpkg')
pnts <- st_read(pnts)
pnts <- pnts %>% st_transform(st_crs(msas))

pnts.ext <- pnts %>% 
    mutate(MSA = st_intersects(pnts, msas, sparse = FALSE) %>% apply(1, sum))
#pnts.ext <- st_intersection(pnts, msas %>% dplyr::select(GEOID))
pnts.ext <- pnts.ext %>%
    mutate(dist_pr050 = ifelse(correct_address >= 50 & !is.na(correct_address), 1, 0),
           dist_pr100 = ifelse(correct_address >= 100 & !is.na(correct_address), 1, 0))


# table
pnts.ext2 <- pnts.ext %>% mutate(cadd = ifelse(is.na(corrected_address), 'complete', corrected_address),
                                 isknown = (cadd != 'unknown')) %>%
            filter(isknown)
table(pnts.ext2$MSA, pnts.ext2$dist_pr050)

ggplot(data = pnts.ext2,
       mapping = aes( x = Accuracy.Type, y = correct_address)) +
       geom_point() +
       coord_flip() +
       scale_y_log10()


### All geocodio data
gcd <- str_c(hdir, 'Geocoding_Base_092420_geocodio.csv')
gcd <- read_csv(gcd) %>%
    st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) %>% 
    st_transform(st_crs(msas))

gcd.ext <- gcd %>% 
    mutate(MSA = st_intersects(gcd, msas, sparse = FALSE) %>% apply(1, sum))

gcd.sample <- gcd.ext %>%
    filter(MSA == 1) %>%
    sample_n(floor(nrow(.) * 0.01)) %>%
    mutate(dist_acc = NA,
           corrected_address = NA)
st_write(gcd.sample, str_c(hdir, 'Geocodio_Accuracy_Test_100720.gpkg'))


## Re-Accuracy test
gac <- str_c(hdir, 'Geocodio_Accuracy_Test_100720.gpkg')
gac <- st_read(gac)

gac %>% colnames
gac$dist_accuracy %>% summary

gac2 <- gac %>% 
    mutate(dist_acc = ifelse(is.na(dist_accuracy), 0, dist_accuracy),
           dist_cut = cut(dist_acc, c(-Inf, 0,100,160,1000,Inf)))
table(gac2$dist_cut, gac2$State)