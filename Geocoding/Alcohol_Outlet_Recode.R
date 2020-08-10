### Recoding alcohol outlets
library(pacman)
p_load(sf, tidyverse)

ao <- st_read('C:/Users/sigma/OneDrive/Data/HIV/Geocoding_cleaned_NAex.gpkg')
recode <- read_csv('C:/Users/sigma/OneDrive/Data/HIV/Geocoding/Premises_Classification_071120.csv')

# Fix the data by clearing replicates
ao_add <- ao %>% 
  bind_cols(data.frame(st_coordinates(.))) %>% 
  st_set_geometry(NULL) %>% 
  group_by(State, License.ID) %>% 
  summarize(X = unique(X), Y = unique(Y),
            Lic_Type = License.Type[which(!is.na(License.Type))],
            Status = Status[which(!is.na(License.Type))],
            D_orig = Date.Original[which(!is.na(License.Type))],
            D_eff = Date.Effective[which(!is.na(License.Type))],
            D_expr = Date.Expiration[which(!is.na(License.Type))],
            Name = Premises.Name[which(!is.na(License.Type))],
            Addr = Premises.Address[which(!is.na(License.Type))],
            Geocoder = unique(Geocoder)) %>% 
  ungroup %>% 
  mutate(Type = plyr::mapvalues(Lic_Type, from = recode$CAT, to = recode$On_Off)) %>% 
  st_as_sf(coords = c('X', 'Y'), crs = 4326) %>% 
  rename(LID = License.ID)

# Double check whether the recoding process was properly done
table(ao %>% filter(State == 'CT') %>% .$License.Type, useNA = 'always')
table(ao_add0 %>% filter(State == 'CT') %>% .$lty, useNA = 'always')

st_write(ao_add, 'C:/Users/sigma/OneDrive/Data/HIV/Geocoding/Geocoding_Premise.shp')

# On-premise
ao_on <- ao_add %>% 
  filter(Type %in% c('On', 'Both'))
# Off-premise
ao_off <- ao_add %>% 
  filter(Type %in% c('Off', 'Both'))