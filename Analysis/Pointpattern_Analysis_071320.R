## Last revision: 101520
## package
library(pacman)
p_load(sf, tidyverse, stpp, spatstat)


hdir <- '/home/felix/'
hdir <- 'C:/Users/sigma/'
hdir <- '/mnt/c/Users/sigma/'

## spatial data
## subset to NJ, NY, CT
county <- st_read(str_c(hdir, 'OneDrive/Data/Geo/tl_2014_us_county.shp'))
county.ny <- county %>% filter(STATEFP == 36) %>% st_transform(4326)
county.nj <- county %>% filter(STATEFP == 34) %>% st_transform(4326)
county.ct <- county %>% filter(STATEFP == '09') %>% st_transform(4326)

state <- st_read(str_c(hdir, 'OneDrive/Data/Geo/tl_2017_us_state.shp'))
state <- st_read(str_c(hdir, 'OneDrive/Data/Geo/State_2017.gpkg'))
state.ny <- state %>% filter(STATEFP == 36) %>% st_transform(4326)
state.nj <- state %>% filter(STATEFP == 34) %>% st_transform(4326)
state.ct <- state %>% filter(STATEFP == '09') %>% st_transform(4326)

## read geocoded data
prep <- st_read(str_c(hdir, 'OneDrive/Data/HIV/PREP_CDC.gpkg'))
#ny.gcdf <- read_rds(str_c(hdir, 'OneDrive/Data/HIV/NY_Alcohol_Geocoding.rds'))
#premise <- st_read(str_c(hdir, 'OneDrive/Data/HIV/Geocoding_cleaned_NAex.gpkg'))

dd = "Name	FIPS State Numeric Code 	Official USPS Code
Alabama 	01 	AL
Alaska 	02 	AK
Arizona 	04 	AZ
Arkansas 	05 	AR
California 	06 	CA
Colorado 	08 	CO
Connecticut 	09 	CT
Delaware 	10 	DE
District of Columbia 	11 	DC
Florida 	12 	FL
Georgia 	13 	GA
Hawaii 	15 	HI
Idaho 	16 	ID
Illinois 	17 	IL
Indiana 	18 	IN
Iowa 	19 	IA
Kansas 	20 	KS
Kentucky 	21 	KY
Louisiana 	22 	LA
Maine 	23 	ME
Maryland 	24 	MD
Massachusetts 	25 	MA
Michigan 	26 	MI
Minnesota 	27 	MN
Mississippi 	28 	MS
Missouri 	29 	MO
Montana 	30 	MT
Nebraska 	31 	NE
Nevada 	32 	NV
New Hampshire 	33 	NH
New Jersey 	34 	NJ
New Mexico 	35 	NM
New York 	36 	NY
North Carolina 	37 	NC
North Dakota 	38 	ND
Ohio 	39 	OH
Oklahoma 	40 	OK
Oregon 	41 	OR
Pennsylvania 	42 	PA
Rhode Island 	44 	RI
South Carolina 	45 	SC
South Dakota 	46 	SD
Tennessee 	47 	TN
Texas 	48 	TX
Utah 	49 	UT
Vermont 	50 	VT
Virginia 	51 	VA
Washington 	53 	WA
West Virginia 	54 	WV
Wisconsin 	55 	WI
Wyoming 	56 	WY"
ddd <- read.table(text = dd, sep = '\t', header = T)


## On- and Off-premise
#alc <- st_read(str_c(hdir, 'OneDrive/Data/HIV/Geocoding/Geocoding_Premise_Cleaned_Active.shp'))
alc <- read_csv('/mnt/c/Users/sigma/OneDrive/Data/HIV/Geocoding/Geocoding_Base_092420_Geocodio.csv') %>% 
    st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)
alc <- alc %>% st_transform(2163)
prep <- prep %>% st_transform(2163)
state.ct <- state.ct %>% st_transform(2163)

ctsf <- prep %>% filter(grepl('Connecticut', State)) %>% mutate(type = 'PrEP') %>% 
    dplyr::select(type) %>% st_transform(crs = 2163)

ct.gcdf <- alc %>% filter(State == 'CT')
ct.alc <- ct.gcdf %>% 
    filter(Type %in% c('On', 'Both') & Status == 'ACTIVE') %>% 
    mutate(type = 'On-premise') %>% 
    dplyr::select(type) %>% 
    .[state.ct,] %>% 
    mutate(geom = geometry) %>% 
    st_drop_geometry()
st_geometry(ct.alc) <- ct.alc$geom

ctppp <- rbind(ctsf, ct.alc) %>%
    dplyr::select(type)
ctppp <- ppp(st_coordinates(ctppp)[,1],
             st_coordinates(ctppp)[,2],
             c(st_bbox(ctppp)[c(1,3)]),
             c(st_bbox(ctppp)[c(2,4)]),
             marks = factor(ctppp$type, levels = c('PrEP', 'On-premise')))

## crossL
ctppp.k <- Kcross(ctppp)
ctppp.ki <- Lcross.inhom(ctppp)
plot(ctppp.k)
plot(ctppp.ki)


data_to_ppp <- function(state.name, state.code, state, 
                        premise, prep.c = prep, type1='PrEP', type2='On', levelset = c(type1, type2), 
                        bound = 'poly', diggle = FALSE, inhom= TRUE, versa = F){
    state.s <- state %>% st_transform(2163)
    stsf <- prep.c %>% 
        filter(grepl(state.name, State)) %>% 
        mutate(type = type1) %>% 
        dplyr::select(type) %>% st_transform(crs = 2163) %>% 
        .[state.s,]
    st.gcdf <- premise %>% filter(State == state.code & Type %in% c(type2, 'Both'))
    st.alc <- st.gcdf %>% 
        mutate(type = type2) %>% 
        dplyr::select(type) %>% 
        .[state.s,] %>% 
        mutate(geom = geometry) %>% 
        st_drop_geometry
    st_geometry(st.alc) <- st.alc$geom
    
    stppp <- rbind(stsf, st.alc)

    if (bound == 'rect'){
        stppp <- ppp(st_coordinates(stppp)[,1],
                     st_coordinates(stppp)[,2],
                     window = as.owin(poly = state %>% st_transform(2163) %>% st_geometry),
                     marks = factor(stppp$type, levels = levelset))
    } else {
        owin.poly <- owin(poly = state %>% st_transform(2163) %>% st_coordinates %>%
                            .[nrow(.):1, 1:2] %>% list(x = .[,1], y = .[,2]))
        stppp <- ppp(st_coordinates(stppp)[,1],
                     st_coordinates(stppp)[,2],
                     window = owin.poly,
                     marks = factor(stppp$type, levels = levelset))
        
    }
    stppp.prep <- split(stppp)[type1][[1]] %>% density.ppp(at = 'points', diggle = diggle)
    stppp.prem <- split(stppp)[type2][[1]] %>% density.ppp(at = 'points', diggle = diggle)
    if (inhom){
        if (versa){
            stppp.li <- Lcross.inhom(stppp, type2, type1, stppp.prem, stppp.prep)
            stppp.ji <- Jcross(stppp, type2, type1)
        } else {
            stppp.li <- Lcross.inhom(stppp, type1, type2, stppp.prep, stppp.prem)
            stppp.ji <- Jcross(stppp, type1, type2)
        }
        #stppp.env <- envelope(stppp, fun = Lcross.inhom, nsim = 99, funargs = list(i=type1, j=type2, lambdaI=stppp.prep, lambdaJ=stppp.prem,
        #correction = 'Ripley'))
    } else {
        if (versa) {
            stppp.li <- Lcross(stppp, type2, type1)
            stppp.ji <- Jcross(stppp, type2, type1)
        } else {
            stppp.li <- Lcross(stppp, type1, type2)
            stppp.ji <- Jcross(stppp, type1, type2)
        }
        #stppp.env <- envelope(stppp, fun = Lcross, nsim = 99, funargs = list(i=type1, j=type2,
        #correction = 'Ripley'))

    }
    #return(stppp)
    return(list(stppp.li, stppp.ji, stppp))
}

# PrEP-Premise
system.time(ct.on <- data_to_ppp('Connecticut', 'CT', state = state.ct, premise = alc, type2= 'On'))
system.time(ct.off <- data_to_ppp('Connecticut', 'CT', state = state.ct, premise = alc, type2= 'Off'))
system.time(ny.on <- data_to_ppp('New York', 'NY', state = state.ny, premise = alc, type2 = 'On', bound = 'poly'))
system.time(ny.off <- data_to_ppp('New York', 'NY', state = state.ny, premise = alc, type2 = 'Off', bound = 'poly'))
system.time(nj.on <- data_to_ppp('New Jersey', 'NJ', state = state.nj, premise = alc, type2 = 'On', bound = 'poly'))
system.time(nj.off <- data_to_ppp('New Jersey', 'NJ', state = state.nj, premise = alc, type2 = 'Off', bound = 'poly'))

# Premise-PrEP
system.time(ct.on.r <- data_to_ppp('Connecticut', 'CT', state = state.ct, premise = alc, type2= 'On', levelset = c('On', 'PrEP'), versa = T))
system.time(ct.off.r <- data_to_ppp('Connecticut', 'CT', state = state.ct, premise = alc, type2= 'Off', levelset = c('Off', 'PrEP'), versa = T))
system.time(ny.on.r <- data_to_ppp('New York', 'NY', state = state.ny, premise = alc, type2 = 'On', levelset = c('On', 'PrEP'), bound = 'poly', versa = T))
system.time(ny.off.r <- data_to_ppp('New York', 'NY', state = state.ny, premise = alc, type2 = 'Off', levelset = c('Off', 'PrEP'), bound = 'poly', versa = T))
system.time(nj.on.r <- data_to_ppp('New Jersey', 'NJ', state = state.nj, premise = alc, type2 = 'On', levelset = c('On', 'PrEP'), bound = 'poly', versa = T))
system.time(nj.off.r <- data_to_ppp('New Jersey', 'NJ', state = state.nj, premise = alc, type2 = 'Off', levelset = c('Off', 'PrEP'), bound = 'poly', versa = T))



system.time(ct.onh <- data_to_ppp('Connecticut', 'CT', state = state.ct, premise = alc, type2= 'On', inhom = F))
system.time(ct.offh <- data_to_ppp('Connecticut', 'CT', state = state.ct, premise = alc, type2= 'Off', inhom = F))

plot(ct.on[[1]], .-r~.x, asp = 1)
plot(ct.on.r[[1]], .-r~.x, asp = 1)
plot(ct.off[[1]], .-r~.x)

plot(ny.on[[1]], .-r~.x)
plot(ny.off[[1]], .-r~.x)

plot(nj.on.r[[1]], .-r~.x)
plot(nj.off.r[[1]], .-r~.x)

# Write plots
write_plots <- function(paa, state, type, tdir = '/mnt/c/Users/sigma/OneDrive/Data/HIV/Geocoding/'){
    png(str_c(tdir, 'PA_', state, '_', type, '.png'),
        width = 20, height = 22, res = 300, units = 'cm')
    plot(paa[[1]], .-r~.x, main = str_c(state, ' (', type, ')'), asp = 1, xlim = c(0, 30000))
    dev.off()
}

write_plots(ct.on, 'Connecticut', 'On')
write_plots(ct.off, 'Connecticut', 'Off')
write_plots(ny.on, 'New York', 'On')
write_plots(ny.off, 'New York', 'Off')
write_plots(nj.on, 'New Jersey', 'On')
write_plots(nj.off, 'New Jersey', 'Off')

write_plots(ct.on.r, 'Connecticut', 'On')
write_plots(ct.off.r, 'Connecticut', 'Off')
write_plots(ny.on.r, 'New York', 'On')
write_plots(ny.off.r, 'New York', 'Off')
write_plots(nj.on.r, 'New Jersey', 'On')
write_plots(nj.off.r, 'New Jersey', 'Off')



### Custom polygon
data_to_ppp2 <- function(poly, field, group = 'up',#state.name, state.code, state, 
                        premise, prep.c = prep, type1='PrEP', type2='On', levelset = c(type1, type2), 
                        bound = 'poly', diggle = FALSE, inhom= TRUE, versa = F){
    #state.s <- state %>% st_transform(2163)
    group_ind <- ifelse(group == 'up', 1, 0)
    poly.su <- poly %>% 
        filter(!!rlang::sym(field) == group_ind)

    stsf <- prep.c %>% 
        mutate(type = type1) %>% 
        dplyr::select(type) %>% st_transform(crs = 2163) %>% 
        .[poly.su,]
    st.gcdf <- premise %>% filter(Type %in% c(type2, 'Both'))
    st.alc <- st.gcdf %>% 
        mutate(type = type2) %>% 
        dplyr::select(type) %>% 
        .[poly.su,] %>% 
        mutate(geom = geometry) %>% 
        st_drop_geometry
    st_geometry(st.alc) <- st.alc$geom
    
    stppp <- rbind(stsf, st.alc)

    if (bound == 'rect'){
        stppp <- ppp(st_coordinates(stppp)[,1],
                     st_coordinates(stppp)[,2],
                     window = as.owin(poly = poly.su %>% st_transform(2163) %>% st_geometry),
                     marks = factor(stppp$type, levels = levelset))
    } else {
        owin.poly <- owin(poly = poly.su %>% st_transform(2163) %>% st_coordinates %>%
                            .[nrow(.):1, 1:2] %>% list(x = .[,1], y = .[,2]))
        stppp <- ppp(st_coordinates(stppp)[,1],
                     st_coordinates(stppp)[,2],
                     window = owin.poly,
                     marks = factor(stppp$type, levels = levelset))
        
    }
    stppp.prep <- split(stppp)[type1][[1]] %>% density.ppp(at = 'points', diggle = diggle)
    stppp.prem <- split(stppp)[type2][[1]] %>% density.ppp(at = 'points', diggle = diggle)
    if (inhom){
        if (versa){
            stppp.li <- Lcross.inhom(stppp, type2, type1, stppp.prem, stppp.prep)
            stppp.ji <- Jcross(stppp, type2, type1)
        } else {
            stppp.li <- Lcross.inhom(stppp, type1, type2, stppp.prep, stppp.prem)
            stppp.ji <- Jcross(stppp, type1, type2)
        }
        #stppp.env <- envelope(stppp, fun = Lcross.inhom, nsim = 99, funargs = list(i=type1, j=type2, lambdaI=stppp.prep, lambdaJ=stppp.prem,
        #correction = 'Ripley'))
    } else {
        if (versa) {
            stppp.li <- Lcross(stppp, type2, type1)
            stppp.ji <- Jcross(stppp, type2, type1)
        } else {
            stppp.li <- Lcross(stppp, type1, type2)
            stppp.ji <- Jcross(stppp, type1, type2)
        }
        #stppp.env <- envelope(stppp, fun = Lcross, nsim = 99, funargs = list(i=type1, j=type2,
        #correction = 'Ripley'))

    }
    #return(stppp)
    return(list(stppp.li, stppp.ji, stppp))
}

# Above
ppp_c10_On <- data_to_ppp2(county_t, 'r_hiv_c10', premise = alc_s, prep.c = prep_s)
ppp_c10_Off <- data_to_ppp2(county_t, 'r_hiv_c10', premise = alc_s, prep.c = prep_s, type2 = 'Off')
ppp_c20_On <- data_to_ppp2(county_t, 'r_hiv_c20', premise = alc_s, prep.c = prep_s)
ppp_c20_Off <- data_to_ppp2(county_t, 'r_hiv_c20', premise = alc_s, prep.c = prep_s, type2 = 'Off')
ppp_c30_On <- data_to_ppp2(county_t, 'r_hiv_c30', premise = alc_s, prep.c = prep_s)
ppp_c30_Off <- data_to_ppp2(county_t, 'r_hiv_c30', premise = alc_s, prep.c = prep_s, type2 = 'Off')
ppp_c40_On <- data_to_ppp2(county_t, 'r_hiv_c40', premise = alc_s, prep.c = prep_s)
ppp_c40_Off <- data_to_ppp2(county_t, 'r_hiv_c40', premise = alc_s, prep.c = prep_s, type2 = 'Off')
ppp_c50_On <- data_to_ppp2(county_t, 'r_hiv_c50', premise = alc_s, prep.c = prep_s)
ppp_c50_Off <- data_to_ppp2(county_t, 'r_hiv_c50', premise = alc_s, prep.c = prep_s, type2 = 'Off')

# Below
ppp_c10u_On <- data_to_ppp2(county_t, 'r_hiv_c10', group = 'd', premise = alc_s, prep.c = prep_s)
ppp_c10u_Off <- data_to_ppp2(county_t, 'r_hiv_c10', group = 'd', premise = alc_s, prep.c = prep_s, type2 = 'Off')
ppp_c20u_On <- data_to_ppp2(county_t, 'r_hiv_c20', group = 'd', premise = alc_s, prep.c = prep_s)
ppp_c20u_Off <- data_to_ppp2(county_t, 'r_hiv_c20', group = 'd', premise = alc_s, prep.c = prep_s, type2 = 'Off')
ppp_c30u_On <- data_to_ppp2(county_t, 'r_hiv_c30', group = 'd', premise = alc_s, prep.c = prep_s)
ppp_c30u_Off <- data_to_ppp2(county_t, 'r_hiv_c30', group = 'd', premise = alc_s, prep.c = prep_s, type2 = 'Off')
ppp_c40u_On <- data_to_ppp2(county_t, 'r_hiv_c40', group = 'd', premise = alc_s, prep.c = prep_s)
ppp_c40u_Off <- data_to_ppp2(county_t, 'r_hiv_c40', group = 'd', premise = alc_s, prep.c = prep_s, type2 = 'Off')
ppp_c50u_On <- data_to_ppp2(county_t, 'r_hiv_c50', group = 'd', premise = alc_s, prep.c = prep_s)
ppp_c50u_Off <- data_to_ppp2(county_t, 'r_hiv_c50', group = 'd', premise = alc_s, prep.c = prep_s, type2 = 'Off')


load('/mnt/c/Users/sigma/OneDrive/Data/HIV/PointPattern_county_stratified_081520.RData')
plot(ppp_c10_On[[1]], lwd = 2, main = 'PrEP-On (Upper 10%)')
plot(ppp_c10u_On[[1]], lwd = 2, main = 'PrEP-On (Below 90%)')
plot(ppp_c10_Off[[1]], lwd = 2, main = 'PrEP-Off (Upper 10%)')
plot(ppp_c10u_Off[[1]], lwd = 2, main = 'PrEP-Off (Below 90%)')
plot(ppp_c20_On[[1]], lwd = 2, main = 'PrEP-On (Upper 20%)')
plot(ppp_c20u_On[[1]], lwd = 2, main = 'PrEP-On (Below 80%)')
plot(ppp_c20_Off[[1]], lwd = 2, main = 'PrEP-Off (Upper 20%)')
plot(ppp_c20u_Off[[1]], lwd = 2, main = 'PrEP-Off (Below 80%)')
plot(ppp_c30_On[[1]], lwd = 2, main = 'PrEP-On (Upper 30%)')
plot(ppp_c30u_On[[1]], lwd = 2, main = 'PrEP-On (Below 70%)')
plot(ppp_c30_Off[[1]], lwd = 2, main = 'PrEP-Off (Upper 30%)')
plot(ppp_c30u_Off[[1]], lwd = 2, main = 'PrEP-Off (Below 70%)')
plot(ppp_c40_On[[1]], lwd = 2, main = 'PrEP-On (Upper 40%)')
plot(ppp_c40u_On[[1]], lwd = 2, main = 'PrEP-On (Below 60%)')
plot(ppp_c40_Off[[1]], lwd = 2, main = 'PrEP-Off (Upper 40%)')
plot(ppp_c40u_Off[[1]], lwd = 2, main = 'PrEP-Off (Below 60%)')
plot(ppp_c50_On[[1]], lwd = 2, main = 'PrEP-On (Upper 50%)')
plot(ppp_c50u_On[[1]], lwd = 2, main = 'PrEP-On (Below 50%)')
plot(ppp_c50_Off[[1]], lwd = 2, main = 'PrEP-Off (Upper 50%)')
plot(ppp_c50u_Off[[1]], lwd = 2, main = 'PrEP-Off (Below 50%)')
dev.off()

save(list=ls()[grep('ppp_c.*', ls())], file = 'PointPattern_county_stratified_081520.RData')