## package
library(pacman)
p_load(sf, tidyverse, stpp, spatstat)

# The directory and file names should be changed before the main analysis is conducted
hdir <- '/mnt/c/Users/sigma/'
hdir <- '/mnt/c/Users/isong/'

## spatial data
## subset to NJ, NY, CT (revised after Sept 2020)
state <- st_read(str_c(hdir, 'OneDrive/Data/Geo/State_2017.gpkg'))
state.ny <- state %>% filter(STATEFP == 36) %>% st_transform(4326)
state.nj <- state %>% filter(STATEFP == 34) %>% st_transform(4326)
state.ct <- state %>% filter(STATEFP == '09') %>% st_transform(4326)

state.ct <- state.ct %>% st_transform(2163)
state.ny <- state.ny %>% st_transform(2163)
state.nj <- state.nj %>% st_transform(2163)


## read geocoded data
prep <- st_read(str_c(hdir, 'OneDrive/Data/HIV/PREP_CDC.gpkg'))

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
alc <- read_csv(str_c(hdir, 'OneDrive/Data/HIV/Geocoding/Geocoding_Base_092420_geocodio.csv'))
alc <- alc %>% 
    st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) %>% 
    st_transform(2163)
prep <- prep %>% st_transform(2163)

doParallel::stopImplicitCluster()
#doParallel::registerDoParallel(22)
parallel::makePSOCKcluster(11)
## crossL
data_to_ppp <- function(state.name, state.code, state, 
                        premise, 
                        prep.c = prep, 
                        type1='PrEP', 
                        type2='On', 
                        levelset = c(type1, type2), 
                        bound = 'poly', # boundary being set as the state polygon 
                        diggle = FALSE, # Diggle's correction
                        inhom= TRUE,    # Inhomogeneous Ripley's K (and its derived L)
                        versa = F,     # Will the type1 and type2 be applied vice versa?
                        envelope = FALSE){ # Will you make a envelope for 99 sims?
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
        stppp <- ppp(st_coordinates(stppp)[,1]/1000, # meter to kilometer
                     st_coordinates(stppp)[,2]/1000,
                     window = as.owin(poly = state %>% st_transform(2163) %>% st_geometry),
                     marks = factor(stppp$type, levels = levelset))
    } else {
        owin.poly <- owin(poly = state %>% st_transform(2163) %>% st_coordinates %>%
                            .[nrow(.):1, 1:2] %>% list(x = .[,1]/1000, y = .[,2]/1000))
        stppp <- ppp(st_coordinates(stppp)[,1]/1000,
                     st_coordinates(stppp)[,2]/1000,
                     window = owin.poly,
                     marks = factor(stppp$type, levels = levelset))
        
    }

        stppp.prep <- split(stppp)[type1][[1]] %>% density.ppp(at = 'points', diggle = diggle)
        stppp.prem <- split(stppp)[type2][[1]] %>% density.ppp(at = 'points', diggle = diggle)

    if (inhom){
        if (versa){
            stppp.li <- Lcross.inhom(stppp, type2, type1, stppp.prem, stppp.prep)
            #stppp.ji <- Jcross(stppp, type2, type1)
            if (envelope){
                ppplist <- replicate(11, stppp, simplify = FALSE)
                envlist <- parallel::mclapply(ppplist, spatstat.core::envelope, savefuns = TRUE, 
                                              fun = spatstat.core::Lcross.inhom, funargs = list(i = type2, j = type1),
                                              nsim = 9, mc.cores = 11)
                envfinal <- do.call(pool, envlist)
            }
        } else {
            stppp.li <- Lcross.inhom(stppp, type1, type2, stppp.prep, stppp.prem)
            #stppp.ji <- Jcross(stppp, type1, type2)
            if (envelope){
                ppplist <- replicate(11, stppp, simplify = FALSE)
                envlist <- parallel::mclapply(ppplist, spatstat.core::envelope, savefuns = TRUE, 
                                              fun = spatstat.core::Lcross.inhom, funargs = list(i = type2, j = type1),
                                              nsim = 9, mc.cores = 11)
                envfinal <- do.call(pool, envlist)
            }
        }
        #stppp.env <- envelope(stppp, fun = Lcross.inhom, nsim = 99, funargs = list(i=type1, j=type2, lambdaI=stppp.prep, lambdaJ=stppp.prem,
        #correction = 'Ripley'))
    } else {
        if (versa) {
            stppp.li <- Lcross(stppp, type2, type1)
            if (envelope){
                ppplist <- replicate(11, stppp, simplify = FALSE)
                envlist <- parallel::mclapply(ppplist, spatstat.core::envelope, savefuns = TRUE, 
                                              fun = spatstat.core::Lcross.inhom, funargs = list(i = type2, j = type1),
                                              nsim = 9, mc.cores = 11)
                envfinal <- do.call(pool, envlist)
            }
            #stppp.ji <- Jcross(stppp, type2, type1)
        } else {
            stppp.li <- Lcross(stppp, type1, type2)
            #stppp.ji <- Jcross(stppp, type1, type2)
            if (envelope){
                ppplist <- replicate(11, stppp, simplify = FALSE)
                envlist <- parallel::mclapply(ppplist, spatstat.core::envelope, savefuns = TRUE, 
                                              fun = spatstat.core::Lcross.inhom, funargs = list(i = type2, j = type1),
                                              nsim = 9, mc.cores = 11)
                envfinal <- do.call(pool, envlist)
            }
        }
        #stppp.env <- envelope(stppp, fun = Lcross, nsim = 99, funargs = list(i=type1, j=type2,
        #correction = 'Ripley'))

    }
    #return(stppp)
    return(list(Lcross = stppp.li, ppp = stppp, envelope = envfinal))
}

system.time(ct.on <- data_to_ppp('Connecticut', 'CT', state = state.ct, premise = alc, type2= 'On', bound = 'poly', envelope = TRUE))
system.time(ct.off <- data_to_ppp('Connecticut', 'CT', state = state.ct, premise = alc, type2= 'Off', bound = 'poly', envelope = TRUE))
system.time(ny.on <- data_to_ppp('New York', 'NY', state = state.ny, premise = alc, type2 = 'On', bound = 'poly', envelope = TRUE))
system.time(ny.off <- data_to_ppp('New York', 'NY', state = state.ny, premise = alc, type2 = 'Off', bound = 'poly', envelope = TRUE))
system.time(nj.on <- data_to_ppp('New Jersey', 'NJ', state = state.nj, premise = alc, type2 = 'On', bound = 'poly', envelope = TRUE))
system.time(nj.off <- data_to_ppp('New Jersey', 'NJ', state = state.nj, premise = alc, type2 = 'Off', bound = 'poly', envelope = TRUE))

save(ct.on, ct.off, ny.on, ny.off, nj.on, nj.off, file = str_c(hdir, 'OneDrive/Data/HIV/PointPattern_Analysis_041421.RData'))
load(str_c(hdir, 'OneDrive/Data/HIV/PointPattern_Analysis_041421.RData'))

# vice versa
system.time(ct.on.versa <- data_to_ppp('Connecticut', 'CT', state = state.ct, premise = alc, type2= 'On', bound = 'poly', versa = TRUE))
system.time(ct.off.versa <- data_to_ppp('Connecticut', 'CT', state = state.ct, premise = alc, type2= 'Off', bound = 'poly', versa = TRUE))
system.time(ny.on.versa <- data_to_ppp('New York', 'NY', state = state.ny, premise = alc, type2 = 'On', bound = 'poly', versa = TRUE))
system.time(ny.off.versa <- data_to_ppp('New York', 'NY', state = state.ny, premise = alc, type2 = 'Off', bound = 'poly', versa = TRUE))
system.time(nj.on.versa <- data_to_ppp('New Jersey', 'NJ', state = state.nj, premise = alc, type2 = 'On', bound = 'poly', versa = TRUE))
system.time(nj.off.versa <- data_to_ppp('New Jersey', 'NJ', state = state.nj, premise = alc, type2 = 'Off', bound = 'poly', versa = TRUE))


# cross-L function plot
plot(ct.on[[3]], obs-r~.x)
plot(ct.off[[1]], .-r~.x)

plot(ny.on[[1]], .-r~.x)
plot(ny.off[[1]], .-r~.x)

plot(nj.on[[1]], .-r~.x)
plot(nj.off[[1]], .-r~.x)

# cross-L function plot (vice versa)
plot(ct.on.versa[[1]], .-r~.x)
plot(ct.off.versa[[1]], .-r~.x)

plot(ny.on.versa[[1]], .-r~.x)
plot(ny.off.versa[[1]], .-r~.x)

plot(nj.on.versa[[1]], .-r~.x)
plot(nj.off.versa[[1]], .-r~.x)



# Write plots
write_plots <- function(paa, state, type, tdir = str_c(hdir, 'OneDrive/Data/HIV/'), versa = FALSE){
    png(filename = str_c(tdir, 'PA_', state, '_', type, '.png'),
        width = 20, height = 22, res = 300, units = 'cm')
    if (versa){
        maintext <- state
        #maintext <- str_c(state, ' (', type, ' and PrEP)')
    } else {
        maintext <- state
        #maintext <- str_c(state, ' (PrEP and ', type, ')')
    }
    paw <- paa[[3]]
    paw$lo <- paw$lo - paw$r
    paw$hi <- paw$hi - paw$r
    paw$theo <- paw$theo - paw$r

    xlimit <- range(paa[[1]]$r)
    ylimit <- range(paa[[1]]$border - paa[[1]]$r)
    ylimit[2] <- ylimit[2] * 1.07
    plot(paw, cbind(lo, hi, theo)~r,#cbind(lo-r, hi-r, theo-r)~r, shade = fvnames(paa[[3]], ".s-r"),
            #add = TRUE, 
            lwd = 2,
            main = maintext, 
        legend = FALSE, 
        xlim = xlimit,
        ylim = ylimit)
    plot(paa[[1]], border - r~.x, add = TRUE, lwd = 2, col = 'red') 
    legend('topright',
           lty = c(1, 2),
           col = c('red', 'green'),
           lwd = 2,
           legend = c(expression({hat(L)[list(inhom,PrEP,On)]^{bord}}(r) - r),
                      expression({L[list(inhom,On,PrEP)]^{theo}}(r) - r )))
    #plot(paa[[3]], obs-r~.x, add = TRUE, lwd = 1.5)
    #plot(paa[[3]], lo-r~.x, add = TRUE, lwd = 1.5)
    #plot(paa[[3]], hi-r~.x, add = TRUE, lwd = 1.5)
    #plot(paa[[2]])
    dev.off()
}

write_plots(ct.on, 'Connecticut', 'On')
write_plots(ct.off, 'Connecticut', 'Off')
write_plots(ny.on, 'New York', 'On')
write_plots(ny.off, 'New York', 'Off')
write_plots(nj.on, 'New Jersey', 'On')
write_plots(nj.off, 'New Jersey', 'Off')


write_pdf <- function(paa, state, type, lwd = 1.5, tdir = str_c(hdir, 'OneDrive/Data/HIV/'), versa = FALSE){
    if (versa){
        maintext <- str_c(state, ' (', type, ' and PrEP)')
    } else {
        maintext <- str_c(state, ' (PrEP and ', type, ')')
    }

    plot(paa[[1]], .-r~.x, main = maintext, lwd = lwd)
    
}


write_plots(ct.on.versa, 'Connecticut', 'On', versa = TRUE)
write_plots(ct.off.versa, 'Connecticut', 'Off', versa = TRUE)
write_plots(ny.on.versa, 'New York', 'On', versa = TRUE)
write_plots(ny.off.versa, 'New York', 'Off', versa = TRUE)
write_plots(nj.on.versa, 'New Jersey', 'On', versa = TRUE)
write_plots(nj.off.versa, 'New Jersey', 'Off', versa = TRUE)


pdf('/mnt/c/Users/sigma/OneDrive/Data/HIV/PrEP-Alcohol/CrossL_033121.pdf')
write_pdf(ct.on, 'Connecticut', 'On')
write_pdf(ct.off, 'Connecticut', 'Off')
write_pdf(ny.on, 'New York', 'On')
write_pdf(ny.off, 'New York', 'Off')
write_pdf(nj.on, 'New Jersey', 'On')
write_pdf(nj.off, 'New Jersey', 'Off')

write_pdf(ct.on.versa, 'Connecticut', 'On', versa = TRUE)
write_pdf(ct.off.versa, 'Connecticut', 'Off', versa = TRUE)
write_pdf(ny.on.versa, 'New York', 'On', versa = TRUE)
write_pdf(ny.off.versa, 'New York', 'Off', versa = TRUE)
write_pdf(nj.on.versa, 'New Jersey', 'On', versa = TRUE)
write_pdf(nj.off.versa, 'New Jersey', 'Off', versa = TRUE)
dev.off()