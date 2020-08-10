### last revision: 081020
library(pacman)
p_load(tidyverse, sf, spdep, dtplyr, tmap, rmapshaper)

county_s <- st_read('/mnt/c/Users/sigma/OneDrive/Data/Geo/tl_2018_us_county.shp') %>% 
    st_transform(2163)

county_add <- read_csv('/mnt/c/Users/sigma/OneDrive/Data/HIV/Yusuf/County/Result/County_Attributes_Add.csv')
covid0807 <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/08-07-2020.csv')
dim(covid0807)
colnames(covid0807)

covid0807 <- covid0807 %>% 
       filter(Country_Region == 'US') %>% 
       dplyr::select(-5:-7, -12:-14) %>% 
       mutate(FIPS = sprintf('%05d', FIPS))

## Attribute
county_attr <- read_csv('/mnt/c/Users/sigma/OneDrive/Data/HIV/Yusuf/County/USA_county_variables.csv') %>% 
    mutate(GEOID = sprintf('%05d', as.integer(FIPS))) %>% 
    dplyr::select(-contains('SE_'), -contains('lowerCI'), -contains('upperCI')) %>% 
    left_join(county_add, by = c('GEOID' = 'FIPS'))

## make merged data
county_merge <- county_s %>% 
    left_join(county_attr, by = c('GEOID' = 'GEOID')) %>% 
    left_join(covid0807, by = c('GEOID' = 'FIPS')) %>% 
    dplyr::select(-2:-3,-5:-20)

county_merge_s <- county_merge %>% 
  ms_simplify(keep = 0.1, method = 'dp')

### Misc functions
mapping_select <- function(dat, field_name, basepath){
    tms <- tm_shape(dat) +
        tm_fill(col = field_name, palette = 'Reds', n = 7) +
        tm_borders(col = 'transparent') +
        tm_layout(legend.show = TRUE, legend.position = c('LEFT', 'BOTTOM'))
    fullpath <- str_c(basepath, field_name, '.png')
    if (!is.null(basepath)){
    tmap_save(tm = tms, filename = fullpath,
            height = 15, width = 24, units = 'cm', dpi = 300)
    } else {
      tms
    }
}

#mapping_select(county_merge_s, 'MAX', NULL)

mi_select <- function(dat, field_name){
    dat_vec <- dat %>% 
        st_set_geometry(NULL) %>% 
        .[,field_name] %>% 
        as.vector
    dat_wm <- nb2listw(poly2nb(dat), zero.policy = TRUE)
    dat_mi <- moran.test(dat_vec, listw = dat_wm, zero.policy = TRUE)
    return(dat_mi)
}

lmi_select <- function(dat, field_name, na.action = na.pass){
    dat_vec <- dat %>% 
        st_set_geometry(NULL) %>% 
        .[,field_name] %>% 
        as.vector
    dat_wm <- nb2listw(poly2nb(dat), zero.policy = TRUE)
    dat_lmi <- localmoran(dat_vec, listw = dat_wm, zero.policy = TRUE, na.action = na.pass)
    return(dat_lmi)

}

county_merge_s <- county_merge_s %>% 
  dplyr::select(-87:-89, -92:-93) %>% 
  mutate_at(.vars = vars(alcohol, incarceration, overdose, black_insurance, readmission,
                         pop_SNAP, primary),
            .funs = list(~ifelse(.=='N/A', NA, as.numeric(.))))
for (i in 5:88){mapping_select(county_merge_s, colnames(county_merge_s)[i], basepath = '/mnt/c/Users/sigma/OneDrive/Data/HIV/Yusuf/County/Maps/Values/')}
for (i in 5:88){
  lmid <- lmi_select(county_merge_s, colnames(county_merge_s)[i])
  png(str_c('/mnt/c/Users/sigma/OneDrive/Data/HIV/Yusuf/County/Maps/LocalMoranI/', colnames(county_merge_s)[i], '.png'),
      width = 25, height = 16, units = 'cm', res = 300)
  plot.local.moran(county_merge_s, colnames(county_merge_s)[i], lmid, clw)
  dev.off()
  }


#save(plot.local.moran, lmi_select, mi_select, mapping_select, clw, county_merge_s, file = '/mnt/c/Users/sigma/OneDrive/Data/HIV/Dashboard_base.RData', compress = 'xz')

## Courtsey of https://github.com/gisUTM/spatialplots/blob/master/plotlocalmoran.R
plot.local.moran <- function(x, variable.name, local.moran, weights, sig = 0.05, plot.only.significant = TRUE, legend.location = "bottomleft", zero.policy = NULL){
  if(!inherits(local.moran, "localmoran"))
    stop("local.moran not an object of class localmoran")
  if(!inherits(weights, "listw"))
    stop("weight not a listw")
  
  # Check if local.moran subsetted missing data
  #x <- na.action(local.moran)
  na.act <- na.action(local.moran)
  
  if (!is.null(na.act)) {
    # Rows to drop in weight matrix (weights)
    subVec <- !(1:length(weights$neighbours) %in% na.act)
    
    # Subset weights
    weights <- subset(weights, subVec, zero.policy = zero.policy)
    
    # Subset localmoran
    local.moran <- local.moran[subVec,]
    
    # Subset Polygons
    origPoly <- x
    #x <- subset(x, subVec)
  }
  origPoly <- x
  x <- x %>% st_set_geometry(NULL)
  # Get length of x
  n <- nrow(x)
  
  #
  vec <- c(1:n)
  vec <- ifelse(local.moran[,5] < sig, 1,0)
  
  # Create the lagged variable
  lagvar <- lag.listw(weights, x[[variable.name]], NAOK = TRUE)
  
  # get the mean of each
  m.myvar <- mean(x[[variable.name]], na.rm = TRUE)
  m.lagvar <- mean(lagvar,na.rm = TRUE)
  
  myvar <- x[[variable.name]]
  
  # Derive quadrants
  q <- c(1:n) 
  
  for (i in 1:n){   
    if (is.na(myvar[[i]]) | is.na(lagvar[[i]])){
      q[i] <- NA
    } else {
    if (myvar[[i]]>=m.myvar & lagvar[[i]]>=m.lagvar)
      q[i] <- 1
    if (myvar[[i]]<m.myvar & lagvar[[i]]<m.lagvar) 
      q[i] <- 2
    if (myvar[[i]]<m.myvar & lagvar[[i]]>=m.lagvar) 
      q[i] <- 3   
    if (myvar[[i]]>=m.myvar & lagvar[[i]]<m.lagvar) 
      q[i] <- 4
  }}
  print(length(q))
  # set coloring scheme
  q.all <- q
  colors <- c(1:n)
  for (i in 1:n) {
    if (is.na(q.all[i])){
      colors[i] <- 'black'
    } else {
    if (q.all[i]==1) 
      colors[i] <- "red"
    if (q.all[i]==2) 
      colors[i] <- "blue"
    if (q.all[i]==3) 
      colors[i] <- "lightblue"
    if (q.all[i]==4) 
      colors[i] <- "pink"
    if (q.all[i]==0) 
      colors[i] <- "white"   
    if (q.all[i]>4) 
      colors[i] <- "white"
  }
  }
  
  # Mark all non-significant regions white
  locm.dt <- q*vec
  colors1 <- colors
  for (i in 1:n){
    if ( !(is.na (locm.dt[i])) )  {

      if (locm.dt[i]==0) colors1[i] <- "grey78"

    }
  }
  
  colors2 <- colors
  colors2 <- paste(colors2,vec)
  pos = list()
  for (i in 1:n) {
    pos[[i]] <- c(which(myvar==colors2["blue 0"]))
  }
  
  blue0 <- which(colors2=="blue 0")
  red0 <- which(colors2=="red 0")
  lightblue0 <- which(colors2=="lightblue 0")
  pink0 <- which(colors2=="pink 0")
  lb <- 6
  labels=c("High-High", "High-Low", "Low-High", "Low-Low", "Not Significant", "Missing Data")
  # plot the map
  # Plot out the full set of polygons (missing data will not be overlaid)
  plot(origPoly %>% st_geometry, col = "white", border = T, lwd = 0.01)
  if (plot.only.significant == TRUE){
    plot(origPoly %>% st_geometry, col=colors1,border=F, add = TRUE) 
  }else{
    plot(origPoly %>% st_geometry, col=colors,border=F, add = TRUE)
  }
  legend(legend.location, legend = labels, fill = c("red", "pink", "lightblue", "blue", "grey78", "black"), bty = "n")

  
}


## purrr nest-unnest: combining weight list into the data.frame design