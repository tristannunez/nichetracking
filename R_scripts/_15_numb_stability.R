# set working directory
dir <- "C:/git_niche_tracking/niche_tracking"
setwd(dir)
source("./R_scripts/_0_tools_libraries.R") # loads libraries, buffers, focal area, etc
#  define the tracking function

h_hex <- read.csv("./Rdata_in_out/hlz_hexagonal_centroids.csv")
climate.paths<-read.csv("./Rdata_in_out/annual_climate_paths.csv")

head(climate.paths)

# create brick of MABT
# take all the hlz rasters for each year and combine into one brick
years<- 1895:1980

for (y in years){
  mabt.in<-raster(as.character(climate.paths$mabt.paths[which(climate.paths$year==y)]))
  
  if(which(years==y)==1){
    mabt.br <- brick(mabt.in)
  } else {
    mabt.br <- addLayer(mabt.br, mabt.in)
  }
}

mabt.br
names(mabt.br)<- paste("Year: ", years, sep="")
#animate(mabt.br)

for (y in years){
  atap.in<-raster(as.character(climate.paths$atap.paths[which(climate.paths$year==y)]))
  
  if(which(years==y)==1){
    atap.br <- brick(atap.in)
  } else {
    atap.br <- addLayer(atap.br, atap.in)
  }
}

atap.br
animate(atap.br)


# then calculate a 30-yr running average ...

avg_yrs <- (1895+30):1980
yr<-1926
brick.in <- atap.br
thirty_year_avg <- function(brick.in, avg_yrs){
 for(yr in avg_yrs){
   print(yr)
   yrnum <- which(avg_yrs==yr)
   brick.window <- brick.in[[1:(yrnum+29)]]  
   window.mean <- mean(brick.window)
#   plot(window.mean)
   if(which(avg_yrs==yr)==1){
     wmean.br <- brick(window.mean)
   } else {
     wmean.br <- addLayer(wmean.br, window.mean)
   }
}
  return(brick(wmean.br))
} # end thirty_year_avg

atap.30yr.mean <- thirty_year_avg(brick.in=atap.br, avg_yrs=(1895+30):1980)
writeRaster(atap.30yr.mean, filename="./Rdata_in_out/atap.30yr.mean", bandorder='BIL', overwrite=TRUE)

mabt.30yr.mean <- thirty_year_avg(brick.in=mabt.br, avg_yrs=(1895+30):1980)
writeRaster(mabt.30yr.mean, filename="./Rdata_in_out/mabt.30yr.mean", bandorder='BIL', overwrite=TRUE)



hlzs <- c("desert", "scrub", "wetforest")
lifezone <- "desert"
series<-brick(paste("./Rdata_in_out/single_hlz_series/",lifezone, sep=""), bandorder='BIL', overwrite=TRUE)


for(lifezone in (unique(combos.ord$hlz.name))){
  print(lifezone)
  focal.hlz.name <- lifezone
  combos.foc <-combos.ord[combos.ord$hlz.name==lifezone,]
  # then use this to read in the series...
  # read in single hlz series
  hlz.series <- brick(paste("./Rdata_in_out/single_hlz_series/",lifezone, sep=""))
  