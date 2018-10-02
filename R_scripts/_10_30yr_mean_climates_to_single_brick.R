# set working directory
dir <- "C:/git_niche_tracking/niche_tracking"
setwd(dir)
source("./R_scripts/_0_tools_libraries.R") # loads libraries, buffers, focal area, etc

h_hex <- read.csv("./Rdata_in_out/hlz_hexagonal_centroids.csv")
climate.paths<-read.csv("./Rdata_in_out/annual_climate_paths.csv")

# take all the hlz rasters for each year and combine into one brick
years<- 1925:1980
y<-1925
for (y in years){
  hlz.outfolder <-"./Rdata_in_out/hlz_30yr"
  hlz.outpath <- paste(hlz.outfolder, "/hlz_30yr_", as.character(y), sep="")
  hlz.in<-raster(hlz.outpath)

  if(which(years==y)==1){
    hlz.br <- brick(hlz.in)
  } else {
    hlz.br <- addLayer(hlz.br, hlz.in)
  }
}

names(hlz.br)<- paste("Year: ", years, sep="")
writeRaster(hlz.br, filename="./Rdata_in_out/hlz_30yr/all_hlz_30yr_brick", bandorder='BIL', overwrite=TRUE)

