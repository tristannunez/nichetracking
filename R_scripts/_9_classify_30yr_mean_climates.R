# set working directory
dir <- "C:/git_niche_tracking/niche_tracking"
setwd(dir)
source("./R_scripts/_0_tools_libraries.R") # loads libraries, buffers, focal area, etc
#  define the tracking function

h_hex <- read.csv("./Rdata_in_out/hlz_hexagonal_centroids.csv")
climate.paths<-read.csv("./Rdata_in_out/annual_climate_paths.csv")

atap.30yr.mean <- brick("./Rdata_in_out/atap_30yr_mean")
mabt.30yr.mean <- brick("./Rdata_in_out/mabt_30yr_mean")



# define functions, pull out centroids, etc
# specify centroids externally
cent.x <- h_hex$mid_x
cent.y <- h_hex$mid_y

# list of hlz centroids
cent.df <- data.frame(cent.x, cent.y)
cent.list <- list()
for(i in 1:nrow(cent.df)){
  cent.list[[i]]<-cent.df[i,]
}
cent.list

# vector of years to loop through
#years<-1962:1980

years <- 1925:1980 
length(years)
nlayers(mabt.30yr.mean)


# 
#y<-1925
# starts loop that classifies each year
for(y in years){
  print(paste("starting year ", y, sep=""))  
  yrnum <- which(years==y)
  mabt.ras <- mabt.30yr.mean[[yrnum]] 
  atap.ras <- atap.30yr.mean[[yrnum]] 
  # get values from climate rasters
  mabt.vals <- values(mabt.ras)
  atap.vals <- values(atap.ras)
  
  for(i in 1:length(cent.list)){
    print(paste("calculating distance to centroid " ,i, " of 30", sep=""))
    centroids <- cent.list[[i]]
    x1<- as.numeric(centroids[1])
    y1 <- as.numeric(centroids[2])
    y0 <- log2(mabt.ras/1.5)
    x0 <- log2(atap.ras/62.5)
    dx <- x1-x0
    dy <- y1-y0
    #plot(dx)
    #plot(dy)
    dxdy.stack<-stack(dx,dy)
    #plot(dxdy.stack)
    
    distCalc <- function(x){ 
      dx <- x[1]
      dy <- x[2]
      if(!is.na(dx)&!is.na(dy)){
        if(sign(dx)==sign(dy)){
          dist.out <-abs(dx+dy)
          as.numeric(dist.out)
        } else {
          dist.out <- max(abs(dx), abs(dy))
          as.numeric(dist.out)
        }
      } else {
        NA
      } 
    }
    
    dist.cent <-calc(dxdy.stack, fun=distCalc)
    if(i==1){
      outBrick <- brick(dist.cent)
    } else {
      outBrick <- addLayer(outBrick, dist.cent)
    }
  }
  
  whichMin <- function(x){
    which(x==min(x))[1]
  }
  
  hlz <- calc(outBrick, fun=whichMin)
  plot(hlz) # this needs checking ...
  
  hlz.outfolder <-"./Rdata_in_out/hlz_30yr"
  hlz.outpath <- paste(hlz.outfolder, "/hlz_30yr_", as.character(y), sep="")
  writeRaster(hlz, filename=hlz.outpath, bandorder='BIL', overwrite=TRUE)
} # end year by year loop












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
  