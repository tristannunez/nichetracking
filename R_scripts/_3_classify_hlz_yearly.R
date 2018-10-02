# set working directory
dir <- "F:/git_niche_tracking/niche_tracking"
setwd(dir)
source("./R_scripts/_0_tools_libraries.R") # loads libraries, buffers, focal area, etc

h_hex <- read.csv("./Rdata_in_out/hlz_hexagonal_centroids.csv")
climate.paths<-read.csv("./Rdata_in_out/annual_climate_paths.csv")

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

years <- climate.paths$year 

# starts loop that classifies each year
for(y in years){
print(paste("starting year ", y, sep=""))  
mabt.ras <- raster(as.character(climate.paths$mabt.paths[climate.paths$year==y]))
atap.ras <- raster(as.character(climate.paths$atap.paths[climate.paths$year==y]))
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

hlz.outfolder <-"./Rdata_in_out/hlz"
hlz.outpath <- paste(hlz.outfolder, "/hlz_", as.character(y), sep="")
writeRaster(hlz, filename=hlz.outpath, bandorder='BIL', overwrite=TRUE)
} # end year by year loop


# 
# 
# h_hex[h_hex$hexno==16,] # desert, temperate
# r <- hlz
# r[r!=16] <- 0
# r[r==16] <- 1
# plot(r)
# r
# 
# h_hex[h_hex$hexno==4,] # very dry forest, tropical
# r <- hlz
# r[r!=4] <- NA
# r[r==4] <- 1
# plot(r)
# r
# 
# h_hex[h_hex$hexno==9,] # warm temperate desert
# r <- hlz
# r[r!=9] <- NA
# r[r==9] <- 1
# plot(r)
# r
# 
# h_hex[h_hex$hexno==25,] # boreal wet forest
# r <- hlz
# r[r!=25] <- NA
# r[r==25] <- 1
# plot(r)
# r
# 
# 
