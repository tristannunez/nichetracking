# set working directory
dir <- "C:/git_niche_tracking/niche_tracking"
setwd(dir)
source("./R_scripts/_0_tools_libraries.R") # loads libraries, buffers, focal area, etc

h_hex <- read.csv("./Rdata_in_out/hlz_hexagonal_centroids.csv")
climate.paths<-read.csv("./Rdata_in_out/annual_climate_paths.csv")

# take all the hlz rasters for each year and combine into one brick
years<- 1925:1980
y<-1925

hlz.30mean.br <- brick("./Rdata_in_out/hlz_30yr/all_hlz_30yr_brick")
hlz.br <- hlz.30mean.br

# identify unique values in hlz
vals.pile <- c() 
for(layer in 1:nlayers(hlz.br)){
  vals.pile<-c(vals.pile, unique(values(hlz.br[[layer]])))
}
vals.unique <- unique(vals.pile)
vals.unique.s<-sort(vals.unique) 
length(sort(vals.unique) )

# just do desert for now, write to raster
desert.hexno <- h_hex$hexno[h_hex$type=="desert"]
vals.unique
new.vals <- c(1, rep(0, 7), 1, rep(0, 6), 1, rep(0,5),1, rep(0,8))
which(new.vals==1)
which(new.vals==1)==desert.hexno
length(new.vals)
recl<-matrix(c(1:30, new.vals),ncol=2)
recl

r <- hlz.in
r.rcl <- reclassify(hlz.br, rcl=recl)
#animate(r.rcl)
desert.series <- r.rcl
count.desert<-sum(desert.series)
plot(count.desert, main="desert")

names(desert.series)<- paste("Year: ", 1895:1980, sep="")
animate(desert.series)

writeRaster(desert.series, filename="./Rdata_in_out/single_hlz_series/desert_30yr_mean", bandorder='BIL', overwrite=TRUE)

wetforest.hexno <- h_hex$hexno[h_hex$type=="wet forest" | h_hex$type=="rain forest"]
vals<-1:30
new.vals<-as.numeric(vals %in% wetforest.hexno)
recl<-matrix(c(1:30, new.vals),ncol=2)
recl
r.rcl <- reclassify(hlz.br, rcl=recl)
wetforest.series <- r.rcl
#animate(wetforest.series)
count.wetforest<-sum(wetforest.series)
plot(count.wetforest,main="wetforest")
writeRaster(wetforest.series, filename="./Rdata_in_out/single_hlz_series/wetforest_30yr_mean", bandorder='BIL', overwrite=TRUE)

steppe.hexno <- h_hex$hexno[h_hex$type=="steppe"]
vals<-1:30
new.vals<-as.numeric(vals %in% steppe.hexno)
recl<-matrix(c(1:30, new.vals),ncol=2)
recl
r.rcl <- reclassify(hlz.br, rcl=recl)
steppe.series <- r.rcl
#animate(steppe.series)
count.steppe<-sum(steppe.series)
plot(count.steppe, main="steppe")
writeRaster(steppe.series, filename="./Rdata_in_out/single_hlz_series/steppe_30yr_mean", bandorder='BIL', overwrite=TRUE)

scrub.hexno <- h_hex$hexno[h_hex$type=="dry scrub" | h_hex$type=="desert scrub"]
vals<-1:30
new.vals<-as.numeric(vals %in% scrub.hexno)
recl<-matrix(c(1:30, new.vals),ncol=2)
recl
r.rcl <- reclassify(hlz.br, rcl=recl)
scrub.series <- r.rcl
#animate(scrub.series)
count.scrub<-sum(scrub.series)
plot(count.scrub, main="scrub")
writeRaster(scrub.series, filename="./Rdata_in_out/single_hlz_series/scrub_30yr_mean", bandorder='BIL', overwrite=TRUE)

