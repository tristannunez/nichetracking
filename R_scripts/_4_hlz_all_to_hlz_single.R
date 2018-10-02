# set working directory
dir <- "F:/git_niche_tracking/niche_tracking"
setwd(dir)
source("./R_scripts/_0_tools_libraries.R") # loads libraries, buffers, focal area, etc

h_hex <- read.csv("./Rdata_in_out/hlz_hexagonal_centroids.csv")
climate.paths<-read.csv("./Rdata_in_out/annual_climate_paths.csv")

# take all the hlz rasters for each year and combine into one brick
years<- 1895:1980

for (y in years){
hlz.outfolder <-"./Rdata_in_out/hlz"
hlz.outpath <- paste(hlz.outfolder, "/hlz_", as.character(y), sep="")
hlz.in<-raster(hlz.outpath)

if(which(years==y)==1){
  hlz.br <- brick(hlz.in)
} else {
  hlz.br <- addLayer(hlz.br, hlz.in)
  }
}

names(hlz.br)<- paste("Year: ", years, sep="")


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

writeRaster(desert.series, filename="./Rdata_in_out/single_hlz_series/desert", bandorder='BIL', overwrite=TRUE)

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
writeRaster(wetforest.series, filename="./Rdata_in_out/single_hlz_series/wetforest", bandorder='BIL', overwrite=TRUE)

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
writeRaster(steppe.series, filename="./Rdata_in_out/single_hlz_series/steppe", bandorder='BIL', overwrite=TRUE)

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
writeRaster(scrub.series, filename="./Rdata_in_out/single_hlz_series/scrub", bandorder='BIL', overwrite=TRUE)



# update this later - make it so writes all of these out their separate files...

dryforest.hexno <- h_hex$hexno[h_hex$type=="dry forest" | h_hex$type=="very dry forest"]
vals<-1:30
new.vals<-as.numeric(vals %in% dryforest.hexno)
recl<-matrix(c(1:30, new.vals),ncol=2)
recl
r.rcl <- reclassify(hlz.br, rcl=recl)
dryforest.series <- r.rcl
#animate(dryforest.series)
count.dryforest<-sum(dryforest.series)
plot(count.dryforest, main="dryforest")

unique(h_hex$type)


moistforest.hexno <- h_hex$hexno[h_hex$type=="moist forest" | h_hex$type=="moist forest"]
vals<-1:30
new.vals<-as.numeric(vals %in% moistforest.hexno)
recl<-matrix(c(1:30, new.vals),ncol=2)
recl
r.rcl <- reclassify(hlz.br, rcl=recl)
moistforest.series <- r.rcl
#animate(moistforest.series)
count.moistforest<-sum(moistforest.series)
plot(count.moistforest, main="moistforest")

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


thorn.hexno <- h_hex$hexno[h_hex$type=="thorn steppe or woodland" | h_hex$type=="thorn woodland"]
vals<-1:30
new.vals<-as.numeric(vals %in% thorn.hexno)
recl<-matrix(c(1:30, new.vals),ncol=2)
recl
r.rcl <- reclassify(hlz.br, rcl=recl)
thorn.series <- r.rcl
#animate(thorn.series)
count.thorn<-sum(thorn.series)
plot(count.thorn, main="thorn")

all.count <- brick(count.desert, count.scrub, count.thorn, count.steppe, count.dryforest, count.moistforest, count.wetforest)
names(all.count) <- c("desert", "scrub", "thorn", "steppe", "dry forest", "moist forest", "wet forest")
plot(all.count)

all.count.nz<-all.count
all.count.nz[all.count==0]<-NA
plot(all.count.nz)

other.count <- 80-all.count.nz
plot(other.count)

other.count.sum <- sum(other.count, na.rm=T)/7
plot(other.count.sum)


#animate(desert.series)
#animate(steppe.series)

# how many hlz's occurred in each pixel?

UniqueHLZ <- function(x){
length(unique(x))
}

unique.ras <- calc(hlz.br, fun=UniqueHLZ)
plot(unique.ras)
hist(unique.ras)


# ok, let's walk one hlz through time. 
# set up a dummy raster for study area

study.ras <- hlz.br[[1]]
study.ras[!is.na(study.ras)]<-0
plot(study.ras)

# use the desert type
plot(count.desert)
count.desert.30 <- count.desert
count.desert.30[count.desert<30] <- 0 
plot(count.desert.30)
# there are some clearly isolated features whose occupancy should be dispersal limited

seed.desert <- count.desert
seed.desert[count.desert<10]<-0
seed.desert[seed.desert>0]<-1
plot(seed.desert) # these are all the places where there were "desert" conditions for at least 10 years 


years <- 1:20
dispersal_matrix <- mat_3_cell_rad
dispersal_matrix<- mat_2_cell_rad_q


for(yr in years){  
focal.suit <- desert.series[[yr]]
plot(focal.suit)
#test <-distance(focal.suit, doEdge=T)
#plot(test)
# calculate area accessible from the seed
if(which(years==yr)==1){
seed.desert[seed.desert!=1]<-NA
access.focal.suit<- focal(seed.desert, w=dispersal_matrix, fun=max, na.rm=T)
occ.ras <- study.ras
occ.ras[access.focal.suit==1 & focal.suit==1] <- 1 
# need to insert previous year in here... from seed.desert
plot(occ.ras, main=yr)
occ.ras.br <- brick(occ.ras)
} else {
  print(yr)
  prev.yr<- occ.ras.br[[yr-1]]
  plot(prev.yr)
  prev.yr[prev.yr!=1]<-NA
  access.focal.suit<- focal(prev.yr, w=dispersal_matrix, fun=max, na.rm=T) # set non-1 vals to NA to speed up
  occ.ras <- study.ras
  occ.ras[access.focal.suit==1 & focal.suit==1] <- 1 
  plot(occ.ras)
  occ.ras[prev.yr==1]<-2 # this adds in the previous year's occurrences
  plot(occ.ras, main=yr)
  occ.ras.br <- addLayer(occ.ras.br, occ.ras)
}
}



#occ.ras.br.nopers <- occ.ras.br
#occ.ras.br.1yrpers <- occ.ras.br
#occ.ras.br.1yrpers.3cell <- occ.ras.br
occ.ras.br.1yrpers.2queens <- occ.ras.br

animate(occ.ras.br.1yrpers.2queens)
yrs.2queens<-sum(occ.ras.br.1yrpers.2queens)
plot(yrs.2queens)

yrs.3.cell<-sum(occ.ras.br.1yrpers.3cell)
plot(yrs.3.cell)

occ.yrs.1yrpers <-sum(occ.ras.br.1yrpers)
plot(occ.yrs.1yrpers)
occ.yrs.nopers <-sum(occ.ras.br.nopers)


plot(brick(count.desert, occ.yrs.1yrpers, yrs.3.cell))


compare.desert <- brick(count.desert, occ.yrs.nopers, occ.yrs.1yrpers)
names(compare.desert) <- c("years suitable", "dispersal limited occurrence no pers", "dispersal limited occ 1 yr pers")
plot(compare.desert)


years<- 1895:1915
yrnum <- 1896
hlz.series <- desert.series
yr<- 2
Path_Suitability <- function(years, dispersal_matrix, hlz.series){
  
  for(yrnum in years){  
    yr <- which(years==yrnum)
    focal.suit <- hlz.series[[yr]]
    plot(focal.suit)
    # calculate area accessible from the seed
    if(yr==1){
      count.hlz <- sum(hlz.series)
      seed.hlz <- count.hlz
      seed.hlz[count.hlz<10]<-0
      seed.hlz[seed.hlz>0]<-1
      seed.hlz[seed.hlz!=1]<-NA
      access.focal.suit<- focal(seed.hlz, w=dispersal_matrix, fun=max, na.rm=T)
      plot(access.focal.suit)
      occ.ras <- study.ras
      occ.ras[seed.hlz==1]<-1 # previous year (in this case the seeded area)     
      plot(occ.ras, main=yr)
      occ.ras[access.focal.suit==1 & focal.suit==1] <- yrnum 
      occ.ras.br <- brick(occ.ras)
      plot(occ.ras.br)
    } else {
      print(yr)
      prev.yr<- occ.ras.br[[yr-1]]
      prev.yr.only <- prev.yr
      plot(prev.yr.only)
      prev.yr.only[prev.yr.only!=(yrnum-1)]<-NA
      prev.yr.only[prev.yr.only==(yrnum-1)]<-1
      plot(prev.yr.only)
      access.focal.suit<- focal(prev.yr.only, w=dispersal_matrix, fun=max, na.rm=T) # set non-1 vals to NA to speed up
      # stopping here 10/6, not sure what is going on... go back to flowchart!
      occ.ras <- study.ras
      occ.ras[access.focal.suit==1 & focal.suit==1] <- yrnum
      plot(occ.ras)
      occ.ras[prev.yr==1]<-2 # this adds in the previous year's occurrences
      plot(occ.ras, main=yr)
      occ.ras.br <- addLayer(occ.ras.br, occ.ras)
    }
  }
  


  
  
  install.packages("animation")
  library(animation)
  saveGIF(animate(hlz.br), movie.name="./figures/hlz.gif" )
  
  
  subset.hlz <- hlz.br[[1:20]]
  plot(subset.hlz[[1]])
  saveGIF(animate(subset.hlz, col=mycol), movie.name="./figures/hlz.gif" )
  
  ?saveGIF

  
  
