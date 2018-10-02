# set working directory
dir <- "F:/git_niche_tracking/niche_tracking"
setwd(dir)
source("./R_scripts/_0_tools_libraries.R") # loads libraries, buffers, focal area, etc

# generate file paths for each year/month allowing us to read in PRISM files
years <- as.character(1895:1980) # specify this in _0_tools_libraries.R eventually
months.sing <- 1:9
months.sing.doub <- paste(as.character(0),as.character(months.sing), sep="")
months.doub <- as.character(10:12)
months <- c(months.sing.doub, months.doub)
combos<-as.data.frame(t(apply(expand.grid(months, years, stringsAsFactors = F), 1, c)))
names(combos)<-c("month", "year")
head(combos)

precip.folder <- "./downloaded_data_in/PRISM/PRISM_ppt_stable_4kmM2_189501_198012_bil/"
dir(precip.folder)
str1.ppt <- "PRISM_ppt_stable_4kmM2_"
str2.ppt <- "_bil.bil"
combos$ppt.path <- paste(precip.folder, str1.ppt, combos$year, combos$month, str2.ppt, sep="")

tmean.folder <- "./downloaded_data_in/PRISM/PRISM_tmean_stable_4kmM2_189501_198012_bil/"
dir(tmean.folder)
str1.tmean <- "PRISM_tmean_stable_4kmM2_"
str2.tmean <- "_bil.bil"
combos$tmean.path <- paste(tmean.folder, str1.tmean, combos$year, combos$month, str2.tmean, sep="")
head(combos)

# generate file paths for each output year of atap and mabt
outfiles.df <- data.frame(year=years)
anppt.outfolder <-"./Rdata_in_out/PRISM_atap"
mabt.outfolder <-"./Rdata_in_out/PRISM_mabt"

anppt.outpaths <- c()
mabt.outpaths <- c()
for (y in years){
anppt.outpath <- paste(anppt.outfolder, "/PRISM_atap_", as.character(y), sep="")
anppt.outpaths <- c(anppt.outpaths, anppt.outpath)
mabt.outpath <- paste(mabt.outfolder, "/PRISM_mabt_", as.character(y), sep="")
mabt.outpaths <- c(mabt.outpaths, mabt.outpath)
}

outfiles.df$mabt.paths <- mabt.outpaths
outfiles.df$atap.paths <- anppt.outpaths
head(outfiles.df)

write.csv(outfiles.df, "./Rdata_in_out/annual_climate_paths.csv", row.names = F)

# calculate total annual precipitation from monthly rasters
for(y in unique(combos$year)){
  print(y)
  year.paths <- combos$ppt.path[combos$year==y]
  for(m in 1:length(year.paths)){
    if(m == 1){
      year.brick <- brick(year.paths[m])
    } else {
      year.brick.new <- brick(year.paths[m])
      year.brick <- addLayer(year.brick, year.brick.new)
    }
  }
  year.sum <- sum(year.brick)
  anppt.outfolder <-"./Rdata_in_out/PRISM_atap"
  anppt.outpath <- paste(anppt.outfolder, "/PRISM_atap_", as.character(y), sep="")
  writeRaster(year.sum, filename=anppt.outpath, bandorder='BIL', overwrite=TRUE)
} 

# next: calculate biotemperature!
# calculate total annual precipitation from monthly rasters
#y<- as.character(1895)

for(y in unique(combos$year)){
  print(y)
  year.paths <- combos$tmean.path[combos$year==y]
  for(m in 1:length(year.paths)){
    if(m == 1){
      year.brick <- brick(year.paths[m])
    } else {
      year.brick.new <- brick(year.paths[m])
      year.brick <- addLayer(year.brick, year.brick.new)
    }
  }
  tmean.0.30 <- year.brick
  tmean.0.30.vals <- values(year.brick)
  tmean.0.30.vals[tmean.0.30.vals<0|tmean.0.30.vals>30]<-0
  values(tmean.0.30)<-tmean.0.30.vals
  #plot(tmean.0.30)
  mabt<-sum(tmean.0.30)/12
  #plot(mabt)
  mabt.outfolder <-"./Rdata_in_out/PRISM_mabt"
  mabt.outpath <- paste(mabt.outfolder, "/PRISM_mabt_", as.character(y), sep="")
  writeRaster(mabt, filename=mabt.outpath, bandorder='BIL', overwrite=TRUE)
} 




