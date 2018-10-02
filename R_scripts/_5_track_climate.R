# set working directory
dir <- "C:/git_niche_tracking/niche_tracking"
setwd(dir)
source("./R_scripts/_0_tools_libraries.R") # loads libraries, buffers, focal area, etc
#  define the tracking function
# define the core path tracking function
Path_Suitability <- function(years=years, 
                             dispersal.matrix=dispersal.matrix, 
                             hlz.series=hlz.series,
                             persistence.foc=persistence.foc, 
                             seed.hlz = seed.hlz, 
                             study.ras = study.ras){
  for(yrnum in years){  
    print(yrnum)
    yr <- which(years==yrnum)
    # focal.suit is the locations where climate is suitable in current year
    focal.suit <- hlz.series[[yr]]
    #    plot(focal.suit)
    if(yr==1){
      # next step is to identify what areas are accessible
      # this requires looking at the raster of year of most recent occurrence with suitability, yr.mst.rec.suit.occ
      yr.mst.rec.suit.occ<-seed.hlz #set up empty one
      #     plot(yr.mst.rec.suit.occ, main="yr.mst.rec.suit.occ")
      # at outset, use the "seed" raster
      access.thresh.deduct <- as.numeric(persistence.foc)
      thresh.year.acc <- yrnum-access.thresh.deduct # suitability in this year or later provides access
      source.cells<-study.ras
      # source.cells identify areas that are occupied due to persistence
      source.cells[yr.mst.rec.suit.occ >= thresh.year.acc] <- 1
      #    plot(source.cells, main="source.cells")
      # identify source cell numbers
      cell.no<-which(values(source.cells)==1)
      # identify adjacent cells
      adj.cells<-adjacent(source.cells, cells=cell.no, directions=dispersal.matrix, pairs=F) 
      # adj.map now shows all the accessible cells (adjacent and source cells)
      acc.map<-study.ras
      acc.map[adj.cells]<-1
      acc.map[cell.no]<-1
      #   plot(acc.map, main="acc.map")
      # identify areas that are suitable and accessible in focal year
      pres.suit.acc <- study.ras
      pres.suit.acc[acc.map == 1 & focal.suit == 1] <- 1
      #  plot(pres.suit.acc, main="pres.suit.acc")
      
      # identify areas that are persisting
      thresh.year.pers <- yrnum-as.numeric(persistence.foc) # suitability in this year or later provides access
      persist.cells<-study.ras
      # source.cells identify areas that are occupied due to persistence
      persist.cells[yr.mst.rec.suit.occ > thresh.year.pers]<-1
      # plot(persist.cells, main="persist.cells")
      # identify union of source cells (which is map of persistence occupied areas) and pres.suit.acc
      all.pres.occ <- study.ras
      all.pres.occ[pres.suit.acc == 1 | persist.cells == 1]<-1
      names(all.pres.occ)<-yrnum
      #      plot(all.pres.occ, main="all.pres.occ")
      # all.pres.occ. need to be added to a brick
      occ.ras.br <- brick(all.pres.occ)
      # then need to assign year for yr.mst.rec.suit.occ
      yr.mst.rec.suit.occ[pres.suit.acc == 1] <- yrnum 
      #plot(occ.ras.br)
      #      plot(yr.mst.rec.suit.occ, main="yr.mst.rec.suit.occ")
      
    } else {
      access.thresh.deduct <- as.numeric(persistence.foc)
      thresh.year.acc <- yrnum-access.thresh.deduct # suitability in this year or later provides access
      source.cells<-study.ras
      # source.cells identify areas that are occupied due to persistence
      source.cells[yr.mst.rec.suit.occ >= thresh.year.acc]<-1
      #plot(source.cells, main="source.cells")
      # identify source cell numbers
      cell.no<-which(values(source.cells)==1)
      # identify adjacent cells
      adj.cells<-adjacent(source.cells, cells=cell.no, directions=dispersal.matrix, pairs=F) 
      # adj.map now shows all the accessible cells (adjacent and source cells)
      acc.map<-study.ras
      acc.map[adj.cells]<-1
      acc.map[cell.no]<-1
      # identify areas that are suitable and accessible in focal year
      pres.suit.acc <- study.ras
      pres.suit.acc[acc.map == 1 & focal.suit == 1] <- 1
      # identify areas that are persistisng
      thresh.year.pers <- yrnum-as.numeric(persistence.foc) # suitability in this year or later provides access
      persist.cells<-study.ras
      # source.cells identify areas that are occupied due to persistence
      persist.cells[yr.mst.rec.suit.occ > thresh.year.pers]<-1
      #plot(persist.cells, main="persist.cells")
      # identify union of source cells (which is map of persistence occupied areas) and pres.suit.acc
      all.pres.occ <- study.ras
      all.pres.occ[pres.suit.acc == 1 | persist.cells == 1] <- 1
      names(all.pres.occ)<-yrnum
      #      plot(all.pres.occ, main="all.pres.occ")
      # all.pres.occ. need to be added to a brick
      occ.ras.br <- addLayer(occ.ras.br, all.pres.occ)
      # then need to assign year for yr.mst.rec.suit.occ
      yr.mst.rec.suit.occ[pres.suit.acc == 1] <- yrnum 
      #plot(occ.ras.br)
      #      plot(yr.mst.rec.suit.occ, main="yr.mst.rec.suit.occ")
    } # end else statement 
  } # end for loop
  out.list <- list(occ.ras.br, yr.mst.rec.suit.occ)
} # end function 




# start off by generating combinations of dispersal distances and persistence values

# next steps 10/28:
#' generate and make a list of the different dispersal matrices
#' generate a csv of the dispersal, persistence, and lifezone combinations
#' include file names for the resulting occupancy maps
#' 
#' loop through all of them

hlzs <- c("desert", "scrub", "thorn", "steppe", "dry forest", "moist forest", "wetforest")
hlzs <- c("desert", "scrub", "wetforest")

pers.vals <- c(1, 1:10*2)
#pers.vals <- c(1,10,20)
class(pers.vals)
dispersal <- c(1:10, ((1:5)*2)+10)
#dispersal <- c(1,10,20)

combos<-as.data.frame(t(apply(expand.grid(hlzs, pers.vals, dispersal, stringsAsFactors = F), 1, c)))
names(combos)<-c("hlz.name", "persistence", "dispersal")
combos
head(combos)
nrow(combos)
str(combos)

filenames<-c()
for (i in 1:nrow(combos)){
  specs<-combos[i,]
  nameoffile <- paste("./Rdata_in_out/occ_ras/occ_ras_", 
                      specs[1,1], specs[1,2], specs[1,3], sep="_")
  nof_spaces<-gsub(" ", "_", nameoffile)  
  filenames[i]<-nof_spaces
}
combos$occ_files<- filenames

filenames<-c()
for (i in 1:nrow(combos)){
  specs<-combos[i,]
  nameoffile <- paste("./Rdata_in_out/occ_ras/mst_rec_yr_", 
                      specs[1,1], specs[1,2], specs[1,3], sep="_")
  nof_spaces<-gsub(" ", "_", nameoffile)  
  filenames[i]<-nof_spaces
}
combos$mstrecyr_files<- filenames

combos$persistence <- as.numeric(as.character(combos$persistence))
combos$dispersal <- as.numeric(as.character(combos$dispersal))

head(combos)
combos.ord<-combos[order(combos$hlz.name),]
str(combos.ord)

# loop through combos.ord here...
#lifezone <- "scrub"
lifezone <- "wetforest"

for(lifezone in (unique(combos.ord$hlz.name))){
  print(lifezone)
  focal.hlz.name <- lifezone
  combos.foc <-combos.ord[combos.ord$hlz.name==lifezone,]
  # then use this to read in the series...
  # read in single hlz series
  hlz.series <- brick(paste("./Rdata_in_out/single_hlz_series/",lifezone, sep=""))
  years <- 1895:1980
  # where niche tracking starts ... 

# also set up study area
# ok, let's walk one hlz through time. 
# set up a dummy raster for study area
study.ras <- hlz.series[[1]]
study.ras[!is.na(study.ras)]<-0
plot(study.ras)

# set up seed raster
seed.hlz <- sum(hlz.series)
seed.hlz[seed.hlz<10]<-0
seed.hlz[seed.hlz>0]<-1
seed.hlz[seed.hlz!=1]<-NA
seed.hlz[seed.hlz==1]<-min(years)-1 # this assigns suitable occurrence year as the year prior to the time series
plot(seed.hlz)

#source("./R_scripts/_0_function_niche_tracking.R") # niche_tracking_function
#iter<-1
# work through combos.foc simulating movements through time series...
for(iter in 1:nrow(combos.foc)){
  print(combos.foc[iter,])
  print(paste("Iteration ", iter, " of ", nrow(combos.foc)))
  persist.foc <- as.numeric(combos.foc$persistence[iter])
  print("Persistence value:")
  print(persist.foc)
  dispersal.rad <- as.numeric(combos.foc$dispersal[iter])
  print("Dispersal value in cells:")
  print(dispersal.rad)
  disp.mat <- MakeMovWinMat(dispersal.rad)

  #plot(seed.hlz)
  nlayers(hlz.series)

  path.suit.list <- Path_Suitability(years=years, 
                   dispersal.matrix=disp.mat, 
                   hlz.series=hlz.series,
                   persistence.foc=persist.foc, 
                   seed.hlz = seed.hlz, 
                   study.ras = study.ras)

#  out.list <- list(occ.ras.br, yr.mst.rec.suit.occ, onp, onp.m, onp.sd)
  
  occ.ras.br <- path.suit.list[[1]]
  writeRaster(occ.ras.br, filename=combos.foc$occ_files[iter], bandorder='BIL', overwrite=TRUE)
    #  animate(occ.ras.br)
  yr.mst.rec.suit.occ <- path.suit.list[[2]]
  plot(yr.mst.rec.suit.occ, main=combos.foc$occ_files[iter])
#  writeRaster(yr.mst.rec.suit.occ, filename=combos.foc$mstrecyr_files[iter], bandorder='BIL', overwrite=TRUE)
} # end occurrence raster writing

} # end looping through lifezones ...


#ok to just run everything above here as-is ... 






# calculate ONP
onp.series.list <- list()
combos$onp.m <- rep(NA, nrow(combos))
combos$onp.sd <- rep(NA, nrow(combos))

for(iter in 1:nrow(combos)){
  print("reading in")
  print(combos[iter,])
  occ.ras.br.in <-   brick(combos$occ_files[iter])
  plot(occ.ras.br.in[[1]])

# calculate area available by year  
pix.avail.vec <- numeric()
for(i in 1:nlayers(hlz.series)){
  avail <-hlz.series[[i]]
  pix.avail <- sum(values(avail), na.rm=T)
  pix.avail.vec <- c(pix.avail.vec, pix.avail)
}
pix.avail.vec
#plot(1:86, pix.avail.vec, type='l')

# calculate area occupied by year
pix.occ.vec <- numeric()
for(i in 1:nlayers(occ.ras.br.in)){
  occ <-occ.ras.br.in[[i]]
  pix.occ <- sum(values(occ), na.rm=T)
  pix.occ.vec <- c(pix.occ.vec, pix.occ)
}

pix.occ.vec

#plot(1:86, pix.occ.vec, type='l')
# calculate ONP
onp <- pix.occ.vec / pix.avail.vec
#  plot(1:86, onp, type='l')
# calculate stats
onp.m <- mean(onp)
onp.sd <- sd(onp)

combos$onp.m[iter]<- onp.m
combos$onp.sd[iter]<- onp.sd

onp.series.list[[iter]] <- onp
}



#save(onp.series.list, file=paste("./Rdata_in_out/onp.series.list.temp2.", lifezone, ".RData", sep=""))
save(combos, file="./Rdata_in_out/combos.scrub.wetfor.desert.combined.RData")

combos.arch <- combos

combos.wetforest <- combos[combos$hlz.name=="wetforest",]
combos.desert <- combos[combos$hlz.name=="desert",]
combos.scrub <- combos[combos$hlz.name=="scrub",]

unique(combos.wetforest$persistence)

names(combos)
combos[,c(2,3,6,7)]



# combos desert
plot.generated <- ggplot(combos, aes(as.numeric(dispersal)*4, as.numeric(persistence))) + 
  geom_point(aes(colour = onp.m, size = onp.sd))+
  scale_radius(range= c(5,18))+ 
  scale_color_gradient(low = "purple", high = "green")+
  labs(x="Dispersal (km)", y="Persistence (yr)", colour="Occupied \n Niche \n Proportion",
       size ="SD of \n Occupied \n Niche \n Proportion")
plot.generated

plot.pers <- ggplot(combos, aes(as.numeric(dispersal)*4, onp.m, group=as.numeric(persistence), colour=as.numeric(persistence))) + 
  geom_line()
plot.pers

plot.disp <- ggplot(combos, aes(as.numeric(persistence), onp.m, group=as.numeric(dispersal), colour = as.numeric(dispersal))) + 
  geom_line()
plot.disp

multiplot(plot.pers, plot.disp, plot.generated, cols=1)




# compare
plot.pers.desert <- ggplot(combos.desert, aes(as.numeric(dispersal)*4, onp.m, group=as.numeric(persistence), colour=as.numeric(persistence))) + 
  geom_line()+
  scale_y_continuous(limits = c(0,max(combos$onp.m)))+
  labs(x="Dispersal Ability, km", y="Proportion Annual Suitable Area Occupied", colour="Persistence \n Ability (yr)")
#plot.pers
plot.disp.desert <- ggplot(combos.desert, aes(as.numeric(persistence), onp.m, group=as.numeric(dispersal), colour = as.numeric(dispersal))) + 
  geom_line()+
  scale_y_continuous(limits = c(0,max(combos$onp.m)))+
labs(x="Persistence Ability, yrs", y="Proportion Annual Suitable Area Occupied", colour="Dispersal \n Ability (km)")
#p
# combos wetforest
plot.pers.wetforest <- ggplot(combos.wetforest, aes(as.numeric(dispersal)*4, onp.m, group=as.numeric(persistence), colour=as.numeric(persistence))) + 
  geom_line()+
  scale_y_continuous(limits = c(0,max(combos$onp.m)))+
labs(x="Dispersal Ability, km", y="Proportion Annual Suitable Area Occupied", colour="Persistence \n Ability (yr)")

plot.disp.wetforest <- ggplot(combos.wetforest, aes(as.numeric(persistence), onp.m, group=as.numeric(dispersal), colour = as.numeric(dispersal))) + 
  geom_line() +
  scale_y_continuous(limits = c(0,max(combos$onp.m)))+
  labs(x="Persistence Ability, yrs", y="Proportion Annual Suitable Area Occupied", colour="Dispersal \n Ability (km)")
#p

# combos scrub
plot.pers.scrub <- ggplot(combos.scrub, aes(as.numeric(dispersal)*4, onp.m, group=as.numeric(persistence), colour=as.numeric(persistence))) + 
  geom_line()+
  scale_y_continuous(limits = c(0,max(combos$onp.m)))+
  labs(x="Dispersal Ability, km", y="Proportion Annual Suitable Area Occupied", colour="Persistence \n Ability (yr)")

plot.disp.scrub <- ggplot(combos.scrub, aes(as.numeric(persistence), onp.m, group=as.numeric(dispersal), colour = as.numeric(dispersal))) + 
  geom_line()+
  scale_y_continuous(limits = c(0,max(combos$onp.m)))+
  labs(x="Persistence Ability, yrs", y="Proportion Annual Suitable Area Occupied", colour="Dispersal \n Ability (km)")
#p



#multiplot(plot.disp.desert,  plot.pers.desert, plot.disp.wetforest, plot.pers.wetforest, cols=2)


multiplot(plot.disp.desert,  plot.pers.desert, 
          plot.disp.wetforest, plot.pers.wetforest,
          plot.disp.scrub, plot.pers.scrub,
          cols=3)


#[need to save onp.series.list or convert to df or something...]



# each niche tracking run for a set of parameters  

#[specify the parameters]

# each observation of ONP.m and ONP.sd in the dataframe is the result of one niche tracking run 
# using a set of parameters for persistence and dispersal

#[loop through niche tracking runs for persistence and dispersal]  

# [loop through dispersal.suite; for each disperal matrix loop through persistence.suite]




path.suit.list <- Path_Suitability(years=years, 
                                   dispersal.matrix=dispersal.matrix, 
                                   hlz.series=hlz.series,
                                   persistence.foc=persistence.foc, 
                                   seed.hlz = seed.hlz, 
                                   study.ras = study.ras)

pers.results.list[[p]] <- path.suit.list

p

persistence.suite <- 1:5

pers.results.list <- list()

for(p in persistence.suite){
  print(paste("persistence value" , p))
  path.suit.outlist <- Path_Suitability(years=years, 
                                        dispersal.matrix=dispersal.matrix, 
                                        hlz.series=hlz.series,
                                        persistence.foc=p, 
                                        seed.hlz = seed.hlz, 
                                        study.ras = study.ras)
  pers.results.list[[p]] <- path.suit.outlist
}


save(pers.results.list, file="./tempresults.RData")


# some thoughts: should make this modular because these take so long to calculate... write out rasters etc

#' 




# dataframe that goes into figure:
# just for one HLZ for now
# cols are: persistence, dispersal, ONP.m, ONP.sd

#[fill in dataframe]  

# ok, the outcome is going to be a heat map showing persistence, dispersal, and average and sd of ONP (occupied niche percentage)   
# to make the heat map, need a dataframe
# figure out the ggplot details later

[make the ggplot figure]  








# this bit of code helps understand how the adjacent function works
r <- raster(nrows=20, ncols=20)
plot(r)
r[c(5,55,250)] <-1
plot(r)
cell.no<-which(values(r)==1)
b<-adjacent(r, cells=cell.no, directions=dispersal_matrix, pairs=F) 
b
r[b]<-3
plot(r)
a<-adjacent(r, cells=cell.no, directions=8, pairs=F) 
a
r[a]<-2
plot(r)
# end test adjacent


https://r-spatial.github.io/mapview/reference/cubeView.html


plot(series[[1]])
install.packages("mapview")
library(mapview)
cubeView(series, at, col.regions = mapviewGetOption("raster.palette"),
         na.color = mapviewGetOption("na.color"), legend = TRUE)
?cubeView

kili_data <- system.file("extdata", "kiliNDVI.tif", package = "mapview")
kiliNDVI <- stack(kili_data)

cubeView(kiliNDVI)

library(RColorBrewer)
clr <- colorRampPalette(brewer.pal(9, "BrBG"))
cubeView(kiliNDVI, at = seq(-0.15, 0.95, 0.1), col.regions = clr)


