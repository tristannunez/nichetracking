# set working directory
dir <- "C:/git_niche_tracking/niche_tracking"
setwd(dir)
source("./R_scripts/_0_tools_libraries.R") # loads libraries, buffers, focal area, etc
#  define the tracking function

hlzs <- c("desert", "scrub", "wetforest")
pers.vals <- c(1, 1:10*2)
dispersal <- c(1:10, ((1:5)*4)+10)
combos<-as.data.frame(t(apply(expand.grid(hlzs, pers.vals, dispersal, stringsAsFactors = F), 1, c)))
names(combos)<-c("hlz.name", "persistence", "dispersal")

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

combos.ord<-combos[order(combos$hlz.name),] # orders by lifezone

years <- 1895:1980 # set years (should go into 0_params eventually)
dropyears <- 20 # drop first 20 years

lifezone<-unique(combos.ord$hlz.name)[1] # for tests

combos.new <- combos[combos$hlz.name=="dummy",] # sets up empty df to fill, bad coding ;)

for(lifezone in (unique(combos.ord$hlz.name))){
  print(lifezone)
  focal.hlz.name <- lifezone
  combos.foc <-combos.ord[combos.ord$hlz.name==lifezone,]
  # then use this to read in the series...
  # read in single hlz series
  hlz.series <- brick(paste("./Rdata_in_out/single_hlz_series/",lifezone, sep=""))


# calculate area available by year  
pix.avail.vec <- numeric()
for(i in dropyears:nlayers(hlz.series)){
  avail <-hlz.series[[i]]
  pix.avail <- sum(values(avail), na.rm=T)
  pix.avail.vec <- c(pix.avail.vec, pix.avail)
}
pix.avail.vec
#plot(1:86, pix.avail.vec, type='l')


# calculate ONP
#onp.series.list <- list()
combos.foc$onp.m <- rep(NA, nrow(combos.foc))
combos.foc$onp.sd <- rep(NA, nrow(combos.foc))
combos.foc$sonp.m<- rep(NA, nrow(combos.foc))
combos.foc$sonp.sd<- rep(NA, nrow(combos.foc))

iter<-1
for(iter in 1:nrow(combos.foc)){
  print("reading in")
  print(combos.foc[iter,])
  occ.ras.br.in <-   brick(combos.foc$occ_files[iter])
  #plot(occ.ras.br.in[[1]])
  
  TotalOccCells<-function(X){
    sum(values(X), na.rm = T)
  }  
  
   
  trunc.occ.ras.br.in<-occ.ras.br.in[[dropyears:nlayers(occ.ras.br.in)]]
  pix.occ.vec<- unlist(lapply(as.list(trunc.occ.ras.br.in), FUN=TotalOccCells))
  
  onp <- pix.occ.vec / pix.avail.vec
  onp.m <- mean(onp)
  onp.sd <- sd(onp)
  combos.foc$onp.m[iter]<- onp.m
  combos.foc$onp.sd[iter]<- onp.sd
  
#i<-20
# calculate area occupied and suitable
  pix.suit.occ.vec <- numeric()
  for(i in dropyears:nlayers(occ.ras.br.in)){
    suit.ras <- hlz.series[[i]]
#    plot(suit.ras)
    occ.ras <- occ.ras.br.in[[i]]
 #   plot(occ.ras)
    suit.occ <- occ.ras
    suit.occ[!is.na(suit.occ)]<-0
  #  plot(suit.occ)
    suit.occ[occ.ras == 1 & suit.ras == 1] <- 1
   # plot(suit.occ)
    pix.suit.occ <- sum(values(suit.occ), na.rm=T)
    pix.suit.occ.vec <- c(pix.suit.occ.vec, pix.suit.occ)
  }
  
  sonp <- pix.suit.occ.vec / pix.avail.vec
  #  plot(1:86, onp, type='l')
  # calculate stats
  sonp.m <- mean(sonp)
  print(sonp.m)
  sonp.sd <- sd(sonp)
  
  combos.foc$sonp.m[iter]<- sonp.m
  combos.foc$sonp.sd[iter]<- sonp.sd
  print(combos.foc[iter,])
}

combos.new <- rbind(combos.foc, combos.new)
} # end looping through lifezones

head(combos.new)

save(combos.new, file="./combos_all_sonp.RData")

#save(combos.desert, file="./combos_desert_nov3.RData")
combos.old <- combos

combos <- combos.new


combos.wetforest <- combos[combos$hlz.name=="wetforest",]
combos.desert <- combos[combos$hlz.name=="desert",]
combos.scrub <- combos[combos$hlz.name=="scrub",]


# compare the life zones
# desert
plot.pers.desert <- ggplot(combos.desert, aes(as.numeric(dispersal)*4, onp.m, group=as.numeric(persistence), colour=as.numeric(persistence))) + 
  geom_line()+
  scale_y_continuous(limits = c(0,max(combos$onp.m)))+
  labs(x="Dispersal Ability, km", y="Proportion Annual Suitable Area Occupied", colour="Persistence \n Ability (yr)")
#plot.pers
plot.disp.desert <- ggplot(combos.desert, aes(as.numeric(persistence), onp.m, group=as.numeric(dispersal), colour = as.numeric(dispersal))) + 
  geom_line()+
  scale_y_continuous(limits = c(0,max(combos$onp.m)))+
  labs(x="Persistence Ability, yrs", y="Proportion Annual Suitable Area Occupied", colour="Dispersal \n Ability (km)")
# combos wetforest
plot.pers.wetforest <- ggplot(combos.wetforest, aes(as.numeric(dispersal)*4, onp.m, group=as.numeric(persistence), colour=as.numeric(persistence))) + 
  geom_line()+
  scale_y_continuous(limits = c(0,max(combos$onp.m)))+
  labs(x="Dispersal Ability, km", y="Proportion Annual Suitable Area Occupied", colour="Persistence \n Ability (yr)")
plot.disp.wetforest <- ggplot(combos.wetforest, aes(as.numeric(persistence), onp.m, group=as.numeric(dispersal), colour = as.numeric(dispersal))) + 
  geom_line() +
  scale_y_continuous(limits = c(0,max(combos$onp.m)))+
  labs(x="Persistence Ability, yrs", y="Proportion Annual Suitable Area Occupied", colour="Dispersal \n Ability (km)")
# combos scrub
plot.pers.scrub <- ggplot(combos.scrub, aes(as.numeric(dispersal)*4, onp.m, group=as.numeric(persistence), colour=as.numeric(persistence))) + 
  geom_line()+
  scale_y_continuous(limits = c(0,max(combos$onp.m)))+
  labs(x="Dispersal Ability, km", y="Proportion Annual Suitable Area Occupied", colour="Persistence \n Ability (yr)")
plot.disp.scrub <- ggplot(combos.scrub, aes(as.numeric(persistence), onp.m, group=as.numeric(dispersal), colour = as.numeric(dispersal))) + 
  geom_line()+
  scale_y_continuous(limits = c(0,max(combos$onp.m)))+
  labs(x="Persistence Ability, yrs", y="Proportion Annual Suitable Area Occupied", colour="Dispersal \n Ability (km)")
# plot
multiplot(plot.disp.desert,  plot.pers.desert, 
          plot.disp.scrub, plot.pers.scrub,
          plot.disp.wetforest, plot.pers.wetforest,
          cols=3)

# next step:  do SONP
# compare the life zones
# desert
plot.sonp.pers.desert <- ggplot(combos.desert, aes(as.numeric(dispersal)*4, sonp.m, group=as.numeric(persistence), colour=as.numeric(persistence))) + 
  geom_line()+
  scale_y_continuous(limits = c(0,max(combos$sonp.m)))+
  labs(x="Dispersal Ability, km", y="Proportion Annual Suitable Area \n Suitable and Occupied", colour="Persistence \n Ability (yr)")
#plot.pers
plot.sonp.disp.desert <- ggplot(combos.desert, aes(as.numeric(persistence), sonp.m, group=as.numeric(dispersal), colour = as.numeric(dispersal))) + 
  geom_line()+
  scale_y_continuous(limits = c(0,max(combos$sonp.m)))+
  labs(x="Persistence Ability, yrs", y="Proportion Annual Suitable Area \n Suitable and Occupied", colour="Dispersal \n Ability (km)")
# combos wetforest
plot.sonp.pers.wetforest <- ggplot(combos.wetforest, aes(as.numeric(dispersal)*4, sonp.m, group=as.numeric(persistence), colour=as.numeric(persistence))) + 
  geom_line()+
  scale_y_continuous(limits = c(0,max(combos$sonp.m)))+
  labs(x="Dispersal Ability, km", y="Proportion Annual Suitable Area \n Suitable and Occupied", colour="Persistence \n Ability (yr)")
plot.sonp.disp.wetforest <- ggplot(combos.wetforest, aes(as.numeric(persistence), sonp.m, group=as.numeric(dispersal), colour = as.numeric(dispersal))) + 
  geom_line() +
  scale_y_continuous(limits = c(0,max(combos$sonp.m)))+
  labs(x="Persistence Ability, yrs", y="Proportion Annual Suitable Area \n Suitable and Occupied", colour="Dispersal \n Ability (km)")
# combos scrub
plot.sonp.pers.scrub <- ggplot(combos.scrub, aes(as.numeric(dispersal)*4, sonp.m, group=as.numeric(persistence), colour=as.numeric(persistence))) + 
  geom_line()+
  scale_y_continuous(limits = c(0,max(combos$sonp.m)))+
  labs(x="Dispersal Ability, km", y="Proportion Annual Suitable Area \n Suitable and Occupied", colour="Persistence \n Ability (yr)")
plot.sonp.disp.scrub <- ggplot(combos.scrub, aes(as.numeric(persistence), sonp.m, group=as.numeric(dispersal), colour = as.numeric(dispersal))) + 
  geom_line()+
  scale_y_continuous(limits = c(0,max(combos$sonp.m)))+
  labs(x="Persistence Ability, yrs", y="Proportion Annual Suitable Area \n Suitable and Occupied", colour="Dispersal \n Ability (km)")
# plot
multiplot(plot.sonp.disp.desert,  plot.sonp.pers.desert, 
          plot.sonp.disp.scrub, plot.sonp.pers.scrub,
          plot.sonp.disp.wetforest, plot.sonp.pers.wetforest,
          cols=3)


# end SONP










plot.pers <- ggplot(combos, aes(as.numeric(dispersal)*4, onp.m, 
                                group=as.numeric(persistence), colour=as.factor(persistence))) + 
  geom_line()
plot.pers


plot.disp <- ggplot(combos, aes(as.numeric(persistence), onp.m, 
                                group=as.numeric(dispersal), colour = as.factor(dispersal))) + 
  geom_line()
plot.disp

multiplot(plot.pers, plot.disp, cols=2)



# for persistence = 4 yrs, dispersal = 8 km / yr
test<-combos[combos$hlz.name=="desert" & combos$persistence == 4 & combos$dispersal == 2,]
animate.occ <- brick(test$occ_files)
animate(animate.occ)

# for persistence = 6 yrs, dispersal = 40 km / yr
test<-combos[combos$hlz.name=="desert" & combos$persistence == 8 & combos$dispersal == 10,]
animate.occ <- brick(test$occ_files)
animate(animate.occ)

# for persistence = 10 yrs, dispersal = 88 km / yr
test<-combos[combos$hlz.name=="desert" & combos$persistence == 10 & combos$dispersal == 22,]
animate.occ <- brick(test$occ_files)
animate(animate.occ)


