# set working directory
dir <- "C:/git_niche_tracking/niche_tracking"
setwd(dir)
source("./R_scripts/_0_tools_libraries.R") # loads libraries, buffers, focal area, etc
#  define the tracking function




##########################################################################################

# plot life zones

# get state boundary data
require(maps)
states<-map_data("state")
polys <- states
lifezone <- "desert"
lifezone.list <- list(c("desert", "Desert"), c("scrub", "Scrub"), c("wetforest", "Wet Forest"))

MapList <- function(X){
  lifezone <- X[1]
  lifezonename <- X[2]
  series<-brick(paste("./Rdata_in_out/single_hlz_series/",lifezone, sep=""), bandorder='BIL', overwrite=TRUE)
  sum.series <- sum(series)
  names(sum.series) <- lifezone
  #plot(sum.series)
  sum.series.points <- rasterToPoints(sum.series)
  sum.series.df<-data.frame(sum.series.points)
  head(sum.series.df)
  # map with polygons from iucn
  series.map.polys<-NULL
  series.map.polys <- ggplot()+
    geom_tile(data=sum.series.df, aes(x, y, fill=sum.series.df[,3])) +
    scale_fill_gradientn(colours = rev(terrain.colors(50)),
                         name="Number Suitable Years ") + #, limits=set.scale.limits)+
    coord_fixed(1)+
    #theme_nothing() +
    theme( line = element_blank(), 
           axis.text = element_blank(), 
           axis.title = element_blank())+
    geom_polygon(data = polys, aes(x=long, y = lat, group = group), fill = NA, colour = "black") + 
    theme(legend.key.size = unit(1, "cm")) +
    theme(legend.position="bottom") +
    ggtitle(lifezonename)
  plot(series.map.polys)
  series.map.polys
}

t<-lapply(lifezone.list, FUN=MapList)

multiplot(t[[1]], t[[2]], t[[3]], cols=3)


# plot suitability
png(filename = "./figures/suit_Nov9.png",
    width = 15, height = 6, units = "in", pointsize = .5,
    bg = "transparent",  res = 300)
multiplot(t[[1]], t[[2]], t[[3]], cols=3)

dev.off()

# plot suitability
png(filename = "./figures/suit_Nov9_large.png",
    width = 6, height = 12, units = "in", pointsize = .5,
    bg = "transparent",  res = 300)
multiplot(t[[1]], t[[2]], t[[3]], cols=1)

dev.off()


#########################################################################################3



# loads combos.new
load("./combos_all_sonp.RData")
unique(combos.new$dispersal)
combos <- combos.new[combos.new$dispersal<14,]
head(combos, 50)

combos.wetforest <- combos[combos$hlz.name=="wetforest",]
combos.desert <- combos[combos$hlz.name=="desert",]
combos.scrub <- combos[combos$hlz.name=="scrub",]



xlabel.nichetracking <- "Dispersal Ability, km/year"
ylab.ntp <- "Niche Tracking Potential"
ylab.occ.pot <- "Occurrence Potential"

#SONP
plot.sonp.pers.desert <- ggplot(combos.desert, aes(as.numeric(dispersal)*4, sonp.m, group=as.factor(persistence), colour=as.factor(persistence))) + 
  geom_line()+
  ggtitle("Desert")+
  scale_y_continuous(limits = c(0,max(combos$sonp.m)))+
  labs(x=xlabel.nichetracking, y=ylab.ntp, colour="Persistence \n Ability (yr)") 
#plot.pers
plot.sonp.pers.wetforest <- ggplot(combos.wetforest, aes(as.numeric(dispersal)*4, sonp.m, group=as.factor(persistence), colour=as.factor(persistence))) + 
  geom_line()+
  ggtitle("Wet Forest")+
  scale_y_continuous(limits = c(0,max(combos$sonp.m)))+
  labs(x=xlabel.nichetracking, y=ylab.ntp, colour="Persistence \n Ability (yr)")
plot.sonp.pers.scrub <- ggplot(combos.scrub, aes(as.numeric(dispersal)*4, sonp.m, group=as.factor(persistence), colour=as.factor(persistence))) + 
  geom_line()+
  ggtitle("Scrub")+
  scale_y_continuous(limits = c(0,max(combos$sonp.m)))+
  labs(x=xlabel.nichetracking, y=ylab.ntp, colour="Persistence \n Ability (yr)")

# plot SONP
png(filename = "./figures/sonp_Nov14.png",
    width = 15, height = 4, units = "in", pointsize = .5,
    bg = "transparent",  res = 300)
multiplot(plot.sonp.pers.desert, 
           plot.sonp.pers.scrub,
           plot.sonp.pers.wetforest,
          cols=3)
dev.off()
# end SONP

################################################################################################
# plot ONP
# compare the life zones
# desert
plot.pers.desert <- ggplot(combos.desert, aes(as.numeric(dispersal)*4, onp.m, group=as.factor(persistence), colour=as.factor(persistence))) + 
  geom_line()+
  ggtitle("Desert")+
  scale_y_continuous(limits = c(0,max(combos$onp.m)))+
  labs(x=xlabel.nichetracking, y=ylab.occ.pot, colour="Persistence \n Ability (yr)")
# combos wetforest
plot.pers.wetforest <- ggplot(combos.wetforest, aes(as.numeric(dispersal)*4, onp.m, group=as.factor(persistence), colour=as.factor(persistence))) + 
  geom_line()+
  ggtitle("Wet Forest")+
  scale_y_continuous(limits = c(0,max(combos$onp.m)))+
  labs(x=xlabel.nichetracking, y=ylab.occ.pot, colour="Persistence \n Ability (yr)")
# combos scrub
plot.pers.scrub <- ggplot(combos.scrub, aes(as.numeric(dispersal)*4, onp.m, group=as.factor(persistence), colour=as.factor(persistence))) + 
  geom_line()+
  ggtitle("Scrub")+
  scale_y_continuous(limits = c(0,max(combos$onp.m)))+
  labs(x=xlabel.nichetracking, y=ylab.occ.pot, colour="Persistence \n Ability (yr)")
# plot
multiplot(plot.pers.desert, 
          plot.pers.scrub,
          plot.pers.wetforest,
          cols=3)

# plot ONP
png(filename = "./figures/onp_Nov14.png",
    width = 15, height = 4, units = "in", pointsize = .5,
    bg = "transparent",  res = 300)
multiplot(plot.pers.desert, 
          plot.pers.scrub,
          plot.pers.wetforest,
          cols=3)
dev.off()
# end ONP


# plot eval of 30 yr means 

load("combos_30yracc_Eval.Rdata")
# loads combos.30yreval
unique(combos.30yreval$dispersal)
combos <- combos.30yreval[combos.30yreval$dispersal<14,]
head(combos, 50)

combos.wetforest <- combos[combos$hlz.name=="wetforest",]
combos.desert <- combos[combos$hlz.name=="desert",]
combos.scrub <- combos[combos$hlz.name=="scrub",]


#omission
plot.omission.pers.desert <- ggplot(combos.desert, aes(as.numeric(dispersal)*4, omission.m, group=as.factor(persistence), colour=as.factor(persistence))) + 
  geom_line()+
  ggtitle("Desert")+
  scale_y_continuous(limits = c(0,max(combos$omission.m)))+
  labs(x=xlabel.nichetracking, y="Omission", colour="Persistence \n Ability (yr)") 
#plot.pers
plot.omission.pers.wetforest <- ggplot(combos.wetforest, aes(as.numeric(dispersal)*4, omission.m, group=as.factor(persistence), colour=as.factor(persistence))) + 
  geom_line()+
  ggtitle("Wet Forest")+
  scale_y_continuous(limits = c(0,max(combos$omission.m)))+
  labs(x=xlabel.nichetracking, y="Omission", colour="Persistence \n Ability (yr)")
plot.omission.pers.scrub <- ggplot(combos.scrub, aes(as.numeric(dispersal)*4, omission.m, group=as.factor(persistence), colour=as.factor(persistence))) + 
  geom_line()+
  ggtitle("Scrub")+
  scale_y_continuous(limits = c(0,max(combos$omission.m)))+
  labs(x=xlabel.nichetracking, y="Omission", colour="Persistence \n Ability (yr)")

# plot omission
png(filename = "./figures/omission_Nov14.png",
    width = 15, height = 4, units = "in", pointsize = .5,
    bg = "transparent",  res = 300)
multiplot(plot.omission.pers.desert, 
          plot.omission.pers.scrub,
          plot.omission.pers.wetforest,
          cols=3)
dev.off()
# end omission

####################### commission

#commission
plot.commission.pers.desert <- ggplot(combos.desert, aes(as.numeric(dispersal)*4, commission.m, group=as.factor(persistence), colour=as.factor(persistence))) + 
  geom_line()+
  ggtitle("Desert")+
  scale_y_continuous(limits = c(0,max(combos$commission.m)))+
  labs(x=xlabel.nichetracking, y="Commission", colour="Persistence \n Ability (yr)") 
#plot.pers
plot.commission.pers.wetforest <- ggplot(combos.wetforest, aes(as.numeric(dispersal)*4, commission.m, group=as.factor(persistence), colour=as.factor(persistence))) + 
  geom_line()+
  ggtitle("Wet Forest")+
  scale_y_continuous(limits = c(0,max(combos$commission.m)))+
  labs(x=xlabel.nichetracking, y="Commission", colour="Persistence \n Ability (yr)")
plot.commission.pers.scrub <- ggplot(combos.scrub, aes(as.numeric(dispersal)*4, commission.m, group=as.factor(persistence), colour=as.factor(persistence))) + 
  geom_line()+
  ggtitle("Scrub")+
  scale_y_continuous(limits = c(0,max(combos$commission.m)))+
  labs(x=xlabel.nichetracking, y="Commission", colour="Persistence \n Ability (yr)")

# plot commission
png(filename = "./figures/commission_Nov14.png",
    width = 15, height = 4, units = "in", pointsize = .5,
    bg = "transparent",  res = 300)
multiplot(plot.commission.pers.desert, 
          plot.commission.pers.scrub,
          plot.commission.pers.wetforest,
          cols=3)
dev.off()
# end commission


# plot turnover by year

load("./Rdata_in_out/turnoverdf.Rdata")
turnover.df
aggregate(turnover~lifezone, data=fill.df, FUN=mean)

turnover.plot <- ggplot(turnover.df, aes(year.foc,turnover, group=as.factor(lifezone), colour=as.factor(lifezone))) + 
  geom_line()+
  ggtitle("Year-to-Year Turnover in Life Zone Locations")+
#  scale_y_continuous(limits = c(0,max(combos$commission.m)))+
  labs(x="Year", y="Turnover Rate", colour="Life Zone")

turnover.plot

# plot commission
png(filename = "./figures/turnover_Nov14.png",
    width = 15, height = 4, units = "in", pointsize = .5,
    bg = "transparent",  res = 300)
turnover.plot
dev.off()


