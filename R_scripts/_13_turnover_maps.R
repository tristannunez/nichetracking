# set working directory
dir <- "C:/git_niche_tracking/niche_tracking"
setwd(dir)
source("./R_scripts/_0_tools_libraries.R") # loads libraries, buffers, focal area, etc
#  define the tracking function



hlzs <- c("desert", "scrub", "wetforest")
lifezone <- "desert"
years <- 1895:1980
years.eval <- years[-1] # remove first

fill.df <- data.frame(lifezone=character(0), 
                      years.foc=numeric(0), 
                      turnover=numeric(0), 
                      agree.suit=numeric(0),
                      total.suit = numeric(0),
                      disagree.suit=numeric(0))


for(lifezone in hlzs){
  print(lifezone)
  hlz.series <- brick(paste("./Rdata_in_out/single_hlz_series/",lifezone ,sep=""))

    for(i in 2:nlayers(hlz.series)){
    print(i)
    year.foc <- years.eval[i-1]
    print(year.foc)
    t0 <- hlz.series[[i-1]]
    v.t0 <- values(t0)
    
    t1 <- hlz.series[[i]]
    v.t1 <- values(t1)

    agree.suit <- sum(v.t0 == 1 & v.t1 == 1, na.rm=T)
    total.suit <- sum(v.t0 == 1, na.rm=T) + sum(v.t1 == 1, na.rm=T) - agree.suit
    disagree.suit <- total.suit - agree.suit
    turnover <- disagree.suit/total.suit
  #fill the df
  to.df <- data.frame(lifezone, year.foc, turnover, agree.suit, total.suit, disagree.suit)
  fill.df<-rbind(fill.df, to.df)
   }# end within lifezone loop

  
} # end looping across all lifezones

turnover.df <- fill.df

save(turnover.df, file="./Rdata_in_out/turnoverdf.Rdata")


aggregate(turnover~lifezone, data=fill.df, FUN=mean)

# to calculate turnover stats reported in the paper
load("./Rdata_in_out/turnoverdf.Rdata")
aggregate(turnover~lifezone, data=turnover.df, FUN=mean)
aggregate(turnover~lifezone, data=turnover.df, FUN=sd)

aggregate(turnover~lifezone, data=turnover.df, FUN=mean)
