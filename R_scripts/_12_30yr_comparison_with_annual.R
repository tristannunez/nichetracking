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

#function used in calculating area classified as suitable
TotalOccCells<-function(X){
  sum(values(X), na.rm = T)
}  

combos$omission.m <- NA
combos$omission.sd <- NA
combos$commission.m <- NA
combos$commission.sd <- NA

for(lifezone in hlzs){
print(lifezone)
hlzs.30.yr.series <- brick(paste("./Rdata_in_out/single_hlz_series/",lifezone ,"_30yr_mean", sep=""))
#hlz.annual.series <- brick(paste("./Rdata_in_out/single_hlz_series/",lifezone, sep=""))
#hlz.annual.series.sub <- hlz.annual.series[[31:nlayers(hlz.annual.series)]]

combos.sub <- subset(combos, hlz.name == lifezone)

# set up empty brick for filling
dummy <- brick(hlzs.30.yr.series)
dummy[!is.na(dummy)]<-0

foc.row <-1
for(foc.row in 1:nrow(combos.sub)){
  foc.occ.path <- combos.sub$occ_files[foc.row]
  foc.occ.br<-brick(foc.occ.path)
  foc.occ.br.sub <- foc.occ.br[[31:nlayers(foc.occ.br)]]
  agree <- dummy
  agree[foc.occ.br.sub==1 & hlzs.30.yr.series==1] <- 1 
#  plot(agree[[10]])

# see: http://uwf.edu/zhu/evr6930/2.pdf  
#commission = 1- user's accuracy, which is: total # correctly assigned as q / total number categorized as q
#omission = 1- producer's accuracy, which is: total number correctly assigned as q / total number of q according to reference

#in this case, the "reference" is the dynamic niche locations  

# for comission  
agree.vec<- unlist(lapply(as.list(agree), FUN=TotalOccCells))#total # correctly assigned as q
hlzs.30.yr.series.vec <- unlist(lapply(as.list(hlzs.30.yr.series), FUN=TotalOccCells))# sum of this is # total # categorized as q
# the resulting vectors divided by each other will be user's accuracy
usersacc <- agree.vec/hlzs.30.yr.series.vec
# then take 1-usersacc to get commission
commission <- 1-usersacc

# then take average and sd for the final figure...
commission.m <- mean(commission)
commission.sd <- sd(commission)

# for omission
foc.occ.br.sub.vec<- unlist(lapply(as.list(foc.occ.br.sub), FUN=TotalOccCells))
# sum of this is # total # of q according to reference
# the resulting vectors divided by each other will be producer's accuracy
prodacc <- agree.vec/foc.occ.br.sub.vec
# then take 1-prodacc to get ommission
omission <- 1-prodacc
# then take average and sd for the final figure...
omission.m <- mean(omission)
omission.sd <- sd(omission)
# ok, so we've calculated
print("Parameters, Commission, Omission")
print(foc.occ.path)
print(commission.m)
print(omission.m)

combosrow <- which(combos$occ_files==foc.occ.path)
combos$omission.m[combosrow] <- omission.m
combos$omission.sd[combosrow] <- omission.sd
combos$commission.m[combosrow] <- commission.m
combos$commission.sd[combosrow] <- commission.sd
} # end looping through d/p combos


} # end looping through lifezones
combos.30yreval <- combos
save(combos.30yreval, file="combos_30yracc_Eval.Rdata")
