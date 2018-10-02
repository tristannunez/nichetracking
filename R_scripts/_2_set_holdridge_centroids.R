# set working directory
dir <- "F:/git_niche_tracking/niche_tracking"
setwd(dir)
source("./R_scripts/_0_tools_libraries.R") # loads libraries, buffers, focal area, etc

# read in holdridge thresholds
h_raw<-read.csv("./downloaded_data_in/holdridge_thresholds.csv")

h_hex <- h_raw
# calculate centroids in hex space
h_hex$high_y <- log2(h_raw$maBt_high/1.5)
h_hex$low_y <- log2(h_raw$maBt_low/1.5)
h_hex$mid_y <- h_hex$high_y - 0.5 # add 0.5 b/c y axis is flipped

h_hex$high_x <- log2(h_raw$atap_high/62.5)
h_hex$low_x <- log2(h_raw$atap_low/62.5)
h_hex$mid_x <- h_hex$high_x - 0.5
# check
head(h_hex)

# convert observed values into hex space

MaBt_to_Hex_y <- function(maBt){
  log2(maBt/1.5)
}

Atap_to_Hex_x <- function(atap){
  log2(atap/62.5)
}

#check
maBt.eg <- 1.5
maBt.hex <- MaBt_to_Hex_y(maBt.eg)
# check
atap.eg <- 10
atap.hex <- Atap_to_Hex_x(atap.eg)

# calculate distance to centroids

head(h_hex)

HexDist_to_Centroid<- function(atap.hex, maBt.hex, h_hex){
  cent.x <- h_hex$mid_x
  cent.y <- h_hex$mid_y
   x0 <- atap.hex
   y0 <- maBt.hex
#pred_type <- character(0)  
dists <- numeric(0) 
 for(i in 1:length(cent.x)){
    x1 <- cent.x[i]
    y1 <- cent.y[i]
    dx <- x1-x0
    dy <- y1-y0
  if(sign(dx)==sign(dy)){
    dist.out <-abs(dx+dy)
  dists[i]<-dist.out
  } else (
    dist.out <- max(abs(dx), abs(dy)))
  dists[i]<-dist.out
  }  
pred_type<-h_hex$type[which(dists==min(dists))]
pred_reg<-h_hex$lat_reg[which(dists==min(dists))]
#print(pred_type)
#print(pred_reg)
return(which(dists==min(dists)))
}

# test with centroid values
HexDist_to_Centroid(0.5, 4.5, h_hex) # desert tropical
HexDist_to_Centroid(0.5, 0.5, h_hex) # dry tundra
HexDist_to_Centroid(2.5, 2.5, h_hex) # steppe
HexDist_to_Centroid(7.5, 4.5, h_hex) # rain forest, tropical
HexDist_to_Centroid(3.5, 4.5, h_hex) # very dry forest
HexDist_to_Centroid(2.5, 4.5, h_hex) # thorn woodland

# test with close to centroid values
HexDist_to_Centroid(0.6, 4.6, h_hex) # desert tropical
HexDist_to_Centroid(0.6, 0.6, h_hex) # dry tundra
HexDist_to_Centroid(2.6, 2.6, h_hex) # steppe
HexDist_to_Centroid(7.6, 4.6, h_hex) # rain forest, tropical
HexDist_to_Centroid(3.6, 4.6, h_hex) # very dry forest
HexDist_to_Centroid(2.6, 4.6, h_hex) # thorn woodland


# maBt.in is example temp in C
maBt.in <- 8
# atap.in is example precip in mm
atap.in <- 400
# 8, 400 should give Steppe


MaBt_Atap_to_Type <- function(atap.in, maBt.in, h_hex){
  maBt.hex <- MaBt_to_Hex_y(maBt.in)  
  atap.hex <- Atap_to_Hex_x(atap.in)
  hexno <- HexDist_to_Centroid(atap.hex, maBt.hex, h_hex)
  return(hexno)
}
# this gives the hex number! using 400, 8, should give steppe
hexno.pred<-MaBt_Atap_to_Type(atap.in,maBt.in, h_hex)


write.csv(h_hex, "./Rdata_in_out/hlz_hexagonal_centroids.csv", row.names = F)


