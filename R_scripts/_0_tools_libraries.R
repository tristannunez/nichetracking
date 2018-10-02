# this is an initializing script in the workflow
# it gets called by the others so you don't have to actually run it.
# but good to look over anyway. 

# set working directory
dir <- "C:/git_niche_tracking/niche_tracking"
setwd(dir)
getwd()
# in the working directory there should be a folder "downloaded_data_in"
# in addition a folder "Rdata_in_out" for saving loaded datasets or resulting analyses as .Rdata files 
# install packages if for the first time:


ipak <- function(pkg){ 
     new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])] 
     if (length(new.pkg))  
         install.packages(new.pkg, dependencies = TRUE) 
     sapply(pkg, require, character.only = TRUE) 
 } 


packages <- c("raster", "rgdal", "gstat", "rgeos", 
                 "geosphere", "maptools",
               "ggplot2", "grid", "cowplot", "tidyr",
              "rasterVis", "RColorBrewer", "ggsn", "plyr")
# install or load packages
ipak(packages) 

# may need to put tidyr back in ... conflict with raster over the extract function

# this sets up a really nice extent for looking at hydrology
center<-c(31,-11) #x,y in deg.
width<-0.008333333*100 # whatever cell width is, 0.008333333 is ~ 1 km
zoom.extent <- extent(center[1]-.5*width, center[1]+.5*width, center[2]-.5*width, center[2]+.5*width)





# run multiplot:
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  #library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# making a moving window for smoothing analyses for the raster::focal function
# using smoothing buffer for now. ...
# generate smoothing buffer ....
raster1<-raster(matrix(rep(NA, 441),ncol=21))
res(raster1)
raster1[]<-0
plot(raster1)
raster1[221]<-1
plot(raster1)
matrix(raster1, ncol=21)
point<-rasterToPoints(raster1, fun=function(x){x>0}, spatial=T)
plot(point, add=T)
buff<-gBuffer(point, width=10*res(raster1)[1])
plot(buff, add=T)
buff_ras<-rasterize(buff, raster1,field=1)
mat_10_cell_rad<-matrix(buff_ras, ncol=21)
mat_10_cell_rad[is.na(mat_10_cell_rad)]<-0
plot(buff_ras)
mat_10_cell_rad # this is a 10-cell moving window

raster1<-raster(matrix(rep(NA, 121),ncol=11))
res(raster1)
raster1[]<-0
plot(raster1)
raster1[61]<-1
plot(raster1)
matrix(raster1, ncol=11)
point<-rasterToPoints(raster1, fun=function(x){x>0}, spatial=T)
plot(point, add=T)
buff<-gBuffer(point, width=5*res(raster1)[1])
plot(buff, add=T)
buff_ras<-rasterize(buff, raster1,field=1)
mat_5_cell_rad<-matrix(buff_ras, ncol=11)
mat_5_cell_rad[is.na(mat_5_cell_rad)]<-0
plot(buff_ras)
mat_5_cell_rad # this is a 5-cell moving window
# end of making the moving window ...



#radius<-3 #for devel

MakeMovWinMat <- function(radius){
if(radius==1){
  dispersal.matrix <- matrix(rep(1,9), ncol=3)
  dispersal.matrix[dispersal.matrix==0]<-NA # the focal cell is a 0
  dispersal.matrix[2,2]<-0
  dispersal.matrix
  
} else if(radius==2){
  m2 <- matrix(rep(1,25), ncol=5)
  m2[1,1]<-NA
  m2[1,5]<-NA
  m2[5,1]<-NA
  m2[5,5]<-NA
  m2[3,3]<-0
  dispersal.matrix<-m2
  dispersal.matrix
} else{
mv_width<-radius*2+1
raster1<-raster(matrix(rep(NA, mv_width^2),ncol=mv_width))
res(raster1)
raster1[]<-0
plot(raster1)
raster1[((mv_width^2)/2+1)]<-1
plot(raster1)
matrix(raster1, ncol=mv_width)
point<-rasterToPoints(raster1, fun=function(x){x>0}, spatial=T)
plot(point, add=T)
buff<-gBuffer(point, width=radius*res(raster1)[1])
plot(buff, add=T)
buff_ras<-rasterize(buff, raster1,field=1)

mat<-matrix(buff_ras, ncol=mv_width)
mat[((mv_width^2)/2+1)]<-0
mat # this is a 5-cell moving window
# end of making the moving window ...
}
}


# might explore generating moving windows from lines as well as polygons, see help for rasterize

