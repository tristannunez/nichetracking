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
