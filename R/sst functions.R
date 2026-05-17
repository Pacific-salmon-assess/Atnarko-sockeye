########################################################################################
# sst functions.R
# a couple of handy functions for processing sst data. Adapted from code written by 
# M. Malick (NOAA)
########################################################################################

sst.anomaly <- function(data, ref.years) {
  ## Calculate monthly, per grid cell, anomalies of SST
  ##
  ## Anomalies are calculated as the difference between a grid cell specific
  ## SST value for a given year/month and the long-term monthly mean (defined
  ## by ref.years) for that grid cell. This follows the methods outlined in
  ## Mueter et al. 2002, CJFAS (https://doi.org/10.1139/f02-020).
  ##
  ## data = data.frame of SST data
  ## ref.years = reference years in which to calculate the long-term mean,
  ##             should be a continuous sequence, e.g., 1950:2016
  
  ## make sure ref.years is a continuous sequence
  if(length(ref.years) != length(min(ref.years):max(ref.years)))
    stop("years vector is not a sequence ascending by 1")
  
  ## make sure ref.years are available in input data
  if(sum(ref.years %in% data$year) != length(ref.years))
    stop("ref.years not contained in input data")
  
  ## subset reference years from data
  ref.sst <- data[data$year >= min(ref.years) & data$year <= max(ref.years), ]
  
  ## calculate monthly long-term mean for each grid cell
  ## NA's are removed in the calculation of long-term mean
  mnth.avg <- aggregate(sst ~ month + id, data = ref.sst,
                        function(x) mean(x, na.rm = TRUE),
                        na.action = na.pass)
  names(mnth.avg)[names(mnth.avg) == 'sst'] <- 'long.avg'
  sst.merge <- merge(data, mnth.avg)
  sst.merge <- sst.merge[order(sst.merge$year,
                               sst.merge$month,
                               sst.merge$lat,
                               sst.merge$lon), ]
  
  ## calculate sst anomaly
  sst.merge$anom <- sst.merge$sst - sst.merge$long.avg
  row.names(sst.merge) <- NULL
  sst <- data.frame(year = sst.merge$year,
                    month = sst.merge$month,
                    lon = sst.merge$lon,
                    lat = sst.merge$lat,
                    id = sst.merge$id,
                    sst = sst.merge$sst,
                    sst.anom = sst.merge$anom)
  return(sst)
}

sst.averager <- function(info, sst, distance = 400, which.months) {
  
  ## This function takes as input a data.frame of sst data output from the
  ## sst.anomaly() function and computes sst averages for each stock only
  ## including sst grid cells that are within a specified distance from the
  ## ocean entry location of a particular stock.
  ##
  ## The function outputs a data frame with columns:
  ##  $year = one year for each year in input SST data and stock.id
  ##  $stock.id = id number for a salmon stock
  ##  $sst = averaged raw SST values
  ##  $sst.anom = averaged SST anomalies
  ##
  ## Function arguments:
  ##   info = stock.info data.frame w/ stock.id number, lon, and lat, should
  ##          have one row per stock
  ##   sst = sst data output from sst.anomaly()
  ##   distance = distance in km from ocean entry location of stock a grid
  ##              cell can be to be included in the averaging. This distance
  ##              is measured to the center of the SST grid cell
  ##   which.months = months over which to average sst anomalies
  
  stock.id <- info$stock.id
  cells    <- unique(subset(sst, select = c(id, lat, lon2)))
  cells    <- cells[order(cells$id), ]
  n.cells  <- length(cells[ , 1])
  row.names(cells) <- NULL
  
  sst.out <- vector("list", length(stock.id))
  for(i in seq_along(stock.id)) {
    
    info.i     <- info[i , ]
    stock.id.i <- info.i$stock.id
    lat.i      <- info.i$lat
    lon.i      <- info.i$lon 
    
    dist <- rep(NA, n.cells)
    for(j in 1:n.cells)
      dist[j] <- haversine(lat.i, lon.i, cells$lat[j], cells$lon2[j])
    
    cells.sub <- cells[which(dist <= distance), ]
    sst.sub   <- sst[sst$id %in% cells.sub$id, ]
    
    months <- which.months
    
    sst.sub.mnths <- sst.sub[sst.sub$month %in% months, ]
    
    sst.avg <- ddply(sst.sub.mnths, .(year), summarize,
                     sst = mean(sst, na.rm = TRUE),
                     sst.anom = mean(sst.anom, na.rm = TRUE))
    
    sst.avg$stock.id <- stock.id.i
    
    sst.out[[i]] <- sst.avg
  }
  sst.out <- rbind.fill(sst.out)
  return(sst.out)
}

haversine <- function(lat1, lon1, lat2, lon2) {
  ## This function computes the great circle distance between two points given
  ## their latitiude and longitude (in decimal degrees) using the haversine
  ## formula. The output is the distance between the two points in km.
  ##
  ## lat1 = latitude of first point
  ## lon1 = longitude of first point
  ## lat2 = latitude of second point
  ## lon2 = longitude of second point
  
  # Convert degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  R <- 6371 # earth mean radius [km]
  delta.lon <- (lon2 - lon1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.lon/2)^2
  d <- 2 * R * asin(min(1, sqrt(a)))
  
  return(d) # distance in km
}