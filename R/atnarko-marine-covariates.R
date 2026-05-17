########################################################################################
# atnarko-marine-covariates.R
# code to wrangle marine covariates for Atnarko sockeye analyses
# see: https://github.com/michaelmalick/r-ersst
#########################################################################################

library(plyr)
library(tidyverse)
source("R/sst functions.R")

## download and process SST data UNCOMMENT IF YOU NEED TO DOWNLOAD RAW SST AND PROCESS IT

ersst::sst_download(years = 1950:2024,
                   months = 1:12,
                   save.dir = "./data/marine/sst_raw/",
                   version = 5)

sst.raw.full <- ersst::sst_load(years = 1950:2024,
                               months = 1:12,
                               read.dir = "./data/marine/sst_raw/",
                               version = 5)

sst.raw.np <- ersst::sst_subset_space(sst.raw.full,
                                     lat.min = 36,
                                     lat.max = 80,
                                     lon.min = 170,
                                     lon.max = 250)

sst.raw.df <- ersst::sst_dataframe(sst.raw.np)

write.csv(sst.raw.df, "data/marine/sst_raw.csv", row.names = FALSE)

sst.raw <- read.csv("data/marine/sst_raw.csv")
head(sst.raw)
tail(sst.raw)
sapply(sst.raw, class)
summary(sst.raw)

## calculate SST anomalies and average across specified period and region

sst.anom <- sst.anomaly(sst.raw, ref.years = 1950:2024)
head(sst.anom)
tail(sst.anom)
summary(sst.anom)
sapply(sst.anom, class)

## Convert longitude to match salmon data
sst.anom$lon2 <- ifelse(sst.anom$lon > 180, sst.anom$lon - 360, sst.anom$lon)

write.csv(sst.anom, "data/marine/sst_raw_anomalies.csv", row.names=F)

## Read in static ocean entry points by region and sst anomalies
ocean_entry <- read.csv("data/marine/ocean_entry.csv")
sst_anom <- read.csv("data/marine/sst_raw_anomalies.csv")

## Calculate average SST anomaly within 2x2 degree area where stock spends few months of marine life 
summer_sst_stock_anomalies <- sst.averager(ocean_entry, sst_anom, distance = 200, which.months = c(5:9))

ggplot(data = summer_sst_stock_anomalies) +
  geom_line(aes(x=year, y = sst)) +
  xlab("Year") +
  ylab("Summer SST") +
  theme_bw() 

write.csv(summer_sst_stock_anomalies,"data/marine/atnarko-sst-summer.csv")

## Read in static ocean entry points by region and sst anomalies
npgo_raw <- read.csv("data/marine/npgo_raw.csv")

npgo <- npgo_raw|>
          group_by(year) |>
          summarise(npgo_index=mean(npgo_index))

ggplot(data = npgo) +
  geom_line(aes(x=year, y = npgo_index)) +
  xlab("Year") +
  ylab("NPGO") +
  theme_bw() 

write.csv(npgo,"data/marine/atnarko-npgo-annual.csv")
