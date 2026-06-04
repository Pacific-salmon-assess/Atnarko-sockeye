########################################################################################
# atnarko-marine-covariates.R
# code to wrangle marine covariates for Atnarko sockeye analyses
# for sst wrangling package see: https://github.com/michaelmalick/r-ersst
#########################################################################################
library(plyr)
library(tidyverse)
source("R/sst functions.R")

## download and process SST data UNCOMMENT IF YOU NEED TO DOWNLOAD RAW SST AND PROCESS IT

# ersst::sst_download(years = 1950:2024,
#                    months = 1:12,
#                    save.dir = "./data/marine/sst_raw/",
#                    version = 5)
# 
# sst.raw.full <- ersst::sst_load(years = 1950:2024,
#                                months = 1:12,
#                                read.dir = "./data/marine/sst_raw/",
#                                version = 5)
# 
# sst.raw.np <- ersst::sst_subset_space(sst.raw.full,
#                                      lat.min = 36,
#                                      lat.max = 80,
#                                      lon.min = 170,
#                                      lon.max = 250)
# 
# sst.raw.df <- ersst::sst_dataframe(sst.raw.np)
# 
# write.csv(sst.raw.df, "data/marine/sst_raw.csv", row.names = FALSE)
# 
# sst.raw <- read.csv("data/marine/sst_raw.csv")
# head(sst.raw)
# tail(sst.raw)
# sapply(sst.raw, class)
# summary(sst.raw)
# 
# ## calculate SST anomalies and average across specified period and region
# 
# sst.anom <- sst.anomaly(sst.raw, ref.years = 1950:2024)
# head(sst.anom)
# tail(sst.anom)
# summary(sst.anom)
# sapply(sst.anom, class)
# 
# ## Convert longitude to match salmon data
# sst.anom$lon2 <- ifelse(sst.anom$lon > 180, sst.anom$lon - 360, sst.anom$lon)
# 
# write.csv(sst.anom, "data/marine/sst_raw_anomalies.csv", row.names=F)

## Read in static ocean entry points by region and sst anomalies
ocean_entry <- read.csv("data/marine/ocean_entry.csv")
sst_anom <- read.csv("data/marine/sst_raw_anomalies.csv")

## Calculate average SST anomaly within 2x2 degree area where stock spends few months of marine life 
summer_sst_stock_anomalies <- sst.averager(ocean_entry, sst_anom, distance = 200, which.months = c(5:9)) |>
  dplyr::rename(summer_sst = sst) |>
  select(year, summer_sst)

winter_spring_sst_stock_anomalies <- sst.averager(ocean_entry, sst_anom, distance = 200, which.months = c(1:4))|>
  dplyr::rename(winter_sst = sst) |>
  select(year, winter_sst)

ssts <- left_join(summer_sst_stock_anomalies,winter_spring_sst_stock_anomalies, by="year")

## Read in NPGO index and wrangle to annual average. See here for data/info: https://www.o3d.org/npgo/
npgo_raw <- read.csv("data/marine/npgo_raw.csv")

npgo <- npgo_raw|>
          group_by(year) |>
          dplyr::summarise(npgo_index=mean(npgo_index))

## North Pacific salmon abundances (updated from https://doi.org/10.1093/icesjms/fsae135)
salmon_abundnaces <- read.csv("data/marine/total_np_salmon.csv")

np_pinks <- salmon_abundnaces|>
  dplyr::rename(year = Return.Year,
         pinks = Pink) |>
  select(year,pinks) 
  
## Combine everything into a single csv
marine_covariates <- left_join(ssts,npgo, by="year") |>
  dplyr::left_join(np_pinks, by="year")

marine_covariates_long <- pivot_longer(marine_covariates,!year, names_to = "covariate", values_to = "value")

library(ggsidekick)
ggplot(data = marine_covariates_long) +
  geom_line(aes(x=year, y = value)) +
  facet_wrap(~covariate, scales="free", ncol=2) +
  xlab("Year") +
  ylab("value") +
  theme_sleek() 

ggsave("Figures/marine-covariates.jpeg")
write.csv(marine_covariates,"data/marine/atnarko-marine_covariates.csv")
