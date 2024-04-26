# Atnarko-sockeye
## Background context
Work in progress repository for Atnarko sockeye. Inteded to support: 

1.	a deep dive on available and updated data; 
2.	updates to brood table and exploration of decomposing system into lake and river rearing stocks; 
3.	improved documentation of key assumptions; and
4.	analyses exploring evidence for regime shifts and hypothesized drivers of changes in system and implications for recovery potential and recovery targets

Some background reading includes [2015 Nuxalk Atanrko Sockeye Recoveyr Plan](https://www.ccira.ca/wp-content/uploads/2018/07/AtnarkoSockeyRecoveryPlan-FullSizeRender-45.pdf) and [Conservation Risk and Uncertainty in Recovery Prospects for a Collapsed and Culturally Important Salmon Population in a Mixed-Stock Fishery](https://afspubs.onlinelibrary.wiley.com/doi/full/10.1002/mcf2.10092)

## Repository structure

- `probable-status.Rmd`: Sources data, wrangles it, calculates rates of change by DU, summarizes probable status and associated DU level metadata. Renders html document.
- `data`: DU metadata and associated time series of mature individuals. Sourced from [here](https://github.com/hertzPSF/COSEWIC-compilation), original data sources detailed in [data sources](https://github.com/Pacific-salmon-assess/COSEWIC-prioritize/blob/main/README.md#data-sources).
- `output`: All outputs including master .csv of percent change in mature individuals and probable DU status designations, stand alone plots of mature individuals over time by species and region.

If
