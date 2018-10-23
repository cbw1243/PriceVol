File Descriptions:
The folder contains 5 files that are required to reproduce the empirical results of the paper by Chen and Villoria (2018), entitled "Climate Shocks, Food Price Stability and International Trade: Evidence from 76 Maize Markets in 27 Net-importing Countries". Three files ("0_Data_ClimateSub.csv", "0_Data_MaizeYieldSub.csv" and "RegressionDatFinal.csv") are the datasets, and another two ("doYieldProjPub.r" and "doPlots.r") are the R codes to analyze the data.



1. Data Descriptions:
1.1 The file "0_Data_ClimateSub.csv" contains country-level historic observations of growing season climate (temperature and precipitation) during 1941-2014 and country-level historic and future projections of growing season climate during 1961-2099. The data of historic observations are labelled by "CRU3.23" in the column of "climate_model" in the dataset and are sourced from the Climatic Research Unit monthly time-series, Version 3.23 (Mitchell and Jones, 2005; Harris, 2015). The projected data are outputs of 5 climate models, including GFDL, HadGEM, IPSL, MIROC and NorESM under two RCP scenarios (RCP 2.6 and RCP 8.5). The projected data are sourced from the CMIP5 downloading facility at Geoshare (Villoria et al., 2016b).

1.2 The file "0_Data_MaizeYield.csv" contains country-level historic observations of maize yields during 1951-2014 and country-level historic and future projections of maize yields during 1961-2050 modeled by AgMIP-GGCMI project. The data of historic observations are labelled by "fao" in the column of "crop_model" in the dataset and are sourced from the FAOSTAT database (accessed on 05/08/2017). The projected data are outputs of 20 climate-RCP-crop model combinations with and without CO2 fertilization. The climate models are GFDL, HadGEM, IPSL, MIROC and NorESM. The crop models are LPJmL and pDSSAT. The RCPs are RCP 2.5 and RCP 8.5. The projected data are sourced from the AgMIP-GGCMI's downloading facility at Geoshare (Villoria et al., 2016a).



2. Code Descriptions:
2.1 The R file "doYieldProjPub.r" projects yields based on Moore et al. (2017)'s response function using the climate data. The AgMIP crop model outputs are also analyzed here. 
### Run this file first. 

2.2 The R file "doPlots.r" is to produce the figures to illustrate the results. 
### Run this file after running 'doYieldProj.r" and all figures will be produced. 

Notes: The R packages dependencies include: "tidyr", "dplyr", "grid", "gridExtra", "lfe", "ggplot2", "countrycode", "cowplot", "mFilter", "data.table". Use "install.packages()" to install them if you have not. 

### Acknowledgements: This work was supported by the Agriculture and Food Research Initiative Competitive Grant 2015-67023-25258 from the USDA National Institute of Food and Agriculture.
