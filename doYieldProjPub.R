rm(list=ls())
library(dplyr)
library(tidyr)
library(data.table)
library(countrycode)
library(lfe)
library(grid)
library(gridExtra)
library(ggplot2)
library(cowplot)
library(readxl)
library(broom)
library(mFilter)
library(maps)
library(mapproj)

#----------------------------------------------------------------------------------------------------#
# Get the relationship between CO2 and temperature (global level) under RCP 2.6 and RCP 8.5.
## Data are from Dr. Moore.
CO2Temp <- read.csv('rcptempco2.csv')

Reg85 <- lm(CO2 ~ Temp + I(Temp^2), data = filter(CO2Temp, Scenario == '85'))
summary(Reg85)

Reg26 <- lm(CO2 ~ Temp + I(Temp^2), data = filter(CO2Temp, Scenario == '26'))
summary(Reg26)
#----------------------------------------------------------------------------------------------------#


#----------------------------------------------------------------------------------------------------#
# Projecting maize yields using Moore et al. (2017) approach and the climate data. (CO2 are imputed
# based on the above two regressions).
# This is done in three steps:
# (1) Use the climate data to predict yield losses.
# (2) Recover the yield level based on baseline yields.
# (3) Calculate deviations from trend.

rm(list = ls())

# Get sample countries.
RegDat1 <- read.csv('RegressionDatFinal.csv')

Countries <- as.character(unique(RegDat1$Country))
# Remove three countries because we lack climate projections on them.
Countries <- Countries[-which(Countries %in% c('Israel', 'Ecuador', 'Honduras'))]

year_low <- 1961; year_upp <- 2050  # Historical time: 1961-2004; future time: 2006-2050.
BaseYears <- c(1979, 2013) # Baseline periods: years between BaseYears[1] and BaseYears[2]
# BaseYears <- c(2000, 2013) # Chaning the baseline period does not affect the variance.

# Read Climate Data.
ClimateProjDat <- fread('0_Data_ClimateSub.csv')

###### The baseline temperature and precipitation.
BaseClimate <- ClimateProjDat %>%
  dplyr::filter(year <= BaseYears[2], year >= BaseYears[1], scenario == 'historic', climate_model == 'CRU3.23') %>%
  group_by(country, variable) %>%
  summarise(MeanValue = mean(value))  # Baseline average growing season temperature and precipitation.


TempDat <- ClimateProjDat %>%   # Calculate changes in precipitation relative to baseline data.
  dplyr::filter(year != 2005, year >= year_low, year <= year_upp, variable == 'temperature', scenario != 'historic') %>%
  mutate(value = ifelse(country == 'PER', value + 5.5, value)) %>%
  left_join(., BaseClimate, by = c('country', 'variable')) %>%
  mutate(TempChange = value - MeanValue, ### Changes are in absolute changes in Celsius degree.
         CO2Change = ifelse(scenario == 'rcp8.5', 358.0136 + 78.4612*TempChange + 15*TempChange^2 -360, 337.155 + 167*TempChange - 67.820*TempChange - 360)) %>%
  dplyr::select(country, climate_model, year, TempChange, scenario, MeanValue, CO2Change)


PrepDat <- ClimateProjDat %>%   # Calculate changes in precipitation relative to baseline data.
  dplyr::filter(year != 2005, year >= year_low, year <= year_upp, variable == 'precipitation', scenario != 'historic') %>%
  left_join(., BaseClimate, by = c('country', 'variable')) %>%
  mutate(PrepChange = 100*(value - MeanValue)/MeanValue) %>%  ### Changes are in percentage changes.
  dplyr::select(country, climate_model, year, PrepChange,  scenario)

# Combine temperature and precipitation and then predict yield losses (in percentage).
MooreYieldProject <- TempDat %>%
  left_join(., PrepDat, by = c('country', 'climate_model', 'year', 'scenario')) %>%
  mutate(YieldChangeTmp = (3.71419*TempChange -0.88715*TempChange^2 - 0.40331*TempChange*MeanValue + 0.03767*(TempChange^2)*MeanValue), # Terms with temperature
         YieldChangePre = 0.21*PrepChange,  # terms with precipitation
         YieldChangeCO2 = 10.82*(CO2Change/(CO2Change + 50)), # terms with CO2
         YieldChangeAdaptTemp = 0.17*TempChange,  # Interaction term with temperature (small and insignificant)
         YieldChangeAdapt = 0, # This parameter is unknown, but it does not affect the variance of detrended yields. Set to be zero.
         YieldChangeTmpCO2 = YieldChangeTmp + YieldChangeCO2,
         YieldChangeTmpPre = YieldChangeTmp + YieldChangePre + YieldChangeAdapt,
         YieldChangeTmpPreCO2 = YieldChangeTmp + YieldChangePre + YieldChangeCO2 + YieldChangeAdapt)

# How many yield changes are negative?
## Without CO2
sum(MooreYieldProject$YieldChangeTmpPre <= 0)/nrow(MooreYieldProject)  # 0.82
## With CO2
sum(MooreYieldProject$YieldChangeTmpPreCO2 <= 0)/nrow(MooreYieldProject) # 0.58

# Calculate the times that the projected yield changes are greater than 100%.
sum(abs(MooreYieldProject$YieldChangeTmpPreCO2) >= 100) ## 56


# Use the projections with temperature and precipitation, with CO2 and without CO2 (adaptation ignored)
MooreYieldProjectCO2NoCO2 <- MooreYieldProject %>%
  dplyr::select(country, year, climate_model, scenario, YieldChangeTmpPre, YieldChangeTmpPreCO2) %>%
  rename(no = YieldChangeTmpPre, yes = YieldChangeTmpPreCO2) %>%
  gather(co2, value, 5:6)
### Get the yield changes until now. Next, use the average historic yields from FAO to recover the yield levels (in tonne per hectare)

FAOYieldDat <- fread('0_Data_MaizeYield.csv') %>%   # Calculate the baseline yields.
  dplyr::filter(country %in% countrycode(Countries, 'country.name', 'iso3c'), crop_model == 'fao') %>%
  dplyr::select(country, year, value) %>%
  dplyr::rename(FAOYield = value)

##### Get deviations from trend for the historic yields from the FAO.
FAOYieldDev <- FAOYieldDat %>%
  dplyr::filter(country %in% countrycode(Countries, 'country.name', 'iso3c')) %>%
  group_by(country) %>%
  mutate(Shock = mFilter::hpfilter(FAOYield, freq = 100, type = 'lambda')$cycle,
         Trend = mFilter::hpfilter(FAOYield, freq = 100, type = 'lambda')$trend,
         DevYield = Shock/Trend,
         Country = countrycode(country, 'iso3c', 'country.name')) %>%
  as.data.frame()


##### The baseline yield data.
FAOYieldMean <- FAOYieldDat %>%
  dplyr::filter(year <= BaseYears[2], year >= BaseYears[1]) %>%
  group_by(country) %>%
  summarise(HistYield = mean(FAOYield))


##### Recover the yield levels by changes in yields.
MooreYieldProjectCO2NoCO2Level <- MooreYieldProjectCO2NoCO2 %>%
  left_join(., FAOYieldMean, by = 'country') %>%
  mutate(YieldPred = (1 + value/100)*HistYield,  ### The projected yield changes are in %, so it has to be divided by 100.
         Country = countrycode(country, 'iso3c', 'country.name')) # Predict the yield levels.


##### Detrend the projected yield data.
MooreYieldPredDat <- MooreYieldProjectCO2NoCO2Level %>%
  dplyr::filter(scenario != 'historic') %>%
  dplyr::select(country, Country, year, climate_model, scenario, co2, YieldPred) %>%
  group_by(country, Country, climate_model, scenario, co2) %>%
  mutate(Shock = mFilter::hpfilter(YieldPred, freq = 100, type = 'lambda')$cycle,
         Trend = mFilter::hpfilter(YieldPred, freq = 100, type = 'lambda')$trend[, 1],
         DevYield = Shock/Trend, ID = 'Moore') %>%
  filter(abs(DevYield) < 1) %>%
  ungroup() %>%
  dplyr::select(Country, year, climate_model, scenario, co2, DevYield, ID, country)

# The yield projections with Moore end here. Next go to AgMIP.
#----------------------------------------------------------------------------------------------------------------------#

#### AgMIP
CropYieldDatRaw <- fread('0_Data_MaizeYieldSub.csv')

AgMIPCropYield <- CropYieldDatRaw %>%
  group_by(crop_model, climate_model, scenario, co2, country) %>%
  mutate(Shock = mFilter::hpfilter(value, freq = 100, type = 'lambda')$cycle,
         Trend = mFilter::hpfilter(value, freq = 100, type = 'lambda')$trend,
         DevYield = Shock/Trend, ID = 'AgMIP') %>%
  ungroup() %>%
  filter(abs(DevYield) < 1) %>%
  mutate(Country  = countrycode(country, 'iso3c', 'country.name')) %>%
  dplyr::select(Country, year, crop_model, climate_model, scenario, co2, DevYield, ID, country)


##### China as an example, how many extremes are there in the future?
# ChinaExpHist <- dplyr::filter(FAOYieldDev, country == 'CHN')
# ChinaExpRCP26 <- dplyr::filter(AgMIPCropYield, Country == 'China', scenario == 'rcp2.6')
# ChinaExpRCP85 <- dplyr::filter(AgMIPCropYield, Country == 'China', scenario == 'rcp8.5')
# sum(abs(ChinaExpHist$DevYield) >= 0.2)/nrow(ChinaExpHist)
# sum(abs(ChinaExpRCP26$DevYield) >= 0.2)/nrow(ChinaExpRCP26)
# sum(abs(ChinaExpRCP85$DevYield) >= 0.2)/nrow(ChinaExpRCP85)

# Combine the projections by Moore and by AgMIP
AllYieldProjections <- bind_rows(MooreYieldPredDat, AgMIPCropYield)

CO2AllYieldProjections <- filter(AllYieldProjections, co2 == 'yes')
NOCO2AllYieldProjections <- filter(AllYieldProjections, co2 == 'no')

RCP26AllYieldProjections <- filter(AllYieldProjections, scenario == 'rcp2.6')
RCP85AllYieldProjections <- filter(AllYieldProjections, scenario == 'rcp8.5')




#---------------------------------------------------------------------------------------------------------------------------------------------#
#                                       Generate distributions of price variability
#---------------------------------------------------------------------------------------------------------------------------------------------#
# Prefered regression.
Reg4 <- lm(PriceCV_MktYear ~ NetImportRatio + LagStockRatio + abs(DevYield) + Conflict + RealExCv +
             factor(Region) + factor(Country) + factor(City) + factor(Year) + factor(Content), data = RegDat1)

## 1. Let other variables in Historical averages.
RegDatAve <- RegDat1 %>%
  group_by(Country, City, Content, Region, ISO) %>%
  summarise(NetImportRatio = mean(NetImportRatio),
            LagStockRatio = mean(LagStockRatio),
            Conflict = mean(Conflict),
            RealExCv = mean(RealExCv))

## 2. Merge the dataset with the yield deviation of FAO.
FAOYieldDevDat <- FAOYieldDev %>%
  dplyr::select(country, year, DevYield) %>%
  left_join(., RegDatAve, by = c('country' = 'ISO')) %>%
  mutate(Year = 2015)    # The regression has year dummies. So I am predicting price variability in 2015.

FAOYieldDevDat$PriceVolPred <- predict(Reg4, newdata = FAOYieldDevDat)
#
# ## 3. Taking averages across countries.
HistPriceVolPredDat <- FAOYieldDevDat %>%
  group_by(Country, year) %>%  # I group by year (not Year) so that I can keep all observations. However, it is the prediction for 2015 only.
  summarise(PriceVolPred = mean(PriceVolPred)) %>%
  mutate(scenario = 'Historical')

#---------------------------------------------------------------------------------------------------------------------------------------------#
# Predict price variability using maize yields that are predicted by climate models.
FuturePriceVolPredDat <- AllYieldProjections %>%
  dplyr::select(country, year, DevYield, crop_model, co2, climate_model, scenario, ID) %>%
  left_join(., RegDatAve, by = c('country' = 'ISO')) %>%
  mutate(Year = 2015)    # The regression has year dummies. So I am predicting price variability in 2015.

FuturePriceVolPredDat$PriceVolPred <- predict(Reg4, newdata = FuturePriceVolPredDat)

FuturePriceVolPredDat <- FuturePriceVolPredDat %>%
  group_by(Country, year, crop_model, co2, climate_model, scenario, ID) %>%
  summarise(PriceVolPred = mean(PriceVolPred))  # Take average across markets for each sample country.

CO2PriceVolPredDat <- filter(FuturePriceVolPredDat, co2 == 'yes')
NOCO2PriceVolPredDat <- filter(FuturePriceVolPredDat, co2 == 'no')


# Using Bolivia as an example.
## Historic likelihood.
HistBol <- HistPriceVolPredDat %>%
  filter(Country == 'Bolivia')

quantile(HistBol$PriceVolPred, probs = 0.9)

FutureBolAgMIP <- NOCO2PriceVolPredDat %>%
  filter(Country == 'Bolivia', ID == 'AgMIP', scenario == 'rcp2.6')

sum(FutureBolAgMIP$PriceVolPred>=quantile(HistBol$PriceVolPred, probs = 0.9))/nrow(FutureBolAgMIP)

FutureBolAgMIP <- NOCO2PriceVolPredDat %>%
  filter(Country == 'Bolivia', ID == 'AgMIP', scenario == 'rcp8.5')

sum(FutureBolAgMIP$PriceVolPred>=quantile(HistBol$PriceVolPred, probs = 0.9))/nrow(FutureBolAgMIP)

FutureBolMoore <- NOCO2PriceVolPredDat %>%
  filter(Country == 'Bolivia', ID == 'Moore', scenario == 'rcp2.6')

sum(FutureBolMoore$PriceVolPred>=quantile(HistBol$PriceVolPred, probs = 0.9))/nrow(FutureBolAgMIP)

FutureBolMoore <- NOCO2PriceVolPredDat %>%
  filter(Country == 'Bolivia', ID == 'Moore', scenario == 'rcp8.5')

sum(FutureBolMoore$PriceVolPred>=quantile(HistBol$PriceVolPred, probs = 0.9))/nrow(FutureBolAgMIP)

#--------------------------------------------------------------------------------------------------------------------------$
# Summarise the changes in the mean and tail of the price variability.
#--------------------------------------------------------------------------------------------------------------------------$
MeanPriceVolHist <- HistPriceVolPredDat %>%
  group_by(Country) %>%
  summarise(MeanPriceVol = mean(PriceVolPred)) %>%
  mutate(Label = 'Raw')

ExtremePriceVolHist <- HistPriceVolPredDat %>%
  group_by(Country) %>%
  summarise(Threshold = quantile(PriceVolPred, 0.9))

MeanPriceVolChange <- FuturePriceVolPredDat %>%
  group_by(Country, scenario, co2, ID) %>%
  summarise(MeanPriceVol = mean(PriceVolPred)) %>%
  full_join(., MeanPriceVolHist, by = 'Country') %>%
  mutate(ChangeRatio = 100*(MeanPriceVol.x/MeanPriceVol.y - 1),
         variable = 'Mean')

ExtremePriceVolPred <- FuturePriceVolPredDat %>%
  full_join(., ExtremePriceVolHist, by = 'Country') %>%
  group_by(Country, scenario, co2, ID) %>%
  summarise(ExProd = 100*sum(PriceVolPred >= Threshold)/n()) %>%
  mutate(variable = 'Extreme') %>%
  mutate(Label = 'Raw')

MeanExtremePriceVol <- rbind(MeanPriceVolChange, ExtremePriceVolPred)

# Calculate the mean and tail changes after changing the import and stock-to-use ratios.
ParaImport <- 0.15 # Import
ParaStock <- 0.3 # Stock
ImportChange <- 0.1
StockChange <- 0.05


ImportMeanPriceVolChange <- FuturePriceVolPredDat %>%
  mutate(PriceVolPred = PriceVolPred - ParaImport*ImportChange) %>%
  group_by(Country, scenario, co2, ID) %>%
  summarise(MeanPriceVol = mean(PriceVolPred)) %>%
  full_join(., MeanPriceVolHist, by = 'Country') %>%
  mutate(ChangeRatio = 100*(MeanPriceVol.x/MeanPriceVol.y - 1),
         variable = 'Mean', Label = 'Import')

StockMeanPriceVolChange <- FuturePriceVolPredDat %>%
  mutate(PriceVolPred = PriceVolPred - ParaStock*StockChange) %>%
  group_by(Country, scenario, co2, ID) %>%
  summarise(MeanPriceVol = mean(PriceVolPred)) %>%
  full_join(., MeanPriceVolHist, by = 'Country') %>%
  mutate(ChangeRatio = 100*(MeanPriceVol.x/MeanPriceVol.y - 1),
         variable = 'Mean', Label = 'Stock')

ImportExtremePriceVolPred <- FuturePriceVolPredDat %>%
  mutate(PriceVolPred = PriceVolPred - ParaImport*ImportChange) %>%
  full_join(., ExtremePriceVolHist, by = 'Country') %>%
  group_by(Country, scenario, co2, ID) %>%
  summarise(ExProd = 100*sum(PriceVolPred >= Threshold)/n()) %>%
  mutate(variable = 'Extreme', Label = 'Import')

StockExtremePriceVolPred <- FuturePriceVolPredDat %>%
  mutate(PriceVolPred = PriceVolPred - ParaStock*StockChange) %>%
  full_join(., ExtremePriceVolHist, by = 'Country') %>%
  group_by(Country, scenario, co2, ID) %>%
  summarise(ExProd = 100*sum(PriceVolPred >= Threshold)/n()) %>%
  mutate(variable = 'Extreme', Label = 'Stock')



AllPriceProj <- bind_rows(MeanExtremePriceVol, ImportMeanPriceVolChange, StockMeanPriceVolChange, ImportExtremePriceVolPred, StockExtremePriceVolPred) %>%
  mutate(Group = paste0(substr(scenario, 1, 3), Label, ID))

AllPriceProjMean <- AllPriceProj %>%
  filter(variable == 'Mean') %>%
  filter(ID == 'AgMIP' | Group == 'rcpRawMoore')

AllPriceProjExtreme <- AllPriceProj %>%
  filter(variable == 'Extreme') %>%
  filter(ID == 'AgMIP' | Group == 'rcpRawMoore')


AllPriceProjMeanMoore <- AllPriceProj %>%
  filter(variable == 'Mean') %>%
  filter(ID == 'Moore' | Group == 'rcpRawAgMIP')

AllPriceProjExtremeMoore <- AllPriceProj %>%
  filter(variable == 'Extreme') %>%
  filter(ID == 'Moore' | Group == 'rcpRawAgMIP')


# Another counterfactucal. What if the countries reduce their import ratios and stock-to-use ratios.

ImportChange2 <- -0.1
StockChange2 <- -0.05

ImportMeanPriceVolChange2 <- FuturePriceVolPredDat %>%
  mutate(PriceVolPred = PriceVolPred - ParaImport*ImportChange2) %>%
  group_by(Country, scenario, co2, ID) %>%
  summarise(MeanPriceVol = mean(PriceVolPred)) %>%
  full_join(., MeanPriceVolHist, by = 'Country') %>%
  mutate(ChangeRatio = 100*(MeanPriceVol.x/MeanPriceVol.y - 1),
         variable = 'Mean', Label = 'Import')

StockMeanPriceVolChange2 <- FuturePriceVolPredDat %>%
  mutate(PriceVolPred = PriceVolPred - ParaStock*StockChange2) %>%
  group_by(Country, scenario, co2, ID) %>%
  summarise(MeanPriceVol = mean(PriceVolPred)) %>%
  full_join(., MeanPriceVolHist, by = 'Country') %>%
  mutate(ChangeRatio = 100*(MeanPriceVol.x/MeanPriceVol.y - 1),
         variable = 'Mean', Label = 'Stock')

ImportExtremePriceVolPred2 <- FuturePriceVolPredDat %>%
  mutate(PriceVolPred = PriceVolPred - ParaImport*ImportChange2) %>%
  full_join(., ExtremePriceVolHist, by = 'Country') %>%
  group_by(Country, scenario, co2, ID) %>%
  summarise(ExProd = 100*sum(PriceVolPred >= Threshold)/n()) %>%
  mutate(variable = 'Extreme', Label = 'Import')

StockExtremePriceVolPred2 <- FuturePriceVolPredDat %>%
  mutate(PriceVolPred = PriceVolPred - ParaStock*StockChange2) %>%
  full_join(., ExtremePriceVolHist, by = 'Country') %>%
  group_by(Country, scenario, co2, ID) %>%
  summarise(ExProd = 100*sum(PriceVolPred >= Threshold)/n()) %>%
  mutate(variable = 'Extreme', Label = 'Stock')



AllPriceProj2 <- bind_rows(MeanExtremePriceVol, ImportMeanPriceVolChange2, StockMeanPriceVolChange2, ImportExtremePriceVolPred2, StockExtremePriceVolPred2) %>%
  mutate(Group = paste0(substr(scenario, 1, 3), Label, ID))

AllPriceProjMean2 <- AllPriceProj2 %>%
  filter(variable == 'Mean') %>%
  filter(ID == 'AgMIP' | Group == 'rcpRawMoore')

AllPriceProjExtreme2 <- AllPriceProj2 %>%
  filter(variable == 'Extreme') %>%
  filter(ID == 'AgMIP' | Group == 'rcpRawMoore')



#----------------------------------------------------------------------------------------------------------------------------------------------#
# End
#----------------------------------------------------------------------------------------------------------------------------------------------#











