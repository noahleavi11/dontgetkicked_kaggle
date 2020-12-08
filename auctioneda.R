library(tidyverse)
library(stringi)
library(mltools)
library(caret)
library(data.table)



cars <- read_csv("training.csv")
cars.test <- read_csv("test.csv")


#####################################################################################################
# Exploring data frame
#####################################################################################################
cars %>% group_by(Make) %>% summarize(count=n()) %>% arrange(desc(count))

cars %>% group_by(Model) %>% summarize(count=n()) %>% arrange(desc(count))

cars %>% group_by(Size) %>% summarize(count=n()) %>% arrange(desc(count))

cars %>% group_by(Transmission) %>% summarize(count=n()) %>% arrange(desc(count))

cars %>% group_by(WheelTypeID) %>% summarize(count=n()) %>% arrange(desc(count))

cars %>% group_by(WheelType) %>% summarize(count=n()) %>% arrange(desc(count))

cars %>% group_by(PRIMEUNIT) %>% summarize(count=n()) %>% arrange(desc(count)) #drop; so many null values

cars %>% group_by(AUCGUART) %>% summarize(count=n()) %>% arrange(desc(count))  #also tons of null values

cars %>% group_by(BYRNO) %>% summarize(count=n()) %>% arrange(desc(count))

cars %>% group_by(IsOnlineSale) %>% summarize(count=n()) %>% arrange(desc(count)) # really skewed to 0

cars %>% group_by(Nationality) %>% summarize(count=n()) %>% 
  arrange(desc(count)) #maybe combine make and nationality for the smaller brands of the cars


#know I want to keep:
# (vehYear, Purchase date, auction, make, model?, transmission, wheel type, Odometer
# size, byrno, vehbcost, warranty, cleanprice*2)
####################################################################################################
#extract year and month from purchase date

topmakes <- cars %>% group_by(Make) %>% summarize(count=n()) %>% 
  arrange(desc(count)) %>% do(head(., n=10)) %>% pull(Make)

MMRAcquisitionAuctionAveragePrice <- cars$MMRAcquisitionAuctionAveragePrice


cars <- cars %>%
  mutate(
    IsBadBuy = if_else(IsBadBuy == 0, "yes","no"),
    generalMake = if_else(Make %in% topmakes, Make, Nationality), # decreases number of makes by half
    MMRAcqAucAvg = if_else(grepl("[[:digit:]]", 
                                 MMRAcquisitionAuctionAveragePrice), 
                           as.numeric(MMRAcquisitionAuctionAveragePrice), 0),  # these lines clean up avg mmr prices
    MMRAcqRetAvg = if_else(grepl("[[:digit:]]", 
                              MMRAcquisitionRetailAveragePrice), 
                        as.numeric(MMRAcquisitionRetailAveragePrice), 0),
    MMRCurrAucAvg = if_else(grepl("[[:digit:]]", 
                               MMRCurrentAuctionAveragePrice), 
                         as.numeric(MMRCurrentAuctionAveragePrice), 0),
    MMRCurrRetAvg = if_else(grepl("[[:digit:]]", 
                               MMRCurrentRetailAveragePrice), 
                         as.numeric(MMRCurrentRetailAveragePrice), 0),
    Transmission = if_else(Transmission %in% c("MANUAL","AUTO", "Manual"), toupper(Transmission), "MANUAL")
  )

levels(cars$Transmission)

cars.cleaned  <- cars %>%
  mutate(
    PurchDate = as.numeric(stri_extract_last_regex(PurchDate, "\\d{4}")),
    IsBadBuy = as.factor(IsBadBuy),
    Auction = as.factor(Auction),
    Size = as.factor(Size),
    generalMake = as.factor(generalMake),
    Transmission = as.factor(Transmission)
  ) %>%
  select(RefId, IsBadBuy, VehYear, PurchDate, Auction, generalMake, Transmission, VehOdo,
         VehBCost, MMRAcqAucAvg, MMRAcqRetAvg, MMRCurrAucAvg, MMRCurrRetAvg,
         WarrantyCost, BYRNO) #left out size because needs to be cleaned


cars.cleaned <- one_hot(as.data.table(cars.cleaned))


cars.test <- cars.test %>%
  mutate(
    generalMake = if_else(Make %in% topmakes, Make, Nationality), # decreases number of makes by half
    MMRAcqAucAvg = if_else(grepl("[[:digit:]]", 
                                 MMRAcquisitionAuctionAveragePrice), 
                           as.numeric(MMRAcquisitionAuctionAveragePrice), 0),  # these lines clean up avg mmr prices
    MMRAcqRetAvg = if_else(grepl("[[:digit:]]", 
                                 MMRAcquisitionRetailAveragePrice), 
                           as.numeric(MMRAcquisitionRetailAveragePrice), 0),
    MMRCurrAucAvg = if_else(grepl("[[:digit:]]", 
                                  MMRCurrentAuctionAveragePrice), 
                            as.numeric(MMRCurrentAuctionAveragePrice), 0),
    MMRCurrRetAvg = if_else(grepl("[[:digit:]]", 
                                  MMRCurrentRetailAveragePrice), 
                            as.numeric(MMRCurrentRetailAveragePrice), 0),
    Transmission = if_else(Transmission %in% c("MANUAL","AUTO", "Manual"), toupper(Transmission), "MANUAL")
  )

cars.test.cleaned  <- cars.test %>%
  mutate(
    PurchDate = as.numeric(stri_extract_last_regex(PurchDate, "\\d{4}")),
    Auction = as.factor(Auction),
    Size = as.factor(Size),
    generalMake = as.factor(generalMake),
    Transmission = as.factor(Transmission)
  ) %>%
  select(RefId, VehYear, PurchDate, Auction, generalMake, Transmission, VehOdo,
         VehBCost, MMRAcqAucAvg, MMRAcqRetAvg, MMRCurrAucAvg, MMRCurrRetAvg,
         WarrantyCost, BYRNO) #left out size because needs to be cleaned

cars.test.cleaned <- one_hot(as.data.table(cars.test.cleaned))

write_csv(cars.test.cleaned, "pytest.csv")
write_csv(cars.cleaned, "pytrain.csv")

##################################################
############ Done Here ###########################
##################################################