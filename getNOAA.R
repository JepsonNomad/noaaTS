# Load required pacakges

library(RCurl)
library(stringr)
library(tidyverse)

# Define parameters


#### Define function ----
## x is the name of the dataset. Can be:
# "NAO" for North Atlantic Oscillation
# "GBI" for Greenland Blocking Index
getNOAA = function(x = "NAO", dataFormat = "day", aggOpt = "none",
                   startDate = NULL, endDate = NULL, 
                   baseURL = "https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/"){
  # Lookup table for basename and correct extension
  extensionDF = data.frame(base = c("NAO"),
                           extn = c("nao.long.data"),
                           stringsAsFactors = FALSE)
  myExtn = extensionDF$extn[which(extensionDF$base == x)]
  completeURL = paste0(baseURL,myExtn)
  myfile <- getURL(completeURL)
  while(grepl(x = myfile, pattern = "  ")){
    myfile = gsub(myfile, pattern = "  ", replacement = " ")
  }
  
  # Format the data
  mydata <- read.csv(textConnection(myfile), header=F, sep=" ",
                     stringsAsFactors = FALSE)
  minYr = mydata[1,2]
  maxYr = mydata[1,3] 
  mydata = mydata[2:nrow(mydata),]
  names(mydata) <- c("Year",c(str_pad(as.character(1:12), 
                                      pad = "0", width = 2, side = "left")))
  # Remove rows that are not meaningful
  mydata = mydata %>% 
    filter(!is.na(as.numeric(Year)))
  
  # Convert to long format
  mydataLong = mydata %>%
    gather(key = "Month", value = "Value", -Year)
  mydataLong$Year = as.numeric(mydataLong$Year)
  
  if(aggOpt == "year"){
    # Aggregate by year
    rawvals = as.matrix(mydata[,-1])
    rawDF = apply(X = rawvals,
                  FUN = function(x){return(as.numeric(gsub(x, pattern = " ", replacement = "")))},
                  MARGIN = c(1,2))
    raw.year <- rowMeans(rawDF)
    mydata.year <- data.frame("Year" = c(minYr:maxYr),
                              "Value" = raw.year)
    mydata.year
    return(mydata.year)
  }else{return(mydata)}
}

NAO = getNOAA()
