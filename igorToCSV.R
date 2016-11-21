library(IgorR)
library(purrr)
library(magrittr)

##Convert raw Igor file to csv for python

#Fxn to annotate data
toSplitData <- function(ts) {
  df <- data.frame(pA = c(ts), sec = c(time(ts)), id = attr(ts, "WaveHeader")$WaveName)
  df$id <- as.character(df$id)
  return(df)
}

#Fxn to stack list
listToDataFrame <- function(tracesList) {
  df <- data.frame(
    id = unlist(lapply(tracesList, "[[", "id")),
    sec = unlist(lapply(tracesList, "[[", "sec")),
    pA = unlist(lapply(tracesList, "[[", "pA"))
  )
  df$id <- as.character(df$id)
  return(df)
}


#convert igor file to list of ts
rawData <- read.pxp("2016.10.26_YW", regex = "w.", ReturnTimeSeries = TRUE)
#remove vars sublist
rawData <- rawData[-1]

#create data frame with sec, pA, id
dfWaves <- purrr::map(rawData, toSplitData) %>%
  listToDataFrame()

write.csv(dfWaves, file = "igorToCSV.csv")