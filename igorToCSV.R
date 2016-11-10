library(IgorR)
library(purrr)

##Convert raw Igor file to csv for python

#convert igor file to ts
rawData <- read.pxp("2016.10.26_YW", regex = "w.", ReturnTimeSeries = TRUE)
rawWaves <- rawData[-1]

#annotate data
toSplitData <- function(ts) {
  df <- data.frame(pA = c(ts), sec = c(time(ts)), id = attr(ts, "WaveHeader")$WaveName)
  df$id <- as.character(df$id)
  return(df)
}

splitWaves <- purrr::map(rawWaves, toSplitData)

#stack list
listToDataFrame <- function(tracesList) {
  df <- data.frame(
    id = unlist(lapply(tracesList, "[[", "id")),
    sec = unlist(lapply(tracesList, "[[", "sec")),
    pA = unlist(lapply(tracesList, "[[", "pA"))
  )
  df$id <- as.character(df$id)
  return(df)
}

dfWaves <- listToDataFrame(splitWaves)

write.csv(dfWaves, file = "igorForPython.csv")