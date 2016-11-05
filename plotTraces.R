library(IgorR)
library(magrittr)
library(ggplot2)
library(openxlsx)
library(purrr)
library(plotly)
library(dplyr) 


##Functions to analyze ephys data##


#Fxns to process data


#time series to data frame
tsToDataframe <- function(ts) {
  df <- data.frame(pA = c(ts), sec = c(time(ts)))
  return(df)
}

#Function to index all waves
indexAllWaves <- function(data, excelSheet) {
  index <- excelSheet[, "waveName"]
  allWaves <- data[index]
  return(allWaves)
}

#Fxn to output data based on 3 time intervals (cap trace, 1st stim, 1st & 2nd stim), store in list
#Also converts time series (ts) to data frame
splitIdentifyData <- function(ts) {
  df <- data.frame(pA = c(ts), sec = c(time(ts)), id = attr(ts, "WaveHeader")$WaveName)
  df$id <- as.character(df$id)
  cap <- df[df[["sec"]] < timeEndCapTrace, ]
  firstStim <- df[df[["sec"]] > timeBeginFirstStim & df[["sec"]] < timeEndFirstStim, ]
  bothStim <- df[df[["sec"]] > timeBeginFirstStim & df[["sec"]] < timeEnd2Stim, ]
  tauTrace <- df[df[["sec"]] > timeBeginFirstStim & df[["sec"]] < timeEndTauTrace, ]
  list(cap = cap, firstStim = firstStim, bothStim = bothStim, tauTrace = tauTrace)
}

#Function to extract cap, firstStim, bothStim into 3 indiv lists
extractWaves <- function(theList, traceName) {
  waves <- purrr::map(theList, function(x) get(traceName, x)) 
  return(waves)
}

#Fxn to index data by notes and stimulation intensity columns in waveInfo
indexByInfo <- function(tracesList, excel = waveInfo) {
  notes <- excel[ , c("waveName", "notes")]
  df1 <- merge(tracesList, notes, by.x = "id", by.y = "waveName")
  stim <- excel[ , c("waveName", "stimInt")]
  df2 <- merge(df1, stim, by.x = "id", by.y = "waveName")
  waveNum <- excel[ , c("waveNum", "waveName")]
  df3 <- merge(df2, waveNum, by.x = "id", by.y = "waveName")
  return(df3)
}

#Normalize traces to zero (first and bothStim lists only)
normalizeTraces <- function(df) {
  baseline <- subset(df, df$sec < timeBeginFirstArtifact) #subset to select only baseline
  avgBaseline <- mean(baseline$pA) #average baseline
  df$pA <- df$pA - avgBaseline #subtract average baseline from entire trace
  return(df)
}

#Calculate leak currents
findLeakCurrents <- function(df, leakThreshold = 600) {
  baseline <- subset(df, df$sec < timeBeginFirstArtifact) 
  avgBaseline <- mean(baseline$pA) 
  return(abs(avgBaseline) < leakThreshold)
}

#Extract and index in one fxn
extractAndIndex <- function(allWavesSplit, traceToExtract) {
  extractedList <- extractWaves(allWavesSplit, traceToExtract)
  indexedList <- purrr::map(extractedList, indexByInfo)
}

#Select only healthy cells and normalize
generateFinalList <- function(indexedList, healthyCellsIndex) { 
  healthyCells <- indexedList[healthyCellsIndex]
  normalizedList <- purrr::map(healthyCells, normalizeTraces)
  return(normalizedList)
}

#Fxn to coerce large lists to dfs
listToDataFrame <- function(tracesList) {
  df_all <- data.frame(
    waveNum = unlist(lapply(tracesList, "[[", "waveNum")),
    id = as.character(unlist(lapply(tracesList, "[[", "id"))),
    sec = unlist(lapply(tracesList, "[[", "sec")),
    pA = unlist(lapply(tracesList, "[[", "pA")),
    stimInt = unlist(lapply(tracesList, "[[", "stimInt")),
    notes = unlist(lapply(tracesList, "[[", "notes"))
  )
  df_all$id <- as.character(df_all$id)
  df_all$notes <- as.character(df_all$notes)
  #remove both stimulus artifacts
  df <- subset(df_all, df_all$sec < timeBeginFirstArtifact | df_all$sec > timeEndFirstArtifact &
                 df_all$sec < timeBegin2Artifact | df_all$sec > timeEnd2Artifact) 
  return(df)
}


#Fxns for plotting


#Fxn to plot all traces, equivalent of show1 in Igor
plotAll <- function(df, title = "All traces") {
  plot_ly(df, x = ~sec, y = ~pA,
          split = ~id,
          type = "scatter", mode = "lines",
          line = list(color = "000000", width = 1)) %>%
    layout(showlegend = FALSE,
           xaxis = list(title = "sec"),
           yaxis = list(title = "pA"),
           hovermode = FALSE,
           title = title)
}

#Fxn to plot individual traces
plotIndivTraces <- function(df, colName, info, color = "000000") {
  infoToPlot <- df[, colName] %in% info
  filtered <- dplyr::filter(df, infoToPlot)
  plot_ly(filtered, x = ~sec,
          y = ~pA,
          split = ~id,
          type = "scatter", mode = "lines",
          line = list(color = color, width = 1))
}

#Fxn to color each trace by stimulation intensity
#Pull out just low intensity responses (<40) to compare
plotColors <- function(tracesStimInt, colorCol = "stimInt", by = 5) {
  df_all <- data.frame(
    sec = unlist(lapply(tracesStimInt, "[[", "sec")),
    pA = unlist(lapply(tracesStimInt, "[[", "pA")),
    id = unlist(lapply(tracesStimInt, "[[", "id")),
    waveNum = unlist(lapply(tracesStimInt, "[[", "waveNum")),
    color = unlist(lapply(tracesStimInt, "[[", colorCol))
  )
  dfNoArtifact <- subset(df_all, df_all$sec < timeBeginFirstStim | df_all$sec > timeEndFirstArtifact) #remove stimulus artifact
  df <- dfNoArtifact[seq(1, nrow(df_all), by = by), ] #keeps every 5th data point
  plot_ly(df, x = ~sec, y = ~pA) %>%
    add_markers(color = ~log(color),
                marker = list(size = 3)) %>%
    layout(hovermode = FALSE) 
}

#Overlay raw vs. smoothed traces
#2016.10.30 Debugging
plotRawVsSmooth <- function(dfRawData, dfSmoothedData, info) {
  filteredRaw <- dplyr::filter(dfRawData, notes == info)
  filteredSmooth <- dplyr::filter(dfSmoothedData, notes == info)
  plot_ly(filteredRaw, x = ~sec,
          y = ~pA,
          split = ~id,
          type = "scatter", mode = "lines",
          line = list(color = "000000", width = 1, name = "Raw")) %>%
    add_lines(filteredSmooth, x = ~sec,
              y = ~pA,
              split = ~id,
              type = "scatter", mode = "lines",
              line = list(color = "FF0000", width = 1, name = "Raw Smooth"))
}

plotRawVsSmooth(dfFirstStim, dfFirstSmoothed, info = "SF")


#Fxns for analysis


#Fxn to calculate moving average
movingAvg <- function(x, n) {
  stats::filter(x, rep(1/n, n), sides = 2)
}

#Fxn to smooth data
smoothTraces <- function(df) {
  pA <- ifelse (df$pA < 0, 
                movingAvg(df$pA, n = 5),
                movingAvg(df$pA, n = 100)
  )
  dfSmoothed <- as.data.frame(pA)
  dfSmoothed$sec <- df$sec
  dfSmoothed$id <- as.character(df$id)
  dfSmoothed$notes <- as.character(df$notes)
  dfSmoothed$stimInt <- df$stimInt
  return(dfSmoothed)
}

#Fxn to find maximals 
findMaximals <- function(df) {
  max <- c()
  max$AMPA <- min(df$pA, na.rm = TRUE)
  max$NMDA <- max(df$pA, na.rm = TRUE)
  return(max)
}

#Fxn to find single fibers (SFs)
findSFs <- function(df) {
  filtered <- filter(df, notes == "SF")
  sf <- c()
  sf$AMPA <- min(filtered$pA, na.rm = TRUE)
  sf$NMDA <- max(filtered$pA, na.rm = TRUE)
  return(sf)
}

#Fxn to calculate paired pulse ratio
findPPR <- function(df) {
  
  #identify max AMPA trace automatically 
  maxTraceID <- df[which.min(df$pA), "id"]
  maxTrace <- subset(df, df$id == maxTraceID)
  
  #measure max AMPA amplitudes
  firstStim <- subset(maxTrace, maxTrace$sec > timeBeginFirstStim & maxTrace$sec < timeEndFirstStim)
  secondStim <- subset(maxTrace, maxTrace$sec > timeBegin2Stim & maxTrace$sec < timeEnd2Stim)
  firstAMPA <- min(firstStim$pA)
  secondAMPA <- min(secondStim$pA)
  
  #calculate PPR
  PPR <- secondAMPA / firstAMPA
  return(PPR)
}

#Fxn to calculate NMDA decay constant
findTau <- function(df, timeBeginTau = 0.115, timeEndTau = 0.54) {
  tauTrace <- subset(df, df$notes == "NMDA tau")
  curveToFit <- subset(tauTrace, tauTrace$sec > timeBeginTau & tauTrace$sec < timeEndTau)
  regModel <- lm(log(curveToFit$pA) ~ curveToFit$sec) #Beware of generating NaN as NMDA response decays to zero
  regModelCoef <- coef(summary(regModel))["curveToFit$sec", "Estimate"]
  tau <- abs(1 / regModelCoef)
  
  #lm plots to look at residuals
  plot(regModel)
  
  return(tau)
}


##Set time intervals for functions##


#Time intervals to subset data
timeEndCapTrace <- 0.02
timeBeginFirstStim <- 0.08
timeEndFirstStim <- 0.125
timeBegin2Stim <- 0.136
timeEnd2Stim <- 0.175
timeEndTauTrace <- 0.58

#Time intervals for stimulation artifacts
timeBeginFirstArtifact <- 0.081
timeEndFirstArtifact <- 0.0845
timeBegin2Artifact <- 0.131
timeEnd2Artifact <- 0.1325


##Process raw data##


#read data into R from Igor format
rawData <- read.pxp("2016.10.26_YW", ReturnTimeSeries = TRUE)

date <- "2016.10.26"
cellNum <- 1
age <- "p15"
waveInfo <- read.xlsx("2016.10.26_Cell1.xlsx")

#Select from rawData only those waves in the spreadsheet (i.e., index by spreadsheet)
indexedWaves <- indexAllWaves(rawData, waveInfo)

#use map fxn to iterate over each ts (wave) in indexedWaves to label data as
#capacitance trace, first stimulation, both stimulation or tau trace
allWavesSplit <- purrr::map(indexedWaves, splitIdentifyData)

#Generate index of healthy cells
healthyCellsIndex <- extractAndIndex(allWavesSplit = allWavesSplit, "firstStim") %>%
  purrr::map_lgl(findLeakCurrents)

#Generate lists of traces
listCap <- extractAndIndex(allWavesSplit = allWavesSplit, "cap") %>%
  generateFinalList(healthyCellsIndex)

listFirstStim <- extractAndIndex(allWavesSplit = allWavesSplit, "firstStim") %>%
  generateFinalList(healthyCellsIndex)

listBothStim <- extractAndIndex(allWavesSplit = allWavesSplit, "bothStim") %>%
  generateFinalList(healthyCellsIndex)

listTauTrace <- extractAndIndex(allWavesSplit = allWavesSplit, "tauTrace") %>%
  generateFinalList(healthyCellsIndex)

#Stack lists to data frames

#All capacitance traces including those with inhibitory currents
dfCap <- listToDataFrame(listCap)

#First and both stimulus traces, no inhibitory currents
dfFirstStim <- listToDataFrame(listFirstStim)
dfFirstStim <- subset(dfFirstStim, dfFirstStim$notes != "noBic")

dfBothAll <- listToDataFrame(listBothStim)
dfBothStim <- subset(dfBothAll, dfBothAll$notes != "noBic")

#Traces with inhibitory currents (before bicuculline)
dfInhTraces <- listToDataFrame(listBothStim) %>%
  subset(dfBothAll$notes == "noBic")

#Trace to calculate NMDA tau
dfTauTrace <- listToDataFrame(listTauTrace)
dfTauTrace <- subset(dfTauTrace, dfTauTrace$notes == "NMDA tau")


#Cap trace
#2016.11.04 still testing
#Must set a threshold, if expRiseTime > threshold == unhealthy
findCapDecay <- function(df, timeEndCapCurve = 0.011) {
  curveStart <- df[which.min(df$pA), "sec"]
  curveToFit <- subset(df, df$sec > curveStart & df$sec < timeEndCapCurve)
  regModel <- lm(log(abs(curveToFit$pA)) ~ curveToFit$sec)
  regModelCoef <- coef(summary(regModel))["curveToFit$sec", "Estimate"]
  expRiseTime <- abs(1 / regModelCoef)
  return(expRiseTime)
}

#2016.11.04: still testing
capExpRise <- purrr::map(listCap, findCapDecay)


##Plots section##


#Plot all traces
plotAll(dfBothStim, title = paste("All traces", date, "Cell", cellNum, age))
#plotly_POST(filename = "public-graph")
#plotAll(dfCap, title = "Capacitance traces")

#Generate traces of maximals and SFs to check cell
plotIndivTraces(dfBothStim, colName = "notes",
                info = c("max", "lastMax", "SF"), color = "notes") %>%
  layout(title = paste("Maximals and SFs", age),
         hovermode = FALSE)

#Capacitance traces of maximals and SFs
plotIndivTraces(dfCap, colName = "notes",
                info = c("max", "lastMax", "SF"), color = "notes") %>%
  layout(title = paste("Maximals and SFs", age),
         hovermode = FALSE)

#Plot only SF
plotIndivTraces(dfFirstStim, colName = "notes", info = "SF") %>%
  layout(title = paste("SFs", age))
#plotly_POST(filename = "public-graph")

#Color traces by stimulation intensity
plotColors(listFirstStim, colorCol = "stimInt") %>%
  layout(title = paste("Stimulation intensity vs. Response amplitude", age))
#plotly_POST(filename = "public-graph")

plotIndivTraces(dfTauTrace, colName = "notes", info = "NMDA tau")

#Traces with inhibitory currents (before bicuculline addition)
plotAll(dfInhTraces) %>%
  layout(title = paste("Before bicuculline addition", age))


##Analyze traces##


#Smooth data
dfFirstSmoothed <- smoothTraces(dfFirstStim)
dfBothSmoothed <- smoothTraces(dfBothStim)

#Compare all smoothed vs. raw traces
#Replace with overlaid traces
subplot(
  plotAll(dfFirstSmoothed),
  plotAll(dfFirstStim),
  shareX = TRUE,
  shareY = TRUE
)

#Standard parameters to measure refinement at retinogeniculate synapses
#Store all parameters for each cell in .csv file or MySQL
#Match dates with mouseInfo
parameters <- c()

parameters$date <- date
parameters$cell <- cellNum

parameters$maximals <- findMaximals(dfFirstSmoothed)
parameters$SFs <- findSFs(dfFirstSmoothed)

parameters$ANRatio <- abs(maximals$AMPA / maximals$NMDA)
parameters$PPR <- findPPR(dfBothSmoothed)

parameters$tau <- findTau(dfTauTrace)

parameters$FFampa <- SFs$AMPA / maximals$AMPA
parameters$FFnmda <- SFs$NMDA / maximals$NMDA
parameters$FFcell <- (FFampa + FFnmda)/2

write.csv(parameters, file = "SummaryInformation.csv")

##Are stimulation intensity and response amplitude correlated, and if so, how much?
##Is this correlation different over development, between sexes, etc.?

#Plot (max) amplitude vs. stimInt
#Must plot max and min vs. stimInt
#Must also corr to cap trace
#In progress as of 2016.11.03
allNMDA <- purrr::map(dfFirstStim$pA, max)

#also can look at when large 2nd currents appear; corr with dev?
#when AMPA2 larger than AMPA1 (and for NMDAs too)
#Also when does 2nd stimulus not appear?





#2016.11.04 Next steps:
#Finalize function to pull out leaky currents
#how to store data after analysis...play with MySQL
#Plot colors will have to be done with low and high stimInt separately
#convert plotly to ggplot
#
# 1. Must put old data into excel sheets
# 
#Reliability of response (low stimInt)
# For low stimInt: define AMPA/NMDA max in time interval
# plot A vs. N, analyze
# plot A and N vs. stimInt, analyze
# Trying to capture how the response comes in and out, esp at younger ages
# 
#relative development of AMPA vs NMDA
# 5. Also measure when is AMPA component of pos response (at +40) present? 
# lower stimInt and younger ages you see AMPA less often

# R, G , B
# Each color has values between 0 and 255 --> 16M
# Specified in "Hexidecimal"
# this is 00 to FF in hexidecimal, 10 = A, 9 = 9, 27 = 1A
# All red is (R, G, B) = (255, 0, 0) = (FF, 0, 0) = FF0000
# AA9900 --> (R, G, B) = (AA, 99, 00) ; 99 = 9*16^1 + 9 * 16^0; 
