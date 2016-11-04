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
  cap <- df[df[["sec"]] < 0.02, ]
  firstStim <- df[df[["sec"]] > 0.08 & df[["sec"]] < 0.125, ]
  bothStim <- df[df[["sec"]] > 0.08 & df[["sec"]] < 0.175, ]
  tauTrace <- df[df[["sec"]] > 0.08 & df[["sec"]] < 0.58, ]
  list(cap = cap, firstStim = firstStim, bothStim = bothStim, tauTrace = tauTrace)
}

#Function to extract cap, firstStim, bothStim into 3 indiv lists
extractWaves <- function(theList, traceName) {
  waves <- purrr::map(theList, function(x) get(traceName, x)) 
  return(waves)
}

#Fxn to index data by notes and stimulation intensity columns in waveInfo
indexByInfo <- function(tracesList, excel = waveInfo) {
  notes <- excel[ , c("waveName", "Notes")]
  df1 <- merge(tracesList, notes, by.x = "id", by.y = "waveName")
  stim <- excel[ , c("waveName", "stimInt")]
  df2 <- merge(df1, stim, by.x = "id", by.y = "waveName")
  return(df2)
}

#Normalize traces to zero (first and bothStim lists only)
normalizeTraces <- function(df) {
  baseline <- subset(df, df$sec < 0.081) #subset to select only sec < 0.081
  avgBaseline <- mean(baseline$pA) #average baseline
  df$pA <- df$pA - avgBaseline #subtract average baseline from entire trace
  return(df)
}

#Fxn to coerce large lists to dfs
listToDataFrame <- function(tracesList) {
  df_all <- data.frame(
    sec = unlist(lapply(tracesList, "[[", "sec")),
    pA = unlist(lapply(tracesList, "[[", "pA")),
    id = as.character(unlist(lapply(tracesList, "[[", "id"))),
    notes = unlist(lapply(tracesList, "[[", "Notes")),
    stimInt = unlist(lapply(tracesList, "[[", "stimInt"))
  )
  df_all$id <- as.character(df_all$id)
  df_all$notes <- as.character(df_all$notes)
  df <- subset(df_all, df_all$sec < 0.081 | df_all$sec > 0.0845 &
                 df_all$sec < 0.131 | df_all$sec > 0.1325) #removes both stimulus artifacts
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

#Fxn to plot maximals, SFs to check cell
#Keep stimulus artifacts in this function
plotMaxAndSF <- function(tracesIndexed, colorCol) {
  df <- data.frame(
    sec = unlist(lapply(tracesIndexed, "[[", "sec")),
    pA = unlist(lapply(tracesIndexed, "[[", "pA")),
    id = unlist(lapply(tracesIndexed, "[[", "id")),
    color = unlist(lapply(tracesIndexed, "[[", colorCol))
  )
  filtered <- dplyr::filter(df, color %in% c("max", "lastMax", "SF"))
  plot_ly(filtered, x = ~sec, y = ~pA, split = ~id, line = list(width = 1)) %>%
    add_lines(color = ~color)
}

#Fxn to color each trace by stimulation intensity
#Pull out just low intensity responses (<40) to compare
plotColors <- function(tracesStimInt, colorCol = "stimInt", by = 5) {
  df_all <- data.frame(
    sec = unlist(lapply(tracesStimInt, "[[", "sec")),
    pA = unlist(lapply(tracesStimInt, "[[", "pA")),
    id = unlist(lapply(tracesStimInt, "[[", "id")),
    color = unlist(lapply(tracesStimInt, "[[", colorCol))
  )
  dfNoArtifact <- subset(df_all, df_all$sec < 0.081 | df_all$sec > 0.0845) #remove stimulus artifact
  df <- dfNoArtifact[seq(1, nrow(df_all), by = by), ] #keeps every 5th data point
  plot_ly(df, x = ~sec, y = ~pA) %>%
    add_markers(color = ~log(color),
                marker = list(size = 3)) %>%
    layout(hovermode = FALSE,
           title = "Stimulation intensity vs. Response amplitude") 
}

#Fxn to plot individual traces
#E.g.: colName = "id", info = c("w10", "w11") to plot waves 10 and 11
plotIndivTraces <- function(df, colName, info) {
  infoToPlot <- df[, colName] %in% info
  filtered <- dplyr::filter(df, infoToPlot)
  plot_ly(filtered, x = ~sec,
          y = ~pA,
          split = ~id,
          type = "scatter", mode = "lines",
          line = list(color = "000000", width = 1))
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
  firstStim <- subset(maxTrace, maxTrace$sec > 0.08 & maxTrace$sec < 0.125)
  secondStim <- subset(maxTrace, maxTrace$sec > 0.136 & maxTrace$sec < 0.175)
  firstAMPA <- min(firstStim$pA)
  secondAMPA <- min(secondStim$pA)
  
  #calculate PPR
  PPR <- secondAMPA / firstAMPA
  return(PPR)
}

#Fxn to calculate NMDA decay constant
findTau <- function(df) {
  tauTrace <- subset(df, df$notes == "NMDA tau")
  curveToFit <- subset(tauTrace, tauTrace$sec > 0.115 & tauTrace$sec < 0.54)
  regModel <- lm(log(curveToFit$pA) ~ curveToFit$sec) #Beware of generating NaN as NMDA response decays to zero
  regModelCoef <- coef(summary(regModel))["curveToFit$sec", "Estimate"]
  tau <- abs(1 / regModelCoef)
  
  #lm plots to look at residuals
  plot(regModel)
  
  return(tau)
}


##Process raw data##


#read data into R from Igor format
rawData <- read.pxp("2016.10.26_YW", ReturnTimeSeries = TRUE)

#Read in excel spreadsheet with wave numbers and stimulation intensities 
waveInfo <- read.xlsx("2016.10.26_Cell1.xlsx")
waveInfoNoBic <- read.xlsx("2016.10.26_noBic.xlsx")

#Select from rawData only those waves in the spreadsheet (i.e., index by spreadsheet)
indexedWaves <- indexAllWaves(rawData, waveInfo)

#use map fxn to iterate over each ts (wave) in indexedWaves to label data as
#capacitance trace, first stimulation, both stimulation or tau trace
allWavesSplit <- purrr::map(indexedWaves, splitIdentifyData)

#Generate lists of cap, firstStim and bothStim traces indexed by wave name
capList <- extractWaves(allWavesSplit, "cap")
firstStimList <- extractWaves(allWavesSplit, "firstStim")
bothStimList <- extractWaves(allWavesSplit, "bothStim")
tauTraceList <- extractWaves(allWavesSplit, "tauTrace")

#Index lists by notes and stimInt in waveInfo
capIndexed <- purrr::map(capList, indexByInfo)
firstStimIndexed <- purrr::map(firstStimList, indexByInfo)
bothStimIndexed <- purrr::map(bothStimList, indexByInfo)
tauTraceIndexed <- purrr::map(tauTraceList, indexByInfo)

#Normalize traces to zero
firstStimNormalized <- purrr::map(firstStimIndexed, normalizeTraces)
bothStimNormalized <- purrr::map(bothStimIndexed, normalizeTraces)
tauTraceNormalized <- purrr::map(tauTraceIndexed, normalizeTraces)

#Lists coerced to data frames
dfCap <- listToDataFrame(capIndexed)
dfFirstStim <- listToDataFrame(firstStimNormalized)
dfBothStim <- listToDataFrame(bothStimNormalized)
dfTauTrace <- listToDataFrame(tauTraceNormalized)


##Determine if cell can be analyzed##


#Cap trace
#2016.11.03: Is end time always the same? 
#Must set a threshold
findCapDecay <- function(df) {
  curveStart <- df[which.min(df$pA), "sec"]
  curveToFit <- subset(df, df$sec > curveStart & df$sec < 0.011)
  regModel <- lm(log(abs(curveToFit$pA)) ~ curveToFit$sec)
  regModelCoef <- coef(summary(regModel))["curveToFit$sec", "Estimate"]
  expRiseTime <- abs(1 / regModelCoef)
  return(expRiseTime)
}

capExpRise <- purrr::map(capIndexed, findCapDecay)


#Calculate leak currents
findLeakCurrents <- function(df) {
  baseline <- subset(df, df$sec < 0.081) 
  avgBaseline <- mean(baseline$pA) 

  #identify and store non-leaky traces
  if (abs(avgBaseline) < 100) { #change threshold to 600 after debugging
    waves <- df
  } else {
    return(avgBaseline)
  }
}

leak <- purrr::map(firstStimIndexed, findLeakCurrents)

#removes traces where leak > threshold
#incorporate into fxn directly?
leak2 <- leak[purrr::map_lgl(leak, function(x) class(x) != "numeric")]


##Plots section##


#Plot all traces
plotAll(dfBothStim, title = "All traces 2016.10.26 p15 Cell 1") #Remember to change
#plotly_POST(filename = "public-graph")
#plotAll(dfCap, title = "Capacitance traces")

#Generate traces of maximals and SFs to check cell
#Stimulation artifacts included
plotMaxAndSF(bothStimNormalized, colorCol = "Notes") %>%
  layout(title = "Maximals and SFs")
#plotly_POST(filename = "public-graph")

plotMaxAndSF(capIndexed, colorCol = "Notes") %>%
  layout(title = "Maximal and SF capacitances",
         hovermode = FALSE)
#plotly_POST(filename = "public-graph")

plotIndivTraces(dfFirstStim, colName = "notes", info = "SF") %>%
  layout(title = "SFs")

#Color traces by stimulation intensity
plotColors(firstStimNormalized, colorCol = "stimInt")
plotly_POST(filename = "public-graph")
#plotColors(capIndexed, color = "stimInt", by = 1) 


##Analyze traces##


#Smooth data
dfFirstSmoothed <- smoothTraces(dfFirstStim)
dfBothSmoothed <- smoothTraces(dfBothStim)

#Compare all smoothed vs. raw traces
subplot(
  plotAll(dfFirstSmoothed),
  plotAll(dfFirstStim),
  shareX = TRUE,
  shareY = TRUE
)

#Store data across sessions... use R.cache library?
#update waveInfo based on maximals...?

#Standard parameters to measure refinement at retinogeniculate synapses
maximals <- findMaximals(dfFirstSmoothed)
SFs <- findSFs(dfFirstSmoothed)

ANRatio <- maximals$AMPA / maximals$NMDA
PPR <- findPPR(dfBothSmoothed)

tau <- findTau(dfTauTrace)
plotIndivTraces(dfTauTrace, colName = "notes", info = "NMDA tau")

FFampa <- SFs$AMPA / maximals$AMPA
FFnmda <- SFs$NMDA / maximals$NMDA
FFcell <- (FFampa + FFnmda)/2


##Are stimulation intensity and response amplitude correlated, and if so, how much?
##Is this correlation different over development, between sexes, etc.?

#Plot (max) amplitude vs. stimInt
#Must plot max and min vs. stimInt
#Must also corr to cap trace
allNMDA <- purrr::map(dfFirstStim$pA, max)

#also can look at when large 2nd currents appear; corr with dev?
#when AMPA2 larger than AMPA1 (and for NMDAs too)
#Also when does 2nd stimulus not appear?



##Look at inhibitory currents (before bicuculline addition)##


indexedWavesNoBic <- indexAllWaves(rawData, waveInfoNoBic)
wavesSplitNoBic <- purrr::map(indexedWavesNoBic, splitIdentifyData)

capListNoBic <- extractWaves(wavesSplitNoBic, "cap")
bothStimListNoBic <- extractWaves(wavesSplitNoBic, "bothStim")

capNoBicIndexed <- purrr::map(capListNoBic, indexByInfo, waveInfoNoBic)
bothNoBicIndexed <- purrr::map(bothStimListNoBic, indexByInfo, waveInfoNoBic)

bothNoBicNormalized <- purrr::map(bothNoBicIndexed, normalizeTraces)

dfCapNoBic <- listToDataFrame(capNoBicIndexed)
dfBothNoBic <- listToDataFrame(bothNoBicNormalized)

plotAll(dfBothNoBic) %>%
  layout(showlegend = FALSE,
         xaxis = list(title = "sec"),
         yaxis = list(title = "pA"),
         title = "Before bicuculline addition")

plotAll(dfCapNoBic) %>%
  layout(showlegend = FALSE,
         xaxis = list(title = "sec"),
         yaxis = list(title = "pA"),
         title = "Capacitance traces, before bicuculline")


#2016.11.03 Next steps:
#Finalize function to pull out leaky currents
#how to store data after analysis
#Plot colors will have to be done with low and high stimInt separately
#convert plotly to ggplot
#
# 1. Must put old data into excel sheets
# 2. made changes on 11.03 to excel sheets, standardize earlier excel sheets
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
