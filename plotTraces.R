library(IgorR)
library(magrittr)
library(ggplot2)
library(openxlsx)
library(purrr)
library(plotly)
library(dplyr) 

##Functions to analyze ephys data##

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
  tau <- df[df[["sec"]] > 0.08 & df[["sec"]] < 0.58, ]
  list(cap = cap, firstStim = firstStim, bothStim = bothStim, tau = tau)
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
  normalized <- df
  return(normalized)
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

#Fxn to plot maximals, SFs
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
plotColors <- function(tracesStimInt, colorCol, by = 5) {
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

#Fxn to plot individual traces (for presentations)
#Generalize to other columns other than notes?
plotIndivTraces <- function(df, info) {
  filtered <- dplyr::filter(df, notes %in% info)
  plot_ly(filtered, x = ~sec,
          y = ~pA,
          split = ~id,
          type = "scatter", mode = "lines",
          line = list(color = "000000", width = 1))
}

#Fxns for analysis

#Fxn to calculate moving average
movingAvg <- function(x, n) {
  stats::filter(x, rep(1/n, n), sides = 2)
}

#Fxn to smooth data
#Normalize traces to zero
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
  filtered <- filter(df, notes %in% c("SF"))
  sf <- c()
  sf$AMPA <- min(filtered$pA, na.rm = TRUE)
  sf$NMDA <- max(filtered$pA, na.rm = TRUE)
  return(sf)
}

#Fxn to calculate paired pulse ratio
findPPR <- function(df) {
  
  #identify max AMPA trace automatically 
  maxTraceID <- df[which.min(df$pA), "id"]
  maxTrace <- subset(df, df$id %in% maxTraceID)
  
  #measure max AMPA amplitudes
  firstStim <- subset(maxTrace, maxTrace$sec > 0.08 & maxTrace$sec < 0.125)
  secondStim <- subset(maxTrace, maxTrace$sec > 0.136 & maxTrace$sec < 0.175)
  firstAMPA <- min(firstStim$pA)
  secondAMPA <- min(secondStim$pA)
  
  #calculate PPR
  PPR <- secondAMPA / firstAMPA
  return(PPR)
}

#Overlay raw vs. smoothed traces
#2016.10.30 Debugging
plotRawVsSmooth <- function(dfRawData, dfSmoothedData, info) {
  filteredRaw <- dplyr::filter(dfRawData, notes %in% info)
  filteredSmooth <- dplyr::filter(dfSmoothedData, notes %in% info)
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


##Process raw data##


#read data into R from Igor format
rawData <- read.pxp("2016.10.26_YW", ReturnTimeSeries = TRUE)

#Read in excel spreadsheet with wave numbers and stimulation intensities 
waveInfo <- read.xlsx("2016.10.26_Cell1.xlsx")
waveInfoNoBic <- read.xlsx("2016.10.26_noBic.xlsx")

#Select from rawData only those waves in the spreadsheet (i.e., index by spreadsheet)
indexedWaves <- indexAllWaves(rawData, waveInfo)

#use map fxn to iterate over each ts (wave) in indexedWaves to label data as
#capacitance trace, first stimulation or both stimulation
allWavesSplit <- purrr::map(indexedWaves, splitIdentifyData)

#Generate lists of cap, firstStim and bothStim traces indexed by wave name
capList <- extractWaves(allWavesSplit, "cap")
firstStimList <- extractWaves(allWavesSplit, "firstStim")
bothStimList <- extractWaves(allWavesSplit, "bothStim")
tauList <- extractWaves(allWavesSplit, "tau")

#Index lists by notes and stimInt in waveInfo
capIndexed <- purrr::map(capList, indexByInfo)
firstStimIndexed <- purrr::map(firstStimList, indexByInfo)
bothStimIndexed <- purrr::map(bothStimList, indexByInfo)
tauIndexed <- purrr::map(tauList, indexByInfo)

#Normalize traces to zero
firstStimNormalized <- purrr::map(firstStimIndexed, normalizeTraces)
bothStimNormalized <- purrr::map(bothStimIndexed, normalizeTraces)
tauNormalized <- purrr::map(tauIndexed, normalizeTraces)

#Lists coerced to data frames
dfCap <- listToDataFrame(capIndexed)
dfFirstStim <- listToDataFrame(firstStimNormalized)
dfBothStim <- listToDataFrame(bothStimNormalized)
dfTau <- listToDataFrame(tauNormalized)


##Plots section##


#Plot all traces
plotAll(dfBothStim, title = "All traces 2016.10.26 p15 Cell 1") #Remember to change
plotly_POST(filename = "public-graph")
#plotAll(dfCap, title = "Capacitance traces")

##Generate traces of maximals and SFs to check cell
plotMaxAndSF(bothStimNormalized, colorCol = "Notes") %>%
  layout(title = "Maximals")
#plotly_POST(filename = "public-graph")

plotMaxAndSF(capIndexed, colorCol = "Notes") %>%
  layout(title = "Maximal and SF capacitances",
         hovermode = FALSE)
#plotly_POST(filename = "public-graph")

#Color traces by stimulation intensity
plotColors(firstStimNormalized, color = "stimInt")
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

#Parameters to measure refinement at retinogeniculate synapses
maximals <- findMaximals(dfFirstSmoothed)
SFs <- findSFs(dfFirstSmoothed)

ANRatio <- maximals$AMPA / maximals$NMDA
PPR <- findPPR(dfBothSmoothed)

tau <- subset(dfTau, dfTau$notes %in% "NMDA tau")
plotIndivTraces(dfTau, "NMDA tau")

FFampa <- SFs$AMPA / maximals$AMPA
FFnmda <- SFs$NMDA / maximals$NMDA
FFcell <- (FFampa + FFnmda)/2


#Calculate: cap trace, NMDA tau

##Are stimulation intensity and response amplitude correlated, and if so, how much?
##Is this correlation different over development, between sexes, etc.?

#Plot amplitude vs. stimInt
#Must plot max and min vs. stimInt
#Must also corr to cap trace
allNMDA <- purrr::map(dfFirstStim$pA, max)





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


#2016.11.01 Next steps:
#curve fitting
#how to store data after analysis

# R, G , B
# Each color has values between 0 and 255 --> 16M
# Specified in "Hexidecimal"
# this is 00 to FF in hexidecimal, 10 = A, 9 = 9, 27 = 1A
# All red is (R, G, B) = (255, 0, 0) = (FF, 0, 0) = FF0000
# AA9900 --> (R, G, B) = (AA, 99, 00) ; 99 = 9*16^1 + 9 * 16^0; 
