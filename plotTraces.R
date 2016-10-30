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
  list(cap = cap, firstStim = firstStim, bothStim = bothStim)
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

#Normalize waves such that baseline is at zero
#Smooth baseline
#Normalize baseline to 0
#Normalize all other points to baseline
#capacitances already normalized

normalizeBaseline <- function(df) {
  #less than 0.081 is baseline
  baseline <- subset(df, df$sec < 0.081)
  smoothBaseline <- movingAvg(baseline, n = 5)
}

w17 <- firstStimIndexed$w17
baseline <- subset(w17, w17$sec < 0.081)
pA <- movingAvg(baseline$pA, n = 1000)
smoothBaseline <- as.data.frame(pA)
smoothBaseline$sec <- baseline$sec
smoothBaseline$id <- as.character(baseline$id)
subplot(plot_ly(baseline, x = ~sec, y = ~pA,
                split = ~id,
                type = "scatter", mode = "lines",
                line = list(color = "000000", width = 1)),
        plot_ly(smoothBaseline, x = ~sec, y = ~x,
                split = ~id,
                type = "scatter", mode = "lines",
                line = list(color = "000000", width = 1)))

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
                marker = list(size = 1)) %>%
    layout(hovermode = FALSE,
           title = "Stimulation intensity vs. Response amplitude") 
}

#Fxn to plot individual traces (for presentations)
plotIndivTraces <- function(df, info) {
  filtered <- dplyr::filter(df, notes %in% c(info))
  plot_ly(filtered, x = ~sec,
          y = ~pA,
          split = ~id,
          type = "scatter", mode = "lines",
          line = list(color = "000000", width = 1))
}

#Fxn to calculate moving average
movingAvg <- function(x, n) {
  stats::filter(x, rep(1/n, n), sides = 2)
}

#Fxn to smooth data
##MUST NORMALIZE TRACES TO ZERO
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

plotRawVsSmooth <- function(dfRawData, dfSmoothedData, info) {
  filteredRaw <- dplyr::filter(dfRawData, notes %in% c(info))
  filteredSmooth <- dplyr::filter(dfSmoothedData, notes %in% c(info))
  plot_ly(filteredRaw, x = ~sec,
          y = ~pA,
          split = ~id,
          type = "scatter", mode = "lines",
          line = list(color = "000000", width = 1, name = "Raw")) %>%
    add_plot(filteredSmooth, x = ~sec,
             y = ~pA,
             split = ~id,
             type = "scatter", mode = "lines",
             line = list(color = "FF0000", width = 1, name = "Raw Smooth"))
}

plotRawVsSmooth(dfFirstStim, dfFirstSmoothed, info = "SF")
plotIndivTraces(dfFirstSmoothed, info = "SF")

plot_ly(list$raw, x = ~sec,
        y = ~pA,
        split = ~id,
        type = "scatter", mode = "lines",
        line = list(color = "000000", width = 1, name = "Raw")) %>%
  add_lines(list$smooth, x = ~sec,
            y = ~pA,
            split = ~id,
            type = "scatter", mode = "lines",
            line = list(color = "FF0000", width = 1, name = "Raw Smooth"))


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

#Index lists by notes and stimInt in waveInfo
capIndexed <- purrr::map(capList, indexByInfo)
firstStimIndexed <- purrr::map(firstStimList, indexByInfo)
bothStimIndexed <- purrr::map(bothStimList, indexByInfo)

#Lists coerced to data frames
dfCap <- listToDataFrame(capIndexed)
dfFirstStim <- listToDataFrame(firstStimIndexed)
dfBothStim <- listToDataFrame(bothStimIndexed)


##Plots section##


#Plot all traces
plotAll(dfBothStim, title = "All traces 2016.10.26 p15 Cell 1") #Remember to change
plotly_POST(filename = "public-graph")

#Plot all capacitance traces
#plotAll(dfCap, title = "Capacitance traces")


##Generate traces of maximals and SFs to check cell
plotMaxAndSF(bothStimIndexed, colorCol = "Notes") %>%
  layout(title = "Maximals")
#plotly_POST(filename = "public-graph")

plotMaxAndSF(capIndexed, colorCol = "Notes") %>%
  layout(title = "Maximal and SF capacitances",
         hovermode = FALSE)
#plotly_POST(filename = "public-graph")


#Color traces by stimulation intensity
plotColors(firstStimIndexed, color = "stimInt")
install.packages("webshot")
export(plotColors(firstStimIndexed, color = "stimInt"), file = "./plot.pdf")
plotly_POST(format = "pdf", out_file = "./plot.pdf")
#plotly_POST(filename = "public-graph")
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

#Determine maximals from all traces
maximals <- findMaximals(dfFirstSmoothed)
#Store data across sessions... use R.cache library?
#update excel sheet based on maximals...?

SFs <- findSFs(dfFirstStim)
SFsmooth <- findSFs(dfFirstSmoothed)

#Calculate: PPR, AMPA/NMDA, cap trace, NMDA tau, fiber fraction

##Are stimulation intensity and response amplitude correlated, and if so, how much?
##Is this correlation different over development, between sexes, etc.?

#Plot amplitude vs. stimInt





##Look at inhibitory currents (before bicuculline addition)##
#not working as of 2016.10.28

indexedWavesNoBic <- indexAllWaves(rawData, waveInfoNoBic)
wavesSplitNoBic <- purrr::map(indexedWavesNoBic, splitIdentifyData)

capListNoBic <- extractWaves(wavesSplitNoBic, "cap")
bothStimListNoBic <- extractWaves(wavesSplitNoBic, "bothStim")

capNoBicIndexed <- purrr::map(capListNoBic, indexByInfo, waveInfoNoBic)
bothNoBicIndexed <- purrr::map(bothStimListNoBic, indexByInfo, waveInfoNoBic)

dfCapNoBic <- listToDataFrame(capNoBicIndexed)
dfBothNoBic <- listToDataFrame(bothNoBicIndexed)

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


#2016.10.26 Next steps:
#process traces to smooth out curves
#Calculate amplitudes
#how to store data after analysis

# R, G , B
# Each color has values between 0 and 255 --> 16M
# Specified in "Hexidecimal"
# this is 00 to FF in hexidecimal, 10 = A, 9 = 9, 27 = 1A
# All red is (R, G, B) = (255, 0, 0) = (FF, 0, 0) = FF0000
# AA9900 --> (R, G, B) = (AA, 99, 00) ; 99 = 9*16^1 + 9 * 16^0; 
