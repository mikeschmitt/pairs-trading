library(tseries)
library(sos)
library(fractal)
library(RCurl)
library(stringr)
library(quantmod)

#CLEAR ALL OBJECTS
rm(list=ls())

#GLOBAL VARIABLES
output.filename <- "C:/Users/Mike/Documents/R/STOCK_COINTEGRATION_RESULTS.csv"

#FUNCTIONS
PullIndustryURLs <- function() {
  l <- list()
  for (i in 100:999) {
    URL <- paste("http://biz.yahoo.com/p/", i, "conameu.html", sep="")
    if (url.exists(URL) == TRUE)
      l <- c(l, URL)
  }
  return(l)
}

PullIndustryList <- function(URL) {
  if (url.exists(URL) == TRUE) {
    lines <- readLines(URL)
    search.pattern <- '&d=t">([^<]*)</a>'
    data.lines <- grep(search.pattern, lines, value=TRUE)
    stock.list <- substring(str_extract(data.lines, '&d=t">[^<]*'), 7)
    
    #DON'T WANT STOCKS WITH PERIOD IN TICKER
    stock.list <- stock.list[grepl("\\.", stock.list) == FALSE]
  } else {
    stock.list <- list()
  }
  return(stock.list)
}

PullStockHistory <- function(stock.ticker, start.date=Sys.Date()-365, end.date=Sys.Date()) {

  URL <- paste("http://ichart.finance.yahoo.com/table.csv?s=", stock.ticker
               , "&a=", format(start.date, "%m")
               , "&b=", format(start.date, "%d")
               , "&c=", format(start.date, "%Y")
               , "&d=", format(end.date, "%m")
               , "&e=", format(end.date, "%d")
               , "&f=", format(end.date, "%Y")
               , "&g=d&ignore=.csv", sep="")
  
  tryCatch(
    {
      stock.hist <- read.csv(URL, header = TRUE)
      stock.hist$Date <- as.Date(stock.hist$Date, "%Y-%m-%d")
    }, error = function(err) {
      stock.hist <- list()
      print(paste("Could not read Yahoo CSV file: ", URL, sep=""))
    }
  )
  return(stock.hist)
}

Cointegration <- function(x,y) {
  vals<-data.frame(x,y)
  beta<-coef(lm(vals[,2]~vals[,1]+0,data=vals))[1]
  (adf.test(vals[,2]-beta*vals[,1], alternative="stationary", k=0))$p.value
}

AdfMod <- function(x, L = 2, int = T, trend = T) { 
  # Construct Data for Augmented Dickey Fuller Model with L lags.
  # To exclude the intercept, use int=F. Same for inclusion/exclusion of trend.

  x <- ts(x) 
  D <- diff(x) 
  if(L > 0) { 
    for(i in 1:L) 
      D <- ts.intersect(D, lag(diff(x),  - i)) 
  } 

  D <- ts.intersect(lag(x, -1), D) 
  
  if(trend == T) 
    D <- ts.intersect(D, time(x)) 
  
  y <- D[, 2] 
  x <- D[, -2] 

  if(int == T) 
    summary(lm(y ~ x)) 
  else summary(lm(y ~ x - 1)) 

} 

PlotPair <- function(stock1.data, stock2.data) {
  #SET UP WINDOW PANE FOR 2 PLOTS ALIGNED VERTICALLY
  par(mfrow=c(2, 1))
    
  #TIME SERIES OF TWO STOCKS ON ONE PLOT
  plot(stock1.data$Date,stock1.data$Adj.Close,type="l", main="Stock Time Series", col=3)
  par(new=T)
  plot(stock2.data$Date,stock2.data$Adj.Close,type="l", col=6)
    
  #CALC BASIC STATS
  m <- median(stock1.data$Adj.Close/stock2.data$Adj.Close)
  s <- sd(stock1.data$Adj.Close/stock2.data$Adj.Close)
    
  #PLOT PAIR RATIO
  plot(stock1.data$Date,stock1.data$Adj.Close/stock2.data$Adj.Close,type="l", main="Stock Pair Ratio", col=3)
  par(new=T)
  abline(h=m, col="dodgerblue3")
  abline(h=m+(1*s), col="darkolivegreen1")
  abline(h=m-(1*s), col="darkolivegreen1")
  abline(h=m+(2*s), col="darkolivegreen2")
  abline(h=m-(2*s), col="darkolivegreen2")
  abline(h=m+(3*s), col="darkolivegreen4")
  abline(h=m-(3*s), col="darkolivegreen4")
}

FindTradingPairs <- function(stock.list=c("KO", "PEP")
                             , print.results=FALSE
                             , cutoff.date.input=Sys.Date()-365
                             , pvalue.threshold=0.05) {

  #HEADER FOR OUTPUT
  output <- c("STOCK A", "STOCK B", "COVARIANCE", "HURST EXP", "COINTEGRATION")

  for (f in stock.list) {
    stock1 <- PullStockHistory(f, cutoff.date.input)
    stock1$Date <- as.Date(stock1$Date,"%Y-%m-%d")
    stock1.name <- f
  
    for (f2 in stock.list) {
      if (f != f2) {
        stock2 <- PullStockHistory(f2, cutoff.date.input)
        stock2$Date <- as.Date(stock2$Date,"%Y-%m-%d")
        stock2.name <- f2
                          
        #IN EVENT STOCK NO LONGER TRADED OR PARTIAL DATA SET
        if (length(stock1$Date)!=length(stock2$Date)) {
          print(paste("Unable to compare ", stock1.name, " and ", stock2.name, ". Data sets are of unequal length.", sep=""))
          next
        }
        #AVOID REPEAT COMPARISONS
        if (stock2.name < stock1.name) {
          next
        }
      
        print(paste("Comparing ", stock1.name, " and ", stock2.name, "...", sep=""))
        cutoff.date <- max(tail(stock1$Date, n=1), tail(stock2$Date, n=1), as.Date(cutoff.date.input,"%Y-%m-%d"))

        #TAKE SUBSET OF DATA BASED ON DATE
        stock1.dates <- with(stock1,stock1$Date > cutoff.date)
        stock1.data <- subset(stock1,stock1.dates)
        stock2.dates <- with(stock2,stock2$Date > cutoff.date)
        stock2.data <- subset(stock2,stock2.dates)
      
        stock1.adj_close.ts <- ts(stock1.data$Adj.Close)
        stock2.adj_close.ts <- ts(stock2.data$Adj.Close)
      
        #CALC STATISTICAL INFO
        results.cov <- cov(stock1.data$Adj.Close, stock2.data$Adj.Close)
        results.hurst <- hurstSpec(stock1.data$Adj.Close/stock2.data$Adj.Close)
        results.cointegration <- Cointegration(stock1.data$Adj.Close, stock2.data$Adj.Close)
      
        if (print.results == TRUE)
        {
          print(paste("Covariance: ", results.cov))
          print(paste("Hurst: ", results.hurst))
          #COINTEGRATION: WANT p < 0.5
          print(paste("Cointegration: ", results.cointegration))
        }
        
        if (results.cointegration < pvalue.threshold) {
          current.data <- c(stock1.name, stock2.name, results.cov, results.hurst, results.cointegration)
          output <- rbind(output, current.data)
        }
      }
    }
  }
  return(output)
}

#CHECK EVERYTHING
URL.list <- PullIndustryURLs()
pairs.output <- list()

for (url in URL.list) {
  stock.list <- PullIndustryList(url)
  pairs.output <- c(pairs.output, FindTradingPairs(stock.list, TRUE))
}

#WRITE RESULTS
write.csv(pairs.output, output.filename, col.names=FALSE, sep="\t", row.names=FALSE)
