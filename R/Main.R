# d <- getHistoricalQuote('AAPL', 'NASD')
# d.vsa <- getHistoricalVSAQuote(d)
# d.vsa <- getHistoricalVSAQuote(d, standardize = T)
# d <- getHistoricalQuote('GOOGL', 'NASD', interval = 3600, period = '1Y')
# d <- getHistoricalQuote('gbpusd', 'CURRENCY', interval = 900, period = '3d')
# head(d, 15)

getHistoricalQuote <- function(symbol, exchange,
                               interval = 86400, period = "10Y") {
  url <- paste0("https://www.google.com/finance/getprices?q=",
    toupper(symbol), "&x=", toupper(exchange), "&i=", interval,
    "&p=", period, "&f=d,c,v,k,o,h,l")
  url_content <- readLines(url)

  if (length(url_content) > 7) {
    df = read.table(text = .parseString(url_content, interval),
      sep = ",", colClasses = "numeric")
    names(df) <- c("UTC_Date", "Timezon_offset", "Close",
      "High", "Low", "Open", "Volume", "CDAYS")

    df$UTC_Date <- as.POSIXct(df$UTC_Date, origin = "1970-01-01",
      tz = "UTC")
    df$Market_Date <- df$UTC_Date + df$Timezon_offset * 60

    df <- data.frame(df$UTC_Date, df$Market_Date, df$Open,
      df$High, df$Low, df$Close, df$Volume)
    names(df) <- c("UTC_Date", "Market_Date", "Open", "High",
      "Low", "Close", "Volume")

    return(df)
  } else warning("Incorrect parameters")
}

getHistoricalVSAQuote <- function(data, period = 15) {
  # Height of bar
  data$Height <- data$High - data$Low

  # The relative close of the bar inside the bar
  data$C <- (data$Close - data$Low) / (data$High - data$Low)

  data$B <- numeric(nrow(data))
  data$V <- numeric(nrow(data))
  data$H <- numeric(nrow(data))
  data$Ce <- numeric(nrow(data))
  data$Change <- numeric(nrow(data))
  data$M <- numeric(nrow(data))

  for (i in 2:nrow(data)) {
    # Type of bar
    if (data$Close[i] > data$High[i - 1]) {
      data$B[i] <- 1.00
    }
    else if (data$Close[i] > data$Close[i - 1]) {
      data$B[i] <- 0.75
    }
    else if (data$Close[i] == data$Close[i - 1]) {
      data$B[i] <- 0.5
    }
    else if (data$Close[i] < data$Low[i - 1]) {
      data$B[i] <- 0.25
    }
    else
      data$B[i] <- 0.00

    # The difference between closes
    data$Change[i] <- abs(data$Close[i] - data$Close[i - 1])
  }
  data$B[1] <- NA
  data$Change[1] <- mean(data$Change[1:period])

  for (i in period:nrow(data)) {
    # The relative volume of bar
    aver.volume <- .getAver(data$Volume, period, i)
    if (data$Volume[i] >= aver.volume * 2) {
      data$V[i] = 1.0
    }
    else if (data$Volume[i] >= aver.volume * 1.713) {
      data$V[i] = 0.8
    }
    else if (data$Volume[i] >= aver.volume * 1.148) {
      data$V[i] = 0.6
    }
    else if (data$Volume[i] >= aver.volume * 0.825) {
      data$V[i] = 0.4
    }
    else if (data$Volume[i] >= aver.volume * 0.287) {
      data$V[i] = 0.2
    }
    else
      data$V[i] = 0.0

    # The relative height of bar
    aver.height <- .getAver(data$Height, period, i)
    if (data$Height[i] >= aver.height * 2) {
      data$H[i] = 1.0
    }
    else if (data$Height[i] >= aver.height * 1.713) {
      data$H[i] = 0.8
    }
    else if (data$Height[i] >= aver.height * 1.148) {
      data$H[i] = 0.6
    }
    else if (data$Height[i] >= aver.height * 0.825) {
      data$H[i] = 0.4
    }
    else if (data$Height[i] >= aver.height * 0.287) {
      data$H[i] = 0.2
    }
    else
      data$H[i] = 0.0

    # The relative close of bar
    n <- i - period + 1
    data$Ce[i] <- (data$Close[i] - min(data$Low[n:i])) /
      (max(data$High[n:i]) - min(data$Low[n:i]))


    # Relative price change of the bar
    aver.change <- .getAver(data$Change, period, i)
    if (data$Change[i] >= aver.change * 2) {
      data$M[i] = 1.0
    }
    else if (data$Change[i] >= aver.change * 1.713) {
      data$M[i] = 0.8
    }
    else if (data$Change[i] >= aver.change * 1.148) {
      data$M[i] = 0.6
    }
    else if (data$Change[i] >= aver.change * 0.825) {
      data$M[i] = 0.4
    }
    else if (data$Change[i] >= aver.change * 0.287) {
      data$M[i] = 0.2
    }
    else
      data$M[i] = 0.0
  }
  data$V[1:(period - 1)] <- NA
  data$H[1:(period - 1)] <- NA
  data$Ce[1:(period - 1)] <- NA
  data$M[1:(period - 1)] <- NA
  data$Height <- NULL
  data$Change <- NULL

  return(data[period:nrow(data), ])
}

.getAver <- function(v, period, i) {
  return(mean(v[(i - period + 1):i]))
}
