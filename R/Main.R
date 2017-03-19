# d <- getHistoricalQuote('AAPL', 'NASD')
# d.vsa <- getVsa(d)
# d.vsa <- getVsa(d, period = c(8, 15))
# d <- getHistoricalQuote('GOOGL', 'NASD', interval = 3600, period = '1Y')
# d <- getHistoricalQuote('gbpusd', 'CURRENCY', interval = 900, period = '3d')
# head(d, 15)

getHistoricalQuote <- function(symbol, exchange, interval = 86400, period = "10Y") {
  url <- paste0("https://www.google.com/finance/getprices?q=",
    toupper(symbol), "&x=", toupper(exchange), "&i=", interval,
    "&p=", period, "&f=d,c,v,k,o,h,l")
  url_content <- readLines(url)

  if (length(url_content) > 7) {
    df = read.table(text = .parseString(url_content, interval),
      sep = ",", colClasses = "numeric")
    names(df) <- c("UTC_Date", "Timezon_offset", "Close",
      "High", "Low", "Open", "Volume", "CDAYS")

    df$UTC_Date <- as.POSIXct(df$UTC_Date, origin = "1970-01-01", tz = "UTC")
    df$Market_Date <- df$UTC_Date + df$Timezon_offset * 60

    df <- data.frame(
      df$UTC_Date, df$Market_Date, df$Open, df$High, df$Low, df$Close, df$Volume
      )
    names(df) <- c(
      "UTC_Date", "Market_Date", "Open", "High", "Low", "Close", "Volume"
      )

    return(df)
  } else warning("Incorrect parameters")
}

getVsa <- function(df, period = 15) {
  vsa <- data.frame(
    Height = numeric(nrow(df)),
    C = numeric(nrow(df)),
    B = numeric(nrow(df)),
    Change = numeric(nrow(df))
  )

  # Height of bar
  vsa$Height <- df$High - df$Low

  # The relative close of the bar inside the bar
  vsa$C <- (df$Close - df$Low) / (df$High - df$Low)

  for (i in 2:nrow(df)) {
    # Type of bar
    if (df$Low[i] > df$High[i - 1])
      vsa$B[i] <- 6/6
    else if (df$Close[i] > df$High[i - 1])
      vsa$B[i] <- 5/6
    else if (df$Close[i] > df$Close[i - 1])
      vsa$B[i] <- 4/6
    else if (df$Close[i] == df$Close[i - 1])
      vsa$B[i] <- 3/6
    else if (df$High[i] < df$Low[i - 1])
      vsa$B[i] <- 0/6
    else if (df$Close[i] < df$Low[i - 1])
      vsa$B[i] <- 1/6
    else if (df$Close[i] < df$Close[i - 1])
      vsa$B[i] <- 2/6

    # The difference between closes
    vsa$Change[i] <- abs(df$Close[i] - df$Close[i - 1])
  }
  vsa$B[1] <- NA
  vsa$Change[1] <- mean(vsa$Change[1:3])

  for (i in 1:length(period)) {
    vsa <- data.frame(vsa, .getVsaPeriod(df, vsa, period[i]))
  }

  vsa$Height <- NULL
  vsa$Change <- NULL

  return(vsa)
}

.getVsaPeriod <- function(df, vsa, period) {
  vsa.period <- data.frame(
    V = numeric(nrow(df)),
    H = numeric(nrow(df)),
    Ce = numeric(nrow(df)),
    M = numeric(nrow(df))
  )

  for (i in period:nrow(df)) {
    # The relative volume of bar
    aver.volume <- .getAver(df$Volume, period, i)
    if (df$Volume[i] >= aver.volume * 2)
      vsa.period$V[i] = 1.0
    else if (df$Volume[i] >= aver.volume * 1.713)
      vsa.period$V[i] = 0.8
    else if (df$Volume[i] >= aver.volume * 1.148)
      vsa.period$V[i] = 0.6
    else if (df$Volume[i] >= aver.volume * 0.825)
      vsa.period$V[i] = 0.4
    else if (df$Volume[i] >= aver.volume * 0.287)
      vsa.period$V[i] = 0.2
    else
      vsa.period$V[i] = 0.0

    # The relative height of bar
    aver.height <- .getAver(vsa$Height, period, i)
    if (vsa$Height[i] >= aver.height * 2)
      vsa.period$H[i] = 1.0
    else if (vsa$Height[i] >= aver.height * 1.713)
      vsa.period$H[i] = 0.8
    else if (vsa$Height[i] >= aver.height * 1.148)
      vsa.period$H[i] = 0.6
    else if (vsa$Height[i] >= aver.height * 0.825)
      vsa.period$H[i] = 0.4
    else if (vsa$Height[i] >= aver.height * 0.287)
      vsa.period$H[i] = 0.2
    else
      vsa.period$H[i] = 0.0

    # The relative close of bar
    n <- i - period + 1
    vsa.period$Ce[i] <- (df$Close[i] - min(df$Low[n:i])) /
      (max(df$High[n:i]) - min(df$Low[n:i]))


    # Relative price change of the bar
    aver.change <- .getAver(vsa$Change, period, i)
    if (vsa$Change[i] >= aver.change * 2)
      vsa.period$M[i] = 1.0
    else if (vsa$Change[i] >= aver.change * 1.713)
      vsa.period$M[i] = 0.8
    else if (vsa$Change[i] >= aver.change * 1.148)
      vsa.period$M[i] = 0.6
    else if (vsa$Change[i] >= aver.change * 0.825)
      vsa.period$M[i] = 0.4
    else if (vsa$Change[i] >= aver.change * 0.287)
      vsa.period$M[i] = 0.2
    else
      vsa.period$M[i] = 0.0
  }
  vsa.period$V[1:(period - 1)] <- NA
  vsa.period$H[1:(period - 1)] <- NA
  vsa.period$Ce[1:(period - 1)] <- NA
  vsa.period$M[1:(period - 1)] <- NA

  names(vsa.period) <- c(paste0(
    "V_", period), paste0("H_", period), paste0("Ce_", period), paste0("M_", period)
    )

  return(vsa.period)
}

.getAver <- function(v, period, i) {
  return(mean(v[(i - period + 1):i]))
}
