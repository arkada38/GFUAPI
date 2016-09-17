# d <- getHistoricalQuote("AAPL", "NASD")
# d.vsa <- getHistoricalVSAQuote(d)
# d.vsa <- getHistoricalVSAQuote(d, standardize = T)
# d <- getHistoricalQuote("GOOGL", "NASD", interval = 3600, period = "1Y")
# d <- getHistoricalQuote("gbpusd", "CURRENCY", interval = 900, period = "3d")
# head(d, 15)

getHistoricalQuote <- function(symbol, exchange, interval = 86400, period = "10Y") {
  url <- paste0("https://www.google.com/finance/getprices?q=",
                toupper(symbol),"&x=",toupper(exchange),"&i=",interval,"&p=",period,"&f=d,c,v,k,o,h,l")
  url_content <- readLines(url)

  if (length(url_content) > 7){
    df = read.table(text = parseString(url_content, interval), sep = ",", colClasses = "numeric")
    names(df) <- c("UTC_Date", "Timezon_offset", "Close", "High", "Low", "Open", "Volume", "CDAYS")

    df$UTC_Date <- as.POSIXct(df$UTC_Date, origin = "1970-01-01", tz="UTC")
    df$Market_Date <- df$UTC_Date + df$Timezon_offset * 60

    df <- data.frame(df$UTC_Date, df$Market_Date, df$Open, df$High, df$Low, df$Close, df$Volume)
    names(df) <- c("UTC_Date", "Market_Date", "Open", "High", "Low", "Close", "Volume")

    return(df)
  }
  else
    warning("Incorrect parameters")
}

getHistoricalVSAQuote <- function(data, period = 15, standardize = F) {
  # Высота бара
  data$Height <- data$High - data$Low

  # Вид бара
  for (i in 1:(nrow(data)-1)) {
    if (data$Close[i] > data$High[i + 1]) {
      data$B[i] <- +2
    } else if (data$Close[i] > data$Close[i + 1]) {
      data$B[i] <- +1
    } else if (data$Close[i] == data$Close[i + 1]) {
      data$B[i] <- 0
    } else if (data$Close[i] > data$Low[i + 1]) {
      data$B[i] <- -1
    } else
      data$B[i] <- -2
  }
  data$B[nrow(data)] <- NA

  # Относительное закрытие бара внутри бара
  data$C <- (data$Close - data$Low) / (data$High - data$Low) * 100

  # Относительный объём бара
  for (i in 1:(nrow(data) - period)) {
    data$V[i] <- data$Volume[i] / mean(data$Volume[i:(i + period - 1)])
  }
  data$V[(nrow(data) - period + 1):nrow(data)] <- NA

  # Относительная высота бара
  for (i in 1:(nrow(data) - period)) {
    data$H[i] <- data$Height[i] / mean(data$Height[i:(i + period - 1)])
  }
  data$H[(nrow(data) - period + 1):nrow(data)] <- NA
  data$Height <- NULL

  # Относительное закрытие бара
  for (i in 1:(nrow(data) - period)) {
    n <- i + period - 1
    data$Ce[i] <- (data$Close[i] - min(data$Low[i:n])) / (max(data$High[i:n]) - min(data$Low[i:n]))
  }
  data$Ce[(nrow(data) - period + 1) : nrow(data)] <- NA

  # Величина между закрытиями
  for (i in 1:(nrow(data) - 1)) {
    data$Change[i] <- abs(data$Close[i] - data$Close[i+1])
  }
  data$Change[nrow(data)] <- NA

  # Относительное изменение цены бара
  data$M <- rep(0, nrow(data))
  for (i in 1:(nrow(data) - period)) {
    data$M[i] <- data$Change[i] / mean(data$Change[i:(i + period - 1)])
  }
  data$M[(nrow(data) - period + 1) : nrow(data)] <- NA
  data$Change <- NULL

  if (standardize) {
    data[,3:ncol(data)] <- as.data.frame(scale(data[,3:ncol(data)]))
  }

  return(data[1:(nrow(data) - period),])
}
