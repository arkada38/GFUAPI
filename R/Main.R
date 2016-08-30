# d <- getHistoricalQuote("AAPL", "NASD")
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
