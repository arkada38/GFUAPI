# d <- getHistoricalQuote("AAPL", "NASD")
# d <- getHistoricalQuote("GOOG", "NASD")
# head(d, 15)

# Rcpp::sourceCpp('src/cpp.cpp')

getHistoricalQuote <- function(symbol, exchange, interval = 86400, period = "10Y") {
  library(magrittr)

  url <- paste0("https://www.google.com/finance/getprices?q=",
                symbol,"&x=",exchange,"&i=",interval,"&p=",period,"&f=d,c,v,k,o,h,l")
  url_content <- readLines(url)

  if (length(url_content) > 7){
    df = read.table(text = parseString(url_content), sep = ",", colClasses = "numeric")
    names(df) <- c("Date", "Timezon_offset", "Close", "High", "Low", "Open", "Volume", "CDAYS")
    df <- data.frame(df$Date, df$Timezon_offset, df$Open, df$High, df$Low, df$Close, df$Volume)
    names(df) <- c("Date", "Timezon_offset", "OPEN", "HIGH", "LOW", "Close", "VOLUME")
    return(df)
  }
  else
    warning("Incorrect parameters")
}
