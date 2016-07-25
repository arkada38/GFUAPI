library(magrittr)

getHistoricalQuote <- function(symbol, exchange, interval = 86400, period = "10Y") {
  url <- paste0("https://www.google.com/finance/getprices?q=",
                symbol,"&x=",exchange,"&i=",interval,"&p=",period,"&f=d,c,v,k,o,h,l")
  url_content <- readLines(url)

  j <- 1

  Date <- vector("numeric")
  Local <- vector("numeric")
  Market <- vector("numeric")
  Open <- vector("numeric")
  High <- vector("numeric")
  Low <- vector("numeric")
  Close <- vector("numeric")
  Volume <- vector("numeric")

  if (length(url_content) > 7)
    for (i in 7:length(url_content)) {
      if (substr(url_content[i], 1, nchar("TIMEZONE_OFFSET=")) == "TIMEZONE_OFFSET=")
        timezone_offset <- url_content[i] %>%
          substring(nchar("TIMEZONE_OFFSET=")+1) %>% as.numeric
      else {
        temp <- strsplit(url_content[i], ",") %>% unlist

        if (substring(temp[1], 1, 1) == "a") {
          absolute_date <- temp[1] %>% substring(2) %>% as.numeric %>% as.POSIXct(origin="1970-01-01")
          Date[j] <- absolute_date
        }
        else {
          Date[j] <- absolute_date + as.numeric(temp[1]) * interval
        }

        Local[j] <- Date[j] %>%
          as.POSIXct(origin="1970-01-01") %>%
          format("%d.%m.%Y %H:%M:%OS")
        Market[j] <- (Date[j] + timezone_offset * 60) %>%
          as.POSIXct(origin="1970-01-01", tz="UTC") %>%
          format("%d.%m.%Y %H:%M:%OS")

        Open[j] <- temp[5] %>% as.numeric
        High[j] <- temp[3] %>% as.numeric
        Low[j] <- temp[4] %>% as.numeric
        Close[j] <- temp[2] %>% as.numeric
        Volume[j] <- temp[6] %>% as.numeric
        j <- j + 1
      }
    }
  else
    warning("Incorrect parameters")

  cat(symbol, "-", substring(url_content[1], nchar("EXCHANGE%3D")+1), "\n", length(Date), "obs.")

  return(data.frame(Date, Local, Market, Open, High, Low, Close, Volume, stringsAsFactors = FALSE))
}
