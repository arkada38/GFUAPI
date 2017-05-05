# data(aapl)
# d <- aapl

# d <- getHistoricalPrices('GOOGL', 'NASD', interval = 3600, period = '1Y')
# d <- getHistoricalPrices('GBPUSD', 'CURRENCY', interval = 900, period = '3d')
# d <- getHistoricalPrices('AAPL', 'NASD')

# d.analysis <- getAnalysis(d)
# d.analysis <- getAnalysis(d, period = c(8, 15))

# d.analysis.with.prevs <- getAnalysisWithPrevious(d.analysis, 3)

# d.actions <- getAction(d, range.up = 0.06, range.down = 0.025)
# table(d.acctions$action.factor)

getHistoricalPrices <- function(symbol, exchange = "", interval = 86400, period = "10Y") {
  if (exchange != "")
    exchange <- paste0("&x=", toupper(exchange))

  url <- paste0(
    "https://www.google.com/finance/getprices?q=",
    toupper(symbol), exchange, "&i=", interval,
    "&p=", period, "&f=d,c,v,k,o,h,l"
  )
  url_content <- readLines(url)

  if (length(url_content) > 7) {
    df = read.table(text = .parseString(url_content, interval),
      sep = ",", colClasses = "numeric")
    names(df) <- c("UTC_Date", "Timezon_offset", "Close",
      "High", "Low", "Open", "Volume", "CDAYS")

    df$UTC_Date <- as.POSIXct(df$UTC_Date, origin = "1970-01-01", tz = "UTC")
    df$Market_Date <- df$UTC_Date + df$Timezon_offset * 60

    df <- data.frame(
      df$Market_Date, df$Open, df$High, df$Low, df$Close, df$Volume
    )
    names(df) <- c(
      "Market_Date", "Open", "High", "Low", "Close", "Volume"
    )

    df <- as.xts(df[, 2:6], df$Market_Date)

    return(df)
  } else warning("Incorrect parameters")
}

getAnalysis <- function(df, period = 15) {
  analysis <- data.frame(
    height.of.bar = numeric(nrow(df)),
    close.inside.bar = numeric(nrow(df)),
    type.of.bar = numeric(nrow(df)),
    change.of.closes = numeric(nrow(df))
  )

  df <- as.data.frame(df)

  # Height of bar
  analysis$height.of.bar <- df$High - df$Low

  # The relative close of the bar inside the bar
  analysis$close.inside.bar <- (df$Close - df$Low) / (df$High - df$Low)

  for (i in 2:nrow(df)) {
    # Type of bar
    if (df$Low[i] > df$High[i - 1])
      analysis$type.of.bar[i] <- 6/6
    else if (df$Close[i] > df$High[i - 1])
      analysis$type.of.bar[i] <- 5/6
    else if (df$Close[i] > df$Close[i - 1])
      analysis$type.of.bar[i] <- 4/6
    else if (df$Close[i] == df$Close[i - 1])
      analysis$type.of.bar[i] <- 3/6
    else if (df$High[i] < df$Low[i - 1])
      analysis$type.of.bar[i] <- 0/6
    else if (df$Close[i] < df$Low[i - 1])
      analysis$type.of.bar[i] <- 1/6
    else if (df$Close[i] < df$Close[i - 1])
      analysis$type.of.bar[i] <- 2/6

    # The difference between closes
    # Разница между текущим и предыдущим закрытием
    # На сколько изменилась цена закрытия
    analysis$change.of.closes[i] <- abs(df$Close[i] - df$Close[i - 1])
  }
  analysis$type.of.bar[1] <- NA
  analysis$change.of.closes[1] <- mean(analysis$change.of.closes[1:3])

  for (i in 1:length(period))
    analysis <- data.frame(analysis, .getAnalysisPeriod(df, analysis, period[i]))

  analysis$height.of.bar <- NULL
  analysis$change.of.closes <- NULL

  return(analysis)
}

.getAnalysisPeriod <- function(df, analysis, period) {
  analysis.period <- data.frame(
    relative.volume = numeric(nrow(df)),
    relative.height = numeric(nrow(df)),
    close.between.extremums = numeric(nrow(df)),
    relative.change.of.closes = numeric(nrow(df)),
    open.mean.deviation = numeric(nrow(df)),
    close.mean.deviation = numeric(nrow(df)),
    relative.open.mean.deviation = numeric(nrow(df)),
    relative.close.mean.deviation = numeric(nrow(df))
  )

  for (i in period:nrow(df)) {
    # The relative volume of bar
    analysis.period$relative.volume[i] <- .getRelativeValue(
      df$Volume[i],
      .getAver(df$Volume, period, i)
    )

    # The relative height of bar
    analysis.period$relative.height[i] <- .getRelativeValue(
      analysis$height.of.bar[i],
      .getAver(analysis$height.of.bar, period, i)
    )

    # In which part between the maximum and minimum for the period is the closure
    # В какой части между максимумом и минимумом за период находится закрытие
    n <- i - period + 1
    analysis.period$close.between.extremums[i] <- (df$Close[i] - min(df$Low[n:i])) /
      (max(df$High[n:i]) - min(df$Low[n:i]))

    # Relative price change of the bar
    analysis.period$relative.change.of.closes[i] <- .getRelativeValue(
      analysis$change.of.closes[i],
      .getAver(analysis$change.of.closes, period, i)
    )

    # Необходимо посчитать отклонения от средней скользящей
    # Затем по этим отклонениям по 6-ти балльной шкале

    # Absolute deviations from the mean of open by period
    analysis.period$open.mean.deviation[i] <- abs(df$Open[i] - .getAver(df$Open, period, i))

    # Absolute deviations from the mean of close by period
    analysis.period$close.mean.deviation[i] <- abs(df$Close[i] - .getAver(df$Close, period, i))
  }

  for (i in (2 * period):nrow(df)) {
    # Relative deviations from the mean of open by period
    analysis.period$relative.open.mean.deviation[i] <- .getTwoWayRelativeValue(
      analysis.period$open.mean.deviation[i],
      .getAver(analysis.period$open.mean.deviation, period, i)
    )

    # Relative deviations from the mean of close by period
    analysis.period$relative.close.mean.deviation[i] <- .getTwoWayRelativeValue(
      analysis.period$close.mean.deviation[i],
      .getAver(analysis.period$close.mean.deviation, period, i)
    )
  }

  analysis.period$open.mean.deviation <- NULL
  analysis.period$close.mean.deviation <- NULL

  analysis.period[1:(2 * period - 1), ] <- NA
  names(analysis.period) <- paste0(names(analysis.period), "_", period)

  return(analysis.period)
}

.getAver <- function(v, period, i) {
  return(mean(v[(i - period + 1):i]))
}

.getRelativeValueOld <- function(item, aver) {
  if (item >= aver * 2.000)
    return(1.0)
  else if (item >= aver * 1.713)
    return(0.8)
  else if (item >= aver * 1.148)
    return(0.6)
  else if (item >= aver * 0.825)
    return(0.4)
  else if (item >= aver * 0.287)
    return(0.2)
  else
    return(0.0)
}

# Возвращает значение от 0 до 1, между 0 и 2-мя средними значениями
.getRelativeValue <- function(item, aver) {
  return(min(item / 2 / aver, 1))
}

# Возвращает значение от 0 до 1, между -2-мя и 2-мя средними значениями
.getTwoWayRelativeValue <- function(item, aver) {
  return(min(max(0, (item + 2 * aver) / 4 / aver), 1))
}

getAnalysisWithPrevious <- function(df.analysis.only, n.of.prevs) {
  # копируем изначальный data.frame
  df.analysis.only.with.prev <- df.analysis.only

  for (i in 1:n.of.prevs) {
    # Составляем data.frame со смещением на n.of.prevs записей
    df.analysis.only.prev.i <- rbind(
      df.analysis.only[1:i,],
      df.analysis.only[1:(nrow(df.analysis.only) - i),]
    )

    # Очищаем записи, для которых значение предыдущих записей неизвестно
    df.analysis.only.prev.i[1:i,] <- NA

    # Переименовываем столбцы
    names(df.analysis.only.prev.i) <- paste0(names(df.analysis.only), "_prev_", i)

    # Добавляем столбцы к существующему data.frame
    df.analysis.only.with.prev <- data.frame(df.analysis.only.with.prev, df.analysis.only.prev.i)
  }

  return(df.analysis.only.with.prev)
}

getAction <- function(df, range.up, range.down) {
  action <- data.frame(
    action = numeric(nrow(df)),
    action.factor = factor(nrow(df)),
    buy = numeric(nrow(df)),
    nothing = numeric(nrow(df)),
    sell = numeric(nrow(df))
  )

  df <- as.data.frame(df)

  action$action <- 0.5
  action$action.factor <- "Nothing"

  # целевая прибыль
  range.up <- 0.06
  # допустимая просадка
  range.down <- 0.025

  for (i in 1:(nrow(action) - 1)) {
    can.buy <- TRUE
    can.sell <- TRUE

    for (j in (i + 1):nrow(action)) {
      if (df$Close[i] / df$High[j] - 1 <= -range.down)
        can.buy <- FALSE
      else if (df$Close[i] / df$Low[j] - 1 >= range.down)
        can.sell <- FALSE

      if (!can.buy & !can.sell)
        break

      if (can.buy & df$Close[i] / df$High[j] - 1 >= range.up) {
        action$action[i] <- 1
        action$action.factor[i] <- "Buy"
        break
      }
      else if (can.sell & df$Close[i] / df$Low[j] - 1 <= -range.up) {
        action$action[i] <- 0
        action$action.factor[i] <- "Sell"
        break
      }
    }
  }

  action$buy <- as.numeric(action$action.factor == "Buy")
  action$sell <- as.numeric(action$action.factor == "Sell")
  action$nothing <- as.numeric(action$action.factor == "Nothing")

  return(action)
}
