#' Forecast function
#' Forecasts for the data
#' @param x data vector to analyse
#' @param sm start month
#' @param sy start year
#' @param em end month
#' @param ey end year
#' @param fort forecast length in months
#' @export
#' @examples
#' forecast(x, sm, sy, em, ey, fort)

forecast <- function(x, sm, sy, em, ey, fort) {
  ts_dat = ts(x, start=c(sy, sm), end=c(ey, em), frequency = 12)
  m_tbats =tbats(ts_dat)
  f_tbats = forecast::forecast(m_tbats, h = fort)
  plot(f_tbats)
  arDat <- auto.arima(ts_dat)
  arFor = forecast::forecast(arDat, h=fort)
  plot(arFor)
  ss = data.frame(f_tbats)
  kk = data.frame(arFor)
  arimaFor=sum(kk$Point.Forecast)
  tBatsFor=sum(ss$Point.Forecast)
  result <- c()
  result[1] <- arimaFor
  result[2] <- tBatsFor
  return(result)
}
