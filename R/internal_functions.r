dates_df <- function (min.date, max.date,
                      by="days",
                      date.format="%d/%m/%Y") {

  start <- as.Date(min.date, format = date.format)
  end   <- as.Date(max.date, format = date.format)

  require(ISOweek)
  
  dates.v <- seq(from=start,to=end, by=by)
  date <- strptime (as.character(dates.v), format = "%Y-%m-%d")
  year <- (date$year+1900)
  month <- date$mon
  mday <- date$mday
  yday <- date$yday
  dow <- date$wday
  week <- as.numeric(substring(ISOweek(date),7,8))
  weekday <- rep(1,length(date))
      weekday[dow==0|dow==6] <- 0
  dates <- as.data.frame(dates.v)
  colnames(dates) <- "dates"
  dates <- cbind(dates, mday, month, year, yday, week, dow, weekday)

  return(dates)

}



convert_days_to_week <- function(counts.df,
                                dates.df,date.format="%Y-%m-%d") {

  if (length(dates.df$week)==0||length(dates.df$year)==0){
    dates.vector <- strptime (as.character(dates.df[1]), format = date.format)
    year <- (dates.vector+1900)
    week <- as.numeric(substring(ISOweek(dates.vector),7,8))
  } else {
    year <- dates.df$year
    week <- dates.df$week
  }

  synd.week<- aggregate(counts.df,by=list(week=week, year=year),sum)

  return(synd.week)
}
