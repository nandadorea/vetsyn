##' Creates weeks in the format ISOweek from week and year provided as numerical formats 
##'
##' @title create_isoweek
##' @name create_isoweek
##'
##' @aliases create_isoweek
##' 
##' @param week the week of the year
##' @param year the year
##' @param reference-day the ISOweek format also stores the actual day of the week,
##' creating a daily sequence. users can set a reference day. if only interested
##' in weekly data, it's suggested to leave as the default reference.day=1
##' 
##' @export 
##' @examples
##' create_isoweek(11,2014)
##' dates <- data.frame (week=1:10, year=2014)
##'     ISOweek <- create_isoweek(dates$week,dates$year)
##'     dates <- cbind(ISOweek,dates)
##'


create_isoweek <- function(week,
                           year,
                           reference.day=1){
  dates = paste (year,"-W",sprintf("%02d", week),"-",reference.day,sep="")
  return(dates)
}

