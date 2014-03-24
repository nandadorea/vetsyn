##' \code{convert_days_to_week}
##'
##' Converts a matrix of daily counts into weekly counts, using the
##' ISOweek standard, provided that a deparate data frame is 
##' appointed from which to extract the actual dates corresponding to each
##' row of the matrix.
##' 
##' This is an internal function of the package vetsyn, made available as an 
##' exported funciton in order to allow users to take advantage of the 
##' function retro_summary() from the package. That function
##' generates a markdown file with R code that can be useful for users
##' during restrospective analysis of syndromic data. When those R codes are run
##' manually by the user, this function may be needed. 
##'
##' @title convert_days_to_week
##' @param counts.df a matrix with rows corresponding to daily counts, and
##' columns corresponding to syndromic groups
##' @param dates.df a data frame where the first column contain dates
##' @param date.format the date format in the dates variable, by default "%Y-%m-%d".
##' 
##' @export 
##' @import ISOweek
##' @examples
##' data(lab.daily)
##' my.syndromic <- raw_to_syndromic (id=lab.daily$SubmissionID,
##'                                   syndromes.var=lab.daily$Syndrome,
##'                                   dates.var=lab.daily$DateofSubmission,
##'                                   date.format="%d/%m/%Y")
##' weekly <- convert_days_to_week(my.syndromic'at'observed; my.syndromic'at'dates)
##'



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

