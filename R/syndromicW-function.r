##' Create an object of the class \code{syndromicW} from pre-cleaned data.
##' 
##' Syndromic is the main class of the package. Two versions are available,
##' \code{syndromicD}, which is intended for use when data are to be monitored daily,
##' and \code{syndromicW}, which is to be used when data are monitored weekly.
##' 
##' The observed data are assumed to be formatted as a \code{matrix}, in 
##' which each row corresponds to an observation time point (here, WEEKS),
##' columns correspond to each syndromic group to be monitored, and the
##' numbers refer to the number of observatiosn per group per time point.
##' For an example of data formatted this way use date(observedW). For data
##' not formatted use directly the function \code{raw_to_syndromicW}.
##' 
##' The dates are expected to be a \code{data.frame} with the first column
##' in the \code{ISOweek} format, though the function will also accept a
##' minimum and maximum week, and take care of constructing the appropriate
##' data frame (this is the recommended use). 
##' 
##' @param observed A \code{matrix} with the observed data
##' @param dates A \code{data.frame} with the complete ISOweeks of of each 
##' time point in the first column (each row in the matrix provided in \code{observed}).
##' @param min.week Alternative to providing dates: the user can specify the
##' minimum week-year, and the maximum week-year, 
##' and the function will generate
##' a complete sequence of weeks between them. This is the recommended use
##' of the function because then additional date variables will also be created
##' and assigned to the slot dates, such as ISOweek, year and numerical weeks. 
##' Note that the sequence must match the number of rows
##' in the matrix \code{observed}.
##' @param min.year see min.week above
##' @param max.week see min.week above
##' @param max.year see min.week above
##' 
##' @param baseline Optional argument. A \code{matrix} of same dimensions as \code{observed},
##' containing observed data that has been pre-processed to remove excessive noise
##' and outbreak signals retrospectively, generating an "outbreak-free baseline".
##' Normally syndromic objects are created without this slot,
##' receiving by default an empty matrix. Methods within this package are then used
##' to create this slot from data on \code{observed}.
##' @param alarms Optional argument. An \code{array} with the same number of 
##' rows and columns as \code{observed}, with an additional dimension to store
##' the alarm output of multiple detection algorithms used. Normally syndromic 
##' objects are created without this slot,
##' receiving by default an empty array. Methods within this package are then used
##' to create this slot from data on \code{observed}.
##' @param UCL Optional argument. An \code{array} with the same number of 
##' rows and columns as \code{observed}, with an additional dimension to store
##' the upper control limit output of multiple detection algorithms used.
##' Normally syndromic objects are created without this slot,
##' receiving by default an empty array. Methods within this package are then used
##' to create this slot from data on \code{observed}.
##' @param LCL Lower control limit. See UCL above. 
##' @param formula A character string (optional) specifying the regression formula to be used
##'     when removing temporal patterns from each of the syndromes in @observed. For instance 
##'     "dow+mon" when the regression formula should be " y~dow + mon", 
##'     indicating that day-of-week and month should be modelled. The names of the variables
##'     should exist in the columns of the slot @dates. Make sure that formulas' index match the
##'     columns in observed (for instance the second formula should correspond to the second
##'     syndrome, or second column in the observed matrix).This is often only filled after 
##'     some analysis in the data, not at the time of object creation.
##' 
##' @name syndromicW
##' @return an object of the class \code{syndromicW} with the slots
##' corresponding to the parameters described. See \code{class-syndromic}
##'  for more details.
##' 
##' @import ISOweek
##' @name syndromicW-function
##' @export
##' @examples
##' data(observedW)
##' my.syndromicW <- syndromicW(observed,min.week=1, min.year=2011, 
##'                               max.week=22, max.year=2013)
##'
##'


syndromicW <- function(observed, 
                      dates=data.frame(), 
                      min.week=NULL, 
                      max.week=NULL, 
                      min.year=NULL, 
                      max.year=NULL, 
                      baseline=matrix(nrow=0,ncol=0), 
                      alarms=array(dim=0), 
                      UCL=array(dim=0), 
                      LCL=array(dim=0),
                      formula=vector()) {
  
  if (!missing("dates"))(dates <- as.data.frame(dates))
  
  if (missing("dates")&&!missing("min.week")&&!missing("max.week")) {

    min.date <- ISOweek2date(create_isoweek(min.week,min.year,reference.day=1))
    max.date <- ISOweek2date(create_isoweek(max.week,max.year,reference.day=1))
    dates <- dates_df(min.date=min.date,max.date=max.date, 
                          by="days",
                          date.format = "%Y-%M-%d")
     isoweekv <- date2ISOweek(dates[,1])
       isoweekv.week <- substr(as.character(isoweekv),1,8)
    week <- as.integer(substr(as.character(isoweekv),7,8))
    year <- as.integer(substr(as.character(isoweekv),1,4))
    
    dates <- data.frame(ISOweek=isoweekv[!duplicated(isoweekv.week)],
                        week=week[!duplicated(isoweekv.week)],
                        year=year[!duplicated(isoweekv.week)])   
    
  }
      
  
  new('syndromicW', observed=observed, dates=dates, 
      baseline=baseline, alarms=alarms, UCL=UCL, LCL=LCL, formula=formula)
}



setGeneric("setDatesW<-",function(object,value){standardGeneric("setDatesW<-")})
 setReplaceMethod(
   f="setDatesW",
   signature="syndromicW",
   definition=function(object,value){
     object@dates <- value
     validObject(object)      #VALIDITY CONTROL
     return(object)
     }
   )

#setDates(test0)<-as.data.frame(dates)
#test0


setGeneric("setBaselineW<-",function(object,value){standardGeneric("setBaselineW<-")})
setReplaceMethod(
  f="setBaselineW",
  signature="syndromicW",
  definition=function(object,value){
    object@baseline <- value
    validObject(object)      #VALIDITY CONTROL
    return(object)
  }
)

setGeneric("setAlarmsW<-",function(object,value){standardGeneric("setAlarmsW<-")})
setReplaceMethod(
  f="setAlarmsW",
  signature="syndromicW",
  definition=function(object,value){
    object@alarms <- value
    validObject(object)      #VALIDITY CONTROL
    return(object)
  }
)

setGeneric("setUCLW<-",function(object,value){standardGeneric("setUCLW<-")})
setReplaceMethod(
  f="setUCLW",
  signature="syndromicW",
  definition=function(object,value){
    object@UCL <- value
    validObject(object)      #VALIDITY CONTROL
    return(object)
  }
)


setGeneric("setLCLW<-",function(object,value){standardGeneric("setLCLW<-")})
setReplaceMethod(
  f="setLCLW",
  signature="syndromicW",
  definition=function(object,value){
    object@LCL <- value
    validObject(object)      #VALIDITY CONTROL
    return(object)
  }
)

setGeneric("setformulaW<-",function(object,value){standardGeneric("setformulaW<-")})
setReplaceMethod(
  f="setformulaW",
  signature="syndromicW",
  definition=function(object,value){
    object@LCL <- value
    validObject(object)      #VALIDITY CONTROL
    return(object)
  }
)