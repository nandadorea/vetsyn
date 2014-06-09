##' Create an object of the class \code{syndromicD} from pre-cleaned data.
##' The observed data are assumed to be formatted as a \code{matrix}, in 
##' which each row corresponds to an observation time point (DAYS),
##' columns correspond to each syndromic group to be monitored, and the
##' numbers refer to the number of observatiosn per group per time point.
##' For an example of data formatted this way use date(observed). For data
##' not formatted use directly the function "raw_to_syndromicD".
##' 
##' The dates are expected to be a \code{data.frame} with the first column
##' in the \code{dates} format, though the function will also accept a
##' minimum and maximum date, and take care of constructing the appropriate
##' data frame (this is the recommended use). 
##'
##' @title syndromicD-function
##' 
##' @param observed A \code{matrix} with the observed data
##' @param dates A \code{data.frame} with the complete dates of of each 
##' time point (each row in the matrix provided in \code{observed}).
##' @param min.date Alternative to providing dates: the user can specify the
##' minimum date, and the maximum date, and the function will generate
##' a complete sequence of days between them. This is the recommended use
##' of the function because then additional date variables will also be created
##' and assigned to the slot dates, such as day of the week, month, year, 
##' is.weekend, etc. Note that the sequence must match the number of rows
##' in the matrix \code{observed}.
##' @param max.date see min.date above
##' @param date.format The date.format of the min.date and max.date provided.
##' Default is "%d/%m/%Y". See strptime() for format specifications.
##' @param weekends An optional argument, by default set to TRUE, meaning
##' weekends will be included in the sequence of dates generated.
##' If observed data refers to weekdays only, then weekends should be set to FALSE,
##' and weekends wil be removed.
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
##' 
##' @name syndromicD-function
##' @return an object of the class \code{syndromicD} with the slots
##' corresponding to the parameters described. See \code{class-syndromicD}
##'  for more details.
##' 
##' @import ISOweek
##' 
##' @export
##' @examples
##' ## Load data
##' data(observed)
##' my.syndromicD <- syndromicD(observed[1:5,]) 
##' my.syndromicD <- syndromicD(observed[1:5,],min.date="01/01/2010",max.date="05/01/2010")
##' my.syndromicD <- syndromicD(observed[1:6,],min.date="01/01/2010",max.date="08/01/2010", 
##'                           weekends=FALSE) 
##' dates = seq(from=as.Date("01/01/2010",format ="%d/%m/%Y" ),
##'               to=as.Date("05/01/2010",format ="%d/%m/%Y" ), 
##'               by="days")
##' my.syndromicD <- syndromicD(observed[1:5,],dates=dates) 
##'
##'


syndromicD <- function(observed, 
                      dates=data.frame(), 
                      min.date=NULL, 
                      max.date=NULL, 
                      date.format="%d/%m/%Y", 
                      weekends=TRUE, 
                      baseline=matrix(nrow=0,ncol=0), 
                      alarms=array(dim=0), 
                      UCL=array(dim=0), 
                      LCL=array(dim=0)) {
  
  if (!missing("dates"))(dates <- as.data.frame(dates))
  if (!missing("dates"))(dates[,1]<-as.Date (dates[,1], format = date.format))
  
  if (missing("dates")&&!missing("min.date")&&!missing("max.date")) {

    dates <- dates_df(min.date=min.date,max.date=max.date, 
                          by="days",
                          date.format = date.format)    
    
      
      if (weekends==FALSE){
    dates   <- dates[dates$weekday==TRUE,]
      }
  }
      
  
  new('syndromicD', observed=observed, dates=dates, 
      baseline=baseline, alarms=alarms, UCL=UCL, LCL=LCL)
}



setGeneric("setDatesD<-",function(object,value){standardGeneric("setDatesD<-")})
 setReplaceMethod(
   f="setDatesD",
   signature="syndromicD",
   definition=function(object,value){
     object@dates <- value
     validObject(object)      #VALIDITY CONTROL
     return(object)
     }
   )

#setDates(test0)<-as.data.frame(dates)
#test0


setGeneric("setBaselineD<-",function(object,value){standardGeneric("setBaselineD<-")})
setReplaceMethod(
  f="setBaselineD",
  signature="syndromicD",
  definition=function(object,value){
    object@baseline <- value
    validObject(object)      #VALIDITY CONTROL
    return(object)
  }
)

setGeneric("setAlarmsD<-",function(object,value){standardGeneric("setAlarmsD<-")})
setReplaceMethod(
  f="setAlarmsD",
  signature="syndromicD",
  definition=function(object,value){
    object@alarms <- value
    validObject(object)      #VALIDITY CONTROL
    return(object)
  }
)

setGeneric("setUCLD<-",function(object,value){standardGeneric("setUCLD<-")})
setReplaceMethod(
  f="setUCLD",
  signature="syndromicD",
  definition=function(object,value){
    object@UCL <- value
    validObject(object)      #VALIDITY CONTROL
    return(object)
  }
)


setGeneric("setLCLD<-",function(object,value){standardGeneric("setLCLD<-")})
setReplaceMethod(
  f="setLCLD",
  signature="syndromicD",
  definition=function(object,value){
    object@LCL <- value
    validObject(object)      #VALIDITY CONTROL
    return(object)
  }
)
