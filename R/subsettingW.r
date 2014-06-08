##' Sub-setting
##'
##' Allows sub-setting of a \code{\link{syndromicW}} object
##' and all of its slots, by a start and end number of rows.
##'
##'
##' @name "["-methods
##' @docType methods
##' @keywords methods
##' @export
##' @import methods
##' @aliases subset 
##' @aliases subset-methods 
##' @aliases subset,syndromicW-method
##' @examples
##' data(lab.daily)
##' my.syndromicW <- rawD_to_syndromicW (id=lab.daily$SubmissionID,
##'                                   syndromes.var=lab.daily$Syndrome,
##'                                   dates.var=lab.daily$DateofSubmission,
##'                                   date.format="%d/%m/%Y")
##' ## create a subset that ony includes the first 10 time points
##' ## (all the slots are trimmed from rows 1 to 10)
##' ## note the use of "," instead of ":"
##' subset <- my.syndromicW[1,10]
##'


setMethod(
  f= "[",
  signature="syndromicW",
  definition=function(x,i,j){
    
    new.syndromic = syndromicW(observed=x@observed[i:j,], dates=x@dates[i:j,])
    
        
    if (length(x@baseline)!=0){
      setBaselineW(new.syndromic)<-x@baseline[i:j,]
    }
    
    if (length(x@alarms)!=0){
      setAlarmsW(new.syndromic)<-x@alarms[i:j,,]
    }
    
    if (length(x@UCL)!=0){
      setUCLW(new.syndromic)<-x@UCL[i:j,,]
    }
    
    if (length(x@LCL)!=0){
      setLCLW(new.syndromic)<-x@LCL[i:j,,]
    }
    
    return(new.syndromic)
    
  }
)

