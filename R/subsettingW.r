##' @name "["-methods
##' @docType methods
##' @keywords methods
##' @export
##' @import methods
##' @aliases subset 
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
      setBaseline(new.syndromic)<-x@baseline[i:j,]
    }
    
    if (length(x@alarms)!=0){
      setAlarms(new.syndromic)<-x@alarms[i:j,,]
    }
    
    if (length(x@UCL)!=0){
      setUCL(new.syndromic)<-x@UCL[i:j,,]
    }
    
    if (length(x@LCL)!=0){
      setLCL(new.syndromic)<-x@LCL[i:j,,]
    }
    
    return(new.syndromic)
    
  }
)

