##' Show
##'
##' Shows information for a syndromic object (\code{syndromicD} or 
##' \code{syndromicW}).
##'
##' @rdname show-methods
##' @docType methods
##' 
##' @section Methods: \describe{
##' \item{\code{signature(object = "syndromic")}}{
##' Show information for a syndromic object.
##' }
##' }
##' @param object a syndromic object
##' @keywords methods
##' @import methods
##' 
##' @rdname show-methods
##' @export

setMethod("show",
          signature(object = "syndromicD"),
          function (object)
      {
          cat(sprintf("****** Syndromic object:******\n"))
          cat(sprintf("Observed data for %s groups, over %s time points.\n",
                      dim(object@observed)[2],
                      dim(object@observed)[1]))
          cat(colnames(object@observed) ," ")
          
          if(length(object@dates)>0L){
            cat(sprintf("\n Start date:%s; End date:%s \n",
                        object@dates[1,1],
                        object@dates[dim(object@dates)[1],1]))
            } else {cat(sprintf("\n dates = EMPTY \n"))}
          
           if(length(object@baseline)>0L){
            cat(sprintf("Baseline data = AVAILABLE \n"))
          } else {cat(sprintf("Baseline data = EMPTY \n"))}
          
          if(length(object@alarms)>0L){
            cat(sprintf("Number of detection algorithms used = %s \n",
                        dim(object@alarms)[3] ))
          } else {cat(sprintf("Alarms outputs = EMPTY \n"))}
         
          if(length(object@UCL)>0L){
            cat(sprintf("Number of upper control limits (UCL) recorded = %s \n",
                        dim(object@UCL)[3] ))
            } else {cat(sprintf("Upper Control Limits (UCL) = EMPTY \n"))}
          
          if(length(object@LCL)>0L) {
            cat(sprintf("Number of lower control limits (LCL) recorded = %s \n",
                        dim(object@LCL)[3] ))
          } else {
            cat(sprintf("Lower Control Limits (LCL) = EMPTY \n"))
          }
          
          
      }
)


##' @rdname show-methods
##' @export
setMethod("show",
          signature(object = "syndromicW"),
          function (object)
      {
          cat(sprintf("****** Syndromic object (WEEKLY data):******\n"))
          cat(sprintf("Observed data for %s groups, over %s time points.\n",
                      dim(object@observed)[2],
                      dim(object@observed)[1]))
          cat(colnames(object@observed) ," ")
          
          if(length(object@dates)>0L){
            cat(sprintf("\n Start week:%s; End week:%s \n",
                        object@dates[1,1],
                        object@dates[dim(object@dates)[1],1]))
            } else {cat(sprintf("\n dates = EMPTY \n"))}
          
           if(length(object@baseline)>0L){
            cat(sprintf("Baseline data = AVAILABLE \n"))
          } else {cat(sprintf("Baseline data = EMPTY \n"))}
          
          if(length(object@alarms)>0L){
            cat(sprintf("Number of detection algorithms used = %s \n",
                        dim(object@alarms)[3] ))
          } else {cat(sprintf("Alarms outputs = EMPTY \n"))}
         
          if(length(object@UCL)>0L){
            cat(sprintf("Number of upper control limits (UCL) recorded = %s \n",
                        dim(object@UCL)[3] ))
            } else {cat(sprintf("Upper Control Limits (UCL) = EMPTY \n"))}
          
          if(length(object@LCL)>0L) {
            cat(sprintf("Number of lower control limits (LCL) recorded = %s \n",
                        dim(object@LCL)[3] ))
          } else {
            cat(sprintf("Lower Control Limits (LCL) = EMPTY \n"))
          }
          
          
      }
)
