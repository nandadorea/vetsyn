##' \code{dim}
##'
##' Get dimension information for a syndromic object.
##'
##' @name dim-methods
##' @aliases dim
##' @aliases dim-methods
##' @aliases dim,syndromic-method
##' @docType methods
##' @keywords methods
##' @export
##' @import methods
##' @return a vector with three items: (1) The number of time points monitored 
##' (the number of rows for all slots of the object); (2)The number of 
##' syndromic groups monitored, as determined by the number of columns in the slot
##' observed; (3) The number of detection algorithms used,
##' as determined by the third dimension of the slot alarms. 
##' 
##' @examples
##' data(lab.daily)
##' my.syndromic <- raw_to_syndromic (id=lab.daily$SubmissionID,
##'                                   syndromes.var=lab.daily$Syndrome,
##'                                   dates.var=lab.daily$DateofSubmission,
##'                                   date.format="%d/%m/%Y")
##' ## create a subset that only includes the first 10 time points
##' ## (all the slots are trimmed from rows 1 to 10)
##' ## note the use of "," instead of ":"
##' subset <- my.syndromic[1,10]
##'
setMethod('dim',
          signature(x = 'syndromic'),
          function (x)
      {
          cat(sprintf("****** Syndromic object:******\n"))
          cat(sprintf("Number of time points (rows) = %s \n",
                      dim(x@observed)[1]))
          cat(sprintf("Number of syndromes monitored = %s \n",
                      dim(x@observed)[2]))
          cat(sprintf("Number of detection algorithms used = %s \n",
                      dim(x@alarms)[3])  )
          return(c(dim(x@observed)[1],dim(x@observed)[2],dim(x@alarms)[3]))
      }
)
