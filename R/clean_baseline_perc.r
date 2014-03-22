##' \code{clean_baseline_perc}
##'
##' Function to retrospectively remove possible outbreak signals and excessive
##' noise, producing an \code{outbreak free baseline} that will serve to
##' train outbreak-signal detection algorithms during prospective analysis.
##' 
##' The cleaning is non-parametric, based on moving
##' percentiles. The user sets a window of time points, around each time point,
##' which will be used to calculate the percentile set in the user in the argument
##' \code{limit}. Any observations falling outside that percentile are removed
##' and substituted by the percentile itself. See examples and references.
##'
##' @name clean_baseline_perc-methods
##' @docType methods
##' @seealso \code{\link{syndromic}}
##' @aliases clean_baseline_perc
##' @aliases clean_baseline_perc-methods
##' @aliases clean_baseline_perc,syndromic-method
##'
##' @param x a \code{syndromic} object, which must have at least 
##' the slot of observed data and a data.frame in the slot dates.
##' @param syndromes an optional parameter, if not specified, all
##' columns in the slot \code{observed} of the \code{syndromic} object
##' will be used. The user can choose to restriict the analyses to 
##' a few syndromic groups listing their name or column position
##' in the \code{observed} matrix. See examples.
##' @param limit the percentile to be used in identifying outliers.
##' @param run.window the number of time points to construct the moving
##' percentile window. By default 120 days.
##' @param plot whether plots comparing observed data and the result of 
##' the cleaning process should be displayed.
##' 
##' @return An object of the class syndromic which contains all 
##' elements form the object provided in x, but in which
##' the slot \code{baseline} has been filled with an outbreak-free baseline
##' for each syndromic group. When the user chooses to restrict analyses to some 
##' syndromes, the remaining columns are kept as is (if the slot was not empty)
##' or filled with zeros when previously empty.
##' 
##' @keywords methods
##' @export
##' @importFrom caTools runquantile
##' @references Fernanda C. Dorea, Crawford W. Revie, Beverly J. McEwen, 
##' W. Bruce McNab, David Kelton, Javier Sanchez (2012). Retrospective 
##' time series analysis of veterinary laboratory data: 
##' Preparing a historical baseline for cluster detection in syndromic 
##' surveillance. Preventive Veterinary Medicine. 
##' DOI: 10.1016/j.prevetmed.2012.10.010.
##' @examples
##'data(lab.daily)
##'my.syndromic <- raw_to_syndromic (id=SubmissionID,
##'                                  syndromes.var=Syndrome,
##'                                  dates.var=DateofSubmission,
##'                                  date.format="%d/%m/%Y",
##'                                  merge.weekends=TRUE,
##'                                  data=lab.daily)
##'my.syndromic <- clean_baseline_perc(my.syndromic)
##'my.syndromic <- clean_baseline_perc(my.syndromic,run.window=90)
##'my.syndromic <- clean_baseline_perc(my.syndromic,
##'                               syndromes="Musculoskeletal")
##'my.syndromic <- clean_baseline_perc(my.syndromic,
##'                               syndromes=c("GIT","Musculoskeletal"))
##'my.syndromic <- clean_baseline_perc(my.syndromic,
##'                               syndromes=3)
##'my.syndromic <- clean_baseline_perc(my.syndromic,
##'                               syndromes=c(1,3))



setGeneric('clean_baseline_perc',
           signature = 'x',
           function(x, ...) standardGeneric('clean_baseline_perc'))

setMethod('clean_baseline_perc',
          signature(x = 'syndromic'),
          function (x,
                    syndromes=NULL,
                    limit=0.95,
                    run.window=120,
                    plot=TRUE)
{
            
            ##check that syndromes is valid
            if (class(syndromes)=="NULL"){
              syndromes <- colnames(x@observed)
            }else{
              if (class(syndromes)!="character"&&class(syndromes)!="numeric") {
                stop("if provided, argument syndromes must be a character or numeric vector")
              }
            }
                      
            #make sure syndrome list is always numeric
            #even if user gives as a list of names
            if (class(syndromes)=="numeric") {
              syndromes.num <- syndromes
            }else{
              syndromes.num <- match(syndromes,colnames(x@observed))
            }
            
            #pulling data form the object to work out of the object          
            observed.matrix=x@observed
            
            #filling baseline with NA only if completely empty before hand
            baseline.matrix <- x@baseline
            if (dim(baseline.matrix)[1]==0){
              baseline.matrix <- matrix(NA,ncol=dim(x@observed)[2],nrow=dim(x@observed)[1],
                                        dimnames=dimnames(x@observed))
            }
            
            #only for the syndromes to be worked out here,
            #adding data form observed which is only modified if an
            #aberration is detected
            baseline.matrix[,syndromes.num] <- observed.matrix[,syndromes.num]
            
            #require(caTools)
            
            loop=0
            for (c in syndromes.num){      
              loop=loop+1
              days = observed.matrix[,c]
              
              limitV <- runquantile(days, run.window, 
                                    probs=limit, endrule="quantile")
            
              
              
              peaks <- which(days > round(limitV))
              x.smooth <- days
              x.smooth [peaks] <- round(limitV[peaks])
              
              
 
                syndrome.name <- colnames(observed.matrix)[c]
                            
              
              if (plot==TRUE) {
                
                if (loop==1){
                  par(mfrow=c(length(syndromes),1),mar=c(2,4,2,2))}
                
                plot(days, x=x@dates[,1], type="l",ylab=syndrome.name)
                lines(x.smooth,x=x@dates[,1], col="red")
                legend("topleft", pch=3,col=c("black","red"),
                       c("Original series","Series with outliers removed"))
              }       
              
              baseline.matrix[,c] <- x.smooth
              
            }
            
            y <- x
            setBaseline(y) <- baseline.matrix
            return(y)
            
          }
)
