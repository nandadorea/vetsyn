##' @name clean_baseline_perc-methods
##' @docType methods
##' @seealso \code{\link{syndromicW}}
##' @aliases clean_baseline_perc
##' @aliases clean_baseline_perc-methods
##' @aliases clean_baseline_perc,syndromicW-method
##' @export
##' @importFrom caTools runquantile

setGeneric('clean_baseline_perc',
           signature = 'x',
           function(x, ...) standardGeneric('clean_baseline_perc'))

setMethod('clean_baseline_perc',
          signature(x = 'syndromicW'),
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
