##' \code{clean_baseline}
##'
##' Function to retrospectively remove possible outbreak signals and excessive
##' noise, producing an \code{outbreak free baseline} that will serve to
##' train outbreak-signal detection algorithms during prospective analysis.
##' 
##' The cleaning is based on fitting the complete time series using regression methods 
##' (by default Poisson regression, but any other glm family is accepted,
##' extended to negative binomial using the package fitdistrplus), and then removing 
##' any observations that fall outside a given confidence interval 
##' (set by the user). These observations are substituted by the model
##' prediction for that time point.
##'
##' @name clean_baseline-methods
##' @docType methods
##' @seealso \code{\link{syndromic}}
##' @aliases clean_baseline
##' @aliases clean_baseline-methods
##' @aliases clean_baseline,syndromic-method
##'
##' @param x a \code{syndromic} object, which must have at least 
##' the slot of observed data and a data.frame in the slot dates.
##' @param syndromes an optional parameter, if not specified, all
##' columns in the slot \code{observed} of the \code{syndromic} object
##' will be used. The user can choose to restrict the analyses to 
##' a few syndromic groups listing their name or column position
##' in the \code{observed} matrix. See examples.
##' @param family the GLM distribution family used, by default 
##' \code{"poisson"}. if \code{"nbinom"} is used, the function
##' glm.nb is used instead.
##' @param limit the confidence interval to be used in identifying outliers.
##' @param formula the regression formula to be used. The following arguments
##' are accepted: trend (for a monotonic trend), month, dow (day of week),
##' sin, cos, Ar1 (auto-regressive for 1 days) to AR7. These elements can be combined
##' into any formula. The default is formula="dow+sin+cos+Ar1+Ar2+AR3+AR4+AR5". See examples. 
##' @param plot whether plots comparing observed data and the result of 
##' the cleaning process should be displayed.
##' @param print.model whether the result of model fitting should be
##' printed on the console. This is recommended when the user is 
##' exploring which dependent variables to keep or drop.
##' 
##' @return An object of the class \code{syndromic} which contains all 
##' elements from the object provided in x, but in which
##' the slot \code{baseline} has been filled with an outbreak-free baseline
##' for each syndromic group. When the user chooses to restrict analyses to some 
##' syndromes, the remaining columns are kept as is (if the slot was not empty)
##' or filled with zeros when previously empty.
##' 
##' @keywords methods
##' @export
##' @importFrom fitdistrplus fitdist
##' @importFrom MASS glm.nb
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
##'                                  remove.dow=c(6,0),
##'                                  add.to=c(2,1),                                  
##'                                  data=lab.daily)
##'my.syndromic <- clean_baseline(my.syndromic)
##'my.syndromic <- clean_baseline(my.syndromic, formula="dow+month+year")
##'my.syndromic <- clean_baseline(my.syndromic, formula="dow+sin+cos+trend")
##'my.syndromic <- clean_baseline(my.syndromic,
##'                               syndromes="Musculoskeletal")
##'my.syndromic <- clean_baseline(my.syndromic,
##'                               syndromes=c("GIT","Musculoskeletal"))
##'my.syndromic <- clean_baseline(my.syndromic,
##'                               syndromes=3)
##'my.syndromic <- clean_baseline(my.syndromic,
##'                               syndromes=c(1,3))
##'
##'my.syndromic <- clean_baseline(my.syndromic,
##'                               family="nbinom")
##'my.syndromic <- clean_baseline(my.syndromic,
##'                               syndromes="Musculoskeletal",family="nbinom")
##'my.syndromic <- clean_baseline(my.syndromic,
##'                               syndromes=c("GIT","Musculoskeletal"),family="nbinom")
##'my.syndromic <- clean_baseline(my.syndromic,
##'                               syndromes=3,family="nbinom")
##'my.syndromic <- clean_baseline(my.syndromic,
##'                               syndromes=c(1,3),family="nbinom")

setGeneric('clean_baseline',
           signature = 'x',
           function(x, ...) standardGeneric('clean_baseline'))

setMethod('clean_baseline',
          signature(x = 'syndromic'),
          function (x,
                    syndromes=NULL,
                    family="poisson",
                    limit=0.95,
                    formula="dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7",
                    plot=TRUE,
                    print.model=TRUE)
        {
    
      ##check that syndromes is valid
       if (class(syndromes)=="NULL"){
         syndromes <- colnames(x@observed)
         }else{
         if (class(syndromes)!="character"&&class(syndromes)!="numeric") {
     stop("if provided, argument syndromes must be a character or numeric vector")
           }
         }
       
       ##check that valid dates are entered
       if (dim(x@observed)[1]!=dim(x@dates)[1]){
         stop("valid data not found in the slot dates")
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
      
       period<-365
      if (length(which(x@dates$weekday==0))==0){
        period<-260
      }
       
       
       loop=0
      for (c in syndromes.num){      
       loop=loop+1
        
        days = observed.matrix[,c]
        t = 1:length(days)
        month = as.factor(x@dates$month)
        dow <- as.factor(x@dates$dow)
        cos = cos (2*pi*t/period)
        sin = sin (2*pi*t/period)
        year <- as.factor(x@dates$year)
        AR1<-c(days[1],days[1:(length(days)-1)])
        AR2<-c(days[1:2],days[1:(length(days)-2)])
        AR3<-c(days[1:3],days[1:(length(days)-3)])
        AR4<-c(days[1:4],days[1:(length(days)-4)])
        AR5<-c(days[1:5],days[1:(length(days)-5)])
        AR6<-c(days[1:6],days[1:(length(days)-6)])
        AR7<-c(days[1:7],days[1:(length(days)-7)])
        trend=t

        fn.formula=as.formula(paste0("days~",formula))
      
      
      if (family=="nbinom"){
    #    require(MASS)
    #    require(fitdistrplus)
        fit1     <- glm.nb(fn.formula)  
        predict1 <- predict(fit1, type="response", se.fit=TRUE)
        nbin     <- fitdist(days,"nbinom")
        series   <- predict1$fit
        se       <- predict1$se.fit
        limitV  <- qnbinom(p=limit, 
                           mu=predict1$fit,size=nbin$estimate[[1]], 
                           lower.tail = TRUE, log.p = FALSE)    
      }else{
        distribution=family            
        
        fit1 <- glm(fn.formula, distribution)
        
        predict1 <- predict(fit1, type="response", se.fit=TRUE)
        series   <- predict1$fit
        se       <- predict1$se.fit
        limitV   <- qpois(p=limit, lambda=predict1$fit, 
                         lower.tail = TRUE, log.p = FALSE)
      }
      
      
       #detecting and substituting aberrations
      peaks <- which(days > round(limitV))
      x.smooth <- days
      x.smooth [peaks] <- round(limitV [peaks])
      
        
        
          syndrome.name <- colnames(observed.matrix)[c]
         
        ##plotting and prinitng    
        if (print.model==TRUE){
              print(syndrome.name)
              print(fit1)    
            }
      
          
        if (plot==TRUE) {
          
          if (loop==1){
            par(mfrow=c(length(syndromes),1),mar=c(2,4,2,2))}
          
          plot(days, x=x@dates[,1],type="l",ylab=syndrome.name)
          lines(x.smooth,,x=x@dates[,1], col="red")
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
