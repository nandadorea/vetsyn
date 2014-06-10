##' @name clean_baseline
##' @docType methods
##' 
##' @param formula the regression formula to be used. The following arguments
##' are accepted for WEEK data (\code{syndromicW}): trend (for a monotonic trend), year,
##' sin, cos, AR1 (auto-regressive for 1 week) to AR4. These elements can be combined
##' into any formula. The default is formula="trend+sin+cos". See examples. 
##' 
##' @keywords methods
##' @export
##' @importFrom fitdistrplus fitdist
##' @importFrom MASS glm.nb
##' @import ISOweek
##' @examples
##'data(lab.daily)
##'my.syndromicW <- rawD_to_syndromicW (id=SubmissionID,
##'                                  syndromes.var=Syndrome,
##'                                  dates.var=DateofSubmission,
##'                                  date.format="%d/%m/%Y",
##'                                  data=lab.daily)
##'my.syndromicW <- clean_baseline(my.syndromicW)
##'my.syndromicW <- clean_baseline(my.syndromicW, formula="sin+cos")
##'my.syndromicW <- clean_baseline(my.syndromicW,
##'                               syndromes="Musculoskeletal")
##'my.syndromicW <- clean_baseline(my.syndromicW,
##'                               syndromes=c("GIT","Musculoskeletal"))
##'my.syndromicW <- clean_baseline(my.syndromicW,
##'                               syndromes=3)
##'my.syndromicW <- clean_baseline(my.syndromicW,
##'                               syndromes=c(1,3))
##'
##'my.syndromicW <- clean_baseline(my.syndromicW,
##'                               family="nbinom")
##'my.syndromicW <- clean_baseline(my.syndromicW,
##'                               syndromes="Musculoskeletal",family="nbinom")

setMethod('clean_baseline',
          signature(x = 'syndromicW'),
          function (x,
                    syndromes=NULL,
                    family="poisson",
                    limit=0.95,
                    formula="year+sin+cos",
                    plot=TRUE,
                    print.model=TRUE,
                    frequency=52)
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
      
 
       
       loop=0
      for (c in syndromes.num){      
       loop=loop+1
        
        week = observed.matrix[,c]
        trend = 1:length(week)
        cos = cos (2*pi*trend/frequency)
        sin = sin (2*pi*trend/frequency)
        year <- as.factor(x@dates$year)
        AR1<-c(week[1],week[1:(length(week)-1)])
        AR2<-c(week[1:2],week[1:(length(week)-2)])
        AR3<-c(week[1:3],week[1:(length(week)-3)])
        AR4<-c(week[1:4],week[1:(length(week)-4)])
        

        fn.formula=as.formula(paste0("week~",formula))
      
      
      if (family=="nbinom"){
    #    require(MASS)
    #    require(fitdistrplus)
        fit1     <- glm.nb(fn.formula)  
        predict1 <- predict(fit1, type="response", se.fit=TRUE)
        nbin     <- fitdist(week,"nbinom")
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
      peaks <- which(week > round(limitV))
      x.smooth <- week
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
          
          plot(week, x=ISOweek2date(x@dates[,1]),type="l",ylab=syndrome.name,xlab="")
          lines(x.smooth,,x=ISOweek2date(x@dates[,1]), col="red")
          legend("topleft", pch=3,col=c("black","red"),
                 c("Original series","Series with outliers removed"))
        }       
        
        baseline.matrix[,c] <- x.smooth
        
      }
      
      y <- x
      setBaselineW(y) <- baseline.matrix
      return(y)
       
          }
)
