##' Function to retrospectively remove possible outbreak signals and excessive
##' noise, producing an outbreak free baseline that will serve to
##' train outbreak-signal detection algorithms during prospective analysis.
##'
##' The cleaning is based on fitting the complete time series using regression methods
##' (by default Poisson regression, but any other glm family is accepted,
##' extended to negative binomial using the package fitdistrplus), and then removing
##' any observations that fall outside a given confidence interval
##' (set by the user). These observations are substituted by the model
##' prediction for that time point.
##'
##' @rdname clean_baseline-methods
##' @docType methods
##'
##' @param x a syndromic (\code{syndromicD} or \code{syndromicW}) object,
##' which must have at least
##' the slot of observed data and a data frame in the slot dates.
##' @param ... Additional arguments to the method.
##' @param syndromes an optional parameter, if not specified, all
##' columns in the slot observed of the syndromic object
##' will be used. The user can choose to restrict the analyses to
##' a few syndromic groups listing their name or column position
##' in the observed matrix. See examples.
##' @param family the GLM distribution family used, by default
##' "poisson". if "nbinom" is used, the function
##' glm.nb is used instead.
##' @param limit the confidence interval to be used in identifying outliers.
##' @param formula the regression formula to be used, in the R formula format: y~x1+x2...
##' If none is provided, the function looks for formulas in the @formula slot of the syndromic object.
##' If a formula is provided when this function is called, then that formula is used. We
##' recommend providing a formula to test various models, but once a model is chosen,
##' we recommend saving that formula in the syndromic object using: 
##' my.syndromic@formula <- list(formula1,formula2...), for as many syndromes as the syndromic object has (columns in observed).
##' NA can be provided when a syndrome is not to be associated with a particular formula.
##' Any variables (x1, x2...) must be given the same name they have in the
##' slot @dates. When providing a formula, two options are possible: providing a single formula to be applied to all 
##' syndromes, or providing the same number of formulas (in a list) as the number of syndromes in the observed object, 
##' even if not of them will be used (see examples!)
##' The variables that are standard in that slot for DAILY data (\code{syndromicD}) are: 
##' trend (for a monotonic trend), year, month, dow (day of week),
##' sin, cos, Ar1 (auto-regressive for 1 days) to AR7. For WEEKLY data
##' (\code{syndromicW}): trend, sin, cos, year, and 1 to 4 
##' autoregressive variables.
##' These elements can be combined into any formula. 
##' Since the @dates slot can be customized by the user, any variables in the dates 
##' data.frame can be called into the formula
##' ##' @param frequency the frequency of repetition in the data, by default
##' one year (365 for DAILY data (object provided belongs to the class
##' \code{syndromicD}) and 52 for WEEK data (object provided belongs to the class
##' \code{syndromicW}))
##' @param plot whether plots comparing observed data and the result of
##' the cleaning process should be displayed.
##' @param print.model whether the result of model fitting should be
##' printed on the console. This is recommended when the user is
##' exploring which dependent variables to keep or drop.
##'
##' @return An object of the class syndromic (\code{syndromicD} or \code{syndromicW})
##'  which contains all
##' elements from the object provided in x, but in which
##' the slot baseline has been filled with an outbreak-free baseline
##' for each syndromic group. When the user chooses to restrict analyses to some
##' syndromes, the remaining columns are kept as is (if the slot was not empty)
##' or filled with NAs when previously empty.
##'
##' @keywords methods
##' @importFrom fitdistrplus fitdist
##' @importFrom MASS glm.nb
##' @references Fernanda C. Dorea, Crawford W. Revie, Beverly J. McEwen,
##' W. Bruce McNab, David Kelton, Javier Sanchez (2012). Retrospective
##' time series analysis of veterinary laboratory data:
##' Preparing a historical baseline for cluster detection in syndromic
##' surveillance. Preventive Veterinary Medicine.
##' DOI: 10.1016/j.prevetmed.2012.10.010.
##' @examples
##' ## Examples for 'syndromicD'
##'data(lab.daily)
##'my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
##'                                  syndromes.var=Syndrome,
##'                                  dates.var=DateofSubmission,
##'                                  date.format="%d/%m/%Y",
##'                                  remove.dow=c(6,0),
##'                                  add.to=c(2,1),
##'                                  data=lab.daily)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                                 formula=list(days~dow+month+year),
##'                                 frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                                 formula=list(days~dow+month+year),
##'                                 frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD, 
##'                                 formula=list(days~dow+month+year),
##'                                 frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                               syndromes="Musculoskeletal",
##'                               formula=list(days~dow+month+year),
##'                               frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                               syndromes=c("GIT","Musculoskeletal"),
##'                               formula=list(NA,y~dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7,
##'                               days~dow+month,NA,NA),
##'                               frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                               syndromes=3,
##'                               formula=list(NA,y~dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7,
##'                               days~dow+month,NA,NA),
##'                               frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                               syndromes=c(2,3),
##'                               formula=list(NA,y~dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7,
##'                               days~dow+month,NA,NA)),
##'                               frequency=260)
##'
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                               family="nbinom",
##'                               formula=list(days~dow+month),
##'                               frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                               syndromes="Musculoskeletal",
##'                               family="nbinom",
##'                               formula=list(y~dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7),
##'                               frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                               syndromes=c("GIT","Musculoskeletal"),
##'                               family="nbinom",
##'                               formula=list(NA,y~dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7,
##'                               days~dow+month,NA,NA),
##'                               frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                               syndromes=3,
##'                               family="nbinom",
##'                               formula=list(days~dow+month),
##'                               frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                               syndromes=c(2,3),
##'                               family="nbinom",
##'                               formula=list(days~dow+month),
##'                               frequency=260)
##'
##' ## Examples for 'syndromicW'
##'data(lab.daily)
##'my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
##'                                  syndromes.var=Syndrome,
##'                                  dates.var=DateofSubmission,
##'                                  date.format="%d/%m/%Y",
##'                                  formula=list(NA,y~year,weeks~trend+sin+cos,NA,NA)
##'                                  data=lab.daily)
##'my.syndromicW <- clean_baseline(my.syndromicW,formula=list(NA,y~year,weeks~trend+sin+cos,NA,NA))
##'my.syndromicW <- clean_baseline(my.syndromicW, formula=list(week~sin+cos))
##'my.syndromicW <- clean_baseline(my.syndromicW,
##'                               syndromes="Musculoskeletal",
##'                               formula=list(week~sin+cos))
##'my.syndromicW <- clean_baseline(my.syndromicW,
##'                               syndromes=c("GIT","Musculoskeletal"),
##'                               formula=list(week~sin+cos))
##'my.syndromicW <- clean_baseline(my.syndromicW,
##'                               syndromes=3,
##'                               formula=list(week~sin+cos))
##'my.syndromicW <- clean_baseline(my.syndromicW,
##'                               syndromes=c(1,3),
##'                               formula=list(NA,y~year,weeks~trend+sin+cos,NA,NA))
##'
##'my.syndromicW <- clean_baseline(my.syndromicW,
##'                               family="nbinom",
##'                               formula=list(week~sin+cos))
##'my.syndromicW <- clean_baseline(my.syndromicW,
##'                               syndromes="Musculoskeletal",family="nbinom",
##'                               formula=list(week~sin+cos))

setGeneric('clean_baseline',
           signature = 'x',
           function(x, ...) standardGeneric('clean_baseline'))

##' @rdname clean_baseline-methods
##' @export
setMethod('clean_baseline',
          signature(x = 'syndromicD'),
          function (x,
                    syndromes=NULL,
                    family="poisson",
                    limit=0.95,
                    formula=NULL,
                    frequency=365,
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
     if (is.null(syndromes)){
       syndromes<-1:dim(x@observed)[2]
     }
     
       if (class(syndromes)=="numeric"|class(syndromes)=="integer") {
       syndromes.num <- syndromes
       }else{
         syndromes.num <- match(syndromes,colnames(x@observed))
       }

            #checking that a formula is available
            if (class(formula)=="NULL"&&length(x@formula)<dim(x@observed)[2]){
              stop("the number of regression formulas saved on the syndromic object,
                   on @formula, is not equal to the number of syndromes in the object,
                   and no formula(s) have been provided in the function call")
            }
            
            
            
            
       #pulling data from the object to work out of the object
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

     #now frequency
#        period<-365
#       if (length(which(x@dates$weekday==0))==0){
#         period<-260
#       }


       loop=0
      for (c in syndromes.num){
       loop=loop+1

       days = observed.matrix[,c]
       t = 1:length(days)
       cos = cos (2*pi*t/frequency)
       sin = sin (2*pi*t/frequency)
       AR1<-c(days[1],days[1:(length(days)-1)])
       AR2<-c(days[1:2],days[1:(length(days)-2)])
       AR3<-c(days[1:3],days[1:(length(days)-3)])
       AR4<-c(days[1:4],days[1:(length(days)-4)])
       AR5<-c(days[1:5],days[1:(length(days)-5)])
       AR6<-c(days[1:6],days[1:(length(days)-6)])
       AR7<-c(days[1:7],days[1:(length(days)-7)])
       
       var <- data.frame(days = days,
                         trend=t,
                         cos,sin,
                         AR1,AR2,AR3,AR4,AR5,AR6,AR7)
       var <- cbind(var,x@dates)
       var$month <- as.factor(var$month)
       var$dow <- as.factor(var$dow)
       var$year <- as.factor(var$year)
       
       
       if(class(formula)=="NULL"){
         formula.c <- x@formula[[c]]
       }else{
         if(length(formula)>1){
           formula.c <- formula[[c]]
         }  else{
           formula.c<-formula[[1]]
         } 
       }
       
       
       v<-all.vars(formula.c)
       v[[1]]<-NA
       v<-v[!is.na(v)]
       m<-match(v,colnames(var))
       m<-m[!is.na(m)]
       
       var <- var[,m,drop=FALSE]
       
       if(length(v)==0){
         stop("Formula assignment did not work properly, check that
              you have provided the formula as a list, even if with only a single object,
              or if a true list is being provided, make sure there is a formula for 
              each of the syndromes in the syndromic object (you can assign NA as the formula for 
              syndromes you are not trying to evaluate, but you still need to provide 
              a formula for each syndrome, unless you provide a list with a single formula.
              See examples in the help for thie function")
       }else{
         fn.formula=as.formula(paste0("days~",paste0(v,collapse="+")))
       }


      if (family=="nbinom"){
    #    require(MASS)
    #    require(fitdistrplus)
        fit1     <- glm.nb(fn.formula, data=var)
        predict1 <- predict(fit1, type="response", se.fit=TRUE)
        nbin     <- fitdist(days,"nbinom")
        series   <- predict1$fit
        se       <- predict1$se.fit
        limitV  <- qnbinom(p=limit,
                           mu=predict1$fit,size=nbin$estimate[[1]],
                           lower.tail = TRUE, log.p = FALSE)
      }else{
        distribution=family

        fit1 <- glm(fn.formula, distribution, data=var)

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
      setBaselineD(y) <- baseline.matrix
      return(y)

          }
)

##' @rdname clean_baseline-methods
##' @export
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
     if (is.null(syndromes)){
       syndromes<-1:dim(x@observed)[2]
     }
       if (class(syndromes)=="numeric"|class(syndromes)=="integer") {
       syndromes.num <- syndromes
       }else{
         syndromes.num <- match(syndromes,colnames(x@observed))
       }
            
            #checking that a formula is available
            if (class(formula)=="NULL"&&length(x@formula)<dim(x@observed)[2]){
              stop("the number of regression formulas saved on the syndromic object,
                   on @formula, is not equal to the number of syndromes in the object,
                   and no formula(s) have been provided in the function call")
            }

       #pulling data from the object to work out of the object
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
       t = 1:length(week)
       cos = cos (2*pi*t/frequency)
       sin = sin (2*pi*t/frequency)
       
       AR1<-c(week[1],week[1:(length(week)-1)])
       AR2<-c(week[1:2],week[1:(length(week)-2)])
       AR3<-c(week[1:3],week[1:(length(week)-3)])
       AR4<-c(week[1:4],week[1:(length(week)-4)])
       
       var <- data.frame(week = week,
                         trend=t,
                         cos,sin,
                         AR1,AR2,AR3,AR4)
       var <- cbind(var,x@dates)
       var$year <- as.factor(var$year)
       
       
       
       if(class(formula)==NULL){
         formula.c <- x@formula[[c]]
       }else{
         if(length(formula)>1){
           formula.c <- formula[[c]]
         }  else{
           formula.c<-formula[[1]]
         }
       }
       
       
       v<-all.vars(formula.c)
       v[[1]]<-NA
       v<-v[!is.na(v)]
       m<-match(v,colnames(var))
       m<-m[!is.na(m)]
       
       var <- var[,m,drop=FALSE]
       
       
       if(length(v)==0){
         stop("Formula assignment did not work properly, check that
              you have provided the formula as a list, even if with only a single object,
              or if a true list is being provided, make sure there is a formula for 
              each of the syndromes in the syndromic object (you can assign NA as the formula for 
              syndromes you are not trying to evaluate, but you still need to provide 
              a formula for each syndrome, unless you provide a list with a single formula.
              See examples in the help for thie function")
       }else{
         fn.formula=as.formula(paste0("week~",paste0(v,collapse="+")))
       }
       
      if (family=="nbinom"){
    #    require(MASS)
    #    require(fitdistrplus)
        fit1     <- glm.nb(fn.formula, data=var)
        predict1 <- predict(fit1, type="response", se.fit=TRUE)
        nbin     <- fitdist(week,"nbinom")
        series   <- predict1$fit
        se       <- predict1$se.fit
        limitV  <- qnbinom(p=limit,
                           mu=predict1$fit,size=nbin$estimate[[1]],
                           lower.tail = TRUE, log.p = FALSE)
      }else{
        distribution=family

        fit1 <- glm(fn.formula, distribution, data=var)

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

