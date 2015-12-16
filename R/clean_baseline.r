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
##' @param formula the regression formula to be used. The following arguments
##' are accepted for DAILY data (\code{syndromicD}): trend (for a monotonic trend),
##' month, dow (day of week), year,
##' sin, cos, AR1 (auto-regressive for 1 days) to AR7. These elements can be combined
##' into any formula. The default is formula="dow+sin+cos+AR1+AR2+AR3+AR4+AR5". See examples.
##' @param frequency the frequency of repetition in the data, by default
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
##'                                 frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                                 formula="dow+month+year",
##'                                 frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD, formula="dow+sin+cos+trend",
##'                                 frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                               syndromes="Musculoskeletal",
##'                               frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                               syndromes=c("GIT","Musculoskeletal"),
##'                               frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                               syndromes=3,
##'                               frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                               syndromes=c(1,3),
##'                               frequency=260)
##'
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                               family="nbinom",
##'                               frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                               syndromes="Musculoskeletal",
##'                               family="nbinom",
##'                               frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                               syndromes=c("GIT","Musculoskeletal"),
##'                               family="nbinom",
##'                               frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                               syndromes=3,
##'                               family="nbinom",
##'                               frequency=260)
##'my.syndromicD <- clean_baseline(my.syndromicD,
##'                               syndromes=c(1,3),
##'                               family="nbinom",
##'                               frequency=260)
##'
##' ## Examples for 'syndromicW'
##'data(lab.daily)
##'my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
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
                    formula="dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7",
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

     #now frequency
#        period<-365
#       if (length(which(x@dates$weekday==0))==0){
#         period<-260
#       }


       loop=0
      for (c in syndromes.num){
       loop=loop+1

        days = observed.matrix[,c]
        trend = 1:length(days)
        month = as.factor(x@dates$month)
        dow <- as.factor(x@dates$dow)
        cos = cos (2*pi*trend/frequency)
        sin = sin (2*pi*trend/frequency)
        year <- as.factor(x@dates$year)
        AR1<-c(days[1],days[1:(length(days)-1)])
        AR2<-c(days[1:2],days[1:(length(days)-2)])
        AR3<-c(days[1:3],days[1:(length(days)-3)])
        AR4<-c(days[1:4],days[1:(length(days)-4)])
        AR5<-c(days[1:5],days[1:(length(days)-5)])
        AR6<-c(days[1:6],days[1:(length(days)-6)])
        AR7<-c(days[1:7],days[1:(length(days)-7)])


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

### testing added from nandadorea RStudio 2015-12-16
