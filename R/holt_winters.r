##' \code{holt_winters_synd}
##'
##' This function applies the \code{Holt-Winters()} algorithm (available in
##' the {stats} R package and loaded in the basic R 
##' installation)). This algorithm
##' is normally used for decomposition and forecasting, but here
##' it is employed as part of an iterative process to allow 
##' detection of outbreak signals. The additional features compared
##' to the basic HoltWinters() algorithm are: 
##' \itemize{
##'  \item{iterative application:} {  the algorithm is applied to a
##'  range of time points in an iterative manner, so if syndromic
##'  data needs to be evaluated for the past 30 days, for instance,
##'  the function is called once and the internal loops evaluate
##'  one day at a time.}
##'  \item{Detection of deviations:} {  in this implementation the
##'  n-days-ahead forecast of the algorithm is used as an 
##'  outbreak signal detector - observations above a confidence
##'  interval are flagged as "aberrations". }
##'  \item{baseline:}{  it is possible to point to an outbrak-free
##'  time series (baseline) which will serve to train the algorithm
##'  and calculate the forecast, which is then compared to the actual
##'  observed data that is being analysed}
##'  \item{recording of the detection limits:}{  the minimum value that 
##'  would have generated an alarm in each time point can be recorded
##'  in the UCL slot of the \code{syndromic} object provided}
##'  \item{data correction:}{  in case an observation is found to be greater
##'  than the confidence interval of the forecast, the user can 
##'  choose to update the outbreak-free baseline by substituting the
##'  observed value with the UCL value}
##'  \item{multiple limits:}{  the user can apply the algorithm with multiple
##'   detection limits - that is to say, different
##'  confidence intervals}
##'  }
##'
##' @param x a syndromic (\code{syndromicD} or \code{syndromicW})
##' object, which must have  
##' the slot of observed data, and a data frame containing the
##' variable  year in the slot dates. If the slot baseline is not 
##' available, then the data in the slot observed will be copied into
##' the baseline slot.
##' @param ... Additional arguments to the method.
##' @param syndromes an optional parameter, if not specified, all
##' columns in the slot observed of the \code{syndromic} object
##' will be used. The user can choose to restrict the analyses to 
##' a few syndromic groups listing their name or column position
##' in the observed matrix. See examples.
##' @param evaluate.window the number of time points to be evaluated.
##' By default only the last time point is evaluated, but the user can set 
##' any window (as long as the number of time points in the time series
##' allows so). Please note that the HoltWinters algorithm requires
##' at least two cycles to initialize. That is, if the cycle of the data 
##' is weekly, then the two initial weeks of the data cannot be included
##' in the analyses.
##' @param frequency The frequency of cycles in the time series. For DAILY data
##' (\code{syndromicD}): Even though
##' this is normally a year (frequency=365), we have found that the algorithm
##' works better with a frequency equal to the week (5 or 7 days, dependening
##' on whether weekends are included) when strong day-of-week effects are
##' present in the data.  
##' @param baseline.window The length of the 
##' baseline used to train the algorithm in order 
##' to provide a forecast, which will serve to decide whether the current 
##' observed data is expected. Normally 1-2 years.
##' @param limit.sd The limit of detection to be used, that is, the cut-off
##' of the confidence interval that decides when an observation is abnormal.
##' Rather than a percentage (for instance 95% or 99%), this should be informed
##' as number of standard deviations above the mean (for instance 2.5, or 3).
##' This is to ensure comparability with the other detection algorithms used iin 
##' this package (control charts). This can be provided as a single value or as 
##' a vector. When a vector is provided, multiple detection results are given.
##' In this case the result of detection is not binary (as traditionally, 0 for no
##' alarm detected and 1 for the detection of an aberration). Instead,
##' an integer is produced refering to how many confidence intervals have been
##' exceeded. If for instance the limits are set to c(2,2.5,3), then an observation 
##' which is greater than the 2.5 limit, but lower than 3, will have a detection score
##' of 2 (2 detection limits "broken"). 
##' @param alarm.dim The syndromic object is set to accept the result of 
##' multiple detection algorithms. These results are stored as a third 
##' dimension in the slot alarms. Here the user can choose which order in that 
##' dimension should store the results of this algorithm. If this is the first
##' aberration detection algorithm used, for instance, leave as the default (1).
##' @param nahead how many days ahead predictions should be made. One-day ahead
##' predictions may have narrower confidence intervals, but they are unsafe
##' as they do not protect the prediction from incoporating undetected 
##' outbreak signals. A one week ahead prediction (5 or 7 days) is recommended.
##' @param alpha the alpha parameter to be passed to the HoltWinters() algorithm.
##' @param beta the beta parameter to be passed to the HoltWinters() algorithm.
##' @param gamma the gamma parameter to be passed to the HoltWinters() algorithm.
##' @param seasonal the seasonal parameter to be passed to the 
##' HoltWinters() algorithm. Default is "additive". The user can change to 
##' "multiplicative", but that is not recommended if the data contains zeros.
##' @param correct.baseline besides detecting abnormal observations, the algorithm
##' can also be used to correct the data, removing these observations and 
##' substituting them by the limit of the confidence interval of the prediction
##' provided by the HoltWinters() algorithm. If that is to be carried out, 
##' the user needs to specify which of the provided limits is to be used
##' for the correction. This variable should be filled with the INDEX of the
##' limit to be used. For example, if limit.sd was provided as "c(2.5,3.0,3.5)", 
##' the use of correct.baseline=1 will cause the algorithm to substitute
##' any observations above 2.5 standard deviations from the predicted value with
##' this exact cut-off limit. If using correct.baseline=2, only observations
##' above a standard deviation of 3 (limit.sd[2]) will be corrected. To avoid that 
##' a baseline is generated or modified, set this argument to zero or NULL.
##' @param UCL the minimum number that would have geerated an alarm, for every time point,
##' can be recorded in the slot UCL of the \code{syndromic} object.The user must provide 
##' the INDEX in the limit.sd vector for which the UCL values should be corrected 
##' (as explained for the argument correct.baseline above). Set to FALSE to prevent
##' the recording.
##' 
##' @seealso HoltWinters
##' @seealso ewma_synd
##' @seealso shew_synd
##' @seealso cusum_synd
##' 
##' @return An object of the class syndromic (\code{syndromicD} or \code{syndromicW},
##' depending on which was provided as input) which contains all 
##' elements form the object provided in x, but in which
##' the slot \code{alarm} has been filled in the following way: for the
##' rows assigned in \code{evaluate.window}, columns indicated in
##'  \code{syndromes} (or all columns from observed if syndromes is left undefined),
##'  and for the third dimension specified in \code{alarm.dim} (1 by default), 
##'  zeros have been assigned if no alarm was generated; otherwise a numerical
##'  value gives the number of alarms detected. That is, how many of the
##'  limits given in \code{limits.sd} detected an abnormal observation. See examples.
##'  If the user sets a correct.baseline value, the baseline will also have 
##'  been modified.
##'  
##' @rdname holt_winters_synd-methods
##' @docType methods
##' 
##' @keywords methods
##' @import abind
##' @examples
##'data(lab.daily)
##'my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
##'                                  syndromes.var=Syndrome,
##'                                  dates.var=DateofSubmission,
##'                                  date.format="%d/%m/%Y",
##'                                  data=lab.daily)
##'my.syndromicD <- holt_winters_synd(x=my.syndromicD,
##'                             evaluate.window=30,
##'                             frequency=7,
##'                             baseline.window=365,
##'                             limit.sd=c(2.5,3,3.5), #default
##'                             nahead=7,
##'                             correct.baseline=2,
##'                             alarm.dim=1)
##'
##'my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
##'                                  syndromes.var=Syndrome,
##'                                  dates.var=DateofSubmission,
##'                                  date.format="%d/%m/%Y",
##'                                  remove.dow=c(6,0),
##'                                  add.to=c(2,1),
##'                                  data=lab.daily)
##'my.syndromicD <- holt_winters_synd(x=my.syndromicD,
##'                             syndromes="Musculoskeletal",
##'                             evaluate.window=30,
##'                             frequency=5,
##'                             baseline.window=260,
##'                             limit.sd=c(2.5,3,3.5), #default
##'                             nahead=5,
##'                             correct.baseline=2,
##'                             alarm.dim=1,
##'                             UCL=2)
##'                             
##'       ##WEEKLY
##'data(lab.daily)
##'my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
##'                                     syndromes.var=Syndrome,
##'                                     dates.var=DateofSubmission,
##'                                     date.format="%d/%m/%Y",
##'                                     data=lab.daily)
##'my.syndromicW <- holt_winters_synd(x=my.syndromicW,
##'                             evaluate.window=12,
##'                             frequency=52,
##'                             baseline.window=104,
##'                             limit.sd=c(2.5,3,3.5), #default
##'                             nahead=2,
##'                             correct.baseline=2,
##'                             alarm.dim=1)
                      

setGeneric('holt_winters_synd',
           signature = 'x',
           function(x, ...) standardGeneric('holt_winters_synd'))

##' @rdname holt_winters_synd-methods
##' @export

setMethod('holt_winters_synd',
          signature(x = 'syndromicD'),
          function (x,
                    syndromes=NULL,
                    evaluate.window=1,
                    frequency=7,
                    baseline.window=365,
                    limit.sd=c(2.5,3,3.5),
                    nahead=7,
                    alpha=0.4,
                    beta=0,
                    gamma=0.15,
                    seasonal="additive",
                    correct.baseline=1,
                    alarm.dim=1,
                    UCL=1
                    )
        {
    
            ##check that syndromes is valid
            if (class(syndromes)=="NULL"){
              syndromes <- colnames(x@observed)
            }else{
              if (class(syndromes)!="character"&&class(syndromes)!="numeric") {
                stop("if provided, argument syndromes must be a character or numeric vector")
              }
            }
            
            #syndromes index to be always numeric
            if (class(syndromes)=="numeric") {
              syndromes.num <- syndromes
            }else{
              syndromes.num <- match(syndromes,colnames(x@observed))
            }
            
           #pulling to a new object to modify directly in the object
    y <- x            

            #if slot alarms does not exist at all, fill it with NA
            #for the minmum dimensions required
            if (dim(y@alarms)[1]==0){
              setAlarmsD(y)<-array(NA,dim=c(dim(y@observed)[1],dim(y@observed)[2],alarm.dim))
                  if (length(dimnames(y@observed)[[2]])==1) {
                    dimnames(y@alarms)[[2]] <- list(dimnames(y@observed)[[2]])
                  } else{
                    dimnames(y@alarms)[[2]] <- dimnames(y@observed)[[2]]
                  }              
            }
            
            #if slot existed, but not up to alarm.dim, add the needed dimensions
            if (dim(y@alarms)[3]<alarm.dim){
              add <- alarm.dim-dim(y@alarms)[3]
              for (d in (dim(y@alarms)[3]+1):(dim(y@alarms)[3]+add) ){
              y@alarms<-abind(y@alarms,
                              matrix(NA,nrow=dim(y@observed)[1],ncol=dim(y@observed)[2]),
                              along=3)
              }
            }
            dimnames(y@alarms)[[3]][alarm.dim] <- "HoltWinters"
            
            #all the same for UCL
            if (UCL!=FALSE){
            if (dim(y@UCL)[1]==0){
              setUCLD(y)<-array(NA,dim=c(dim(y@observed)[1],dim(y@observed)[2],alarm.dim))
              if (length(dimnames(y@observed)[[2]])==1) {
                dimnames(y@UCL)[[2]] <- list(dimnames(y@observed)[[2]])
              } else{
                dimnames(y@UCL)[[2]] <- dimnames(y@observed)[[2]]
              }   
            }
            
            #if slot existed, but not up to alarm.dim, add the needed dimensions
            if (dim(y@UCL)[3]<alarm.dim){
              add <- alarm.dim-dim(y@UCL)[3]
              for (d in (dim(y@UCL)[3]+1):(dim(y@UCL)[3]+add) ){
                y@UCL<-abind(y@UCL,
                                matrix(NA,nrow=dim(y@observed)[1],ncol=dim(y@observed)[2]),
                                along=3)
              }
            }
            dimnames(y@UCL)[[3]][alarm.dim] <- "HoltWinters"
            }
            
           #if Baseline does not exist at all, fill it with NA for the dimensions required 
            if (dim(y@baseline)[1]==0){
              setBaselineD(y)<-matrix(NA,nrow=dim(y@observed)[1],ncol=dim(y@observed)[2],
                                     dimnames=dimnames(y@observed))
            }
            
            
 #number of time points to iterate           
range <- (dim(y@observed)[1]-evaluate.window+1):dim(y@observed)[1]                
            
            
for (syndrome in syndromes.num){
  for (tpoint in range){
      
      #if baseline for the syndrome in question is not available
      #(filled with NA just to reach dimensions necessary), thn
      #for the syndrome in use replace with data from observed
      if (sum(y@baseline[,syndrome],na.rm=TRUE)==0){
        y@baseline[,syndrome]<-y@observed[,syndrome]
      }
    
      #fitting data to a time series and applying HoltWinters()
    time.series <-ts(as.double(y@baseline[(tpoint-nahead-baseline.window):
                                            (tpoint-nahead),syndrome]),
                     start = c(as.double(y@dates$year[tpoint-nahead-baseline.window]),1), 
                     frequency = frequency)
    
    hw = HoltWinters(time.series, 
                     seasonal=seasonal, start.periods=2, 
                     alpha=alpha, beta=beta, gamma=gamma)
      
      for (l in 1:length(limit.sd)){
        
      #forecasting
    pred = predict(hw, n.ahead=as.double(nahead), 
                   prediction.interval=TRUE, 
                   level = as.double(pnorm(limit.sd[l])))

     #before deciding if an alarm exists, a zero is automatically added to the 
      #time point if this is the first loop for two reasons:
      #1-because if the data were never analysed, the slot had a NA before,
      #and adding 0 will signal that it has now been processed
      #2-because if the data HAS been analyzed before, we want the results of these
      #analyses to OVERRIDE, not to SUM to the previous analyses.
    if(l==1){
      y@alarms[tpoint,syndrome,alarm.dim]<-0
    } 
    
      #prediction sometimes fails
    if (is.na(pred[nahead,2])==TRUE) next
      
      if (l==UCL){
      y@UCL[tpoint,syndrome,alarm.dim]<-pred[nahead,2]
      }
      
      #ADD a one if the result of this loop was a detection
    if (as.double(y@observed[tpoint,syndrome])>max(0,as.double(pred[nahead,2]))){
      y@alarms[tpoint,syndrome,alarm.dim]<-y@alarms[tpoint,syndrome,alarm.dim]+1
    }
      
      #Correct baseline IF the user indicated so
      if (correct.baseline==l){
        y@baseline[tpoint,syndrome]<- y@observed[tpoint,syndrome]
            if (as.double(y@observed[tpoint,syndrome])>max(0,as.double(pred[nahead,2]))){
              y@baseline[tpoint,syndrome] <- max(0,as.double(pred[nahead,2]))
        }
      }
    
    }
  }
}
            
              return(y)          
            }
)


##' @rdname holt_winters_synd-methods
##' @export
setMethod('holt_winters_synd',
          signature(x = 'syndromicW'),
          function (x,
                    syndromes=NULL,
                    evaluate.window=1,
                    frequency=52,
                    baseline.window=104,
                    limit.sd=c(2.5,3,3.5),
                    nahead=2,
                    alpha=0.4,
                    beta=0,
                    gamma=0.15,
                    seasonal="additive",
                    correct.baseline=1,
                    alarm.dim=1,
                    UCL=1
          )
{
            
            ##check that syndromes is valid
            if (class(syndromes)=="NULL"){
              syndromes <- colnames(x@observed)
            }else{
              if (class(syndromes)!="character"&&class(syndromes)!="numeric") {
                stop("if provided, argument syndromes must be a character or numeric vector")
              }
            }
            
            #syndromes index to be always numeric
            if (class(syndromes)=="numeric") {
              syndromes.num <- syndromes
            }else{
              syndromes.num <- match(syndromes,colnames(x@observed))
            }
            
            #pulling to a new object to modify directly in the object
            y <- x            
            
            #if slot alarms does not exist at all, fill it with NA
            #for the minmum dimensions required
            if (dim(y@alarms)[1]==0){
              setAlarmsW(y)<-array(NA,dim=c(dim(y@observed)[1],dim(y@observed)[2],alarm.dim))
              if (length(dimnames(y@observed)[[2]])==1) {
                dimnames(y@alarms)[[2]] <- list(dimnames(y@observed)[[2]])
              } else{
                dimnames(y@alarms)[[2]] <- dimnames(y@observed)[[2]]
              }    
            }
            
            #if slot existed, but not up to alarm.dim, add the needed dimensions
            if (dim(y@alarms)[3]<alarm.dim){
              add <- alarm.dim-dim(y@alarms)[3]
              for (d in (dim(y@alarms)[3]+1):(dim(y@alarms)[3]+add) ){
                y@alarms<-abind(y@alarms,
                                matrix(NA,nrow=dim(y@observed)[1],ncol=dim(y@observed)[2]),
                                along=3)
              }
            }
            dimnames(y@alarms)[[3]][alarm.dim] <- "HoltWinters"
            
            #all the same for UCL
            if (UCL!=FALSE){
              if (dim(y@UCL)[1]==0){
                setUCLW(y)<-array(NA,dim=c(dim(y@observed)[1],dim(y@observed)[2],alarm.dim))
                if (length(dimnames(y@observed)[[2]])==1) {
                  dimnames(y@UCL)[[2]] <- list(dimnames(y@observed)[[2]])
                } else{
                  dimnames(y@UCL)[[2]] <- dimnames(y@observed)[[2]]
                }   
              }
              
              #if slot existed, but not up to alarm.dim, add the needed dimensions
              if (dim(y@UCL)[3]<alarm.dim){
                add <- alarm.dim-dim(y@UCL)[3]
                for (d in (dim(y@UCL)[3]+1):(dim(y@UCL)[3]+add) ){
                  y@UCL<-abind(y@UCL,
                               matrix(NA,nrow=dim(y@observed)[1],ncol=dim(y@observed)[2]),
                               along=3)
                }
              }
              dimnames(y@UCL)[[3]][alarm.dim] <- "HoltWinters"
            }
            
            #if Baseline does not exist at all, fill it with NA for the dimensions required 
            if (dim(y@baseline)[1]==0){
              setBaselineW(y)<-matrix(NA,nrow=dim(y@observed)[1],ncol=dim(y@observed)[2],
                                      dimnames=dimnames(y@observed))
            }
            
            
            #number of time points to iterate           
            range <- (dim(y@observed)[1]-evaluate.window+1):dim(y@observed)[1]                
            
            
            for (syndrome in syndromes.num){
              for (tpoint in range){
                
                #if baseline for the syndrome in question is not available
                #(filled with NA just to reach dimensions necessary), thn
                #for the syndrome in use replace with data from observed
                if (sum(y@baseline[,syndrome],na.rm=TRUE)==0){
                  y@baseline[,syndrome]<-y@observed[,syndrome]
                }
                
                #fitting data to a time series and applying HoltWinters()
                time.series <-ts(as.double(y@baseline[(tpoint-nahead-baseline.window):
                                                        (tpoint-nahead),syndrome]),
                                 start = c(as.double(y@dates$year[tpoint-nahead-baseline.window]),1), 
                                 frequency = frequency)
                
                hw = HoltWinters(time.series, 
                                 seasonal=seasonal, start.periods=2, 
                                 alpha=alpha, beta=beta, gamma=gamma)
                
                for (l in 1:length(limit.sd)){
                  
                  #forecasting
                  pred = predict(hw, n.ahead=as.double(nahead), 
                                 prediction.interval=TRUE, 
                                 level = as.double(pnorm(limit.sd[l])))
                  
                  #before deciding if an alarm exists, a zero is automatically added to the 
                  #time point if this is the first loop for two reasons:
                  #1-because if the data were never analysed, the slot had a NA before,
                  #and adding 0 will signal that it has now been processed
                  #2-because if the data HAS been analyzed before, we want the results of these
                  #analyses to OVERRIDE, not to SUM to the previous analyses.
                  if(l==1){
                    y@alarms[tpoint,syndrome,alarm.dim]<-0
                  } 
                  
                  #prediction sometimes fails
                  if (is.na(pred[nahead,2])==TRUE) next
                  
                  if (l==UCL){
                    y@UCL[tpoint,syndrome,alarm.dim]<-pred[nahead,2]
                  }
                  
                  #ADD a one if the result of this loop was a detection
                  if (as.double(y@observed[tpoint,syndrome])>max(0,as.double(pred[nahead,2]))){
                    y@alarms[tpoint,syndrome,alarm.dim]<-y@alarms[tpoint,syndrome,alarm.dim]+1
                  }
                  
                  #Correct baseline IF the user indicated so
                  if (correct.baseline==l){
                    y@baseline[tpoint,syndrome]<- y@observed[tpoint,syndrome]
                    if (as.double(y@observed[tpoint,syndrome])>max(0,as.double(pred[nahead,2]))){
                      y@baseline[tpoint,syndrome] <- max(0,as.double(pred[nahead,2]))
                    }
                  }
                  
                }
              }
            }
            
            return(y)          
          }
)
