##' @name holt_winters_synd
##' @docType methods
##' @export
##' @import abind
##' @examples
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
              dimnames(y@alarms)[[2]] <- dimnames(y@observed)[[2]]
              dimnames(y@alarms)[[3]][alarm.dim] <- "HoltWinters"
            }
            
            #if slot existed, but not up to alarm.dim, add the needed dimensions
            if (dim(y@alarms)[3]<alarm.dim){
              add <- alarm.dim-dim(y@alarms)[3]
              for (d in (dim(y@alarms)[3]+1):(dim(y@alarms)[3]+add) ){
                y@alarms<-abind(y@alarms,
                                matrix(NA,nrow=dim(y@observed)[1],ncol=dim(y@observed)[2]),
                                along=3)
              }
              dimnames(y@alarms)[[3]][alarm.dim] <- "HoltWinters"
            }
            
            #all the same for UCL
            if (UCL!=FALSE){
              if (dim(y@UCL)[1]==0){
                setUCLW(y)<-array(NA,dim=c(dim(y@observed)[1],dim(y@observed)[2],alarm.dim))
                dimnames(y@UCL)[[2]] <- dimnames(y@observed)[[2]]
                dimnames(y@UCL)[[3]][alarm.dim] <- "HoltWinters"
              }
              
              #if slot existed, but not up to alarm.dim, add the needed dimensions
              if (dim(y@UCL)[3]<alarm.dim){
                add <- alarm.dim-dim(y@UCL)[3]
                for (d in (dim(y@UCL)[3]+1):(dim(y@UCL)[3]+add) ){
                  y@UCL<-abind(y@UCL,
                               matrix(NA,nrow=dim(y@observed)[1],ncol=dim(y@observed)[2]),
                               along=3)
                }
                dimnames(y@UCL)[[3]][alarm.dim] <- "HoltWinters"
              }
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
