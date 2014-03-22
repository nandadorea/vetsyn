data(lab.daily)
data(lab.daily.update)
my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                   syndromes.var=Syndrome,
                                   dates.var=DateofSubmission,
                                   date.format="%d/%m/%Y",
                                   data=lab.daily)
 
 #should have 877rows
my.syndromic2 <- update_syndromic(x=my.syndromic,
                                   id=SubmissionID,
                                   syndromes.var=Syndrome, 
                                   add.syndromes=TRUE,
                                   dates.var=DateofSubmission, 
                                   date.format="%d/%m/%Y", 
                                   remove.dow=FALSE,
                                   add.to=0,
                                   replace.dates=TRUE,
                                   data=lab.daily.update)
#dim(my.syndromic2@observed)                                  
#dim(my.syndromic2@dates) 



###########with alarms as array

my.syndromic <- holt_winters_synd(x=my.syndromic,
                                  evaluate.window=40,
                                  frequency=5,
                                  baseline.window=260,
                                  limit.sd=c(2.5,3,3.5), #default
                                  nahead=5,
                                  correct.baseline=2,
                                  alarm.dim=1)

my.syndromic <- ewma_synd(x=my.syndromic,
                          evaluate.window=40,
                          baseline.window=260,
                          lambda=0.2,
                          limit.sd=c(2.5,3,3.5),
                          guard.band=5,
                          correct.baseline=FALSE,
                          alarm.dim=2,
                          pre.process="diff",
                          diff.window=5)

my.syndromic2 <- update_syndromic(x=my.syndromic,
                                  id=SubmissionID,
                                  syndromes.var=Syndrome, 
                                  add.syndromes=TRUE,
                                  dates.var=DateofSubmission, 
                                  date.format="%d/%m/%Y", 
                                  remove.dow=FALSE,
                                  add.to=0,
                                  replace.dates=TRUE,
                                  data=lab.daily.update)

dim(my.syndromic2@observed)                                  
dim(my.syndromic2@dates)                                  
dim(my.syndromic2@baseline)
dim(my.syndromic2@alarms)
dim(my.syndromic2@UCL)
dim(my.syndromic2@LCL)

tail(my.syndromic2@alarms,35)
tail(my.syndromic2@UCL,35)


###########replace dates=False


my.syndromic2 <- update_syndromic(x=my.syndromic,
                                  id=SubmissionID,
                                  syndromes.var=Syndrome, 
                                  add.syndromes=TRUE,
                                  dates.var=DateofSubmission, 
                                  date.format="%d/%m/%Y", 
                                  remove.dow=FALSE,
                                  add.to=0,
                                  replace.dates=FALSE,
                                  data=lab.daily.update)

dim(my.syndromic2@observed)                                  
dim(my.syndromic2@dates)                                  
dim(my.syndromic2@baseline)
dim(my.syndromic2@alarms)
dim(my.syndromic2@UCL)
dim(my.syndromic2@LCL)

tail(my.syndromic2@alarms,35)
tail(my.syndromic2@UCL,35)

#first syndromic with only one syndrome, and add syndrome=false and then true
my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                  syndromes.var=Syndrome,
                                  syndromes.name=c("Musculoskeletal","GIT"),
                                  dates.var=DateofSubmission,
                                  date.format="%d/%m/%Y",
                                  data=lab.daily)

my.syndromic2 <- update_syndromic(x=my.syndromic,
                                  id=SubmissionID,
                                  syndromes.var=Syndrome, 
                                  add.syndromes=TRUE,
                                  dates.var=DateofSubmission, 
                                  date.format="%d/%m/%Y", 
                                  remove.dow=FALSE,
                                  add.to=0,
                                  replace.dates=TRUE,
                                  data=lab.daily.update)
dim(my.syndromic@observed)                                  
dim(my.syndromic@dates) 

dim(my.syndromic2@observed)                                  
dim(my.syndromic2@dates) 


my.syndromic <- holt_winters_synd(x=my.syndromic,
                                  evaluate.window=40,
                                  frequency=5,
                                  baseline.window=260,
                                  limit.sd=c(2.5,3,3.5), #default
                                  nahead=5,
                                  correct.baseline=2,
                                  alarm.dim=1)

my.syndromic <- ewma_synd(x=my.syndromic,
                          evaluate.window=40,
                          baseline.window=260,
                          lambda=0.2,
                          limit.sd=c(2.5,3,3.5),
                          guard.band=5,
                          correct.baseline=FALSE,
                          alarm.dim=2,
                          pre.process="diff",
                          diff.window=5)

my.syndromic2 <- update_syndromic(x=my.syndromic,
                                  id=SubmissionID,
                                  syndromes.var=Syndrome, 
                                  add.syndromes=TRUE,
                                  dates.var=DateofSubmission, 
                                  date.format="%d/%m/%Y", 
                                  remove.dow=FALSE,
                                  add.to=0,
                                  replace.dates=TRUE,
                                  data=lab.daily.update)

dim(my.syndromic2@observed)                                  
dim(my.syndromic2@dates)                                  
dim(my.syndromic2@baseline)
dim(my.syndromic2@alarms)
dim(my.syndromic2@UCL)
dim(my.syndromic2@LCL)

(my.syndromic2@alarms[800:877,,])
(my.syndromic2@UCL[800:877,,])









x=my.syndromic;
id=lab.daily.update$SubmissionID;
syndromes.var=lab.daily.update$Syndrome;
add.syndromes=TRUE;
dates.var=lab.daily.update$DateofSubmission;
date.format="%d/%m/%Y";
remove.dow=FALSE;
add.to=0;
replace.dates=TRUE;
