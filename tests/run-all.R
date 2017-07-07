# manual testing while the right tests are not set up

#data(lab.daily)
#head(lab.weekly)
#data(observed)

#data with only one column

#unnamed columns


#install.packages("devtools")
library(devtools)

install_github("nandadorea/vetsyn")
require(vetsyn)


data(observed)
data(observedW)
data(lab.daily)
data(lab.weekly)
data(lab.daily.update)
data(lab.weekly.update)

#syndromicD(observed, 
#           dates=data.frame(), 
#           max.date=NULL, 
#           date.format="%d/%m/%Y", 
#           weekends=TRUE, 
#           baseline=matrix(nrow=0,ncol=0), 
#           alarms=array(dim=0), 
#           UCL=array(dim=0), 
#           LCL=array(dim=0))

my.syndromicD <- syndromicD(observed,min.date="01/01/2011",max.date="26/05/2013")

# syndromicW (observed, 
#             dates=data.frame(), 
#             min.week=NULL, 
#             max.week=NULL, 
#             min.year=NULL, 
#             max.year=NULL, 
#             baseline=matrix(nrow=0,ncol=0), 
#             alarms=array(dim=0), 
#             UCL=array(dim=0), 
#             LCL=array(dim=0))

my.syndromicW <- syndromicW(observedW,min.week=1, min.year=2011, 
                            max.week=22, max.year=2013)


# raw_to_syndromicD(id,
#                   syndromes.var, 
#                   syndromes.name,
#                   dates.var, 
#                   date.format="%d/%m/%Y", 
#                   min.date, 
#                   max.date,
#                   remove.dow=FALSE,
#                   add.to=0,
#                   sort=TRUE,
#                   data=NULL) 

my.syndromicD <- raw_to_syndromicD (id=lab.daily$SubmissionID,
                                    syndromes.var=lab.daily$Syndrome,
                                    dates.var=lab.daily$DateofSubmission,
                                    date.format="%d/%m/%Y")

my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)

my.syndromicD <- raw_to_syndromicD (id=list(HerdID,AnimalID),
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)

my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    syndromes.name=c("GIT","Musculoskeletal"),
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)

my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    syndromes.name=c("GIT","Musculoskeletal","NonExisting"),
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)

my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    min.date="01/01/2011",
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),
                                    data=lab.daily)



# raw_to_syndromicW(id,
#                                syndromes.var, 
#                                syndromes.name,
#                                dates.var, 
#                                date.format="%d/%m/%Y", 
#                                min.date, 
#                                max.date,
#                                sort=TRUE,
#                                data=NULL) 

my.syndromicW <- raw_to_syndromicW (id=lab.daily$SubmissionID,
                                    syndromes.var=lab.daily$Syndrome,
                                    dates.var=lab.daily$DateofSubmission,
                                    date.format="%d/%m/%Y")

my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)

my.syndromicW <- raw_to_syndromicW (id=list(HerdID,AnimalID),
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)

my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    syndromes.name=c("GIT","Musculoskeletal"),
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)

my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    syndromes.name=c("GIT","Musculoskeletal","NonExisting"),
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)

my.syndromicW <- raw_to_syndromicW (id=lab.daily$SubmissionID,
                                    syndromes.var=lab.daily$Syndrome,
                                    dates.var=lab.daily$DateofSubmission,
                                    min.date="01/01/2011",
                                    date.format="%d/%m/%Y")


##

my.syndromicW <- raw_to_syndromicW (id=lab.weekly$SubmissionID,
                                    syndromes.var=lab.weekly$Syndrome,
                                    dates.var=lab.weekly$DateofSubmission,
                                    date.format="ISOweek")

my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="ISOweek",
                                    data=lab.weekly)

my.syndromicW <- raw_to_syndromicW (id=list(HerdID,AnimalID),
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="ISOweek",
                                    data=lab.weekly)

my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    syndromes.name=c("GIT","Musculoskeletal"),
                                    dates.var=DateofSubmission,
                                    date.format="ISOweek",
                                    data=lab.weekly)

my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    syndromes.name=c("GIT","Musculoskeletal","NonExisting"),
                                    dates.var=DateofSubmission,
                                    date.format="ISOweek",
                                    data=lab.weekly)

my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="ISOweek",
                                    min.date="2010-W50-1",
                                    data=lab.weekly)


my.syndromicD

my.syndromicW

my.syndromicD[1,10]

my.syndromicW[1,10]

dim(my.syndromicD)

dim(my.syndromicW)





#retro_summary(x,
#              object.name="my.syndromic",
#              file.name="syndromic.retro.summary",
#              frequency=365,
#              short=FALSE)
retro_summary(my.syndromicD)

#retro_summary(x,
#              object.name="my.syndromic",
#              file.name="syndromic.retro.summary",
#              frequency=52,
#              short=FALSE)


retro_summary(my.syndromicW)


# clean_baseline(x,
#                syndromes=NULL,
#                family="poisson",
#                limit=0.95,
#                formula="dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7",
#                frequency=365,
#                plot=TRUE,
#                print.model=TRUE)
my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                  
                                    data=lab.daily)
my.syndromicD <- clean_baseline(my.syndromicD,
                                frequency=260)
my.syndromicD <- clean_baseline(my.syndromicD, 
                                formula="dow+month+year",
                                frequency=260)
my.syndromicD <- clean_baseline(my.syndromicD, formula="dow+sin+cos+trend",
                                frequency=260)
my.syndromicD <- clean_baseline(my.syndromicD,
                                syndromes="Musculoskeletal",
                                frequency=260)
my.syndromicD <- clean_baseline(my.syndromicD,
                                syndromes=c("GIT","Musculoskeletal"),
                                frequency=260)
my.syndromicD <- clean_baseline(my.syndromicD,
                                syndromes=3,
                                frequency=260)
my.syndromicD <- clean_baseline(my.syndromicD,
                                syndromes=c(1,3),
                                frequency=260)

my.syndromicD <- clean_baseline(my.syndromicD,
                                family="nbinom",
                                frequency=260)
my.syndromicD <- clean_baseline(my.syndromicD,
                                syndromes="Musculoskeletal",
                                family="nbinom",
                                frequency=260)
my.syndromicD <- clean_baseline(my.syndromicD,
                                syndromes=c("GIT","Musculoskeletal"),
                                family="nbinom",
                                frequency=260)
my.syndromicD <- clean_baseline(my.syndromicD,
                                syndromes=3,
                                family="nbinom",
                                frequency=260)
my.syndromicD <- clean_baseline(my.syndromicD,
                                syndromes=c(1,3),
                                family="nbinom",
                                frequency=260)

# clean_baseline(x,
#                syndromes=NULL,
#                family="poisson",
#                limit=0.95,
#                formula="year+sin+cos",
#                plot=TRUE,
#                print.model=TRUE,
#                frequency=52)

my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
my.syndromicW <- clean_baseline(my.syndromicW)
my.syndromicW <- clean_baseline(my.syndromicW, formula="sin+cos")
my.syndromicW <- clean_baseline(my.syndromicW,
                                syndromes="Musculoskeletal")
my.syndromicW <- clean_baseline(my.syndromicW,
                                syndromes=c("GIT","Musculoskeletal"))
my.syndromicW <- clean_baseline(my.syndromicW,
                                syndromes=3)
my.syndromicW <- clean_baseline(my.syndromicW,
                                syndromes=c(1,3))

my.syndromicW <- clean_baseline(my.syndromicW,
                                family="nbinom")
my.syndromicW <- clean_baseline(my.syndromicW,
                                syndromes="Musculoskeletal",family="nbinom")


# clean_baseline_perc(x,
#                     syndromes=NULL,
#                     limit=0.95,
#                     run.window=120,
#                     plot=TRUE)

my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
my.syndromicD <- clean_baseline_perc(my.syndromicD)
my.syndromicD <- clean_baseline_perc(my.syndromicD,run.window=90)
my.syndromicD <- clean_baseline_perc(my.syndromicD,
                                     syndromes="Musculoskeletal")
my.syndromicD <- clean_baseline_perc(my.syndromicD,
                                     syndromes=c("GIT","Musculoskeletal"))
my.syndromicD <- clean_baseline_perc(my.syndromicD,
                                     syndromes=3)
my.syndromicD <- clean_baseline_perc(my.syndromicD,
                                     syndromes=c(1,3))

my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
my.syndromicW <- clean_baseline_perc(my.syndromicW)
my.syndromicW <- clean_baseline_perc(my.syndromicW,run.window=90)
my.syndromicW <- clean_baseline_perc(my.syndromicW,
                                     syndromes="Musculoskeletal")
my.syndromicW <- clean_baseline_perc(my.syndromicW,
                                     syndromes=c("GIT","Musculoskeletal"))
my.syndromicW <- clean_baseline_perc(my.syndromicW,
                                     syndromes=3)
my.syndromicW <- clean_baseline_perc(my.syndromicW,
                                     syndromes=c(1,3))




# holt_winters_synd(x,
#                   syndromes=NULL,
#                   evaluate.window=1,
#                   frequency=7,
#                   baseline.window=365,
#                   limit.sd=c(2.5,3,3.5),
#                   nahead=7,
#                   alpha=0.4,
#                   beta=0,
#                   gamma=0.15,
#                   seasonal="additive",
#                   correct.baseline=1,
#                   alarm.dim=1,
#                   UCL=1)


my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
my.syndromicD <- holt_winters_synd(x=my.syndromicD,
                                   evaluate.window=30,
                                   frequency=7,
                                   baseline.window=365,
                                   limit.sd=c(2.5,3,3.5), #default
                                   nahead=7,
                                   correct.baseline=2,
                                   alarm.dim=1)

my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),
                                    data=lab.daily)
my.syndromicD <- holt_winters_synd(x=my.syndromicD,
                                   syndromes="Musculoskeletal",
                                   evaluate.window=30,
                                   frequency=5,
                                   baseline.window=260,
                                   limit.sd=c(2.5,3,3.5), #default
                                   nahead=5,
                                   correct.baseline=2,
                                   alarm.dim=1,
                                   UCL=2)

# holt_winters_synd(x,
#                   syndromes=NULL,
#                   evaluate.window=1,
#                   frequency=52,
#                   baseline.window=104,
#                   limit.sd=c(2.5,3,3.5),
#                   nahead=2,
#                   alpha=0.4,
#                   beta=0,
#                   gamma=0.15,
#                   seasonal="additive",
#                   correct.baseline=1,
#                   alarm.dim=1,
#                   UCL=1

my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
my.syndromicW <- holt_winters_synd(x=my.syndromicW,
                                   evaluate.window=12,
                                   frequency=52,
                                   baseline.window=104,
                                   limit.sd=c(2.5,3,3.5), #default
                                   nahead=2,
                                   correct.baseline=2,
                                   alarm.dim=1)


# ewma_synd(x,
#           syndromes=NULL,
#           evaluate.window=1,
#           baseline.window=365,
#           lambda=0.2,
#           limit.sd=c(2.5,3,3.5),
#           guard.band=7,
#           correct.baseline=FALSE,
#           alarm.dim=2,
#           UCL=1,
#           LCL=FALSE,
#           pre.process=FALSE,
#           diff.window=7,
#           family="poisson",
#           formula="dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7",
#           frequency=365)
my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),
                                    data=lab.daily)
#system.time({
my.syndromicD <- ewma_synd(x=my.syndromicD,
                           syndrome="Musculoskeletal",
                           evaluate.window=10,
                           baseline.window=260,
                           lambda=0.2,
                           limit.sd=c(2.5,3,3.5),
                           guard.band=5,
                           correct.baseline=FALSE,
                           alarm.dim=2,
                           pre.process="glm",
                           family="nbinom",
                           formula="dow+sin+cos+AR1+AR2+AR3+AR4+AR5",
                           frequency=260)
#})

#system.time({
my.syndromicD <- ewma_synd(x=my.syndromicD,
                           #syndrome= c(1,2,4,5),
                           evaluate.window=60,
                           baseline.window=260,
                           lambda=0.2,
                           limit.sd=c(2.5,3,3.5),
                           guard.band=5,
                           correct.baseline=FALSE,
                           alarm.dim=2,
                           pre.process="diff",
                           diff.window=5)
})



# ewma_synd(x,
#           syndromes=NULL,
#           evaluate.window=1,
#           baseline.window=52,
#           lambda=0.2,
#           limit.sd=c(2.5,3,3.5),
#           guard.band=2,
#           correct.baseline=FALSE,
#           alarm.dim=2,
#           UCL=1,
#           LCL=FALSE,
#           pre.process=FALSE,
#           diff.window=4,
#           family="poisson",
#           formula="trend+sin+cos",
#           frequency=52)

my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
my.syndromicW <- ewma_synd(x=my.syndromicW,
                           syndrome="Musculoskeletal",
                           evaluate.window=10,
                           baseline.window=104,
                           lambda=0.2,
                           limit.sd=c(2.5,3,3.5),
                           guard.band=2,
                           correct.baseline=FALSE,
                           alarm.dim=2,
                           pre.process="glm",
                           family="nbinom",
                           formula="trend+sin+cos",
                           frequency=52)

my.syndromicW <- ewma_synd(x=my.syndromicW,
                           syndrome= c(1,2,4,5),
                           evaluate.window=10,
                           baseline.window=104,
                           lambda=0.2,
                           limit.sd=c(2.5,3,3.5),
                           guard.band=2,
                           correct.baseline=FALSE,
                           alarm.dim=2,
                           pre.process="diff",
                           diff.window=4)


# pre_process_glm(x,
#                 slot="observed",
#                 syndromes=NULL,
#                 family="poisson",
#                 formula="dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7",
#                 frequency=365,
#                 print.model=TRUE,
#                 plot=TRUE)


my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),
                                    data=lab.daily)
pre_processed_data <- pre_process_glm(my.syndromicD)
pre_processed_data <- pre_process_glm(my.syndromicD,
                                      syndromes="Musculoskeletal")
pre_processed_data <- pre_process_glm(my.syndromicD,
                                      syndromes=c("GIT","Musculoskeletal"))
pre_processed_data <- pre_process_glm(my.syndromicD,
                                      syndromes=3)
pre_processed_data <- pre_process_glm(my.syndromicD,
                                      syndromes=c(1,3))

pre_processed_data <- pre_process_glm(my.syndromicD,
                                      family="nbinom")



# pre_process_glm(x,
#                 slot="observed",
#                 syndromes=NULL,
#                 family="poisson",
#                 formula="trend+sin+cos",
#                 frequency=52,
#                 print.model=TRUE,
#                 plot=TRUE)

my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
pre_processed_data <- pre_process_glm(my.syndromicW)
pre_processed_data <- pre_process_glm(my.syndromicW,
                                      syndromes="Musculoskeletal")
pre_processed_data <- pre_process_glm(my.syndromicW,
                                      syndromes=c("GIT","Musculoskeletal"))
pre_processed_data <- pre_process_glm(my.syndromicW,
                                      syndromes=3)
pre_processed_data <- pre_process_glm(my.syndromicW,
                                      syndromes=c(1,3))

pre_processed_data <- pre_process_glm(my.syndromicW,
                                      family="nbinom")
pre_processed_data <- pre_process_glm(my.syndromicW,slot="baseline")



# shew_synd(x,
#           syndromes=NULL,
#           evaluate.window=1,
#           baseline.window=365,
#           limit.sd=c(2.5,3,3.5),
#           guard.band=7,
#           correct.baseline=FALSE,
#           alarm.dim=3,
#           UCL=1,
#           LCL=FALSE,
#           pre.process=FALSE,
#           diff.window=7,
#           family="poisson",
#           formula="dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7",
#           frequency=365)


my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),
                                    data=lab.daily)
my.syndromicD <- shew_synd(x=my.syndromicD,
                           syndromes="Musculoskeletal",
                           evaluate.window=1,
                           baseline.window=365,
                           limit.sd=c(2.5,3,3.5),
                           guard.band=7,
                           correct.baseline=FALSE,
                           alarm.dim=3,
                           UCL=1,
                           LCL=FALSE,
                           pre.process="glm",
                           diff.window=7,
                           family="poisson",
                           formula="dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7",
                           frequency=365)
my.syndromicD <- shew_synd(x=my.syndromicD,
                           syndromes="Musculoskeletal",
                           evaluate.window=10,
                           baseline.window=260,
                           limit.sd=c(2.5,3,3.5),
                           guard.band=5,
                           correct.baseline=FALSE,
                           alarm.dim=3,
                           pre.process="glm",
                           family="poisson",
                           formula="dow+sin+cos+AR1+AR2+AR3+AR4+AR5",
                           frequency=260)

my.syndromicD <- shew_synd(x=my.syndromicD,
                           syndrome= c(1,2,4,5),
                           evaluate.window=30,
                           baseline.window=260,
                           limit.sd=c(2.5,3,3.5),
                           guard.band=5,
                           correct.baseline=FALSE,
                           alarm.dim=3,
                           pre.process="diff",
                           diff.window=5)

# shew_synd(x,
#            syndromes=NULL,
#            evaluate.window=1,
#            baseline.window=52,
#            limit.sd=c(2.5,3,3.5),
#            guard.band=2,
#            correct.baseline=FALSE,
#            alarm.dim=3,
#            UCL=1,
#            LCL=FALSE,
#            pre.process=FALSE,
#            diff.window=4,
#            family="poisson",
#            formula="trend+sin+cos",
#            frequency=52)

my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
my.syndromicW <- shew_synd(x=my.syndromicW,
                           syndrome="Musculoskeletal",
                           evaluate.window=10,
                           baseline.window=104,
                           limit.sd=c(2.5,3,3.5),
                           guard.band=2,
                           correct.baseline=FALSE,
                           alarm.dim=3,
                           pre.process="glm",
                           family="nbinom",
                           formula="trend+sin+cos",
                           frequency=52)

my.syndromicW <- shew_synd(x=my.syndromicW,
                           syndrome= c(1,2,4,5),
                           evaluate.window=10,
                           baseline.window=104,
                           limit.sd=c(2.5,3,3.5),
                           guard.band=2,
                           correct.baseline=FALSE,
                           alarm.dim=3,
                           pre.process="diff",
                           diff.window=4)


# cusum_synd(x,
#            syndromes=NULL,
#            evaluate.window=1,
#            baseline.window=365,
#            limit.sd=c(2.5,3,3.5),
#            guard.band=7,
#            correct.baseline=FALSE,
#            alarm.dim=4,
#            UCL=1,
#            LCL=FALSE,
#            pre.process=FALSE,
#            diff.window=7,
#            family="poisson",
#            formula="dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7",
#            frequency=365)
my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),
                                    data=lab.daily)
my.syndromicD <- cusum_synd(x=my.syndromicD,
                            syndromes="Musculoskeletal",
                            evaluate.window=30,
                            baseline.window=260,
                            limit.sd=c(2.5,3,3.5),
                            guard.band=5,
                            correct.baseline=FALSE,
                            alarm.dim=4,
                            pre.process="glm",
                            family="poisson",
                            formula="dow+sin+cos+AR1+AR2+AR3+AR4+AR5",
                            frequency=260)

my.syndromicD <- cusum_synd(x=my.syndromicD,
                            syndrome= c(1,2,4,5),
                            evaluate.window=30,
                            baseline.window=260,
                            limit.sd=c(2.5,3,3.5),
                            guard.band=5,
                            correct.baseline=FALSE,
                            alarm.dim=4,
                            pre.process="diff",
                            diff.window=5)



# cusum_synd(x,
#            syndromes=NULL,
#            evaluate.window=1,
#            baseline.window=52,
#            limit.sd=c(2.5,3,3.5),
#            guard.band=2,
#            correct.baseline=FALSE,
#            alarm.dim=4,
#            UCL=1,
#            LCL=FALSE,
#            pre.process=FALSE,
#            diff.window=4,
#            family="poisson",
#            formula="trend+sin+cos",
#            frequency=52)

my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
my.syndromicW <- cusum_synd(x=my.syndromicW,
                            syndrome="Musculoskeletal",
                            evaluate.window=10,
                            baseline.window=104,
                            limit.sd=c(2.5,3,3.5),
                            guard.band=2,
                            correct.baseline=FALSE,
                            alarm.dim=4,
                            pre.process="glm",
                            family="nbinom",
                            formula="trend+sin+cos",
                            frequency=52)

my.syndromicW <- cusum_synd(x=my.syndromicW,
                            syndrome= c(1,2,4,5),
                            evaluate.window=10,
                            baseline.window=104,
                            limit.sd=c(2.5,3,3.5),
                            guard.band=2,
                            correct.baseline=FALSE,
                            alarm.dim=4,
                            pre.process="diff",
                            diff.window=4)

# plot(x, syndromes=NULL,
#      window=365,
#      baseline=FALSE,
#      UCL=1,
#      algorithms=NULL,
#      limit=NULL)

plot(my.syndromicD)

# plot(x, syndromes=NULL,
#  window=52,
#  baseline=FALSE,
#  UCL=1,
#  algorithms=NULL,
#  limit=NULL)

plot(my.syndromicW)


# plot_syndromic(x,
#                syndromes=NULL,
#                window=365,
#                baseline=TRUE,
#                UCL=1,
#                algorithms=NULL,
#                limit=1)

plot_syndromic(my.syndromicD)

# plot_syndromic(x,
#                syndromes=NULL,
#                window=52,
#                baseline=TRUE,
#                UCL=1,
#                algorithms=NULL,
#                limit=1)

plot_syndromic(my.syndromicW)


# syndromic_alarm(x,
#                 pdf.report=TRUE,
#                 email.alarm.to=NULL,
#                 email.noalarm.to=NULL,
#                 date=NULL,
#                 plot.all=FALSE,
#                 window=365,
#                 baseline=TRUE,
#                 UCL=1,
#                 algorithms=NULL,
#                 limit=1,
#                 email.from=NULL,
#                 smtpServer=NULL,
#                 subject= paste((Sys.Date()),"ALARM: There are alarms today",sep=","),
#                 message=NULL,
#                 height=7.5,
#                 width=10.5,
#                 pdf.dir=TRUE,
#                 file.name=NULL)

syndromic_alarm(x=my.syndromicD,
                plot.all=TRUE,
                email.alarm.to="<dorea.meyer@gmail.com>",
                email.noalarm.to="<dorea.meyer@gmail.com>")

syndromic_alarm(x=my.syndromicD,
                plot.all=TRUE,
                date="2013-01-05",
                email.alarm.to="<dorea.meyer@gmail.com>",
                email.noalarm.to="<dorea.meyer@gmail.com>")


# syndromic_alarm(x,
#                 pdf.report=TRUE,
#                 email.alarm.to=NULL,
#                 email.noalarm.to=NULL,
#                 date=NULL,
#                 plot.all=FALSE,
#                 window=52,
#                 baseline=TRUE,
#                 UCL=1,
#                 algorithms=NULL,
#                 limit=1,
#                 email.from=NULL,
#                 smtpServer=NULL,
#                 subject= paste((Sys.Date()),"ALARM: There are alarms today",sep=","),
#                 message=NULL,
#                 height=7.5,
#                 width=10.5,
#                 pdf.dir=TRUE,
#                 file.name=NULL)

syndromic_alarm(x=my.syndromicW,
                plot.all=TRUE,
                email.alarm.to="<dorea.meyer@gmail.com>",
                email.noalarm.to="<dorea.meyer@gmail.com>")

syndromic_alarm(x=my.syndromicW,
                plot.all=TRUE,
                date="2013-W02-1",
                email.alarm.to="<dorea.meyer@gmail.com>",
                email.noalarm.to="<dorea.meyer@gmail.com>")



# syndromic.page(x,
#                tpoints.display=7,
#                window=365,
#                baseline=TRUE,
#                UCL=1,
#                algorithms=NULL,
#                limit=1,
#                file.name="my.syndromic",
#                title="My syndromic",
#                data.page=FALSE,
#                data=NULL,
#                date.format="%d/%m/%Y",
#                dates.var=NULL,
#                syndromes.var=NULL,
#                color.null="F8F8FF",
#                color.low="F8FF2F",
#                color.alarm="FF0000",
#                scale=9, 
#                fill.colors=c("yellow2","orange","tomato"),
#                arrow.colors=c("green","orange","tomato","red"))

syndromic_page (x=my.syndromicD,
                tpoints.display=5,
                file.name="SpeciesX",
                title="Lab data daily for Species X",
                data.page=TRUE,
                data=lab.daily,
                date.format="%d/%m/%Y",
                dates.var="DateofSubmission",
                syndromes.var="Syndrome",
                scale=9)                                 



# syndromic_page(x,
#                tpoints.display=4,
#                window=52,
#                baseline=TRUE,
#                UCL=1,
#                algorithms=NULL,
#                limit=1,
#                file.name="my.syndromic",
#                title="My syndromic",
#                data.page=FALSE,
#                data=NULL,
#                date.format="ISOweek",
#                dates.var=NULL,
#                syndromes.var=NULL,
#                color.null="F8F8FF",
#                color.low="F8FF2F",
#                color.alarm="FF0000",
#                scale=9, 
#                fill.colors=c("yellow2","orange","tomato"),
#                arrow.colors=c("green","orange","tomato","red"))

syndromic_page(x=my.syndromicW,
               tpoints.display=4,
               file.name="SpeciesX",
               title="Lab data daily for Species X",
               data.page=TRUE,
               data=lab.daily,
               date.format="%d/%m/%Y",
               dates.var="DateofSubmission",
               syndromes.var="Syndrome",
               scale=9) 


# update_syndromic(x,
#                  id,
#                  syndromes.var, 
#                  add.syndromes=TRUE,
#                  dates.var, 
#                  date.format="%d/%m/%Y", 
#                  remove.dow=FALSE,
#                  add.to=0,
#                  replace.dates=TRUE,
#                  data=NULL)


my.syndromicD <- update_syndromic(x=my.syndromicD,
                                  id=SubmissionID,
                                  syndromes.var=Syndrome, 
                                  add.syndromes=TRUE,
                                  dates.var=DateofSubmission, 
                                  date.format="%d/%m/%Y", 
                                  remove.dow=c(6,0),
                                  add.to=c(2,1),
                                  replace.dates=TRUE,
                                  data=lab.daily.update)

# update_syndromic(x,
#                   id,
#                   syndromes.var, 
#                   add.syndromes=TRUE,
#                   dates.var, 
#                   date.format="%d/%m/%Y", 
#                   replace.dates=TRUE,
#                   data=NULL)

my.syndromicW <- update_syndromic(x=my.syndromicW,
                                  id=lSubmissionID,
                                  syndromes.var=Syndrome, 
                                  add.syndromes=TRUE,
                                  dates.var=DateofSubmission, 
                                  date.format="%d/%m/%Y", 
                                  replace.dates=TRUE,
                                  data=lab.daily.update)


my.syndromicW <- update_syndromic(x=my.syndromicW,
                                  id=SubmissionID,
                                  syndromes.var=Syndrome, 
                                  add.syndromes=TRUE,
                                  dates.var=DateofSubmission, 
                                  date.format="ISOweek", 
                                  replace.dates=TRUE,
                                  data=lab.weekly.update)

