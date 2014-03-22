data(lab.daily)

my.syndromic <- raw_to_syndromic (id=SubmissionID, 
                                  syndromes.var=Syndrome,
                                  dates.var=DateofSubmission, 
                                  date.format="%d/%m/%Y", 
                                  remove.dow=c(6,0),
                                  add.to=c(2,1),
                                  sort=TRUE,
                                  data=lab.daily)

my.syndromic <- holt_winters_synd(x=my.syndromic,
                                  evaluate.window=30,
                                  frequency=5,
                                  baseline.window=260,
                                  limit.sd=c(2.5,3,3.5), #default
                                  nahead=5,
                                  correct.baseline=2,
                                  alarm.dim=1)

my.syndromic <- ewma_synd(x=my.syndromic,
                          syndromes="Musculoskeletal",
                          evaluate.window=30,
                          baseline.window=260,
                          lambda=0.2,
                          limit.sd=c(2.5,3,3.5),
                          guard.band=5,
                          correct.baseline=FALSE,
                          alarm.dim=2,
                          UCL=2,
                          LCL=FALSE,                          
                          pre.process="glm",
                          family="poisson",
                          formula="dow+sin+cos+AR1+AR2+AR3+AR4+AR5",
                          period=260)


my.syndromic <- ewma_synd(x=my.syndromic,
                          syndromes= c(1,2,4,5),
                          evaluate.window=30,
                          baseline.window=260,
                          lambda=0.2,
                          limit.sd=c(2.5,3,3.5),
                          guard.band=5,
                          UCL=2,
                          correct.baseline=FALSE,
                          alarm.dim=2,
                          pre.process="diff",
                          diff.window=5)

my.syndromic <- shew_synd(x=my.syndromic,
                          syndromes="Musculoskeletal",
                          evaluate.window=30,
                          baseline.window=260,
                          limit.sd=c(2.5,3,3.5),
                          guard.band=5,
                          correct.baseline=FALSE,
                          alarm.dim=3,
                          UCL=2,
                          LCL=FALSE,                          
                          pre.process="glm",
                          family="poisson",
                          formula="dow+sin+cos+AR1+AR2+AR3+AR4+AR5",
                          period=260)


my.syndromic <- shew_synd(x=my.syndromic,
                          syndromes= c(1,2,4,5),
                          evaluate.window=30,
                          baseline.window=260,
                          limit.sd=c(2.5,3,3.5),
                          guard.band=5,
                          UCL=2,
                          correct.baseline=FALSE,
                          alarm.dim=3,
                          pre.process="diff",
                          diff.window=5)

my.syndromic <- cusum_synd(x=my.syndromic,
                           evaluate.window=30,
                           baseline.window=260,
                           limit.sd=c(2.5,3,3.5),
                           guard.band=5,
                           correct.baseline=FALSE,
                           alarm.dim=4,
                           UCL=2,
                           LCL=FALSE,                          
                           pre.process="glm",
                           family="poisson",
                           formula="dow+sin+cos+AR1+AR2+AR3+AR4+AR5",
                           period=260)

###################################

plot_syndromic(x=my.syndromic,
               syndromes=NULL,
               window=365,
               baseline=TRUE,
               UCL=1,
               algorithms=NULL,
               limit=1)

plot_syndromic(x=my.syndromic,
               syndromes="Musculoskeletal",
               window=365,
               baseline=TRUE,
               UCL=1,
               algorithms=NULL,
               limit=1)


plot_syndromic(x=my.syndromic,
               syndromes=c(1,3),
               window=365,
               baseline=TRUE,
               UCL=1,
               algorithms=NULL,
               limit=1)


plot_syndromic(x=my.syndromic,
               syndromes=c(1,3),
               window=200,
               baseline=TRUE,
               UCL=1,
               algorithms=NULL,
               limit=1)

plot_syndromic(x=my.syndromic,
               syndromes=c(1,3),
               window=365,
               baseline=FALSE,
               UCL=1,
               algorithms=NULL,
               limit=1)

plot_syndromic(x=my.syndromic,
               syndromes=c(1,3),
               window=365,
               baseline=TRUE,
               UCL=3,
               algorithms=NULL,
               limit=1)

plot_syndromic(x=my.syndromic,
               syndromes=c(1,3),
               window=365,
               baseline=TRUE,
               UCL=0,
               algorithms=NULL,
               limit=1)


plot_syndromic(x=my.syndromic,
               syndromes=c(1,3),
               window=365,
               baseline=TRUE,
               UCL=NULL,
               algorithms=NULL,
               limit=1)


plot_syndromic(x=my.syndromic,
               syndromes=c(1,3),
               window=365,
               baseline=TRUE,
               UCL=1,
               algorithms=c(3,4),
               limit=1)


plot_syndromic(x=my.syndromic,
               syndromes=c(1,3),
               window=365,
               baseline=TRUE,
               UCL=1,
               algorithms=2,
               limit=1)


plot_syndromic(x=my.syndromic,
               syndromes=c(1,3),
               window=365,
               baseline=TRUE,
               UCL=1,
               algorithms=0,
               limit=1)


plot_syndromic(x=my.syndromic,
               syndromes=c(1,3),
               window=365,
               baseline=TRUE,
               UCL=1,
               algorithms=NULL,
               limit=0)


plot_syndromic(x=my.syndromic,
               syndromes=c(1,3),
               window=365,
               baseline=TRUE,
               UCL=1,
               algorithms=2,
               limit=NULL)