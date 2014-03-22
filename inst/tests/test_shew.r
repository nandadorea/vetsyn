my.syndromic <- raw_to_syndromic (id=SubmissionID, 
                                  syndromes.var=Syndrome,
                                  dates.var=DateofSubmission, 
                                  date.format="%d/%m/%Y", 
                                  remove.dow=c(6,0),
                                  add.to=c(2,1),
                                  sort=TRUE,
                                  data=lab.daily)


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
                          correct.baseline=FALSE,
                          alarm.dim=3,
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
                          family="nbinom",
                          formula="dow+sin+cos+AR1+AR2+AR3+AR4+AR5",
                          period=260)


my.syndromic <- shew_synd(x=my.syndromic,
                          syndromes="Musculoskeletal",
                          evaluate.window=30,
                          baseline.window=260,
                          limit.sd=c(2.5,3,3.5),
                          guard.band=5,
                          correct.baseline=FALSE,
                          alarm.dim=3,
                          UCL=2,
                          LCL=2,                          
                          pre.process="glm",
                          family="poisson",
                          formula="dow+sin+cos+AR1+AR2+AR3+AR4+AR5",
                          period=260)


my.syndromic <- ewma_synd(x=my.syndromic,
                           syndromes="Musculoskeletal",
                           evaluate.window=30,
                           baseline.window=260,
                           limit.sd=c(2.5,3,3.5),
                           guard.band=5,
                           correct.baseline=TRUE,
                           alarm.dim=3,
                           UCL=2,
                           LCL=FALSE,                          
                           pre.process="glm",
                           family="poisson",
                           formula="dow+sin+cos+AR1+AR2+AR3+AR4+AR5",
                           period=260)
