context('EWMA')

load("data/lab.daily.rda")

test_that("minimum", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic)
  
 expect_true(dim(my.syndromic@alarms)>0&&sum(my.syndromic@UCL,na.rm=TRUE)==0)
})

test_that("minimum_UCL", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic,correct.baseline=2)
  
  expect_true(dim(my.syndromic@alarms)>0&&sum(my.syndromic@UCL,na.rm=TRUE)>0)
})

test_that("minimum_clean_baseline", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- clean_baseline(my.syndromic,plot=FALSE,print=FALSE)
  my.syndromic <- ewma_synd(x=my.syndromic)
  
  expect_true(dim(my.syndromic@alarms)[[3]]>0)
})


test_that("syndromes", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic,syndromes="Musculoskeletal",correct.baseline=2)
  
  expect_true(dim(my.syndromic@alarms)>0&&
                sum(my.syndromic@UCL,na.rm=TRUE)>0&&
                sum(my.syndromic@alarms[,c(1,2,4,5),],na.rm=TRUE)==0&&
                sum(my.syndromic@UCL[,c(1,2,4,5),],na.rm=TRUE)==0)
})


test_that("evaluate.window", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic,evaluate.window=5,correct.baseline=2)
  
  expect_true(dim(my.syndromic@alarms)>0&&
                sum(my.syndromic@UCL,na.rm=TRUE)>0&&
                sum(!is.na(my.syndromic@alarms[1:(dim(my.syndromic@alarms)[1]-
                                                    5),,]),na.rm=TRUE)==0&&
                sum(!is.na(my.syndromic@alarms[(dim(my.syndromic@alarms)[1]-
                                                  5+1):(dim(my.syndromic@alarms)[1]),,]),
                    na.rm=TRUE)>0&&
                sum(!is.na(my.syndromic@UCL[1:(dim(my.syndromic@UCL)[1]-
                                                    5),,]),na.rm=TRUE)==0&&
                sum(!is.na(my.syndromic@UCL[(dim(my.syndromic@UCL)[1]-
                                                  5+1):(dim(my.syndromic@UCL)[1]),,]),
                    na.rm=TRUE)>0)
                
})



test_that("baseline.window", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic,baseline.window=90,correct.baseline=2)
  
  expect_true(dim(my.syndromic@alarms)>0&&sum(my.syndromic@UCL,na.rm=TRUE)>0)
})


test_that("lambda", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic,lambda=0.5,correct.baseline=2)
  
  expect_true(dim(my.syndromic@alarms)>0&&sum(my.syndromic@UCL,na.rm=TRUE)>0)
})




test_that("limit.sd", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic1 <- ewma_synd(x=my.syndromic,limit.sd=c(3),evaluate.window=20,correct.baseline=2)
  my.syndromic2 <- ewma_synd(x=my.syndromic,limit.sd=c(1.5,2,3),evaluate.window=20,correct.baseline=2)
  
  
  expect_true(sum(my.syndromic1@alarms,na.rm=TRUE)<sum(my.syndromic2@alarms,na.rm=TRUE)&&
                sum(my.syndromic1@UCL,na.rm=TRUE)>sum(my.syndromic2@UCL,na.rm=TRUE))
})


test_that("guard.band", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic,guard.band=3,correct.baseline=2)
  
  expect_true(dim(my.syndromic@alarms)>0&&sum(my.syndromic@UCL,na.rm=TRUE)>0)
})


test_that("NO_UCL", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic,UCL=FALSE)
  
  expect_true(dim(my.syndromic@UCL)==0)
})


test_that("NO_LCL", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic)
  
  expect_true(dim(my.syndromic@LCL)==0)
})


test_that("YES_LCL", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic,LCL=1,correct.baseline=1)
  
  expect_true(dim(my.syndromic@LCL)[[3]]>0)
})


test_that("alarm.dim", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic,alarm.dim=3, correct.baseline=3)
  
  expect_true(dim(my.syndromic@UCL)[3]==3&&
                dim(my.syndromic@alarms)[3]==3&&
                sum(my.syndromic@alarms[,,1:2],na.rm=TRUE)==0)
})



test_that("GLM-poisson", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic,pre.process="glm",family="poisson")
  
  expect_true(dim(my.syndromic@alarms)[[3]]>0)
})


test_that("GLM-period", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic,pre.process="glm",family="poisson",
                            formula="sin+cos")
  
  expect_true(dim(my.syndromic@alarms)[[3]]>0)
})


test_that("GLM-period2", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic,pre.process="glm",family="poisson",
                            formula="sin+cos",period=265)
  
  expect_true(dim(my.syndromic@alarms)[[3]]>0)
})


test_that("GLM-poisson-formula", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic,pre.process="glm",family="poisson",
                            formula="dow+month+trend")
  
  expect_true(dim(my.syndromic@alarms)[[3]]>0)
})


test_that("GLM-Nbinom", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic,pre.process="glm",family="nbinom")
  
  expect_true(dim(my.syndromic@alarms)[[3]]>0)
})


test_that("GLM-Nbinom-formula", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic,pre.process="glm",family="nbinom",
                            formula="dow+month+year",correct.baseline=1)
  
  expect_true(dim(my.syndromic@alarms)>0&&sum(my.syndromic@UCL,na.rm=TRUE)>0)
})


test_that("differencing", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic,pre.process="diff",correct.baseline=1)
  
  expect_true(dim(my.syndromic@alarms)>0&&sum(my.syndromic@UCL,na.rm=TRUE)>0)
})


test_that("differencing2", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- ewma_synd(x=my.syndromic,pre.process="diff", diff.window=5)
  
  expect_true(dim(my.syndromic@alarms)[[3]]>0)
})