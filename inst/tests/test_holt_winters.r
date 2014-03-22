context('Holt Winters')

load("data/lab.daily.rda")

test_that("minimum", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- holt_winters_synd(x=my.syndromic)
  
 expect_true(dim(my.syndromic@alarms)>0&&sum(my.syndromic@UCL,na.rm=TRUE)>0)
})

test_that("minimum_clean_baseline", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- clean_baseline(my.syndromic,plot=FALSE,print=FALSE)
  my.syndromic <- holt_winters_synd(x=my.syndromic)
  
  expect_true(dim(my.syndromic@alarms)>0&&sum(my.syndromic@UCL,na.rm=TRUE)>0)
})


test_that("syndromes", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- holt_winters_synd(x=my.syndromic,syndromes="Musculoskeletal")
  
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
  my.syndromic <- holt_winters_synd(x=my.syndromic,evaluate.window=5)
  
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


test_that("frequency", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- holt_winters_synd(x=my.syndromic,frequency=5)
  
  expect_true(dim(my.syndromic@alarms)>0&&sum(my.syndromic@UCL,na.rm=TRUE)>0)
})


test_that("baseline.window", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- holt_winters_synd(x=my.syndromic,baseline.window=90)
  
  expect_true(dim(my.syndromic@alarms)>0&&sum(my.syndromic@UCL,na.rm=TRUE)>0)
})


test_that("limit.sd", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic1 <- holt_winters_synd(x=my.syndromic,limit.sd=c(3),evaluate.window=20)
  my.syndromic2 <- holt_winters_synd(x=my.syndromic,limit.sd=c(1.5,2,3),evaluate.window=20)
  
  
  expect_true(sum(my.syndromic1@alarms,na.rm=TRUE)<sum(my.syndromic2@alarms,na.rm=TRUE)&&
                sum(my.syndromic1@UCL,na.rm=TRUE)>sum(my.syndromic2@UCL,na.rm=TRUE))
})


test_that("nahead", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- holt_winters_synd(x=my.syndromic,nahead=2)
  
  expect_true(dim(my.syndromic@alarms)>0&&sum(my.syndromic@UCL,na.rm=TRUE)>0)
})


test_that("correct.baseline", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic1 <- holt_winters_synd(x=my.syndromic)
  my.syndromic2 <- holt_winters_synd(x=my.syndromic,correct.baseline=3)
  
  expect_true(sum(my.syndromic1@UCL,na.rm=TRUE)!=sum(my.syndromic2@UCL,na.rm=TRUE))
})

test_that("NOT_correct.baseline", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- clean_baseline(my.syndromic,plot=FALSE,print=FALSE)
  my.syndromic1 <- holt_winters_synd(x=my.syndromic,correct.baseline=0)
  
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)==sum(my.syndromic1@baseline,na.rm=TRUE))
})

test_that("NO_UCL", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- holt_winters_synd(x=my.syndromic,UCL=FALSE)
  
  expect_true(dim(my.syndromic@UCL)==0)
})


test_that("alarm.dim", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    data=lab.daily)
  my.syndromic <- holt_winters_synd(x=my.syndromic,alarm.dim=3)
  
  expect_true(dim(my.syndromic@UCL)[3]==3&&
                dim(my.syndromic@alarms)[3]==3&&
                sum(my.syndromic@alarms[,,1:2],na.rm=TRUE)==0)
})


