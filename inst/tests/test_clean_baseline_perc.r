context('Clean baseline Percentiles')

load("data/lab.daily.rda")

test_that("minimum", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    merge.weekends=TRUE,
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline_perc(my.syndromic,plot=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})

test_that("limit", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    merge.weekends=TRUE,
                                    data=lab.daily)
  
  my.syndromic1 <- clean_baseline_perc(my.syndromic,plot=FALSE)
  my.syndromic2 <- clean_baseline_perc(my.syndromic,
                                  limit=0.90,plot=FALSE)
  
  expect_false(sum(my.syndromic1@baseline,na.rm=TRUE)==sum(my.syndromic2@baseline,na.rm=TRUE))
})


test_that("syndrome_character", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    merge.weekends=TRUE,
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline_perc(my.syndromic,
                                 syndromes="Musculoskeletal",
                                 plot=FALSE)
  expect_true(sum(my.syndromic@baseline[,3],na.rm=TRUE)>0)
})

test_that("syndrome_character2", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    merge.weekends=TRUE,
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline_perc(my.syndromic,
                                 syndromes="Musculoskeletal",
                                 plot=FALSE)
  expect_true(sum(my.syndromic@baseline[,c(1,2,4,5)],na.rm=TRUE)==0)
})

test_that("syndrome_character_multiple", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    merge.weekends=TRUE,
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline_perc(my.syndromic,
                                 syndromes=c("GIT","Musculoskeletal"),
                                 plot=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})

test_that("syndrome_character_multiple2", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    merge.weekends=TRUE,
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline_perc(my.syndromic,
                                 syndromes=c("GIT","Musculoskeletal"),
                                      plot=FALSE)
  expect_true(sum(my.syndromic@baseline[,c(1,4,5)],na.rm=TRUE)==0)
})

test_that("syndrome_numeric", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    merge.weekends=TRUE,
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline_perc(my.syndromic,
                                 syndromes=3,plot=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})

test_that("syndrome_numeric2", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    merge.weekends=TRUE,
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline_perc(my.syndromic,
                                 syndromes=3,plot=FALSE)
  expect_true(sum(my.syndromic@baseline[,c(1,2,4,5)],na.rm=TRUE)==0)
})

test_that("syndrome_numeric_multiple", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    merge.weekends=TRUE,
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline_perc(my.syndromic,
                                 syndromes=c(1,3),plot=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})

test_that("syndrome_numeric_multiple2", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    merge.weekends=TRUE,
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline_perc(my.syndromic,
                                 syndromes=c(1,3),plot=FALSE)
  expect_true(sum(my.syndromic@baseline[,c(2,4,5)],na.rm=TRUE)==0)
})

