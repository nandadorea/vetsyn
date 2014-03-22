context('Clean baseline GLM')

load("data/lab.daily.rda")

test_that("minimum", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})

test_that("limit", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic1 <- clean_baseline(my.syndromic,plot=FALSE,print.model=FALSE)
  my.syndromic2 <- clean_baseline(my.syndromic,
                                  limit=0.90,plot=FALSE,print.model=FALSE)
  
  expect_false(sum(my.syndromic1@baseline,na.rm=TRUE)==sum(my.syndromic2@baseline,na.rm=TRUE))
})


test_that("year", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,formula="year",
                                 plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})

test_that("dow", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 formula="dow",plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})

test_that("month", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 formula="month",
                                 plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})


test_that("trend", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 formula="dow+trend",
                                 plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})

test_that("syndrome_character", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 syndromes="Musculoskeletal",
                                 plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline[,3],na.rm=TRUE)>0)
})

test_that("syndrome_character2", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 syndromes="Musculoskeletal",
                                 plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline[,c(1,2,4,5)],na.rm=TRUE)==0)
})

test_that("syndrome_character_multiple", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 syndromes=c("GIT","Musculoskeletal"),
                                 plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})

test_that("syndrome_character_multiple2", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 syndromes=c("GIT","Musculoskeletal"),
                                 plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline[,c(1,4,5)],na.rm=TRUE)==0)
})

test_that("syndrome_numeric", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 syndromes=3,plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})

test_that("syndrome_numeric2", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 syndromes=3,plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline[,c(1,2,4,5)],na.rm=TRUE)==0)
})

test_that("syndrome_numeric_multiple", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 syndromes=c(1,3),plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})

test_that("syndrome_numeric_multiple2", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 syndromes=c(1,3),plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline[,c(2,4,5)],na.rm=TRUE)==0)
})



####################nbinom

test_that("minimumNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 family="nbinom",plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})


test_that("limitNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic1 <- clean_baseline(my.syndromic,plot=FALSE,print.model=FALSE)
  my.syndromic2 <- clean_baseline(my.syndromic,
                                  limit=0.90,
                                  family="nbinom", plot=FALSE,print.model=FALSE)
  
  expect_false(sum(my.syndromic1@baseline,na.rm=TRUE)==sum(my.syndromic2@baseline,na.rm=TRUE))
})


test_that("yearNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,formula="year",
                                 family="nbinom", plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})

test_that("dowNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 formula="dow",
                                 family="nbinom", plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})

test_that("monthNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 formula="month",
                                 family="nbinom", plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})


test_that("trendNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 formula="trend",
                                 family="nbinom", plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})

test_that("syndrome_characterNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 syndromes="Musculoskeletal",
                                 family="nbinom",plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline[,3],na.rm=TRUE)>0)
})

test_that("syndrome_character2NB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 syndromes="Musculoskeletal",
                                 family="nbinom",plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline[,c(1,2,4,5)],na.rm=TRUE)==0)
})

test_that("syndrome_character_multipleNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 syndromes=c("GIT","Musculoskeletal"),
                                 family="nbinom",plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})

test_that("syndrome_character_multiple2NB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 syndromes=c("GIT","Musculoskeletal"),
                                 family="nbinom",plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline[,c(1,4,5)],na.rm=TRUE)==0)
})

test_that("syndrome_numericNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,
                                 syndromes=3,
                                 family="nbinom",plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})

test_that("syndrome_numeric2NB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,syndromes=3,
                                 family="nbinom",plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline[,c(1,2,4,5)],na.rm=TRUE)==0)
})

test_that("syndrome_numeric_multipleNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,syndromes=c(1,3),
                                 family="nbinom",plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline,na.rm=TRUE)>0)
})

test_that("syndrome_numeric_multiple2NB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),                                    
                                    data=lab.daily)
  
  my.syndromic <- clean_baseline(my.syndromic,syndromes=c(1,3),
                                 family="nbinom",plot=FALSE,print.model=FALSE)
  expect_true(sum(my.syndromic@baseline[,c(2,4,5)],na.rm=TRUE)==0)
})
