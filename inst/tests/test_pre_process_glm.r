context('Pre-process GLM')


load("data/lab.daily.rda")

test_that("minimum", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})


test_that("baseline", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),
                                    data=lab.daily)
  setBaseline(my.syndromic)<- my.syndromic@observed
  
  proc.data <- pre_process_glm(my.syndromic,slot="baseline",
                               plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("year", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,formula="dow+year",
                                 plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("dow", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),
                                    data=lab.daily)  
  proc.data <- pre_process_glm(my.syndromic,
                                 formula="dow",plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("month", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,
                               formula="month",
                                 plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("AR", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,
                               formula="AR1+AR2+AR3",
                                 plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("trend", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,
                               formula="trend",
                                 plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("syndrome_character", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,
                                 syndromes="Musculoskeletal",
                                 plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("syndrome_character_baseline", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  setBaseline(my.syndromic)<- my.syndromic@observed
  
  proc.data <- pre_process_glm(my.syndromic, slot="baseline",
                               syndromes="Musculoskeletal",
                               plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("syndrome_character2", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,
                                 syndromes="Musculoskeletal",
                                 plot=FALSE,print.model=FALSE)
  expect_true(sum(proc.data[,c(1,2,4,5)],na.rm=TRUE)==0)
})

test_that("syndrome_character_multiple", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,
                                 syndromes=c("GIT","Musculoskeletal"),
                                 plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("syndrome_character_multiple2", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,
                                 syndromes=c("GIT","Musculoskeletal"),
                                 plot=FALSE,print.model=FALSE)
  expect_true(sum(proc.data[,c(1,4,5)],na.rm=TRUE)==0)
})

test_that("syndrome_numeric", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,
                                 syndromes=3,plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("syndrome_numeric2", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,
                                 syndromes=3,plot=FALSE,print.model=FALSE)
  expect_true(sum(proc.data[,c(1,2,4,5)],na.rm=TRUE)==0)
})

test_that("syndrome_numeric_multiple", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,
                                 syndromes=c(1,3),plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("syndrome_numeric_multiple2", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,
                                 syndromes=c(1,3),plot=FALSE,print.model=FALSE)
  expect_true(sum(proc.data[,c(2,4,5)],na.rm=TRUE)==0)
})


####################
########  NB



test_that("minimumNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,family="nbinom",
                               plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})


test_that("baselineNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  setBaseline(my.syndromic)<- my.syndromic@observed
  
  proc.data <- pre_process_glm(my.syndromic,slot="baseline",
                               family="nbinom",plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("yearNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,formula="year",family="nbinom",
                               plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("dowNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,family="nbinom",
                               formula="dow",plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("monthNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,family="nbinom",
                               formula="month",
                               plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})


test_that("trendNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,family="nbinom",
                               formula="trend",
                               plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("syndrome_characterNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,family="nbinom",
                               syndromes="Musculoskeletal",
                               plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("syndrome_character_baselineNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  setBaseline(my.syndromic)<- my.syndromic@observed
  
  proc.data <- pre_process_glm(my.syndromic, slot="baseline",family="nbinom",
                               syndromes="Musculoskeletal",
                               plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("syndrome_character2NB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,
                                  syndromes="Musculoskeletal",family="nbinom",
                                  plot=FALSE,print.model=FALSE)
  expect_true(sum(proc.data[,c(1,2,4,5)],na.rm=TRUE)==0)
})

test_that("syndrome_character_multipleNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,family="nbinom",
                               syndromes=c("GIT","Musculoskeletal"),
                               plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("syndrome_character_multiple2NB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,family="nbinom",
                               syndromes=c("GIT","Musculoskeletal"),
                               plot=FALSE,print.model=FALSE)
  expect_true(sum(proc.data[,c(1,4,5)],na.rm=TRUE)==0)
})

test_that("syndrome_numericNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,family="nbinom",
                               syndromes=3,plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("syndrome_numeric2NB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,family="nbinom",
                               syndromes=3,plot=FALSE,print.model=FALSE)
  expect_true(sum(proc.data[,c(1,2,4,5)],na.rm=TRUE)==0)
})

test_that("syndrome_numeric_multipleNB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",             
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,family="nbinom",
                               syndromes=c(1,3),plot=FALSE,print.model=FALSE)
  expect_true(dim(proc.data)[1]>0)
})

test_that("syndrome_numeric_multiple2NB", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission,
                                    date.format="%d/%m/%Y",           
                                    remove.dow=c(6,0),                                     
                                    add.to=c(2,1), 
                                    data=lab.daily)
  
  proc.data <- pre_process_glm(my.syndromic,family="nbinom",
                               syndromes=c(1,3),plot=FALSE,print.model=FALSE)
  expect_true(sum(proc.data[,c(2,4,5)],na.rm=TRUE)==0)
})

