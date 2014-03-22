context('Raw to syndromic')

load("data/lab.daily.rda")
load("data/observed.rda")

test_that("minimum", {
  
  #mainly just want to test that the example works, since the 
  #things that could go wrong are already checked for in the validity of the object.
  my.syndromic <- raw_to_syndromic (id=lab.daily$SubmissionID, 
                                    syndromes.var=lab.daily$Syndrome,
                                    dates.var=lab.daily$DateofSubmission,
                                    date.format="%d/%m/%Y", 
                                    sort=TRUE,
                                    data=NULL)
  
  
  expect_is(my.syndromic,"syndromic")
})

test_that("minimum.date", {
  
  my.syndromic <- raw_to_syndromic (id=lab.daily$SubmissionID, 
                                    syndromes.var=lab.daily$Syndrome,
                                    dates.var=lab.daily$DateofSubmission,
                                    date.format="%d/%m/%Y", 
                                    sort=TRUE,
                                    data=NULL)
  
  
  expect_true(colnames(my.syndromic@dates)[1]=="dates")
})


test_that("min.col.names", {
  
  my.syndromic <- raw_to_syndromic (id=lab.daily$SubmissionID, 
                                    syndromes.var=lab.daily$Syndrome,
                                    dates.var=lab.daily$DateofSubmission,
                                    date.format="%d/%m/%Y", 
                                    sort=TRUE,
                                    data=NULL)
  
  
  expect_equal(min(colnames(my.syndromic@observed)==colnames(observed)),1)
})


test_that("minimum.data", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID, 
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission, 
                                    date.format="%d/%m/%Y", 
                                    sort=TRUE,
                                    data=lab.daily)
  
  expect_is(my.syndromic,"syndromic")
})


test_that("multipleID", {
  
  my.syndromic <- raw_to_syndromic (id=list(HerdID,AnimalID), 
                                    syndromes.var=Syndrome,
                                    dates.var=DateofSubmission, 
                                    date.format="%d/%m/%Y", 
                                    sort=TRUE,
                                    data=lab.daily)
  
  expect_is(my.syndromic,"syndromic")
})


test_that("multipleID2", {
  
  my.syndromic1 <- raw_to_syndromic (id=list(HerdID,AnimalID), 
                                     syndromes.var=Syndrome,
                                     dates.var=DateofSubmission, 
                                     date.format="%d/%m/%Y", 
                                     sort=TRUE,
                                     data=lab.daily)
  my.syndromic2 <- raw_to_syndromic (id=AnimalID, 
                                     syndromes.var=Syndrome,
                                     dates.var=DateofSubmission, 
                                     date.format="%d/%m/%Y", 
                                     sort=TRUE,
                                     data=lab.daily)
  my.syndromic3 <- raw_to_syndromic (id=HerdID, 
                                     syndromes.var=Syndrome,
                                     dates.var=DateofSubmission, 
                                     date.format="%d/%m/%Y", 
                                     sort=TRUE,
                                     data=lab.daily)
  
  
  expect_false(sum(my.syndromic1@observed[,1],na.rm=TRUE)==sum(
    my.syndromic2@observed[,1],na.rm=TRUE))
  expect_false(sum(my.syndromic1@observed[,1],na.rm=TRUE)==sum(
    my.syndromic3@observed[,1],na.rm=TRUE))
  
})




test_that("specific.syndromes", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID, 
                                    syndromes.var=Syndrome,
                                    syndromes.name=c("GIT","Musculoskeletal"),
                                    dates.var=DateofSubmission, 
                                    date.format="%d/%m/%Y", 
                                    sort=TRUE,
                                    data=lab.daily)
  
  expect_is(my.syndromic,"syndromic")
})



test_that("specific.syndromes.colnames", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID, 
                                    syndromes.var=Syndrome,
                                    syndromes.name=c("GIT","Musculoskeletal"),
                                    dates.var=DateofSubmission, 
                                    date.format="%d/%m/%Y", 
                                    sort=TRUE,
                                    data=lab.daily)
  
  expect_equal(min(colnames(my.syndromic@observed)==c("GIT","Musculoskeletal")),1)
})



test_that("fake.syndrome", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID, 
                                    syndromes.var=Syndrome,
                                    syndromes.name=c("GIT","Musculoskeletal","NonExisting"),
                                    dates.var=DateofSubmission, 
                                    date.format="%d/%m/%Y", 
                                    #min.date, max.date,
                                    sort=TRUE,
                                    data=lab.daily)
  expect_is(my.syndromic,"syndromic")
})

test_that("fake.syndrome2", {
  
  #same as before, just with syndromes in different order
  #just to make sure that it doesn't depend on the zero-count
  #syndrome coming lst (after dimensions were already set
  #by the other vectors)
  my.syndromic <- raw_to_syndromic (id=SubmissionID, 
                                    syndromes.var=Syndrome,
                                    syndromes.name=c("NonExisting","GIT","Musculoskeletal"),
                                    dates.var=DateofSubmission, 
                                    date.format="%d/%m/%Y", 
                                    #min.date, max.date,
                                    sort=TRUE,
                                    data=lab.daily)
  expect_is(my.syndromic,"syndromic")
})



test_that("fake.syndrome.colnames", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID, 
                                    syndromes.var=Syndrome,
                                    syndromes.name=c("GIT","Musculoskeletal","NonExisting"),
                                    dates.var=DateofSubmission, 
                                    date.format="%d/%m/%Y", 
                                    #min.date, max.date,
                                    sort=TRUE,
                                    data=lab.daily)
  expect_equal(min(colnames(my.syndromic@observed)==c("GIT","Musculoskeletal","NonExisting")),1)
})


test_that("min.date", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID, 
                                    syndromes.var=Syndrome,
                                    #syndromes.name,
                                    dates.var=DateofSubmission, 
                                    date.format="%d/%m/%Y", 
                                    min.date="01/01/2011", 
                                    #max.date,
                                    sort=TRUE,
                                    data=lab.daily)
  expect_is(my.syndromic,"syndromic")
})


test_that("min.date.col", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID, 
                                    syndromes.var=Syndrome,
                                    #syndromes.name,
                                    dates.var=DateofSubmission, 
                                    date.format="%d/%m/%Y", 
                                    min.date="01/01/2011", 
                                    #max.date,
                                    sort=TRUE,
                                    data=lab.daily)
  expect_true(colnames(my.syndromic@dates)[1]=="dates")
})


test_that("min.max.date", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID, 
                                    syndromes.var=Syndrome,
                                    #syndromes.name,
                                    dates.var=DateofSubmission, 
                                    date.format="%d/%m/%Y", 
                                    min.date="01/01/2011", 
                                    max.date="01/01/2014",
                                    sort=TRUE,
                                    data=lab.daily)
  expect_is(my.syndromic,"syndromic")
})


test_that("max.date", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID, 
                                    syndromes.var=Syndrome,
                                    #syndromes.name,
                                    dates.var=DateofSubmission, 
                                    date.format="%d/%m/%Y", 
                                    #min.date="01/01/2011", 
                                    max.date="01/01/2014",
                                    sort=TRUE,
                                    data=lab.daily)
  expect_is(my.syndromic,"syndromic")
})


test_that("merge.weekends", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID, 
                                    syndromes.var=Syndrome,
                                    #syndromes.name,
                                    dates.var=DateofSubmission, 
                                    date.format="%d/%m/%Y", 
                                    #min.date="01/01/2011", 
                                    max.date="01/01/2014",
                                    remove.dow=c(6,0),
                                    add.to=c(2,1),
                                    sort=TRUE,
                                    data=lab.daily)
  expect_is(my.syndromic,"syndromic")
})


test_that("no.sort", {
  
  my.syndromic <- raw_to_syndromic (id=SubmissionID, 
                                    syndromes.var=Syndrome,
                                    #syndromes.name,
                                    dates.var=DateofSubmission, 
                                    date.format="%d/%m/%Y", 
                                    #min.date="01/01/2011", 
                                    max.date="01/01/2014",
                                    sort=FALSE,
                                    data=lab.daily)
  expect_is(my.syndromic,"syndromic")
})




