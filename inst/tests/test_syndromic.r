context('Syndromic')

load("data/observed.rda")

test_that("minimum", {
  expect_is(my.syndromic <- syndromic(observed[1:5,]),"syndromic")
})

test_that("date_dimensions", {
  expect_error(my.syndromic <- syndromic(observed[1:6,],
                                         min.date="01/01/2010",max.date="08/01/2010"))
})


test_that("date_dimensions_weekend", {
  expect_is(my.syndromic <- syndromic(observed[1:6,],
                                      min.date="01/01/2010",max.date="08/01/2010",
                                      weekends=FALSE),"syndromic")
})

test_that("separate_date", {
  dates = seq(from=as.Date("01/01/2010",format ="%d/%m/%Y" ),
              to=as.Date("05/01/2010",format ="%d/%m/%Y" ), by="days")
  
  expect_is(my.syndromic <- syndromic(observed[1:5,],
                                      dates=dates),"syndromic")
})

test_that("date_columns", {
  my.syndromic <- syndromic(observed[1:6,],
                            min.date="01/01/2010",max.date="06/01/2010")
  
  expect_true(colnames(my.syndromic@dates)[1]=="dates")
})