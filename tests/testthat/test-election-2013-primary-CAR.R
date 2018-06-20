context("election-2013-primary-CAR")

file1 <- system.file("data/PDF_data/2013 Primary",
                     "2013 Primary RTC 1 CAR.pdf",
                     package = "phillyvotes")
file2 <- system.file("data/PDF_data/2013 Primary",
                     "2013 Primary RTC 4 CAR.pdf",
                     package = "phillyvotes")
file3 <- system.file("data/PDF_data/2013 Primary",
                     "2013 Primary RTC CT CAR.pdf",
                     package = "phillyvotes")

file1 <- scrape_CAR(file1)
file2 <- scrape_CAR(file2)
file3 <- scrape_CAR(file3)

test_that("right file name", {
  expect_equal(unique(file1$file), "2013 Primary RTC 1 CAR")
  expect_equal(unique(file2$file), "2013 Primary RTC 4 CAR")
  expect_equal(unique(file3$file), "2013 Primary RTC CT CAR")
})

test_that("year values are right", {
  expect_equal(max(file1$page), 1460)
  expect_equal(max(file2$page), 795)
  expect_equal(max(file3$page), 452)
})

test_that("precinct values are right", {
  expect_equal(head(file1$precinct), c("01-01", "01-01", "01-01",
                                       "01-01", "01-01", "01-01"))
  expect_equal(tail(file1$precinct), c("51-28", "51-28", "51-28",
                                       "51-28", "51-28", "51-28"))


  expect_equal(head(file2$precinct), c("08-01", "08-01", "08-01",
                                       "08-01", "08-01", "08-01"))
  expect_equal(tail(file2$precinct), c("47-14", "47-14", "47-14",
                                       "47-14", "47-14", "47-14"))


  expect_equal(head(file3$precinct), c("05-01", "05-01", "05-01",
                                       "05-01", "05-01", "05-01"))
  expect_equal(tail(file3$precinct), c("31-19", "31-19", "31-19",
                                       "31-19", "31-19", "31-19"))

})

test_that("ward values are right", {
  expect_equal(head(file1$ward), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file1$ward), c(51, 51, 51, 51, 51, 51))


  expect_equal(head(file2$ward), c(8, 8, 8, 8, 8, 8))
  expect_equal(tail(file2$ward), c(47, 47, 47, 47, 47, 47))


  expect_equal(head(file3$ward), c(5, 5, 5, 5, 5, 5))
  expect_equal(tail(file3$ward), c(31, 31, 31, 31, 31, 31))

})

test_that("district values are right", {
  expect_equal(head(file1$district), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file1$district), c(28, 28, 28, 28, 28, 28))


  expect_equal(head(file2$district), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file2$district), c(14, 14, 14, 14, 14, 14))


  expect_equal(head(file3$district), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file3$district), c(19, 19, 19, 19, 19, 19))

})

test_that("serial number values are right", {
  expect_equal(head(file1$serial), c("020001", "020001", "020001",
                                     "020001", "020001", "020001"))
  expect_equal(tail(file1$serial), c("022516", "022516", "022516",
                                     "022516", "022516", "022516"))


  expect_equal(head(file2$serial), c("020323", "020323", "020323",
                                     "020323", "020323", "020323"))
  expect_equal(tail(file2$serial), c("022310", "022310", "022310",
                                     "022310", "022310", "022310"))


  expect_equal(head(file3$serial), c("020183", "020183", "020183",
                                     "020183", "020183", "020183"))
  expect_equal(tail(file3$serial), c("021378", "021378", "021378",
                                     "021378", "021378", "021378"))

})

test_that("hour values are right", {
  expect_equal(head(file1$hour), c(7, 7, 8, 9, 9, 11))
  expect_equal(tail(file1$hour), c(18, 18, 19, 19, 19, 19))


  expect_equal(head(file2$hour), c(7, 7, 7, 7, 8, 8))
  expect_equal(tail(file2$hour), c(10, 10, 11, 12, 15, 19))


  expect_equal(head(file3$hour), c(20, 8, 9, 11, 12, 15))
  expect_equal(tail(file3$hour), c(18, 19, 19, 19, 19, 20))

})

# test_that("year values are right", {
#   expect_equal(unique(file1$year), )
#   expect_equal(unique(file2$year), )
#   expect_equal(unique(file3$year), )
# })







test_that("all actions are voter cast ballot", {
  expect_equal(unique(file1$action), "voter cast ballot")
  expect_equal(unique(file2$action), "voter cast ballot")
  expect_equal(unique(file3$action), "voter cast ballot")
})

test_that("all actions are voter cast ballot", {
  expect_equal(sum(is.na((file1$precinct))), 0)
  expect_equal(sum(is.na((file2$precinct))), 0)
  expect_equal(sum(is.na((file3$precinct))), 0)

  # expect_equal(sum(is.na((file1$time))), 0)
  # expect_equal(sum(is.na((file2$time))), 0)
  # expect_equal(sum(is.na((file3$time))), 0)

  expect_equal(sum(is.na((file1$serial))), 0)
  expect_equal(sum(is.na((file2$serial))), 0)
  expect_equal(sum(is.na((file3$serial))), 0)

  expect_equal(sum(is.na((file1$page))), 0)
  expect_equal(sum(is.na((file2$page))), 0)
  expect_equal(sum(is.na((file3$page))), 0)

  expect_equal(sum(is.na((file1$action))), 0)
  expect_equal(sum(is.na((file2$action))), 0)
  expect_equal(sum(is.na((file3$action))), 0)

  expect_equal(sum(is.na((file1$ward))), 0)
  expect_equal(sum(is.na((file2$ward))), 0)
  expect_equal(sum(is.na((file3$ward))), 0)

  expect_equal(sum(is.na((file1$district))), 0)
  expect_equal(sum(is.na((file2$district))), 0)
  expect_equal(sum(is.na((file3$district))), 0)

  # expect_equal(sum(is.na((file1$hour))), 0)
  # expect_equal(sum(is.na((file2$hour))), 0)
  # expect_equal(sum(is.na((file3$hour))), 0)
})
