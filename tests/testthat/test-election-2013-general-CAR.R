context("election-2013-general-CAR")

file1 <- system.file("data/PDF_data/2013 General",
                     "RTC 4 CAR.pdf",
                     package = "phillyvotes")
file2 <- system.file("data/PDF_data/2013 General",
                     "RTC 3 CAR.pdf",
                     package = "phillyvotes")
file3 <- system.file("data/PDF_data/2013 General",
                     "RTC 1 CAR.pdf",
                     package = "phillyvotes")

file1 <- scrape_CAR(file1)
file2 <- scrape_CAR(file2)
file3 <- scrape_CAR(file3)

test_that("right file name", {
  expect_equal(unique(file1$file), "RTC 4 CAR")
  expect_equal(unique(file2$file), "RTC 3 CAR")
  expect_equal(unique(file3$file), "RTC 1 CAR")
})

test_that("year values are right", {
  expect_equal(max(file1$page), 642)
  expect_equal(max(file2$page), 897)
  expect_equal(max(file3$page), 1089)
})

test_that("precinct values are right", {
  expect_equal(head(file1$precinct), c("08-01", "08-01", "08-01",
                                       "08-01", "08-01", "08-01"))
  expect_equal(tail(file1$precinct), c("47-14", "47-14", "47-14",
                                       "47-14", "47-14", "47-14"))


  expect_equal(head(file2$precinct), c("56-01", "56-01", "56-01",
                                       "56-01", "56-01", "56-01"))
  expect_equal(tail(file2$precinct), c("66-46", "66-46", "66-46",
                                       "66-46", "66-46", "66-46"))


  expect_equal(head(file3$precinct), c("01-08", "01-08", "01-08",
                                       "01-08", "01-08", "01-08"))
  expect_equal(tail(file3$precinct), c("58-41", "58-41", "58-41",
                                       "58-41", "58-41", "58-41"))


})

test_that("ward values are right", {
  expect_equal(head(file1$ward), c(8, 8, 8, 8, 8, 8))
  expect_equal(tail(file1$ward), c(47, 47, 47, 47, 47, 47))


  expect_equal(head(file2$ward), c(56, 56, 56, 56, 56, 56))
  expect_equal(tail(file2$ward), c(66, 66, 66, 66, 66, 66))


  expect_equal(head(file3$ward), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file3$ward), c(58, 58, 58, 58, 58, 58))


})

test_that("division values are right", {
  expect_equal(head(file1$division), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file1$division), c(14, 14, 14, 14, 14, 14))


  expect_equal(head(file2$division), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file2$division), c(46, 46, 46, 46, 46, 46))


  expect_equal(head(file3$division), c(8, 8, 8, 8, 8, 8))
  expect_equal(tail(file3$division), c(41, 41, 41, 41, 41, 41))


})

test_that("serial number values are right", {
  expect_equal(head(file1$serial), c("020323", "020323", "020323",
                                     "020323", "020323", "020323"))
  expect_equal(tail(file1$serial), c("022310", "022310", "022310",
                                     "022310", "022310", "022310"))


  expect_equal(head(file2$serial), c("022721", "022721", "022721",
                                     "022721", "022721", "022721"))
  expect_equal(tail(file2$serial), c("023374", "023374", "023374",
                                     "023374", "023374", "023374"))


  expect_equal(head(file3$serial), c("020016", "020016", "020016",
                                     "020016", "020016", "020016"))
  expect_equal(tail(file3$serial), c("022940", "022940", "022940",
                                     "022940", "022940", "022940"))

})

test_that("hour values are right", {
  expect_equal(head(file1$hour), c(7, 7, 7, 7, 8, 8))
  expect_equal(tail(file1$hour), c(16, 17, 18, 18, 18, 18))


  expect_equal(head(file2$hour), c(7, 7, 7, 8, 8, 8))
  expect_equal(tail(file2$hour), c(19, 19, 19, 19, 19, 19))


  expect_equal(head(file3$hour), c(14, 14, 15, 16, 17, 18))
  expect_equal(tail(file3$hour), c(19, 19, 19, 19, 19, 20))


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

  expect_equal(sum(is.na((file1$division))), 0)
  expect_equal(sum(is.na((file2$division))), 0)
  expect_equal(sum(is.na((file3$division))), 0)

  # expect_equal(sum(is.na((file1$hour))), 0)
  # expect_equal(sum(is.na((file2$hour))), 0)
  # expect_equal(sum(is.na((file3$hour))), 0)
})

