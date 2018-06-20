context("election-2012-general-CAR")

file1 <- system.file("data/PDF_data/2012 General",
                     "2012 General RTC 1 CAR 1 of 2.pdf",
                     package = "phillyvotes")
file2 <- system.file("data/PDF_data/2012 General",
                     "2012 General RTC 3 CAR 1 of 2.pdf",
                     package = "phillyvotes")
file3 <- system.file("data/PDF_data/2012 General",
                     "2012 General RTC BT2 CAR 2 of 2.pdf",
                     package = "phillyvotes")
file4 <- system.file("data/PDF_data/2012 General",
                     "2012 General RTC 4 CAR.pdf",
                     package = "phillyvotes")
file5 <- system.file("data/PDF_data/2012 General",
                     "2012 General RTC 6 CAR.pdf",
                     package = "phillyvotes")

file1 <- scrape_CAR(file1)
file2 <- scrape_CAR(file2)
file3 <- scrape_CAR(file3)
file4 <- scrape_CAR(file4)
file5 <- scrape_CAR(file5)

test_that("right file name", {
  expect_equal(unique(file1$file), "2012 General RTC 1 CAR 1 of 2")
  expect_equal(unique(file2$file), "2012 General RTC 3 CAR 1 of 2")
  expect_equal(unique(file3$file), "2012 General RTC BT2 CAR 2 of 2")
  expect_equal(unique(file4$file), "2012 General RTC 4 CAR")
  expect_equal(unique(file5$file), "2012 General RTC 6 CAR")
})

# test_that("year values are right", {
#   expect_equal(unique(file1$year), 2012)
#   expect_equal(unique(file2$year), 2012)
#   expect_equal(unique(file3$year), 2012)
#   expect_equal(unique(file4$year), 2012)
#   expect_equal(unique(file5$year), 2012)
# })

test_that("precinct values are right", {
  expect_equal(head(file1$precinct), c("01-01", "01-01", "01-01",
                                       "01-01", "01-01", "01-01"))
  expect_equal(tail(file1$precinct), c("39-05", "39-05", "39-05",
                                       "39-05", "39-05", "39-05"))


  expect_equal(head(file2$precinct), c("56-01", "56-01", "56-01",
                                       "56-01", "56-01", "56-01"))
  expect_equal(tail(file2$precinct), c("63-02", "63-02", "63-02",
                                       "63-02", "63-02", "63-02"))


  expect_equal(head(file3$precinct), c("21-43", "21-43", "21-43",
                                       "21-43", "21-43", "21-43"))
  expect_equal(tail(file3$precinct), c("61-28", "61-28", "61-28",
                                       "61-28", "61-28", "61-28"))


  expect_equal(head(file4$precinct), c("08-01", "08-01", "08-01",
                                       "08-01", "08-01", "08-01"))
  expect_equal(tail(file4$precinct), c("47-14", "47-14", "47-14",
                                       "47-14", "47-14", "47-14"))


  expect_equal(head(file5$precinct), c("07-01", "07-01", "07-01",
                                       "07-01", "07-01", "07-01"))
  expect_equal(tail(file5$precinct), c("45-25", "45-25", "45-25",
                                       "45-25", "45-25", "45-25"))
})

test_that("ward values are right", {
  expect_equal(head(file1$ward), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file1$ward), c(39, 39, 39, 39, 39, 39))


  expect_equal(head(file2$ward), c(56, 56, 56, 56, 56, 56))
  expect_equal(tail(file2$ward), c(63, 63, 63, 63, 63, 63))


  expect_equal(head(file3$ward), c(21, 21, 21, 21, 21, 21))
  expect_equal(tail(file3$ward), c(61, 61, 61, 61, 61, 61))


  expect_equal(head(file4$ward), c(8, 8, 8, 8, 8, 8))
  expect_equal(tail(file4$ward), c(47, 47, 47, 47, 47, 47))


  expect_equal(head(file5$ward), c(7, 7, 7, 7, 7, 7))
  expect_equal(tail(file5$ward), c(45, 45, 45, 45, 45, 45))
})

test_that("division values are right", {
  expect_equal(head(file1$division), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file1$division), c(5, 5, 5, 5, 5, 5))


  expect_equal(head(file2$division), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file2$division), c(2, 2, 2, 2, 2, 2))


  expect_equal(head(file3$division), c(43, 43, 43, 43, 43, 43))
  expect_equal(tail(file3$division), c(28, 28, 28, 28, 28, 28))


  expect_equal(head(file4$division), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file4$division), c(14, 14, 14, 14, 14, 14))


  expect_equal(head(file5$division), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file5$division), c(25, 25, 25, 25, 25, 25))
})

test_that("serial number values are right", {
  expect_equal(head(file1$serial), c("020001", "020001", "020001",
                                     "020001", "020001", "020001"))
  expect_equal(tail(file1$serial), c("021811", "021811", "021811",
                                     "021811", "021811", "021811"))


  expect_equal(head(file2$serial), c("022721", "022721", "022721",
                                     "022721", "022721", "022721"))
  expect_equal(tail(file2$serial), c("023153", "023153", "023153",
                                     "023153", "023153", "023153"))


  expect_equal(head(file3$serial), c("020945", "020945", "020945",
                                     "020946", "020946", "020946"))
  expect_equal(tail(file3$serial), c("023098", "023098", "023098",
                                     "023098", "023098", "023098"))


  expect_equal(head(file4$serial), c("020323", "020323", "020323",
                                     "020323", "020323", "020323"))
  expect_equal(tail(file4$serial), c("022310", "022310", "022310",
                                     "022310", "022310", "022310"))


  expect_equal(head(file5$serial), c("020277", "020277", "020277",
                                     "020277", "020277", "020277"))
  expect_equal(tail(file5$serial), c("022236", "022236", "022236",
                                     "022236", "022236", "022236"))
})

test_that("hour values are right", {
  expect_equal(head(file1$hour), c(7, 7, 7, 7, 7, 7))
  expect_equal(tail(file1$hour), c(18, 19, 19, 19, 19, 20))


  expect_equal(head(file2$hour), c(7, 7, 7, 7, 7, 7))
  expect_equal(tail(file2$hour), c(14, 14, 14, 14, 14, 14))


  expect_equal(head(file3$hour), c(19, 19, 19, 7, 7, 7))
  expect_equal(tail(file3$hour), c(19, 19, 19, 19, 19, 19))


  expect_equal(head(file4$hour), c(7, 7, 7, 7, 7, 7))
  expect_equal(tail(file4$hour), c(18, 18, 19, 19, 19, 20))


  expect_equal(head(file5$hour), c(7, 7, 7, 7, 7, 8))
  expect_equal(tail(file5$hour), c(19, 19, 19, 19, 19, 19))
})







test_that("all actions are voter cast ballot", {
  expect_equal(unique(file1$action), "voter cast ballot")
  expect_equal(unique(file2$action), "voter cast ballot")
  expect_equal(unique(file3$action), "voter cast ballot")
  expect_equal(unique(file4$action), "voter cast ballot")
  expect_equal(unique(file5$action), "voter cast ballot")
})

test_that("all actions are voter cast ballot", {
  expect_equal(sum(is.na((file1$precinct))), 0)
  expect_equal(sum(is.na((file2$precinct))), 0)
  expect_equal(sum(is.na((file3$precinct))), 0)
  expect_equal(sum(is.na((file4$precinct))), 0)
  expect_equal(sum(is.na((file5$precinct))), 0)

  # expect_equal(sum(is.na((file1$time))), 0)
  # expect_equal(sum(is.na((file2$time))), 0)
  # expect_equal(sum(is.na((file3$time))), 0)
  # expect_equal(sum(is.na((file4$time))), 0)
  # expect_equal(sum(is.na((file5$time))), 0)

  expect_equal(sum(is.na((file1$serial))), 0)
  expect_equal(sum(is.na((file2$serial))), 0)
  expect_equal(sum(is.na((file3$serial))), 0)
  expect_equal(sum(is.na((file4$serial))), 0)
  expect_equal(sum(is.na((file5$serial))), 0)

  expect_equal(sum(is.na((file1$page))), 0)
  expect_equal(sum(is.na((file2$page))), 0)
  expect_equal(sum(is.na((file3$page))), 0)
  expect_equal(sum(is.na((file4$page))), 0)
  expect_equal(sum(is.na((file5$page))), 0)

  expect_equal(sum(is.na((file1$action))), 0)
  expect_equal(sum(is.na((file2$action))), 0)
  expect_equal(sum(is.na((file3$action))), 0)
  expect_equal(sum(is.na((file4$action))), 0)
  expect_equal(sum(is.na((file5$action))), 0)

  expect_equal(sum(is.na((file1$ward))), 0)
  expect_equal(sum(is.na((file2$ward))), 0)
  expect_equal(sum(is.na((file3$ward))), 0)
  expect_equal(sum(is.na((file4$ward))), 0)
  expect_equal(sum(is.na((file5$ward))), 0)

  expect_equal(sum(is.na((file1$division))), 0)
  expect_equal(sum(is.na((file2$division))), 0)
  expect_equal(sum(is.na((file3$division))), 0)
  expect_equal(sum(is.na((file4$division))), 0)
  expect_equal(sum(is.na((file5$division))), 0)

  # expect_equal(sum(is.na((file1$hour))), 0)
  # expect_equal(sum(is.na((file2$hour))), 0)
  # expect_equal(sum(is.na((file3$hour))), 0)
  # expect_equal(sum(is.na((file4$hour))), 0)
  # expect_equal(sum(is.na((file5$hour))), 0)
})

