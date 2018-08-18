context("election-2012-primary-CAR")

file1 <- system.file("data/PDF_data/2012 Primary",
                     "2012 Primary RTC 3 CAR.pdf",
                     package = "phillyvotes")
file2 <- system.file("data/PDF_data/2012 Primary",
                     "2012 Primary RTC 1 CAR.pdf",
                     package = "phillyvotes")
file3 <- system.file("data/PDF_data/2012 Primary",
                     "2012 Primary RTC 2 CAR.pdf",
                     package = "phillyvotes")
file4 <- system.file("data/PDF_data/2012 Primary",
                     "2012 Primary RTC 4 CAR.pdf",
                     package = "phillyvotes")
file5 <- system.file("data/PDF_data/2012 Primary",
                     "2012 Primary RTC 5 CAR.pdf",
                     package = "phillyvotes")

file1 <- scrape_CAR(file1)
file2 <- scrape_CAR(file2)
file3 <- scrape_CAR(file3)
file4 <- scrape_CAR(file4)
file5 <- scrape_CAR(file5)

test_that("right file name", {
  expect_equal(unique(file1$file), "2012 Primary RTC 3 CAR")
  expect_equal(unique(file2$file), "2012 Primary RTC 1 CAR")
  expect_equal(unique(file3$file), "2012 Primary RTC 2 CAR")
  expect_equal(unique(file4$file), "2012 Primary RTC 4 CAR")
  expect_equal(unique(file5$file), "2012 Primary RTC 5 CAR")
})

test_that("year values are right", {
  expect_equal(max(file1$pdf_page), 1649)
  expect_equal(max(file2$pdf_page), 1943)
  expect_equal(max(file3$pdf_page), 2649)
  expect_equal(max(file4$pdf_page), 1264)
  expect_equal(max(file5$pdf_page), 1109)
})

test_that("precinct values are right", {
  expect_equal(head(file1$precinct), c("56-01", "56-01", "56-01",
                                       "56-01", "56-01", "56-01"))
  expect_equal(tail(file1$precinct), c("66-46", "66-46", "66-46",
                                       "66-46", "66-46", "66-46"))


  expect_equal(head(file2$precinct), c("01-01", "01-01", "01-01",
                                       "01-01", "01-01", "01-01"))
  expect_equal(tail(file2$precinct), c("51-28", "51-28", "51-28",
                                       "51-28", "51-28", "51-28"))


  expect_equal(head(file3$precinct), c("03-01", "03-01", "03-01",
                                       "03-01", "03-01", "03-01"))
  expect_equal(tail(file3$precinct), c("60-23", "60-23", "60-23",
                                       "60-23", "60-23", "60-23"))


  expect_equal(head(file4$precinct), c("08-01", "08-01", "08-01",
                                       "08-01", "08-01", "08-01"))
  expect_equal(tail(file4$precinct), c("47-14", "47-14", "47-14",
                                       "47-14", "47-14", "47-14"))


  expect_equal(head(file5$precinct), c("23-01", "23-01", "23-01",
                                       "23-01", "23-01", "23-01"))
  expect_equal(tail(file5$precinct), c("64-18", "64-18", "64-18",
                                       "64-18", "64-18", "64-18"))
})

test_that("ward values are right", {
  expect_equal(head(file1$ward), c(56, 56, 56, 56, 56, 56))
  expect_equal(tail(file1$ward), c(66, 66, 66, 66, 66, 66))


  expect_equal(head(file2$ward), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file2$ward), c(51, 51, 51, 51, 51, 51))


  expect_equal(head(file3$ward), c(3, 3, 3, 3, 3, 3))
  expect_equal(tail(file3$ward), c(60, 60, 60, 60, 60, 60))


  expect_equal(head(file4$ward), c(8, 8, 8, 8, 8, 8))
  expect_equal(tail(file4$ward), c(47, 47, 47, 47, 47, 47))


  expect_equal(head(file5$ward), c(23, 23, 23, 23, 23, 23))
  expect_equal(tail(file5$ward), c(64, 64, 64, 64, 64, 64))
})

test_that("division values are right", {
  expect_equal(head(file1$division), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file1$division), c(46, 46, 46, 46, 46, 46))


  expect_equal(head(file2$division), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file2$division), c(28, 28, 28, 28, 28, 28))


  expect_equal(head(file3$division), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file3$division), c(23, 23, 23, 23, 23, 23))


  expect_equal(head(file4$division), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file4$division), c(14, 14, 14, 14, 14, 14))


  expect_equal(head(file5$division), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file5$division), c(18, 18, 18, 18, 18, 18))
})

test_that("serial number values are right", {
  expect_equal(head(file1$serial), c("022721", "022721", "022721",
                                     "022721", "022721", "022721"))
  expect_equal(tail(file1$serial), c("023374", "023374", "023374",
                                     "023374", "023374", "023374"))


  expect_equal(head(file2$serial), c("020001", "020001", "020001",
                                     "020001", "020001", "020001"))
  expect_equal(tail(file2$serial), c("022516", "022516", "022516",
                                     "022516", "022516", "022516"))


  expect_equal(head(file3$serial), c("020097", "020097", "020097",
                                     "020097", "020097", "020097"))
  expect_equal(tail(file3$serial), c("023042", "023042", "023042",
                                     "023042", "023042", "023042"))


  expect_equal(head(file4$serial), c("020323", "020323", "020323",
                                     "020323", "020323", "020323"))
  expect_equal(tail(file4$serial), c("022309", "022309", "022309",
                                     "022310", "022310", "022310"))


  expect_equal(head(file5$serial), c("021009", "021009", "021009",
                                     "021009", "021009", "021009"))
  expect_equal(tail(file5$serial), c("023236", "023236", "023236",
                                     "023236", "023236", "023236"))
})

test_that("hour values are right", {
  expect_equal(head(file1$hour), c(7, 8, 8, 8, 9, 9))
  expect_equal(tail(file1$hour), c(19, 19, 19, 19, 19, 19))


  expect_equal(head(file2$hour), c(7, 7, 7, 8, 9, 9))
  expect_equal(tail(file2$hour), c(18, 18, 19, 19, 19, 19))


  expect_equal(head(file3$hour), c(7, 7, 7, 9, 9, 10))
  expect_equal(tail(file3$hour), c(19, 19, 19, 19, 20, 20))


  expect_equal(head(file4$hour), c(7, 7, 8, 8, 8, 8))
  expect_equal(tail(file4$hour), c(19, 19, 19, 13, 16, 19))


  expect_equal(head(file5$hour), c(7, 8, 9, 13, 13, 14))
  expect_equal(tail(file5$hour), c(18, 18, 19, 19, 19, 20))
})

# test_that("year values are right", {
#   expect_equal(unique(file1$year), )
#   expect_equal(unique(file2$year), )
#   expect_equal(unique(file3$year), )
#   expect_equal(unique(file4$year), )
#   expect_equal(unique(file5$year), )
# })

test_that("all submit actions are voter cast ballot", {
  expect_equal(unique(file1$submit_action), "voter cast ballot")
  expect_equal(unique(file2$submit_action), "voter cast ballot")
  expect_equal(unique(file3$submit_action), "voter cast ballot")
  expect_equal(unique(file4$submit_action), "voter cast ballot")
  expect_equal(unique(file5$submit_action), "voter cast ballot")
})

test_that("all start actions are voter enabled", {
  expect_equal(unique(file1$start_action), "voter enabled")
  expect_equal(unique(file2$start_action), "voter enabled")
  expect_equal(unique(file3$start_action), "voter enabled")
  expect_equal(unique(file4$start_action), "voter enabled")
  expect_equal(unique(file5$start_action), "voter enabled")
})

test_that("no NA", {
  expect_equal(sum(is.na((file1$division))), 0)
  expect_equal(sum(is.na((file2$division))), 0)
  expect_equal(sum(is.na((file3$division))), 0)
  expect_equal(sum(is.na((file4$division))), 0)
  expect_equal(sum(is.na((file5$division))), 0)

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

  expect_equal(sum(is.na((file1$pdf_page))), 0)
  expect_equal(sum(is.na((file2$pdf_page))), 0)
  expect_equal(sum(is.na((file3$pdf_page))), 0)
  expect_equal(sum(is.na((file4$pdf_page))), 0)
  expect_equal(sum(is.na((file5$pdf_page))), 0)

  expect_equal(sum(is.na((file1$start_action))), 0)
  expect_equal(sum(is.na((file2$start_action))), 0)
  expect_equal(sum(is.na((file3$start_action))), 0)
  expect_equal(sum(is.na((file4$start_action))), 0)
  expect_equal(sum(is.na((file5$start_action))), 0)

  expect_equal(sum(is.na((file1$submit_action))), 0)
  expect_equal(sum(is.na((file2$submit_action))), 0)
  expect_equal(sum(is.na((file3$submit_action))), 0)
  expect_equal(sum(is.na((file4$submit_action))), 0)
  expect_equal(sum(is.na((file5$submit_action))), 0)

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
