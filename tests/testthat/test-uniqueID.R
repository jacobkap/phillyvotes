context("unique IDs are correct - order and total count")

primary_2017_black1 <- system.file("data/PDF_data/2017 Primary", "2017 Primary - Black BIR 1.pdf",
                                   package = "phillyvotes")
votes <- philly_votes(primary_2017_black1)


test_that("unique IDs in right order", {
  expect_equal(unique(votes$uniqueID[1:24]), "022721 1")
  expect_equal(unique(tail(votes$uniqueID)), "022917 3")
})

test_that("proper number of unique ID for a location and serial number match", {

  expect_equal(length(unique(votes$uniqueID[votes$serial_number == "022721"])), 41)
  expect_equal(length(unique(votes$uniqueID[votes$serial_number == "022840"])), 37)
  expect_equal(length(unique(votes$uniqueID[votes$serial_number == "022839"])), 44)
  expect_equal(length(unique(votes$uniqueID[votes$serial_number == "022895"])), 66)
  expect_equal(length(unique(votes$uniqueID[votes$serial_number == "022814"])), 69)
})

test_that("unique IDs have proper number of rows", {
  expect_equal(nrow(votes[votes$uniqueID == "022721 1", ]), 24)
  expect_equal(nrow(votes[votes$uniqueID == "022889 60", ]), 13)
  expect_equal(nrow(votes[votes$uniqueID == "022757 30", ]), 23)
  expect_equal(nrow(votes[votes$uniqueID == "022814 55", ]), 3)
  expect_equal(nrow(votes[votes$uniqueID == "022814 56", ]), 2)
  expect_equal(nrow(votes[votes$uniqueID == "022814 57", ]), 1)
  expect_equal(nrow(votes[votes$uniqueID == "022917 3", ]), 7)
})
