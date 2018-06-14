context("location, wards, and divisions are correct")

primary_2017_black1 <- system.file("data/PDF_data/2017 Primary", "2017 Primary - Black BIR 1.pdf",
                                   package = "phillyvotes")
votes <- philly_votes(primary_2017_black1)
primary_2017_yellow <- system.file("data",
                                   "2017 Primary - YELLOW BIR.pdf",
                                   package = "phillyvotes")
votes2 <- philly_votes(primary_2017_yellow)
primary_2017_pink1 <- system.file("data",
                            "2017 Primary - PINK BIR 1.pdf",
                            package = "phillyvotes")
votes3 <- philly_votes(primary_2017_pink1)
primary_2017orange1 <- system.file("data",
                             "2017 Primary - ORANGE BIR 1.pdf",
                             package = "phillyvotes")
votes4 <- philly_votes(primary_2017orange1)


test_that("All rows have location values", {
  expect_false(any(is.na(votes$location)))
  expect_false(any(is.na(votes$ward)))
  expect_false(any(is.na(votes$division)))

  expect_false(any(is.na(votes2$location)))
  expect_false(any(is.na(votes2$ward)))
  expect_false(any(is.na(votes2$division)))

  expect_false(any(is.na(votes3$location)))
  expect_false(any(is.na(votes3$ward)))
  expect_false(any(is.na(votes3$division)))

  expect_false(any(is.na(votes4$location)))
  expect_false(any(is.na(votes4$ward)))
  expect_false(any(is.na(votes4$division)))
})

test_that("Values for ward + division and identical to location", {
  expect_equal(votes$location, paste(votes$ward, votes$division, sep = "-"))
  expect_equal(votes2$location, paste(votes2$ward, votes2$division, sep = "-"))
  expect_equal(votes3$location, paste(votes3$ward, votes3$division, sep = "-"))
  expect_equal(votes4$location, paste(votes4$ward, votes4$division, sep = "-"))
})
