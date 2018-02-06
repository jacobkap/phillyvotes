context("values in 'ballot ballot_position' are correct - order and total count")

testing_file <- system.file("data", "2017 Primary - Black BIR 1.pdf",
                            package = "phillyvotes")
votes <- philly_votes(testing_file)


test_that("ballot ballot_position values are correct", {
  # FIRST VOTER
  expect_equal(votes$ballot_position[1:24], c("3C", "6C", "7C", "8C", "10C", "12C",
                                       "14C", "16C", "18C", "25C", "27C", "32C",
                                       "33C", "36C", "7D", "9D", "12D", "19D",
                                       "21D", "25D", "14E", "20E", "13L", "26L"))

  # SECOND VOTER
  expect_equal(votes$ballot_position[25:38], c("3C", "6C", "7C", "9D", "10D", "12D",
                                        "14D", "15D", "17D", "18D", "19D",
                                        "20D", "24E", "28E"))

  # LAST 6 votes
  expect_equal(tail(votes$ballot_position), c("22C", "23C", "29C", "31C", "6D", "9D"))
})


test_that("Ballots with No Vote all have position 43A", {
  expect_equal(unique(votes$ballot_position[votes$candidate == "No Vote"]),
               "43A")
})
