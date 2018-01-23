context("values in 'candidate name' are correct - order and total count")

testing_file <- system.file("data", "2017 Primary - Black BIR 1.pdf",
                            package = "phillyvotes")
votes <- philly_votes(testing_file)

test_that("candidate name is in right order", {
  expect_equal(votes$candidate[1], "WOODRUFF, DWAYNE")
  expect_equal(votes$candidate[9], "BOZZELLI, LAWRENCE J")
  expect_equal(votes$candidate[10], "TWARDY, GEORGE")
  expect_equal(votes$candidate[15], "CAYE, BILL")
  expect_equal(votes$candidate[20], "TOPPIN, SHERMAN")
  expect_equal(votes$candidate[23], "NO NO")
  expect_equal(votes$candidate[24], "YES SI")
  expect_equal(votes$candidate[25], "WOODRUFF, DWAYNE")

  # last 6 votes
  expect_equal(tail(votes$candidate), c("COHEN, MARK B",
                                        "WOLF, MATT",
                                        "UNTERMEYER, MICHAEL W",
                                        "RHYNHART, REBECCA",
                                        "KUNSELMAN, DEBBIE",
                                        "CEISLER, ELLEN"))

})
