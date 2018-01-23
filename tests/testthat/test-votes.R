context("values in 'votes' are correct - order and total count")

testing_file <- system.file("data", "2017 Primary - Black BIR 1.pdf",
                            package = "phillyvotes")
votes <- philly_votes(testing_file)


test_that("all votes are equal to 1", {
  expect_equal(unique(votes$votes), 1)
})
