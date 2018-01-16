context("values in 'votes' are correct - order and total count")

votes <- philly_votes()

test_that("all votes are equal to 1", {
  expect_equal(unique(votes$votes), 1)
})
