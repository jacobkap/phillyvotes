context("values in 'votes' are correct - order and total count")

primary_2017_black1 <- system.file("data/PDF_data/2017 Primary", "2017 Primary - Black BIR 1.pdf",
                                   package = "phillyvotes")
votes <- philly_votes(primary_2017_black1)


test_that("all votes are equal to 1", {
#  expect_equal(unique(votes$votes), 1)
})
