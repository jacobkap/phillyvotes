context("values in 'category' are correct - order and total count")

votes <- philly_votes()

test_that("categories are in the right order", {
  expect_equal(votes$category[1], "JUSTICE OF THE SUPREME COURT-DEM")
  expect_equal(votes$category[2], "JUDGE OF THE SUPERIOR COURT-DEM")
  expect_equal(votes$category[3], "JUDGE OF THE SUPERIOR COURT-DEM")
  expect_equal(votes$category[4], "JUDGE OF THE SUPERIOR COURT-DEM")
  expect_equal(votes$category[5], "JUDGE OF THE COMMONWEALTH COURT-DEM")
  expect_equal(votes$category[10], "JUDGE OF THE MUNICIPAL COURT-DEM")
  expect_equal(votes$category[11], "DISTRICT ATTORNEY-DEM")
  expect_equal(votes$category[12], "CITY CONTROLLER-DEM")
  expect_equal(votes$category[13], "JUDGE OF ELECTION-DEM-56-01")
  expect_equal(votes$category[14], "INSPECTOR OF ELECTION-DEM-56-01")
  expect_equal(votes$category[15], "JUDGE OF THE SUPERIOR COURT-DEM")


  # last 6 votes
  expect_equal(tail(votes$category), c("JUDGE OF THE COURT OF COMMON PLEAS-DEM",
                                       "JUDGE OF THE MUNICIPAL COURT-DEM",
                                       "DISTRICT ATTORNEY-DEM",
                                       "CITY CONTROLLER-DEM",
                                       "JUDGE OF THE SUPERIOR COURT-DEM",
                                       "JUDGE OF THE COMMONWEALTH COURT-DEM"))
})
