context("values in 'category' are correct - order and total count")

testing_file <- system.file("data",
                            "2017 Primary - Black BIR 1.pdf",
                            package = "phillyvotes")
votes <- philly_votes(testing_file)
testing_file2 <- system.file("data",
                             "2017 Primary - YELLOW BIR.pdf",
                            package = "phillyvotes")
votes2 <- philly_votes(testing_file2)


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

test_that("Categories have correct name", {
# Voting category doesn't include candidate name
expect_equal(votes2$category[votes2$sernum == "022105" &
                               votes2$location == "43-04" &
                               votes2$voterecord == 1 &
                               votes2$position == "33C"],
             "JUDGE OF ELECTION-DEM-43-04")

expect_equal(votes2$category[votes2$sernum == "021454" &
                               votes2$location == "33-07" &
                               votes2$voterecord == 22 &
                               votes2$position == "36F"],
             "INSPECTOR OF ELECTION-REP-33-07")
})

