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
  expect_equal(votes$category[1], "Justice of the Supreme Court-Dem")
  expect_equal(votes$category[2], "Judge of the Superior Court-Dem")
  expect_equal(votes$category[3], "Judge of the Superior Court-Dem")
  expect_equal(votes$category[4], "Judge of the Superior Court-Dem")
  expect_equal(votes$category[5], "Judge of the Commonwealth Court-Dem")
  expect_equal(votes$category[10], "Judge of the Municipal Court-Dem")
  expect_equal(votes$category[11], "District Attorney-Dem")
  expect_equal(votes$category[12], "City Controller-Dem")
  expect_equal(votes$category[13], "Judge of Election-Dem-56-01")
  expect_equal(votes$category[14], "Inspector of Election-Dem-56-01")
  expect_equal(votes$category[15], "Judge of the Superior Court-Dem")


  # last 6 votes
  expect_equal(tail(votes$category),
               c("Judge of the Court of Common Pleas-Dem",
                 "Judge of the Municipal Court-Dem",
                 "District Attorney-Dem",
                 "City Controller-Dem",
                 "Judge of the Superior Court-Dem",
                 "Judge of the Commonwealth Court-Dem"))
})

test_that("Categories have correct name", {
  # Voting category doesn't include candidate name
  expect_equal(votes2$category[votes2$serial_number == "022105" &
                                 votes2$location == "43-04" &
                                 votes2$voter_record == 1 &
                                 votes2$ballot_position == "33C"],
               "Judge of Election-Dem-43-04")

  expect_equal(votes2$category[votes2$serial_number == "021454" &
                                 votes2$location == "33-07" &
                                 votes2$voter_record == 22 &
                                 votes2$ballot_position == "36F"],
               "Inspector of Election-Rep-33-07")
})

test_that("Write In ballots have correct category", {
  expect_equal(votes$category[votes$serial_number == "022880" &
                                votes$location == "58-11" &
                                votes$voter_record == 10  &
                                votes$ballot_position == "22G" &
                                votes$candidate == "Write In"],
               "Judge of the Court of Common Pleas-Rep")
  expect_equal(votes$category[votes$serial_number == "022883" &
                                votes$location == "58-13" &
                                votes$voter_record == 28 &
                                votes$ballot_position == "35G" &
                                votes$candidate == "Write In"],
               "Judge of Election-Rep-58-13")
  expect_equal(votes$category[votes$serial_number == "022889" &
                                votes$location == "58-16" &
                                votes$voter_record == 5 &
                                votes$ballot_position == "35E" &
                                votes$candidate == "Write In"],
               "Judge of Election-Dem-58-16")
  expect_equal(votes$category[votes$serial_number == "022889" &
                                votes$location == "58-16" &
                                votes$voter_record == 5 &
                                votes$ballot_position == "38E" &
                                votes$candidate == "Write In"],
               "Inspector of Election-Dem-58-16")
  expect_equal(votes2$category[votes2$serial_number == "020303" &
                                 votes2$location == "07-14" &
                                 votes2$voter_record == 12 &
                                 votes2$ballot_position == "29E" &
                                 votes2$candidate == "Write In"],
               "District Attorney-Dem")
  expect_equal(votes2$category[votes2$serial_number == "020320" &
                                 votes2$location == "07-22" &
                                 votes2$voter_record == 6 &
                                 votes2$ballot_position == "35E" &
                                 votes2$candidate == "Write In"],
               "Judge of Election-Dem-07-22")

})


test_that("Ballots with No Vote all have category NA", {
  expect_true(is.na(unique(votes$category[votes$candidate == "No Vote"])))
})

