context("values in 'candidate name' are correct - order and total count")

testing_file <- system.file("data",
                            "2017 Primary - Black BIR 1.pdf",
                            package = "phillyvotes")
votes <- philly_votes(testing_file)


testing_file2 <- system.file("data",
                             "2017 Primary - YELLOW BIR.pdf",
                             package = "phillyvotes")
votes2 <- philly_votes(testing_file2)

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


test_that("Candidate has right name", {

  expect_equal(votes2$candidate[votes2$serial_number == "022105" &
                                  votes2$location == "43-04" &
                                  votes2$voter_record == 1 &
                                  votes2$ballot_position == "33C"],
               "RIVERS SR, MICHAEL A")

  expect_equal(votes2$candidate[votes2$serial_number == "021454" &
                                  votes2$location == "33-07" &
                                  votes2$voter_record == 22 &
                                  votes2$ballot_position == "36F"],
               "STREEPER JR, BILL E")

})

test_that("Write In ballots have correct name", {
  expect_equal(votes$candidate[votes$serial_number == "022727" &
                                  votes$location == "56-04" &
                                  votes$voter_record == 36 &
                                  votes$ballot_position == "5G"],
               "Write In")
  expect_equal(votes$candidate[votes$serial_number == "022909" &
                                 votes$location == "58-26" &
                                 votes$voter_record == 13 &
                                 votes$ballot_position == "35E"],
               "Write In")
  expect_equal(votes$candidate[votes$serial_number == "022909" &
                                 votes$location == "58-26" &
                                 votes$voter_record == 13 &
                                 votes$ballot_position == "38E"],
               "Write In")
  expect_equal(votes$candidate[votes$serial_number == "022754" &
                                 votes$location == "56-17" &
                                 votes$voter_record == 9 &
                                 votes$ballot_position == "22G"],
               "Write In")
  expect_equal(votes$candidate[votes$serial_number == "022754" &
                                 votes$location == "56-17" &
                                 votes$voter_record == 9 &
                                 votes$ballot_position == "26G"],
               "Write In")
  expect_equal(votes$candidate[votes$serial_number == "022754" &
                                 votes$location == "56-17" &
                                 votes$voter_record == 9 &
                                 votes$ballot_position == "35G"],
               "Write In")
  expect_equal(votes$candidate[votes$serial_number == "022754" &
                                 votes$location == "56-17" &
                                 votes$voter_record == 9 &
                                 votes$ballot_position == "38G"],
               "Write In")
  expect_equal(votes$candidate[votes$serial_number == "022880" &
                                 votes$location == "58-11" &
                                 votes$voter_record == 10 &
                                 votes$ballot_position == "22G"],
               "Write In")
})
