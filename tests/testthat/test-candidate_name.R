context("values in 'candidate name' are correct - order and total count")

primary_2017_black1 <- system.file("data/PDF_data/2017 Primary", "2017 Primary - Black BIR 1.pdf",
                                   package = "phillyvotes")
votes <- philly_votes(primary_2017_black1)


primary_2017_yellow <- system.file("data",
                             "2017 Primary - YELLOW BIR.pdf",
                             package = "phillyvotes")
votes2 <- philly_votes(primary_2017_yellow)

test_that("candidate name is in right order", {
  expect_equal(votes$candidate[1], "Dwayne Woodruff")
  expect_equal(votes$candidate[9], "Lawrence J Bozzelli")
  expect_equal(votes$candidate[10], "George Twardy")
  expect_equal(votes$candidate[15], "Bill Caye")
  expect_equal(votes$candidate[20], "Sherman Toppin")
  expect_equal(votes$candidate[23], "No No")
  expect_equal(votes$candidate[24], "Yes Si")
  expect_equal(votes$candidate[25], "Dwayne Woodruff")

  # last 6 votes
  expect_equal(tail(votes$candidate), c("Mark B Cohen",
                                        "Matt Wolf",
                                        "Michael W Untermeyer",
                                        "Rebecca Rhynhart",
                                        "Debbie Kunselman",
                                        "Ellen Ceisler"))

})


test_that("Candidate has right name", {

  expect_equal(votes2$candidate[votes2$serial_number == "022105" &
                                  votes2$location == "43-04" &
                                  votes2$voter_record == 1 &
                                  votes2$ballot_position == "33C"],
               "Michael A Rivers Sr")

  expect_equal(votes2$candidate[votes2$serial_number == "021454" &
                                  votes2$location == "33-07" &
                                  votes2$voter_record == 22 &
                                  votes2$ballot_position == "36F"],
               "Bill E Streeper Jr")

  expect_false(all(grepl(",", votes$candidate)))
  expect_false(all(grepl(",", votes2$candidate)))

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

test_that("No Vote ballots have correct name", {
  expect_equal(votes2$candidate[votes2$serial_number == "022058" &
                                 votes2$location == "42-05" &
                                 votes2$voter_record == 22 &
                                 votes2$ballot_position == "43A"],
               "No Vote")
  expect_equal(votes2$candidate[votes2$serial_number == "021745" &
                                 votes2$location == "37-14" &
                                 votes2$voter_record == 28 &
                                 votes2$ballot_position == "43A"],
               "No Vote")

  expect_equal(votes$candidate[votes$serial_number == "022751" &
                                 votes$location == "56-16" &
                                 votes$voter_record == 34 &
                                 votes$ballot_position == "43A"],
               "No Vote")

  expect_equal(votes$candidate[votes$serial_number == "022724" &
                                 votes$location == "56-02" &
                                 votes$voter_record == 17 &
                                 votes$ballot_position == "43A"],
               "No Vote")
  expect_equal(votes$candidate[votes$serial_number == "022818" &
                                 votes$location == "57-08" &
                                 votes$voter_record == 51 &
                                 votes$ballot_position == "43A"],
               "No Vote")
})
