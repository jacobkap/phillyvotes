context("election-2017-primary")

primary_2017_black1 <- system.file("data/PDF_data/2017 Primary", "2017 Primary - Black BIR 1.pdf",
                                   package = "phillyvotes")
votes <- philly_votes(primary_2017_black1)
primary_2017_yellow <- system.file("data/PDF_data/2017 Primary",
                                   "2017 Primary - YELLOW BIR.pdf",
                                   package = "phillyvotes")
votes2 <- philly_votes(primary_2017_yellow)
primary_2017_pink1 <- system.file("data/PDF_data/2017 Primary",
                                  "2017 Primary - PINK BIR 1.pdf",
                                  package = "phillyvotes")
votes3 <- philly_votes(primary_2017_pink1)
primary_2017orange1 <- system.file("data/PDF_data/2017 Primary",
                                   "2017 Primary - ORANGE BIR 1.pdf",
                                   package = "phillyvotes")
votes4 <- philly_votes(primary_2017orange1)

test_that("right file name", {
  expect_equal(unique(votes$file), "2017 Primary - Black BIR 1")
  expect_equal(unique(votes2$file), "2017 Primary - YELLOW BIR")
  expect_equal(unique(votes3$file), "2017 Primary - PINK BIR 1")
  expect_equal(unique(votes4$file), "2017 Primary - ORANGE BIR 1")
})


test_that("right number of pages", {
  expect_equal(max(votes$pdf_page), 3000)
  expect_equal(max(votes2$pdf_page), 2947)
  expect_equal(max(votes3$pdf_page), 3000)
  expect_equal(max(votes4$pdf_page), 3000)
})

test_that("unique voters have correct number of votes recorded", {
  expect_equal(nrow(votes[votes$uniqueID == "022721 1",]), 24)
  expect_equal(nrow(votes[votes$uniqueID == "022721 8",]), 6)
  expect_equal(nrow(votes[votes$uniqueID == "022825 4",]), 24)
  expect_equal(nrow(votes[votes$uniqueID == "022917 3",]), 7)
  expect_equal(nrow(votes2[votes2$uniqueID == "021457 7",]), 19)
  expect_equal(nrow(votes2[votes2$uniqueID == "021448 14",]), 19)
  expect_equal(nrow(votes2[votes2$uniqueID == "022058 26",]), 1)
  expect_equal(nrow(votes2[votes2$uniqueID == "022058 27",]), 13)
  expect_equal(nrow(votes3[votes3$uniqueID == "021009 1",]), 15)
  expect_equal(nrow(votes3[votes3$uniqueID == "022669 9",]), 14)
  expect_equal(nrow(votes3[votes3$uniqueID == "022669 10",]), 5)
  expect_equal(nrow(votes4[votes4$uniqueID == "020405 10",]), 16)
  expect_equal(nrow(votes4[votes4$uniqueID == "020405 11",]), 20)
  expect_equal(nrow(votes4[votes4$uniqueID == "020443 87",]), 1)
  expect_equal(nrow(votes4[votes4$uniqueID == "020443 86",]), 19)

})



test_that("all votes are equal to 1", {
  #  expect_equal(unique(votes$votes), 1)
})

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


test_that("categories are in the right order", {
  expect_equal(votes$category[1], "Justice of the Supreme Court - Democrat")
  expect_equal(votes$category[2], "Judge of the Superior Court - Democrat")
  expect_equal(votes$category[3], "Judge of the Superior Court - Democrat")
  expect_equal(votes$category[4], "Judge of the Superior Court - Democrat")
  expect_equal(votes$category[5], "Judge of the Commonwealth Court - Democrat")
  expect_equal(votes$category[10], "Judge of the Municipal Court - Democrat")
  expect_equal(votes$category[11], "District Attorney - Democrat")
  expect_equal(votes$category[12], "City Controller - Democrat")
  expect_equal(votes$category[13], "Judge of Election - Democrat-56-01")
  expect_equal(votes$category[14], "Inspector of Election - Democrat-56-01")
  expect_equal(votes$category[15], "Judge of the Superior Court - Democrat")


  # last 6 votes
  expect_equal(tail(votes$category),
               c("Judge of the Court of Common Pleas - Democrat",
                 "Judge of the Municipal Court - Democrat",
                 "District Attorney - Democrat",
                 "City Controller - Democrat",
                 "Judge of the Superior Court - Democrat",
                 "Judge of the Commonwealth Court - Democrat"))
})

test_that("Categories have correct name", {
  # Voting category doesn't include candidate name
  expect_equal(votes2$category[votes2$serial_number == "022105" &
                                 votes2$location == "43-04" &
                                 votes2$voter_record == 1 &
                                 votes2$ballot_position == "33C"],
               "Judge of Election - Democrat-43-04")

  expect_equal(votes2$category[votes2$serial_number == "021454" &
                                 votes2$location == "33-07" &
                                 votes2$voter_record == 22 &
                                 votes2$ballot_position == "36F"],
               "Inspector of Election - Republican-33-07")
})

test_that("Write In ballots have correct category", {
  expect_equal(votes$category[votes$serial_number == "022880" &
                                votes$location == "58-11" &
                                votes$voter_record == 10  &
                                votes$ballot_position == "22G" &
                                votes$candidate == "Write In"],
               "Judge of the Court of Common Pleas - Republican")
  expect_equal(votes$category[votes$serial_number == "022883" &
                                votes$location == "58-13" &
                                votes$voter_record == 28 &
                                votes$ballot_position == "35G" &
                                votes$candidate == "Write In"],
               "Judge of Election - Republican-58-13")
  expect_equal(votes$category[votes$serial_number == "022889" &
                                votes$location == "58-16" &
                                votes$voter_record == 5 &
                                votes$ballot_position == "35E" &
                                votes$candidate == "Write In"],
               "Judge of Election - Democrat-58-16")
  expect_equal(votes$category[votes$serial_number == "022889" &
                                votes$location == "58-16" &
                                votes$voter_record == 5 &
                                votes$ballot_position == "38E" &
                                votes$candidate == "Write In"],
               "Inspector of Election - Democrat-58-16")
  expect_equal(votes2$category[votes2$serial_number == "020303" &
                                 votes2$location == "07-14" &
                                 votes2$voter_record == 12 &
                                 votes2$ballot_position == "29E" &
                                 votes2$candidate == "Write In"],
               "District Attorney - Democrat")
  expect_equal(votes2$category[votes2$serial_number == "020320" &
                                 votes2$location == "07-22" &
                                 votes2$voter_record == 6 &
                                 votes2$ballot_position == "35E" &
                                 votes2$candidate == "Write In"],
               "Judge of Election - Democrat-07-22")

})


test_that("Ballots with No Vote all have category NA", {
  expect_true(is.na(unique(votes$category[votes$candidate == "No Vote"])))
})

test_that("All rows have location values", {
  expect_false(any(is.na(votes$location)))
  expect_false(any(is.na(votes$ward)))
  expect_false(any(is.na(votes$division)))

  expect_false(any(is.na(votes2$location)))
  expect_false(any(is.na(votes2$ward)))
  expect_false(any(is.na(votes2$division)))

  expect_false(any(is.na(votes3$location)))
  expect_false(any(is.na(votes3$ward)))
  expect_false(any(is.na(votes3$division)))

  expect_false(any(is.na(votes4$location)))
  expect_false(any(is.na(votes4$ward)))
  expect_false(any(is.na(votes4$division)))
})

test_that("unique IDs in right order", {
  expect_equal(unique(votes$uniqueID[1:24]), "022721 1")
  expect_equal(unique(tail(votes$uniqueID)), "022917 3")
})

test_that("proper number of unique ID for a location and serial number match", {

  expect_equal(length(unique(votes$uniqueID[votes$serial_number == "022721"])), 41)
  expect_equal(length(unique(votes$uniqueID[votes$serial_number == "022840"])), 37)
  expect_equal(length(unique(votes$uniqueID[votes$serial_number == "022839"])), 44)
  expect_equal(length(unique(votes$uniqueID[votes$serial_number == "022895"])), 66)
  expect_equal(length(unique(votes$uniqueID[votes$serial_number == "022814"])), 69)
})

test_that("unique IDs have proper number of rows", {
  expect_equal(nrow(votes[votes$uniqueID == "022721 1", ]), 24)
  expect_equal(nrow(votes[votes$uniqueID == "022889 60", ]), 13)
  expect_equal(nrow(votes[votes$uniqueID == "022757 30", ]), 23)
  expect_equal(nrow(votes[votes$uniqueID == "022814 55", ]), 3)
  expect_equal(nrow(votes[votes$uniqueID == "022814 56", ]), 2)
  expect_equal(nrow(votes[votes$uniqueID == "022814 57", ]), 1)
  expect_equal(nrow(votes[votes$uniqueID == "022917 3", ]), 7)
})

