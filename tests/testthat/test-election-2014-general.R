context("election-2014-general")

file1 <- system.file("data/PDF_data/2014 General",
                     "BLACK 1 BIR.pdf",
                     package = "phillyvotes")
file2 <- system.file("data/PDF_data/2014 General",
                     "BLACK 2 BIR.pdf",
                     package = "phillyvotes")
file3 <- system.file("data/PDF_data/2014 General",
                     "BLUE 4 BIR.pdf",
                     package = "phillyvotes")
file4 <- system.file("data/PDF_data/2014 General",
                     "GREEN 2 BIR.pdf",
                     package = "phillyvotes")
file5 <- system.file("data/PDF_data/2014 General",
                     "READ TO WRONG COLOR BIR.pdf",
                     package = "phillyvotes")

file1 <- philly_votes(file1)
file2 <- philly_votes(file2)
file3 <- philly_votes(file3)
file4 <- philly_votes(file4)
file5 <- philly_votes(file5)

test_that("right file name", {
  expect_equal(unique(file1$file), "BLACK 1 BIR")
  expect_equal(unique(file2$file), "BLACK 2 BIR")
  expect_equal(unique(file3$file), "BLUE 4 BIR")
  expect_equal(unique(file4$file), "GREEN 2 BIR")
  expect_equal(unique(file5$file), "READ TO WRONG COLOR BIR")
})


test_that("right number of pages", {
  expect_equal(max(file1$pdf_page), 3000)
  expect_equal(max(file2$pdf_page), 6000)
  expect_equal(max(file3$pdf_page), 13330)
  expect_equal(max(file4$pdf_page), 4780)
  expect_equal(max(file5$pdf_page), 85)
})




test_that("ballot ballot_position values are correct", {
  # First 6 votes
  expect_equal(head(file1$ballot_position), c("6C", "9C", "15D", "10L", "20L", "29L"))
  # Last 6 votes
  expect_equal(tail(file1$ballot_position), c("10L", "20L", "30L", "6D", "9D", "15D"))

  # First 6 votes
  expect_equal(head(file2$ballot_position), c("9L", "19L", "29L", "6C", "9C", "15D"))
  # Last 6 votes
  expect_equal(tail(file2$ballot_position), c("6D", "15D", "10L", "20L", "29L", "6C"))

  # First 6 votes
  expect_equal(head(file3$ballot_position), c("6D", "9D", "12D", "15D", "9L", "20L"))
  # Last 6 votes
  expect_equal(tail(file3$ballot_position), c("9D", "12D", "15D", "9L", "19L", "29L"))

  # First 6 votes
  expect_equal(head(file4$ballot_position), c("10L", "20L", "30L", "6D", "9D", "15D"))
  # Last 6 votes
  expect_equal(tail(file4$ballot_position), c("6D", "9D", "15D", "9L", "19L", "29L"))

  # First 6 votes
  expect_equal(head(file5$ballot_position), c("6D", "9D", "15D", "6D", "9D", "15D"))
  # Last 6 votes
  expect_equal(tail(file5$ballot_position), c("12D", "15D", "6D", "9D", "12D", "15D"))

})

test_that("unique IDs in right order", {
  expect_equal(head(file1$uniqueID), c("022721 1", "022721 1", "022721 1",
                                       "022721 1", "022721 1", "022721 1"))
  expect_equal(tail(file1$uniqueID), c("022847 63", "022847 63", "022847 63",
                                       "022847 64", "022847 64", "022847 64"))

  expect_equal(head(file2$uniqueID), c("022847 64", "022847 64", "022847 64",
                                       "022847 65", "022847 65", "022847 65"))
  expect_equal(tail(file2$uniqueID), c("023160 42", "023160 42", "023160 42",
                                       "023160 42", "023160 42", "023160 43"))

  expect_equal(head(file3$uniqueID), c("021912 2", "021912 2", "021912 2",
                                       "021912 2", "021912 2", "021912 2"))
  expect_equal(tail(file3$uniqueID), c("022516 175", "022516 175", "022516 175",
                                       "022516 175", "022516 175", "022516 175"))

  expect_equal(head(file4$uniqueID), c("020793 5", "020793 5", "020793 5",
                                       "020793 6", "020793 6", "020793 6"))
  expect_equal(tail(file4$uniqueID), c("020239 52", "020239 52", "020239 52",
                                       "020239 52", "020239 52", "020239 52"))

  expect_equal(head(file5$uniqueID), c("021329 1", "021329 1", "021329 1",
                                       "021329 2", "021329 2", "021329 2"))
  expect_equal(tail(file5$uniqueID), c("022482 153", "022482 153", "022482 154",
                                       "022482 154", "022482 154", "022482 154"))
})




test_that("proper number of unique ID for a location and serial number match", {

  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022847"])), 64)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022819"])), 62)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022807"])), 130)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022721"])), 109)

  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "023160"])), 43)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "022899"])), 111)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "022869"])), 159)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "022848"])), 93)

  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "022516"])), 175)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "022466"])), 95)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "022312"])), 47)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "021970"])), 196)

  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020239"])), 52)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "021376"])), 72)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "021356"])), 86)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020820"])), 105)

  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "022482"])), 154)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "021329"])), 117)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "021656"])), 108)
})

test_that("unique voters have correct number of files recorded", {
  expect_equal(nrow(file1[file1$uniqueID == "022721 5",]), 4)
  expect_equal(nrow(file1[file1$uniqueID == "022721 6",]), 3)
  expect_equal(nrow(file1[file1$uniqueID == "022721 7",]), 6)
  expect_equal(nrow(file1[file1$uniqueID == "022721 1",]), 6)
  expect_equal(nrow(file2[file2$uniqueID == "022848 12",]), 6)
  expect_equal(nrow(file2[file2$uniqueID == "022848 15",]), 3)
  expect_equal(nrow(file2[file2$uniqueID == "023160 42",]), 5)
  expect_equal(nrow(file2[file2$uniqueID == "023160 43",]), 1)
  expect_equal(nrow(file3[file3$uniqueID == "021970 174",]), 6)
  expect_equal(nrow(file3[file3$uniqueID == "022317 15",]), 4)
  expect_equal(nrow(file3[file3$uniqueID == "022317 16",]), 4)
  expect_equal(nrow(file3[file3$uniqueID == "022516 175",]), 7)
  expect_equal(nrow(file4[file4$uniqueID == "020820 43",]), 4)
  expect_equal(nrow(file4[file4$uniqueID == "021353 60",]), 3)
  expect_equal(nrow(file4[file4$uniqueID == "021353 61",]), 2)
  expect_equal(nrow(file4[file4$uniqueID == "020239 52",]), 6)
  expect_equal(nrow(file5[file5$uniqueID == "022482 153",]), 4)
  expect_equal(nrow(file5[file5$uniqueID == "022482 154",]), 4)
  expect_equal(nrow(file5[file5$uniqueID == "021656 29",]), 7)
  expect_equal(nrow(file5[file5$uniqueID == "021329 1",]), 3)

})


test_that("voter number in right order", {
  expect_equal(head(file1$voter_record), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file1$voter_record), c(63, 63, 63, 64, 64, 64))

  expect_equal(head(file2$voter_record), c(64, 64, 64, 65, 65, 65))
  expect_equal(tail(file2$voter_record), c(42, 42, 42, 42, 42, 43))

  expect_equal(head(file3$voter_record), c(2, 2, 2, 2, 2, 2))
  expect_equal(tail(file3$voter_record), c(175, 175, 175, 175, 175, 175))

  expect_equal(head(file4$voter_record), c(5, 5, 5, 6, 6, 6))
  expect_equal(tail(file4$voter_record), c(52, 52, 52, 52, 52, 52))

  expect_equal(head(file5$voter_record), c(1, 1, 1, 2, 2, 2))
  expect_equal(tail(file5$voter_record), c(153, 153, 154, 154, 154, 154))
})


test_that("Candidate has right name", {

  expect_equal(file1$candidate[file1$uniqueID == "022847 62"],
               c("No No",
                 "Yes Si",
                 "Yes Si"))
  expect_equal(file1$candidate[file1$uniqueID == "022847 63"],
               c("John Sabatina Jr",
                 "No No",
                 "No No",
                 "No No"))
  expect_equal(file1$candidate[file1$uniqueID == "022721 6"],
               c("Tom Wolf",
                 "Brendan F Boyle",
                 "Kevin J Boyle"))
  expect_equal(file1$candidate[file1$uniqueID == "022721 22"],
               c("Tom Corbett",
                 "Carson Dee Adcock"))
  expect_equal(file2$candidate[file2$uniqueID == "023160 43"],
               c("Tom Corbett"))
  expect_equal(file2$candidate[file2$uniqueID == "023160 40"],
               c("Carson Dee Adcock",
                 "John Jenkins",
                 "No No"))
  expect_equal(file2$candidate[file2$uniqueID == "022898 69"],
               c("Tom Corbett",
                 "Brendan F Boyle",
                 "Brendan F Boyle"))
  expect_equal(file2$candidate[file2$uniqueID == "022898 56"],
               c("Tom Corbett", "Carson Dee Adcock"))
  expect_equal(file3$candidate[file3$uniqueID == "022516 152"],
               c("Tom Wolf",
                 "Chaka Fattah",
                 "Anthony Hardy Williams",
                 "James R Roebuck"))
  expect_equal(file3$candidate[file3$uniqueID == "022516 153"],
               c("Tom Wolf"))
  expect_equal(file3$candidate[file3$uniqueID == "022325 5"],
               c("Tom Wolf",
                 "Anthony Hardy Williams",
                 "Jordan A Harris"))
  expect_equal(file3$candidate[file3$uniqueID == "021966 16"],
               c("Tom Wolf",
                 "Robert A Brady"))
  expect_equal(file4$candidate[file4$uniqueID == "020239 41"],
               c("Tom Wolf",
                 "Robert A Brady",
                 "Brian Sims"))
  expect_equal(file4$candidate[file4$uniqueID == "020239 42"],
               c("Tom Wolf",
                 "Brian Sims"))
  expect_equal(file4$candidate[file4$uniqueID == "021345 59"],
               c("Tom Corbett",
                 "Megan Rath",
                 "John Jenkins"))
  expect_equal(file4$candidate[file4$uniqueID == "021345 54"],
               c("Tom Wolf",
                 "No No"))
  expect_equal(file4$candidate[file4$uniqueID == "021345 27"],
               c("Tom Wolf"))
  expect_equal(file5$candidate[file5$uniqueID == "021329 1"],
               c("Tom Wolf",
                 "Chaka Fattah",
                 "Jordan A Harris"))
  expect_equal(file5$candidate[file5$uniqueID == "021329 18"],
               c("Tom Wolf",
                 "Chaka Fattah",
                 "Jordan A Harris"))
  expect_equal(file5$candidate[file5$uniqueID == "021329 25"],
               c("Tom Wolf",
                 "Chaka Fattah",
                 "Jordan A Harris"))
  expect_equal(file5$candidate[file5$uniqueID == "021656 88"],
               c("Tom Wolf",
                 "Chaka Fattah",
                 "Anthony Hardy Williams",
                 "Maria P Donatucci"))
})


test_that("Categories are correct", {

  expect_equal(file1$category[file1$uniqueID == "022847 62"],
               c("Proposed Charter Change Question #1",
                 "Proposed Charter Change Question #2",
                 "City Bond Question"))
  expect_equal(file1$category[file1$uniqueID == "022847 63"],
               c("Representative in the General Assembly - 174th District",
                 "Proposed Charter Change Question #1",
                 "Proposed Charter Change Question #2",
                 "City Bond Question"))
  expect_equal(file1$category[file1$uniqueID == "022721 6"],
               c("Governor and Lieutenant Governor",
                 "Representative in Congress - 13th District",
                 "Representative in the General Assembly - 172nd District"))
  expect_equal(file1$category[file1$uniqueID == "022721 22"],
               c("Governor and Lieutenant Governor",
                 "Representative in Congress - 13th District"))
  expect_equal(file2$category[file2$uniqueID == "023160 43"],
               c("Governor and Lieutenant Governor"))
  expect_equal(file2$category[file2$uniqueID == "023160 40"],
               c("Representative in Congress - 13th District",
                 "Senator in the General Assembly - 2nd District",
                 "City Bond Question"))
  expect_equal(file2$category[file2$uniqueID == "022898 69"],
               c("Governor and Lieutenant Governor",
                 "Representative in Congress - 13th District",
                 "Representative in the General Assembly - 170th District"))
  expect_equal(file2$category[file2$uniqueID == "022898 56"],
               c("Governor and Lieutenant Governor",
                 "Representative in Congress - 13th District"))
  expect_equal(file3$category[file3$uniqueID == "022516 152"],
               c("Governor and Lieutenant Governor",
                 "Representative in Congress - 2nd District",
                 "Senator in the General Assembly - 8th District",
                 "Representative in the General Assembly - 188th District"))
  expect_equal(file3$category[file3$uniqueID == "022516 153"],
               c("Governor and Lieutenant Governor"))
  expect_equal(file3$category[file3$uniqueID == "022325 5"],
               c("Governor and Lieutenant Governor",
                 "Senator in the General Assembly - 8th District",
                 "Representative in the General Assembly - 186th District"))
  expect_equal(file3$category[file3$uniqueID == "021966 16"],
               c("Governor and Lieutenant Governor",
                 "Representative in Congress - 1st District"))
  expect_equal(file4$category[file4$uniqueID == "020239 41"],
               c("Governor and Lieutenant Governor",
                 "Representative in Congress - 1st District",
                 "Representative in the General Assembly - 182nd District"))
  expect_equal(file4$category[file4$uniqueID == "020239 42"],
               c("Governor and Lieutenant Governor",
                 "Representative in the General Assembly - 182nd District"))
  expect_equal(file4$category[file4$uniqueID == "021345 54"],
               c("Governor and Lieutenant Governor", "City Bond Question"))
  expect_equal(file5$category[file5$uniqueID == "021329 1"],
               c("Governor and Lieutenant Governor",
                 "Representative in Congress - 2nd District",
                 "Representative in the General Assembly - 186th District"))
  expect_equal(file5$category[file5$uniqueID == "021329 18"],
               c("Governor and Lieutenant Governor",
                 "Representative in Congress - 2nd District",
                 "Representative in the General Assembly - 186th District"))
  expect_equal(file5$category[file5$uniqueID == "021329 25"],
               c("Governor and Lieutenant Governor",
                 "Representative in Congress - 2nd District",
                 "Representative in the General Assembly - 186th District"))
  expect_equal(file5$category[file5$uniqueID == "021656 88"],
               c("Governor and Lieutenant Governor",
                 "Representative in Congress - 2nd District",
                 "Senator in the General Assembly - 8th District",
                 "Representative in the General Assembly - 185th District"))

})


# No change needed here ---------------------------------------------------

test_that("No commas in candidate names", {
  expect_false(all(grepl(",", file1$candidate)))
  expect_false(all(grepl(",", file2$candidate)))
  expect_false(all(grepl(",", file3$candidate)))
  expect_false(all(grepl(",", file4$candidate)))
  expect_false(all(grepl(",", file5$candidate)))
})


# test_that("all votes are equal to 1", {
#   expect_true(unique(file1$votes) %in% c(1, NA))
#   expect_true(unique(file2$votes) %in% c(1, NA))
#   expect_true(unique(file3$votes) %in% c(1, NA))
#   expect_true(unique(file4$votes) %in% c(1, NA))
#   expect_true(unique(file5$votes) %in% c(1, NA))
# })


test_that("Ballots with No Vote all have position 43A", {
  if (length(unique(file1$ballot_position[file1$candidate == "No Vote"])) > 0) {
    expect_equal(unique(file1$ballot_position[file1$candidate == "No Vote"]),
                 "43A")
  }
  if (length(unique(file2$ballot_position[file2$candidate == "No Vote"])) > 0) {
    expect_equal(unique(file2$ballot_position[file2$candidate == "No Vote"]),
                 "43A")
  }
  if (length(unique(file3$ballot_position[file3$candidate == "No Vote"])) > 0) {
    expect_equal(unique(file3$ballot_position[file3$candidate == "No Vote"]),
                 "43A")
  }
  if (length(unique(file4$ballot_position[file4$candidate == "No Vote"])) > 0) {
    expect_equal(unique(file4$ballot_position[file4$candidate == "No Vote"]),
                 "43A")
  }
  if (length(unique(file5$ballot_position[file5$candidate == "No Vote"])) > 0) {
    expect_equal(unique(file5$ballot_position[file5$candidate == "No Vote"]),
                 "43A")
  }
})


test_that("Ballots with No Vote all have category NA", {
  if (length(unique(file1$category[file1$candidate == "No Vote"])) > 0) {
    expect_true(is.na(unique(file1$category[file1$candidate == "No Vote"])))
  }
  if (length(unique(file2$category[file2$candidate == "No Vote"])) > 0) {
    expect_true(is.na(unique(file2$category[file2$candidate == "No Vote"])))
  }
  if (length(unique(file3$category[file3$candidate == "No Vote"])) > 0) {
    expect_true(is.na(unique(file3$category[file3$candidate == "No Vote"])))
  }
  if (length(unique(file4$category[file4$candidate == "No Vote"])) > 0) {
    expect_true(is.na(unique(file4$category[file4$candidate == "No Vote"])))
  }
  if (length(unique(file5$category[file5$candidate == "No Vote"])) > 0) {
    expect_true(is.na(unique(file5$category[file5$candidate == "No Vote"])))
  }
})


test_that("All rows have location values", {
  expect_false(any(is.na(file1$location)))
  expect_false(any(is.na(file1$ward)))
  expect_false(any(is.na(file1$division)))

  expect_false(any(is.na(file2$location)))
  expect_false(any(is.na(file2$ward)))
  expect_false(any(is.na(file2$division)))

  expect_false(any(is.na(file3$location)))
  expect_false(any(is.na(file3$ward)))
  expect_false(any(is.na(file3$division)))

  expect_false(any(is.na(file4$location)))
  expect_false(any(is.na(file4$ward)))
  expect_false(any(is.na(file4$division)))

  expect_false(any(is.na(file5$location)))
  expect_false(any(is.na(file5$ward)))
  expect_false(any(is.na(file5$division)))
})
