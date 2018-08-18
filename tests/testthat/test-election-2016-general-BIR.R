context("election-2016-general")

file1 <- system.file("data/PDF_data/2016 General",
                     "2016 General - Ward 1,2,26 BIR 1.pdf",
                     package = "phillyvotes")
file2 <- system.file("data/PDF_data/2016 General",
                     "2016 General - Ward 1,2,26 BIR 2.pdf",
                     package = "phillyvotes")
file3 <- system.file("data/PDF_data/2016 General",
                     "2016 General - Ward 7,25,33,37 BIR 3.pdf",
                     package = "phillyvotes")
file4 <- system.file("data/PDF_data/2016 General",
                     "2016 General - Ward 8,15 BIR 3.pdf",
                     package = "phillyvotes")
file5 <- system.file("data/PDF_data/2016 General",
                     "2016 General - Ward 58 BIR 2.pdf",
                     package = "phillyvotes")

file1 <- philly_votes(file1)
file2 <- philly_votes(file2)
file3 <- philly_votes(file3)
file4 <- philly_votes(file4)
file5 <- philly_votes(file5)

test_that("right file name", {
  expect_equal(unique(file1$file), "2016 General - Ward 1,2,26 BIR 1")
  expect_equal(unique(file2$file), "2016 General - Ward 1,2,26 BIR 2")
  expect_equal(unique(file3$file), "2016 General - Ward 7,25,33,37 BIR 3")
  expect_equal(unique(file4$file), "2016 General - Ward 8,15 BIR 3")
  expect_equal(unique(file5$file), "2016 General - Ward 58 BIR 2")
})


test_that("right number of pages", {
  expect_equal(max(file1$pdf_page), 3000)
  expect_equal(max(file2$pdf_page), 6000)
  expect_equal(max(file3$pdf_page), 8252)
  expect_equal(max(file4$pdf_page), 9959)
  expect_equal(max(file5$pdf_page), 6264)
})




test_that("ballot ballot_position values are correct", {
  # First 6 votes
  expect_equal(head(file1$ballot_position), c("9C", "9D", "10D", "12D", "14D", "16D"))
  # Last 6 votes
  expect_equal(tail(file1$ballot_position), c("16C", "18C", "21C", "24C", "23L", "33L"))

  # First 6 votes
  expect_equal(head(file2$ballot_position), c("10C", "12C", "14C", "16C", "18C", "21C"))
  # Last 6 votes
  expect_equal(tail(file2$ballot_position), c("16C", "18C", "21C", "24C", "23L", "32L"))

  # First 6 votes
  expect_equal(head(file3$ballot_position), c("16C", "18C", "24C", "9C", "10C", "12C"))
  # Last 6 votes
  expect_equal(tail(file3$ballot_position), c("14C", "16C", "18C", "21C", "24C", "5L"))

  # First 6 votes
  expect_equal(head(file4$ballot_position), c("14D", "16D", "18D", "7L", "23L", "32L"))
  # Last 6 votes
  expect_equal(tail(file4$ballot_position), c("16C", "18C", "21C", "24C", "23L", "32L"))

  # First 6 votes
  expect_equal(head(file5$ballot_position), c("9C", "10C", "12C", "14C", "16C", "18C"))
  # Last 6 votes
  expect_equal(tail(file5$ballot_position), c("12C", "14C", "16C", "18C", "21C", "24C"))

})

test_that("unique IDs in right order", {
  expect_equal(head(file1$uniqueID), c("020001 1", "020001 2", "020001 2",
                                       "020001 2", "020001 2", "020001 2"))
  expect_equal(tail(file1$uniqueID), c("020048 39", "020048 39", "020048 39",
                                       "020048 39", "020048 39", "020048 39"))

  expect_equal(head(file2$uniqueID), c("020048 40", "020048 40", "020048 40",
                                       "020048 40", "020048 40", "020048 40"))
  expect_equal(tail(file2$uniqueID), c("020088 188", "020088 188", "020088 188",
                                       "020088 188", "020088 188", "020088 188"))

  expect_equal(head(file3$uniqueID), c("021481 84", "021481 84", "021481 84",
                                       "021481 85", "021481 85", "021481 85"))
  expect_equal(tail(file3$uniqueID), c("021760 224", "021760 224", "021760 224",
                                       "021760 224", "021760 224", "021760 224"))

  expect_equal(head(file4$uniqueID), c("020376 80", "020376 80", "020376 80",
                                       "020376 80", "020376 80", "020376 80"))
  expect_equal(tail(file4$uniqueID), c("020672 285", "020672 285", "020672 285",
                                       "020672 285", "020672 285", "020672 285"))

  expect_equal(head(file5$uniqueID), c("022901 98", "022901 98", "022901 98",
                                       "022901 98", "022901 98", "022901 98"))
  expect_equal(tail(file5$uniqueID), c("022946 198", "022946 198", "022946 198",
                                       "022946 198", "022946 198", "022946 198"))
})

test_that("voter number in right order", {
  expect_equal(head(file1$voter_record), c(1, 2, 2, 2, 2, 2))
  expect_equal(tail(file1$voter_record), c(39, 39, 39, 39, 39, 39))

  expect_equal(head(file2$voter_record), c(40, 40, 40, 40, 40, 40))
  expect_equal(tail(file2$voter_record), c(188, 188, 188, 188, 188, 188))

  expect_equal(head(file3$voter_record), c(84, 84, 84, 85, 85, 85))
  expect_equal(tail(file3$voter_record), c(224, 224, 224, 224, 224, 224))

  expect_equal(head(file4$voter_record), c(80, 80, 80, 80, 80, 80))
  expect_equal(tail(file4$voter_record), c(285, 285, 285, 285, 285, 285))

  expect_equal(head(file5$voter_record), c(98, 98, 98, 98, 98, 98))
  expect_equal(tail(file5$voter_record), c(198, 198, 198, 198, 198, 198))
})


test_that("proper number of unique ID for a location and serial number match", {

  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020048"])), 39)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020001"])), 168)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020006"])), 257)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020015"])), 190)

  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "020088"])), 188)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "020079"])), 202)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "023376"])), 134)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "020049"])), 253)

  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "021760"])), 224)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "021747"])), 232)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "021743"])), 228)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "021486"])), 186)

  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020672"])), 285)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020379"])), 327)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020641"])), 227)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020650"])), 231)

  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "022946"])), 198)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "022937"])), 200)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "022923"])), 224)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "022910"])), 294)
})

test_that("unique voters have correct number of files recorded", {
  expect_equal(nrow(file1[file1$uniqueID == "020048 39",]), 10)
  expect_equal(nrow(file1[file1$uniqueID == "020048 38",]), 10)
  expect_equal(nrow(file1[file1$uniqueID == "020001 1",]), 1)
  expect_equal(nrow(file1[file1$uniqueID == "020006 203",]), 2)

  expect_equal(nrow(file2[file2$uniqueID == "020088 188",]), 10)
  expect_equal(nrow(file2[file2$uniqueID == "020079 140",]), 10)
  expect_equal(nrow(file2[file2$uniqueID == "023376 37",]), 10)
  expect_equal(nrow(file2[file2$uniqueID == "020049 15",]), 6)

  expect_equal(nrow(file3[file3$uniqueID == "021760 224",]), 9)
  expect_equal(nrow(file3[file3$uniqueID == "021747 25",]), 9)
  expect_equal(nrow(file3[file3$uniqueID == "021743 39",]), 9)
  expect_equal(nrow(file3[file3$uniqueID == "021481 84",]), 3)

  expect_equal(nrow(file4[file4$uniqueID == "020672 285",]), 10)
  expect_equal(nrow(file4[file4$uniqueID == "020672 260",]), 4)
  expect_equal(nrow(file4[file4$uniqueID == "020376 80",]), 6)
  expect_equal(nrow(file4[file4$uniqueID == "020641 178",]), 8)

  expect_equal(nrow(file5[file5$uniqueID == "022946 198",]), 8)
  expect_equal(nrow(file5[file5$uniqueID == "022946 190",]), 1)
  expect_equal(nrow(file5[file5$uniqueID == "022937 38",]), 7)
  expect_equal(nrow(file5[file5$uniqueID == "022923 136",]), 7)

})


test_that("Candidate has right name", {

  expect_equal(file1$candidate[file1$uniqueID == "020001 1"],
               c("Hillary Clinton"))
  expect_equal(file1$candidate[file1$uniqueID == "020001 1"],
               c("Hillary Clinton"))
  expect_equal(file1$candidate[file1$uniqueID == "020021 257"],
               c("Hillary Clinton"))
  expect_equal(file1$candidate[file1$uniqueID == "020021 265"],
               c("Donald J Trump"))

  expect_equal(file2$candidate[file2$uniqueID == "020048 57"],
               c("Hillary Clinton"))
  expect_equal(file2$candidate[file2$uniqueID == "020059 94"],
               c("Donald J Trump"))
  expect_equal(file2$candidate[file2$uniqueID == "020059 101"],
               c("Yes Si", "No No"))
  expect_equal(file2$candidate[file2$uniqueID == "020070 96"],
               c("Donald J Trump"))

  expect_equal(file3$candidate[file3$uniqueID == "021481 84"],
               c("Joe Torsella",
                 "Robert A Brady",
                 "Angel Cruz"))
  expect_equal(file3$candidate[file3$uniqueID == "021481 90"],
               c("Katie Mcginty", "Write In"))
  expect_equal(file3$candidate[file3$uniqueID == "021726 25"],
               c("Hillary Clinton",
                 "Katie Mcginty",
                 "Dwight Evans"))
  expect_equal(file3$candidate[file3$uniqueID == "021740 143"],
               c("Hillary Clinton",
                 "Otto Voit",
                 "James A Jones"))

  expect_equal(file4$candidate[file4$uniqueID == "020376 85"],
               c("Hillary Clinton"))
  expect_equal(file4$candidate[file4$uniqueID == "020376 95"],
               c("Hillary Clinton",
                 "Katie Mcginty"))
  expect_equal(file4$candidate[file4$uniqueID == "020656 28"],
               c("Robert A Brady"))
  expect_equal(file4$candidate[file4$uniqueID == "020656 43"],
               c("No No"))

  expect_equal(file5$candidate[file5$uniqueID == "022901 119"],
               c("Martina White"))
  expect_equal(file5$candidate[file5$uniqueID == "022901 131"],
               c("Yes Si",
                 "No No"))
  expect_equal(file5$candidate[file5$uniqueID == "022907 156"],
               c("Hillary Clinton"))
  expect_equal(file5$candidate[file5$uniqueID == "023503 216"],
               c("Donald J Trump",
                 "Pat Toomey",
                 "Martina White"))
})


test_that("Categories are correct", {

  expect_equal(file1$category[file1$uniqueID == "020001 1"],
               c("President and Vice President of the United States"))
  expect_equal(file1$category[file1$uniqueID == "020001 4"],
               c("President and Vice President of the United States"))
  expect_equal(file1$category[file1$uniqueID == "020021 257"],
               c("President and Vice President of the United States"))
  expect_equal(file1$category[file1$uniqueID == "020021 265"],
               c("President and Vice President of the United States"))

  expect_equal(file2$category[file2$uniqueID == "020048 57"],
               c("President and Vice President of the United States"))
  expect_equal(file2$category[file2$uniqueID == "020059 94"],
               c("President and Vice President of the United States"))
  expect_equal(file2$category[file2$uniqueID == "020059 101"],
               c("Proposed Constitutional Amendment",
                 "City Bond Question"))
  expect_equal(file2$category[file2$uniqueID == "020070 96"],
               c("President and Vice President of the United States"))

  expect_equal(file3$category[file3$uniqueID == "021481 84"],
               c("State Treasurer",
                 "Representative in Congress - 1st District",
                 "Representative in the General Assembly - 180th District"))
  expect_equal(file3$category[file3$uniqueID == "021481 90"],
               c("United States Senator",
                 "President and Vice President of the United States"))
  expect_equal(file3$category[file3$uniqueID == "021726 25"],
               c("President and Vice President of the United States",
                 "United States Senator",
                 "Representative in Congress - 2nd District"))
  expect_equal(file3$category[file3$uniqueID == "021740 143"],
               c("President and Vice President of the United States",
                 "State Treasurer",
                 "Representative in Congress - 2nd District"))

  expect_equal(file4$category[file4$uniqueID == "020376 85"],
               c("President and Vice President of the United States"))
  expect_equal(file4$category[file4$uniqueID == "020376 95"],
               c("President and Vice President of the United States",
                 "United States Senator"))
  expect_equal(file4$category[file4$uniqueID == "020656 28"],
               c("Representative in Congress - 1st District"))
  expect_equal(file4$category[file4$uniqueID == "020656 43"],
               c("City Bond Question"))

  expect_equal(file5$category[file5$uniqueID == "022901 119"],
               c("Representative in the General Assembly - 170th District"))
  expect_equal(file5$category[file5$uniqueID == "022901 131"],
               c("Proposed Constitutional Amendment",
                 "City Bond Question"))
  expect_equal(file5$category[file5$uniqueID == "022907 156"],
               c("President and Vice President of the United States"))
  expect_equal(file5$category[file5$uniqueID == "023503 216"],
               c("President and Vice President of the United States",
                 "United States Senator",
                 "Representative in the General Assembly - 170th District"))

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
