context("election-2015-general")

file1 <- system.file("data/PDF_data/2015 General",
                     "2015 General - BLACK BIR 1 of 9.pdf",
                     package = "phillyvotes")
file2 <- system.file("data/PDF_data/2015 General",
                     "2015 General - BLACK BIR 5 of 9.pdf",
                     package = "phillyvotes")
file3 <- system.file("data/PDF_data/2015 General",
                     "2015 General - BROWN _ PINK BIR 1 of 16.pdf",
                     package = "phillyvotes")
file4 <- system.file("data/PDF_data/2015 General",
                     "2015 General - GREEN BIR 1 of 4.pdf",
                     package = "phillyvotes")
file5 <- system.file("data/PDF_data/2015 General",
                     "2015 General - YELLOW BIR 2 of 4.pdf",
                     package = "phillyvotes")

file1 <- philly_votes(file1)
file2 <- philly_votes(file2)
file3 <- philly_votes(file3)
file4 <- philly_votes(file4)
file5 <- philly_votes(file5)

test_that("right file name", {
  expect_equal(unique(file1$file), "2015 General - BLACK BIR 1 of 9")
  expect_equal(unique(file2$file), "2015 General - BLACK BIR 5 of 9")
  expect_equal(unique(file3$file), "2015 General - BROWN _ PINK BIR 1 of 16")
  expect_equal(unique(file4$file), "2015 General - GREEN BIR 1 of 4")
  expect_equal(unique(file5$file), "2015 General - YELLOW BIR 2 of 4")
})


test_that("right number of pages", {
  expect_equal(max(file1$pdf_page), 3000)
  expect_equal(max(file2$pdf_page), 15000)
  expect_equal(max(file3$pdf_page), 3000)
  expect_equal(max(file4$pdf_page), 3000)
  expect_equal(max(file5$pdf_page), 6000)
})




test_that("ballot ballot_position values are correct", {
  # First 6 votes
  expect_equal(head(file1$ballot_position), c("4B", "5B", "6B", "17B", "19B", "21B"))
  # Last 6 votes
  expect_equal(tail(file1$ballot_position), c("12B", "13B", "14B", "15B", "16B", "17B"))

  # First 6 votes
  expect_equal(head(file2$ballot_position), c("39D", "42F", "37I", "35L", "4B", "19B"))
  # Last 6 votes
  expect_equal(tail(file2$ballot_position), c("4D", "6D", "8D", "10D", "17D", "20D"))

  # First 6 votes
  expect_equal(head(file3$ballot_position), c("3B", "4B", "5B", "6B", "11B", "13B"))
  # Last 6 votes
  expect_equal(tail(file3$ballot_position), c("13B", "15B", "17B", "19B", "20B", "21B"))

  # First 6 votes
  expect_equal(head(file4$ballot_position), c("3B", "4B", "5B", "6B", "8B", "10B"))
  # Last 6 votes
  expect_equal(tail(file4$ballot_position), c("24J", "3B", "4B", "5B", "6B", "8B"))

  # First 6 votes
  expect_equal(head(file5$ballot_position), c("3B", "4B", "5B", "6B", "8B", "10B"))
  # Last 6 votes
  expect_equal(tail(file5$ballot_position), c("25B", "26B", "27B", "28B", "29B", "39B"))

})

test_that("unique IDs in right order", {
  expect_equal(head(file1$uniqueID), c("022721 1", "022721 1", "022721 1",
                                       "022721 1", "022721 1", "022721 1"))
  expect_equal(tail(file1$uniqueID), c("022778 5", "022778 5", "022778 5",
                                       "022778 5", "022778 5", "022778 5"))

  expect_equal(head(file2$uniqueID), c("022927 8", "022927 8", "022927 8",
                                       "022927 8", "022927 9", "022927 9"))
  expect_equal(tail(file2$uniqueID), c("023174 19", "023174 19", "023174 19",
                                       "023174 19", "023174 19", "023174 19"))

  expect_equal(head(file3$uniqueID), c("020097 1", "020097 1", "020097 1",
                                       "020097 1", "020097 1", "020097 1"))
  expect_equal(tail(file3$uniqueID), c("020141 3", "020141 3", "020141 3",
                                       "020141 3", "020141 3", "020141 3"))

  expect_equal(head(file4$uniqueID), c("020183 1", "020183 1", "020183 1",
                                       "020183 1", "020183 1", "020183 1"))
  expect_equal(tail(file4$uniqueID), c("020210 63", "020210 64", "020210 64",
                                       "020210 64", "020210 64", "020210 64"))

  expect_equal(head(file5$uniqueID), c("021466 12", "021466 12", "021466 12",
                                       "021466 12", "021466 12", "021466 12"))
  expect_equal(tail(file5$uniqueID), c("023451 22", "023451 22", "023451 22",
                                       "023451 22", "023451 22", "023451 22"))
})




test_that("proper number of unique ID for a location and serial number match", {

  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022778"])), 5)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022777"])), 101)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022721"])), 79)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022736"])), 150)

  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "023174"])), 19)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "023166"])), 122)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "022935"])), 110)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "022941"])), 56)

  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "020140"])), 62)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "020125"])), 88)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "020097"])), 52)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "020127"])), 99)

  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020210"])), 64)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020198"])), 116)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020191"])), 142)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020183"])), 164)

  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "023451"])), 22)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "022061"])), 61)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "021748"])), 49)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "021735"])), 88)
})

test_that("unique voters have correct number of files recorded", {
  expect_equal(nrow(file1[file1$uniqueID == "022778 4",]), 7)
  expect_equal(nrow(file1[file1$uniqueID == "022777 100",]), 4)
  expect_equal(nrow(file1[file1$uniqueID == "022777 101",]), 9)
  expect_equal(nrow(file1[file1$uniqueID == "022721 4",]), 17)

  expect_equal(nrow(file2[file2$uniqueID == "023174 19",]), 8)
  expect_equal(nrow(file2[file2$uniqueID == "023166 50",]), 9)
  expect_equal(nrow(file2[file2$uniqueID == "022927 8",]), 4)
  expect_equal(nrow(file2[file2$uniqueID == "022941 42",]), 18)

  expect_equal(nrow(file3[file3$uniqueID == "020141 3",]), 14)
  expect_equal(nrow(file3[file3$uniqueID == "020127 69",]), 6)
  expect_equal(nrow(file3[file3$uniqueID == "020127 56",]), 7)
  expect_equal(nrow(file3[file3$uniqueID == "020112 4",]), 4)

  expect_equal(nrow(file4[file4$uniqueID == "020210 63",]), 9)
  expect_equal(nrow(file4[file4$uniqueID == "020210 64",]), 5)
  expect_equal(nrow(file4[file4$uniqueID == "020183 14",]), 16)
  expect_equal(nrow(file4[file4$uniqueID == "020186 40",]), 11)

  expect_equal(nrow(file5[file5$uniqueID == "023451 19",]), 14)
  expect_equal(nrow(file5[file5$uniqueID == "022061 50",]), 3)
  expect_equal(nrow(file5[file5$uniqueID == "021748 26",]), 1)
  expect_equal(nrow(file5[file5$uniqueID == "021735 29",]), 4)

})

test_that("voter number in right order", {
  expect_equal(head(file1$voter_record), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file1$voter_record), c(5, 5, 5, 5, 5, 5))

  expect_equal(head(file2$voter_record), c(8, 8, 8, 8, 9, 9))
  expect_equal(tail(file2$voter_record), c(19, 19, 19, 19, 19, 19))

  expect_equal(head(file3$voter_record), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file3$voter_record), c(3, 3, 3, 3, 3, 3))

  expect_equal(head(file4$voter_record), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file4$voter_record), c(63, 64, 64, 64, 64, 64))

  expect_equal(head(file5$voter_record), c(12, 12, 12, 12, 12, 12))
  expect_equal(tail(file5$voter_record), c(22, 22, 22, 22, 22, 22))
})


test_that("Candidate has right name", {

  expect_equal(file1$candidate[file1$uniqueID == "022721 9"],
               c("Kevin M Dougherty",
                 "Lisa M Deeley",
                 "Mike George",
                 "Melissa Murray Bailey",
                 "Al Taubenberger",
                 "Brian J O Neill"))
  expect_equal(file1$candidate[file1$uniqueID == "022721 20"],
               c("Kevin M Dougherty",
                 "Michael Wojcik",
                 "Jim Kenney",
                 "Lisa M Deeley"))
  expect_equal(file1$candidate[file1$uniqueID == "022721 22"],
               c("Kevin M Dougherty",
                 "Lisa M Deeley"))
  expect_equal(file1$candidate[file1$uniqueID == "022721 23"],
               c("Kevin M Dougherty",
                 "Jim Kenney"))

  expect_equal(file2$candidate[file2$uniqueID == "022927 10"],
               c("Kevin M Dougherty",
                 "Allan Domb",
                 "Schulman Yes Si"))
  expect_equal(file2$candidate[file2$uniqueID == "022940 97"],
               c("Judy Olson",
                 "Mike George",
                 "Anne Covey",
                 "Melissa Murray Bailey"))
  expect_equal(file2$candidate[file2$uniqueID == "023153 98"],
               c("Yes Si",
                 "Melissa Murray Bailey",
                 "Brian J O Neill",
                 "Yes Si", "Yes Si"))
  expect_equal(file2$candidate[file2$uniqueID == "023153 101"],
               c("Kevin M Dougherty",
                 "Sharon Williams Losier",
                 "Derek S Green",
                 "Al Schmidt"))

  expect_equal(file3$candidate[file3$uniqueID == "020097 4"],
               c("Kevin M Dougherty",
                 "Jim Kenney",
                 "Jewell Williams",
                 "Blondell Reynolds Brown",
                 "Jannie L Blackwell"))
  expect_equal(file3$candidate[file3$uniqueID == "020097 10"],
               c("Yes Si"))
  expect_equal(file3$candidate[file3$uniqueID == "020097 14"],
               c("Kevin M Dougherty",
                 "Jim Kenney",
                 "Jewell Williams",
                 "Blondell Reynolds Brown",
                 "Jannie L Blackwell"))
  expect_equal(file3$candidate[file3$uniqueID == "020117 30"],
               c("David Oh"))

  expect_equal(file4$candidate[file4$uniqueID == "020185 19"],
               c("Kevin M Dougherty",
                 "Jim Kenney"))
  expect_equal(file4$candidate[file4$uniqueID == "020185 39"],
               c("Mark F Squilla",
                 "Melissa Murray Bailey"))
  expect_equal(file4$candidate[file4$uniqueID == "020187 89"],
               c("Jim Kenney",
                 "Allan Domb",
                 "Helen Gym",
                 "Fox Yes Si"))
  expect_equal(file4$candidate[file4$uniqueID == "020198 76"],
               c("David Wecht",
                 "Kevin M Dougherty",
                 "Ronald R Donatucci",
                 "Rainy Papademetriou",
                 "Carrafiello Yes Si"))

  expect_equal(file5$candidate[file5$uniqueID == "021748 26"],
               c("Jim Kenney"))
  expect_equal(file5$candidate[file5$uniqueID == "021466 15"],
               c("Jim Kenney"))
  expect_equal(file5$candidate[file5$uniqueID == "021466 17"],
               c("Kevin M Dougherty",
                 "Maria Quinones Sanchez"))
  expect_equal(file5$candidate[file5$uniqueID == "021466 20"],
               c("Melissa Murray Bailey"))
})


test_that("Categories are correct", {

  expect_equal(file1$category[file1$uniqueID == "022721 9"],
               c("Justice of the Supreme Court",
                 "City Commissioners", "Justice of the Supreme Court",
                 "Mayor", "Council at Large", "District Council - 10th District"))
  expect_equal(file1$category[file1$uniqueID == "022721 20"],
               c("Justice of the Supreme Court", "Judge of the Commonwealth Court",
                 "Mayor", "City Commissioners"))
  expect_equal(file1$category[file1$uniqueID == "022721 22"],
               c("Justice of the Supreme Court", "City Commissioners"))
  expect_equal(file1$category[file1$uniqueID == "022721 23"],
               c("Justice of the Supreme Court", "Mayor"))

  expect_equal(file2$category[file2$uniqueID == "022927 10"],
               c("Justice of the Supreme Court", "Council at Large",
                 "Retention - Judge of the Court of Common Pleas - Susan I Sch"))
  expect_equal(file2$category[file2$uniqueID == "022940 97"],
               c("Justice of the Supreme Court", "Justice of the Supreme Court",
                 "Justice of the Supreme Court", "Mayor"))
  expect_equal(file2$category[file2$uniqueID == "023153 98"],
               c("City Bond Question", "Mayor", "District Council - 10th District",
                 "Proposed Charter Change Question #1",
                 "Proposed Charter Change Question #2"))
  expect_equal(file2$category[file2$uniqueID == "023153 101"],
               c("Justice of the Supreme Court",
                 "Judge of the Municipal Court",
                 "Council at Large", "City Commissioners"))

  expect_equal(file3$category[file3$uniqueID == "020097 4"],
               c("Justice of the Supreme Court", "Mayor",
                 "Sheriff", "Council at Large", "District Council - 3rd District"))
  expect_equal(file3$category[file3$uniqueID == "020097 10"],
               c("City Bond Question"))
  expect_equal(file3$category[file3$uniqueID == "020097 14"],
               c("Justice of the Supreme Court", "Mayor", "Sheriff",
                 "Council at Large", "District Council - 3rd District"))
  expect_equal(file3$category[file3$uniqueID == "020117 30"],
               c("Council at Large"))

  expect_equal(file4$category[file4$uniqueID == "020185 19"],
               c("Justice of the Supreme Court", "Mayor"))
  expect_equal(file4$category[file4$uniqueID == "020185 39"],
               c("District Council - 1st District", "Mayor"))
  expect_equal(file4$category[file4$uniqueID == "020187 89"],
               c("Mayor", "Council at Large", "Council at Large",
                 "Retention - Judge of the Court of Common Pleas - Idee C Fox"))
  expect_equal(file4$category[file4$uniqueID == "020198 76"],
               c("Justice of the Supreme Court",
                 "Justice of the Supreme Court",
                 "Register of Wills",
                 "Judge of the Court of Common Pleas",
                 "Retention - Judge of the Court of Common Pleas - Matthew D C"))

  expect_equal(file5$category[file5$uniqueID == "021748 26"],
               c("Mayor"))
  expect_equal(file5$category[file5$uniqueID == "021466 15"],
               c("Mayor"))
  expect_equal(file5$category[file5$uniqueID == "021466 17"],
               c("Justice of the Supreme Court",
                 "District Council - 7th District"))
  expect_equal(file5$category[file5$uniqueID == "021466 20"],
               c("Mayor"))

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
