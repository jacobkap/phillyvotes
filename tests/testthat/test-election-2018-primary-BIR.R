context("election-2018-primary")

file1 <- system.file("data/PDF_data/2018 Primary",
                     "2018 Primary - BLACK BIR 1.pdf",
                     package = "phillyvotes")
file2 <- system.file("data/PDF_data/2018 Primary",
                     "2018 Primary - BLUE BIR 2.pdf",
                     package = "phillyvotes")
file3 <- system.file("data/PDF_data/2018 Primary",
                     "2018 PRIMARY - GREEN BIR.pdf",
                     package = "phillyvotes")
file4 <- system.file("data/PDF_data/2018 Primary",
                     "2018 PRIMARY - ORANGE BIR 3.pdf",
                     package = "phillyvotes")
file5 <- system.file("data/PDF_data/2018 Primary",
                     "2018 PRIMARY - PINK BIR.pdf",
                     package = "phillyvotes")

file1 <- philly_votes(file1)
file2 <- philly_votes(file2)
file3 <- philly_votes(file3)
file4 <- philly_votes(file4)
file5 <- philly_votes(file5)

test_that("right file name", {
  expect_equal(unique(file1$file), "2018 Primary - BLACK BIR 1")
  expect_equal(unique(file2$file), "2018 Primary - BLUE BIR 2")
  expect_equal(unique(file3$file), "2018 PRIMARY - GREEN BIR")
  expect_equal(unique(file4$file), "2018 PRIMARY - ORANGE BIR 3")
  expect_equal(unique(file5$file), "2018 PRIMARY - PINK BIR")
})


test_that("right number of pages", {
  expect_equal(max(file1$pdf_page), 3000)
  expect_equal(max(file2$pdf_page), 6000)
  expect_equal(max(file3$pdf_page), 4772)
  expect_equal(max(file4$pdf_page), 9000)
  expect_equal(max(file5$pdf_page), 4235)
})




test_that("ballot ballot_position values are correct", {
  # First 6 votes
  expect_equal(head(file1$ballot_position), c("13C", "20C", "32C", "10D", "27E", "4F"))
  # Last 6 votes
  expect_equal(tail(file1$ballot_position), c("28L", "38L", "20F", "13L", "28L", "37L"))

  # First 6 votes
  expect_equal(head(file2$ballot_position), c("27L", "37L", "3C", "6C", "11C", "13C"))
  # Last 6 votes
  expect_equal(tail(file2$ballot_position), c("16E", "13L", "27L", "38L", "3C", "6C"))

  # First 6 votes
  expect_equal(head(file3$ballot_position), c("3C", "6C", "9C", "20C", "23C", "24C"))
  # Last 6 votes
  expect_equal(tail(file3$ballot_position), c("26D", "23E", "25E", "12L", "27L", "37L"))

  # First 6 votes
  expect_equal(head(file4$ballot_position), c("32C", "33C", "13L", "28L", "37L", "3C"))
  # Last 6 votes
  expect_equal(tail(file4$ballot_position), c("24C", "26C", "32C", "33C", "10D", "13L"))

  # First 6 votes
  expect_equal(head(file5$ballot_position), c("3C", "6C", "13C", "17C", "21C", "24C"))
  # Last 6 votes
  expect_equal(tail(file5$ballot_position), c("20F", "28F", "32F", "13L", "27L", "37L"))

})

test_that("unique IDs in right order", {
  expect_equal(head(file1$uniqueID), c("022721 1", "022721 1", "022721 1",
                                       "022721 1", "022721 1", "022721 2"))
  expect_equal(tail(file1$uniqueID), c("022940 101", "022940 101", "022940 102",
                                       "022940 102", "022940 102", "022940 102"))

  expect_equal(head(file2$uniqueID), c("020091 78", "020091 78", "020091 79",
                                       "020091 79", "020091 79", "020091 79"))
  expect_equal(tail(file2$uniqueID), c("021677 36", "021677 36", "021677 36",
                                       "021677 36", "021677 37", "021677 37"))

  expect_equal(head(file3$uniqueID), c("020183 1", "020183 1", "020183 1",
                                       "020183 1", "020183 1", "020183 1"))
  expect_equal(tail(file3$uniqueID), c("023378 27", "023378 27", "023378 27",
                                       "023378 27", "023378 27", "023378 27"))

  expect_equal(head(file4$uniqueID), c("020754 0", "020754 0", "020754 0",
                                       "020754 0", "020754 0", "020755 1"))
  expect_equal(tail(file4$uniqueID), c("020964 31", "020964 31", "020964 31",
                                       "020964 31", "020964 31", "020964 31"))

  expect_equal(head(file5$uniqueID), c("021009 1", "021009 1", "021009 1",
                                       "021009 1", "021009 1", "021009 1"))
  expect_equal(tail(file5$uniqueID), c("023236 64", "023236 64", "023236 64",
                                       "023236 64", "023236 64", "023236 64"))
})

test_that("voter number in right order", {
  expect_equal(head(file1$voter_record), c(1, 1, 1, 1, 1, 2))
  expect_equal(tail(file1$voter_record), c(101, 101, 102, 102, 102, 102))

  expect_equal(head(file2$voter_record), c(78, 78, 79, 79, 79, 79))
  expect_equal(tail(file2$voter_record), c(36, 36, 36, 36, 37, 37))

  expect_equal(head(file3$voter_record), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file3$voter_record), c(27, 27, 27, 27, 27, 27))

  expect_equal(head(file4$voter_record), c(0, 0, 0, 0, 0, 1))
  expect_equal(tail(file4$voter_record), c(31, 31, 31, 31, 31, 31))

  expect_equal(head(file5$voter_record), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file5$voter_record), c(64, 64, 64, 64, 64, 64))
})



test_that("proper number of unique ID for a location and serial number match", {

  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022940"])), 102)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022862"])), 16)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022861"])), 16)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022794"])), 34)

  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021677"])), 37)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021644"])), 17)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021334"])), 53)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021151"])), 74)

  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "023378"])), 27)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "020784"])), 111)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "020768"])), 76)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "020202"])), 110)

  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020964"])), 31)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020951"])), 214)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020919"])), 74)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020917"])), 44)

  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "023236"])), 64)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "022667"])), 34)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "022666"])), 32)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "022665"])), 45)
})

test_that("unique voters have correct number of files recorded", {
  expect_equal(nrow(file1[file1$uniqueID == "022940 102",]), 4)
  expect_equal(nrow(file1[file1$uniqueID == "022940 96",]), 3)
  expect_equal(nrow(file1[file1$uniqueID == "022862 8",]), 10)
  expect_equal(nrow(file1[file1$uniqueID == "022861 11",]), 5)

  expect_equal(nrow(file2[file2$uniqueID == "021677 37",]), 2)
  expect_equal(nrow(file2[file2$uniqueID == "021644 16",]), 14)
  expect_equal(nrow(file2[file2$uniqueID == "021334 18",]), 9)
  expect_equal(nrow(file2[file2$uniqueID == "020091 78",]), 2)

  expect_equal(nrow(file3[file3$uniqueID == "023378 25",]), 2)
  expect_equal(nrow(file3[file3$uniqueID == "020768 43",]), 7)
  expect_equal(nrow(file3[file3$uniqueID == "020768 40",]), 6)
  expect_equal(nrow(file3[file3$uniqueID == "020202 45",]), 3)

  expect_equal(nrow(file4[file4$uniqueID == "020951 24",]), 3)
  expect_equal(nrow(file4[file4$uniqueID == "020951 17",]), 9)
  expect_equal(nrow(file4[file4$uniqueID == "020919 7",]), 11)
  expect_equal(nrow(file4[file4$uniqueID == "020917 40",]), 7)

  expect_equal(nrow(file5[file5$uniqueID == "023236 63",]), 7)
  expect_equal(nrow(file5[file5$uniqueID == "023236 64",]), 9)
  expect_equal(nrow(file5[file5$uniqueID == "022667 11",]), 3)
  expect_equal(nrow(file5[file5$uniqueID == "022667 1",]), 1)

})


test_that("Candidate has right name", {

  expect_equal(file1$candidate[file1$uniqueID == "022721 7"],
               c("Barbara Deeley",
                 "Nina Ahmad"))
  expect_equal(file1$candidate[file1$uniqueID == "022721 20"],
               c("Yes Si",
                 "Yes Si",
                 "Yes Si"))
  expect_equal(file1$candidate[file1$uniqueID == "022820 43"],
               c("John Dellricci",
                 "James G Donnelly Sr",
                 "Kathy Enggasser",
                 "Patrick Parkinson",
                 "Mary Ann Quartullo"))
  expect_equal(file1$candidate[file1$uniqueID == "022821 4"],
               c("Yes Si", "Yes Si"))

  expect_equal(file2$candidate[file2$uniqueID == "020091 78"],
               c("Yes Si",
                 "Yes Si"))
  expect_equal(file2$candidate[file2$uniqueID == "021156 13"],
               c("Mike Stack",
                 "Rich Lazer"))
  expect_equal(file2$candidate[file2$uniqueID == "021156 46"],
               c("Maria P Donatucci",
                 "Rich Lazer"))
  expect_equal(file2$candidate[file2$uniqueID == "021156 50"],
               c("Yes Si",
                 "Yes Si",
                 "Yes Si"))

  expect_equal(file3$candidate[file3$uniqueID == "020183 11"],
               c("Bob Casey Jr",
                 "Tom Wolf",
                 "John Fetterman"))
  expect_equal(file3$candidate[file3$uniqueID == "020183 24"],
               c("Yes Si"))
  expect_equal(file3$candidate[file3$uniqueID == "023396 88"],
               c("Tom Wolf",
                 "Malcolm Kenyatta"))
  expect_equal(file3$candidate[file3$uniqueID == "023378 25"],
               c("Michael H O Brien",
                 "Nina Ahmad"))

  expect_equal(file4$candidate[file4$uniqueID == "020755 8"],
               c("Dwight Evans",
                 "Rosita C Youngblood"))
  expect_equal(file4$candidate[file4$uniqueID == "020910 45"],
               c("Jim Christiana",
                 "Scott R Wagner"))
  expect_equal(file4$candidate[file4$uniqueID == "020911 4"],
               c("Bob Casey Jr",
                 "Tom Wolf",
                 "Pamela Delissio",
                 "Keith D Myers"))
  expect_equal(file4$candidate[file4$uniqueID == "020963 81"],
               c("Yes Si",
                 "Yes Si",
                 "Yes Si"))

  expect_equal(file5$candidate[file5$uniqueID == "021009 5"],
               c("Bob Casey Jr",
                 "Joe Hohenstein"))
  expect_equal(file5$candidate[file5$uniqueID == "021009 10"],
               c("Bob Casey Jr"))
  expect_equal(file5$candidate[file5$uniqueID == "022593 32"],
               c("Nicolas C Ortega"))
  expect_equal(file5$candidate[file5$uniqueID == "023236 54"],
               c("Tom Wolf",
                 "Brendan F Boyle",
                 "Sean Kilkenny"))
})


test_that("Categories are correct", {

  expect_equal(file1$category[file1$uniqueID == "022721 7"],
               c("Ward Executive Committee - 56-01 - Democrat",
                 "Lieutenant Governor - Democrat"))
  expect_equal(file1$category[file1$uniqueID == "022721 20"],
               c("Proposed Charter Change Question #1",
                 "Proposed Charter Change Question #2",
                 "Proposed Charter Change Question #3"))
  expect_equal(file1$category[file1$uniqueID == "022820 43"],
               c("Member of the Democratic State Committee - 5th Senatorial District",
                 "Member of the Democratic State Committee - 5th Senatorial District",
                 "Member of the Democratic State Committee - 5th Senatorial District",
                 "Member of the Democratic State Committee - 5th Senatorial District",
                 "Member of the Democratic State Committee - 5th Senatorial District"))
  expect_equal(file1$category[file1$uniqueID == "022821 4"],
               c("Proposed Charter Change Question #2",
                 "Proposed Charter Change Question #3"))

  expect_equal(file2$category[file2$uniqueID == "021156 50"],
               c("Proposed Charter Change Question #1",
                 "Proposed Charter Change Question #2",
                 "Proposed Charter Change Question #3"))
  expect_equal(file2$category[file2$uniqueID == "021156 80"],
               c("Representative in the General Assembly - 185th District - Democrat"))
  expect_equal(file2$category[file2$uniqueID == "021677 37"],
               c("United States Senator - Democrat",
                 "Governor - Democrat"))
  expect_equal(file2$category[file2$uniqueID == "021677 32"],
               c("Representative in Congress - 3rd District - Democrat",
                 "Senator in the General Assembly - 8th District - Democrat",
                 "Representative in the General Assembly - 186th District - Democrat"))

  expect_equal(file3$category[file3$uniqueID == "020183 11"],
               c("United States Senator - Democrat",
                 "Governor - Democrat",
                 "Lieutenant Governor - Democrat"))
  expect_equal(file3$category[file3$uniqueID == "020183 24"],
               c("Proposed Charter Change Question #1"))
  expect_equal(file3$category[file3$uniqueID == "023396 88"],
               c("Governor - Democrat",
                 "Representative in the General Assembly - 181st District - Democrat"))
  expect_equal(file3$category[file3$uniqueID == "023378 25"],
               c("Representative in the General Assembly - 175th District - Democrat",
                 "Lieutenant Governor - Democrat"))

  expect_equal(file4$category[file4$uniqueID == "020755 8"],
               c("Representative in Congress - 3rd District - Democrat",
                 "Representative in the General Assembly - 198th District - Democrat"))
  expect_equal(file4$category[file4$uniqueID == "020910 45"],
               c("United States Senator - Republican",
                 "Governor - Republican"))
  expect_equal(file4$category[file4$uniqueID == "020911 4"],
               c("United States Senator - Democrat",
                 "Governor - Democrat",
                 "Representative in the General Assembly - 194th District - Democrat",
                 "Ward Executive Committee - 21-26 - Democrat"))
  expect_equal(file4$category[file4$uniqueID == "020963 81"],
               c("Proposed Charter Change Question #1",
                 "Proposed Charter Change Question #2",
                 "Proposed Charter Change Question #3"))

  expect_equal(file5$category[file5$uniqueID == "021009 5"],
               c("United States Senator - Democrat",
                 "Representative in the General Assembly - 177th District - Democrat"))
  expect_equal(file5$category[file5$uniqueID == "021009 10"],
               c("United States Senator - Democrat"))
  expect_equal(file5$category[file5$uniqueID == "022593 32"],
               c("Ward Executive Committee - 53-11 - Democrat"))
  expect_equal(file5$category[file5$uniqueID == "023236 54"],
               c("Governor - Democrat",
                 "Representative in Congress - 2nd District - Democrat",
                 "Representative in the General Assembly - 177th District - Democrat"))

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
