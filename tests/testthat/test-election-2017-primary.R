context("election-2017-primary")

file1 <- system.file("data/PDF_data/2017 Primary",
                     "2017 Primary - Black BIR 1.pdf",
                     package = "phillyvotes")
file2 <- system.file("data/PDF_data/2017 Primary",
                     "2017 Primary - BLUE BIR 2.pdf",
                     package = "phillyvotes")
file3 <- system.file("data/PDF_data/2017 Primary",
                     "2017 Primary - BROWN BIR 2.pdf",
                     package = "phillyvotes")
file4 <- system.file("data/PDF_data/2017 Primary",
                     "2017 Primary - ORANGE BIR 1.pdf",
                     package = "phillyvotes")
file5 <- system.file("data/PDF_data/2017 Primary",
                     "2017 Primary - PINK BIR 2.pdf",
                     package = "phillyvotes")

file1 <- philly_votes(file1)
file2 <- philly_votes(file2)
file3 <- philly_votes(file3)
file4 <- philly_votes(file4)
file5 <- philly_votes(file5)

test_that("right file name", {
  expect_equal(unique(file1$file), "2017 Primary - Black BIR 1")
  expect_equal(unique(file2$file), "2017 Primary - BLUE BIR 2")
  expect_equal(unique(file3$file), "2017 Primary - BROWN BIR 2")
  expect_equal(unique(file4$file), "2017 Primary - ORANGE BIR 1")
  expect_equal(unique(file5$file), "2017 Primary - PINK BIR 2")
})


test_that("right number of pages", {
  expect_equal(max(file1$pdf_page), 3000)
  expect_equal(max(file2$pdf_page), 6000)
  expect_equal(max(file3$pdf_page), 6000)
  expect_equal(max(file4$pdf_page), 3000)
  expect_equal(max(file5$pdf_page), 4621)
})




test_that("ballot ballot_position values are correct", {
  # First 6 votes
  expect_equal(head(file1$ballot_position), c("3C", "6C", "7C", "8C", "10C", "12C"))
  # Last 6 votes
  expect_equal(tail(file1$ballot_position), c("22C", "23C", "29C", "31C", "6D", "9D"))

  # First 6 votes
  expect_equal(head(file2$ballot_position), c("15D", "17D", "20D", "28D", "9E", "13L"))
  # Last 6 votes
  expect_equal(tail(file2$ballot_position), c("22C", "23C", "24C", "32C", "33C", "36C"))

  # First 6 votes
  expect_equal(head(file3$ballot_position), c("12L", "26L", "3C", "6C", "7C", "8C"))
  # Last 6 votes
  expect_equal(tail(file3$ballot_position), c("18D", "24D", "14E", "18E", "12L", "26L"))

  # First 6 votes
  expect_equal(head(file4$ballot_position), c("3C", "8C", "12C", "21C", "32C", "36C"))
  # Last 6 votes
  expect_equal(tail(file4$ballot_position), c("8C", "12C", "14C", "16C", "19C", "21C"))

  # First 6 votes
  expect_equal(head(file5$ballot_position), c("29C", "3C", "6C", "8C", "9C", "23C"))
  # Last 6 votes
  expect_equal(tail(file5$ballot_position), c("17E", "18E", "20E", "21E", "24E", "35E"))

})

test_that("unique IDs in right order", {
  expect_equal(head(file1$uniqueID), c("022721 1", "022721 1", "022721 1",
                                       "022721 1", "022721 1", "022721 1"))
  expect_equal(tail(file1$uniqueID), c("022917 3", "022917 3", "022917 3",
                                       "022917 3", "022917 3", "022917 3"))

  expect_equal(head(file2$uniqueID), c("020080 64", "020080 64", "020080 64",
                                       "020080 64", "020080 64", "020080 64"))
  expect_equal(tail(file2$uniqueID), c("021333 30", "021333 30", "021333 30",
                                       "021333 30", "021333 30", "021333 30"))

  expect_equal(head(file3$uniqueID), c("021070 51", "021070 51", "021070 52",
                                       "021070 52", "021070 52", "021070 52"))
  expect_equal(tail(file3$uniqueID), c("021553 36", "021553 36", "021553 36",
                                       "021553 36", "021553 36", "021553 36"))

  expect_equal(head(file4$uniqueID), c("020383 1", "020383 1", "020383 1",
                                       "020383 1", "020383 1", "020383 1"))
  expect_equal(tail(file4$uniqueID), c("020443 88", "020443 88", "020443 88",
                                       "020443 88", "020443 88", "020443 88"))

  expect_equal(head(file5$uniqueID), c("022669 10", "022669 11", "022669 11",
                                       "022669 11", "022669 11", "022669 11"))
  expect_equal(tail(file5$uniqueID), c("023236 50", "023236 50", "023236 50",
                                       "023236 50", "023236 50", "023236 50"))
})

test_that("voter number in right order", {
  expect_equal(head(file1$voter_record), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file1$voter_record), c(3, 3, 3, 3, 3, 3))

  expect_equal(head(file2$voter_record), c(64, 64, 64, 64, 64, 64))
  expect_equal(tail(file2$voter_record), c(30, 30, 30, 30, 30, 30))

  expect_equal(head(file3$voter_record), c(51, 51, 52, 52, 52, 52))
  expect_equal(tail(file3$voter_record), c(36, 36, 36, 36, 36, 36))

  expect_equal(head(file4$voter_record), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file4$voter_record), c(88, 88, 88, 88, 88, 88))

  expect_equal(head(file5$voter_record), c(10, 11, 11, 11, 11, 11))
  expect_equal(tail(file5$voter_record), c(50, 50, 50, 50, 50, 50))
})



test_that("proper number of unique ID for a location and serial number match", {

  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022917"])), 3)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022883"])), 53)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022835"])), 58)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022721"])), 41)

  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021333"])), 30)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021317"])), 135)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021307"])), 118)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "020093"])), 124)

  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "021553"])), 36)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "021528"])), 46)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "021512"])), 122)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "021494"])), 55)

  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020443"])), 88)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020422"])), 70)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020412"])), 166)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "023418"])), 117)

  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "023236"])), 50)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "023214"])), 40)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "023135"])), 32)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "023115"])), 13)
})

test_that("unique voters have correct number of files recorded", {
  expect_equal(nrow(file1[file1$uniqueID == "022721 4",]), 2)
  expect_equal(nrow(file1[file1$uniqueID == "022721 14",]), 2)
  expect_equal(nrow(file1[file1$uniqueID == "022763 81",]), 2)
  expect_equal(nrow(file1[file1$uniqueID == "022763 82",]), 4)

  expect_equal(nrow(file2[file2$uniqueID == "020093 99",]), 2)
  expect_equal(nrow(file2[file2$uniqueID == "020093 105",]), 1)
  expect_equal(nrow(file2[file2$uniqueID == "020093 114",]), 2)
  expect_equal(nrow(file2[file2$uniqueID == "021173 35",]), 3)

  expect_equal(nrow(file3[file3$uniqueID == "021494 14",]), 2)
  expect_equal(nrow(file3[file3$uniqueID == "021494 20",]), 2)
  expect_equal(nrow(file3[file3$uniqueID == "021494 25",]), 1)
  expect_equal(nrow(file3[file3$uniqueID == "021494 26",]), 3)

  expect_equal(nrow(file4[file4$uniqueID == "020443 87",]), 1)
  expect_equal(nrow(file4[file4$uniqueID == "023418 30",]), 4)
  expect_equal(nrow(file4[file4$uniqueID == "023418 72",]), 1)
  expect_equal(nrow(file4[file4$uniqueID == "020412 139",]), 1)

  expect_equal(nrow(file5[file5$uniqueID == "023115 8",]), 4)
  expect_equal(nrow(file5[file5$uniqueID == "023115 13",]), 2)
  expect_equal(nrow(file5[file5$uniqueID == "023117 10",]), 3)
  expect_equal(nrow(file5[file5$uniqueID == "023118 11",]), 4)

})


test_that("Candidate has right name", {

  expect_equal(file1$candidate[file1$uniqueID == "022721 4"],
               c("Yes Si",
                 "Yes Si"))
  expect_equal(file1$candidate[file1$uniqueID == "022721 14"],
               c("Alan L Butkovitz",
                 "Danyl S Patterson"))
  expect_equal(file1$candidate[file1$uniqueID == "022763 81"],
               c("George Twardy",
                 "Michael W Untermeyer"))
  expect_equal(file1$candidate[file1$uniqueID == "022763 82"],
               c("Matt Wolf",
                 "George Twardy",
                 "Michael W Untermeyer",
                 "Daniel R Sulman"))

  expect_equal(file2$candidate[file2$uniqueID == "020093 99"],
               c("Yes Si",
                 "No No"))
  expect_equal(file2$candidate[file2$uniqueID == "020093 105"],
               c("Joe Khan"))
  expect_equal(file2$candidate[file2$uniqueID == "020093 114"],
               c("Joe Khan",
                 "Alan L Butkovitz"))
  expect_equal(file2$candidate[file2$uniqueID == "021173 35"],
               c("Dwayne Woodruff",
                 "Joe Khan",
                 "Alan L Butkovitz"))

  expect_equal(file3$candidate[file3$uniqueID == "021494 14"],
               c("Deborah Canty",
                 "David Conroy"))
  expect_equal(file3$candidate[file3$uniqueID == "021494 20"],
               c("No No",
                 "Yes Si"))
  expect_equal(file3$candidate[file3$uniqueID == "021494 25"],
               c("Deborah Canty"))
  expect_equal(file3$candidate[file3$uniqueID == "021494 26"],
               c("Joe Khan",
                 "Lynette M Bennett",
                 "Charlotte Cook"))

  expect_equal(file4$candidate[file4$uniqueID == "023418 30"],
               c("Stella Tsai",
                 "Lawrence J Bozzelli",
                 "Joe Khan",
                 "Rebecca Rhynhart"))
  expect_equal(file4$candidate[file4$uniqueID == "023418 50"],
               c("Alan L Butkovitz",
                 "Lawrence S Krasner",
                 "Yes Si",
                 "Yes Si"))
  expect_equal(file4$candidate[file4$uniqueID == "023418 72"],
               c("Yes Si"))
  expect_equal(file4$candidate[file4$uniqueID == "020412 139"],
               c("Lawrence S Krasner"))

  expect_equal(file5$candidate[file5$uniqueID == "023115 8"],
               c("Sallie Mundy",
                 "Emil Giordano",
                 "Vincent Furlong",
                 "Beth Grossman"))
  expect_equal(file5$candidate[file5$uniqueID == "023115 13"],
               c("Lawrence S Krasner",
                 "Write In"))
  expect_equal(file5$candidate[file5$uniqueID == "023117 10"],
               c("Joe Cosgrove",
                 "Matt Wolf",
                 "Rebecca Rhynhart"))
  expect_equal(file5$candidate[file5$uniqueID == "023118 11"],
               c("Daniel R Sulman",
                 "Zac Shaffer",
                 "Vincent Melchiorre",
                 "Jon Marshall"))
})


test_that("Categories are correct", {

  expect_equal(file1$category[file1$uniqueID == "022721 4"],
               c("Proposed Charter Change Question #1",
                 "Proposed Charter Change Question #2"))
  expect_equal(file1$category[file1$uniqueID == "022721 14"],
               c("City Controller - Democrat",
                 "Judge of the Court of Common Pleas - Democrat"))
  expect_equal(file1$category[file1$uniqueID == "022763 81"],
               c("Judge of the Municipal Court - Democrat",
                 "District Attorney - Democrat"))
  expect_equal(file1$category[file1$uniqueID == "022763 82"],
               c("Judge of the Municipal Court - Democrat",
                 "Judge of the Municipal Court - Democrat",
                 "District Attorney - Democrat",
                 "Judge of the Court of Common Pleas - Democrat"))

  expect_equal(file2$category[file2$uniqueID == "020093 99"],
               c("Proposed Charter Change Question #1",
                 "Proposed Charter Change Question #2"))
  expect_equal(file2$category[file2$uniqueID == "020093 105"],
               c("District Attorney - Democrat"))
  expect_equal(file2$category[file2$uniqueID == "020093 114"],
               c("District Attorney - Democrat",
                 "City Controller - Democrat"))
  expect_equal(file2$category[file2$uniqueID == "021173 35"],
               c("Justice of the Supreme Court - Democrat",
                 "District Attorney - Democrat",
                 "City Controller - Democrat"))

  expect_equal(file3$category[file3$uniqueID == "021494 14"],
               c("Judge of the Court of Common Pleas - Democrat",
                 "Judge of the Court of Common Pleas - Democrat"))
  expect_equal(file3$category[file3$uniqueID == "021494 20"],
               c("Proposed Charter Change Question #1",
                 "Proposed Charter Change Question #2"))
  expect_equal(file3$category[file3$uniqueID == "021494 25"],
               c("Judge of the Court of Common Pleas - Democrat"))
  expect_equal(file3$category[file3$uniqueID == "021494 26"],
               c("District Attorney - Democrat",
                 "Judge of Election - Democrat - 34-03",
                 "Inspector of Election - Democrat - 34-03"))

  expect_equal(file4$category[file4$uniqueID == "023418 30"],
               c("Judge of the Court of Common Pleas - Democrat",
                 "Judge of the Court of Common Pleas - Democrat",
                 "District Attorney - Democrat",
                 "City Controller - Democrat"))
  expect_equal(file4$category[file4$uniqueID == "023418 50"],
               c("City Controller - Democrat",
                 "District Attorney - Democrat",
                 "Proposed Charter Change Question #1",
                 "Proposed Charter Change Question #2"))
  expect_equal(file4$category[file4$uniqueID == "023418 72"],
               c("Proposed Charter Change Question #2"))
  expect_equal(file4$category[file4$uniqueID == "020412 139"],
               c("District Attorney - Democrat"))

  expect_equal(file5$category[file5$uniqueID == "023115 8"],
               c("Justice of the Supreme Court - Republican",
                 "Judge of the Superior Court - Republican",
                 "Judge of the Court of Common Pleas - Republican",
                 "District Attorney - Republican"))
  expect_equal(file5$category[file5$uniqueID == "023115 13"],
               c("District Attorney - Democrat",
                 "Judge of Election - Democrat - 62-09"))
  expect_equal(file5$category[file5$uniqueID == "023117 10"],
               c("Judge of the Commonwealth Court - Democrat",
                 "Judge of the Municipal Court - Democrat",
                 "City Controller - Democrat"))
  expect_equal(file5$category[file5$uniqueID == "023118 11"],
               c("Judge of the Court of Common Pleas - Democrat",
                 "Judge of the Court of Common Pleas - Democrat",
                 "Judge of the Court of Common Pleas - Democrat",
                 "Judge of the Court of Common Pleas - Democrat"))

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
#   expect_equal(unique(file1$votes), c(1, NA))
#   expect_equal(unique(file2$votes), c(1, NA))
#   expect_equal(unique(file3$votes), c(1, NA))
#   expect_equal(unique(file4$votes), c(1, NA))
#   expect_equal(unique(file5$votes), c(1, NA))
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

