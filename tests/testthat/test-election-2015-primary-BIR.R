context("election-2015-primary")

file1 <- system.file("data/PDF_data/2015 Primary",
                     "2015 Primary RTC 1 BIR 4 of 8.pdf",
                     package = "phillyvotes")
file2 <- system.file("data/PDF_data/2015 Primary",
                     "2015 Primary RTC 1 BIR 5 of 8.pdf",
                     package = "phillyvotes")
file3 <- system.file("data/PDF_data/2015 Primary",
                     "2015 Primary RTC 3 BIR 3 of 5.pdf",
                     package = "phillyvotes")
file4 <- system.file("data/PDF_data/2015 Primary",
                     "2015 Primary RTC BT BIR 4 of 12.pdf",
                     package = "phillyvotes")
file5 <- system.file("data/PDF_data/2015 Primary",
                     "2015 Primary RTC CT BIR 1 of 3.pdf",
                     package = "phillyvotes")

file1 <- philly_votes(file1)
file2 <- philly_votes(file2)
file3 <- philly_votes(file3)
file4 <- philly_votes(file4)
file5 <- philly_votes(file5)

test_that("right file name", {
  expect_equal(unique(file1$file), "2015 Primary RTC 1 BIR 4 of 8")
  expect_equal(unique(file2$file), "2015 Primary RTC 1 BIR 5 of 8")
  expect_equal(unique(file3$file), "2015 Primary RTC 3 BIR 3 of 5")
  expect_equal(unique(file4$file), "2015 Primary RTC BT BIR 4 of 12")
  expect_equal(unique(file5$file), "2015 Primary RTC CT BIR 1 of 3")
})


test_that("right number of pages", {
  expect_equal(max(file1$pdf_page), 12000)
  expect_equal(max(file2$pdf_page), 15000)
  expect_equal(max(file3$pdf_page), 9000)
  expect_equal(max(file4$pdf_page), 12000)
  expect_equal(max(file5$pdf_page), 3000)
})




test_that("ballot ballot_position values are correct", {
  # First 6 votes
  expect_equal(head(file1$ballot_position), c("2C", "3C", "5C", "7C", "9C", "11C"))
  # Last 6 votes
  expect_equal(tail(file1$ballot_position), c("13H", "12I", "14I", "41I", "41K", "2D"))

  # First 6 votes
  expect_equal(head(file2$ballot_position), c("19D", "42E", "42G", "42I", "42K", "3C"))
  # Last 6 votes
  expect_equal(tail(file2$ballot_position), c("29G", "41G", "10H", "12H", "41I", "41K"))

  # First 6 votes
  expect_equal(head(file3$ballot_position), c("14H", "28H", "11I", "40B", "2C", "5C"))
  # Last 6 votes
  expect_equal(tail(file3$ballot_position), c("29G", "41G", "28H", "29H", "42I", "41K"))

  # First 6 votes
  expect_equal(head(file4$ballot_position), c("26C", "28C", "31C", "28D", "41E", "14F"))
  # Last 6 votes
  expect_equal(tail(file4$ballot_position), c("16C", "17C", "24C", "26C", "31C", "16D"))

  # First 6 votes
  expect_equal(head(file5$ballot_position), c("3C", "24C", "26C", "31C", "19D", "2E"))
  # Last 6 votes
  expect_equal(tail(file5$ballot_position), c("19C", "28C", "2D", "28D", "41E", "41G"))

})

test_that("unique IDs in right order", {
  expect_equal(head(file1$uniqueID), c("021645 23", "021645 23", "021645 23",
                                       "021645 23", "021645 23", "021645 23"))
  expect_equal(tail(file1$uniqueID), c("021811 1", "021811 1", "021811 1",
                                       "021811 1", "021811 1", "021811 2"))

  expect_equal(head(file2$uniqueID), c("021811 2", "021811 2", "021811 2",
                                       "021811 2", "021811 2", "021811 3"))
  expect_equal(tail(file2$uniqueID), c("021891 37", "021891 37", "021891 37",
                                       "021891 37", "021891 37", "021891 37"))

  expect_equal(head(file3$uniqueID), c("022893 29", "022893 29", "022893 29",
                                       "022893 30", "022893 30", "022893 30"))
  expect_equal(tail(file3$uniqueID), c("023171 128", "023171 128", "023171 128",
                                       "023171 128", "023171 128", "023171 128"))

  expect_equal(head(file4$uniqueID), c("020551 52", "020551 52", "020551 52",
                                       "020551 52", "020551 52", "020551 52"))
  expect_equal(tail(file4$uniqueID), c("020718 43", "020718 43", "020718 43",
                                       "020718 43", "020718 43", "020718 43"))

  expect_equal(head(file5$uniqueID), c("020183 1", "020183 1", "020183 1",
                                       "020183 1", "020183 1", "020183 1"))
  expect_equal(tail(file5$uniqueID), c("023394 74", "023394 74", "023394 74",
                                       "023394 74", "023394 74", "023394 74"))
})




test_that("proper number of unique ID for a location and serial number match", {

  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "021811"])), 2)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "021709"])), 106)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "021693"])), 54)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "021667"])), 70)

  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021891"])), 37)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021876"])), 71)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021832"])), 161)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021811"])), 59)

  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "023171"])), 128)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "023157"])), 89)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "022915"])), 86)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "022899"])), 89)

  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020718"])), 43)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020713"])), 101)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020581"])), 88)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020564"])), 77)

  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "023394"])), 74)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020218"])), 120)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020209"])), 72)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020183"])), 118)
})

test_that("unique voters have correct number of files recorded", {
  expect_equal(nrow(file1[file1$uniqueID == "021811 2",]), 1)
  expect_equal(nrow(file1[file1$uniqueID == "021709 50",]), 2)
  expect_equal(nrow(file1[file1$uniqueID == "021709 45",]), 4)
  expect_equal(nrow(file1[file1$uniqueID == "021667 38",]), 2)

  expect_equal(nrow(file2[file2$uniqueID == "021891 36",]), 4)
  expect_equal(nrow(file2[file2$uniqueID == "021876 26",]), 4)
  expect_equal(nrow(file2[file2$uniqueID == "021876 21",]), 13)
  expect_equal(nrow(file2[file2$uniqueID == "021811 2",]), 5)

  expect_equal(nrow(file3[file3$uniqueID == "023171 127",]), 7)
  expect_equal(nrow(file3[file3$uniqueID == "023157 37",]), 9)
  expect_equal(nrow(file3[file3$uniqueID == "023157 30",]), 8)
  expect_equal(nrow(file3[file3$uniqueID == "022915 64",]), 4)

  expect_equal(nrow(file4[file4$uniqueID == "020718 43",]), 11)
  expect_equal(nrow(file4[file4$uniqueID == "020713 29",]), 7)
  expect_equal(nrow(file4[file4$uniqueID == "020713 30",]), 11)
  expect_equal(nrow(file4[file4$uniqueID == "020581 36",]), 3)

  expect_equal(nrow(file5[file5$uniqueID == "023394 74",]), 7)
  expect_equal(nrow(file5[file5$uniqueID == "020218 49",]), 24)
  expect_equal(nrow(file5[file5$uniqueID == "020209 54",]), 7)
  expect_equal(nrow(file5[file5$uniqueID == "020183 1",]), 11)

})

test_that("voter number in right order", {
  expect_equal(head(file1$voter_record), c(23, 23, 23, 23, 23, 23))
  expect_equal(tail(file1$voter_record), c(1, 1, 1, 1, 1, 2))

  expect_equal(head(file2$voter_record), c(2, 2, 2, 2, 2, 3))
  expect_equal(tail(file2$voter_record), c(37, 37, 37, 37, 37, 37))

  expect_equal(head(file3$voter_record), c(29, 29, 29, 30, 30, 30))
  expect_equal(tail(file3$voter_record), c(128, 128, 128, 128, 128, 128))

  expect_equal(head(file4$voter_record), c(52, 52, 52, 52, 52, 52))
  expect_equal(tail(file4$voter_record), c(43, 43, 43, 43, 43, 43))

  expect_equal(head(file5$voter_record), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file5$voter_record), c(74, 74, 74, 74, 74, 74))
})


test_that("Candidate has right name", {

  expect_equal(file1$candidate[file1$uniqueID == "021811 2"],
               c("Kevin M Dougherty"))
  expect_equal(file1$candidate[file1$uniqueID == "021645 29"],
               c("Kevin M Dougherty"))
  expect_equal(file1$candidate[file1$uniqueID == "021645 31"],
               c("Christine Donohue",
                 "Anthony Hardy Williams",
                 "Kenyatta Johnson"))
  expect_equal(file1$candidate[file1$uniqueID == "021645 33"],
               c("Kenyatta Johnson"))

  expect_equal(file2$candidate[file2$uniqueID == "021811 4"],
               c("Mark F Squilla",
                 "Kevin M Dougherty",
                 "Jim Kenney"))
  expect_equal(file2$candidate[file2$uniqueID == "021811 8"],
               c("Anthony Hardy Williams",
                 "Kevin M Dougherty"))
  expect_equal(file2$candidate[file2$uniqueID == "021837 12"],
               c("Melissa Murray Bailey",
                 "Ross Feinberg"))
  expect_equal(file2$candidate[file2$uniqueID == "021837 20"],
               c("Jodi Lobel"))

  expect_equal(file3$candidate[file3$uniqueID == "022893 29"],
               c("William J Ciancaglini",
                 "Ed Neilson",
                 "Chris Mccabe"))
  expect_equal(file3$candidate[file3$uniqueID == "022893 31"],
               c("Jim Kenney"))
  expect_equal(file3$candidate[file3$uniqueID == "022893 36"],
               c("Kevin M Dougherty", "Jim Kenney"))
  expect_equal(file3$candidate[file3$uniqueID == "022904 63"],
               c("David Oh", "Daniel Tinney",
                 "Al Taubenberger"))
  expect_equal(file3$candidate[file3$uniqueID == "022893 61"],
               c("Timothy Dailey",
                 "David Wecht",
                 "Christine Donohue",
                 "Alice Beck Dubow",
                 "Christine Hope",
                 "Jim Kenney",
                 "Anne E Lazarus",
                 "Lisa M Deeley",
                 "No No",
                 "Sherrie Cohen",
                 "Yes Si",
                 "Chris Mallios",
                 "Ed Neilson",
                 "No No",
                 "No No"))

  expect_equal(file4$candidate[file4$uniqueID == "020551 54"],
               c("Anthony Hardy Williams"))
  expect_equal(file4$candidate[file4$uniqueID == "020551 55"],
               c("Yes Si", "Yes Si", "Yes Si", "Yes Si"))
  expect_equal(file4$candidate[file4$uniqueID == "020577 2"],
               c("Anthony Hardy Williams"))
  expect_equal(file4$candidate[file4$uniqueID == "020577 3"],
               c("Anthony Hardy Williams"))

  expect_equal(file5$candidate[file5$uniqueID == "020183 17"],
               c("Jim Kenney", "Allan Domb"))
  expect_equal(file5$candidate[file5$uniqueID == "020183 39"],
               c("Jewell Williams",
                 "Jim Kenney",
                 "Lisa M Deeley"))
  expect_equal(file5$candidate[file5$uniqueID == "020189 13"],
               c("Lynne M Abraham"))
  expect_equal(file5$candidate[file5$uniqueID == "020189 31"],
               c("Cheryl Allen", "Judy Olson"))
})


test_that("Categories are correct", {

  expect_equal(file1$category[file1$uniqueID == "021811 2"],
               c("Justice of the Supreme Court - Democrat"))
  expect_equal(file1$category[file1$uniqueID == "021645 29"],
               c("Justice of the Supreme Court - Democrat"))
  expect_equal(file1$category[file1$uniqueID == "021645 31"],
               c("Justice of the Supreme Court - Democrat",
                 "Mayor - Democrat",
                 "District Council - 2nd District - Democrat"))
  expect_equal(file1$category[file1$uniqueID == "021645 33"],
               c("District Council - 2nd District - Democrat"))

  expect_equal(file2$category[file2$uniqueID == "021811 4"],
               c("District Council - 1st District - Democrat",
                 "Justice of the Supreme Court - Democrat",
                 "Mayor - Democrat"))
  expect_equal(file2$category[file2$uniqueID == "021811 8"],
               c("Mayor - Democrat",
                 "Justice of the Supreme Court - Democrat"))
  expect_equal(file2$category[file2$uniqueID == "021837 12"],
               c("Mayor - Republican",
                 "Register of Wills - Republican"))
  expect_equal(file2$category[file2$uniqueID == "021837 20"],
               c("Judge of the Court of Common Pleas - Democrat"))

  expect_equal(file3$category[file3$uniqueID == "022893 29"],
               c("Judge of the Court of Common Pleas - Democrat",
                 "Council at Large - Democrat",
                 "Judge of the Court of Common Pleas - Democrat"))
  expect_equal(file3$category[file3$uniqueID == "022893 31"],
               c("Mayor - Democrat"))
  expect_equal(file3$category[file3$uniqueID == "022893 36"],
               c("Justice of the Supreme Court - Democrat",
                 "Mayor - Democrat"))
  expect_equal(file3$category[file3$uniqueID == "022904 63"],
               c("Council at Large - Republican",
                 "Council at Large - Republican",
                 "Council at Large - Republican"))
  expect_equal(file3$category[file3$uniqueID == "022893 61"],
               c("Special Election - Senator in the General Assembly - 5th District",
                 "Justice of the Supreme Court - Democrat",
                 "Justice of the Supreme Court - Democrat",
                 "Judge of the Superior Court - Democrat",
                 "Judge of the Municipal Court - Democrat",
                 "Mayor - Democrat",
                 "Justice of the Supreme Court - Democrat",
                 "City Commissioners - Democrat",
                 "Proposed Charter Change Question #1",
                 "Council at Large - Democrat",
                 "Proposed Charter Change Question #2",
                 "Judge of the Court of Common Pleas - Democrat",
                 "Council at Large - Democrat",
                 "Proposed Charter Change Question #3",
                 "Proposed Charter Change Question #4"))

  expect_equal(file4$category[file4$uniqueID == "020551 54"],
               c("Mayor - Democrat"))
  expect_equal(file4$category[file4$uniqueID == "020551 55"],
               c("Proposed Charter Change Question #1",
                 "Proposed Charter Change Question #2",
                 "Proposed Charter Change Question #3",
                 "Proposed Charter Change Question #4"))
  expect_equal(file4$category[file4$uniqueID == "020577 2"],
               c("Mayor - Democrat"))
  expect_equal(file4$category[file4$uniqueID == "020577 3"],
               c("Mayor - Democrat"))

  expect_equal(file5$category[file5$uniqueID == "020183 17"],
               c("Mayor - Democrat",
                 "Council at Large - Democrat"))
  expect_equal(file5$category[file5$uniqueID == "020183 39"],
               c("Sheriff - Democrat", "Mayor - Democrat",
                 "City Commissioners - Democrat"))
  expect_equal(file5$category[file5$uniqueID == "020189 13"],
               c("Mayor - Democrat"))
  expect_equal(file5$category[file5$uniqueID == "020189 31"],
               c("Justice of the Supreme Court - Republican",
                 "Justice of the Supreme Court - Republican"))

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
    expect_true(unique(file1$ballot_position[file1$candidate == "No Vote"]) %in%
                 c("43A", "39L"))
  }
  if (length(unique(file2$ballot_position[file2$candidate == "No Vote"])) > 0) {
    expect_true(unique(file2$ballot_position[file2$candidate == "No Vote"]) %in%
                 c("43A", "39L"))
  }
  if (length(unique(file3$ballot_position[file3$candidate == "No Vote"])) > 0) {
    expect_true(unique(file3$ballot_position[file3$candidate == "No Vote"]) %in%
                 c("43A", "39L"))
  }
  if (length(unique(file4$ballot_position[file4$candidate == "No Vote"])) > 0) {
    expect_true(unique(file4$ballot_position[file4$candidate == "No Vote"]) %in%
                 c("43A", "39L"))
  }
  if (length(unique(file5$ballot_position[file5$candidate == "No Vote"])) > 0) {
    expect_true(unique(file5$ballot_position[file5$candidate == "No Vote"]) %in%
                 c("43A", "39L"))
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
