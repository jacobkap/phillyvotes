context("election-2012-primary")

file1 <- system.file("data/PDF_data/2012 Primary",
                     "2012 Primary RTC 1 BIR 1 of 3.pdf",
                     package = "phillyvotes")
file2 <- system.file("data/PDF_data/2012 Primary",
                     "2012 Primary RTC 1 BIR 2 of 3.pdf",
                     package = "phillyvotes")
file3 <- system.file("data/PDF_data/2012 Primary",
                     "2012 Primary RTC 2 BIR 4 of 4.pdf",
                     package = "phillyvotes")
file4 <- system.file("data/PDF_data/2012 Primary",
                     "2012 Primary RTC 4 BIR 1 of 2.pdf",
                     package = "phillyvotes")
file5 <- system.file("data/PDF_data/2012 Primary",
                     "2012 Primary RTC BT1 BIR 1 of 4.pdf",
                     package = "phillyvotes")

file1 <- philly_votes(file1)
file2 <- philly_votes(file2)
file3 <- philly_votes(file3)
file4 <- philly_votes(file4)
file5 <- philly_votes(file5)

test_that("right file name", {
  expect_equal(unique(file1$file), "2012 Primary RTC 1 BIR 1 of 3")
  expect_equal(unique(file2$file), "2012 Primary RTC 1 BIR 2 of 3")
  expect_equal(unique(file3$file), "2012 Primary RTC 2 BIR 4 of 4")
  expect_equal(unique(file4$file), "2012 Primary RTC 4 BIR 1 of 2")
  expect_equal(unique(file5$file), "2012 Primary RTC BT1 BIR 1 of 4")
})


test_that("right number of pages", {
  expect_equal(max(file1$pdf_page), 3000)
  expect_equal(max(file2$pdf_page), 6000)
  expect_equal(max(file3$pdf_page), 12590)
  expect_equal(max(file4$pdf_page), 3000)
  expect_equal(max(file5$pdf_page), 3000)
})

test_that("unique voters have correct number of files recorded", {
  expect_equal(nrow(file1[file1$uniqueID == "020001 1",]), 3)
  expect_equal(nrow(file1[file1$uniqueID == "020001 2",]), 2)
  expect_equal(nrow(file1[file1$uniqueID == "021334 29",]), 13)
  expect_equal(nrow(file1[file1$uniqueID == "020094 17",]), 13)
  expect_equal(nrow(file2[file2$uniqueID == "021334 30",]), 4)
  expect_equal(nrow(file2[file2$uniqueID == "021334 31",]), 14)
  expect_equal(nrow(file2[file2$uniqueID == "021890 26",]), 1)
  expect_equal(nrow(file2[file2$uniqueID == "021890 25",]), 14)
  expect_equal(nrow(file3[file3$uniqueID == "022273 92",]), 15)
  expect_equal(nrow(file3[file3$uniqueID == "022273 94",]), 14)
  expect_equal(nrow(file3[file3$uniqueID == "021092 4",]), 2)
  expect_equal(nrow(file3[file3$uniqueID == "021092 2",]), 26)
  expect_equal(nrow(file4[file4$uniqueID == "020323 1",]), 3)
  expect_equal(nrow(file4[file4$uniqueID == "020323 2",]), 4)
  expect_equal(nrow(file4[file4$uniqueID == "020671 66",]), 11)
  expect_equal(nrow(file4[file4$uniqueID == "020370 21",]), 8)
  expect_equal(nrow(file5[file5$uniqueID == "020383 1",]), 10)
  expect_equal(nrow(file5[file5$uniqueID == "020404 84",]), 11)
  expect_equal(nrow(file5[file5$uniqueID == "020414 10",]), 16)
  expect_equal(nrow(file5[file5$uniqueID == "020461 85",]), 13)

})

# test_that("all votes are equal to 1", {
#   expect_true(unique(file1$votes) %in% c(1, NA))
#   expect_true(unique(file2$votes) %in% c(1, NA))
#   expect_true(unique(file3$votes) %in% c(1, NA))
#   expect_true(unique(file4$votes) %in% c(1, NA))
#   expect_true(unique(file5$votes) %in% c(1, NA))
# })


test_that("ballot ballot_position values are correct", {
  # First voter
  expect_equal(file1$ballot_position[1:3], c("11F", "18F", "21F"))
  # Second voter
  expect_equal(file1$ballot_position[4:5], c("10F", "21F"))
  # Last 6 votes
  expect_equal(tail(file1$ballot_position), c("21F", "32F", "33F", "34F", "35F", "36F"))

  # First voter
  expect_equal(file2$ballot_position[1:14], c("37F", "32G", "33G", "34G", "35G",
                                              "36G", "32H", "33H", "34H", "35H",
                                              "36H", "32I", "33I", "8L"))
  # Second voter
  expect_equal(file2$ballot_position[15:18], c("3F", "10F", "21F", "32F"))
  # Last 6 votes
  expect_equal(tail(file2$ballot_position), c("33F", "34F", "35F", "36F", "37F", "3F"))

  # First voter
  expect_equal(file3$ballot_position[1:15], c("6F", "10F", "11F", "13F", "15F",
                                              "18F", "21F", "32F", "34F", "36F",
                                              "37F", "33G", "34G", "32H", "33H"))
  # Second voter
  expect_equal(file3$ballot_position[16:32], c("3F", "7F", "9F", "11F", "13F",
                                               "15F", "18F", "22F", "32F", "34F",
                                               "35F", "36F", "37F", "35G", "35H",
                                               "36H", "33I"))
  # Last 6 votes
  expect_equal(tail(file3$ballot_position), c("29C", "30C", "24D", "29D", "4C", "8C"))

  # First voter
  expect_equal(file4$ballot_position[1:3], c("3F", "10F", "21F"))
  # Second voter
  expect_equal(file4$ballot_position[4:7], c("3F", "7F", "10F", "22F"))
  # Last 6 votes
  expect_equal(tail(file4$ballot_position), c("15F", "18F", "21F", "32F", "33F", "34F"))

  # First voter
  expect_equal(file5$ballot_position[1:10], c("3F", "7F", "10F", "11F", "13F",
                                              "15F", "32F", "35F", "34G", "35G"))
  # Second voter
  expect_equal(file5$ballot_position[11:23], c("3F", "6F", "9F", "11F", "13F",
                                               "15F", "22F", "32F", "33F", "34F",
                                               "35F", "36F", "37F"))
  # Last 6 votes
  expect_equal(tail(file5$ballot_position), c("32F", "33F", "34F", "35F", "36F", "37F"))
})

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

test_that("voter number in right order", {
  expect_equal(head(file1$voter_record), c(1, 1, 1, 2, 2, 3))
  expect_equal(tail(file1$voter_record), c(29, 29, 29, 29, 29, 29))

  expect_equal(head(file2$voter_record), c(29, 29, 29, 29, 29, 29))
  expect_equal(tail(file2$voter_record), c(25, 25, 25, 25, 25, 26))

  expect_equal(head(file3$voter_record), c(92, 92, 92, 92, 92, 92))
  expect_equal(tail(file3$voter_record), c(3, 3, 3, 3, 4, 4))

  expect_equal(head(file4$voter_record), c(1, 1, 1, 2, 2, 2))
  expect_equal(tail(file4$voter_record), c(66, 66, 66, 66, 66, 66))

  expect_equal(head(file5$voter_record), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file5$voter_record), c(85, 85, 85, 85, 85, 85))
})


test_that("Candidate has right name", {

  expect_equal(file1$candidate[file1$uniqueID == "020001 1"],
               c("Eugene A Depasquale",
                 "Lawrence M Farnese Jr",
                 "William F Keller"))
  expect_equal(file1$candidate[file1$uniqueID == "021330 70"],
               c("Barack Obama",
                 "Joseph John Vodvarka",
                 "Kathleen G Kane",
                 "Eugene A Depasquale",
                 "Robert M Mccord",
                 "Chaka Fattah",
                 "Jordan A Harris",
                 "Kenyatta J Johnson",
                 "Cindy Bass",
                 "Blondell Reynolds Brown",
                 "Jacquelyn Manns Smalley",
                 "Stephen A Cozen",
                 "Andrew P Toy",
                 "Heather Steinmiller"))
  expect_equal(file1$candidate[file1$uniqueID == "020001 2"],
               c("Patrick Murphy",
                 "William F Keller"))
  expect_equal(file1$candidate[file1$uniqueID == "020001 5"],
               c("Barack Obama",
                 "Bob Casey Jr",
                 "Patrick Murphy",
                 "Eugene A Depasquale",
                 "Robert M Mccord",
                 "Robert A Brady",
                 "Lawrence M Farnese Jr",
                 "William F Keller"))
  expect_equal(file1$candidate[file1$uniqueID == "020001 7"],
               c("Kathleen G Kane"))

  expect_equal(file2$candidate[file2$uniqueID == "021890 26"],
               c("Barack Obama"))
  expect_equal(file2$candidate[file2$uniqueID == "021334 30"],
               c("Barack Obama",
                 "Patrick Murphy",
                 "Jordan A Harris",
                 "Kenyatta J Johnson"))
  expect_equal(file2$candidate[file2$uniqueID == "021335 2"],
               c("David Alan Christian",
                 "David J Freed",
                 "John Maher",
                 "Diana Irey Vaughan",
                 "Robert Allen Mansfield Jr",
                 "Alfonso Gambone Jr",
                 "Ron Paul",
                 "Adam A Lang",
                 "Denise M Furey"))
  expect_equal(file2$candidate[file2$uniqueID == "021335 16"],
               c("Barack Obama",
                 "Bob Casey Jr",
                 "Patrick Murphy",
                 "Eugene A Depasquale",
                 "Robert M Mccord",
                 "Chaka Fattah",
                 "Harold James"))

  expect_equal(file3$candidate[file3$uniqueID == "021092 4"],
               c("Mitt Romney",
                 "Steven D Welch"))
  expect_equal(file3$candidate[file3$uniqueID == "022273 96"],
               c("Barack Obama",
                 "Chaka Fattah",
                 "Fatimah Loren Muhammad"))
  expect_equal(file3$candidate[file3$uniqueID == "022273 97"],
               c("Barack Obama",
                 "Joseph John Vodvarka",
                 "Eugene A Depasquale",
                 "Chaka Fattah",
                 "Vincent J Hughes",
                 "Fatimah Loren Muhammad"))
  expect_equal(file3$candidate[file3$uniqueID == "022568 21"],
               c("Barack Obama",
                 "Louise Bishop"))
  expect_equal(file4$candidate[file4$uniqueID == "020370 23"],
               c("Mitt Romney",
                 "Robert Allen Mansfield Jr",
                 "Alfonso Gambone Jr"))

  expect_equal(file4$candidate[file4$uniqueID == "020323 1"],
               c("Barack Obama", "Patrick Murphy", "Brian K Sims"))
  expect_equal(file4$candidate[file4$uniqueID == "020323 2"],
               c("Barack Obama",
                 "Bob Casey Jr",
                 "Patrick Murphy",
                 "Babette Josephs"))
  expect_equal(file4$candidate[file4$uniqueID == "020671 66"],
               c("Barack Obama",
                 "Bob Casey Jr",
                 "Patrick Murphy",
                 "Eugene A Depasquale",
                 "Robert M Mccord",
                 "Robert A Brady",
                 "Lawrence M Farnese Jr",
                 "Andrew J Kleeman",
                 "Ted Kirsch",
                 "Ryan N Boyer",
                 "Mindy M Posoff"))

  expect_equal(file5$candidate[file5$uniqueID == "020383 3"],
               c("Barack Obama",
                 "Chaka Fattah",
                 "Cherelle Lesley Parker",
                 "Joseph C Meade"))
  expect_equal(file5$candidate[file5$uniqueID == "020383 11"],
               c("Barack Obama",
                 "Joseph John Vodvarka",
                 "Patrick Murphy",
                 "Chaka Fattah",
                 "Cherelle Lesley Parker",
                 "Kenyatta J Johnson",
                 "Mark Alan Aronchick",
                 "Cindy Bass",
                 "Jacquelyn Manns Smalley",
                 "Jalond M Levin"))
  expect_equal(file5$candidate[file5$uniqueID == "020383 18"],
               c("Barack Obama",
                 "Kathleen G Kane",
                 "Eugene A Depasquale"))
  expect_equal(file5$candidate[file5$uniqueID == "020383 19"],
               c("Bob Casey Jr",
                 "Patrick Murphy",
                 "Kenyatta J Johnson",
                 "Cindy Bass",
                 "Blondell Reynolds Brown",
                 "Andrew P Toy",
                 "Jerome W Mondesire",
                 "Vincent J Hughes"))



  expect_false(all(grepl(",", file1$candidate)))
  expect_false(all(grepl(",", file2$candidate)))
  expect_false(all(grepl(",", file3$candidate)))
  expect_false(all(grepl(",", file4$candidate)))
  expect_false(all(grepl(",", file5$candidate)))

})


test_that("Categories are correct", {

  expect_equal(file1$category[file1$uniqueID == "020001 7"],
               c("Attorney General - Democrat"))
  expect_equal(file1$category[file1$uniqueID == "021330 70"],
               c("President of the United States - Democrat",
                 "United States Senator - Democrat",
                 "Attorney General - Democrat",
                 "Auditor General - Democrat",
                 "State Treasurer - Democrat",
                 "Representative in Congress - 2nd District - Democrat",
                 "Representative in the General Assembly - 186th District - Democrat",
                 "Delegate to the Democratic National Convention 2nd District",
                 "Delegate to the Democratic National Convention 2nd District",
                 "Delegate to the Democratic National Convention 2nd District",
                 "Delegate to the Democratic National Convention 2nd District",
                 "Delegate to the Democratic National Convention 2nd District",
                 "Delegate to the Democratic National Convention 2nd District",
                 "Delegate to the Democratic National Convention 2nd District"))
  expect_equal(file1$category[file1$uniqueID == "020001 1"],
               c("Auditor General - Democrat",
                 "Senator in the General Assembly - 1st District - Democrat",
                 "Representative in the General Assembly - 184th District - Democrat"))
  expect_equal(file1$category[file1$uniqueID == "020001 2"],
               c("Attorney General - Democrat",
                 "Representative in the General Assembly - 184th District - Democrat"))
  expect_equal(file1$category[file1$uniqueID == "020001 7"],
               c("Attorney General - Democrat"))

  expect_equal(file2$category[file2$uniqueID == "021334 30"],
               c("President of the United States - Democrat",
                 "Attorney General - Democrat",
                 "Representative in the General Assembly - 186th District - Democrat",
                 "Delegate to the Democratic National Convention 2nd District"))
  expect_equal(file2$category[file2$uniqueID == "021890 23"],
               c("President of the United States - Republican",
                 "United States Senator - Republican",
                 "Senator in the General Assembly - 1st District - Republican",
                 "Delegate to the Republican National Convention 1st District",
                 "Alternate Delegate to the Republican National Convention 1"))
  expect_equal(file2$category[file2$uniqueID == "021890 24"],
               c("United States Senator - Democrat",
                 "Attorney General - Democrat",
                 "Auditor General - Democrat",
                 "State Treasurer - Democrat",
                 "Representative in Congress - 1st District - Democrat",
                 "Senator in the General Assembly - 1st District - Democrat",
                 "Representative in the General Assembly - 184th District - Democrat"))
  expect_equal(file2$category[file2$uniqueID == "021890 26"],
               c("President of the United States - Democrat"))

  expect_equal(file3$category[file3$uniqueID == "022568 21"],
               c("President of the United States - Democrat",
                 "Representative in the General Assembly - 192nd District - Democrat"))
  expect_equal(file3$category[file3$uniqueID == "021092 4"],
               c("President of the United States - Republican",
                 "United States Senator - Republican"))
  expect_equal(file3$category[file3$uniqueID == "022273 96"],
               c("President of the United States - Democrat",
                 "Representative in Congress - 2nd District - Democrat",
                 "Representative in the General Assembly - 188th District - Democrat"))
  expect_equal(file3$category[file3$uniqueID == "022273 102"],
               c("President of the United States - Democrat",
                 "United States Senator - Democrat",
                 "Attorney General - Democrat",
                 "Representative in Congress - 2nd District - Democrat",
                 "Senator in the General Assembly - 7th District - Democrat",
                 "Representative in the General Assembly - 188th District - Democrat"))

  expect_equal(file4$category[file4$uniqueID == "020323 1"],
               c("President of the United States - Democrat",
                 "Attorney General - Democrat",
                 "Representative in the General Assembly - 182nd District - Democrat"))
  expect_equal(file4$category[file4$uniqueID == "020323 2"],
               c("President of the United States - Democrat",
                 "United States Senator - Democrat",
                 "Attorney General - Democrat",
                 "Representative in the General Assembly - 182nd District - Democrat"))
  expect_equal(file4$category[file4$uniqueID == "020644 2"],
               c("United States Senator - Democrat",
                 "Attorney General - Democrat",
                 "Representative in the General Assembly - 195th District - Democrat",
                 "Delegate to the Democratic National Convention 1st District"))
  expect_equal(file4$category[file4$uniqueID == "020644 9"],
               c("President of the United States - Republican",
                 "United States Senator - Republican",
                 "Attorney General - Republican",
                 "Auditor General - Republican",
                 "State Treasurer - Republican",
                 "Representative in Congress - 1st District - Republican",
                 "Senator in the General Assembly - 1st District - Republican"))

  expect_equal(file5$category[file5$uniqueID == "020383 18"],
               c("President of the United States - Democrat",
                 "Attorney General - Democrat",
                 "Auditor General - Democrat"))
  expect_equal(file5$category[file5$uniqueID == "020383 3"],
               c("President of the United States - Democrat",
                 "Representative in Congress - 2nd District - Democrat",
                 "Representative in the General Assembly - 200th District - Democrat",
                 "Delegate to the Democratic National Convention 2nd District"))
  expect_equal(file5$category[file5$uniqueID == "020383 18"],
               c("President of the United States - Democrat",
                 "Attorney General - Democrat",
                 "Auditor General - Democrat"))
  expect_equal(file5$category[file5$uniqueID == "020461 84"],
               c("President of the United States - Democrat",
                 "United States Senator - Democrat",
                 "Attorney General - Democrat",
                 "Representative in the General Assembly - 203rd District - Democrat"))

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


test_that("proper number of unique ID for a location and serial number match", {

  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "021334"])), 29)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "021319"])), 49)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "021148"])), 30)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020084"])), 45)

  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021890"])), 26)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021831"])), 63)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021691"])), 91)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021657"])), 58)

  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "021092"])), 4)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "023008"])), 53)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "022559"])), 81)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "022542"])), 106)

  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020671"])), 66)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020642"])), 76)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020638"])), 99)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020335"])), 105)

  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020461"])), 85)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020443"])), 129)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020412"])), 93)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020383"])), 103)
})


test_that("unique IDs in right order", {
  expect_equal(head(file1$uniqueID), c("020001 1", "020001 1", "020001 1",
                                       "020001 2", "020001 2", "020001 3"))
  expect_equal(tail(file1$uniqueID), c("021334 29", "021334 29", "021334 29",
                                       "021334 29", "021334 29", "021334 29"))

  expect_equal(head(file2$uniqueID), c("021334 29", "021334 29", "021334 29",
                                       "021334 29", "021334 29", "021334 29"))
  expect_equal(tail(file2$uniqueID), c("021890 25", "021890 25", "021890 25",
                                       "021890 25", "021890 25", "021890 26"))

  expect_equal(head(file3$uniqueID), c("022273 92", "022273 92", "022273 92",
                                       "022273 92", "022273 92", "022273 92"))
  expect_equal(tail(file3$uniqueID), c("021092 3", "021092 3", "021092 3",
                                       "021092 3", "021092 4", "021092 4"))

  expect_equal(head(file4$uniqueID), c("020323 1", "020323 1", "020323 1",
                                       "020323 2", "020323 2", "020323 2"))
  expect_equal(tail(file4$uniqueID), c("020671 66", "020671 66", "020671 66",
                                       "020671 66", "020671 66", "020671 66"))

  expect_equal(head(file5$uniqueID), c("020383 1", "020383 1", "020383 1",
                                       "020383 1", "020383 1", "020383 1"))
  expect_equal(tail(file5$uniqueID), c("020461 85", "020461 85", "020461 85",
                                       "020461 85", "020461 85", "020461 85"))
})
