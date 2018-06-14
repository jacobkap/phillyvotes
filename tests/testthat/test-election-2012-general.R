context("election-2012-general")

file1 <- system.file("data/PDF_data/2012 General",
                     "2012 General RTC 1 BIR 1 of 11.pdf",
                     package = "phillyfile")
file2 <- system.file("data/PDF_data/2012 General",
                     "2012 General RTC 1 BIR 9 of 11.pdf",
                     package = "phillyfile")
file3 <- system.file("data/PDF_data/2012 General",
                     "2012 General RTC 3 BIR 1 of 10.pdf",
                     package = "phillyfile")
file4 <- system.file("data/PDF_data/2012 General",
                     "2012 General RTC 4 BIR 4 of 7.pdf",
                     package = "phillyfile")
file5 <- system.file("data/PDF_data/2012 General",
                     "2012 General RTC BT2 BIR 1 of 10.pdf",
                     package = "phillyfile")

file1 <- philly_file(file1)
file2 <- philly_file(file2)
file3 <- philly_file(file3)
file4 <- philly_file(file4)
file5 <- philly_file(file5)

test_that("right file name", {
  expect_equal(unique(file1$file), "2012 General RTC 1 BIR 1 of 11")
  expect_equal(unique(file2$file), "2012 General RTC 1 BIR 9 of 11")
  expect_equal(unique(file3$file), "2012 General RTC 3 BIR 1 of 10")
  expect_equal(unique(file4$file), "2012 General RTC 4 BIR 4 of 7")
  expect_equal(unique(file5$file), "2012 General RTC BT2 BIR 1 of 10")
})


test_that("right number of pages", {
  expect_equal(max(file1$pdf_page), 3000)
  expect_equal(max(file2$pdf_page), 27000)
  expect_equal(max(file3$pdf_page), 3000)
  expect_equal(max(file4$pdf_page), 12000)
  expect_equal(max(file5$pdf_page), 3000)
})

test_that("unique voters have correct number of files recorded", {
  expect_equal(nrow(file1[file1$uniqueID == "020054 177",]), 7)
  expect_equal(nrow(file1[file1$uniqueID == "020054 178",]), 12)
  expect_equal(nrow(file1[file1$uniqueID == "020001 1",]), 12)
  expect_equal(nrow(file1[file1$uniqueID == "020008 37",]), 12)
  expect_equal(nrow(file2[file2$uniqueID == "022324 55",]), 7)
  expect_equal(nrow(file2[file2$uniqueID == "022324 59",]), 7)
  expect_equal(nrow(file2[file2$uniqueID == "021957 115",]), 7)
  expect_equal(nrow(file2[file2$uniqueID == "021957 163",]), 7)
  expect_equal(nrow(file3[file3$uniqueID == "022775 70",]), 12)
  expect_equal(nrow(file3[file3$uniqueID == "022775 71",]), 12)
  expect_equal(nrow(file3[file3$uniqueID == "022747 39",]), 12)
  expect_equal(nrow(file3[file3$uniqueID == "022747 37",]), 8)
  expect_equal(nrow(file4[file4$uniqueID == "021236 70",]), 5)
  expect_equal(nrow(file4[file4$uniqueID == "021236 63",]), 2)
  expect_equal(nrow(file4[file4$uniqueID == "021236 57",]), 8)
  expect_equal(nrow(file4[file4$uniqueID == "021236 55",]), 8)
  expect_equal(nrow(file5[file5$uniqueID == "020418 46",]), 4)
  expect_equal(nrow(file5[file5$uniqueID == "020409 128",]), 6)
  expect_equal(nrow(file5[file5$uniqueID == "020393 121",]), 11)
  expect_equal(nrow(file5[file5$uniqueID == "020383 1",]), 11)

})

test_that("all votes are equal to 1", {
  expect_equal(unique(file1$votes), c(1, NA))
  expect_equal(unique(file2$votes), c(1, NA))
  expect_equal(unique(file3$votes), c(1, NA))
  expect_equal(unique(file4$votes), c(1, NA))
  expect_equal(unique(file5$votes), c(1, NA))
})


test_that("ballot ballot_position values are correct", {
  # First voter
  expect_equal(file1$ballot_position[1:12], c("8C", "9D", "11D", "13D", "15D",
                                              "17D", "20D", "23D", "11L", "20L",
                                              "28L", "37L"))
  # Second voter
  expect_equal(file1$ballot_position[13:22], c("9C", "11C", "8D", "17D", "20D",
                                               "23D", "11L", "20L", "29L", "37L"))
  # Last 6 votes
  expect_equal(tail(file1$ballot_position), c("8D", "9D", "11D", "13D", "15D", "17D"))

  # First voter
  expect_equal(file2$ballot_position[1:7], c("8D", "9D", "11D", "13D", "15D",
                                             "17D", "23D"))
  # Second voter
  expect_equal(file2$ballot_position[8:14], c("8D", "9D", "11D", "13D", "15D", "17D",
                                              "23D"))
  # Last 6 votes
  expect_equal(tail(file2$ballot_position), c("17D", "23D", "8D", "9D", "11D", "13D"))

  # First voter
  expect_equal(file3$ballot_position[1:2], c("8C", "17C"))
  # Second voter
  expect_equal(file3$ballot_position[3:14], c("8C", "9C", "11C", "13C", "15C", "17C",
                                              "20C", "23D", "12L", "21L", "29L", "37L"))
  # Last 6 votes
  expect_equal(tail(file3$ballot_position), c("12L", "20L", "29L", "36L", "8D", "9D"))

  # First voter
  expect_equal(file4$ballot_position[1:1], c("23D"))
  # Second voter
  expect_equal(file4$ballot_position[2:8], c("8D", "11D", "13D", "15D", "17D", "20D",
                                             "23D"))
  # Last 6 votes
  expect_equal(tail(file4$ballot_position), c("20D", "23D", "11L", "20L", "28L", "36L"))

  # First voter
  expect_equal(file5$ballot_position[1:11], c("8D", "9D", "11D", "13D", "15D", "17D",
                                              "23D", "11L", "20L", "28L", "36L"))
  # Second voter
  expect_equal(file5$ballot_position[12:22], c("8D", "9D", "11D", "13D", "15D",
                                               "17D", "23D", "12L", "20L", "29L",
                                               "36L"))
  # Last 6 votes
  expect_equal(tail(file5$ballot_position), c("17D", "23D", "11L", "20L", "28L", "36L"))
})

test_that("Ballots with No Vote all have position 43A", {
  expect_equal(unique(file1$ballot_position[file1$candidate == "No Vote"]),
               "43A")
  expect_equal(unique(file2$ballot_position[file2$candidate == "No Vote"]),
               "43A")
  expect_equal(unique(file3$ballot_position[file3$candidate == "No Vote"]),
               "43A")
  expect_equal(unique(file4$ballot_position[file4$candidate == "No Vote"]),
               "43A")
  expect_equal(unique(file5$ballot_position[file5$candidate == "No Vote"]),
               "43A")
})


test_that("Candidate has right name", {

  expect_equal(file1$candidate[file1$uniqueID == "020054 178", ],
               c("Barack Obama", "Bob Casey Jr", "Kathleen G Kane",
                 "Eugene A Depasquale", "Robert M Mccord", "Robert A Brady"))
  expect_equal(file1$candidate[file1$uniqueID == "020001 1", ],
               c("Mitt Romney", "Bob Casey Jr", "Kathleen G Kane",
                 "Eugene A Depasquale", "Robert M Mccord", "Robert A Bradey",
                 "Lawrence M Farnese Jr", "Yes Si", "Yes Si",
                 "Yes Si", "No No"))
  expect_equal(file1$candidate[file1$uniqueID == "020001 2", ],
               c("Tom Smith", "David J Freed", "Barack Obama",
                 "Robert A Brady", "Lawrence M Farnese Jr",
                 "William F Keller", "Yes Si", "Yes Si",
                 "No No"))
  expect_equal(file1$candidate[file1$uniqueID == "020025 65", ],
               c("Barack Obama", "Bob Casey Jr", "Kathleen G Kane",
                 "Eugene A Depasquale", "Robert M Mccord",
                 "Robert A Brady", "Lawrence M Farnese Jr",
                 "William F Keller", "No No", "Yes Si",
                 "Yes Si", "Yes Si"))
  expect_equal(file2$candidate[file2$uniqueID == "022324 59", ],
               c("Barack Obama", "Bob Casey Jr", "Kathleen G Kane",
                 "Eugene A Depasquale"))
  expect_equal(file2$candidate[file2$uniqueID == "022324 57", ],
               c("Mitt Romney", "Bob Casey Jr", "Kathleen G Kane",
                 "Eugene A Depasquale", "Jordan A Harris"))
  expect_equal(file2$candidate[file2$uniqueID == "021957 40", ],
               c("Ronald G Waters"))
  expect_equal(file2$candidate[file2$uniqueID == "021984 77", ],
               c("Barack Obama", "Bob Casey Jr", "Kathleen G Kane",
                 "Eugene A Depasquale", "Robert M Mccord",
                 "Robert A Brady", "Maria P Donatucci"))
  expect_equal(file3$candidate[file3$uniqueID == "022775 71", ],
               c("Barack Obama", "Bob Casey Jr"))
  expect_equal(file3$candidate[file3$uniqueID == "022721 1", ],
               c("Mitt Romney", "Joseph James Rooney"))
  expect_equal(file3$candidate[file3$uniqueID == "022721 11", ],
               c("Barack Obama", "Bob Casey Jr", "Kathleen G Kane",
                 "Eugene A Depasquale", "Robert M Mccord",
                 "Allyson Y Schwartz", "Mike Stack", "John P Sabatina Jr",
                 "Yes Si", "Yes Si", "Yes Si", "Yes Si"))
  expect_equal(file3$candidate[file3$uniqueID == "022763 107", ],
               c("Mitt Romney", "Tom Smith", "David J Freed",
                 "John Maher", "Diana Irey Vaughan",
                 "Joseph James Rooney", "Michael J Tomlinson",
                 "John P Sabatina Jr"))
  expect_equal(file4$candidate[file4$uniqueID == "020676 148", ],
               c("J P Miranda"))
  expect_equal(file4$candidate[file4$uniqueID == "020676 155", ],
               c("Barack Obama", "Bob Casey Jr", "Kathleen G Kane",
                 "Eugene A Depasquale", "Robert M Mccord",
                 "Chaka Fattah", "Shirley M Kitchen", "J P Miranda"))
  expect_equal(file4$candidate[file4$uniqueID == "021236 71", ],
               c("Barack Obama", "Bob Casey Jr", "Kathleen G Kane",
                 "Eugene A Depasquale", "Robert M Mccord", "Chaka Fattah",
                 "Vincent K Hughes", "Vanessa L Brown",
                 "Yes Si", "Yes Si", "Yes Si", "Yes Si"))
  expect_equal(file4$candidate[file4$uniqueID == "021236 70", ],
               c("Barack Obama", "Yes Si", "No No", "Yes Si", "Yes Si"))
  expect_equal(file5$candidate[file5$uniqueID == "020418 47", ],
               c("Barack Obama", "Bob Casey Jr", "Kathleen G Kane",
                 "Eugene A Depasquale", "Robert M Mccord", "Chaka Fattah",
                 "Dwight Evans", "Yes Si", "Yes Si", "Yes Si", "Yes Si"))
  expect_equal(file5$candidate[file5$uniqueID == "020418 46", ],
               c("Barack Obama", "Kathleen G Kane", "Chaka Fattah",
                 "Dwight Evans"))
  expect_equal(file5$candidate[file5$uniqueID == "020383 1", ],
               c("Barack Obama", "Bob Casey Jr", "Kathleen G Kane",
                 "Eugene A Depasquale", "Robert M Mccord", "Chaka Fattah",
                 "Cherelee Lesley Parker", "Yes Si", "Yes Si", "Yes Si",
                 "Yes Si"))
  expect_equal(file5$candidate[file5$uniqueID == "020383 6", ],
               c("Barack Obama", "Bob Casey Jr", "Kathleen G Kane",
                 "Eugene A Depasquale"))



  expect_false(all(grepl(",", file1$candidate)))
  expect_false(all(grepl(",", file2$candidate)))
  expect_false(all(grepl(",", file3$candidate)))
  expect_false(all(grepl(",", file4$candidate)))
  expect_false(all(grepl(",", file5$candidate)))

})


test_that("Categories are correct", {

  expect_equal(file1$category[file1$uniqueID == "020054 178", ],
               c("President and Vice President of the United States",
                 "United States Senator",
                 "Attorney General",
                 "Auditor General",
                 "State Treasurer",
                 "Representative in Congress - 1st District"))
  expect_equal(file1$category[file1$uniqueID == "020006 148", ],
               c("President and Vice President of the United States",
                 "United States Senator",
                 "Attorney General",
                 "Auditor General",
                 "State Treasurer",
                 "Representative in Congress - 1st District",
                 "Senator in the General Assembly - 1st District"))
  expect_equal(file1$category[file1$uniqueID == "020001 3", ],
               c("President and Vice President of the United States",
                 "United States Senator",
                 "Attorney General",
                 "Auditor General",
                 "State Treasurer",
                 "Representative in Congress - 1st District",
                 "Senator in the General Assembly - 1st District",
                 "Representative in the General Assembly - 184th District",
                 "Proposed Charter Change Question 1",
                 "Proposed Charter Change Question 2",
                 "Proposed Charter Change Question 3",
                 "City Bond QUestion"))
  expect_equal(file1$category[file1$uniqueID == "020001 17", ],
               c("President and Vice President of the United States",
                 "United States Senator",
                 "Attorney General"))
  expect_equal(file2$category[file2$uniqueID == "022324 59", ],
               c("President and Vice President of the United States",
                 "United States Senator",
                 "Attorney General",
                 "Auditor General"))
  expect_equal(file2$category[file2$uniqueID == "022324 56", ],
               c("President and Vice President of the United States",
                 "United States Senator",
                 "Attorney General",
                 "Auditor General",
                 "State Treasurer",
                 "Representative in Congress - 2nd District",
                 "Representative in the General Assembly - 186th District"))
  expect_equal(file2$category[file2$uniqueID == "021975 61", ],
               c("President and Vice President of the United States",
                 "United States Senator",
                 "Attorney General",
                 "Auditor General",
                 "State Treasurer",
                 "Representative in Congress - 1st District",
                 "Representative in the General Assembly - 185th District"))
  expect_equal(file2$category[file2$uniqueID == "021965 68", ],
               c("President and Vice President of the United States",
                 "United States Senator",
                 "Attorney General",
                 "Auditor General",
                 "State Treasurer",
                 "Representative in Congress - 1st District",
                 "Representative in the General Assembly - 185th District"))
  expect_equal(file3$category[file3$uniqueID == "020676 149", ],
               c("President and Vice President of the United States",
                 "United States Senator",
                 "Attorney General",
                 "Auditor General",
                 "State Treasurer",
                 "Representative in Congress - 2nd District",
                 "Senator in the General Assembly - 3rd District",
                 "Representative in the General Assembly - 197th District"))
  expect_equal(file3$category[file3$uniqueID == "020845 85", ],
               c("United States Senator",
                 "Attorney General",
                 "Auditor General",
                 "State Treasurer",
                 "Representative in Congress - 1st District",
                 "Senator in the General Assembly - 3rd District",
                 "Representative in the General Assembly - 181st District"))
  expect_equal(file3$category[file3$uniqueID == "020845 76", ],
               c("President and Vice President of the United States",
                 "Senator in the General Assembly - 3rd District",
                 "Representative in the General Assembly - 181st District"))
  expect_equal(file3$category[file3$uniqueID == "020845 77", ],
               c("United States Senator"))
  expect_equal(file4$category[file4$uniqueID == "020393 213", ],
               c("President and Vice President of the United States"))
  expect_equal(file4$category[file4$uniqueID == "020383 6 ", ],
               c("President and Vice President of the United States",
                 "United States Senator",
                 "Attorney General",
                 "Auditor General"))
  expect_equal(file4$category[file4$uniqueID == "020418 46", ],
               c("President and Vice President of the United States",
                 "Attorney General",
                 "Representative in Congress - 2nd District",
                 "Representative in the General Assembly - 203rd District"))
  expect_equal(file4$category[file4$uniqueID == "023424 82", ],
               c("President and Vice President of the United States",
                 "United States Senator",
                 "Attorney General",
                 "Auditor General",
                 "State Treasurer",
                 "Representative in Congress - 2nd District",
                 "Representative in the General Assembly - 198th District"))
  expect_equal(file5$category[file5$uniqueID == "020390 81", ],
               c("President and Vice President of the United States",
                 "United States Senator",
                 "Representative in Congress - 2nd District"))
  expect_equal(file5$category[file5$uniqueID == "020397 160", ],
               c("President and Vice President of the United States",
                 "United States Senator",
                 "Attorney General",
                 "Auditor General",
                 "State Treasurer",
                 "Representative in Congress - 2nd District",
                 "Representative in the General Assembly - 200th District",
                 "Proposed Charter Change Question 1",
                 "Proposed Charter Change Question 2",
                 "Proposed Charter Change Question 3",
                 "City Bond Question"))
  expect_equal(file5$category[file5$uniqueID == "020418 44", ],
               c("President and Vice President of the United States",
                 "United States Senator",
                 "Attorney General",
                 "Auditor General",
                 "State Treasurer",
                 "Represenatative in Congress - 2nd District",
                 "Representative in the General Assembly - 203rd District"))
  expect_equal(file5$category[file5$uniqueID == "020418 46", ],
               c("President and Vice President of the United States",
                 "Attorney General",
                 "Representative in Congress - 2nd District",
                 "Representative in the General Assembly - 203rd District"))

})

test_that("Ballots with No Vote all have category NA", {
  expect_true(is.na(unique(file1$category[file1$candidate == "No Vote"])))
  expect_true(is.na(unique(file2$category[file2$candidate == "No Vote"])))
  expect_true(is.na(unique(file3$category[file3$candidate == "No Vote"])))
  expect_true(is.na(unique(file4$category[file4$candidate == "No Vote"])))
  expect_true(is.na(unique(file5$category[file5$candidate == "No Vote"])))
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

  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020001"])), 92)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020009"])), 122)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020020"])), 185)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020034"])), 133)

  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021965"])), 181)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021973"])), 291)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021980"])), 197)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021984"])), 260)

  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "022775"])), 71)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "022764"])), 231)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "022754"])), 148)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "022738"])), 175)

  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020691"])), 64)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020704"])), 157)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "023434"])), 206)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020845"])), 225)

  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020390"])), 235)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020395"])), 229)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020398"])), 144)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "023424"])), 354)
})


test_that("unique IDs in right order", {
  expect_equal(unique(head(file1$uniqueID)), c("020001 1", "020001 2", "020001 3",
                                               "020001 4", "020001 5", "020001 6"))
  expect_equal(unique(tail(file1$uniqueID)), c("020054 173", "020054 174", "020054 175",
                                               "020054 176", "020054 177", "020054 178"))

  expect_equal(unique(head(file2$uniqueID)), c("021957 114", "021957 115", "021957 116",
                                               "021957 117", "021957 118", "021957 119"))
  expect_equal(unique(tail(file2$uniqueID)), c("022324 54", "022324 55", "022324 56",
                                               "022324 57", "022324 58", "022324 59"))

  expect_equal(unique(head(file3$uniqueID)), c("022721 1", "022721 2", "022721 3",
                                               "022721 4", "022721 5", "022721 6"))
  expect_equal(unique(tail(file3$uniqueID)), c("022775 64", "022775 65", "022775 68",
                                               "022775 69", "022775 70", "022775 71"))

  expect_equal(unique(head(file4$uniqueID)), c("020676 148", "020676 149", "020676 150",
                                               "020676 151", "020676 152", "020676 153"))
  expect_equal(unique(tail(file4$uniqueID)), c("021236 66", "021236 67", "021236 68",
                                               "021236 69", "021236 70", "021236 71"))

  expect_equal(unique(head(file5$uniqueID)), c("020383 1", "020383 2", "020383 3",
                                               "020383 4", "020383 5", "020383 6"))
  expect_equal(unique(tail(file5$uniqueID)), c("020418 42", "020418 43", "020418 44",
                                               "020418 45", "020418 46", "020418 47"))
})
