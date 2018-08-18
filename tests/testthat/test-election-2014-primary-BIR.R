context("election-2014-primary")

file1 <- system.file("data/PDF_data/2014 Primary",
                     "BLACK _ YELLOW 1 BIR.pdf",
                     package = "phillyvotes")
file2 <- system.file("data/PDF_data/2014 Primary",
                     "BLUE 2 BIR.pdf",
                     package = "phillyvotes")
file3 <- system.file("data/PDF_data/2014 Primary",
                     "BLUE 3 BIR.pdf",
                     package = "phillyvotes")
file4 <- system.file("data/PDF_data/2014 Primary",
                     "PINK 1 BIR.pdf",
                     package = "phillyvotes")
file5 <- system.file("data/PDF_data/2014 Primary",
                     "ORANGE 4 BIR.pdf",
                     package = "phillyvotes")

file1 <- philly_votes(file1)
file2 <- philly_votes(file2)
file3 <- philly_votes(file3)
file4 <- philly_votes(file4)
file5 <- philly_votes(file5)

test_that("right file name", {
  expect_equal(unique(file1$file), "BLACK _ YELLOW 1 BIR")
  expect_equal(unique(file2$file), "BLUE 2 BIR")
  expect_equal(unique(file3$file), "BLUE 3 BIR")
  expect_equal(unique(file4$file), "PINK 1 BIR")
  expect_equal(unique(file5$file), "ORANGE 4 BIR")
})


test_that("right number of pages", {
  expect_equal(max(file1$pdf_page), 3000)
  expect_equal(max(file2$pdf_page), 6000)
  expect_equal(max(file3$pdf_page), 8798)
  expect_equal(max(file4$pdf_page), 3000)
  expect_equal(max(file5$pdf_page), 12000)
})




test_that("ballot ballot_position values are correct", {
  # First 6 votes
  expect_equal(head(file1$ballot_position), c("13E", "15E", "6E", "9E", "14E", "16E"))
  # Last 6 votes
  expect_equal(tail(file1$ballot_position), c("42K", "9L", "3E", "6E", "9E", "4C"))

  # First 6 votes
  expect_equal(head(file2$ballot_position), c("23E", "24E", "26E", "27E", "41E", "22F"))
  # Last 6 votes
  expect_equal(tail(file2$ballot_position), c("6E", "15E", "22E", "26E", "4F", "7L"))

  # First 6 votes
  expect_equal(head(file3$ballot_position), c("3E", "6E", "9E", "15E", "26E", "27E"))
  # Last 6 votes
  expect_equal(tail(file3$ballot_position), c("41E", "4F", "23F", "42H", "41K", "7L"))

  # First 6 votes
  expect_equal(head(file4$ballot_position), c("3E", "6E", "10E", "14E", "6E", "14E"))
  # Last 6 votes
  expect_equal(tail(file4$ballot_position), c("9E", "16E", "6E", "9E", "15E", "42E"))

  # First 6 votes
  expect_equal(head(file5$ballot_position), c("4E", "6E", "12E", "42E", "22F", "23F"))
  # Last 6 votes
  expect_equal(tail(file5$ballot_position), c("6E", "9E", "15E", "23E", "24E", "25E"))

})

test_that("unique IDs in right order", {
  expect_equal(head(file1$uniqueID), c("020277 1", "020277 1", "020277 2",
                                       "020277 2", "020277 2", "020277 2"))
  expect_equal(tail(file1$uniqueID), c("022188 12", "022188 12", "022188 13",
                                       "022188 13", "022188 13", "022188 14"))

  expect_equal(head(file2$uniqueID), c("021326 61", "021326 61", "021326 61",
                                       "021326 61", "021326 61", "021326 61"))
  expect_equal(tail(file2$uniqueID), c("021893 43", "021893 43", "021893 43",
                                       "021893 43", "021893 43", "021893 43"))

  expect_equal(head(file3$uniqueID), c("021893 44", "021893 44", "021893 44",
                                       "021893 44", "021893 44", "021893 44"))
  expect_equal(tail(file3$uniqueID), c("022516 30", "022516 30", "022516 30",
                                       "022516 30", "022516 30", "022516 30"))

  expect_equal(head(file4$uniqueID), c("021009 1", "021009 1", "021009 1",
                                       "021009 1", "021009 2", "021009 2"))
  expect_equal(tail(file4$uniqueID), c("022624 16", "022624 16", "022624 17",
                                       "022624 17", "022624 17", "022624 17"))

  expect_equal(head(file5$uniqueID), c("020952 165", "020952 165", "020952 165",
                                       "020952 165", "020952 165", "020952 165"))
  expect_equal(tail(file5$uniqueID), c("023457 2", "023457 2", "023457 2",
                                       "023457 2", "023457 2", "023457 2"))
})




test_that("proper number of unique ID for a location and serial number match", {

  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022188"])), 14)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "021736"])), 59)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020277"])), 44)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "021449"])), 69)

  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021893"])), 43)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021336"])), 84)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021327"])), 87)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021326"])), 10)

  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "022516"])), 30)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "022316"])), 86)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "021970"])), 124)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "021894"])), 60)

  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "022624"])), 17)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "021634"])), 38)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "021590"])), 100)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "021009"])), 26)

  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "023457"])), 2)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "021764"])), 91)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020993"])), 65)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020982"])), 71)
})

test_that("unique voters have correct number of files recorded", {
  expect_equal(nrow(file1[file1$uniqueID == "022188 13",]), 3)
  expect_equal(nrow(file1[file1$uniqueID == "022188 14",]), 1)
  expect_equal(nrow(file1[file1$uniqueID == "021736 51",]), 8)
  expect_equal(nrow(file1[file1$uniqueID == "020277 1",]), 2)

  expect_equal(nrow(file2[file2$uniqueID == "021893 43",]), 6)
  expect_equal(nrow(file2[file2$uniqueID == "021336 54",]), 18)
  expect_equal(nrow(file2[file2$uniqueID == "021327 1",]), 9)
  expect_equal(nrow(file2[file2$uniqueID == "021326 61",]), 10)

  expect_equal(nrow(file3[file3$uniqueID == "022516 29",]), 4)
  expect_equal(nrow(file3[file3$uniqueID == "022516 30",]), 10)
  expect_equal(nrow(file3[file3$uniqueID == "021970 50",]), 7)
  expect_equal(nrow(file3[file3$uniqueID == "021894 44",]), 6)

  expect_equal(nrow(file4[file4$uniqueID == "022624 15",]), 3)
  expect_equal(nrow(file4[file4$uniqueID == "022624 16",]), 4)
  expect_equal(nrow(file4[file4$uniqueID == "021634 16",]), 4)
  expect_equal(nrow(file4[file4$uniqueID == "021009 1",]), 4)

  expect_equal(nrow(file5[file5$uniqueID == "023457 2",]), 7)
  expect_equal(nrow(file5[file5$uniqueID == "021764 40",]), 9)
  expect_equal(nrow(file5[file5$uniqueID == "020993 1",]), 5)
  expect_equal(nrow(file5[file5$uniqueID == "020982 7",]), 10)

})

test_that("voter number in right order", {
  expect_equal(head(file1$voter_record), c(1, 1, 2, 2, 2, 2))
  expect_equal(tail(file1$voter_record), c(12, 12, 13, 13, 13, 14))

  expect_equal(head(file2$voter_record), c(61, 61, 61, 61, 61, 61))
  expect_equal(tail(file2$voter_record), c(43, 43, 43, 43, 43, 43))

  expect_equal(head(file3$voter_record), c(44, 44, 44, 44, 44, 44))
  expect_equal(tail(file3$voter_record), c(30, 30, 30, 30, 30, 30))

  expect_equal(head(file4$voter_record), c(1, 1, 1, 1, 2, 2))
  expect_equal(tail(file4$voter_record), c(16, 16, 17, 17, 17, 17))

  expect_equal(head(file5$voter_record), c(165, 165, 165, 165, 165, 165))
  expect_equal(tail(file5$voter_record), c(2, 2, 2, 2, 2, 2))
})


test_that("Candidate has right name", {

  expect_equal(file1$candidate[file1$uniqueID == "020277 1"],
               c("Tomas Sanchez",
                 "Quetcy Lozada"))
  expect_equal(file1$candidate[file1$uniqueID == "020277 5"],
               c("Angel L Cruz"))
  expect_equal(file1$candidate[file1$uniqueID == "021442 5"],
               c("Yes Si",
                 "Yes Si",
                 "Yes Si",
                 "Matt Wolfe"))
  expect_equal(file1$candidate[file1$uniqueID == "021466 41"],
               c("Angel L Cruz"))

  expect_equal(file2$candidate[file2$uniqueID == "021327 9"],
               c("Tom Wolf",
                 "Mike Stack",
                 "Chaka Fattah",
                 "Jordan A Harris"))
  expect_equal(file2$candidate[file2$uniqueID == "021683 10"],
               c("Tom Wolf",
                 "Ed Neilson"))
  expect_equal(file2$candidate[file2$uniqueID == "021683 15"],
               c("Tom Wolf",
                 "Chaka Fattah",
                 "Ed Neilson"))
  expect_equal(file2$candidate[file2$uniqueID == "021711 33"],
               c("William A Ryan Jr"))

  expect_equal(file3$candidate[file3$uniqueID == "021893 46"],
               c("Tom Wolf",
                 "Tina A Grassia",
                 "John F Hundzynski"))
  expect_equal(file3$candidate[file3$uniqueID == "021893 50"],
               c("Allyson Y Schwartz"))
  expect_equal(file3$candidate[file3$uniqueID == "021893 73"],
               c("Robert M Mccord",
                 "Mark Smith"))
  expect_equal(file3$candidate[file3$uniqueID == "021978 24"],
               c("Tom Wolf"))

  expect_equal(file4$candidate[file4$uniqueID == "021009 12"],
               c("John J Taylor", "Yes Si"))
  expect_equal(file4$candidate[file4$uniqueID == "021010 12"],
               c("Tomas Sanchez"))
  expect_equal(file4$candidate[file4$uniqueID == "021010 16"],
               c("Tomas Sanchez"))
  expect_equal(file4$candidate[file4$uniqueID == "021010 18"],
               c("No Vote"))
  expect_equal(file4$candidate[file4$uniqueID == "021636 19"],
               c("Tom Wolf",
                 "Mike Stack",
                 "Brendan F Boyle",
                 "Christine M Tartaglione",
                 "Mark B Cohen",
                 "No No",
                 "No No",
                 "No No"))

  expect_equal(file5$candidate[file5$uniqueID == "020966 80"],
               c("Yes Si",
                 "Yes Si",
                 "Yes Si", "Nikki Allen Poe"))
  expect_equal(file5$candidate[file5$uniqueID == "020978 88"],
               c("Chaka Fattah",
                 "Allyson Y Schwartz"))
  expect_equal(file5$candidate[file5$uniqueID == "020978 89"],
               c("Tom Wolf",
                 "Mike Stack",
                 "Chaka Fattah"))
  expect_equal(file5$candidate[file5$uniqueID == "020978 90"],
               c("No No", "No No", "Yes Si"))
})


test_that("Categories are correct", {

  expect_equal(file1$category[file1$uniqueID == "020277 1"],
               c("Senator in the General Assembly - 2nd District - Democrat",
                 "Representative in the General Assembly - 180th District - Democrat"))
  expect_equal(file1$category[file1$uniqueID == "020277 5"],
               c("Representative in the General Assembly - 180th District - Democrat"))
  expect_equal(file1$category[file1$uniqueID == "021442 5"],
               c("Proposed Charter Change Question #1",
                 "Proposed Charter Change Question #2",
                 "Proposed Charter Change Question #3",
                 "Special Election - Council at Large"))
  expect_equal(file1$category[file1$uniqueID == "021466 41"],
               c("Representative in the General Assembly - 180th District - Democrat"))

  expect_equal(file2$category[file2$uniqueID == "021327 9"],
               c("Governor - Democrat", "Lieutenant Governor - Democrat",
                 "Representative in Congress - 2nd District - Democrat",
                 "Representative in the General Assembly - 186th District - Democrat"))
  expect_equal(file2$category[file2$uniqueID == "021683 10"],
               c("Governor - Democrat",
                 "Special Election - Council at Large"))
  expect_equal(file2$category[file2$uniqueID == "021683 15"],
               c("Governor - Democrat",
                 "Representative in Congress - 2nd District - Democrat",
                 "Special Election - Council at Large"))
  expect_equal(file2$category[file2$uniqueID == "021711 33"],
               c("Ward Executive Committee - Democrat - 36-38"))

  expect_equal(file3$category[file3$uniqueID == "021893 46"],
               c("Governor - Democrat",
                 "Ward Executive Committee - Democrat - 39-46",
                 "Ward Executive Committee - Democrat - 39-46"))
  expect_equal(file3$category[file3$uniqueID == "021893 50"],
               c("Governor - Democrat"))
  expect_equal(file3$category[file3$uniqueID == "021893 73"],
               c("Governor - Democrat", "Lieutenant Governor - Democrat"))
  expect_equal(file3$category[file3$uniqueID == "021978 24"],
               c("Governor - Democrat"))

  expect_equal(file4$category[file4$uniqueID == "021009 12"],
               c("Representative in the General Assembly - 177th District - Republican",
                 "Proposed Charter Change Question #1"))
  expect_equal(file4$category[file4$uniqueID == "021010 12"],
               c("Senator in the General Assembly - 2nd District - Democrat"))
  expect_equal(file4$category[file4$uniqueID == "021010 16"],
               c("Senator in the General Assembly - 2nd District - Democrat"))
  expect_true(is.na(file4$category[file4$uniqueID == "021010 18"]))
  expect_equal(file4$category[file4$uniqueID == "021636 19"],
               c("Governor - Democrat",
                 "Lieutenant Governor - Democrat",
                 "Representative in Congress - 13th District - Democrat",
                 "Senator in the General Assembly - 2nd District - Democrat",
                 "Representative in the General Assembly - 202nd District - Democrat",
                 "Proposed Charter Change Question #1",
                 "Proposed Charter Change Question #2",
                 "Proposed Charter Change Question #3"))

  expect_equal(file5$category[file5$uniqueID == "020966 80"],
               c("Proposed Charter Change Question #1",
                 "Proposed Charter Change Question #2",
                 "Proposed Charter Change Question #3",
                 "Special Election - Council at Large"))
  expect_equal(file5$category[file5$uniqueID == "020978 88"],
               c("Representative in Congress - 2nd District - Democrat",
                 "Governor - Democrat"))
  expect_equal(file5$category[file5$uniqueID == "020978 89"],
               c("Governor - Democrat", "Lieutenant Governor - Democrat",
                 "Representative in Congress - 2nd District - Democrat"))
  expect_equal(file5$category[file5$uniqueID == "020978 90"],
               c("Proposed Charter Change Question #1",
                 "Proposed Charter Change Question #2",
                 "Proposed Charter Change Question #3"))

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
