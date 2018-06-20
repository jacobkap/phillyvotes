context("election-2016-primary")

file1 <- system.file("data/PDF_data/2016 Primary",
                     "2016 PRIMARY - YELLOW BIR 1.pdf",
                     package = "phillyvotes")
file2 <- system.file("data/PDF_data/2016 Primary",
                     "2016 PRIMARY - YELLOW BIR 3.pdf",
                     package = "phillyvotes")
file3 <- system.file("data/PDF_data/2016 Primary",
                     "2016 PRIMARY - PINK BIR 4.pdf",
                     package = "phillyvotes")
file4 <- system.file("data/PDF_data/2016 Primary",
                     "2016 PRIMARY - GREEN BIR 3.pdf",
                     package = "phillyvotes")
file5 <- system.file("data/PDF_data/2016 Primary",
                     "2016 PRIMARY - BROWN BIR 6.pdf",
                     package = "phillyvotes")

file1 <- philly_votes(file1)
file2 <- philly_votes(file2)
file3 <- philly_votes(file3)
file4 <- philly_votes(file4)
file5 <- philly_votes(file5)

test_that("right file name", {
  expect_equal(unique(file1$file), "2016 PRIMARY - YELLOW BIR 1")
  expect_equal(unique(file2$file), "2016 PRIMARY - YELLOW BIR 3")
  expect_equal(unique(file3$file), "2016 PRIMARY - PINK BIR 4")
  expect_equal(unique(file4$file), "2016 PRIMARY - GREEN BIR 3")
  expect_equal(unique(file5$file), "2016 PRIMARY - BROWN BIR 6")
})


test_that("right number of pages", {
  expect_equal(max(file1$pdf_page), 833)
  expect_equal(max(file2$pdf_page), 10000)
  expect_equal(max(file3$pdf_page), 13099)
  expect_equal(max(file4$pdf_page), 9000)
  expect_equal(max(file5$pdf_page), 18000)
})




test_that("ballot ballot_position values are correct", {
  # First 6 votes
  expect_equal(head(file1$ballot_position), c("3C", "11C", "21C", "4C", "3C", "21C"))
  # Last 6 votes
  expect_equal(tail(file1$ballot_position), c("25E", "27E", "27F", "16L", "27L", "39L"))

  # First 6 votes
  expect_equal(head(file2$ballot_position), c("26D", "28D", "29D", "25E", "26E", "29E"))
  # Last 6 votes
  expect_equal(tail(file2$ballot_position), c("29F", "16L", "26L", "39L", "3C", "7C"))

  # First 6 votes
  expect_equal(head(file3$ballot_position), c("21C", "24C", "25C", "26C", "27C", "31C"))
  # Last 6 votes
  expect_equal(tail(file3$ballot_position), c("24F", "25F", "26F", "17L", "26L", "40L"))

  # First 6 votes
  expect_equal(head(file4$ballot_position), c("24E", "25E", "27E", "24F", "26F", "4C"))
  # Last 6 votes
  expect_equal(tail(file4$ballot_position), c("10C", "12C", "14C", "16C", "21C", "25C"))

  # First 6 votes
  expect_equal(head(file5$ballot_position), c("19C", "24C", "27C", "28C", "29C", "31C"))
  # Last 6 votes
  expect_equal(tail(file5$ballot_position), c("28C", "29C", "31C", "7D", "17D", "25D"))

})

test_that("unique IDs in right order", {
  expect_equal(head(file1$uniqueID), c("020277 1", "020277 1", "020277 1",
                                       "020277 2", "020277 3", "020277 3"))
  expect_equal(tail(file1$uniqueID), c("020322 68", "020322 68", "020322 68",
                                       "020322 68", "020322 68", "020322 68"))

  expect_equal(head(file2$uniqueID), c("021739 93", "021739 93", "021739 93",
                                       "021739 93", "021739 93", "021739 93"))
  expect_equal(tail(file2$uniqueID), c("022114 40", "022114 40", "022114 40",
                                       "022114 40", "022114 41", "022114 41"))

  expect_equal(head(file3$uniqueID), c("022693 7", "022693 7", "022693 7",
                                       "022693 7", "022693 7", "022693 7"))
  expect_equal(tail(file3$uniqueID), c("023236 133", "023236 133", "023236 133",
                                       "023236 133", "023236 133", "023236 133"))

  expect_equal(head(file4$uniqueID), c("020779 128", "020779 128", "020779 128",
                                       "020779 128", "020779 128", "020779 129"))
  expect_equal(tail(file4$uniqueID), c("021365 90", "021365 90", "021365 90",
                                       "021365 90", "021365 90", "021365 90"))

  expect_equal(head(file5$uniqueID), c("022162 29", "022162 29", "022162 29",
                                       "022162 29", "022162 29", "022162 29"))
  expect_equal(tail(file5$uniqueID), c("022267 8", "022267 8", "022267 8",
                                       "022267 8", "022267 8", "022267 8"))
})


test_that("voter number in right order", {
  expect_equal(head(file1$voter_record), c(1, 1, 1, 2, 3, 3))
  expect_equal(tail(file1$voter_record), c(68, 68, 68, 68, 68, 68))

  expect_equal(head(file2$voter_record), c(93, 93, 93, 93, 93, 93))
  expect_equal(tail(file2$voter_record), c(40, 40, 40, 40, 41, 41))

  expect_equal(head(file3$voter_record), c(7, 7, 7, 7, 7, 7))
  expect_equal(tail(file3$voter_record), c(133, 133, 133, 133, 133, 133))

  expect_equal(head(file4$voter_record), c(128, 128, 128, 128, 128, 129))
  expect_equal(tail(file4$voter_record), c(90, 90, 90, 90, 90, 90))

  expect_equal(head(file5$voter_record), c(29, 29, 29, 29, 29, 29))
  expect_equal(tail(file5$voter_record), c(8, 8, 8, 8, 8, 8))
})




test_that("proper number of unique ID for a location and serial number match", {

  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020322"])), 68)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020277"])), 96)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "023407"])), 140)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020297"])), 70)

  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "022114"])), 41)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "022076"])), 132)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021740"])), 97)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "022055"])), 73)

  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "023236"])), 133)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "023115"])), 67)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "022694"])), 135)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "022720"])), 83)

  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "021351"])), 132)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020823"])), 65)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020802"])), 151)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020790"])), 136)

  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "022267"])), 8)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "022255"])), 131)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "022243"])), 139)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "022238"])), 208)
})

test_that("unique voters have correct number of files recorded", {
  expect_equal(nrow(file1[file1$uniqueID == "020322 67",]), 1)
  expect_equal(nrow(file1[file1$uniqueID == "020322 66",]), 1)
  expect_equal(nrow(file1[file1$uniqueID == "020277 1",]), 3)
  expect_equal(nrow(file1[file1$uniqueID == "020277 2",]), 1)

  expect_equal(nrow(file2[file2$uniqueID == "022114 41",]), 2)
  expect_equal(nrow(file2[file2$uniqueID == "022114 39",]), 1)
  expect_equal(nrow(file2[file2$uniqueID == "022076 48",]), 6)
  expect_equal(nrow(file2[file2$uniqueID == "021739 94",]), 5)

  expect_equal(nrow(file3[file3$uniqueID == "023236 132",]), 3)
  expect_equal(nrow(file3[file3$uniqueID == "023236 129",]), 3)
  expect_equal(nrow(file3[file3$uniqueID == "023115 11",]), 8)
  expect_equal(nrow(file3[file3$uniqueID == "022694 1",]), 7)

  expect_equal(nrow(file4[file4$uniqueID == "021351 83",]), 5)
  expect_equal(nrow(file4[file4$uniqueID == "021351 77",]), 1)
  expect_equal(nrow(file4[file4$uniqueID == "020823 30",]), 1)
  expect_equal(nrow(file4[file4$uniqueID == "020823 31",]), 1)

  expect_equal(nrow(file5[file5$uniqueID == "022255 35",]), 4)
  expect_equal(nrow(file5[file5$uniqueID == "022243 69",]), 1)
  expect_equal(nrow(file5[file5$uniqueID == "022243 70",]), 3)
  expect_equal(nrow(file5[file5$uniqueID == "022238 81",]), 3)

})


test_that("Candidate has right name", {

  expect_equal(file1$candidate[file1$uniqueID == "020277 1"],
               c("Hillary Clinton",
                 "Stephen A Zappala Jr",
                 "Angel Cruz"))
  expect_equal(file1$candidate[file1$uniqueID == "020277 2"],
               c("Bernie Sanders"))
  expect_equal(file1$candidate[file1$uniqueID == "020290 20"],
               c("Hillary Clinton",
                 "Stephen A Zappala Jr"))
  expect_equal(file1$candidate[file1$uniqueID == "020290 26"],
               c("No Vote"))

  expect_equal(file2$candidate[file2$uniqueID == "021740 8"],
               c("Chaka Fattah",
                 "Kenneth T Walker Jr"))
  expect_equal(file2$candidate[file2$uniqueID == "021740 11"],
               c("Hillary Clinton",
                 "Sharif Street"))
  expect_equal(file2$candidate[file2$uniqueID == "021740 17"],
               c("Hillary Clinton"))
  expect_equal(file2$candidate[file2$uniqueID == "022055 14"],
               c("Hillary Clinton",
                 "Joe Sestak"))

  expect_equal(file3$candidate[file3$uniqueID == "022693 15"],
               c("Hillary Clinton"))
  expect_equal(file3$candidate[file3$uniqueID == "022693 18"],
               c("No No",
                 "No No",
                 "No No"))
  expect_equal(file3$candidate[file3$uniqueID == "023124 98"],
               c("Hillary Clinton"))
  expect_equal(file3$candidate[file3$uniqueID == "023124 101"],
               c("Yes Si",
                 "Yes Si"))

  expect_equal(file4$candidate[file4$uniqueID == "020823 30"],
               c("Hillary Clinton"))
  expect_equal(file4$candidate[file4$uniqueID == "020823 31"],
               c("Hillary Clinton"))
  expect_equal(file4$candidate[file4$uniqueID == "020794 61"],
               c("Hillary Clinton"))
  expect_equal(file4$candidate[file4$uniqueID == "020794 70"],
               c("Hillary Clinton",
                 "Joseph J Vodvarka"))

  expect_equal(file5$candidate[file5$uniqueID == "022162 47"],
               c("Bernie Sanders"))
  expect_equal(file5$candidate[file5$uniqueID == "022171 13"],
               c("Hillary Clinton",
                 "Chaka Fattah"))
  expect_equal(file5$candidate[file5$uniqueID == "022171 27"],
               c("Hillary Clinton"))
  expect_equal(file5$candidate[file5$uniqueID == "022266 92"],
               c("No No",
                 "No No",
                 "Yes Si"))
})


test_that("Categories are correct", {

  expect_equal(file1$category[file1$uniqueID == "020277 1"],
               c("President of the United States - Democrat",
                 "Attorney General - Democrat",
                 "Representative in the General Assembly - 180th District - Democrat"))
  expect_equal(file1$category[file1$uniqueID == "020277 2"],
               c("President of the United States - Democrat"))
  expect_equal(file1$category[file1$uniqueID == "020290 20"],
               c("President of the United States - Democrat",
                 "Attorney General - Democrat"))
  expect_true(is.na(file1$category[file1$uniqueID == "020290 26"]))

  expect_equal(file2$category[file2$uniqueID == "021740 8"],
               c("Representative in Congress - 2nd District - Democrat",
                 "Representative in the General Assembly - 181st District - Democrat"))
  expect_equal(file2$category[file2$uniqueID == "021740 11"],
               c("President of the United States - Democrat",
                 "Senator in the General Assembly - 3rd District - Democrat"))
  expect_equal(file2$category[file2$uniqueID == "021740 17"],
               c("President of the United States - Democrat"))
  expect_equal(file2$category[file2$uniqueID == "022055 14"],
               c("President of the United States - Democrat",
                 "United States Senator - Democrat"))

  expect_equal(file3$category[file3$uniqueID == "022693 15"],
               c("President of the United States - Democrat"))
  expect_equal(file3$category[file3$uniqueID == "022693 18"],
               c("Proposed Constitutional Amendment #1",
                 "Proposed Constitutional Amendment #2",
                 "Proposed Charter Change Question #3"))
  expect_equal(file3$category[file3$uniqueID == "023124 98"],
               c("President of the United States - Democrat"))
  expect_equal(file3$category[file3$uniqueID == "023124 101"],
               c("Proposed Constitutional Amendment #2",
                 "Proposed Charter Change Question #3"))

  expect_equal(file4$category[file4$uniqueID == "020823 30"],
               c("President of the United States - Democrat"))
  expect_equal(file4$category[file4$uniqueID == "020823 30"],
               c("President of the United States - Democrat"))
  expect_equal(file4$category[file4$uniqueID == "020794 61"],
               c("President of the United States - Democrat"))
  expect_equal(file4$category[file4$uniqueID == "020794 70"],
               c("President of the United States - Democrat",
                 "United States Senator - Democrat"))

  expect_equal(file5$category[file5$uniqueID == "022162 47"],
               c("President of the United States - Democrat"))
  expect_equal(file5$category[file5$uniqueID == "022171 13"],
               c("President of the United States - Democrat",
                 "Representative in Congress - 2nd District - Democrat"))
  expect_equal(file5$category[file5$uniqueID == "022171 27"],
               c("President of the United States - Democrat"))
  expect_equal(file5$category[file5$uniqueID == "022266 92"],
               c("Proposed Constitutional Amendment #1",
                 "Proposed Constitutional Amendment #2",
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
