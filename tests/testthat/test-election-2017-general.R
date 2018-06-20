context("election-2017-general")

file1 <- system.file("data/PDF_data/2017 General",
                     "2017 GENERAL - BROWN BIR 7.pdf",
                     package = "phillyvotes")
file2 <- system.file("data/PDF_data/2017 General",
                     "2017 GENERAL - BLACK BIR 1.pdf",
                     package = "phillyvotes")
file3 <- system.file("data/PDF_data/2017 General",
                     "2017 GENERAL - BLACK BIR 5.pdf",
                     package = "phillyvotes")
file4 <- system.file("data/PDF_data/2017 General",
                     "2017 GENERAL - BROWN BIR 3.pdf",
                     package = "phillyvotes")
file5 <- system.file("data/PDF_data/2017 General",
                     "2017 GENERAL - GREEN BIR 3.pdf",
                     package = "phillyvotes")

file1 <- philly_votes(file1)
file2 <- philly_votes(file2)
file3 <- philly_votes(file3)
file4 <- philly_votes(file4)
file5 <- philly_votes(file5)

test_that("right file name", {
  expect_equal(unique(file1$file), "2017 GENERAL - BROWN BIR 7")
  expect_equal(unique(file2$file), "2017 GENERAL - BLACK BIR 1")
  expect_equal(unique(file3$file), "2017 GENERAL - BLACK BIR 5")
  expect_equal(unique(file4$file), "2017 GENERAL - BROWN BIR 3")
  expect_equal(unique(file5$file), "2017 GENERAL - GREEN BIR 3")
})


test_that("right number of pages", {
  expect_equal(max(file1$pdf_page), 21000)
  expect_equal(max(file2$pdf_page), 3000)
  expect_equal(max(file3$pdf_page), 15000)
  expect_equal(max(file4$pdf_page), 9000)
  expect_equal(max(file5$pdf_page), 9000)
})




test_that("ballot ballot_position values are correct", {
  # First 6 votes
  expect_equal(head(file1$ballot_position), c("23C", "25C", "27C", "29C", "41G", "6I"))
  # Last 6 votes
  expect_equal(tail(file1$ballot_position), c("6I", "13I", "17I", "23I", "26I", "29I"))

  # First 6 votes
  expect_equal(head(file2$ballot_position), c("4C", "6C", "7C", "8C", "9C", "10C"))
  # Last 6 votes
  expect_equal(tail(file2$ballot_position), c("4D", "23D", "25D", "42G", "42L", "4C"))

  # First 6 votes
  expect_equal(head(file3$ballot_position), c("9C", "19C", "4D", "23D", "42G", "6I"))
  # Last 6 votes
  expect_equal(tail(file3$ballot_position), c("12C", "13C", "14C", "15C", "16C", "17C"))

  # First 6 votes
  expect_equal(head(file4$ballot_position), c("14C", "15C", "16C", "17C", "18C", "19C"))
  # Last 6 votes
  expect_equal(tail(file4$ballot_position), c("21C", "22C", "23C", "25C", "27C", "29C"))

  # First 6 votes
  expect_equal(head(file5$ballot_position), c("23C", "25C", "27C", "29C", "41G", "41L"))
  # Last 6 votes
  expect_equal(tail(file5$ballot_position), c("18C", "19C", "20C", "21C", "22C", "23C"))

})

test_that("unique IDs in right order", {
  expect_equal(head(file1$uniqueID), c("022524 30", "022524 30", "022524 30",
                                       "022524 30", "022524 30", "022524 30"))
  expect_equal(tail(file1$uniqueID), c("022570 78", "022570 78", "022570 78",
                                       "022570 78", "022570 78", "022570 78"))

  expect_equal(head(file2$uniqueID), c("022721 1", "022721 1", "022721 1",
                                       "022721 1", "022721 1", "022721 1"))
  expect_equal(tail(file2$uniqueID), c("022817 66", "022817 66", "022817 66",
                                       "022817 66", "022817 66", "022817 67"))

  expect_equal(head(file3$uniqueID), c("023255 2", "023255 2", "023255 2",
                                       "023255 2", "023255 2", "023255 2"))
  expect_equal(tail(file3$uniqueID), c("023330 28", "023330 28", "023330 28",
                                       "023330 28", "023330 28", "023330 28"))

  expect_equal(head(file4$uniqueID), c("021085 14", "021085 14", "021085 14",
                                       "021085 14", "021085 14", "021085 14"))
  expect_equal(tail(file4$uniqueID), c("021506 31", "021506 31", "021506 31",
                                       "021506 31", "021506 31", "021506 31"))

  expect_equal(head(file5$uniqueID), c("020632 2", "020632 2", "020632 2",
                                       "020632 2", "020632 2", "020632 2"))
  expect_equal(tail(file5$uniqueID), c("021345 33", "021345 33", "021345 33",
                                       "021345 33", "021345 33", "021345 33"))
})

test_that("voter number in right order", {
  expect_equal(head(file1$voter_record), c(30, 30, 30, 30, 30, 30))
  expect_equal(tail(file1$voter_record), c(78, 78, 78, 78, 78, 78))

  expect_equal(head(file2$voter_record), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file2$voter_record), c(66, 66, 66, 66, 66, 67))

  expect_equal(head(file3$voter_record), c(2, 2, 2, 2, 2, 2))
  expect_equal(tail(file3$voter_record), c(28, 28, 28, 28, 28, 28))

  expect_equal(head(file4$voter_record), c(14, 14, 14, 14, 14, 14))
  expect_equal(tail(file4$voter_record), c(31, 31, 31, 31, 31, 31))

  expect_equal(head(file5$voter_record), c(2, 2, 2, 2, 2, 2))
  expect_equal(tail(file5$voter_record), c(33, 33, 33, 33, 33, 33))
})



test_that("proper number of unique ID for a location and serial number match", {

  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022570"])), 78)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022555"])), 110)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022544"])), 107)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "022529"])), 98)

  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "022817"])), 67)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "022785"])), 68)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "022769"])), 46)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "022742"])), 48)

  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "023330"])), 28)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "023308"])), 42)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "023281"])), 93)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "023274"])), 60)

  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "021506"])), 31)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "021233"])), 126)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "021198"])), 72)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "021209"])), 41)

  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "021345"])), 33)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020788"])), 109)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020777"])), 124)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020770"])), 84)
})

test_that("unique voters have correct number of files recorded", {
  expect_equal(nrow(file1[file1$uniqueID == "022570 76",]), 5)
  expect_equal(nrow(file1[file1$uniqueID == "022555 84",]), 1)
  expect_equal(nrow(file1[file1$uniqueID == "022544 57",]), 3)
  expect_equal(nrow(file1[file1$uniqueID == "022529 85",]), 9)

  expect_equal(nrow(file2[file2$uniqueID == "022817 66",]), 6)
  expect_equal(nrow(file2[file2$uniqueID == "022785 6",]), 6)
  expect_equal(nrow(file2[file2$uniqueID == "022769 39",]), 14)
  expect_equal(nrow(file2[file2$uniqueID == "022769 40",]), 2)

  expect_equal(nrow(file3[file3$uniqueID == "023330 28",]), 13)
  expect_equal(nrow(file3[file3$uniqueID == "023330 19",]), 2)
  expect_equal(nrow(file3[file3$uniqueID == "023308 27",]), 6)
  expect_equal(nrow(file3[file3$uniqueID == "023281 5",]), 10)

  expect_equal(nrow(file4[file4$uniqueID == "021233 25",]), 3)
  expect_equal(nrow(file4[file4$uniqueID == "021233 19",]), 4)
  expect_equal(nrow(file4[file4$uniqueID == "021085 18",]), 1)
  expect_equal(nrow(file4[file4$uniqueID == "021198 16",]), 6)

  expect_equal(nrow(file5[file5$uniqueID == "020788 73",]), 5)
  expect_equal(nrow(file5[file5$uniqueID == "020788 68",]), 10)
  expect_equal(nrow(file5[file5$uniqueID == "020777 122",]), 10)
  expect_equal(nrow(file5[file5$uniqueID == "020632 2",]), 6)

})


test_that("Candidate has right name", {

  expect_equal(file1$candidate[file1$uniqueID == "022570 76"],
               c("Dwayne Woodruff",
                 "Debbie Kunselman",
                 "Irene M Clark",
                 "Lucretia C Clemons",
                 "Lawrence S Krasner"))
  expect_equal(file1$candidate[file1$uniqueID == "022555 37"],
               c("Dwayne Woodruff",
                 "Carolyn H Nichols",
                 "Ellen Ceisler",
                 "Lawrence S Krasner",
                 "Rebecca Rhynhart",
                 "Vernadean Mack",
                 "Almetta E Ingram"))
  expect_equal(file1$candidate[file1$uniqueID == "022555 27"],
               c("Lawrence S Krasner",
                 "Vernadean Mack",
                 "Almetta E Ingram",
                 "Sallie Mundy"))
  expect_equal(file1$candidate[file1$uniqueID == "022555 7"],
               c("Dwayne Woodruff",
                 "Carolyn H Nichols",
                 "Deborah Canty",
                 "Vernadean Mack"))

  expect_equal(file2$candidate[file2$uniqueID == "022721 23"],
               c("Write In"))
  expect_equal(file2$candidate[file2$uniqueID == "022721 47"],
               c("Write In"))
  expect_equal(file2$candidate[file2$uniqueID == "022722 10"],
               c("Lawrence S Krasner",
                 "Write In",
                 "Yes Si"))
  expect_equal(file2$candidate[file2$uniqueID == "022722 13"],
               c("Maria Mclaughlin",
                 "Stella Tsai",
                 "Marissa Brumbach",
                 "Matt Wolf"))

  expect_equal(file3$candidate[file3$uniqueID == "023255 7"],
               c("Beth Grossman"))
  expect_equal(file3$candidate[file3$uniqueID == "023255 21"],
               c("Ellen Ceisler",
                 "Beth Grossman",
                 "Michael Tomlinson"))
  expect_equal(file3$candidate[file3$uniqueID == "023255 37"],
               c("Beth Grossman",
                 "Michael Tomlinson"))
  expect_equal(file3$candidate[file3$uniqueID == "023477 38"],
               c("Beth Grossman",
                 "No No",
                 "No No"))

  expect_equal(file4$candidate[file4$uniqueID == "021085 18"],
               c("Dwayne Woodruff"))
  expect_equal(file4$candidate[file4$uniqueID == "021086 14"],
               c("Stella Tsai"))
  expect_equal(file4$candidate[file4$uniqueID == "021086 23"],
               c("Dwayne Woodruff",
                 "Mark B Cohen",
                 "Charles Edwards"))
  expect_equal(file4$candidate[file4$uniqueID == "021216 87"],
               c("Lawrence S Krasner",
                 "Yes Si",
                 "Yes Si"))

  expect_equal(file5$candidate[file5$uniqueID == "020778 5"],
               c("Rebecca Rhynhart",
                 "No No",
                 "Yes Si"))
  expect_equal(file5$candidate[file5$uniqueID == "020778 27"],
               c("Beth Grossman",
                 "No No",
                 "Yes Si"))
  expect_equal(file5$candidate[file5$uniqueID == "020778 56"],
               c("Lawrence S Krasner",
                 "No No",
                 "Yes Si"))
  expect_equal(file5$candidate[file5$uniqueID == "021345 28"],
               c("Maria Mclaughlin",
                 "Ellen Ceisler",
                 "Lucretia C Clemons"))
})


test_that("Categories are correct", {

  expect_equal(file1$category[file1$uniqueID == "022570 76"],
               c("Justice of the Supreme Court",
                 "Judge of the Superior Court",
                 "Judge of the Commonwealth Court",
                 "Judge of the Court of Common Pleas",
                 "District Attorney"))
  expect_equal(file1$category[file1$uniqueID == "022555 37"],
               c("Justice of the Supreme Court",
                 "Judge of the Superior Court",
                 "Judge of the Commonwealth Court",
                 "District Attorney", "City Controller",
                 "Judge of Election-52-20",
                 "Inspector of Election-52-20"))
  expect_equal(file1$category[file1$uniqueID == "022555 27"],
               c("District Attorney",
                 "Judge of Election-52-20",
                 "Inspector of Election-52-20",
                 "Justice of the Supreme Court"))
  expect_equal(file1$category[file1$uniqueID == "022555 7"],
               c("Justice of the Supreme Court",
                 "Judge of the Superior Court",
                 "Judge of the Court of Common Pleas",
                 "Judge of Election-52-20"))

  expect_equal(file2$category[file2$uniqueID == "022721 23"],
               c("Judge of Election-56-01"))
  expect_equal(file2$category[file2$uniqueID == "022721 47"],
               c("Judge of Election-56-01"))
  expect_equal(file2$category[file2$uniqueID == "022722 10"],
               c("District Attorney",
                 "Judge of Election-56-01",
                 "City Bond Question"))
  expect_equal(file2$category[file2$uniqueID == "022722 13"],
               c("Judge of the Superior Court",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Municipal Court",
                 "Judge of the Municipal Court"))

  expect_equal(file3$category[file3$uniqueID == "023255 7"],
               c("District Attorney"))
  expect_equal(file3$category[file3$uniqueID == "023255 21"],
               c("Judge of the Commonwealth Court",
                 "District Attorney",
                 "City Controller"))
  expect_equal(file3$category[file3$uniqueID == "023255 37"],
               c("District Attorney", "City Controller"))
  expect_equal(file3$category[file3$uniqueID == "023477 38"],
               c("District Attorney",
                 "Proposed Constitutional Amendment",
                 "City Bond Question"))

  expect_equal(file4$category[file4$uniqueID == "021085 18"],
               c("Justice of the Supreme Court"))
  expect_equal(file4$category[file4$uniqueID == "021086 14"],
               c("Judge of the Court of Common Pleas"))
  expect_equal(file4$category[file4$uniqueID == "021086 23"],
               c("Justice of the Supreme Court",
                 "Judge of the Court of Common Pleas",
                 "Inspector of Election-24-16"))
  expect_equal(file4$category[file4$uniqueID == "021216 87"],
               c("District Attorney",
                 "Proposed Constitutional Amendment",
                 "City Bond Question"))

  expect_equal(file5$category[file5$uniqueID == "020778 5"],
               c("City Controller",
                 "Proposed Constitutional Amendment",
                 "City Bond Question"))
  expect_equal(file5$category[file5$uniqueID == "020778 27"],
               c("District Attorney",
                 "Proposed Constitutional Amendment",
                 "City Bond Question"))
  expect_equal(file5$category[file5$uniqueID == "020778 56"],
               c("District Attorney",
                 "Proposed Constitutional Amendment",
                 "City Bond Question"))
  expect_equal(file5$category[file5$uniqueID == "021345 28"],
               c("Judge of the Superior Court",
                 "Judge of the Commonwealth Court",
                 "Judge of the Court of Common Pleas"))

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
