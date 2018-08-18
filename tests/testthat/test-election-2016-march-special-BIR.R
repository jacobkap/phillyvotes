context("election-2016-march-special")

file1 <- system.file("data/PDF_data/2016 March Special",
                     "2016 March Special - Ward 34 BIR.pdf",
                     package = "phillyvotes")
file2 <- system.file("data/PDF_data/2016 March Special",
                     "2016 March Special - Wards 4 _ 52 BIR.pdf",
                     package = "phillyvotes")
file3 <- system.file("data/PDF_data/2016 March Special",
                     "2016 March Special - Ward 50 BIR.pdf",
                     package = "phillyvotes")
file4 <- system.file("data/PDF_data/2016 March Special",
                     "2016 March Special - Wards 9 _ 22 BIR.pdf",
                     package = "phillyvotes")

file1 <- philly_votes(file1)
file2 <- philly_votes(file2)
file3 <- philly_votes(file3)
file4 <- philly_votes(file4)

test_that("right file name", {
  expect_equal(unique(file1$file), "2016 March Special - Ward 34 BIR")
  expect_equal(unique(file2$file), "2016 March Special - Wards 4 _ 52 BIR")
  expect_equal(unique(file3$file), "2016 March Special - Ward 50 BIR")
  expect_equal(unique(file4$file), "2016 March Special - Wards 9 _ 22 BIR")
})


test_that("right number of pages", {
  expect_equal(max(file1$pdf_page), 249)
  expect_equal(max(file2$pdf_page), 180)
  expect_equal(max(file3$pdf_page), 252)
  expect_equal(max(file4$pdf_page), 374)
})




test_that("ballot ballot_position values are correct", {
  # First 6 votes
  expect_equal(head(file1$ballot_position), c("19F", "19F", "19H", "19F", "19F", "19F"))
  # Last 6 votes
  expect_equal(tail(file1$ballot_position), c("19F", "19F", "19F", "19F", "19F", "19F"))

  # First 6 votes
  expect_equal(head(file2$ballot_position), c("19F", "19F", "19F", "19F", "19H", "19F"))
  # Last 6 votes
  expect_equal(tail(file2$ballot_position), c("19F", "19H", "19H", "19F", "19F", "19H"))

  # First 6 votes
  expect_equal(head(file3$ballot_position), c("19F", "19F", "19F", "19F", "19F", "19G"))
  # Last 6 votes
  expect_equal(tail(file3$ballot_position), c("19F", "19F", "19F", "19F", "19F", "19G"))

  # First 6 votes
  expect_equal(head(file4$ballot_position), c("21H", "21H", "21H", "21H", "21H", "21H"))
  # Last 6 votes
  expect_equal(tail(file4$ballot_position), c("19F", "19F", "19F", "19F", "19F", "19F"))


})

test_that("unique IDs in right order", {
  expect_equal(head(file1$uniqueID), c("020121 1", "020121 2", "020122 1",
                                       "020122 2", "020122 3", "020122 4"))
  expect_equal(tail(file1$uniqueID), c("023633 41", "023633 42", "023633 43",
                                       "023633 44", "023633 45", "023633 46"))

  expect_equal(head(file2$uniqueID), c("020001 1", "020001 2", "020001 3",
                                       "020001 4", "020001 5", "020001 6"))
  expect_equal(tail(file2$uniqueID), c("023722 44", "023722 45", "023722 46",
                                       "023722 47", "023722 48", "023722 49"))

  expect_equal(head(file3$uniqueID), c("023635 1", "023635 2", "023635 3",
                                       "023635 4", "023635 5", "023635 6"))
  expect_equal(tail(file3$uniqueID), c("023694 40", "023694 41", "023694 42",
                                       "023694 43", "023694 44", "023694 45"))

  expect_equal(head(file4$uniqueID), c("020029 1", "020029 2", "020029 3",
                                       "020029 4", "020029 5", "020029 6"))
  expect_equal(tail(file4$uniqueID), c("020120 23", "020120 24", "020120 25",
                                       "020120 26", "020120 27", "020120 28"))

})

test_that("voter number in right order", {
  expect_equal(head(file1$voter_record), c(1, 2, 1, 2, 3, 4))
  expect_equal(tail(file1$voter_record), c(41, 42, 43, 44, 45, 46))

  expect_equal(head(file2$voter_record), c(1, 2, 3, 4, 5, 6))
  expect_equal(tail(file2$voter_record), c(44, 45, 46, 47, 48, 49))

  expect_equal(head(file3$voter_record), c(1, 2, 3, 4, 5, 6))
  expect_equal(tail(file3$voter_record), c(40, 41, 42, 43, 44, 45))

  expect_equal(head(file4$voter_record), c(1, 2, 3, 4, 5, 6))
  expect_equal(tail(file4$voter_record), c(23, 24, 25, 26, 27, 28))

})



test_that("proper number of unique ID for a location and serial number match", {

  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020122"])), 18)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020121"])), 2)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020156"])), 30)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020158"])), 40)

  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "023722"])), 49)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "023704"])), 28)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "020016"])), 30)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "020001"])), 39)

  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "023635"])), 27)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "023654"])), 40)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "023664"])), 10)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "023694"])), 45)

  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020049"])), 64)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020048"])), 42)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020063"])), 133)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020120"])), 28)

})

test_that("unique voters have correct number of files recorded", {
  expect_equal(nrow(file1[file1$uniqueID == "020121 1",]), 1)
  expect_equal(nrow(file1[file1$uniqueID == "020121 2",]), 1)
  expect_equal(nrow(file1[file1$uniqueID == "020158 1",]), 1)
  expect_equal(nrow(file1[file1$uniqueID == "020158 20",]), 1)

  expect_equal(nrow(file2[file2$uniqueID == "023722 45",]), 1)
  expect_equal(nrow(file2[file2$uniqueID == "023704 8",]), 1)
  expect_equal(nrow(file2[file2$uniqueID == "023704 1",]), 1)
  expect_equal(nrow(file2[file2$uniqueID == "020016 29",]), 1)

  expect_equal(nrow(file3[file3$uniqueID == "023635 1",]), 1)
  expect_equal(nrow(file3[file3$uniqueID == "023635 26",]), 1)
  expect_equal(nrow(file3[file3$uniqueID == "023645 17",]), 1)
  expect_equal(nrow(file3[file3$uniqueID == "023654 17",]), 1)

  expect_equal(nrow(file4[file4$uniqueID == "020120 1",]), 1)
  expect_equal(nrow(file4[file4$uniqueID == "020120 28",]), 1)
  expect_equal(nrow(file4[file4$uniqueID == "020063 98",]), 1)
  expect_equal(nrow(file4[file4$uniqueID == "020063 99",]), 1)


})


test_that("Candidate has right name", {

  expect_equal(file1$candidate[file1$uniqueID == "020121 1"],
               c("Lynwood Savage"))
  expect_equal(file1$candidate[file1$uniqueID == "020121 2"],
               c("Lynwood Savage"))
  expect_equal(file1$candidate[file1$uniqueID == "020156 20"],
               c("Robert David Bedford Jr"))
  expect_equal(file1$candidate[file1$uniqueID == "020156 21"],
               c("Lynwood Savage"))

  expect_equal(file2$candidate[file2$uniqueID == "023722 45"],
               c("Leon Williams"))
  expect_equal(file2$candidate[file2$uniqueID == "023722 46"],
               c("Leon Williams"))
  expect_equal(file2$candidate[file2$uniqueID == "020016 28"],
               c("Lynwood Savage"))
  expect_equal(file2$candidate[file2$uniqueID == "020016 29"],
               c("Leon Williams"))

  expect_equal(file3$candidate[file3$uniqueID == "023635 1"],
               c("Tonyelle Cook Artis"))
  expect_equal(file3$candidate[file3$uniqueID == "023645 17"],
               c("Tonyelle Cook Artis"))
  expect_equal(file3$candidate[file3$uniqueID == "023654 17"],
               c("Tonyelle Cook Artis"))
  expect_equal(file3$candidate[file3$uniqueID == "023654 18"],
               c("Tonyelle Cook Artis"))

  expect_equal(file4$candidate[file4$uniqueID == "020120 28"],
               c("Tonyelle Cook Artis"))
  expect_equal(file4$candidate[file4$uniqueID == "020063 98"],
               c("Tonyelle Cook Artis"))
  expect_equal(file4$candidate[file4$uniqueID == "020063 99"],
               c("Write In"))
  expect_equal(file4$candidate[file4$uniqueID == "020049 1"],
               c("Tonyelle Cook Artis"))

})


test_that("Categories are correct", {

  expect_equal(file1$category[file1$uniqueID == "020121 1"],
               c("Representative in the General Assembly - 192nd District"))
  expect_equal(file1$category[file1$uniqueID == "020121 2"],
               c("Representative in the General Assembly - 192nd District"))
  expect_equal(file1$category[file1$uniqueID == "020156 20"],
               c("Representative in the General Assembly - 192nd District"))
  expect_equal(file1$category[file1$uniqueID == "020156 21"],
               c("Representative in the General Assembly - 192nd District"))

  expect_equal(file2$category[file2$uniqueID == "023722 45"],
               c("Representative in the General Assembly - 192nd District"))
  expect_equal(file2$category[file2$uniqueID == "023722 46"],
               c("Representative in the General Assembly - 192nd District"))
  expect_equal(file2$category[file2$uniqueID == "020016 28"],
               c("Representative in the General Assembly - 192nd District"))
  expect_equal(file2$category[file2$uniqueID == "020016 29"],
               c("Representative in the General Assembly - 192nd District"))

  expect_equal(file3$category[file3$uniqueID == "023635 1"],
               c("Representative in the General Assembly - 200th District"))
  expect_equal(file3$category[file3$uniqueID == "023645 17"],
               c("Representative in the General Assembly - 200th District"))
  expect_equal(file3$category[file3$uniqueID == "023654 17"],
               c("Representative in the General Assembly - 200th District"))
  expect_equal(file3$category[file3$uniqueID == "023654 18"],
               c("Representative in the General Assembly - 200th District"))

  expect_equal(file4$category[file4$uniqueID == "020120 28"],
               c("Representative in the General Assembly - 200th District"))
  expect_equal(file4$category[file4$uniqueID == "020063 98"],
               c("Representative in the General Assembly - 200th District"))
  expect_equal(file4$category[file4$uniqueID == "020063 99"],
               c("Representative in the General Assembly - 200th District"))
  expect_equal(file4$category[file4$uniqueID == "020049 1"],
               c("Representative in the General Assembly - 200th District"))


})


# No change needed here ---------------------------------------------------

test_that("No commas in candidate names", {
  expect_false(all(grepl(",", file1$candidate)))
  expect_false(all(grepl(",", file2$candidate)))
  expect_false(all(grepl(",", file3$candidate)))
  expect_false(all(grepl(",", file4$candidate)))
})


# test_that("all votes are equal to 1", {
#   expect_true(unique(file1$votes) %in% c(1, NA))
#   expect_true(unique(file2$votes) %in% c(1, NA))
#   expect_true(unique(file3$votes) %in% c(1, NA))
#   expect_true(unique(file4$votes) %in% c(1, NA))
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
})

