context("election-2015-august-special")

file1 <- system.file("data/PDF_data/2015 August Special",
                     "2015 August Special - 174th BIR.pdf",
                     package = "phillyvotes")
file2 <- system.file("data/PDF_data/2015 August Special",
                     "2015 August Special - 191st BIR.pdf",
                     package = "phillyvotes")
file3 <- system.file("data/PDF_data/2015 August Special",
                     "2015 August Special - 195th BIR.pdf",
                     package = "phillyvotes")

file1 <- philly_votes(file1)
file2 <- philly_votes(file2)
file3 <- philly_votes(file3)

test_that("right file name", {
  expect_equal(unique(file1$file), "2015 August Special - 174th BIR")
  expect_equal(unique(file2$file), "2015 August Special - 191st BIR")
  expect_equal(unique(file3$file), "2015 August Special - 195th BIR")
})


test_that("right number of pages", {
  expect_equal(max(file1$pdf_page), 481)
  expect_equal(max(file2$pdf_page), 270)
  expect_equal(max(file3$pdf_page), 416)
})




test_that("ballot ballot_position values are correct", {
  # First 6 votes
  expect_equal(head(file1$ballot_position), c("19F", "19F", "19F", "19F", "19F", "19G"))
  # Last 6 votes
  expect_equal(tail(file1$ballot_position), c("19F", "19F", "19F", "19F", "19G", "19F"))

  # First 6 votes
  expect_equal(head(file2$ballot_position), c("20F", "20F", "20F", "20F", "20F", "20F"))
  # Last 6 votes
  expect_equal(tail(file2$ballot_position), c("20F", "20F", "20F", "20F", "20F", "20G"))

  # First 6 votes
  expect_equal(head(file3$ballot_position), c("19F", "19F", "19F", "19F", "19F", "19G"))
  # Last 6 votes
  expect_equal(tail(file3$ballot_position), c("19F", "19F", "19F", "19F", "19G", "19F"))


})

test_that("unique IDs in right order", {
  expect_equal(unique(head(file1$uniqueID)), c("020285 1", "020285 2", "020285 3",
                                               "020285 4", "020285 5", "020285 6"))
  expect_equal(unique(tail(file1$uniqueID)), c("020415 76", "020415 77", "020415 78",
                                               "020415 79", "020415 80", "020415 81"))

  expect_equal(unique(head(file2$uniqueID)), c("020002 1", "020002 2", "020002 3",
                                               "020002 4", "020002 5", "020002 6"))
  expect_equal(unique(tail(file2$uniqueID)), c("020401 34", "020401 35", "020401 36",
                                               "020401 37", "020402 1", "020402 2"))

  expect_equal(unique(head(file3$uniqueID)), c("020045 1", "020045 2", "020045 3",
                                               "020045 4", "020045 5", "020045 6"))
  expect_equal(unique(tail(file3$uniqueID)), c("020230 5", "020230 6", "020230 7",
                                               "020109 1", "020109 2", "020109 3"))

})

test_that("voter number in right order", {
  expect_equal(unique(head(file1$voter_record)), c(1, 2, 3, 4, 5, 6))
  expect_equal(unique(tail(file1$voter_record)), c(76, 77, 78, 79, 80, 81))

  expect_equal(unique(head(file2$voter_record)), c(1, 2, 3, 4, 5, 6))
  expect_equal(unique(tail(file2$voter_record)), c(34, 35, 36, 37, 1, 2))

  expect_equal(unique(head(file3$voter_record)), c(1, 2, 3, 4, 5, 6))
  expect_equal(unique(tail(file3$voter_record)), c(5, 6, 7, 1, 2, 3))
})



test_that("proper number of unique ID for a location and serial number match", {

  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020415"])), 81)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020377"])), 42)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020305"])), 19)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020285"])), 27)

  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "020401"])), 37)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "020402"])), 2)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "020240"])), 8)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "020239"])), 8)

  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "020230"])), 7)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "020045"])), 41)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "020072"])), 22)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "020118"])), 18)

})

test_that("unique voters have correct number of files recorded", {
  expect_equal(nrow(file1[file1$uniqueID == "020415 80",]), 1)
  expect_equal(nrow(file1[file1$uniqueID == "020415 81",]), 1)
  expect_equal(nrow(file1[file1$uniqueID == "020377 25",]), 1)
  expect_equal(nrow(file1[file1$uniqueID == "020377 26",]), 1)

  expect_equal(nrow(file2[file2$uniqueID == "020401 34",]), 1)
  expect_equal(nrow(file2[file2$uniqueID == "020401 35",]), 1)
  expect_equal(nrow(file2[file2$uniqueID == "020402 1",]), 1)
  expect_equal(nrow(file2[file2$uniqueID == "020402 2",]), 1)

  expect_equal(nrow(file3[file3$uniqueID == "020230 6",]), 1)
  expect_equal(nrow(file3[file3$uniqueID == "020230 7",]), 1)
  expect_equal(nrow(file3[file3$uniqueID == "020045 1",]), 1)
  expect_equal(nrow(file3[file3$uniqueID == "020045 2",]), 1)


})


test_that("Candidate has right name", {

  expect_equal(file1$candidate[file1$uniqueID == "020285 1"],
               c("Ed Neilson"))
  expect_equal(file1$candidate[file1$uniqueID == "020285 2"],
               c("Ed Neilson"))
  expect_equal(file1$candidate[file1$uniqueID == "020322 8"],
               c("Timothy Dailey"))
  expect_equal(file1$candidate[file1$uniqueID == "020322 9"],
               c("Ed Neilson"))

  expect_equal(file2$candidate[file2$uniqueID == "020401 34"],
               c("Joanna E Mcclinton"))
  expect_equal(file2$candidate[file2$uniqueID == "020401 35"],
               c("Joanna E Mcclinton"))
  expect_equal(file2$candidate[file2$uniqueID == "020402 1"],
               c("Joanna E Mcclinton"))
  expect_equal(file2$candidate[file2$uniqueID == "020402 2"],
               c("Charles A Wilkins Jr"))

  expect_equal(file3$candidate[file3$uniqueID == "020230 6"],
               c("Donna Bullock"))
  expect_equal(file3$candidate[file3$uniqueID == "020230 7"],
               c("Donna Bullock"))
  expect_equal(file3$candidate[file3$uniqueID == "020045 1"],
               c("Donna Bullock"))
  expect_equal(file3$candidate[file3$uniqueID == "020045 1"],
               c("Donna Bullock"))

})


test_that("Categories are correct", {

  expect_equal(file1$category[file1$uniqueID == "020322 8"],
               c("Representative in the General Assembly - 174th District"))
  expect_equal(file1$category[file1$uniqueID == "020322 9"],
               c("Representative in the General Assembly - 174th District"))
  expect_equal(file1$category[file1$uniqueID == "020415 80"],
               c("Representative in the General Assembly - 174th District"))
  expect_equal(file1$category[file1$uniqueID == "020415 81"],
               c("Representative in the General Assembly - 174th District"))

  expect_equal(file2$category[file2$uniqueID == "020401 34"],
               c("Representative in the General Assembly - 191st District"))
  expect_equal(file2$category[file2$uniqueID == "020401 35"],
               c("Representative in the General Assembly - 191st District"))
  expect_equal(file2$category[file2$uniqueID == "020402 1"],
               c("Representative in the General Assembly - 191st District"))
  expect_equal(file2$category[file2$uniqueID == "020402 2"],
               c("Representative in the General Assembly - 191st District"))

  expect_equal(file3$category[file3$uniqueID == "020045 1"],
               c("Representative in the General Assembly - 195th District"))
  expect_equal(file3$category[file3$uniqueID == "020045 2"],
               c("Representative in the General Assembly - 195th District"))
  expect_equal(file3$category[file3$uniqueID == "020074 11"],
               c("Representative in the General Assembly - 195th District"))
  expect_equal(file3$category[file3$uniqueID == "020142 11"],
               c("Representative in the General Assembly - 195th District"))

})


# No change needed here ---------------------------------------------------

test_that("No commas in candidate names", {
  expect_false(all(grepl(",", file1$candidate)))
  expect_false(all(grepl(",", file2$candidate)))
  expect_false(all(grepl(",", file3$candidate)))
})

#
# test_that("all votes are equal to 1", {
#   expect_true(unique(file1$votes) %in% c(1, NA))
#   expect_true(unique(file2$votes) %in% c(1, NA))
#   expect_true(unique(file3$votes) %in% c(1, NA))
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

})
