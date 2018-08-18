context("election-2015-march-special")

file1 <- system.file("data/PDF_data/2015 March Special",
                     "2015 March Special BIR.pdf",
                     package = "phillyvotes")

file1 <- philly_votes(file1)

test_that("right file name", {
  expect_equal(unique(file1$file), "2015 March Special BIR")
})


test_that("right number of pages", {
  expect_equal(max(file1$pdf_page), 816)
})

test_that("ballot ballot_position values are correct", {
  # First 6 votes
  expect_equal(head(file1$ballot_position), c("19F", "19G", "19F", "19G", "19F", "19F"))
  # Last 6 votes
  expect_equal(tail(file1$ballot_position), c("19G", "19F", "19G", "19F", "19G", "19F"))

})

test_that("unique IDs in right order", {
  expect_equal(unique(head(file1$uniqueID)), c("023623 1", "023623 2", "023623 3",
                                               "023623 4", "023623 5", "023623 6"))
  expect_equal(unique(tail(file1$uniqueID)), c("023560 87", "023560 88", "023560 89",
                                               "023560 90", "023560 91", "023560 92"))
})

test_that("voter number in right order", {
  expect_equal(head(file1$voter_record), c(1, 2, 3, 4, 5, 6))
  expect_equal(tail(file1$voter_record), c(87, 88, 89, 90, 91, 92))

})



test_that("proper number of unique ID for a location and serial number match", {

  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "023623"])), 30)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "023624"])), 16)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "023625"])), 12)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "023626"])), 12)

})

test_that("unique voters have correct number of files recorded", {
  expect_equal(nrow(file1[file1$uniqueID == "023623 1",]), 1)
  expect_equal(nrow(file1[file1$uniqueID == "023623 2",]), 1)
  expect_equal(nrow(file1[file1$uniqueID == "023623 3",]), 1)
  expect_equal(nrow(file1[file1$uniqueID == "023623 4",]), 1)


})


test_that("Candidate has right name", {

  expect_equal(file1$candidate[file1$uniqueID == "023626 5"],
               c("Martina White"))
  expect_equal(file1$candidate[file1$uniqueID == "023626 6"],
               c("Sarah Del Ricci"))
  expect_equal(file1$candidate[file1$uniqueID == "023560 91"],
               c("Martina White"))
  expect_equal(file1$candidate[file1$uniqueID == "023560 92"],
               c("Sarah Del Ricci"))

})


test_that("Categories are correct", {

  expect_equal(file1$category[file1$uniqueID == "023626 5"],
               c("Representative in the General Assembly - 170th District"))
  expect_equal(file1$category[file1$uniqueID == "023626 6"],
               c("Representative in the General Assembly - 170th District"))
  expect_equal(file1$category[file1$uniqueID == "023560 91"],
               c("Representative in the General Assembly - 170th District"))
  expect_equal(file1$category[file1$uniqueID == "023560 92"],
               c("Representative in the General Assembly - 170th District"))

})


# No change needed here ---------------------------------------------------

test_that("No commas in candidate names", {
  expect_false(all(grepl(",", file1$candidate)))
})


# test_that("all votes are equal to 1", {
#   expect_true(unique(file1$votes) %in% c(1, NA))
# })



test_that("Ballots with No Vote all have position 43A", {
  if (length(unique(file1$ballot_position[file1$candidate == "No Vote"])) > 0) {
    expect_equal(unique(file1$ballot_position[file1$candidate == "No Vote"]),
                 "43A")
  }
})


test_that("Ballots with No Vote all have category NA", {
  if (length(unique(file1$category[file1$candidate == "No Vote"])) > 0) {
    expect_true(is.na(unique(file1$category[file1$candidate == "No Vote"])))
  }
})


test_that("All rows have location values", {
  expect_false(any(is.na(file1$location)))
  expect_false(any(is.na(file1$ward)))
  expect_false(any(is.na(file1$division)))
})

