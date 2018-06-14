context("election-2012-general")

file1 <- system.file("data/PDF_data/201",
                     "",
                     package = "phillyfile")
file2 <- system.file("data/PDF_data/201",
                     "",
                     package = "phillyfile")
file3 <- system.file("data/PDF_data/201",
                     "",
                     package = "phillyfile")
file4 <- system.file("data/PDF_data/201",
                     "",
                     package = "phillyfile")
file5 <- system.file("data/PDF_data/201",
                     "",
                     package = "phillyfile")

file1 <- philly_file(file1)
file2 <- philly_file(file2)
file3 <- philly_file(file3)
file4 <- philly_file(file4)
file5 <- philly_file(file5)

test_that("right file name", {
  expect_equal(unique(file1$file), "")
  expect_equal(unique(file2$file), "")
  expect_equal(unique(file3$file), "")
  expect_equal(unique(file4$file), "")
  expect_equal(unique(file5$file), "")
})


test_that("right number of pages", {
  expect_equal(max(file1$pdf_page), )
  expect_equal(max(file2$pdf_page), )
  expect_equal(max(file3$pdf_page), )
  expect_equal(max(file4$pdf_page), )
  expect_equal(max(file5$pdf_page), )
})




test_that("ballot ballot_position values are correct", {
  # First 6 votes
  expect_equal(head(file1$ballot_position), c("", "", "", "", "", ""))
  # Last 6 votes
  expect_equal(tail(file1$ballot_position), c("", "", "", "", "", ""))

  # First 6 votes
  expect_equal(head(file2$ballot_position), c("", "", "", "", "", ""))
  # Last 6 votes
  expect_equal(tail(file2$ballot_position), c("", "", "", "", "", ""))

  # First 6 votes
  expect_equal(head(file3$ballot_position), c("", "", "", "", "", ""))
  # Last 6 votes
  expect_equal(tail(file3$ballot_position), c("", "", "", "", "", ""))

  # First 6 votes
  expect_equal(head(file4$ballot_position), c("", "", "", "", "", ""))
  # Last 6 votes
  expect_equal(tail(file4$ballot_position), c("", "", "", "", "", ""))

  # First 6 votes
  expect_equal(head(file5$ballot_position), c("", "", "", "", "", ""))
  # Last 6 votes
  expect_equal(tail(file5$ballot_position), c("", "", "", "", "", ""))

})

test_that("unique IDs in right order", {
  expect_equal(unique(head(file1$uniqueID)), c("", "", "",
                                               "", "", ""))
  expect_equal(unique(tail(file1$uniqueID)), c("", "", "",
                                               "", "", ""))

  expect_equal(unique(head(file2$uniqueID)), c("", "", "",
                                               "", "", ""))
  expect_equal(unique(tail(file2$uniqueID)), c("", "", "",
                                               "", "", ""))

  expect_equal(unique(head(file3$uniqueID)), c("", "", "",
                                               "", "", ""))
  expect_equal(unique(tail(file3$uniqueID)), c("", "", "",
                                               "", "", ""))

  expect_equal(unique(head(file4$uniqueID)), c("", "", "",
                                               "", "", ""))
  expect_equal(unique(tail(file4$uniqueID)), c("", "", "",
                                               "", "", ""))

  expect_equal(unique(head(file5$uniqueID)), c("", "", "",
                                               "", "", ""))
  expect_equal(unique(tail(file5$uniqueID)), c("", "", "",
                                               "", "", ""))
})




test_that("proper number of unique ID for a location and serial number match", {

  expect_equal(length(unique(file1$uniqueID[file1$serial_number == ""])), )
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == ""])), )
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == ""])), )
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == ""])), )

  expect_equal(length(unique(file2$uniqueID[file2$serial_number == ""])), )
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == ""])), )
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == ""])), )
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == ""])), )

  expect_equal(length(unique(file3$uniqueID[file3$serial_number == ""])), )
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == ""])), )
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == ""])), )
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == ""])), )

  expect_equal(length(unique(file4$uniqueID[file4$serial_number == ""])), )
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == ""])), )
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == ""])), )
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == ""])), )

  expect_equal(length(unique(file5$uniqueID[file5$serial_number == ""])), )
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == ""])), )
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == ""])), )
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == ""])), )
})

test_that("No commas in candidate names", {
  expect_false(all(grepl(",", file1$candidate)))
  expect_false(all(grepl(",", file2$candidate)))
  expect_false(all(grepl(",", file3$candidate)))
  expect_false(all(grepl(",", file4$candidate)))
  expect_false(all(grepl(",", file5$candidate)))
})



test_that("Candidate has right name", {

  expect_equal(file1$candidate[file1$uniqueID == "", ],
               c(""))
  expect_equal(file1$candidate[file1$uniqueID == "", ],
               c(""))
  expect_equal(file1$candidate[file1$uniqueID == "", ],
               c(""))
  expect_equal(file1$candidate[file1$uniqueID == "", ],
               c(""))
  expect_equal(file2$candidate[file2$uniqueID == "", ],
               c(""))
  expect_equal(file2$candidate[file2$uniqueID == "", ],
               c(""))
  expect_equal(file2$candidate[file2$uniqueID == "", ],
               c(""))
  expect_equal(file2$candidate[file2$uniqueID == "", ],
               c(""))
  expect_equal(file3$candidate[file3$uniqueID == "", ],
               c(""))
  expect_equal(file3$candidate[file3$uniqueID == "", ],
               c(""))
  expect_equal(file3$candidate[file3$uniqueID == "", ],
               c(""))
  expect_equal(file3$candidate[file3$uniqueID == "", ],
               c(""))
  expect_equal(file4$candidate[file4$uniqueID == "", ],
               c(""))
  expect_equal(file4$candidate[file4$uniqueID == "", ],
               c(""))
  expect_equal(file4$candidate[file4$uniqueID == "", ],
               c(""))
  expect_equal(file4$candidate[file4$uniqueID == "", ],
               c(""))
  expect_equal(file5$candidate[file5$uniqueID == "", ],
               c(""))
  expect_equal(file5$candidate[file5$uniqueID == "", ],
               c(""))
  expect_equal(file5$candidate[file5$uniqueID == "", ],
               c(""))
  expect_equal(file5$candidate[file5$uniqueID == "", ],
               c(""))
})


test_that("Categories are correct", {

  expect_equal(file1$category[file1$uniqueID == "", ],
               c(""))
  expect_equal(file1$category[file1$uniqueID == "", ],
               c(""))
  expect_equal(file1$category[file1$uniqueID == "", ],
               c(""))
  expect_equal(file1$category[file1$uniqueID == "", ],
               c(""))
  expect_equal(file2$category[file2$uniqueID == "", ],
               c(""))
  expect_equal(file2$category[file2$uniqueID == "", ],
               c(""))
  expect_equal(file2$category[file2$uniqueID == "", ],
               c(""))
  expect_equal(file2$category[file2$uniqueID == "", ],
               c(""))
  expect_equal(file3$category[file3$uniqueID == "", ],
               c(""))
  expect_equal(file3$category[file3$uniqueID == "", ],
               c(""))
  expect_equal(file3$category[file3$uniqueID == "", ],
               c(""))
  expect_equal(file3$category[file3$uniqueID == "", ],
               c(""))
  expect_equal(file4$category[file4$uniqueID == "", ],
               c(""))
  expect_equal(file4$category[file4$uniqueID == "", ],
               c(""))
  expect_equal(file4$category[file4$uniqueID == "", ],
               c(""))
  expect_equal(file4$category[file4$uniqueID == "", ],
               c(""))
  expect_equal(file5$category[file5$uniqueID == "", ],
               c(""))
  expect_equal(file5$category[file5$uniqueID == "", ],
               c(""))
  expect_equal(file5$category[file5$uniqueID == "", ],
               c(""))
  expect_equal(file5$category[file5$uniqueID == "", ],
               c(""))

})


# No change needed here ---------------------------------------------------



test_that("unique voters have correct number of files recorded", {
  expect_equal(nrow(file1[file1$uniqueID == "",]), )
  expect_equal(nrow(file1[file1$uniqueID == "",]), )
  expect_equal(nrow(file1[file1$uniqueID == "",]), )
  expect_equal(nrow(file1[file1$uniqueID == "",]), )
  expect_equal(nrow(file2[file2$uniqueID == "",]), )
  expect_equal(nrow(file2[file2$uniqueID == "",]), )
  expect_equal(nrow(file2[file2$uniqueID == "",]), )
  expect_equal(nrow(file2[file2$uniqueID == "",]), )
  expect_equal(nrow(file3[file3$uniqueID == "",]), )
  expect_equal(nrow(file3[file3$uniqueID == "",]), )
  expect_equal(nrow(file3[file3$uniqueID == "",]), )
  expect_equal(nrow(file3[file3$uniqueID == "",]), )
  expect_equal(nrow(file4[file4$uniqueID == "",]), )
  expect_equal(nrow(file4[file4$uniqueID == "",]), )
  expect_equal(nrow(file4[file4$uniqueID == "",]), )
  expect_equal(nrow(file4[file4$uniqueID == "",]), )
  expect_equal(nrow(file5[file5$uniqueID == "",]), )
  expect_equal(nrow(file5[file5$uniqueID == "",]), )
  expect_equal(nrow(file5[file5$uniqueID == "",]), )
  expect_equal(nrow(file5[file5$uniqueID == "",]), )

})

test_that("all votes are equal to 1", {
  expect_equal(unique(file1$votes), c(1, NA))
  expect_equal(unique(file2$votes), c(1, NA))
  expect_equal(unique(file3$votes), c(1, NA))
  expect_equal(unique(file4$votes), c(1, NA))
  expect_equal(unique(file5$votes), c(1, NA))
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
