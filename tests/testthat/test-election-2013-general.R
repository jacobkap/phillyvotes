context("election-2013-general")

file1 <- system.file("data/PDF_data/2013 General",
                     "2013 General 1 of 7 BIR.pdf",
                     package = "phillyvotes")
file2 <- system.file("data/PDF_data/2013 General",
                     "2013 General 3 of 7 BIR.pdf",
                     package = "phillyvotes")
file3 <- system.file("data/PDF_data/2013 General",
                     "2013 General 2 of 7 BIR.pdf",
                     package = "phillyvotes")
file4 <- system.file("data/PDF_data/2013 General",
                     "2013 General 4 of 7 BIR.pdf",
                     package = "phillyvotes")
file5 <- system.file("data/PDF_data/2013 General",
                     "2013 General 7 of 7 BIR.pdf",
                     package = "phillyvotes")

file1 <- philly_votes(file1)
file2 <- philly_votes(file2)
file3 <- philly_votes(file3)
file4 <- philly_votes(file4)
file5 <- philly_votes(file5)

test_that("right file name", {
  expect_equal(unique(file1$file), "2013 General 1 of 7 BIR")
  expect_equal(unique(file2$file), "2013 General 3 of 7 BIR")
  expect_equal(unique(file3$file), "2013 General 2 of 7 BIR")
  expect_equal(unique(file4$file), "2013 General 4 of 7 BIR")
  expect_equal(unique(file5$file), "2013 General 7 of 7 BIR")
})


test_that("right number of pages", {
  expect_equal(max(file1$pdf_page), 7540)
  expect_equal(max(file2$pdf_page), 6312)
  expect_equal(max(file3$pdf_page), 15653)
  expect_equal(max(file4$pdf_page), 4812)
  expect_equal(max(file5$pdf_page), 697)
})

test_that("unique voters have correct number of files recorded", {
  expect_equal(nrow(file1[file1$uniqueID == "020016 1",]), 1)
  expect_equal(nrow(file1[file1$uniqueID == "020016 2",]), 2)
  expect_equal(nrow(file1[file1$uniqueID == "020016 4",]), 11)
  expect_equal(nrow(file1[file1$uniqueID == "021834 8",]), 5)

  expect_equal(nrow(file2[file2$uniqueID == "022869 11",]), 5)
  expect_equal(nrow(file2[file2$uniqueID == "022869 24",]), 7)
  expect_equal(nrow(file2[file2$uniqueID == "022903 31",]), 5)
  expect_equal(nrow(file2[file2$uniqueID == "022721 4",]), 1)

  expect_equal(nrow(file3[file3$uniqueID == "023295 39",]), 5)
  expect_equal(nrow(file3[file3$uniqueID == "023295 40",]), 7)
  expect_equal(nrow(file3[file3$uniqueID == "021525 1",]), 5)
  expect_equal(nrow(file3[file3$uniqueID == "021524 42",]), 14)

  expect_equal(nrow(file4[file4$uniqueID == "020323 8",]), 6)
  expect_equal(nrow(file4[file4$uniqueID == "020323 17",]), 16)
  expect_equal(nrow(file4[file4$uniqueID == "023408 27",]), 6)
  expect_equal(nrow(file4[file4$uniqueID == "020646 63",]), 13)

  expect_equal(nrow(file5[file5$uniqueID == "023340 63",]), 8)
  expect_equal(nrow(file5[file5$uniqueID == "023340 64",]), 6)
  expect_equal(nrow(file5[file5$uniqueID == "021298 11",]), 4)
  expect_equal(nrow(file5[file5$uniqueID == "021298 15",]), 2)

})

# test_that("all votes are equal to 1", {
#   expect_true(unique(file1$votes) %in% c(1, NA))
#   expect_true(unique(file2$votes) %in% c(1, NA))
#   expect_true(unique(file3$votes) %in% c(1, NA))
#   expect_true(unique(file4$votes) %in% c(1, NA))
#   expect_true(unique(file5$votes) %in% c(1, NA))
# })


test_that("ballot ballot_position values are correct", {
  # First 6 votes
  expect_equal(head(file1$ballot_position), c("25C", "13D", "14D", "26J", "37I", "36L"))
  # Last 6 votes
  expect_equal(tail(file1$ballot_position), c("14D", "15D", "16D", "17D", "4D", "9D"))

  # First 6 votes
  expect_equal(head(file2$ballot_position), c("26L", "26J", "36L", "5J", "30I", "18J"))
  # Last 6 votes
  expect_equal(tail(file2$ballot_position), c("23C", "37C", "7C", "4C", "20C", "17C"))

  # First 6 votes
  expect_equal(head(file3$ballot_position), c("36L", "30J", "36J", "18K", "22K", "26K"))
  # Last 6 votes
  expect_equal(tail(file3$ballot_position), c("18I", "20D", "17D", "16D", "15D", "14D"))

  # First 6 votes
  expect_equal(head(file4$ballot_position), c("22L", "30I", "36I", "6J", "11J", "18J"))
  # Last 6 votes
  expect_equal(tail(file4$ballot_position), c("17C", "20C", "8C", "7C", "4C", "38C"))

  # First voter
  expect_equal(head(file5$ballot_position), c("4C", "7C", "8C", "20C", "8D", "9D"))
  # Last 6 votes
  expect_equal(tail(file5$ballot_position), c("26K", "36K", "18L", "22L", "26L", "36L"))
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
  expect_equal(head(file1$voter_record), c(1, 2, 2, 3, 3, 3))
  expect_equal(tail(file1$voter_record), c(89, 89, 89, 89, 89, 89))

  expect_equal(head(file2$voter_record), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file2$voter_record), c(46, 46, 46, 46, 46, 46))

  expect_equal(head(file3$voter_record), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file3$voter_record), c(6, 6, 6, 6, 6, 6))

  expect_equal(head(file4$voter_record), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file4$voter_record), c(1, 1, 1, 1, 1, 1))

  expect_equal(head(file5$voter_record), c(1, 1, 1, 1, 1, 1))
  expect_equal(tail(file5$voter_record), c(65, 65, 65, 65, 65, 65))
})


test_that("Candidate has right name", {

  expect_equal(file1$candidate[file1$uniqueID == "022940 88"],
               c("Daniel A Alvarez",
                 "Kenneth J Powell Jr",
                 "Vic Stabile",
                 "Anne Marie Coyle",
                 "Terrence J Tracy Jr"))
  expect_equal(file1$candidate[file1$uniqueID == "022940 84"],
               c("Vic Stabile",
                 "Terrence J Tracy Jr",
                 "Castille Yes Si",
                 "Anne Marie Coyle",
                 "Daniel A Alvarez",
                 "Kenneth J Powell Jr"))
  expect_equal(file1$candidate[file1$uniqueID == "020016 1"],
               c("Charles A Sulpizio"))
  expect_equal(file1$candidate[file1$uniqueID == "021641 26"],
               c("No Vote"))
  expect_equal(file2$candidate[file2$uniqueID == "022869 11"],
               c("Henry Lewandowski",
                 "Alan Butkovitz",
                 "Daniel A Alvarez",
                 "Castille Yes Si",
                 "Daniel D Mccaffery"))
  expect_equal(file2$candidate[file2$uniqueID == "022869 24"],
               c("No No",
                 "Daniel A Alvarez",
                 "Castille Yes Si",
                 "Baer Yes Si",
                 "Panella Yes Si",
                 "Terrence J Tracy Jr",
                 "Gantman Yes Si"))
  expect_equal(file2$candidate[file2$uniqueID == "022903 31"],
               c("Daniel A Alvarez",
                 "Terrence J Tracy Jr",
                 "Vic Stabile",
                 "Kenneth J Powell Jr",
                 "Anne Marie Coyle"))
  expect_equal(file2$candidate[file2$uniqueID == "022904 1"],
               c("Gantman Yes Si",
                 "Vic Stabile",
                 "Castille Yes Si"))
  expect_equal(file3$candidate[file3$uniqueID == "020001 2"],
               c("Terrence J Tracy Jr",
                 "Castille No No",
                 "Yes Si"))
  expect_equal(file3$candidate[file3$uniqueID == "023295 39"],
               c("Yes Si",
                 "R Seth Williams",
                 "Castille Yes Si",
                 "O Keefe Yes Si",
                 "O Neill Yes Si"))
  expect_equal(file3$candidate[file3$uniqueID == "023295 40"],
               c("Anne Marie Coyle",
                 "No No",
                 "John Kilbride",
                 "Theresa Olsen",
                 "Terrence J Tracy Jr",
                 "Kenneth J Powell Jr",
                 "Daniel A Alvarez"))
  expect_equal(file3$candidate[file3$uniqueID == "021525 1"],
               c("Sierra Thomas Street",
                 "R Seth Williams",
                 "Jack Mcvay Jr",
                 "Yes Si",
                 "Alan Butkovitz"))
  expect_equal(file4$candidate[file4$uniqueID == "020644 18"],
               c("Stephen Miller Miller"))
  expect_equal(file4$candidate[file4$uniqueID == "020644 15"],
               c("Castille Yes Si",
                 "Gantman Yes Si",
                 "Baer Yes Si",
                 "R Seth Williams",
                 "Yes Si",
                 "Alan Butkovitz",
                 "Panella Yes Si"))
  expect_equal(file4$candidate[file4$uniqueID == "020643 51"],
               c("R Seth Williams",
                 "Alan Butkovitz",
                 "Castille Yes Si"))
  expect_equal(file4$candidate[file4$uniqueID == "020860 1"],
               c("Daniel A Alvarez",
                 "Terrence J Tracy Jr",
                 "Kenneth J Powell Jr",
                 "Anne Marie Coyle",
                 "Vic Stabile",
                 "No No"))
  expect_equal(file5$candidate[file5$uniqueID == "022292 1"],
               c("Jack Mcvay Jr",
                 "R Seth Williams"))
  expect_equal(file5$candidate[file5$uniqueID == "023443 9"],
               c("Jack Mcvay Jr",
                 "Anne Marie Coyle",
                 "Daniel D Mccaffery",
                 "J Scott O Keefe",
                 "Henry Lewandowski",
                 "R Seth Williams",
                 "Alan Butkovitz",
                 "Tiffany Bradley",
                 "Tiffany Bradley",
                 "Castille Yes Si",
                 "Panella Yes Si",
                 "Brinkley Yes Si"))
  expect_equal(file5$candidate[file5$uniqueID == "022288 6"],
               c("Sierra Thomas Street",
                 "R Seth Williams",
                 "Tiffany Bradley",
                 "Tiffany Bradley",
                 "Frazier Lyde Yes Si"))
  expect_equal(file5$candidate[file5$uniqueID == "022288 1"],
               c("Allen Yes Si",
                 "Ford Yes Si",
                 "Deni Yes Si",
                 "Castille Yes Si",
                 "Panella Yes Si",
                 "Wright Padilla Yes Si",
                 "Patrick Yes Si"))



  expect_false(all(grepl(",", file1$candidate)))
  expect_false(all(grepl(",", file2$candidate)))
  expect_false(all(grepl(",", file3$candidate)))
  expect_false(all(grepl(",", file4$candidate)))
  expect_false(all(grepl(",", file5$candidate)))

})


test_that("Categories are correct", {

  expect_equal(file1$category[file1$uniqueID == "022940 88"],
               c("District Attorney", "Judge of the Court of Common Pleas",
                 "Judge of the Superior Court", "Judge of the Court of Common Pleas",
                 "Controller"))
  expect_equal(file1$category[file1$uniqueID == "020016 1"],
               c("Inspector of Election-01-08"))
  expect_equal(file1$category[file1$uniqueID == "020016 2"],
               c("Judge of the Court of Common Pleas",
                 "Judge of the Municipal Court"))
  expect_true(is.na(file1$category[file1$uniqueID == "021641 26"]))
  expect_equal(file2$category[file2$uniqueID == "022869 11"],
               c("Judge of the Municipal Court", "Controller",
                 "District Attorney",
                 "Retention - Justice of the Supreme Court - Ronald D Castille",
                 "Judge of the Court of Common Pleas"))
  expect_equal(file2$category[file2$uniqueID == "022869 24"],
               c("City Bond Question", "District Attorney",
                 "Retention - Justice of the Supreme Court - Ronald D Castille",
                 "Retention - Justice of the Supreme Court - Max Baer",
                 "Retention - Judge of the Superior Court - Jack Panella",
                 "Controller",
                 "Retention - Judge of the Superior Court - Susan Peikes Gant"))
  expect_equal(file2$category[file2$uniqueID == "022903 31"],
               c("District Attorney",
                 "Controller",
                 "Judge of the Superior Court",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas"))
  expect_equal(file2$category[file2$uniqueID == "022904 1"],
               c("Retention - Judge of the Superior Court - Susan Peikes Gant",
                 "Judge of the Superior Court",
                 "Retention - Justice of the Supreme Court - Ronald D Castille"))
  expect_equal(file3$category[file3$uniqueID == "020001 2"],
               c("Controller",
                 "Retention - Justice of the Supreme Court - Ronald D Castille",
                 "City Bond Question"))
  expect_equal(file3$category[file3$uniqueID == "023295 39"],
               c("City Bond Question", "District Attorney",
                 "Retention - Justice of the Supreme Court - Ronald D Castille",
                 "Retention - Judge of the Court of Common Pleas - Joseph D O K",
                 "Retention - Judge of the Municipal Court - Joseph J O Neill"))
  expect_equal(file3$category[file3$uniqueID == "023295 40"],
               c("Judge of the Court of Common Pleas", "City Bond Question",
                 "Inspector of Election-66-07", "Judge of Election-66-07",
                 "Controller", "Judge of the Court of Common Pleas",
                 "District Attorney"))
  expect_equal(file3$category[file3$uniqueID == "021525 1"],
               c("Judge of the Court of Common Pleas",
                 "District Attorney",
                 "Judge of the Superior Court",
                 "City Bond Question",
                 "Controller"))
  expect_equal(file4$category[file4$uniqueID == "020644 18"],
               c("Judge of the Court of Common Pleas"))
  expect_equal(file4$category[file4$uniqueID == "020644 15"],
               c("Retention - Justice of the Supreme Court - Ronald D Castille",
                 "Retention - Judge of the Superior Court - Susan Peikes Gant",
                 "Retention - Justice of the Supreme Court - Max Baer",
                 "District Attorney", "City Bond Question", "Controller",
                 "Retention - Judge of the Superior Court - Jack Panella"))
  expect_equal(file4$category[file4$uniqueID == "020643 51"],
               c("District Attorney", "Controller",
                 "Retention - Justice of the Supreme Court - Ronald D Castille"))
  expect_equal(file4$category[file4$uniqueID == "020860 1"],
               c("District Attorney", "Controller",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Superior Court",
                 "City Bond Question"))
  expect_equal(file5$category[file5$uniqueID == "022292 1"],
               c("Judge of the Superior Court",
                 "District Attorney"))
  expect_equal(file5$category[file5$uniqueID == "023443 9"],
               c("Judge of the Superior Court",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Municipal Court",
                 "District Attorney",
                 "Controller", "Judge of Election-47-03",
                 "Inspector of Election-47-03",
                 "Retention - Justice of the Supreme Court - Ronald D Castille",
                 "Retention - Judge of the Superior Court - Jack Panella",
                 "Retention - Judge of the Court of Common Pleas - Genece E B"))
  expect_equal(file5$category[file5$uniqueID == "022288 6"],
               c("Judge of the Court of Common Pleas",
                 "District Attorney",
                 "Judge of Election-47-03",
                 "Inspector of Election-47-03",
                 "Retention - Judge of the Municipal Court - Jacquelyn Frazier"))
  expect_equal(file5$category[file5$uniqueID == "022288 1"],
               c("Retention - Judge of the Court of Common Pleas - Jacquelin",
                 "Retention - Judge of the Court of Common Pleas - Holly J Fo",
                 "Retention - Judge of the Municipal Court - Teresa Carr Deni",
                 "Retention - Justice of the Supreme Court - Ronald D Castille",
                 "Retention - Judge of the Superior Court - Jack Panella",
                 "Retention - Judge of the Court of Common Pleas - Nina Wrigh",
                 "Retention - Judge of the Court of Common Pleas - Paula A Pa"))

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

  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020016"])), 6)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020021"])), 71)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "021336"])), 46)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "021337"])), 56)

  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "023374"])), 46)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "022721"])), 43)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "022869"])), 42)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "022903"])), 31)

  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "020001"])), 41)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "021092"])), 6)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "022556"])), 83)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "021567"])), 43)

  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020643"])), 58)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020360"])), 74)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020323"])), 55)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020860"])), 1)

  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020214"])), 46)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020820"])), 22)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "022828"])), 32)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "023340"])), 65)
})


test_that("unique IDs in right order", {
  expect_equal(head(file1$uniqueID), c("020016 1", "020016 2", "020016 2",
                                       "020016 3", "020016 3", "020016 3"))
  expect_equal(tail(file1$uniqueID), c("022940 89", "022940 89", "022940 89",
                                       "022940 89", "022940 89", "022940 89"))

  expect_equal(head(file2$uniqueID), c("022721 1", "022721 1", "022721 1",
                                       "022721 1", "022721 1", "022721 1"))
  expect_equal(tail(file2$uniqueID), c("023374 46", "023374 46", "023374 46",
                                       "023374 46", "023374 46", "023374 46"))

  expect_equal(head(file3$uniqueID), c("020001 1", "020001 1", "020001 1",
                                       "020001 1", "020001 1", "020001 1"))
  expect_equal(tail(file3$uniqueID), c("021092 6", "021092 6", "021092 6",
                                       "021092 6", "021092 6", "021092 6"))

  expect_equal(head(file4$uniqueID), c("020323 1", "020323 1", "020323 1",
                                       "020323 1", "020323 1", "020323 1"))
  expect_equal(tail(file4$uniqueID), c("020860 1", "020860 1", "020860 1",
                                       "020860 1", "020860 1", "020860 1"))

  expect_equal(head(file5$uniqueID), c("020214 1", "020214 1", "020214 1",
                                       "020214 1", "020214 1", "020214 1"))
  expect_equal(tail(file5$uniqueID), c("023340 65", "023340 65", "023340 65",
                                       "023340 65", "023340 65", "023340 65"))
})
