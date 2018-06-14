context("election-2013-primary")

file1 <- system.file("data/PDF_data/2013 Primary",
                     "2013 Primary RTC 1 BIR 1 of 2.pdf",
                     package = "phillyfile")
file2 <- system.file("data/PDF_data/2013 Primary",
                     "2013 Primary RTC 5 BIR.pdf",
                     package = "phillyfile")
file3 <- system.file("data/PDF_data/2013 Primary",
                     "2013 Primary RTC 6 BIR.pdf",
                     package = "phillyfile")
file4 <- system.file("data/PDF_data/2013 Primary",
                     "2013 Primary RTC BT BIR 1 of 3.pdf",
                     package = "phillyfile")
file5 <- system.file("data/PDF_data/2013 Primary",
                     "2013 Primary RTC BT BIR 2 of 3.pdf",
                     package = "phillyfile")

file1 <- philly_file(file1)
file2 <- philly_file(file2)
file3 <- philly_file(file3)
file4 <- philly_file(file4)
file5 <- philly_file(file5)

test_that("right file name", {
  expect_equal(unique(file1$file), "2013 Primary RTC 1 BIR 1 of 2")
  expect_equal(unique(file2$file), "2013 Primary RTC 5 BIR")
  expect_equal(unique(file3$file), "2013 Primary RTC 6 BIR")
  expect_equal(unique(file4$file), "2013 Primary RTC BT BIR 1 of 3")
  expect_equal(unique(file5$file), "2013 Primary RTC BT BIR 2 of 3")
})


test_that("right number of pages", {
  expect_equal(max(file1$pdf_page), 3000)
  expect_equal(max(file2$pdf_page), 2162)
  expect_equal(max(file3$pdf_page), 1508)
  expect_equal(max(file4$pdf_page), 3000)
  expect_equal(max(file5$pdf_page), 6000)
})

test_that("unique voters have correct number of files recorded", {
  expect_equal(nrow(file1[file1$uniqueID == "021840 17",]), 15)
  expect_equal(nrow(file1[file1$uniqueID == "021840 14",]), 3)
  expect_equal(nrow(file1[file1$uniqueID == "021840 12",]), 6)
  expect_equal(nrow(file1[file1$uniqueID == "021840 9",]), 2)
  expect_equal(nrow(file2[file2$uniqueID == "023236 23",]), 12)
  expect_equal(nrow(file2[file2$uniqueID == "023236 19",]), 2)
  expect_equal(nrow(file2[file2$uniqueID == "023236 20",]), 5)
  expect_equal(nrow(file2[file2$uniqueID == "021009 2",]), 2)
  expect_equal(nrow(file3[file3$uniqueID == "022236 2",]), 9)
  expect_equal(nrow(file3[file3$uniqueID == "022236 3",]), 5)
  expect_equal(nrow(file3[file3$uniqueID == "022236 1",]), 4)
  expect_equal(nrow(file3[file3$uniqueID == "020277 1",]), 3)
  expect_equal(nrow(file4[file4$uniqueID == "020727 24",]), 4)
  expect_equal(nrow(file4[file4$uniqueID == "020727 22",]), 10)
  expect_equal(nrow(file4[file4$uniqueID == "020383 47",]), 8)
  expect_equal(nrow(file4[file4$uniqueID == "020383 52",]), 13)
  expect_equal(nrow(file5[file5$uniqueID == "022400 24",]), 1)
  expect_equal(nrow(file5[file5$uniqueID == "022400 22",]), 6)
  expect_equal(nrow(file5[file5$uniqueID == "020727 25",]), 14)
  expect_equal(nrow(file5[file5$uniqueID == "020727 29",]), 11)

})

test_that("all votes are equal to 1", {
  expect_equal(unique(file1$votes), c(1, NA))
  expect_equal(unique(file2$votes), c(1, NA))
  expect_equal(unique(file3$votes), c(1, NA))
  expect_equal(unique(file4$votes), c(1, NA))
  expect_equal(unique(file5$votes), c(1, NA))
})


test_that("ballot ballot_position values are correct", {
  # First 6 votes
  expect_equal(head(file1$ballot_position), c("3E", "7E", "14E", "20E", "22E", "26E"))
  # Last 6 votes
  expect_equal(tail(file1$ballot_position), c("5F", "9F", "10F", "12G", "19H", "19H"))

  # First 6 votes
  expect_equal(head(file2$ballot_position), c("3E", "10E", "13E", "14E", "22E", "24E"))
  # Last 6 votes
  expect_equal(tail(file2$ballot_position), c("16F", "19F", "5G", "13G", "19G", "9H"))

  # First 6 votes
  expect_equal(head(file3$ballot_position), c("27E", "30E", "15H", "10E", "27E", "30E"))
  # Last 6 votes
  expect_equal(tail(file3$ballot_position), c("19H", "5E", "14E", "19E", "22E", "26E"))

  # First 6 votes
  expect_equal(head(file4$ballot_position), c("3E", "5E", "6E", "8E", "24E", "27E"))
  # Last 6 votes
  expect_equal(tail(file4$ballot_position), c("12H", "17H", "3E", "5E", "9E", "17F"))

  # First 6 votes
  expect_equal(head(file5$ballot_position), c("3E", "5E", "6E", "8E", "9E", "10"))
  # Last 6 votes
  expect_equal(tail(file5$ballot_position), c("13F", "20F", "6G", "9G", "12G", "3E"))

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


test_that("proper number of unique ID for a location and serial number match", {

  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "021840"])), 17)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "021839"])), 17)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020001"])), 25)
  expect_equal(length(unique(file1$uniqueID[file1$serial_number == "020078"])), 41)

  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021009"])), 8)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "021047"])), 37)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "022003"])), 33)
  expect_equal(length(unique(file2$uniqueID[file2$serial_number == "023236"])), 23)

  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "020277"])), 9)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "021101"])), 27)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "021748"])), 10)
  expect_equal(length(unique(file3$uniqueID[file3$serial_number == "022101"])), 23)

  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020410"])), 42)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020409"])), 48)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020383"])), 80)
  expect_equal(length(unique(file4$uniqueID[file4$serial_number == "020492"])), 47)

  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020727"])), 46)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020912"])), 28)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "020954"])), 60)
  expect_equal(length(unique(file5$uniqueID[file5$serial_number == "021766"])), 38)
})


test_that("unique IDs in right order", {
  expect_equal(unique(head(file1$uniqueID)), c("020001", "020001", "020001",
                                               "020001", "020001", "020001"))
  expect_equal(unique(tail(file1$uniqueID)), c("021840", "021840", "021840",
                                               "021840", "021840", "021840"))

  expect_equal(unique(head(file2$uniqueID)), c("021009", "021009", "021009",
                                               "021009", "021009", "021009"))
  expect_equal(unique(tail(file2$uniqueID)), c("023236", "023236", "023236",
                                               "023236", "023236", "023236"))

  expect_equal(unique(head(file3$uniqueID)), c("020277", "020277", "020277",
                                               "020277", "020277", "020277"))
  expect_equal(unique(tail(file3$uniqueID)), c("022236", "022236", "022236",
                                               "022236", "022236", "022236"))

  expect_equal(unique(head(file4$uniqueID)), c("020383", "020383", "020383",
                                               "020383", "020383", "020383"))
  expect_equal(unique(tail(file4$uniqueID)), c("020727", "020727", "020727",
                                               "020727", "020727", "020727"))

  expect_equal(unique(head(file5$uniqueID)), c("020727", "020727", "020727",
                                               "020727", "020727", "020727"))
  expect_equal(unique(tail(file5$uniqueID)), c("022400", "022400", "022400",
                                               "022400", "022400", "022400"))
})


test_that("No commas in candidate names", {
  expect_false(all(grepl(",", file1$candidate)))
  expect_false(all(grepl(",", file2$candidate)))
  expect_false(all(grepl(",", file3$candidate)))
  expect_false(all(grepl(",", file4$candidate)))
  expect_false(all(grepl(",", file5$candidate)))
})


test_that("Candidate has right name", {

  expect_equal(file1$candidate[file1$uniqueID == "020078 4", ],
               c("Vince Giusini", "Marianne Squillaciotti",
                 "Josephine Baggio"))
  expect_equal(file1$candidate[file1$uniqueID == "020078 6", ],
               c("Vince Giusini"))
  expect_equal(file1$candidate[file1$uniqueID == "021840 14", ],
               c("Henry Lewandoski", "Alan Butkovitz",
                 "James A Ward"))
  expect_equal(file1$candidate[file1$uniqueID == "021840 9", ],
               c("Joseph C Waters Jr", "Henry Lewandowski"))
  expect_equal(file2$candidate[file2$uniqueID == "023236 19", ],
               c("Terrence J Tracy Jr", "Elaine Delmont"))
  expect_equal(file2$candidate[file2$uniqueID == "023236 10", ],
               c("Vic Stabile", "Anne Marie Coyle", "Ella P Butcher",
                 "Daniel A Alvarez"))
  expect_equal(file2$candidate[file2$uniqueID == "023236 2", ],
               c("Anne Marie Coyle", "Christopher M Vogler",
                 "Elaine Delmont"))
  expect_equal(file2$candidate[file2$uniqueID == "023235 36", ],
               c("Henry Lewandowski", "Mark Zecca"))
  expect_equal(file3$candidate[file3$uniqueID == "020277 1", ],
               c("Aida Luisa Ribot", "Ernesto Rivera Jr",
                 "Jose A Figueroa"))
  expect_equal(file3$candidate[file3$uniqueID == "020277 3", ],
               c("John J O Connor Jr"))
  expect_equal(file3$candidate[file3$uniqueID == "020277 5", ],
               c("Aida Luisa Ribor", "Ernesto Rivera Jr",
                 "Rania Major"))
  expect_equal(file3$candidate[file3$uniqueID == "022235 11", ],
               c("Anne Marie Coyle", "Terrence J Tracy Jr"))
  expect_equal(file4$candidate[file4$uniqueID == "020492 26", ],
               c("Stephanie M Sawyer"))
  expect_equal(file4$candidate[file4$uniqueID == "020492 20", ],
               c("Vince Giusini", "Martin Coleman", "Suzanne Harmon Carn",
                 "Mark Zecca"))
  expect_equal(file4$candidate[file4$uniqueID == "020533 51", ],
               c("Katherine Lindsay", "Margaret Hutchinson"))
  expect_equal(file4$candidate[file4$uniqueID == "020533 4", ],
               c("Joseph C Waters Jr", "R Seth Williams",
                 "Katherine Lindsay", "Margaret Hutchinson"))
  expect_equal(file5$candidate[file5$uniqueID == "021766 31", ],
               c("Joseph C Waters Jr", "Write In"))
  expect_equal(file5$candidate[file5$uniqueID == "022359 1", ],
               c("R Seth Williams", "Alan Butkovitz",
                 "Jose A Figueroa", "Keith Jackson"))
  expect_equal(file5$candidate[file5$uniqueID == "020727 27", ],
               c("R Seth Williams"))
  expect_equal(file5$candidate[file5$uniqueID == "020727 37", ],
               c("Timika Lane", "Martin Coleman", "Henry Lewandowski",
                 "R Seth Williams"))



})


test_that("Categories are correct", {

  expect_equal(file1$category[file1$uniqueID == "020078 4", ],
               c("Judge of the Court of Common Pleas - Democrat",
                 "Judge of Election- Democrat - 02-18",
                 "Inspector of Election - Democrat - 02-18"))
  expect_equal(file1$category[file1$uniqueID == "020078 6", ],
               c("Judge of the Court of Common Pleas - Democrat"))
  expect_equal(file1$category[file1$uniqueID == "021840 14", ],
               c("Judge of the Municipal Court - Democrat",
                 "Controller - Democrat",
                 "Inspector of Election - Democrat - 39-19"))
  expect_equal(file1$category[file1$uniqueID == "021840 9", ],
               c("Judge of the Superior Court - Democrat",
                 "Judge of the Municipal Court - Democrat"))
  expect_equal(file2$category[file2$uniqueID == "023236 19", ],
               c("Controller - Republican",
                 "Inspector of Election - Republican - 64-18"))
  expect_equal(file2$category[file2$uniqueID == "023236 10", ],
               c("Judge of the Superior Court - Republican",
                 "Judge of the Court of Common Pleas - Republican",
                 "Judge of the Traffic Court - Republican",
                 "District Attorney - Republican"))
  expect_equal(file2$category[file2$uniqueID == "023236 2", ],
               c("Judge of the Court of Common Pleas- Republican",
                 "Jude of the Traffic Court - Republican",
                 "Inspector of Election - Republican - 64-18"))
  expect_equal(file2$category[file2$uniqueID == "023235 36", ],
               c("Judge of the Municipal Court - Democrat",
                 "Controller - Democrat"))
  expect_equal(file3$category[file3$uniqueID == "020277 1", ],
               c("Judge of Election - Democrat - 07-01",
                 "Inspector of Election - Democrat - 07-01",
                 "Judge of the Traffic Court - Democrat - Democrat"))
  expect_equal(file3$category[file3$uniqueID == "020277 3", ],
               c("Judge of the Court of Common Pleas - Democrat"))
  expect_equal(file3$category[file3$uniqueID == "022235 11", ],
               c("Judge of the Court of Common PLeas - Republican",
                 "Controller - Republican"))
  expect_equal(file3$category[file3$uniqueID == "022235 3", ],
               c("Judge of the Court of Common Pleas - Democrat",
                 "Judge of the Court of Common Pleas - Democrat"))
  expect_equal(file4$category[file4$uniqueID == "020492 26", ],
               c("Judge of the Court of Common Pleas - Democrat"))
  expect_equal(file4$category[file4$uniqueID == "020492 20", ],
               c("Judge of the Court of Common Pleas - Democrat",
                 "Judge of the Municiapl Court - Democrat",
                 "Judge of the Traffic Court - Democrat",
                 "Controller - Democrat"))
  expect_equal(file4$category[file4$uniqueID == "020533 51", ],
               c("Judge of Election - Democrat - 12-10",
                 "Inspector of Election - Democrat - 12-10"))
  expect_equal(file4$category[file4$uniqueID == "020533 4", ],
               c("Judge of the Superior Court - Democrat",
                 "District Attorney - Democrat",
                 "Judge of Election - Democrat - 12-10",
                 "Inspector of Election - Democrat 12-10"))
  expect_equal(file5$category[file5$uniqueID == "021766", ],
               c("Judge of the Superior Court - Democrat",
                 "Judge of Election - Democrat - 38-03"))
  expect_equal(file5$category[file5$uniqueID == "022359 1", ],
               c("District Attorney - Democrat", "Controller - Democrat",
                 "Judge of the Traffic Court - Democrat",
                 "Judge of the Traffic Court - Democrat"))
  expect_equal(file5$category[file5$uniqueID == "020727 27", ],
               c("District Attorney - Democrat"))
  expect_equal(file5$category[file5$uniqueID == "020727 37", ],
               c("Judge of the Court of Common Pleas - Democrat",
                 "Judge of the Municipal Court - Democrat",
                 "Judge of the Municipal Court - Democrat",
                 "District Attorney"))

})
