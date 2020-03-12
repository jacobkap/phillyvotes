# file1 <- "C:/Users/user/Documents/philly_voting_data/2019_general_election_bir/1c.pdf"
# file2 <- "C:/Users/user/Documents/philly_voting_data/2019_general_election_bir/14967c.pdf"
# file3 <- "C:/Users/user/Documents/philly_voting_data/2019_general_election_bir/24026c.pdf"
# file4 <- "C:/Users/user/Documents/philly_voting_data/2019_general_election_bir/70275c.pdf"
# file5 <- "C:/Users/user/Documents/philly_voting_data/2019_general_election_bir/10030c.pdf"
# file6 <- "C:/Users/user/Documents/philly_voting_data/2019_general_election_bir/137707c.pdf"
# file7 <- "C:/Users/user/Documents/philly_voting_data/2019_general_election_bir/204185c.pdf"
# file8 <- "C:/Users/user/Documents/philly_voting_data/2019_general_election_bir/208716c.pdf"
# file9 <- "C:/Users/user/Documents/philly_voting_data/2019_general_election_bir/256223c.pdf"
# file10 <- "C:/Users/user/Documents/philly_voting_data/2019_general_election_bir/306900c.pdf"


file1 <- system.file("data/PDF_data/2019_general_for_tests", "1c.pdf",
                     package = "phillyvotes")
file2 <- system.file("data/PDF_data/2019_general_for_tests", "14967c.pdf",
                      package = "phillyvotes")
file3 <- system.file("data/PDF_data/2019_general_for_tests", "24026c.pdf",
                      package = "phillyvotes")
file4 <- system.file("data/PDF_data/2019_general_for_tests", "70275c.pdf",
                      package = "phillyvotes")
file5 <- system.file("data/PDF_data/2019_general_for_tests", "10030c.pdf",
                      package = "phillyvotes")
file6 <- system.file("data/PDF_data/2019_general_for_tests", "137707c.pdf",
                      package = "phillyvotes")
file7 <- system.file("data/PDF_data/2019_general_for_tests", "204185c.pdf",
                      package = "phillyvotes")
file8 <- system.file("data/PDF_data/2019_general_for_tests", "208716c.pdf",
                      package = "phillyvotes")
file9 <- system.file("data/PDF_data/2019_general_for_tests", "256223c.pdf",
                      package = "phillyvotes")
file10 <- system.file("data/PDF_data/2019_general_for_tests", "306900c.pdf",
                      package = "phillyvotes")



file1  <- scrape_new_bir(file1, candidate_file)
file2  <- scrape_new_bir(file2, candidate_file)
file3  <- scrape_new_bir(file3, candidate_file)
file4  <- scrape_new_bir(file4, candidate_file)
file5  <- scrape_new_bir(file5, candidate_file)
file6  <- scrape_new_bir(file6, candidate_file)
file7  <- scrape_new_bir(file7, candidate_file)
file8  <- scrape_new_bir(file8, candidate_file)
file9  <- scrape_new_bir(file9, candidate_file)
file10 <- scrape_new_bir(file10, candidate_file)





test_that("unique IDs correct", {
  expect_equal(unique(file1$uniqueID),  1)
  expect_equal(unique(file2$uniqueID),  14967)
  expect_equal(unique(file3$uniqueID),  24026)
  expect_equal(unique(file4$uniqueID),  70275)
  expect_equal(unique(file5$uniqueID),  10030)
  expect_equal(unique(file6$uniqueID),  137707)
  expect_equal(unique(file7$uniqueID),  204185)
  expect_equal(unique(file8$uniqueID),  208716)
  expect_equal(unique(file9$uniqueID),  256223)
  expect_equal(unique(file10$uniqueID), 306900)
})

test_that("location correct", {
  expect_equal(unique(file1$location),  "04-04")
  expect_equal(unique(file2$location),  "01-08")
  expect_equal(unique(file3$location),  "01-20")
  expect_equal(unique(file4$location),  "17-05")
  expect_equal(unique(file5$location),  "05-23")
  expect_equal(unique(file6$location),  "35-09")
  expect_equal(unique(file7$location),  "16-03")
  expect_equal(unique(file8$location),  "48-08")
  expect_equal(unique(file9$location),  "60-20")
  expect_equal(unique(file10$location), "62-16")
})


test_that("straight ticket checks", {
  expect_equal(unique(file1$straight_party_ticket_selected),  1)
  expect_equal(unique(file2$straight_party_ticket_selected),  0)
  expect_equal(unique(file3$straight_party_ticket_selected),  0)
  expect_equal(unique(file4$straight_party_ticket_selected),  1)
  expect_equal(unique(file5$straight_party_ticket_selected),  1)
  expect_equal(unique(file6$straight_party_ticket_selected),  0)
  expect_equal(unique(file7$straight_party_ticket_selected),  1)
  expect_equal(unique(file8$straight_party_ticket_selected),  1)
  expect_equal(unique(file9$straight_party_ticket_selected),  1)
  expect_equal(unique(file10$straight_party_ticket_selected), 0)

  expect_equal(unique(file1$straight_party_ticket_party),  "democratic")
  expect_equal(unique(file2$straight_party_ticket_party),  NA)
  expect_equal(unique(file3$straight_party_ticket_party),  NA)
  expect_equal(unique(file4$straight_party_ticket_party),  "democratic")
  expect_equal(unique(file5$straight_party_ticket_party),  "working families party")
  expect_equal(unique(file6$straight_party_ticket_party),  NA)
  expect_equal(unique(file7$straight_party_ticket_party),  "democratic")
  expect_equal(unique(file8$straight_party_ticket_party),  "republican")
  expect_equal(unique(file9$straight_party_ticket_party),  "democratic")
  expect_equal(unique(file10$straight_party_ticket_party), NA)

  expect_equal(unique(file1$straight_party_ticket_actual),  0)
  expect_equal(unique(file2$straight_party_ticket_actual),  NA)
  expect_equal(unique(file3$straight_party_ticket_actual),  NA)
  expect_equal(unique(file4$straight_party_ticket_actual),  1)
  expect_equal(unique(file5$straight_party_ticket_actual),  0)
  expect_equal(unique(file6$straight_party_ticket_actual),  NA)
  expect_equal(unique(file7$straight_party_ticket_actual),  1)
  expect_equal(unique(file8$straight_party_ticket_actual),  1)
  expect_equal(unique(file9$straight_party_ticket_actual),  1)
  expect_equal(unique(file10$straight_party_ticket_actual), NA)

  expect_equal(unique(file1$straight_party_ticket_vote_other_party),  0)
  expect_equal(unique(file2$straight_party_ticket_vote_other_party),  NA)
  expect_equal(unique(file3$straight_party_ticket_vote_other_party),  NA)
  expect_equal(unique(file4$straight_party_ticket_vote_other_party),  0)
  expect_equal(unique(file5$straight_party_ticket_vote_other_party),  1)
  expect_equal(unique(file6$straight_party_ticket_vote_other_party),  NA)
  expect_equal(unique(file7$straight_party_ticket_vote_other_party),  0)
  expect_equal(unique(file8$straight_party_ticket_vote_other_party),  0)
  expect_equal(unique(file9$straight_party_ticket_vote_other_party),  0)
  expect_equal(unique(file10$straight_party_ticket_vote_other_party), NA)

  expect_equal(unique(file1$straight_party_ticket_undervote),  1)
  expect_equal(unique(file2$straight_party_ticket_undervote),  NA)
  expect_equal(unique(file3$straight_party_ticket_undervote),  NA)
  expect_equal(unique(file4$straight_party_ticket_undervote),  0)
  expect_equal(unique(file5$straight_party_ticket_undervote),  1)
  expect_equal(unique(file6$straight_party_ticket_undervote),  NA)
  expect_equal(unique(file7$straight_party_ticket_undervote),  0)
  expect_equal(unique(file8$straight_party_ticket_undervote),  0)
  expect_equal(unique(file9$straight_party_ticket_undervote),  0)
  expect_equal(unique(file10$straight_party_ticket_undervote), NA)
})

test_that("undercount checks", {
  expect_equal(unique(file1$undercount),  1)
  expect_equal(unique(file2$undercount),  1)
  expect_equal(unique(file3$undercount),  1)
  expect_equal(unique(file4$undercount),  1)
  expect_equal(unique(file5$undercount),  1)
  expect_equal(unique(file6$undercount),  1)
  expect_equal(unique(file7$undercount),  1)
  expect_equal(unique(file8$undercount),  1)
  expect_equal(unique(file9$undercount),  1)
  expect_equal(unique(file10$undercount), 1)


  expect_equal(unique(file1$undercount_excluding_retention_or_proposals),  1)
  expect_equal(unique(file2$undercount_excluding_retention_or_proposals),  1)
  expect_equal(unique(file3$undercount_excluding_retention_or_proposals),  1)
  expect_equal(unique(file4$undercount_excluding_retention_or_proposals),  0)
  expect_equal(unique(file5$undercount_excluding_retention_or_proposals),  1)
  expect_equal(unique(file6$undercount_excluding_retention_or_proposals),  1)
  expect_equal(unique(file7$undercount_excluding_retention_or_proposals),  0)
  expect_equal(unique(file8$undercount_excluding_retention_or_proposals),  1)
  expect_equal(unique(file9$undercount_excluding_retention_or_proposals),  0)
  expect_equal(unique(file10$undercount_excluding_retention_or_proposals), 1)
})



test_that("number of votes", {
  expect_equal(unique(file1$offices_voted_for),  12)
  expect_equal(unique(file2$offices_voted_for),  31)
  expect_equal(unique(file3$offices_voted_for),  31)
  expect_equal(unique(file4$offices_voted_for),  13)
  expect_equal(unique(file5$offices_voted_for),  9)
  expect_equal(unique(file6$offices_voted_for),  30)
  expect_equal(unique(file7$offices_voted_for),  15)
  expect_equal(unique(file8$offices_voted_for),  6)
  expect_equal(unique(file9$offices_voted_for),  9)
  expect_equal(unique(file10$offices_voted_for), 8)

  expect_equal(unique(file1$offices_voted_for_excluding_retentions_or_proposal),  9)
  expect_equal(unique(file2$offices_voted_for_excluding_retentions_or_proposal),  9)
  expect_equal(unique(file3$offices_voted_for_excluding_retentions_or_proposal),  9)
  expect_equal(unique(file4$offices_voted_for_excluding_retentions_or_proposal),  9)
  expect_equal(unique(file5$offices_voted_for_excluding_retentions_or_proposal),  9)
  expect_equal(unique(file6$offices_voted_for_excluding_retentions_or_proposal),  8)
  expect_equal(unique(file7$offices_voted_for_excluding_retentions_or_proposal),  9)
  expect_equal(unique(file8$offices_voted_for_excluding_retentions_or_proposal),  5)
  expect_equal(unique(file9$offices_voted_for_excluding_retentions_or_proposal),  9)
  expect_equal(unique(file10$offices_voted_for_excluding_retentions_or_proposal), 2)

  expect_equal(unique(file1$candidates_voted_for),  23)
  expect_equal(unique(file2$candidates_voted_for),  42)
  expect_equal(unique(file3$candidates_voted_for),  40)
  expect_equal(unique(file4$candidates_voted_for),  25)
  expect_equal(unique(file5$candidates_voted_for),  19)
  expect_equal(unique(file6$candidates_voted_for),  36)
  expect_equal(unique(file7$candidates_voted_for),  27)
  expect_equal(unique(file8$candidates_voted_for),  11)
  expect_equal(unique(file9$candidates_voted_for),  21)
  expect_equal(unique(file10$candidates_voted_for), 15)


  expect_equal(unique(file1$candidates_voted_for_excluding_retentions_or_proposal),  20)
  expect_equal(unique(file2$candidates_voted_for_excluding_retentions_or_proposal),  20)
  expect_equal(unique(file3$candidates_voted_for_excluding_retentions_or_proposal),  18)
  expect_equal(unique(file4$candidates_voted_for_excluding_retentions_or_proposal),  21)
  expect_equal(unique(file5$candidates_voted_for_excluding_retentions_or_proposal),  19)
  expect_equal(unique(file6$candidates_voted_for_excluding_retentions_or_proposal),  14)
  expect_equal(unique(file7$candidates_voted_for_excluding_retentions_or_proposal),  21)
  expect_equal(unique(file8$candidates_voted_for_excluding_retentions_or_proposal),  10)
  expect_equal(unique(file9$candidates_voted_for_excluding_retentions_or_proposal),  21)
  expect_equal(unique(file10$candidates_voted_for_excluding_retentions_or_proposal), 9)
})




test_that("Candidate has right name", {

  expect_equal(head(file1$candidate),
               c("Amanda Green-Hawkins",
                 "Jennifer Schultz",
                 "Anthony Kyriakakis",
                 "Joshua Roberts",
                 "Tiffany Palmer",
                 "James C Crumlish"))
  expect_equal(head(file2$candidate),
               c("Amanda Green-Hawkins",
                 "Daniel D Mccaffery",
                 "Jennifer Schultz",
                 "Anthony Kyriakakis",
                 "Joshua Roberts",
                 "Tiffany Palmer"))
  expect_equal(head(file3$candidate),
               c("Amanda Green-Hawkins",
                 "Daniel D Mccaffery",
                 "Jennifer Schultz",
                 "Anthony Kyriakakis",
                 "Tiffany Palmer",
                 "Carmella Jacquinto"))
  expect_equal(head(file4$candidate),
               c("Amanda Green-Hawkins",
                 "Daniel D Mccaffery",
                 "Jennifer Schultz",
                 "Anthony Kyriakakis",
                 "Joshua Roberts",
                 "Tiffany Palmer"))
  expect_equal(head(file5$candidate),
               c("Daniel D Mccaffery",
                 "Christylee Peck",
                 "Jennifer Schultz",
                 "Joshua Roberts",
                 "Tiffany Palmer",
                 "James C Crumlish"))
  expect_equal(head(file6$candidate),
               c("Megan Mccarthy King",
                 "Jennifer Schultz",
                 "Joshua Roberts",
                 "Tiffany Palmer",
                 "Crystal B Powell",
                 "David H Conroy"))
  expect_equal(head(file7$candidate),
               c("Amanda Green-Hawkins",
                 "Daniel D Mccaffery",
                 "Jennifer Schultz",
                 "Anthony Kyriakakis",
                 "Joshua Roberts",
                 "Tiffany Palmer"))
  expect_equal(head(file8$candidate),
               c("Megan Mccarthy King",
                 "Christylee Peck",
                 "Billy Ciancaglini",
                 "Al Schmidt",
                 "Dan Tinney",
                 "Al Taubenberger"))
  expect_equal(head(file9$candidate),
               c("Amanda Green-Hawkins",
                 "Daniel D Mccaffery",
                 "Jennifer Schultz",
                 "Anthony Kyriakakis",
                 "Joshua Roberts",
                 "Tiffany Palmer"))
  expect_equal(head(file10$candidate),
               c("Jennifer Schultz",
                 "Anthony Kyriakakis",
                 "Joshua Roberts",
                 "Tiffany Palmer",
                 "James C Crumlish",
                 "Carmella Jacquinto"))




  expect_equal(tail(file1$candidate),
               c("Derek S Green",
                 "Katherine Gilmore Richardson",
                 "Curtis Jones Jr",
                 "Yes",
                 "Yes",
                 "Yes"))
  expect_equal(tail(file2$candidate),
               c("Yes",
                 "Yes",
                 "Yes",
                 "Yes",
                 "Yes",
                 "Yes"))
  expect_equal(tail(file3$candidate),
               c("Yes",
                 "Yes",
                 "Yes",
                 "No",
                 "Yes",
                 "Yes"))
  expect_equal(tail(file4$candidate),
               c("Katherine Gilmore Richardson",
                 "Cindy Bass",
                 "Yes",
                 "Yes",
                 "Yes",
                 "Yes"))
  expect_equal(tail(file5$candidate),
               c("Helen Gym",
                 "Allan Domb",
                 "Isaiah Thomas",
                 "Derek S Green",
                 "Katherine Gilmore Richardson",
                 "Darrell L Clarke"))
  expect_equal(tail(file6$candidate),
               c("Yes",
                 "Yes",
                 "Yes",
                 "Yes",
                 "No",
                 "No"))
  expect_equal(tail(file7$candidate),
               c("Yes",
                 "Yes",
                 "No",
                 "Yes",
                 "Yes",
                 "Yes"))
  expect_equal(tail(file8$candidate),
               c("Al Taubenberger",
                 "Matt Wolfe",
                 "Bill Heeney",
                 "David Oh",
                 "Michael Bradley",
                 "Yes"))
  expect_equal(tail(file9$candidate),
               c("Helen Gym",
                 "Allan Domb",
                 "Isaiah Thomas",
                 "Derek S Green",
                 "Katherine Gilmore Richardson",
                 "Jamie Gauthier"))
  expect_equal(tail(file10$candidate),
               c("Yes",
                 "Yes",
                 "Yes",
                 "Yes",
                 "Yes",
                 "Yes"))

})


test_that("Categories are correct", {

  expect_equal(head(file1$category),
               c("Judge of the Superior Court",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas"))
  expect_equal(head(file2$category),
               c("Judge of the Superior Court",
                 "Judge of the Superior Court",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas"))
  expect_equal(head(file3$category),
               c("Judge of the Superior Court",
                 "Judge of the Superior Court",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas"))
  expect_equal(head(file4$category),
               c("Judge of the Superior Court",
                 "Judge of the Superior Court",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas"))
  expect_equal(head(file5$category),
               c("Judge of the Superior Court",
                 "Judge of the Superior Court",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas"))
  expect_equal(head(file6$category),
               c("Judge of the Superior Court",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Municipal Court"))
  expect_equal(head(file7$category),
               c("Judge of the Superior Court",
                 "Judge of the Superior Court",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas"))
  expect_equal(head(file8$category),
               c("Judge of the Superior Court",
                 "Judge of the Superior Court",
                 "Mayor",
                 "City Commissioners",
                 "Council At-Large",
                 "Council At-Large"))
  expect_equal(head(file9$category),
               c("Judge of the Superior Court",
                 "Judge of the Superior Court",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas"))
  expect_equal(head(file10$category),
               c("Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas",
                 "Judge of the Court of Common Pleas"))




  expect_equal(tail(file1$category),
               c("Council At-Large",
                 "Council At-Large",
                 "District Council 4th District",
                 "Proposed Constitutional Amendment",
                 "Proposed Charter Change Question",
                 "Proposed City Bond Question"))
  expect_equal(tail(file2$category),
               c("Municipal Court Retention - Henry Lewandowski",
                 "Municipal Court Retention - Wendy Lynn Pew",
                 "Municipal Court Retention - T Francis Shields",
                 "Proposed Constitutional Amendment",
                 "Proposed Charter Change Question",
                 "Proposed City Bond Question"))
  expect_equal(tail(file3$category),
               c("Municipal Court Retention - Henry Lewandowski",
                 "Municipal Court Retention - Wendy Lynn Pew",
                 "Municipal Court Retention - T Francis Shields",
                 "Proposed Constitutional Amendment",
                 "Proposed Charter Change Question",
                 "Proposed City Bond Question"))
  expect_equal(tail(file4$category),
               c("Council At-Large",
                 "District Council 8th District",
                 "Superior Court Retention - Anne E Lazarus",
                 "Court of Common Pleas Retention - Diane R Thompson",
                 "Municipal Court Retention - Martin S Coleman",
                 "Proposed City Bond Question"))
  expect_equal(tail(file5$category),
               c("Council At-Large",
                 "Council At-Large",
                 "Council At-Large",
                 "Council At-Large",
                 "Council At-Large",
                 "District Council 5th District"))
  expect_equal(tail(file6$category),
               c("Municipal Court Retention - Henry Lewandowski",
                 "Municipal Court Retention - Wendy Lynn Pew",
                 "Municipal Court Retention - T Francis Shields",
                 "Proposed Constitutional Amendment",
                 "Proposed Charter Change Question",
                 "Proposed City Bond Question"))
  expect_equal(tail(file7$category),
               c("Superior Court Retention - Anne E Lazarus",
                 "Commonwealth Court Retention - Kevin Brobson",
                 "Commonwealth Court Retention - Patricia A Mccullough",
                 "Court of Common Pleas Retention - Roxanne E Covington",
                 "Court of Common Pleas Retention - Diane R Thompson",
                 "Municipal Court Retention - Wendy Lynn Pew"))
  expect_equal(tail(file8$category),
               c("Council At-Large",
                 "Council At-Large",
                 "Council At-Large",
                 "Council At-Large",
                 "District Council 2nd District",
                 "Court of Common Pleas Retention - Robert P Coleman"))
  expect_equal(tail(file9$category),
               c("Council At-Large",
                 "Council At-Large",
                 "Council At-Large",
                 "Council At-Large",
                 "Council At-Large",
                 "District Council 3rd District"))

  expect_equal(tail(file10$category),
               c("Superior Court Retention - Anne E Lazarus",
                 "Superior Court Retention - Judy Olson",
                 "Court of Common Pleas Retention - Daniel Anders",
                 "Court of Common Pleas Retention - Karen Shreeves-Johns",
                 "Court of Common Pleas Retention - Sheila Woods-Skipper",
                 "Municipal Court Retention - Wendy Lynn Pew"))


})


# No change needed here ---------------------------------------------------

test_that("No commas in candidate names", {
  expect_false(all(grepl(",", file1$candidate)))
  expect_false(all(grepl(",", file2$candidate)))
  expect_false(all(grepl(",", file3$candidate)))
  expect_false(all(grepl(",", file4$candidate)))
  expect_false(all(grepl(",", file5$candidate)))
  expect_false(all(grepl(",", file6$candidate)))
  expect_false(all(grepl(",", file7$candidate)))
  expect_false(all(grepl(",", file8$candidate)))
  expect_false(all(grepl(",", file9$candidate)))
  expect_false(all(grepl(",", file10$candidate)))
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

  expect_false(any(is.na(file6$location)))
  expect_false(any(is.na(file6$ward)))
  expect_false(any(is.na(file6$division)))

  expect_false(any(is.na(file7$location)))
  expect_false(any(is.na(file7$ward)))
  expect_false(any(is.na(file7$division)))

  expect_false(any(is.na(file8$location)))
  expect_false(any(is.na(file8$ward)))
  expect_false(any(is.na(file8$division)))

  expect_false(any(is.na(file9$location)))
  expect_false(any(is.na(file9$ward)))
  expect_false(any(is.na(file9$division)))

  expect_false(any(is.na(file10$location)))
  expect_false(any(is.na(file10$ward)))
  expect_false(any(is.na(file10$division)))
})
