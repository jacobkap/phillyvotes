# file1 <- system.file("data/PDF_data/201",
#                      "",
#                      package = "phillyvotes")
# file2 <- system.file("data/PDF_data/201",
#                      "",
#                      package = "phillyvotes")
# file3 <- system.file("data/PDF_data/201",
#                      "",
#                      package = "phillyvotes")
#
# file1 <- scrape_CAR(file1)
# file2 <- scrape_CAR(file2)
# file3 <- scrape_CAR(file3)
#
# test_that("right file name", {
#   expect_equal(unique(file1$file), "")
#   expect_equal(unique(file2$file), "")
#   expect_equal(unique(file3$file), "")
# })
#
# test_that("year values are right", {
#   expect_equal(max(file1$page), )
#   expect_equal(max(file2$page), )
#   expect_equal(max(file3$page), )
# })
#
# test_that("precinct values are right", {
#   expect_equal(head(file1$precinct), c("", "", "",
#                                        "", "", ""))
#   expect_equal(tail(file1$precinct), c("", "", "",
#                                        "", "", ""))
#
#
#   expect_equal(head(file2$precinct), c("", "", "",
#                                        "", "", ""))
#   expect_equal(tail(file2$precinct), c("", "", "",
#                                        "", "", ""))
#
#
#   expect_equal(head(file3$precinct), c("", "", "",
#                                        "", "", ""))
#   expect_equal(tail(file3$precinct), c("", "", "",
#                                        "", "", ""))
#
# })
#
# test_that("ward values are right", {
#   expect_equal(head(file1$ward), c(, , , , , ))
#   expect_equal(tail(file1$ward), c(, , , , , ))
#
#
#   expect_equal(head(file2$ward), c(, , , , , ))
#   expect_equal(tail(file2$ward), c(, , , , , ))
#
#
#   expect_equal(head(file3$ward), c(, , , , , ))
#   expect_equal(tail(file3$ward), c(, , , , , ))
#
# })
#
# test_that("division values are right", {
#   expect_equal(head(file1$division), c(, , , , , ))
#   expect_equal(tail(file1$division), c(, , , , , ))
#
#
#   expect_equal(head(file2$division), c(, , , , , ))
#   expect_equal(tail(file2$division), c(, , , , , ))
#
#
#   expect_equal(head(file3$division), c(, , , , , ))
#   expect_equal(tail(file3$division), c(, , , , , ))
#
# })
#
# test_that("serial number values are right", {
#   expect_equal(head(file1$serial), c("", "", "",
#                                      "", "", ""))
#   expect_equal(tail(file1$serial), c("", "", "",
#                                      "", "", ""))
#
#
#   expect_equal(head(file2$serial), c("", "", "",
#                                      "", "", ""))
#   expect_equal(tail(file2$serial), c("", "", "",
#                                      "", "", ""))
#
#
#   expect_equal(head(file3$serial), c("", "", "",
#                                      "", "", ""))
#   expect_equal(tail(file3$serial), c("", "", "",
#                                      "", "", ""))
#
# })
#
# test_that("hour values are right", {
#   expect_equal(head(file1$hour), c(, , , , , ))
#   expect_equal(tail(file1$hour), c(, , , , , ))
#
#
#   expect_equal(head(file2$hour), c(, , , , , ))
#   expect_equal(tail(file2$hour), c(, , , , , ))
#
#
#   expect_equal(head(file3$hour), c(, , , , , ))
#   expect_equal(tail(file3$hour), c(, , , , , ))
#
# })
#
# # test_that("year values are right", {
# #   expect_equal(unique(file1$year), )
# #   expect_equal(unique(file2$year), )
# #   expect_equal(unique(file3$year), )
# # })
#
#
#
#
#
#
#
# test_that("all actions are voter cast ballot", {
#   expect_equal(unique(file1$action), "voter cast ballot")
#   expect_equal(unique(file2$action), "voter cast ballot")
#   expect_equal(unique(file3$action), "voter cast ballot")
# })
#
# test_that("No NA values", {
#   expect_equal(sum(is.na((file1$division))), 0)
#   expect_equal(sum(is.na((file2$division))), 0)
#   expect_equal(sum(is.na((file3$division))), 0)
#
#   # expect_equal(sum(is.na((file1$time))), 0)
#   # expect_equal(sum(is.na((file2$time))), 0)
#   # expect_equal(sum(is.na((file3$time))), 0)
#
#   expect_equal(sum(is.na((file1$serial))), 0)
#   expect_equal(sum(is.na((file2$serial))), 0)
#   expect_equal(sum(is.na((file3$serial))), 0)
#
#   expect_equal(sum(is.na((file1$pdf_page))), 0)
#   expect_equal(sum(is.na((file2$pdf_page))), 0)
#   expect_equal(sum(is.na((file3$pdf_page))), 0)

# expect_equal(sum(is.na((file1$start_action))), 0)
# expect_equal(sum(is.na((file2$start_action))), 0)
# expect_equal(sum(is.na((file3$start_action))), 0)
# expect_equal(sum(is.na((file4$start_action))), 0)
# expect_equal(sum(is.na((file5$start_action))), 0)

# expect_equal(sum(is.na((file1$submit_action))), 0)
# expect_equal(sum(is.na((file2$submit_action))), 0)
# expect_equal(sum(is.na((file3$submit_action))), 0)
# expect_equal(sum(is.na((file4$submit_action))), 0)
# expect_equal(sum(is.na((file5$submit_action))), 0)
#
#   expect_equal(sum(is.na((file1$ward))), 0)
#   expect_equal(sum(is.na((file2$ward))), 0)
#   expect_equal(sum(is.na((file3$ward))), 0)
#
#   expect_equal(sum(is.na((file1$division))), 0)
#   expect_equal(sum(is.na((file2$division))), 0)
#   expect_equal(sum(is.na((file3$division))), 0)
#
#   # expect_equal(sum(is.na((file1$hour))), 0)
#   # expect_equal(sum(is.na((file2$hour))), 0)
#   # expect_equal(sum(is.na((file3$hour))), 0)
# })
