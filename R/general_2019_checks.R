# election_2019_general_car$before_5th <- 0
# election_2019_general_car$before_5th[election_2019_general_car$start_date < ymd("2019-11-05")] <- 1
#
# election_2019_general_car$after_5th <- 0
# election_2019_general_car$after_5th[election_2019_general_car$start_date > ymd("2019-11-05")] <- 1
#
# election_2019_general_car$before_7am <- 0
# election_2019_general_car$before_7am[election_2019_general_car$hour < 7] <- 1
#
# election_2019_general_car$after_8pm <- 0
# election_2019_general_car$after_8pm[election_2019_general_car$hour > 20] <- 1
#
#
# election_2019_general_car$location <- paste0(election_2019_general_car$ward,
#                                              " ",
#                                              election_2019_general_car$division)
#
# library(dplyr)
# test <-
#   election_2019_general_car %>%
#   group_by(location) %>%
#   summarize(before_5th = sum(before_5th),
#             after_5th  = sum(after_5th),
#             before_7am = sum(before_7am),
#             after_8pm  = sum(after_8pm))
#
# library(readr)
# write_csv(test, path = "general_2019_dates_times_by_ward_division.csv")
#
#
# temp <- table(election_2019_general_car$location[election_2019_general_car$before_5th == 1])
#
#
#
#
# temp <- data.frame(temp)
# temp$Var1 <- as.character(temp$Var1)
# names(temp) <- c("ward_division", "frequency")
# temp <- temp[order(temp$ward_division), ]
#
#
#
# temp <- unique(election_2019_general_bir$location)
# temp <- gsub("-", " ", temp)
# temp <- sort(temp)
# temp <- data.frame(temp)
# names(temp) <- "ward_division"
# temp$ward_division <- as.character(temp$ward_division)
# library(readr)
# write_csv(temp, path = "included_ward_divisions.csv")
