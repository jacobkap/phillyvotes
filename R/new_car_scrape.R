# setwd("~/philly_voting_data")
# files <- list.files(pattern = ".csv")
# library(lubridate)
# library(dplyr)
#
# election_2019_general_car <- data.frame()
# for (file in files) {
#   temp <- new_scrape_CAR(file)
#   election_2019_general_car <- bind_rows(election_2019_general_car, temp)
# }
#
#
# spanish_votes_by_wards_real_date <-
#   election_2019_general_car %>%
#   filter(date.y %in% "2019-11-05") %>%
#   group_by(ward) %>%
#   summarize(only_spanish_selected_nov_5 = sum(only_spanish_selected)) %>%
#   arrange(ward) %>%
#   ungroup()
#
# spanish_votes_by_wards_any_date <-
#   election_2019_general_car %>%
#   group_by(ward) %>%
#   summarize(only_spanish_selected_any_date = sum(only_spanish_selected)) %>%
#   arrange(ward) %>%
#   ungroup()
#
# spanish_votes_by_wards <-
#   spanish_votes_by_wards_real_date %>%
#   left_join(spanish_votes_by_wards_any_date)
#readr::write_csv(spanish_votes_by_wards, "spanish_votes_by_wards.csv")


# summary(election_2019_general_car)
# table(election_2019_general_car$hour)
# table(election_2019_general_car$ward)
# table(election_2019_general_car$division)
# setwd("C:/Users/user/Dropbox/R_project/phillyvotes/data/clean_data/")
# save(election_2019_general_car,
#      file = "election_2019_general_car.rda")


new_scrape_CAR <- function(file) {
  data <- readr::read_delim(file, delim = "|") %>%
    mutate_all(trimws) %>%
    rename_all(crimeutils::fix_column_names) %>%
    mutate_all(tolower)


  data <- data[grep("paper card inserted", data$message)[1]:nrow(data), ]
  data$voter_number <- NA
  data$voter_number[grep("paper card inserted", data$message)] <-
    1:length(grep("paper card inserted", data$message))
  data$voter_number <- zoo::na.locf(data$voter_number)

  data <-
    data %>%
    filter(message %in% c("card ejected: rear",
                          "paper card inserted",
                          "language 2 enabled",
                          "language 1 enabled"))

  data <- data[data$voter_number %in%
                 (data$voter_number[table(data$voter_number) == 2]), ]

  top_code_1 <- function(x) {
    x[x > 1] <- 1
    return(x)
  }

  data_language <-
    data %>%
    filter(message %in% c("language 2 enabled",
                          "language 1 enabled")) %>%
    select(voter_number,
           message)
  data_language$spanish_selected <- 0
  data_language$english_selected <- 0
  data_language$spanish_selected[data_language$message %in% "language 2 enabled"] <- 1
  data_language$english_selected[data_language$message %in% "language 1 enabled"] <- 1
  data_language <-
    data_language %>%
    group_by(voter_number) %>%
    summarize(spanish_selected = sum(spanish_selected),
              english_selected = sum(english_selected)) %>%
    mutate(spanish_selected = top_code_1(spanish_selected),
           english_selected = top_code_1(english_selected))
  data_language$both_languages_selected <- data_language$spanish_selected *
    data_language$english_selected
  data_language$only_spanish_selected <- 1
  data_language$only_spanish_selected[data_language$both_languages_selected == 1] <- 0

  data_ejected <-
    data %>%
    filter(message %in% "card ejected: rear") %>%
    select(message,
           time,
           date,
           voter_number) %>%
    rename(submit_action = message,
           submit_time   = time,
           submit_date   = date) %>%
    mutate(date          = submit_date)

  data <-
    data %>%
    filter(message %in% "paper card inserted") %>%
    rename(start_action = message,
           start_time   = time,
           start_date   = date) %>%
    mutate(date         = start_date) %>%
    left_join(data_ejected, by = 'voter_number') %>%
    left_join(data_language, by = "voter_number")



  data$ward     <- gsub("-.*", "", data$poll_place_name)
  data$division <- gsub(".*-", "", data$poll_place_name)
  data$ward     <- as.numeric(as.character(data$ward))
  data$division <- as.numeric(as.character(data$division))
  data$location <- paste(data$ward, data$division)

  data$date_start_time  <- paste(data$start_date, data$start_time)
  data$date_start_time  <- ymd_hms(data$date_start_time)
  data$date_submit_time <- paste(data$submit_date, data$submit_time)
  data$date_submit_time <- ymd_hms(data$date_submit_time)

  data$hour              <- lubridate::hour(data$date_submit_time)
  data$vote_time_seconds <- as.numeric(data$date_submit_time - data$date_start_time)
  data$vote_time_minutes <- data$vote_time_seconds / 60 # Makes the units be minutes

  data$file <- file

  data$spanish_selected[is.na(data$spanish_selected)] <- 0
  data$english_selected[is.na(data$english_selected)] <- 0
  data$both_languages_selected[is.na(data$both_languages_selected)] <- 0
  data$only_spanish_selected[is.na(data$only_spanish_selected)] <- 0

  return(data)
}
