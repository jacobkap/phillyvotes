library(tidyverse)
library(fastDummies)
library(stringr)
library(lubridate)
library(data.table)
#
# wards_divisions <- data.frame(wards = 1:66,
#                               divisions = c(21, 27, 22, 21, 31,
#                                             18, 23, 30, 17, 29,
#                                             20, 24, 25, 11, 19,
#                                             18, 29, 17, 19, 11,
#                                             45, 29, 23, 19, 24,
#                                             23, 23, 18, 18, 17,
#                                             19, 31, 24, 42, 32,
#                                             41, 21, 21, 46, 51,
#                                             26, 25, 25, 19, 25,
#                                             23, 14, 23, 22, 30,
#                                             28, 28, 23, 22, 29,
#                                             41, 28, 44, 25, 23,
#                                             28, 26, 25, 18, 23,
#                                             46))
# locations <- c()
# for (i in 1:nrow(wards_divisions)) {
#   temp <- paste0(wards_divisions$wards[i], "-", 1:wards_divisions$divisions[i])
#   locations <- c(locations, temp)
# }
#
setwd("C:/Users/user/Dropbox/R_project/phillyvotes/data/clean_data/")
files <- list.files(pattern = ".rda")
BIR_files <- files[grep("bir.rda", files)]
CAR_files <- files[grep("car.rda", files)]
BIR_files_reg <- BIR_files[!grepl("special", BIR_files)]

for (file in BIR_files_reg[1:13]) {
  setwd("C:/Users/user/Dropbox/R_project/phillyvotes/data/clean_data/")
  file_name <- gsub(".rda", "", file)
  election <- gsub("_...$", "", file_name)
  load(file)
  assign("data", get(file_name))
  do.call("rm", list(as.name(file_name)))
  print(file_name)

  z <- sort(unique(data$category))
  z <- z[!grepl("Ward|judge|inspector", z, ignore.case = TRUE)]
  #print(z)
  message("\n")
  #print(sort(unique(data$candidate)))
  #print(grep("court order|write", unique(data$candidate)))
  cands <- unique(data$candidate)
  #print(cands[nchar(cands) == min(nchar(cands))])
  print(cands[nchar(cands) == max(nchar(cands))])
  message("\n\n\n\n\n")
  #
  #
  # js_results(data, election)
  # js_num_selected(data, election)
  system.time(js_cond_table_prep(data, election))
  #vote_time_data <-   js_vote_time(data, election)
  #
  #
  #
  #   # zz = data %>%
  #   #   group_by(serial) %>%
  #   #    summarize(time = min(time))
  #   #  print(table(hour(zz$time)))
  #
  #   #  all_wards <- 1:66
  #   #  print(all_wards[!all_wards %in% unique(data$ward)])
  #   #  data$temp_location <- paste0(data$ward, "-", data$division)
  #   #  print(locations[!locations %in% unique(data$temp_location)])
  rm(data); gc(); # Sys.sleep(3)
}

js_vote_time <- function(data, election) {

  data$vote_time_minutes[data$vote_time_minutes > 30] <- NA
  data$vote_time_minutes[data$vote_time_minutes == 0] <- NA
  final_data <- data.frame(stringsAsFactors = FALSE)

  data$hour[data$hour < 7] <- 7
  data$hour[data$hour > 19] <- 19

  # city-wide data
  df <-
    data %>%
    dplyr::filter(!is.na(hour)) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(votes = n(),
                     vote_time = median(vote_time_minutes,
                                        na.rm = TRUE)) %>%
    dplyr::mutate(vote_percent = round(votes / sum(votes) * 100, digits =  1),
                  ward         = "All",
                  division     = "All")
  final_data <- dplyr::bind_rows(final_data, df)

  # ward-wide data
  df <-
    data %>%
    dplyr::filter(!is.na(hour)) %>%
    dplyr::group_by(hour, ward) %>%
    dplyr::summarise(votes = n(),
                     vote_time = median(vote_time_minutes,
                                        na.rm = TRUE)) %>%
    dplyr::group_by(ward) %>%
    dplyr::mutate(vote_percent = round(votes / sum(votes) * 100, digits =  1),
                  division    = "All")
  df$ward <- as.character(df$ward)
  final_data <- dplyr::bind_rows(final_data, df)

  # ward-division data
  df <-
    data %>%
    dplyr::filter(!is.na(hour)) %>%
    dplyr::group_by(hour, ward, division) %>%
    dplyr::summarise(votes = n(),
                     vote_time = median(vote_time_minutes,
                                        na.rm = TRUE)) %>%
    dplyr::group_by(ward, division) %>%
    dplyr::mutate(vote_percent = round(votes / sum(votes) * 100,
                                       digits =  1))
  df$ward <- as.character(df$ward)
  df$division <- as.character(df$division)
  final_data <- dplyr::bind_rows(final_data, df)
  print(summary(final_data$vote_time))

  for (selected_ward in unique(final_data$ward)) {
    temp <-
      final_data %>%
      dplyr::filter(ward %in% selected_ward) %>%
      dplyr::select(-ward) %>%
      dplyr::mutate(vote_time = round(vote_time, 2)) %>%
      dplyr::select(hour,
                    votes,
                    vote_percent,
                    division,
                    vote_time)

    temp <- t(temp)
    temp <- jsonlite::toJSON(temp, pretty = TRUE)
    setwd(paste0("C:/Users/user/Dropbox/phillyvotingtool/data/vote_time/",
                 election))
    write(temp, paste0(election, "_ward_", selected_ward,  "_time.json"))

  }
  possible_wards <- unique(final_data$ward)
  possible_wards <- possible_wards[possible_wards != "All"]
  possible_wards <- sort(as.numeric(possible_wards))
  possible_wards <- c("All", possible_wards)
  possible_wards <- jsonlite::toJSON(possible_wards,
                                     pretty = TRUE)
  write(possible_wards,
        paste0("wards.json"))

  possible_locations <- unique(paste0(final_data$ward, "-",
                                      final_data$division))
  possible_locations <- possible_locations[!grepl("All", possible_locations)]
  possible_locations <- jsonlite::toJSON(possible_locations,
                                         pretty = TRUE)
  write(possible_locations,
        paste0("locations.json"))

  return(final_data)
}

js_results <- function(data, election) {
  offices <- get_offices(data)
  data <-
    data %>%
    dplyr::filter(category %in% offices)

  final_matrix <- data.frame(stringsAsFactors = FALSE)
  # city-wide data
  df <-
    data %>%
    dplyr::group_by(category, candidate) %>%
    dplyr::summarise(votes = n()) %>%
    dplyr::arrange(desc(votes))
  df$ward <- "All"
  df$division <- "All"
  final_matrix <- dplyr::bind_rows(final_matrix, df)

  # ward-wide data
  df <-
    data %>%
    dplyr::group_by(category, candidate, ward) %>%
    dplyr::summarise(votes = n()) %>%
    dplyr::arrange(desc(votes)) %>%
    dplyr::mutate(ward = as.character(ward))
  df$division <- "All"
  final_matrix <- dplyr::bind_rows(final_matrix, df)

  # ward-division data
  df <-
    data %>%
    dplyr::group_by(category, candidate, ward, division) %>%
    dplyr::summarise(votes = n()) %>%
    dplyr::arrange(desc(votes))
  df$ward     <- as.character(df$ward)
  df$division <- as.character(df$division)
  final_matrix <- dplyr::bind_rows(final_matrix, df)

  # Saves a file to generate the available offices dropdown
  all_offices <- unique(df$category)
  setwd(paste0("C:/Users/user/Dropbox/phillyvotingtool/data/election_results/",
               election))
  all_offices <- jsonlite::toJSON(all_offices,
                                  pretty = TRUE)
  write(all_offices,
        paste0("election_results_office_choices.json"))


  for (office in offices) {
    results <-
      final_matrix %>%
      dplyr::filter(category %in% office) %>%
      dplyr::select(-category)

    possible_wards <- unique(results$ward)
    possible_wards <- possible_wards[possible_wards != "All"]
    possible_wards <- sort(as.numeric(possible_wards))
    possible_wards <- c("All", possible_wards)
    possible_wards_vector <- possible_wards
    possible_wards <- jsonlite::toJSON(possible_wards,
                                       pretty = TRUE)
    write(possible_wards,
          paste0("wards_", office, ".json"))

    possible_locations <- unique(paste0(results$ward, "-",
                                        results$division))
    possible_locations <- possible_locations[!grepl("All", possible_locations)]
    possible_locations <- jsonlite::toJSON(possible_locations,
                                           pretty = TRUE)
    write(possible_locations,
          paste0("locations_", office, ".json"))

    # If no votes for a candidate-location pair, adds a row with a
    # votes value of 0
    #
    #     results <- dummy_rows(results, select_columns = c("candidate",
    #                                                       "ward",
    #                                                       "division"),
    #                           dummy_value = 0)
    for (selected_ward in possible_wards_vector) {
      final_results <-
        results %>%
        dplyr::filter(ward %in% selected_ward) %>%
        dplyr::select(-ward)
      final_results <- t(final_results)
      final_results <- jsonlite::toJSON(final_results, pretty = TRUE)
      setwd(paste0("C:/Users/user/Dropbox/phillyvotingtool/data/election_results/",
                   election))
      write(final_results,
            paste0("election_results_", office, "_ward_", selected_ward, ".json"))
    }
  }
}
# This produces a vector of the names of offices where a person can vote for
# more than one candidate - useful for making the options a user can select
# for the num_selected_graph
js_num_selected <- function(data, election) {
  if (!grepl("special", election)) {

    office_choices <- data.frame(stringsAsFactors = FALSE)
    offices <- get_offices(data)
    data <-
      data %>%
      dplyr::filter(tolower(candidate) != "write in" &
                      category %in% offices)
    results <- data.frame(stringsAsFactors = FALSE)

    # Ward 6, division 5 has an issue in this election where there are duplicate
    # reports of votes (some voter reported in 2 files) so this removes
    # that location entirely.
    if (election == "election_2017_general") {
      data <-
        data %>%
        dplyr::filter(location != "06-05")
    }


    if (grepl("primary", election, ignore.case = TRUE)) {
      data <-
        data %>%
        dplyr::group_by(uniqueID) %>%
        dplyr::mutate(democrat = ifelse(grepl("democrat", category,
                                              ignore.case = TRUE),
                                        1, 0),
                      republican = ifelse(grepl("republican", category,
                                                ignore.case = TRUE),
                                          1, 0)) %>%
        dplyr::filter(democrat == 1 | republican == 1)


    } else {
      # This is just a placeholder to give every value 1 so everything is
      # grouped together during general elections when votes aren't split
      # by party. It doesn't actually mean everyone is Democrat.
      data$democrat <- 1
    }

    # total_uniqueIDs <-
    #   data %>%
    #   group_by(democrat) %>%
    #   dplyr::summarize(total_uniqueIDs = length(unique(uniqueID)))
    #
    # ward_total_uniqueIDs <-
    #   data %>%
    #   group_by(ward, democrat) %>%
    #   dplyr::summarize(total_uniqueIDs = length(unique(uniqueID)))

    ward_division_total_uniqueIDs <-
      data %>%
      group_by(ward, division, democrat) %>%
      dplyr::summarize(total_uniqueIDs = length(unique(uniqueID)))

    # ward-division wide data
    df <-
      data %>%
      dplyr::group_by(category, ward, division, uniqueID, democrat) %>%
      dplyr::summarize(count = n()) %>%
      dplyr::group_by(category, ward, division, count, democrat) %>%
      dplyr::tally() %>%
      dplyr::left_join(ward_division_total_uniqueIDs)
    temp <-
      df %>%
      dplyr::group_by(category, ward, division) %>%
      dplyr::mutate(n     = total_uniqueIDs - sum(n),
                    count = 0) %>%
      dplyr::distinct(category, .keep_all = TRUE)
    df <- bind_rows(df, temp)
    df$ward <- as.character(df$ward)
    df$division <- as.character(df$division)
    results <- bind_rows(results, df)

    # ward-wide data
    ward_total_uniqueIDs <-
      results %>%
      group_by(category, ward, democrat) %>%
      dplyr::summarize(total_uniqueIDs = sum(n))
    ward_total_uniqueIDs$ward <- as.numeric(ward_total_uniqueIDs$ward)

    df <- data.frame(df)
    df <-
      df %>%
      dplyr::mutate(ward = as.numeric(ward)) %>%
      dplyr::group_by(category, ward, count, democrat) %>%
      dplyr::rename(temp_n = n) %>%
      dplyr::summarize(n = sum(temp_n)) %>%
      dplyr::mutate(division = "All") %>%
      dplyr::left_join(ward_total_uniqueIDs)
    df$ward <- as.character(df$ward)
    results <- bind_rows(results, df)


    # City-wide data
    df <-
      df %>%
      dplyr::select(-ward, -division) %>%
      dplyr::group_by(category, democrat, count) %>%
      dplyr::rename(temp_n = n) %>%
      dplyr::summarize(n = sum(temp_n),
                       total_uniqueIDs = sum(total_uniqueIDs)) %>%
      dplyr::group_by(category, count, democrat) %>%
      dplyr::mutate(ward     = "All",
                    division = "All")
    results <- bind_rows(df, results)



    multiple_choice_offices <-
      results %>%
      dplyr::group_by(category) %>%
      dplyr::summarize(max_choices = max(count))

    results <-
      results %>%
      dplyr::filter(category %in% multiple_choice_offices$category) %>%
      dplyr::mutate(percent = round(n / total_uniqueIDs * 100, 1)) %>%
      dplyr::select(-democrat,
                    -n,
                    -total_uniqueIDs)
    results$democrat <- NULL
    results <- data.frame(results)
    for (office in unique(results$category)) {
      final_results <-
        results %>%
        dplyr::filter(category %in% office) %>%
        dplyr::select(-category) %>%
        dplyr::arrange(count)

      setwd(paste0("C:/Users/user/Dropbox/phillyvotingtool/data/num_selected/",
                   election))
      possible_wards <- unique(final_results$ward)
      possible_wards <- possible_wards[possible_wards != "All"]
      possible_wards <- sort(as.numeric(possible_wards))
      possible_wards <- c("All", possible_wards)
      possible_wards_vector <- possible_wards
      possible_wards <- jsonlite::toJSON(possible_wards,
                                         pretty = TRUE)
      write(possible_wards,
            paste0("wards_", office, ".json"))

      possible_locations <- unique(paste0(final_results$ward, "-",
                                          final_results$division))
      possible_locations <- possible_locations[!grepl("All", possible_locations)]
      possible_locations <- jsonlite::toJSON(possible_locations,
                                             pretty = TRUE)
      write(possible_locations,
            paste0("locations_", office, ".json"))

      for (selected_ward in possible_wards_vector) {
        real_final_results <-
          final_results %>%
          dplyr::filter(ward %in% selected_ward) %>%
          dplyr::select(-ward)
        real_final_results <- t(real_final_results)
        real_final_results <- jsonlite::toJSON(real_final_results, pretty = TRUE)
        setwd(paste0("C:/Users/user/Dropbox/phillyvotingtool/data/num_selected/",
                     election))
        write(real_final_results,
              paste0("num_selected_", office, "_ward_", selected_ward, ".json"))
      }
    }
    setwd(paste0("C:/Users/user/Dropbox/phillyvotingtool/data/num_selected/",
                 election))
    multiple_choice_offices <- as.data.frame(multiple_choice_offices)
    multiple_choice_offices <- jsonlite::toJSON(multiple_choice_offices,
                                                pretty = TRUE)
    write(multiple_choice_offices,
          paste0("num_selected_office_choices.json"))
  }
}


js_cond_table_prep <- function(data, election) {

  # Ward 6, division 5 has an issue in this election where there are duplicate
  # reports of votes (some voter reported in 2 files) so this removes
  # that location entirely.
  if (election == "election_2017_general") {
    data <-
      data %>%
      dplyr::filter(location != "06-05")
  }
  source('C:/Users/user/Dropbox/R_project/phillyvotes/R/conditional_table.R')
  offices <- get_offices(data)
  # offices <- offices[grep("general assembly",
  #                         offices, ignore.case = TRUE,
  #                         invert = TRUE)]
  setwd(paste0("C:/Users/user/Dropbox/phillyvotingtool/data/cand_comb/", election))
  all_offices <- jsonlite::toJSON(offices,
                                  pretty = TRUE)
  write(all_offices,
        paste0("cand_comb_office_choices.json"))

  data$office_location <- paste(data$category,
                                data$ward,
                                data$division)


  if (grepl("primary", election, ignore.case = TRUE)) {
    dem_offices <- offices[grepl("democrat", offices, ignore.case = TRUE)]
    rep_offices <- offices[grepl("republican", offices, ignore.case = TRUE)]
    dem_office_lists <- combn(dem_offices, 2, simplify = FALSE)
    rep_office_lists <- combn(rep_offices, 2, simplify = FALSE)
    office_lists <- append(as.list(offices), dem_office_lists)
    office_lists <- append(office_lists, rep_office_lists)
  } else {
    office_lists <- combn(offices, 2, simplify = FALSE)
    office_lists <- append(as.list(offices), office_lists)
  }

  # Since you cannot vote for two district, removes if both of the offices
  # are districts
  gen_assembly_rep     <- "repres.*assembly.*district"
  gen_assembly_senator <- "senator.*assembly.*district"
  congress_rep         <- "repres.*congress.*district"
  district_council_rep <- "district council"
  for (i in length(office_lists):1) {
    num_districts_1 <- str_count(tolower(office_lists[[i]]), gen_assembly_rep)
    num_districts_2 <- str_count(tolower(office_lists[[i]]), gen_assembly_senator)
    num_districts_3 <- str_count(tolower(office_lists[[i]]), congress_rep)
    num_districts_4 <- str_count(tolower(office_lists[[i]]), district_council_rep)
    num_districts_5 <- str_count(tolower(office_lists[[i]]),
                                 "general assembly")
    if (sum(num_districts_1) > 1 |
        sum(num_districts_2) > 1 |
        sum(num_districts_3) > 1 |
        sum(num_districts_4) > 1 |
        sum(num_districts_5) > 1) {
      office_lists[[i]] <- NULL
    }
  }



  data <- data.table::data.table(data)
  data <- data[data$category %in% offices]
  gc(); Sys.sleep(1)

  for (n in 1:length(office_lists)) {

    office <- office_lists[[n]]
    office <- sort(office)
    office_temp <- data[data$category %in% office]
    gc()


    # If the offices DO share any common wards, allow to continue
    location1 <- unique(office_temp$location[office_temp$category %in% office[1]])
    location2 <- unique(office_temp$location[office_temp$category %in% office[length(office)]])
    common_locations <- intersect(location1, location2)
    if (length(common_locations) > 0) {
      office_temp <- office_temp[office_temp$location %in% common_locations]
      gc()

      common_locations <- sort(common_locations)
      unique_locations <- c("All-All",
                            sort(paste0(unique(office_temp$ward), "-All")),
                            common_locations)
      possible_wards <- c()
      possible_locations <- c()
      final_data <- data.frame(stringsAsFactors = FALSE)

      for (i in 1:length(unique_locations)) {
        selected_location <- unique_locations[i]
       # message(selected_location)
        if (selected_location == "All-All") {
          df <- office_temp
        } else if (grepl("[0-9]-All", selected_location)) {
          selected_ward <- gsub("-All", "", selected_location)
          df <- office_temp[ward %in% selected_ward]
        } else {
          df <- office_temp[location %in% selected_location]
        }


        df <- conditional_table(office_temp, df)
        temp <- data.frame(t(names(df)), stringsAsFactors = FALSE)
        names(temp) <- names(df)
        df <- bind_rows(temp, df)

        names(df) <- paste0("col_", 1:ncol(df))
        df$ward <- gsub("-.*", "", selected_location)
        if (df$ward[1] != "All"){
          df$ward <- as.character(as.numeric(df$ward))
        }
        df$division <- gsub(".*-", "", selected_location)
        if (df$division[1] != "All") {
          df$division <- as.character(as.numeric(df$division))
        }
        final_data <- dplyr::bind_rows(final_data, df)


        possible_wards <- c(possible_wards,
                            unique(df$ward))
        possible_locations <- c(possible_locations,
                                selected_location)
      }
      office_save_name <- paste(office, collapse = " ")
      possible_wards <- unique(possible_wards)
      possible_wards <- possible_wards[possible_wards != "All"]
      possible_wards <- sort(as.numeric(possible_wards))
      possible_wards <- c("All", possible_wards)
      possible_wards_vector <- possible_wards
      possible_wards <- jsonlite::toJSON(possible_wards,
                                         pretty = TRUE)

      possible_locations <- possible_locations[!grepl("All", possible_locations)]
      possible_locations <- jsonlite::toJSON(possible_locations,
                                             pretty = TRUE)
      write(possible_locations,
            paste0("locations_", office_save_name, ".json"))


      for (k in 1:ncol(final_data)) {
        final_data[, k][is.na(final_data[, k])] <- 0
        final_data[, k][final_data[, k] == "V1"] <- ""
        final_data[, k][grepl("To Remove", final_data[, k])] <- ""

      }

      write(possible_wards,
            paste0("wards_", office_save_name, ".json"))


      for (selected_ward in possible_wards_vector) {

        real_final_data <-
          final_data %>%
          dplyr::filter(ward %in% selected_ward) %>%
          dplyr::select(-ward)

        if (length(real_final_data) > 0) {
          readr::write_csv(real_final_data, path = paste0("cand_comb_",
                                                          office_save_name,
                                                          "_ward_",
                                                          selected_ward,
                                                          ".csv"))
        }
      }
    }
 #   message(office)
  }
}



get_offices <- function(data) {
  offices <- unique(data$category)
  state_committee <- grep("state committee",
                          offices,
                          value = TRUE,
                          ignore.case = TRUE)
  offices <- grep("INSPECTOR|JUDGE OF ELECTION|CHARTER|RETENTION|BOND|WARD|AMENDMENT|ALTERNATE DELEGATE|COMMITTEE|PROPOSED",
                  offices,
                  value = TRUE,
                  ignore.case = TRUE,
                  invert = TRUE)
  offices <- c(offices, state_committee)
  offices <- offices[!is.na(offices)]
  offices <- sort(offices)
  return(offices)
}


