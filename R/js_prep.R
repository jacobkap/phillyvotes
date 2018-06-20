library(dplyr)
library(fastDummies)
# Creates the JSON files for use in the javascript site
# js_results_prep_func(all_votes)
# office_choices <- js_num_selected_prep(data)
# js_cond_table_prep(office_choices$office)


setwd("C:/Users/user/Dropbox/R_project/phillyvotes/data/clean_data/")
files <- list.files(pattern = ".rda")
BIR_files <- files[grep("bir.rda", files)]
CAR_files <- files[grep("car.rda", files)]

files <- files[!grepl("Special", files)]
for (file in CAR_files) {
  setwd("C:/Users/user/Dropbox/R_project/phillyvotes/data/clean_data/")
  file_name <- gsub(".rda", "", file)
  election <- gsub("_...$", "", file_name)
  load(file)
  assign("data", get(file_name))
  do.call("rm", list(as.name(file_name)))
  js_vote_time(data, election)


  message(file_name)
  # all_wards <- 1:66
  # print(all_wards[!all_wards %in% unique(data$ward)])
  rm(data); gc(); Sys.sleep(3)
}

js_vote_time <- function(data, election) {
  final_data <- data.frame(stringsAsFactors = FALSE)
  df <-
    data %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(votes = n()) %>%
    dplyr::filter(hour %in% 7:20) %>%
    dplyr::mutate(vote_percent = round(votes / sum(votes) * 100, digits =  1),
                  ward         = "All",
                  division     = "All")
  final_data <- dplyr::bind_rows(final_data, df)

  # ward-wide data
  df <-
    data %>%
    dplyr::group_by(hour, ward) %>%
    dplyr::summarise(votes = n()) %>%
    dplyr::filter(hour %in% 7:20) %>%
    dplyr::group_by(ward) %>%
    dplyr::mutate(vote_percent = round(votes / sum(votes) * 100, digits =  1),
                  division    = "All")
  df$ward <- as.character(df$ward)
  final_data <- dplyr::bind_rows(final_data, df)

  # ward-division data
  df <-
    data %>%
    dplyr::group_by(hour, ward, division) %>%
    dplyr::summarise(votes = n()) %>%
    dplyr::filter(hour %in% 7:20) %>%
    dplyr::group_by(ward, division) %>%
    dplyr::mutate(vote_percent = round(votes / sum(votes) * 100, digits =  1))
  df$ward <- as.character(df$ward)
  df$division <- as.character(df$division)
  final_data <- dplyr::bind_rows(final_data, df)

  for (selected_ward in unique(final_data$ward)) {
    temp <-
      final_data %>%
      dplyr::filter(ward %in% selected_ward) %>%
      dplyr::select(-ward)

    temp <- t(temp)
    temp <- jsonlite::toJSON(temp, pretty = TRUE)
    setwd(paste0("C:/Users/user/Dropbox/phillyvotingtool/data/vote_time/",
                 election))
    write(temp, paste0(election, "_ward_", selected_ward,  "_time.json"))
  }
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


  for (office in offices) {
    results <-
      final_matrix %>%
      dplyr::filter(category %in% office) %>%
      dplyr::select(-category)
    # If no votes for a candidate-location pair, adds a row with a
    # votes value of 0
    results <- dummy_rows(results, select_columns = c("candidate",
                                                      "location"),
                          dummy_value = 0)
    results <- t(results)
    results <- jsonlite::toJSON(results, pretty = TRUE)
    setwd(paste0("C:/Users/user/Dropbox/phillyvotingtool/data/election_results/",
                 election))
    write(results,
          paste0("election_results_", office, ".json"))
  }
}
# This produces a vector of the names of offices where a person can vote for
# more than one candidate - useful for making the options a user can select
# for the num_selected_graph
js_num_selected <- function(data, election) {
  office_choices <- data.frame(stringsAsFactors = FALSE)
  offices <- get_offices(data)
  data <-
    data %>%
    dplyr::filter(tolower(candidate) != "write in" &
                    category %in% offices)
  results <- data.frame(stringsAsFactors = FALSE)

  # city-wide data

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

  total_uniqueIDs <-
    data %>%
    group_by(democrat) %>%
    dplyr::summarize(total_uniqueIDs = length(unique(uniqueID)))

  ward_total_uniqueIDs <-
    data %>%
    group_by(ward, democrat) %>%
    dplyr::summarize(total_uniqueIDs = length(unique(uniqueID)))

  ward_division_total_uniqueIDs <-
    data %>%
    group_by(ward, division, democrat) %>%
    dplyr::summarize(total_uniqueIDs = length(unique(uniqueID)))

  df <-
    data %>%
    dplyr::group_by(category, uniqueID, democrat) %>%
    dplyr::summarize(count         = n()) %>%
    dplyr::group_by(category, count, democrat) %>%
    dplyr::tally() %>%
    dplyr::mutate(ward     = "All",
                  division = "All") %>%
    left_join(total_uniqueIDs)
  temp <-
    df %>%
    dplyr::group_by(category) %>%
    dplyr::mutate(n     = total_uniqueIDs - sum(n),
                  count = 0) %>%
    dplyr::distinct(category, .keep_all = TRUE)
  df <- bind_rows(df, temp)

  results <- bind_rows(results, df)

  # ward-wide data
  df <-
    data %>%
    dplyr::group_by(category, ward, uniqueID, democrat) %>%
    dplyr::summarize(count = n()) %>%
    dplyr::group_by(category, ward, count, democrat) %>%
    dplyr::tally() %>%
    dplyr::mutate(division = "All") %>%
    left_join(ward_total_uniqueIDs)
  temp <-
    df %>%
    dplyr::group_by(category, ward) %>%
    dplyr::mutate(n     = total_uniqueIDs - sum(n),
                  count = 0) %>%
    dplyr::distinct(category, .keep_all = TRUE)
  df <- bind_rows(df, temp)
  df$ward <- as.character(df$ward)
  results <- bind_rows(results, df)

  # ward-division wide data
  # ward-wide data
  df <-
    data %>%
    dplyr::group_by(category, ward, division, uniqueID, democrat) %>%
    dplyr::summarize(count = n()) %>%
    dplyr::group_by(category, ward, division, count, democrat) %>%
    dplyr::tally() %>%
    left_join(ward_division_total_uniqueIDs)
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

  multiple_choice_offices <-
    df %>%
    dplyr::group_by(category) %>%
    dplyr::summarize(max_choices = max(count)) %>%
    dplyr::filter(max_choices > 1)

  results <-
    results %>%
    dplyr::filter(category %in% multiple_choice_offices$category) %>%
    dplyr::mutate(percent = round(n / total_uniqueIDs * 100, 1)) %>%
    dplyr::select(-democrat,
                  -n,
                  -total_uniqueIDs)

  for (office in unique(results$category)) {
    final_results <-
      results %>%
      dplyr::filter(category %in% office) %>%
      dplyr::select(-category) %>%
      dplyr::arrange(desc(percent))
    final_results <- t(final_results)
    final_results <- jsonlite::toJSON(final_results, pretty = TRUE)
    setwd(paste0("C:/Users/user/Dropbox/phillyvotingtool/data/num_selected/",
                 election))
    write(final_results,
          paste0("num_selected_", office, ".json"))
  }

  return(multiple_choice_offices)
}

js_cond_table_prep <- function(data) {
  offices <- get_offices(data)
  data$office_location <- paste(data$category,
                                data$ward,
                                data$division)

  data <- data.table::data.table(data)
  data <- data[category %in% offices]
  # data <-
  #   data %>%
  #   dplyr::filter(category %in% offices)



  for (office in unique(data$category)) {
    # office_temp <- data %>%
    #   dplyr::filter(category %in% office)

    office_temp <- data[category %in% office]

    final_list <- list()
    unique_locations <- c("All-All",
                         paste0(1:66, "-All"),
                         unique(office_temp$location))
    for (i in 1:length(unique_locations)) {
      selected_location <- unique_locations[i]
      if (selected_location == "All-All") {
        df <- office_temp
      } else if (grepl("[0-9]-All", selected_location)) {
        selected_ward <- gsub("-All", "", selected_location)
        df <- office_temp[ward %in% selected_ward]
      } else {
        df <- office_temp[location %in% selected_location]
      }

      df <- as.data.frame(df, stringsAsFactors = FALSE)
      df <- conditional_table(df)
      names_col <- data.frame(Candidate = c("", names(df), "Total"),
                              stringsAsFactors = FALSE)
      names_row <- data.frame(t(names(df)),
                              stringsAsFactors = FALSE)
      names(names_row) <- names(df)
      df <- bind_rows(names_row, df)
      df <- bind_cols(names_col, df)
      df$ward <- gsub("-.*", "", selected_location)
      if (df$ward[1] != "All") df$ward <- as.numeric(df$ward)
      df$division <- gsub(".*-", "", selected_location)
      if (df$division[1] != "All") {
        df$division <- as.numeric(df$division)
      }
      #df <- as.matrix(df)
      final_list[[i]] <- df
      message(i)
    }
    for (final_ward in c("All", 1:66)) {

      cond <- sapply(final_list, function(x) x$ward == final_ward)
      cond <- sapply(cond, "[", 1)
      to_save = final_list[cond]

      to_save <- jsonlite::toJSON(to_save, pretty = TRUE)
      setwd(paste0("C:/Users/user/Dropbox/phillyvotingtool/data/cond_table/", election))
      write(to_save, paste0("cond_table_",
                               office,
                               "ward_",
                               final_ward,
                               ".json"))

    }


  }
}

get_offices <- function(data) {
  offices <- unique(data$category)
  offices <- grep("INSPECTOR|JUDGE OF ELECTION|CHARTER|RETENTION|BOND|WARD|AMENDMENT|DELEGATE|GENERAL",
                  offices,
                  value = TRUE,
                  ignore.case = TRUE,
                  invert = TRUE)
  offices <- offices[!is.na(offices)]
  return(offices)
}
