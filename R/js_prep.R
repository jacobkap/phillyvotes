# Creates the JSON files for use in the javascript site
# js_results_prep_func(all_votes)
# office_choices <- js_num_selected_prep(data)
# js_cond_table_prep(office_choices$office)

# Prepares the data for the Election Results tab. Here it just gives the
# max number of candidates for use in the slider ui.

js_results_prep_func <- function(data) {

  offices <- unique(data$category)
  offices <- grep("INSPECTOR|JUDGE OF ELECTION|CHARTER|RETENTION|BOND",
                          offices,
                          value = TRUE,
                          ignore.case = TRUE,
                          invert = TRUE)
  wards_divisions <-
    data %>%
    group_by(ward) %>%
    summarize(max_divisions = max(division))
  offices <- offices[!is.na(offices)]
  for (office in offices) {
    final_matrix <- data.frame(stringsAsFactors = FALSE)
    df <- data
    for (ward in c("All", 1:66)) {
      df <- data
      df <-
        df %>%
        dplyr::filter(category == office)

      if (ward != "All") {
        df <-
          df %>%
          filter(ward == ward)
      }


      df <-
        df %>%
        dplyr::group_by(candidate) %>%
        dplyr::summarise(votes = n()) %>%
        dplyr::arrange(desc(votes))
      if (nrow(df) == 0) {
        df <- data.frame(candidate = "No Candidate", votes = 0,
                         stringsAsFactors = FALSE)
      }
      df$ward <- ward
      final_matrix <- bind_rows(final_matrix, df)

    }
    final_matrix <- t(final_matrix)
    final_matrix <- jsonlite::toJSON(final_matrix, pretty = TRUE)
    setwd("C:/Users/user/Dropbox/phillyvotingtool/data/election_results")
    write(final_matrix,
          paste0(election, "_election_results_", office, ".json"))
  }
}

  # This produces a vector of the names of offices where a person can vote for
# more than one candidate - useful for making the options a user can select
# for the num_selected_graph

js_num_selected_prep <- function(data) {
  office_choices <- data.frame(stringsAsFactors = FALSE)

  for (office in unique(data$category)) {
    df <- data
    for (location in c("All", 1:66)) {
      df <- data
      df <-
        df %>%
        dplyr::filter(category == office &
                        tolower(candidate) != "write in")

      if (location != "All") {
        df <-
          df %>%
          filter(ward == location)
        total_uniqueIDs <-
          data %>%
          filter(ward == location)
        total_uniqueIDs <- length(unique(total_uniqueIDs$uniqueID))
      } else {
        total_uniqueIDs <- length(unique(data$uniqueID))
      }




      df <-
        df %>%
        group_by(uniqueID) %>%
        summarize(count = n()) %>%
        group_by(count) %>%
        tally()
      if (nrow(df) > 1) {

        new_row <- data.frame(office = office,
                              max_choices = max(df$count),
                              stringsAsFactors = FALSE)
        office_choices <- dplyr::bind_rows(office_choices, new_row)

        zero_choices <- tibble(count = 0,
                               n = (total_uniqueIDs - sum(df$n)))
        df <- dplyr::bind_rows(df, zero_choices)


        df$percent <- df$n / sum(df$n) * 100
        df$n <- NULL
        df <- df[order(df$percent, decreasing = TRUE),]
        df$percent <- round(df$percent)


        df <- t(as.matrix(df))
        df <- toJSON(df, pretty = TRUE)
        setwd("C:/Users/user/Dropbox/phillyvotingtool/data/num_selected")
        write(df, paste0("num_selected_", office, "_ward_", location, ".json"))
      } else break
    }
  }
  office_choices <- office_choices[!duplicated(office_choices$office), ]
  return(office_choices)
}

js_cond_table_prep <- function(offices) {
  for (office in offices) {
    df <- data
    for (location in c("All", 1:66)) {
      df <- data
      df <-
        df %>%
        dplyr::filter(category == office)

      if (location != "All") {
        df <-
          df %>%
          filter(ward == location)
      }

      df <- conditional_table(df, office)
      df <- df[[1]]
      names_col <- data.frame(Candidate = c("", names(df), "Total"),
                              stringsAsFactors = FALSE)
      names_row <- data.frame(t(names(df)),
                              stringsAsFactors = FALSE)
      names(names_row) <- names(df)
      df <- bind_rows(names_row, df)
      df <- bind_cols(names_col, df)
      df <- as.matrix(df)
      df <- toJSON(df, pretty = TRUE)
      setwd("C:/Users/user/Dropbox/phillyvotingtool/data/cond_table")
      write(df, paste0("cond_table_", office, "_ward_", location, ".json"))
    }
  }
}
