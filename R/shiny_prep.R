 # setwd("C:/Users/user/Dropbox/R_project/phillyvotes/shiny_data")
 # results_prep <- results_prep_func(all_votes)
 # num_selected <- num_selected_prep(all_votes)
 # cond_tables <- cond_table_prep(all_votes)
 # save(results_prep, file = "results_prep.rda")
 # save(num_selected, file = "num_selected.rda")
 # save(cond_tables, file = "cond_tables.rda")



# This produces a vector of the names of offices where a person can vote for
# more than one candidate - useful for making the options a user can select
# for the num_selected_graph
num_selected_prep <- function(data) {
  num_selected <- list()
  for (office in unique(data$category)) {
    temp <- data %>% dplyr::filter(category == office  &
                                     tolower(candidate) != "write in") %>%
      group_by(uniqueID) %>%
      summarize(count = n()) %>%
      group_by(count) %>%
      tally()
    if (nrow(temp) > 1) {
      num_selected[[length(num_selected) + 1]] <-
        num_selected_graph(df = temp,
                           office = office)
      names(num_selected)[length(num_selected)] <- office
    }
  }
  return(num_selected)
}

# Creates the conditional probability tables so it is faster on the
# shiny site.
cond_table_prep <- function(data) {
  cond_tables <- list()
  for (office in names(num_selected)) {
    cond_table <- conditional_table(data, office)

    cond_tables[[length(cond_tables) + 1]] <- cond_table
    names(cond_tables)[length(cond_tables)] <- office
  }
  return(cond_tables)
}


# Prepares the data for the Election Results tab. Here it just gives the
# max number of candidates for use in the slider ui.
results_prep_func <- function(data) {
  results_prep <- data.frame(matrix(ncol = 2,
                                    nrow = length(unique(data$category))))
  names(results_prep) <- c("Office", "max_candidates")
  results_prep$Office <- unique(data$category)

  for (office in unique(data$category)) {
    df <- data %>% dplyr::filter(category == office) %>%
      dplyr::group_by(candidate) %>%
      dplyr::summarise(votes = n()) %>%
      dplyr::arrange(desc(votes))
    results_prep$max_candidates[results_prep$Office == office] <- nrow(df)
  }
  return(results_prep)
}

