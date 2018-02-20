#
# results_barplot(all_votes, "JUDGE OF THE COURT OF COMMON PLEAS-DEM",
#                 percent = TRUE)
# results_barplot(all_votes, "JUDGE OF THE COURT OF COMMON PLEAS-DEM")
#
# results_barplot(all_votes, "JUDGE OF THE COURT OF COMMON PLEAS-DEM",
#                 percent = TRUE, top_n = 3)
# results_barplot(all_votes, "JUDGE OF THE COURT OF COMMON PLEAS-DEM",
#                 top_n = 3)
#
# results_barplot(all_votes, "JUDGE OF THE COURT OF COMMON PLEAS-DEM",
#                 percent = TRUE, top_n = 15)
# results_barplot(all_votes, "JUDGE OF THE COURT OF COMMON PLEAS-DEM",
#                 top_n = 15)

results_barplot <- function(data, office, percent = FALSE, top_n = 6) {



  df <- data %>% filter(category == office) %>%
    group_by(candidate) %>%
    summarise(votes = n()) %>%
    arrange(desc(votes)) %>%
    top_n(votes, n = top_n)

  ylabel <- "# of Votes"
  if (percent)  {
    df$votes <- round((df$votes / sum(df$votes) * 100), 2)
    ylabel <- "% of Votes"
  }


  p <- ggplot2::ggplot(df, aes(x = reorder(candidate, votes), y = votes)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_minimal() +
    ggtitle(office) +
    ylab(ylabel) +
    xlab("") +
    theme(axis.text.x=element_text(colour = "black")) +
    theme(axis.text.y=element_text(colour = "black"))


  if (percent) {
    p + geom_text(aes(label = paste0(votes, "%")), color = "white", hjust = 1.1)
  }else {
    p + geom_text(aes(label = votes), color = "white", hjust = 1.1)
  }
}

#make_num_selected_graph("JUDGE OF THE SUPERIOR COURT-DEM")
make_num_selected_graph <- function(data, office) {
  df <- data[data$category %in% office, ]
  df <- as.numeric(table(df$uniqueID))
  df <- data.frame(table(df))
  names(df)[1] <- "number"
  df$percent <- round((df$Freq / sum(df$Freq) * 100), 2)

  ggplot(data = df, aes(x = reorder(number, -percent), y = percent)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    ggtitle(categofficeory, subtitle = "# Selected by Voters, by Percentage") +
    xlab("Number of Selections Made") +
    ylab("%") +
    geom_text(aes(label= paste0(percent, "%")), color = "white", vjust = 1.1) +
    theme(axis.text.x=element_text(colour = "black")) +
    theme(axis.text.y=element_text(colour = "black"))

}
