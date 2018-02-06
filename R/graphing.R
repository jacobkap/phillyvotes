

#results_barplot("JUDGE OF THE COURT OF COMMON PLEAS-REP", TRUE)
#results_barplot("JUDGE OF THE COURT OF COMMON PLEAS-REP")
results_barplot <- function(category, percent = FALSE) {
  df <- all_votes[all_votes$category %in% category, ]
  df <- aggregate(votes ~ candidate, data = df, FUN = sum)
  df <- df[order(df$votes, decreasing = TRUE), ]

  ylabel <- "# of Votes"

  if (percent)  {
    df$votes <- round((df$votes / sum(df$votes) * 100), 2)
    ylabel <- "% of Votes"
  }


  p <- ggplot(data = df, aes(x = reorder(candidate, votes), y = votes)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_minimal() +
    ggtitle(category) +
    ylab(ylabel) +
    xlab("")

  if (percent) {
    p + geom_text(aes(label= paste0(votes, "%")), color = "white", hjust = 1.1)
  }else {
    p + geom_text(aes(label=votes), color = "white", hjust = 1.1)
  }
}

#make_num_selected_graph("JUDGE OF THE COURT OF COMMON PLEAS-REP")
make_num_selected_graph <- function(category) {
  df <- all_votes[all_votes$category %in% category, ]
  df <- as.numeric(table(df$uniqueID))
  df <- data.frame(table(df))
  names(df)[1] <- "number"
  df$percent <- round((df$Freq / sum(df$Freq) * 100), 2)

  ggplot(data = df, aes(x = reorder(number, -percent), y = percent)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    ggtitle(category, subtitle = "# Selected by Voters, by Percentage") +
    xlab("Number of Selections Made") +
    ylab("%") +
    geom_text(aes(label= paste0(percent, "%")), color = "white", vjust = 1.1)
}
