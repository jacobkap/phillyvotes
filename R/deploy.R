# File to deploy shiny app

deployApp(appFiles = c("R/", "all_votes.rda",
                       "server.R", "ui.R", "global.R"),
          appTitle = "Philadelphia Votes")
