shinyUI(fluidPage(theme = shinythemes::shinytheme("flatly"),

                  # Application title
                  navbarPage("Philadelphia 2017 Primary Election",
                             tabPanel("Election Results",
                                      # Sidebar with a slider input for number of bins

                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput("results_office", label = h3("Select a ballot position"),
                                                      choices = sort(unique(all_votes$category))),
                                          sliderInput("num_of_results",
                                                      label = h3("Number of candidates to show"), min = 1,
                                                      max = 10, value = 6),
                                          checkboxInput("percent", label = "Show Percent of Votes", value = FALSE)
                                        ),

                                        # Show a plot of the generated distribution
                                        mainPanel(
                                          plotOutput("results_plot")
                                        ))),
                             tabPanel("Choices per Office",
                                      # Sidebar with a slider input for number of bins

                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput("num_office", label = h3("Select a ballot position"),
                                                      choices = sort(unique(names(num_selected))))
                                        ),

                                        # Show a plot of the generated distribution
                                        mainPanel(
                                          plotOutput("num_voters_graph")
                                        )
                                      )
                             ),
                             tabPanel("Conditional Table",
                                      # Sidebar with a slider input for number of bins

                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput("cond_office", label = h3("Select a ballot position"),
                                                      choices = sort(unique(names(num_selected)))),
                                          h4("This table shows the number of people
                                    who voted each candidate conditional
                                    on voting for another candidate."),
                                          br(),
                                          h4("When a candidate column aligns with that
                                    candidate's row, that shows how many people
                                    voted for only that candidate."),
                                          br(),
                                           h4("For example,
                                    row 1 column 1 shows how many people only voted for
                                    that candidate. Row 1 column 2 shows how many people
                                    voted for the candidate in column 2 who ALSO voted
                                    for the candidate in row 1.")
                                        ),

                                        # Show a plot of the generated distribution
                                        mainPanel(
                                          tableOutput("conditional_table"),
                                          plotOutput("cond_legend")
                                        )
                                      )
                             )

                  )))
