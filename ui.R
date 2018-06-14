shinyUI(fluidPage(theme = shinythemes::shinytheme("flatly"),

                  # Application title
                  navbarPage("Philadelphia Election Results Tool",
                             tabPanel("Election Results",
                                      # Sidebar with a slider input for number of bins

                                      sidebarLayout(
                                        sidebarPanel(
                                          selectizeInput("results_office",
                                                         label = "Select a ballot position",
                                                      choices = sort(unique(all_votes$category))),
                                          selectizeInput("ward_choice_elect_results",
                                                         label = "Select a voting ward",
                                                      choices = c("All", 1:66)),
                                          uiOutput("num_of_results"),


                                          checkboxInput("percent",
                                                        label = "Show Percent of Votes",
                                                        value = FALSE),
                                          "Select a voting ward by clicking on the map",
                                          leafletOutput("results_map")
                                        ),


                                        # Show a plot of the generated distribution
                                        mainPanel(
                                          plotOutput("results_plot")
                                        ))),
                             tabPanel("Choices per Office",
                                      # Sidebar with a slider input for number of bins

                                      sidebarLayout(
                                        sidebarPanel(
                                          selectizeInput("num_office",
                                                      label = "Select a ballot position",
                                                      choices = sort(unique(names(num_selected)))),
                                          selectizeInput("ward_choice_choices_per_office", label = h3("Select a ward"),
                                                      choices = c("All", 1:66)),
                                          "Select a voting ward by clicking on the map",
                                          leafletOutput("choices_map")
                                          ),

                                        # Show a plot of the generated distribution
                                        mainPanel(
                                          plotOutput("num_voters_graph")
                                        )
                                      )
                             ),
                             tabPanel("Candidate Combinations",
                                      # Sidebar with a slider input for number of bins

                                      sidebarLayout(
                                        sidebarPanel(
                                          selectizeInput("cond_office", label = "Select a ballot position",
                                                      choices = sort(unique(names(num_selected)))),

                                          uiOutput("cond_table_num_selected"),
                                          selectizeInput("ward_choice_cond_table",
                                                         label = "Select a voting ward",
                                                      choices = c("All", 1:66)),
                                          "Select a voting ward by clicking on the map",
                                          leafletOutput("cond_table_map"),

                                          h5("This table shows the number of people
                                    who voted for each candidate conditional
                                    on voting for another candidate."),
                                          br(),
                                          h5("When a candidate column aligns with that
                                    candidate's row, that shows how many people
                                    voted for only that candidate."),
                                          br(),
                                           h5("For example,
                                    row 1 column 1 shows how many people only voted for
                                    that candidate. Row 1 column 2 shows how many people
                                    voted for the candidate in column 2 who ALSO voted
                                    for the candidate in row 1.")
                                        ),

                                        # Show a plot of the generated distribution
                                        mainPanel(
                                          plotOutput("cond_legend", height = "45px"),
                                          tableOutput("conditional_table")
                                        )
                                      )
                             )

                  )))
