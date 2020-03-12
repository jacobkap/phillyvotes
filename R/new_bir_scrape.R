source(here::here('R/scrape_BIR.R'))
setwd("~/philly_voting_data")
library(dplyr)
library(crimeutils)
candidate_file <- readxl::read_xlsx("2019 General Candidates Public - With Button Numbers.xlsx") %>%
  rename_all(crimeutils::fix_column_names) %>%
  mutate_all(tolower) %>%
  rename(candidate = candidate_name,
         category  = office,
         candidate_party = party) %>%
  filter(candidate_party != "") %>%
  select(-button_number)
candidate_file$category[grep("1|4|5|6|7|8|9|10",
                             candidate_file$category)] <-
  paste0(candidate_file$category[grep("1|4|5|6|7|8|9|10",
                                      candidate_file$category)], "th district")
candidate_file$category[grep("2", candidate_file$category)] <-
  paste0(candidate_file$category[grep("2", candidate_file$category)], "nd district")
candidate_file$category[grep("3", candidate_file$category)] <-
  paste0(candidate_file$category[grep("3", candidate_file$category)], "rd district")

setwd("~/philly_voting_data/2019_general_election_bir")
files <- list.files()




# data_1_100000 <- scrape_bir_list(files[1:100000], candidate_file)
# data_1_100000 <- data.table::rbindlist(data_1_100000)
# setwd(here::here("data/bir_general_2019_temp"))
# save(data_1_100000, file = "data_1_100000.rda")
# rm(data_1_100000)
# gc()

# setwd("~/philly_voting_data/2019_general_election_bir")
# data_100001_200000 <- scrape_bir_list(files[100001:200000], candidate_file)
# data_100001_200000 <- data.table::rbindlist(data_100001_200000)
# setwd(here::here("data/bir_general_2019_temp"))
# save(data_100001_200000, file = "data_100001_200000.rda")
# rm(data_100001_200000)
# gc()

# setwd("~/philly_voting_data/2019_general_election_bir")
# data_200001_end <- scrape_bir_list(files[200001:length(files)], candidate_file)
# data_200001_end <- data.table::rbindlist(data_200001_end)
# setwd(here::here("data/bir_general_2019_temp"))
# save(data_200001_end, file = "data_200001_end.rda")
# rm(data_200001_end)
# gc()

setwd(here::here("data/bir_general_2019_temp"))
load("data_1_100000.rda")
load("data_100001_200000.rda")
load("data_200001_end.rda")
library(dplyr)
election_2019_general_bir <-
  data_1_100000 %>%
  bind_rows(data_100001_200000) %>%
  bind_rows(data_200001_end)
sort(table(election_2019_general_bir$category))
sort(table(election_2019_general_bir$candidate))
table(election_2019_general_bir$straight_party_ticket_selected)
table(election_2019_general_bir$straight_party_ticket_party)
table(election_2019_general_bir$straight_party_ticket_actual)
table(election_2019_general_bir$straight_party_ticket_vote_other_party)
table(election_2019_general_bir$straight_party_ticket_undervote)
table(election_2019_general_bir$undercount)
table(election_2019_general_bir$undercount_excluding_retention_or_proposals)

summary(election_2019_general_bir)
setwd(here::here("data/clean_data"))
save(election_2019_general_bir, file = "election_2019_general_bir.rda")

scrape_bir_list <- function(files, candidate_file) {
  data <- rep(list(NA), length(files))
  for (i in 1:length(files)) {
    temp <- scrape_new_bir(files[i], candidate_file)
    data[[i]] <- temp
    if (i %% 1000 == 0) {
      print(i)
    }
  }
  return(data)
}


scrape_new_bir <- function(file, candidate_file) {

  txt <- suppressMessages(pdftools::pdf_text(file))
  txt <- unlist(strsplit(txt, split = "\n"))
  txt <- trimws(txt)
  txt <- tolower(txt)


  location       <- txt[grep("precinct:|division:", txt)]
  location       <- gsub("precinct: |division: ", "", location)
  serial_number  <- txt[grep("cast vote record: ", txt)]
  serial_number  <- gsub("cast vote record: ", "", serial_number)
  serial_number  <- readr::parse_number(serial_number)
  machine_serial <- txt[grep("machine serial: ", txt)]
  machine_serial <- gsub("machine serial: ", "", machine_serial)
  tabuator_cvr   <- txt[grep("tabulator cvr: ", txt)]
  tabuator_cvr   <- gsub("tabulator cvr: ", "", tabuator_cvr)
  party          <- txt[grep("party: ", txt)]
  party          <- gsub("party: ", "", party)
  ballot_style   <- txt[grep("ballot style: ", txt)]
  ballot_style   <- gsub("ballot style: ", "", ballot_style)
  reporting_group   <- txt[grep("reporting group: ", txt)]
  reporting_group   <- gsub("reporting group: ", "", reporting_group)


  overvote <- 0
  if (any(grepl("overvoted", txt, ignore.case = TRUE))) {
    overvote <- 1
  }



  votes_only <- txt[(grep("vote for: ", txt)[1]-1):length(txt)]
  votes_only <- gsub("^katherine gilmore richardson  ",
                     "katherine gilmore richardson (15826)   ",
                     votes_only)
  votes_only <- gsub("^katherine gilmore richardson c",
                     "katherine gilmore richardson (15826)   c",
                     votes_only)
  votes_only <- gsub("^\\(15826\\)$", "", votes_only)
  votes_only <- votes_only[votes_only != ""]

  votes_only <- gsub("yes \\(15617\\)$",
                     "yes \\(15617\\)    counted",
                     votes_only)
  votes_only <- data.frame(candidate = votes_only,
                           category  = votes_only,
                           stringsAsFactors = FALSE)
  votes_only$category[grep("count|undervote|vote for|overvoted",
                           votes_only$category)] <- NA
  votes_only$category <- zoo::na.locf(votes_only$category,
                                      na.rm = FALSE)




  votes_only$straight_party_ticket_selected         <- NA
  votes_only$straight_party_ticket_party            <- NA
  votes_only$straight_party_ticket_actual           <- NA
  votes_only$straight_party_ticket_vote_other_party <- NA
  votes_only$straight_party_ticket_undervote        <- NA



  votes_only           <- votes_only[-grep("vote for", votes_only$candidate), ]
  votes_only           <- votes_only[votes_only$candidate != votes_only$category, ]
  votes_only           <- votes_only[votes_only$candidate != "counted", ]
  votes_only$candidate <- gsub("overvote*", NA, votes_only$candidate)
  votes_only$candidate <- gsub(" \\(.*", "", votes_only$candidate)
  votes_only$category  <- gsub(" \\(.*", "", votes_only$category)
  votes_only$candidate <- gsub("undervoted.*", NA, votes_only$candidate)

  votes_only$undercount <- 0
  votes_only$undercount_excluding_retention_or_proposals <- 0
  if (any(is.na(votes_only$candidate))) {
    votes_only$undercount <- 1
  }
  if (any(is.na(votes_only$candidate)[grep("retention|^prop",
                                           votes_only$category,
                                           ignore.case = TRUE,
                                           invert = TRUE)])) {
    votes_only$undercount_excluding_retention_or_proposals <- 1
  }



  votes_only <- dplyr::left_join(votes_only, candidate_file,
                                 by = c("candidate", "category"))

  # Do straight party ticket section
  if(any(is.na(votes_only$candidate[votes_only$category %in%
                                    "straight party ticket"]))) {
    votes_only$straight_party_ticket_selected <- 0
    votes_only <- votes_only[!votes_only$category %in% "straight party ticket", ]
    votes_only <- votes_only[!is.na(votes_only$candidate), ]
  } else {
    votes_only$straight_party_ticket_actual    <- 1

    votes_only$straight_party_ticket_selected <- 1
    votes_only$straight_party_ticket_party    <- votes_only$candidate[votes_only$category    %in% "straight party ticket"]
    votes_only <- votes_only[!votes_only$category %in% "straight party ticket", ]
    votes_only <- votes_only[!is.na(votes_only$candidate), ]

    candidate_file_party_only <-
      candidate_file %>%
      filter(candidate_party %in% unique(votes_only$straight_party_ticket_party),
             category        %in% unique(votes_only$category))


    if (nrow(votes_only) > 0 & any(!unique(votes_only$candidate_party[!is.na(votes_only$candidate_party)]) %in%
                                   unique(votes_only$straight_party_ticket_party))) {
      votes_only$straight_party_ticket_vote_other_party <- 1
      votes_only$straight_party_ticket_actual <- 0
    } else {
      votes_only$straight_party_ticket_vote_other_party <- 0
    }


    if (!(all(candidate_file_party_only$candidate %in%
              votes_only$candidate))) {
      votes_only$straight_party_ticket_undervote <- 1
      votes_only$straight_party_ticket_actual    <- 0
    } else {
      votes_only$straight_party_ticket_undervote <- 0
    }

  }



  if (nrow(votes_only) == 0) {
    votes_only <- votes_only[1,]
    votes_only$candidate <- "No Vote"
    votes_only$offices_voted_for    <- 0
    votes_only$candidates_voted_for <- 0
    votes_only$offices_voted_for_excluding_retentions_or_proposal    <- 0
    votes_only$candidates_voted_for_excluding_retentions_or_proposal <- 0
  } else {
    votes_only$candidate <- capitalize_words(votes_only$candidate)
    votes_only           <- fix_category_names(votes_only)

    votes_only$offices_voted_for    <- length(unique(votes_only$category))
    votes_only$candidates_voted_for <- length(votes_only$candidate)
    votes_only$offices_voted_for_excluding_retentions_or_proposal <-
      length(unique(votes_only$category[grep("retention|^prop",
                                             votes_only$category,
                                             ignore.case = TRUE,
                                             invert = TRUE)]))
    votes_only$candidates_voted_for_excluding_retentions_or_proposal    <-
      length(votes_only$candidate[grep("retention|^prop",
                                       votes_only$category,
                                       ignore.case = TRUE,
                                       invert = TRUE)])
  }

  votes_only$overvote <- overvote

  data <- data.frame(
    location,
    serial_number,
    uniqueID          = serial_number,
    machine_serial,
    tabuator_cvr,
    party,
    ballot_style,
    reporting_group,
    ballot_position   = NA,
    votes_only, #####################################
    ward              = substr(location, 1, 2),
    division          = substr(location, 4, 5),
    file,
    uniqueID_with_pdf = paste(serial_number, file),
    stringsAsFactors  = FALSE)

  return(data)
}

#unzip_unfolder_files()
unzip_unfolder_files <- function() {
  setwd("~/philly_voting_data")
  zip_folders <- list.files(pattern = "zip")



  for (folder in zip_folders) {
    unzip(folder)
  }

  folders <- list.files()
  folders <- folders[-grep("\\.zip$|general_election_(bir|car)|storage|.csv$|.pdf$|.xlsx|storage",
                           folders)]

  for (folder in folders) {
    setwd(folder)
    print(folder)
    files <- list.files(pattern = "c.pdf$")
    file.copy(files, "~/philly_voting_data/storage")
    folder_temp <- list.files()
    if (length(folder_temp) > 0) {
      if (grepl(".pdf", folder_temp, ignore.case = TRUE)) {
        folder_temp <- folder_temp[-grep(".pdf", folder_temp)]
      }
      for (temp in folder_temp) {
        setwd(temp)
        files <- list.files(pattern = "c.pdf$")
        file.copy(files, "~/philly_voting_data/storage")
        setwd("~/philly_voting_data/storage")
        files <- list.files()
        file.rename(files, paste0(folder, "_", files))
        files <- list.files()
        file.copy(files, "~/philly_voting_data/2019_general_election_bir",
                  overwrite = TRUE)
        file.remove(files)
        #        setwd("..")
      }
    }
    setwd("..")

  }
}

