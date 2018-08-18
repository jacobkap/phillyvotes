philly_votes <- function(file_location){
  ### Convert PDF to lines of text ###
  # suppressWarnings() simply hides and error that cannot be
  # addressed but doesn't impact the output
  txt <- suppressMessages(pdftools::pdf_text(file_location))
  txt <- unlist(strsplit(txt, split = "\n"))
  txt <- trimws(txt)

  txt <- gsub("CO COOPERMAN ", "CO COOPERMAN ", txt, ignore.case = TRUE)
  txt <- gsub("BUTC BUTCHART ", "BUTC BUTCHART ", txt, ignore.case = TRUE)
  txt <- gsub("J C CUNNINGHAM", "J C CUNNINGHAM", txt, ignore.case = TRUE)
  txt <- gsub("PALU PALUMBO", "PALU PALUMBO", txt, ignore.case = TRUE)
  txt <- gsub("B BR BRONSON", "B BR BRONSON", txt, ignore.case = TRUE)
  txt <- gsub("FOX FOX", "FOX FOX", txt, ignore.case = TRUE)
  txt <- gsub("JOHN MILTO YOUNGE", "JOHN MILTO YOUNGE", txt, ignore.case = TRUE)
  txt <- gsub("GWENDOLY BRIGHT", "GWENDOLY BRIGHT", txt, ignore.case = TRUE)
  txt <- gsub("W TUC TUCKER", "W TUC TUCKER", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)
  txt <- gsub("", "", txt, ignore.case = TRUE)

  txt <- gsub("CONVENTION (.*) DIST ", "CONVENTION \\1 DISTRICT    ",
              txt, ignore.case = TRUE)
  txt <- gsub("DISTRI |DISTR ", "DISTRICT    ", txt, ignore.case = TRUE)
  txt <- gsub("CONGRESSIONAL D ", "CONGRESSIONAL DISTRICT     ", txt, ignore.case = TRUE)
  txt <- gsub("SENATORIAL DISTRICT ", "SENATORIAL DISTRICT    ", txt, ignore.case = TRUE)
  txt <- gsub("SENATORIAL DIST ", "SENATORIAL DISTRICT    ", txt, ignore.case = TRUE)
  txt <- gsub("TH DISTRICT ", "TH DISTRICT    ", txt, ignore.case = TRUE)
  txt <- gsub("ST DISTRICT ", "ST DISTRICT    ", txt, ignore.case = TRUE)
  txt <- gsub("RD DISTRICT ", "RD DISTRICT    ", txt, ignore.case = TRUE)
  txt <- gsub("ND DISTRICT ", "ND DISTRICT    ", txt, ignore.case = TRUE)
  txt <- gsub("CONGRESSION ", "CONGRESSION       ", txt, ignore.case = TRUE)
  txt <- gsub(" DISTRICT-REP ", " DISTRICT-REP          ", txt, ignore.case = TRUE)
  txt <- gsub(" DISTRICT-DEM ", " DISTRICT-DEM          ", txt, ignore.case = TRUE)
  txt <- gsub(" DISTRICT-R ", " DISTRICT-R          ", txt, ignore.case = TRUE)
  txt <- gsub(" DISTRICT-D ", " DISTRICT-D          ", txt, ignore.case = TRUE)
  txt <- gsub(" DIST-REP ", " DIST-REP          ", txt, ignore.case = TRUE)
  txt <- gsub(" DIST-DEM ", " DIST-DEM          ", txt, ignore.case = TRUE)
  txt <- gsub(" DIST - REP ", " DIST-REP          ", txt, ignore.case = TRUE)
  txt <- gsub(" DIST - DEM ", " DIST-DEM          ", txt, ignore.case = TRUE)
  txt <- gsub("ASSEMBLY - 1ST DIST ",
              "ASSEMBLY -  1ST DIST           ", txt, ignore.case = TRUE)
  txt <- gsub("ASSEMBLY - 13TH DIST ",
              "ASSEMBLY -  13TH DIST           ", txt, ignore.case = TRUE)
  txt <- gsub("ASSEMBLY - 2ND DIST ",
              "ASSEMBLY -  2ND DIST           ", txt, ignore.case = TRUE)
  txt <- gsub("5TH DISTR ", "5TH DISTR           ", txt, ignore.case = TRUE)
  txt <- gsub("2ND DIST ", "2ND DIST           ", txt, ignore.case = TRUE)
  txt <- gsub(" Assembly - 1 ", " Assembly - 1         ", txt, ignore.case = TRUE)
  txt <- gsub("A PE PECHKUROW ", "A PE    PECHKUROW ", txt, ignore.case = TRUE)
  txt <- gsub("DUMA DUMAS ", "DUMA      DUMAS ", txt, ignore.case = TRUE)
  txt <- gsub("CONVENTION-", "CONVENTION - ", txt, ignore.case = TRUE)
  txt <- gsub("CONVENTION ([0-9]) ", "CONVENTION \\1    ", txt, ignore.case = TRUE)

  txt <- gsub("DISTRICT ", "DISTRICT       ", txt, ignore.case = TRUE)
  txt <- gsub("DISTRICT\\s+ATTORNEY", "DISTRICT ATTORNEY", txt, ignore.case = TRUE)
  txt <- gsub("DISTRICT\\s+COUNCIL", "DISTRICT COUNCIL", txt, ignore.case = TRUE)
  txt <- gsub("-D      ", "-DEM        ", txt, ignore.case = TRUE)
  txt <- gsub("-D      ", "-DEM        ", txt, ignore.case = TRUE)
  txt <- gsub("-DEM ", "-DEM     ", txt, ignore.case = TRUE)
  txt <- gsub("-DEM-", " - DEMOCRAT - ", txt, ignore.case = TRUE)
  txt <- gsub("-REP-", " - REPUBLICAN - ", txt, ignore.case = TRUE)
  txt <- gsub("SENATORIAL D ", "SENATORIAL D    ", txt, ignore.case = TRUE)
  txt <- gsub("FRAZIER FRAZIER", "FRAZIER      FRAZIER", txt, ignore.case = TRUE)
  txt <- gsub("SAYLOR SAYLOR", "SAYLOR      SAYLOR", txt, ignore.case = TRUE)
  txt <- gsub("Castille Castille", "Castille        Castille", txt, ignore.case = TRUE)
  txt <- gsub("Panella Panella", "Panella           Panella", txt, ignore.case = TRUE)
  txt <- gsub("Baer Baer", "Baer            Baer", txt, ignore.case = TRUE)
  txt <- gsub("JEFFREY P M MINEHART", "JEFFREY P M       MINEHART", txt, ignore.case = TRUE)
  txt <- gsub("D O O KEEFE", "D O K          O KEEFE", txt, ignore.case = TRUE)
  txt <- gsub("D O K O KEEFE", "D O K          O KEEFE", txt, ignore.case = TRUE)
  txt <- gsub("F L LACHMAN ", "F L           LACHMAN ", txt, ignore.case = TRUE)
  txt <- gsub("MC MCINERNEY ", "MC           MCINERNEY ", txt, ignore.case = TRUE)
  txt <- gsub("Sch Schulman", "Sch       Schulman", txt, ignore.case = TRUE)
  txt <- gsub("WRIGH WRIGHT", "WRIGH       WRIGHT", txt, ignore.case = TRUE)
  txt <- gsub("D C Carrafiello", "D C       Carrafiello", txt, ignore.case = TRUE)
  txt <- gsub("E B BRINKLEY", "E B       BRINKLEY", txt, ignore.case = TRUE)
  txt <- gsub("STEVE JOHNSON", "STEVE      JOHNSON", txt, ignore.case = TRUE)
  txt <- gsub("FREDERICA A MASSIAH", "FREDERICA A       MASSIAH", txt, ignore.case = TRUE)
  txt <- gsub("A M MEANS", "A M         MEANS", txt, ignore.case = TRUE)
  txt <- gsub("FREDERICA A MASSIAH", "FREDERICA A         MASSIAH", txt, ignore.case = TRUE)
  txt <- gsub("YES\\s{1,}SI", "YES SI", txt, ignore.case = TRUE)
  txt <- gsub("NO\\s{1,}NO", "NO NO", txt, ignore.case = TRUE)
  txt <- gsub("CONGRESSIONA ", "CONGRESSIONAl DISTRICT     ", txt, ignore.case = TRUE)

  txt <- gsub("CONGRESSION ", "CONGRESSIONAl DISTRICT     ", txt, ignore.case = TRUE)
  txt <- gsub("ASSEMBLY - 1 JAMES", "ASSEMBLY - 1      JAMES", txt, ignore.case = TRUE)

  # Adds blank space at the end - for files without vote data
  txt <- paste0(txt, "     ")
  txt2 <- gsub("^[0-9]{6}", "", txt)
  txt2 <- stringr::str_trim(txt2)
  txt2 <- stringr::str_split(txt2, "\\s{2,}")


  ballot   <- get_split_pieces(txt2, 1)
  category <- get_split_pieces(txt2, 2)
  name     <- get_split_pieces(txt2, 3)
  vote     <- get_split_pieces(txt2, 4)

  if (any(grepl("NO VOTE", category, ignore.case = TRUE))) {
    name[tolower(category) == "no vote"] <- "NO VOTE"
    category[tolower(category) == "no vote"] <- NA
    vote[tolower(category) == "no vote"] <- NA
  }


  ### Begin searches for searching for specific items ###
  # These use regexpr() to find the starting and stopping location of each
  # item followed by data cleaning to remove
  #   extra text that may be pulled in addition to the text we
  # actually want.

  ### Voting Location ###
  page <- txt
  page[!grepl("Page.* [0-9]", txt)] <- NA
  page <- readr::parse_number(page)

  ### Serial Number ###
  serial <- gsub(".*(^[0-9]{6}).*", "\\1", txt)
  serial[!grepl("^[0-9]{6}", serial)] <- NA
  # serial.loc <- regexpr("[0-9]{6}", txt)
  # # Since serial number should appear in first position,
  # # length is same as end position
  # serial.stop <- attributes(serial.loc)$match.length
  # serial <- substr(txt,
  #                  start = serial.loc,
  #                  stop = serial.stop)




  ### Ballot Position ###
  # * in middle ensures full code is pulled
  # ballot.loc <- regexpr("[0-9]*[A-Z]", txt)
  # ballot <- regmatches(txt, ballot.loc)
  # ballot <- gsub("^[[:alpha:]]$", "", ballot)
  # ballot.stop <- ballot.loc + attributes(ballot.loc)$match.length

  ### Voting Location ###
  location.loc <- regexpr(" [0-9][0-9]-[0-9][0-9]", txt)
  location.stop <- location.loc +
    attributes(location.loc)$match.length
  location <- substr(txt,
                     start = location.loc,
                     stop = location.stop)

  ### Voter Record ###
  record.loc <- regexpr("[0-9]+\\s+OF +[0-9]+", txt)
  record.stop <- record.loc + attributes(record.loc)$match.length
  record <- substr(txt,
                   start = record.loc,
                   stop = record.stop)
  # Method pulls phrase preceeding voter record and extra space
  record <- gsub("OF.*", "", record)

  ### Candidate Name ###
  # Need first A-Z for middle initial
  #
  #   name.loc <- regexpr("([A-Z]+ )*[A-Z]+, ([A-Z]+ )*\\s{2}|Write In|NO\\s+NO|YES\\s+SI|No Vote| [A-Z]? [A-Z]+ NO\\s+NO| [A-Z]? [A-Z]+ YES\\s+SI", txt)
  #   name.stop <- name.loc + attributes(name.loc)$match.length
  #   name <- substr(txt,
  #                  start = name.loc,
  #                  stop = name.stop)
  name <- stringr::str_trim(name)
  name <- gsub("(.*), (.*)", "\\2 \\1", name)
  name <- gsub("\\s+", " ", name)
  #
  #   ### Votes ###
  #   vote.loc <- regexpr("[0-9]\\s+?$", txt)
  #   vote.stop <- vote.loc + attributes(vote.loc)$match.length
  #   vote <- substr(txt,
  #                  start = vote.loc,
  #                  stop = vote.stop)
  #   # Make sure we only keep observations that make sense given the
  #   # ballot information
  #   vote <- ifelse(ballot == "", "", vote)
  #   vote <- stringr::str_trim(vote)
  #
  #   ### Category ###
  #   # Instead of search for some complex regular expression, we take advantage of all the other information we have found
  #   #   including the end of the item to the left and the start of
  #   # the item to the right
  #   category.start <- ifelse(ballot == "", -1, ballot.stop + 1)
  #   category.stop <- ifelse(ballot == "", -1, name.loc - 1)
  #   category <- substr(txt,
  #                      start = category.start,
  #                      stop = category.stop)
  category <- stringr::str_trim(category)
  category <- gsub("\\s+", " ", category)
  category <- gsub("-R$", " - Republican", category)
  category <- gsub("-D$", " - Democrat", category)

  ### Merge Data ###
  # We haven't changed order, so observations will still line up properly
  data <- data.frame(location          = location,
                     serial_number     = serial,
                     voter_record      = record,
                     ballot_position   = ballot,
                     category          = category,
                     candidate         = name,
                     votes             = vote,
                     stringsAsFactors  = FALSE)

  data <- data.frame(sapply(data,  stringr::str_trim), stringsAsFactors = FALSE)


  # Makes category and candidate have proper capitalization
  # - original was all caps
  data$category <- sapply(data$category, simpleCap)
  data$category <- gsub("-dem$| - dem$|-D$", " - Democrat",
                        data$category)
  data$category <- gsub("-d-", " - Democrat - ",
                        data$category, ignore.case = TRUE)
  data$category <- gsub("-r-", " - Republican - ",
                        data$category, ignore.case = TRUE)
  data$category <- gsub("-rep$| - rep$|-R$", " - Republican",
                        data$category)
  data$category <- gsub("Congress-", "Congress - ",
                        data$category)
  data$category <- gsub("Council-", "Council - ",
                        data$category)
  data$category <- gsub("Assembly-", "Assembly - ",
                        data$category)
  data$category <- gsub("Committee-", "Committee - ",
                        data$category)
  data$category <- gsub("representative", "Representative",
                        data$category)
  data$category <- gsub("democratic", "Democratic",
                        data$category)
  data$category <- gsub("republican", "Republican",
                        data$category)
  data$category <- gsub("\\s+", " ",
                        data$category)
  data$category <- gsub("Democratic", "TEMPORARY",
                        data$category)
  data$category <- gsub("Democratocrat |dem[A-Z]+ |dem[A-Z]+$", "Democrat ",
                        data$category, ignore.case = TRUE)
  data$category <- gsub("RepRepublican |Republic[A-Z]+ |republic[A-Z]+$",
                        "Republican ",
                        data$category, ignore.case = TRUE)
  data$category <- gsub("TEMPORARY", "Democratic",
                        data$category)
  data$category <- gsub("Senatorial D$", "Senatorial District", data$category)
  data$category <- gsub(" Dist ", " District ", data$category)
  data$category <- gsub(" Distr | Distr$", " District ", data$category)
  data$category <- stringr::str_trim(data$category)

  data$candidate <- sapply(data$candidate, simpleCap)


  # Turn "" into NA - mostly for dealing with them easier in R
  data <- data.frame(apply(data, FUN = function(x) (ifelse(x %in%
                                                             c("", "NANA"),
                                                           NA,
                                                           x)),
                           MARGIN = 2),
                     stringsAsFactors = FALSE)

  # Fill in relevant columns
  # na.locf take the first non-NA value of an object and then
  # fills it foward until the next non-NA value. great for
  #   filling in data based on order
  data$location <- zoo::na.locf(data$location,
                                na.rm = FALSE)
  data$ward <- substr(data$location, 1, 2)
  data$division <- substr(data$location, 4 ,5)
  data$serial_number <- zoo::na.locf(data$serial_number,
                                     na.rm = FALSE)
  data$voter_record <- zoo::na.locf(data$voter_record,
                                    na.rm = FALSE)
  voter_record <- data$voter_record[!is.na(data$voter_record)]
  data$voter_record[is.na(data$voter_record)] <- as.numeric(voter_record[1]) - 1
  data$uniqueID   <-  paste(data$serial_number,
                            data$voter_record)
  data$file <- gsub(".*/|.pdf", "", file_location)

  # Fix Format
  data$voter_record <- as.numeric(as.character(data$voter_record))
  data$pdf_page     <- page
  data$pdf_page <- zoo::na.locf(data$pdf_page,
                                na.rm = FALSE,
                                fromLast = TRUE)
  data$uniqueID_with_pdf <- paste(data$uniqueID, data$file)

  data$votes <- as.numeric(as.character(data$votes))
  data$ward <- as.numeric(as.character(data$ward))
  data$division <- as.numeric(as.character(data$division))

  # Remove uncessary rows
  data <- data[!is.na(data$serial_number), ]
  data <- data[!is.na(data$candidate), ]


  return(data)

}

simpleCap <- function(words) {
  words <- tolower(words)
  words <- strsplit(words, " ")[[1]]
  words <- paste(toupper(substring(words, 1,1)),
                 substring(words, 2),
                 sep = "", collapse = " ")
  words <- gsub("Of", "of", words)
  words <- gsub("Dem", "dem", words)
  words <- gsub("Rep", "rep", words)
  words <- gsub(" At ", " at ", words)
  words <- gsub(" In ", " in ", words)
  words <- gsub(" To ", " to ", words)
  words <- gsub(" The ", " the ", words)
  words <- gsub(" And ", " and ", words)
  words <- gsub(" Dist ", " District ", words)
  words <- gsub(" Distr ", " District ", words)
  words <- gsub(" Dist$", " District", words)
  return(words)
}

get_split_pieces <- function(data, list_element) {
  results <-   sapply(data, function(x) {
    if (!grepl("[0-9]{1,}[A-Z]", x[1])) {
      return(NA)
    } else {
      return(x[list_element] )
    }
  })
  return(results)
}


