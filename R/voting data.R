philly_votes <- function(file_location){
  ### Convert PDF to lines of text ###
  # suppressWarnings() simply hides and error that cannot be
  # addressed but doesn't impact the output
  txt <- suppressMessages(pdftools::pdf_text(file_location))
  txt <- unlist(strsplit(txt, split = "\n"))
  txt <- trimws(txt)


  ### Begin searches for searching for specific items ###
  # These use regexpr() to find the starting and stopping location of each
  # item followed by data cleaning to remove
  #   extra text that may be pulled in addition to the text we
  # actually want.

  ### Voting Location ###
  page <- txt
  page[!grepl("Page.* [0-9]", txt, ignore.case = TRUE)] <- NA
  page <- readr::parse_number(page)

  ### Ballot Position ###
  # * in middle ensures full code is pulled
  ballot.loc <- regexpr("[0-9]*[A-Z]", txt)
  ballot <- regmatches(txt, ballot.loc)
  ballot <- gsub("^[[:alpha:]]$", "", ballot)
  ballot.stop <- ballot.loc + attributes(ballot.loc)$match.length

  ### Serial Number ###
  serial.loc <- regexpr("[0-9]{6}", txt)
  # Since serial number should appear in first position,
  # length is same as end position
  serial.stop <- attributes(serial.loc)$match.length
  serial <- substr(txt,
                   start = serial.loc,
                   stop = serial.stop)

  ### Voting Location ###
  location.loc <- regexpr(" [0-9][0-9]-[0-9][0-9]", txt)
  location.stop <- location.loc +
                   attributes(location.loc)$match.length
  location <- substr(txt,
                     start = location.loc,
                     stop = location.stop)

  ### Voter Record ###
  record.loc <- regexpr("[0-9]+ OF +[0-9]+", txt)
  record.stop <- record.loc + attributes(record.loc)$match.length
  record <- substr(txt,
                   start = record.loc,
                   stop = record.stop)
  # Method pulls phrase preceeding voter record and extra space
  record <- gsub(" OF.*", "", record)

  ### Candidate Name ###
  # Need first A-Z for middle initial
  txt <- gsub("TH DISTRICT", "TH DISTRICT    ", txt)
  name.loc <- regexpr("([A-Z]+ )*[A-Z]+, [A-Z]+(\\s+[[[:alpha:]])?|Write In|NO\\s+NO|YES\\s+SI|No Vote", txt)
  name.stop <- name.loc + attributes(name.loc)$match.length
  name <- substr(txt,
                 start = name.loc,
                 stop = name.stop)
  name <- stringr::str_trim(name)
  name <- gsub("(.*), (.*)", "\\2 \\1", name)

  ### Votes ###
  vote.loc <- regexpr("[0-9]$", txt)
  vote.stop <- vote.loc + attributes(vote.loc)$match.length
  vote <- substr(txt,
                 start = vote.loc,
                 stop = vote.stop)
  # Make sure we only keep observations that make sense given the
  # ballot information
  vote <- ifelse(ballot == "", "", vote)

  ### Category ###
  # Instead of search for some complex regular expression, we take advantage of all the other information we have found
  #   including the end of the item to the left and the start of
  # the item to the right
  category.start <- ifelse(ballot == "", -1, ballot.stop + 1)
  category.stop <- ifelse(ballot == "", -1, name.loc - 1)
  category <- substr(txt,
                     start = category.start,
                     stop = category.stop)

  ### Merge Data ###
  # We haven't changed order, so observations will still line up properly
  data <- data.frame(location              = location,
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
  data$category <- gsub("-dem| - dem|-D$", " - Democrat", data$category)
  data$category <- gsub("-rep| - rep|-R$", " - Republican", data$category)
  data$category <- gsub("-D-", " - Democrat - ", data$category)
  data$category <- gsub("-R-", " - Republican - ", data$category)

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
  words <- gsub("The", "the", words)
  words <- gsub("And", "and", words)
  words <- gsub(" Dist ", " District ", words)
  words <- gsub(" Dist$", " District", words)
  return(words)
}



