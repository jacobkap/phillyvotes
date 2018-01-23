philly_votes <- function(file.location){
  ### Convert PDF to lines of text ###
  # suppressWarnings() simply hides and error that cannot be addressed but doesn't impact the output
  doc <- suppressMessages(pdftools::pdf_text(file.location))
  txt <- unlist(strsplit(doc, split = "\n"))
  txt <- trimws(txt)

  if (any(grepl("cartridge audit log report", txt[1:10], ignore.case = TRUE))) {
    return(data.frame())
  }

  ### Begin searches for searching for specific items ###
  # These use regexpr() to find the starting and stopping location of each item followed by data cleaning to remove
  #   extra text that may be pulled in addition to the text we actually want.

  ### Ballot Position ###
  # * in middle ensures full code is pulled
  ballot.loc <- regexpr("[0-9]*[A-Z]", txt)
  ballot <- regmatches(txt, ballot.loc)

  ballot.start <- as.numeric(ballot.loc)
  ballot.stop <- ballot.start + attributes(ballot.loc)$match.length
  ballot <- gsub("^[[:alpha:]]$", "", ballot)
  ballot <- trimws(ballot)


  ### Serial Number ###
  serial.loc <- regexpr("[0-9]{6}", txt)
  serial.start <- as.numeric(serial.loc)
  # Since serial number should appear in first position, length is same as end position
  serial.stop <- attributes(serial.loc)$match.length
  serial <- substr(txt,
                   start = serial.start,
                   stop = serial.stop)

  ### Voting Location ###
  location.loc <- regexpr(" [0-9][0-9]-[0-9][0-9]", txt)
  location.start <- as.numeric(location.loc)
  location.stop <- location.start + attributes(location.loc)$match.length
  location <- substr(txt,
                     start = location.start,
                     stop = location.stop)
  # Method pulls extra space
  location <- trimws(location)

  ### Voter Record ###
  record.loc <- regexpr("[0-9]+ OF +[0-9]+", txt)
  record.start <- as.numeric(record.loc)
  record.stop <- record.start + attributes(record.loc)$match.length
  record <- substr(txt,
                   start = record.start,
                   stop = record.stop)
  # Method pulls phrase preceeding voter record and extra space
  record <- gsub(" OF.*", "", record)

  ### Candidate Name ###
  # Need first A-Z for middle initial
  name.loc <- regexpr("([A-Z] )?[A-Za-z]+, [A-Za-z]+(\\s+[[[:alpha:]])?", txt)
  name.start <- as.numeric(name.loc)
  name.stop <- name.start + attributes(name.loc)$match.length
  name <- substr(txt,
                 start = name.start,
                 stop = name.stop)
  # Find Ballot Measures with no candidate name - matched with "YES SI"
  measure1.loc <- regexpr("YES SI", txt)
  measure1.start <- as.numeric(measure1.loc)
  measure1.stop <- measure1.start + attributes(measure1.loc)$match.length
  measure1 <- substr(txt,
                     start = measure1.start,
                     stop = measure1.stop)
  # Find Ballot Measures with no candidate name - matched with "NO NO"
  measure2.loc <- regexpr("NO NO", txt)
  measure2.start <- as.numeric(measure2.loc)
  measure2.stop <- measure2.start + attributes(measure2.loc)$match.length
  measure2 <- substr(txt,
                     start = measure2.start,
                     stop = measure2.stop)
  # Track when the names start, we will not use this for the actual name but rather for another item
  name.start <- ifelse(name.start == -1, # If no name
                       ifelse(measure1.start == -1, # If no "YES SI" vote
                              measure2.start, # Return "NO NO"
                              measure1.start), # Return "YES SI"
                       name.start) # Return Name
  # Continue cleaning by first merge all data together (as no one can have a name and ballot measure in the same location
  #   this will not have to worry about copying over data)
  name <- ifelse(name == "", ifelse(measure1 == "", measure2, measure1), name)
  name <- trimws(name) # Trim trailing and leading spaces

  ### Votes ###
  vote.loc <- regexpr("[0-9]$", txt)
  vote.start <- as.numeric(vote.loc)
  vote.stop <- vote.start + attributes(vote.loc)$match.length
  vote <- substr(txt,
                 start = vote.start,
                 stop = vote.stop)
  # Make sure we only keep observations that make sense given the ballot information
  vote <- ifelse(ballot == "", "", vote)

  ### Category ###
  # Instead of search for some complex regular expression, we take advantage of all the other information we have found
  #   including the end of the item to the left and the start of the item to the right
  category.start <- ifelse(ballot == "", -1, ballot.stop + 1)
  category.stop <- ifelse(ballot == "", -1, name.start - 1)
  category <- substr(txt,
                     start = category.start,
                     stop = category.stop)
  category <- trimws(category) # Trim trailing and leading spaces

  ### Merge Data ###
  # We haven't changed order, so observations will still line up properly
  txt.data <- data.frame(location   = location,
                         sernum     = serial,
                         voterecord = record,
                         position   = ballot,
                         category   = category,
                         candidate  = name,
                         votes      = vote,
                         stringsAsFactors = FALSE)

  # Turn "" into NA - mostly for dealing with them easier in R
  txt.data <- data.frame(apply(txt.data, FUN = function(x) (ifelse(x == "", NA, x)), MARGIN = 2),
                         stringsAsFactors = FALSE)

  # Fill in relevant columns
  # na.locf take the first non-NA value of an object and then fills it foward until the next non-NA value. great for
  #   filling in data based on order
  txt.data$location <- zoo::na.locf(txt.data$location, na.rm = FALSE)
  txt.data$sernum <- zoo::na.locf(txt.data$sernum, na.rm = FALSE)
  txt.data$voterecord <- zoo::na.locf(txt.data$voterecord, na.rm = FALSE)
  txt.data$uniqueID   <-  paste(txt.data$location, txt.data$sernum, txt.data$voterecord)

  # Remove uncessary rows
  txt.data <- txt.data[!is.na(txt.data$votes), ]

  # Fix Format
  txt.data$voterecord <- as.numeric(as.character(txt.data$voterecord))
  txt.data$votes <- as.numeric(as.character(txt.data$votes))

  return(txt.data)

}
