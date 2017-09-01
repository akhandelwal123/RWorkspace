library(pdftools)
library(rJava)
library(xlsxjars)
library(xlsx)
library(WriteXLS)


# function to filter the characters 
filterChar <- function(c){
  y <- unique(na.omit(unlist(strsplit(unlist(c), "[^a-zA-Z's]+"))))
  z <- paste(y, collapse = ' ')
  return (trimws(z))
}

handleFoRtFp <- function(revSecondSplit,FinishingPosition,Time,FinalOdds){
  for (i in 1:10) {
    #2    1  .5  31.45 2.30 Bmpd, Kept To TasK  ==> these are 10 elemnts of revSecondSplit
    if (!grepl("^[A-Za-z,&1st\r]+$", revSecondSplit[i], perl = T)) {
      fo <- revSecondSplit[i]
      rt <- revSecondSplit[i + 1]
      fp <- revSecondSplit [i + 3]
      break
    }
  }
  if (exists("fp") && exists("rt") && exists("fo")) {
    assign("FinishingPosition",  c(FinishingPosition, fp), envir = .GlobalEnv)
    assign("Time",  c(Time, rt), envir = .GlobalEnv)
    assign("FinalOdds",  c(FinalOdds, fo), envir = .GlobalEnv)
  }
  
}

handleStartPos <- function(secondSplit,StartingPosition){
  for(l in 3:6) {
    if (nchar(secondSplit[l]) == 1){
      assign("StartingPosition",  c(StartingPosition, secondSplit[l]), envir = .GlobalEnv)
      break
    }
  }
}

handleName <- function(secondSplit,Name){
  if (!(grepl('NULL' ,secondSplit[1]) | grepl("\\d", secondSplit[1]))) {
    nameCol <- filterChar(c(secondSplit[1],secondSplit[2],secondSplit[3],secondSplit[4]))
    assign("Name",  c(Name, nameCol), envir = .GlobalEnv) 
  }
}

handleTrack <- function(trk) {
  trackhead <- tolower(trimws(trk))
  track <- get(trackhead,trackmap)
  return (track)
}

handleDate <- function(x){
  breakPoints <- unlist(gregexpr(pattern = ',', x))
  dat <- trimws(substr(x, breakPoints[1] + 1, nchar(x)))
  
  #logic for friEve
  for (v in ls(racemap)) {
    if (grepl(v , x , ignore.case = TRUE)) {
      dat<- paste(dat,racemap[[v]],sep = '')
    }
  }
  return(dat)
}

handleDistance <- function(x,RaceNumber,Distance,Grade){
  rn <- trimws(substr(x, 1, 4))
  gd <- trimws(substr(x, 12, 13))
  dist <- trimws(substr(x, 24, 27))
  assign("RaceNumber",  c(RaceNumber, rn), envir = .GlobalEnv) 
  assign("Distance",  c(Distance, dist), envir = .GlobalEnv) 
  assign("Grade",  c(Grade, gd), envir = .GlobalEnv) 
}

#function to store racenumber to finalodds
filteredLines <- function(x) {
  
}

#function to store racenumber to finalodds
filteredLinesWithNumber <- function(x) {
  #date
  
}

#RaceMaps
racemap <- new.env(hash=T, parent=emptyenv())
assign('Evening', 'Eve', racemap)
assign('Twilight', 'Twi', racemap)
assign('Afternoon', 'Aft', racemap)
assign('Monday', 'Mon', racemap)
assign('Tuesday', 'Tue', racemap)
assign('Wednesday', 'Wed', racemap)
assign('Thursday', 'Thur', racemap)
assign('Friday', 'Fri', racemap)
assign('Saturday', 'Sat', racemap)
assign('Sunday', 'Sun', racemap)
assign('Evening', 'Eve', racemap)

#Tracks
trackmap <- new.env(hash=T, parent=emptyenv())
assign('caliente', 'CA', trackmap)
assign('daytona', 'Twi', trackmap)
assign('derby Lane', 'DE', trackmap)
assign('Ebro', 'EB', trackmap)
assign('Flagler', 'FL', trackmap)
assign('Iowa', 'IO', trackmap)
assign('orange park', 'OR', trackmap)
assign('Palm Beach', 'PB', trackmap)
assign('Pensacola', 'PE', trackmap)
assign('Sanford-Orlando', 'Sun', trackmap)
assign('southland', 'SO', trackmap)
assign('Tri State', 'TR', trackmap)
assign('Wheeling', 'WH', trackmap)

temp <- c()
Track <- c()
RaceDate <- c()
#Fileds in excel
RaceNumber <-  Distance <-  Grade <-  Name <-  StartingPosition <-
  FinishingPosition <- Time <- FinalOdds <- Win <-  Place <-
  Show <- Quinella <- Perfecta <- Trifecta <- Superfecta <- Winplaceshow <- c()
track = TRUE

# file <-
#   pdf_text('C://abhiimpdata//R//pdfs//SOUTHLAND-Aug24-Thursday-Twilight-Charts.pdf')
# info <-
#   pdf_info('C://abhiimpdata//R//pdfs//SOUTHLAND-Aug24-Thursday-Twilight-Charts.pdf')

file <-
  pdf_text('C://abhiimpdata//R//ORANGE-PARK-Aug18-Friday-Evening-Charts.pdf')
info <-
  pdf_info('C://abhiimpdata//R//ORANGE-PARK-Aug18-Friday-Evening-Charts.pdf')

pages <- info$pages


#loop for multiple pages
for (k in 1:pages) {
  pdfTextFile <- strsplit(file[[k]], "\n")[[1]]
  lines <- strsplit(pdfTextFile, "\n")
  
  #calculating the limit
  if (max(nchar(lines)) > 130) {
    limit <- max(nchar(lines))
  }
  else if (!exists("limit")) {
    limit <- max(nchar(lines)) * 2
  }
  
  #handling track and date
  #Track
  if (track) {
    Track <- handleTrack(lines[1])
    RaceDate <- handleDate(trimws(lines[2]))
    track =!track
  }
 
  
  #processing each line
  for (line in lines[-c(1,2)]) {
    x <- substr(line , 1 , limit / 2)
    if (!is.null(x) && !nchar(trimws(x)) == 0) {
      if (grepl("[A-z]$", trimws(x)) && !grepl('-' , x , ignore.case = TRUE) && !grepl('/' , x , ignore.case = TRUE)) {
        # filter for main criteria
        if (grepl('Distance' , x, ignore.case = TRUE)) {
            handleDistance(x,RaceNumber,Distance,Grade)
        }
        else {
          firstSplit <- strsplit(x[[1]], " ")[[1]]
          secondSplit <- strsplit(firstSplit, " ")
          secondSplit[lapply(secondSplit, length) > 0]
          secondSplit <- Filter(length, secondSplit)
          
          handleName(secondSplit,Name)
          
          handleStartPos(secondSplit,StartingPosition)
          
          handleFoRtFp(rev(secondSplit),FinishingPosition,Time,FinalOdds)
        }
      }
      else {
        filteredLinesWithNumber(x)
      }
    }
  }
}
  
