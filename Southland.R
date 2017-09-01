library(pdftools)
library(rJava)
library(xlsxjars)
library(xlsx)
library(WriteXLS)

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
  assign("RaceNumber",  c(Distance, dist), envir = .GlobalEnv) 
  assign("RaceNumber",  c(Grade, gd), envir = .GlobalEnv) 
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
assign('Orange Park', 'OR', trackmap)
assign('Palm Beach', 'PB', trackmap)
assign('Pensacola', 'PE', trackmap)
assign('Sanford-Orlando', 'Sun', trackmap)
assign('southland', 'SO', trackmap)
assign('Tri State', 'TR', trackmap)
assign('Wheeling', 'WH', trackmap)

temp <- c()
Track <- c()
RaceDate <- c()
RaceNumber <-
  Distance <-
  Grade <- c()
track = TRUE

file <-
  pdf_text('C://abhiimpdata//R//pdfs//SOUTHLAND-Aug24-Thursday-Twilight-Charts.pdf')
info <-
  pdf_info('C://abhiimpdata//R//pdfs//SOUTHLAND-Aug24-Thursday-Twilight-Charts.pdf')
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
      if (grepl("[A-z]$", trimws(x))) {
        # filter for main criteria
        if (grepl('Distance' , x, ignore.case = TRUE)) {
            handleDistance(x,RaceNumber,Distance,Grade)
        }
        else {
          
        }
      }
      else {
        filteredLinesWithNumber(x)
      }
    }
  }
}
  
