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

fitToExcelGRD <- function(div,RaceNumber,Grade,Distance) {
  h <- 1
  for (r in div[1:length(RaceNumber)]) {
    elemr <- as.list(rep(RaceNumber[r], 7))
    elemg <- as.list(rep(Grade[r], 7))
    elemd <- as.list(rep(Distance[r], 7))
    assign("RaceNumber",   append(RaceNumber,elemr,div[h]), envir = .GlobalEnv)
    assign("Grade",  c(Grade, append(Grade,elemg,div[h])), envir = .GlobalEnv)
    assign("Distance",  c(Distance, append(Distance,elemd,div[h])), envir = .GlobalEnv)
    h <- h+1
  }
}

fitToExcelWSP <- function(Name,wps,Win,Place,Show){
  for ( nm in Name) {
    check = TRUE
    for (m in ls(wps)) {
      if (grepl(nm, m , fixed = TRUE)) {
        assign("Win",   wps[[m]][1], envir = .GlobalEnv)
        assign("Place", wps[[m]][2], envir = .GlobalEnv)
        assign("Show",  wps[[m]][3], envir = .GlobalEnv)
        check = FALSE
        break
      }
    }
    if (check){
      assign("Win",   ' ', envir = .GlobalEnv)
      assign("Place", ' ', envir = .GlobalEnv)
      assign("Show",  ' ', envir = .GlobalEnv)
    }
  }
}

handleQui <- function(x,flag,Trifecta,Superfecta){
  # assign("Quinella",  c(Quinella, trimws(substr(x, 27, 33))), envir = .GlobalEnv)
  # assign("Perfecta",  c(Perfecta, trimws(substr(x, 55, 61))), envir = .GlobalEnv)

  #logic to make work for T & S
  myValuesPos <- unlist(gregexpr(pattern = ':', x))
  if (flag){
     myValuesPos <- myValuesPos[3:length(myValuesPos)]
  }
  S <- unlist(gregexpr(pattern = 'Superfecta', x))
  T <- unlist(gregexpr(pattern = 'Trifecta', x))
  
  if (S != -1) {
    posS <- min(which(myValuesPos > S))
    assign("Superfecta",  c(Superfecta, trimws(substr(x, myValuesPos[posS] + 1, myValuesPos[posS] + 7))), envir = .GlobalEnv)

  }
  if (T != -1) {
    posT <- min(which(myValuesPos > T))
    assign("Trifecta",  c(Trifecta, trimws(substr(x, myValuesPos[posT] + 1, myValuesPos[posT] + 7))), envir = .GlobalEnv)
  }
}

handleSplit <- function(x){
  firstSplit <- strsplit(x[[1]], " ")[[1]]
  secondSplit <- strsplit(firstSplit, " ")
  secondSplit[lapply(secondSplit, length) > 0]
  secondSplit <- Filter(length, secondSplit)
  return (secondSplit)
}
handleWPS <- function(revSecondSplit,Win,Place,Show){
  myflag = TRUE
  for (j in 1:3) {
    if (j == 1) {
      #fixed is true as . is itself a pattern in R
      if (grepl("." , revSecondSplit[j] , fixed = TRUE)) {
        show  <-  revSecondSplit[j]
      }
      else {
        show  <- ' '
        winperson <- filterChar(rev(revSecondSplit))
        myflag = FALSE
      }
    }
    if (j == 3) {
      if (grepl("." , revSecondSplit[j] , fixed = TRUE)) {
        win <- revSecondSplit[j]
        winperson <- filterChar(rev(revSecondSplit))
      }
      else {
        win  <- ' '
        if (myflag) {
          winperson <- filterChar(rev(revSecondSplit))
        }
      }
    }
    if (j == 2) {
      if (grepl("." , revSecondSplit[j] , fixed = TRUE)) {
        place <- revSecondSplit[j]
      }
      else {
        place  <- ' '
        if (myflag) {
          winperson <- filterChar(rev(revSecondSplit))
        }
        myflag = FALSE
      }
    }
  }
  Winplaceshow <-  c(win,place,show)
  # wps$winperson <- Winplaceshow
  assign(winperson, Winplaceshow , wps, inherits = FALSE)
  
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
  dat <- gsub("/",".",dat)
  #logic for friEve
  for (v in ls(racemap)) {
    if (grepl(v , x , ignore.case = TRUE)) {
      dat<- paste(dat,racemap[[v]],sep = '')
      #dat <- append(dat,racemap[[v]],1)
    }
  }
  for (v in ls(racemaptwo)) {
    if (grepl(v , x , ignore.case = TRUE)) {
      dat<- paste(dat,racemaptwo[[v]],sep = '')
    }
  }
  return(dat)
}

handleDistance <- function(x,RaceNumber,Distance,Grade){
  rn <- trimws(substr(x, 1, 4))
  rn <- matches <- regmatches(rn, gregexpr("[[:digit:]]+", rn))
  bb <- unlist(gregexpr(pattern = 'Grade', x))
  gd <- substr(x,bb[1]+nchar("Grade") +1,bb[1]+nchar("Grade")+3)
 # gd <- trimws(substr(x, 12, 13))
  gd <- regmatches(gd, gregexpr("[A-z]+", gd))
  dist <- trimws(substr(x, 24, 29))
  dist <- regmatches(dist, gregexpr("[[:digit:]]+", dist))
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
assign('Monday', 'Mon', racemap)
assign('Tuesday', 'Tue', racemap)
assign('Wednesday', 'Wed', racemap)
assign('Thursday', 'Thur', racemap)
assign('Friday', 'Fri', racemap)
assign('Saturday', 'Sat', racemap)
assign('Sunday', 'Sun', racemap)

#racemap <- new.env(hash=T, parent=emptyenv())
racemaptwo <- new.env(hash=T, parent=emptyenv())
assign('Evening', 'Eve', racemaptwo)
assign('Twilight', 'Twi', racemaptwo)
assign('Afternoon', 'Aft', racemaptwo)



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

#WPSMaps
wps <- new.env(hash=T)

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
  pdf_text('C://abhiimpdata//R//ORANGE-PARK-Aug23-Wednesday-Evening-Charts.pdf')
info <-
  pdf_info('C://abhiimpdata//R//ORANGE-PARK-Aug23-Wednesday-Evening-Charts.pdf')

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
 
  if (k == 1) {
    lines <- lines[-c(1,2)]
  }
  
  
  #processing each line
  for (line in lines) {
    x <- substr(line , 1 , (limit/2 - 1))
    if (!is.null(x) && !nchar(trimws(x)) == 0) {
      #end would be string
      if (grepl("[A-z.]$", trimws(x))) {
        if (!grepl('/' , x , ignore.case = TRUE)) {
        # filter for main criteria
          if (grepl('Distance' , x, ignore.case = TRUE)) {
              handleDistance(x,RaceNumber,Distance,Grade)

            if(length(RaceNumber) - length(Trifecta) >= 2 ) {
              Trifecta <- c(Trifecta,' ')
            }
            if(length(RaceNumber) - length(Superfecta) >= 2  ) {
              Superfecta <- c(Superfecta,' ')
            }
          }
          else {
            if (!grepl(':' , x , ignore.case = TRUE)) {
            secondSplit <- handleSplit(x)

            handleName(secondSplit,Name)

            handleStartPos(secondSplit,StartingPosition)

            handleFoRtFp(rev(secondSplit),FinishingPosition,Time,FinalOdds)

            }
          }
        }
      }
      else if (!grepl('Quinella' , x , ignore.case = TRUE) && !grepl('fecta' , x , ignore.case = TRUE)) {
        secondSplit <- handleSplit(x)
        handleWPS(rev(secondSplit),Win,Place,Show)
      }
      else {
          if (grepl('Exotics: Quinella' , x , ignore.case = TRUE)){
            bb <- unlist(gregexpr(pattern = 'Exotics: Quinella', x))
            val<- substr(x,nchar("Exotics: Quinella") +10,nchar("Exotics: Quinella")+16)
            Quinella <- c(Quinella, trimws(val))
          }
        if (grepl('Perfecta' , x , fixed = TRUE)){
            bb <- unlist(gregexpr(pattern = 'Perfecta', x))
            val<- substr(x,bb[1]+nchar("Perfecta") +10,bb[1]+nchar("Perfecta")+16)
            Perfecta <- c(Perfecta, trimws(val))
        }
        if (grepl('Trifecta' , x , ignore.case = TRUE) && !grepl('Twin Trifecta' , x , ignore.case = TRUE)){
            bb <- unlist(gregexpr(pattern = 'Trifecta', x))
            val <- substr(x,bb[1]+nchar("Trifecta") +12,bb[1]+nchar("Trifecta")+18)
            Trifecta <- c(Trifecta, trimws(val))
        }
        if (grepl('Superfecta' , x , ignore.case = TRUE)){
          bb <- unlist(gregexpr(pattern = 'Superfecta', x))
          if (bb == 1) {
            val<- substr(x,nchar("Superfecta") + 14,nchar("Superfecta") + 20)
          }
          else {
          val <- substr(x, bb[1] + nchar("Superfecta") + 14, bb[1] + nchar("Superfecta") + 20)
          }
          Superfecta <- c(Superfecta, trimws(val))
        }

        }
      }
    }
  
  # 
  # #Get the remaing part of the pdf if only it exists
  if (max(nchar(lines) > 130)) {
    for (line in lines) {
      x <- substr(line , limit / 2 , limit)
      if (unlist(gregexpr("[a-z]", substr(x,1,1))) != -1){
      x <- substr(x,2,limit)
      }
      if (!is.null(x) && !nchar(trimws(x)) == 0) {
        if (grepl("[A-z.]$", trimws(x))) {
          if (!grepl('/' , x , ignore.case = TRUE)) {
          # filter for main criteria
          if (grepl('Distance' , x, ignore.case = TRUE)) {
            handleDistance(x,RaceNumber,Distance,Grade)

            if(length(RaceNumber) - length(Trifecta) >= 2 ) {
              Trifecta <- c(Trifecta,' ')
            }
            if(length(RaceNumber) - length(Superfecta) >= 2 ) {
              Superfecta <- c(Superfecta,' ')
            }
          }
          else {
            if (!grepl(':' , x , ignore.case = TRUE)) {
            secondSplit <- handleSplit(x)

            handleName(secondSplit,Name)

            handleStartPos(secondSplit,StartingPosition)

            handleFoRtFp(rev(secondSplit),FinishingPosition,Time,FinalOdds)
            }
          }
          }
        }
        else if (!grepl('Quinella' , x , ignore.case = TRUE) && !grepl('fecta' , x , ignore.case = TRUE)) {
          secondSplit <- handleSplit(x)
          handleWPS(rev(secondSplit),Win,Place,Show)
        }
        else {
          if (grepl('Exotics: Quinella' , x , ignore.case = TRUE)){
            bb <- unlist(gregexpr(pattern = 'Exotics: Quinella', x))
            val<- substr(x,nchar("Exotics: Quinella") +10,nchar("Exotics: Quinella")+16)
            Quinella <- c(Quinella, trimws(val))
          }
          if (grepl('Perfecta' , x , fixed = TRUE)){
            bb <- unlist(gregexpr(pattern = 'Perfecta', x))
            val<- substr(x,bb[1]+nchar("Perfecta") +10,bb[1]+nchar("Perfecta")+16)
            Perfecta <- c(Perfecta, trimws(val))
          }
          if (grepl('Trifecta' , x , ignore.case = TRUE) && !grepl('Twin Trifecta' , x , ignore.case = TRUE)){
            bb <- unlist(gregexpr(pattern = 'Trifecta', x))
            val <- substr(x,bb[1]+nchar("Trifecta") +12,bb[1]+nchar("Trifecta")+18)
            Trifecta <- c(Trifecta, trimws(val))
          }
          if (grepl('Superfecta' , x , ignore.case = TRUE)){
            bb <- unlist(gregexpr(pattern = 'Superfecta', x))
            if (bb == 1) {
              val<- substr(x,nchar("Superfecta") + 14,nchar("Superfecta") + 20)
            }
            else {
              val <- substr(x, bb[1] + nchar("Superfecta") + 14, bb[1] + nchar("Superfecta") + 20)
            }
            Superfecta <- c(Superfecta, trimws(val))
          }

        }
      }
    }
  }
}

#new code to format conversion 

RaceNumber <- lapply(RaceNumber ,as.numeric)
Distance <- lapply(Distance ,as.numeric)

Time <- lapply(Time ,as.numeric)
Win <- lapply(Win ,as.numeric)
StartingPosition <- lapply(StartingPosition ,as.numeric)
Place <- lapply(Place ,as.numeric)
Show <- lapply(Show ,as.numeric)
#ncode ends

#Refactoring Grade , RaceNumber , Distance 
div <- c(1,9,17,25,33,41,49,57,65,73,81,89,97,105,113,121,129,137,145,153)

h <- 1
for (r in div[1:length(RaceNumber)]) {
  elemr <- as.list(rep(RaceNumber[r], 7))
  elemg <- as.list(rep(Grade[r], 7))
  elemd <- as.list(rep(Distance[r], 7))
  RaceNumber <- append(RaceNumber,elemr,div[h])
  Grade <- append(Grade,elemg,div[h])
  Distance <- append(Distance,elemd,div[h])
  h <- h+1
}


for (r in 1 : length(Quinella)) {
  elem <- as.list(rep('', 7))
  Quinella <- append(Quinella,elem,div[r])
  Perfecta <- append(Perfecta,elem,div[r])
  Trifecta <- append(Trifecta,elem,div[r])
  Superfecta <- append(Superfecta,elem,div[r])
}

# handling win place show to fit exactly at same place as required
for ( nm in Name) {
  check = TRUE
  for (m in ls(wps)) {
    if (grepl(nm, m , fixed = TRUE)) {
      Win <- c(Win ,wps[[m]][1])
      Place <- c(Place ,wps[[m]][2])
      Show <- c(Show ,wps[[m]][3])
      check = FALSE
      break
    }
  }
  if (check){
    Win <- c(Win ,'')
    Place <- c(Place ,'')
    Show <- c(Show , '')
  }
}

#only for ORANGE-PARK-Evening-Charts
if(length(Trifecta) > length(Name)){
  Trifecta <- Trifecta[-length(Name)]
}
if(length(Superfecta) > length(Name)){
  Superfecta <- Superfecta[-length(Name)]
}

Quinella <- c(do.call("cbind",Quinella))
Perfecta <- c(do.call("cbind",Perfecta))
Trifecta <- c(do.call("cbind",Trifecta))
Superfecta <- c(do.call("cbind",Superfecta))
RaceNumber <- c(do.call("cbind",RaceNumber))
Grade <- c(do.call("cbind",Grade))
StartingPosition <- c(do.call("cbind",StartingPosition))
FinishingPosition <- c(do.call("cbind",FinishingPosition))
Distance <- c(do.call("cbind",Distance))
Time <- c(do.call("cbind",Time))
Win <- c(do.call("cbind",Win))
Place <- c(do.call("cbind",Place))
Show <- c(do.call("cbind",Show))

#Unique Identifier
UniqueIdentifier <- paste(RaceDate,"_",Track,"_",RaceNumber,"_",Grade,"_",StartingPosition, sep = '')

#####Exporting to excel
exportDataToExcel <- data.frame(Name,UniqueIdentifier,RaceDate,Track,RaceNumber,Grade,StartingPosition,FinishingPosition,Distance,Time,Win,Place,Show,Quinella,Perfecta,Trifecta,Superfecta, check.rows= FALSE)
write.xlsx(exportDataToExcel,"D:/dummy3.xlsx",sheetName = "Newdata1")

