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
      if (revSecondSplit [i + 3] != 'NULL'){
      fp <- revSecondSplit [i + 3]
      }
      else {
      fp  <- ' '
      }
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
  if (!(grepl('NULL' ,secondSplit[1]))) {
    nameCol <- filterChar(c(secondSplit[1],secondSplit[2],secondSplit[3],secondSplit[4]))
    assign("Name",  c(Name, nameCol), envir = .GlobalEnv) 
  }
}

handleTrackSO <- function(trk) {
  breakPoints <- unlist(gregexpr(pattern = ':', trk))
  trk <- substr(trk,1,breakPoints[1]-1)
  trackhead <- tolower(trimws(trk))
  track <- get(trackhead,trackmap)
  return (track)
}

handleDateSO <- function(x){
  breakPoints <- unlist(gregexpr(pattern = ',', x))
  dat <- trimws(substr(x, breakPoints[1] + 1, breakPoints[2]))
  breakPoints1 <- unlist(gregexpr(pattern = ' ', dat))
  dat1 <- trimws(substr(dat, 1, breakPoints1[1]))
  dat2 <-  get(tolower(dat1),calmap)
  newdat <- paste(dat2, trimws(substr(dat, breakPoints1[1]+1, breakPoints1[1]+2)) ,sep ='/')
  newdat <- paste(newdat,'17',sep ='/')
  for (v in ls(racemap)) {
    if (grepl(v , x , ignore.case = TRUE)) {
      newdat<- paste(newdat,racemap[[v]],sep = '')
      #dat <- append(dat,racemap[[v]],1)
    }
  }
  return(newdat)
}

handleDistanceSO <- function(x,RaceNumber,Distance,Grade){
  rn <- trimws(substr(x, 5, 8))
  rn <- regmatches(rn, gregexpr("[[:digit:]]+", rn))
  gd <- trimws(substr(x, 14, 16))
  gd <- regmatches(gd, gregexpr("[A-z]+", gd))
  dist <- trimws(substr(x, 15, 20))
  dist <- regmatches(dist, gregexpr("[[:digit:]]+", dist))
  assign("RaceNumber",  c(RaceNumber, rn), envir = .GlobalEnv) 
  assign("Distance",  c(Distance, dist), envir = .GlobalEnv) 
  assign("Grade",  c(Grade, gd), envir = .GlobalEnv) 
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
assign('Evening', 'Eve', racemap)
assign('Twilight', 'Twi', racemap)
assign('Afternoon', 'Aft', racemap)


#Tracks
trackmap <- new.env(hash=T, parent=emptyenv())
assign('caliente', 'CA', trackmap)
assign('daytona', 'Twi', trackmap)
assign('derby Lane', 'DE', trackmap)
assign('ebro', 'EB', trackmap)
assign('flagler', 'FL', trackmap)
assign('iowa', 'IO', trackmap)
assign('orange park', 'OR', trackmap)
assign('palm beach', 'PB', trackmap)
assign('pensacola', 'PE', trackmap)
assign('sanford-orlando', 'Sun', trackmap)
assign('southland', 'SO', trackmap)
assign('tri state', 'TR', trackmap)
assign('wheeling', 'WH', trackmap)

#Cal
calmap <- new.env(hash=T, parent=emptyenv())
assign('january', '1', calmap)
assign('february', '2', calmap)
assign('march', '3', calmap)
assign('april', '4', calmap)
assign('may', '5', calmap)
assign('june', '6', calmap)
assign('july', '7', calmap)
assign('august', '8', calmap)
assign('september', '9', calmap)
assign('october', '10', calmap)
assign('november', '11', calmap)
assign('december', '12', calmap)

#WPSMaps
wps <- new.env(hash=T)

temp <- c()
Track <- c()
RaceDate <- c()
#Fileds in excel
RaceNumber <-  Distance <-  Grade <-  Name <-  StartingPosition <-
  FinishingPosition <- Time <- FinalOdds <- Win <-  Place <-
  Show <- Quinella <- Exacta <- Trifecta <- DimeSuper <- Winplaceshow <- c()
track = TRUE
# 
# file <-
#   pdf_text('C://abhiimpdata//R//pdfs//SOUTHLAND-Aug18-Friday-Evening-Charts.pdf')
# info <-
#   pdf_info('C://abhiimpdata//R//pdfs//SOUTHLAND-Aug18-Friday-Evening-Charts.pdf')

file <-
  pdf_text('C://abhiimpdata//R//pdfs//SOUTHLAND-Fri-Twi-8-18-charts.pdf')
info <-
  pdf_info('C://abhiimpdata//R//pdfs//SOUTHLAND-Fri-Twi-8-18-charts.pdf')

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
    Track <- "SO"
    RaceDate <- handleDateSO(trimws(lines[1]))
    track =!track
  }
  
  if (k == 1) {
    lines <- lines[-c(1)]
  }
  
  for (line in lines) {
    x <- substr(line , 1 , limit / 2)
    if (!is.null(x) && !nchar(trimws(x)) == 0 && substr(x,1,1) != '$') {
      
      if (grepl("[A-z]$", trimws(x))) {
        
        if (!grepl('/' , x , ignore.case = TRUE))
        {
          # filter for main criteria
          secondSplit <- handleSplit(x)
          handleName(secondSplit,Name)
          handleStartPos(secondSplit,StartingPosition)
          handleFoRtFp(rev(secondSplit),FinishingPosition,Time,FinalOdds)
        }
        
      }
      else if (grepl('RACE' , x, ignore.case = TRUE)) {
        handleDistanceSO(x,RaceNumber,Distance,Grade)
        temp <- c(temp,length(Name))
        if (length(RaceNumber) - length(Quinella) >= 2 ) {
          Quinella <- c(Quinella,' ')
        }
        if(length(RaceNumber) - length(Exacta) >= 2 ) {
          Exacta <- c(Exacta,' ')
        }
        if(length(RaceNumber) - length(Trifecta) >= 2 ) {
          Trifecta <- c(Trifecta,' ')
        }
        if(length(RaceNumber) - length(DimeSuper) >= 2 ) {
          DimeSuper <- c(DimeSuper,' ')
        }
      }
      else if (grepl('Quiniela ' , x , ignore.case = TRUE) | grepl('Q1' , x) ){
        #Q <- c(Q, trimws(substr(x, length(x), )))
        z <- substr(x,1,which(strsplit(x, "")[[1]]=="$")-1)
        secondSplit <- handleSplit(z)
        handleWPS(rev(secondSplit),Win,Place,Show)
        y <- substr(x,which(strsplit(x, "")[[1]]=="$"),nchar(x))
        thirdSplit <- handleSplit(y)
        Quinella <- c(Quinella, rev(thirdSplit)[1])
      }
      else if (grepl('Exacta ' , x , ignore.case = TRUE)) {
        z <- substr(x,1,which(strsplit(x, "")[[1]]=="$")-1)
        secondSplit <- handleSplit(z)
        handleWPS(rev(secondSplit),Win,Place,Show)
        y <- substr(x,which(strsplit(x, "")[[1]]=="$"),nchar(x))
        thirdSplit <- handleSplit(y)
        Exacta <- c(Exacta, rev(thirdSplit)[1])
      }
      else if (grepl('Trifecta ' , x , ignore.case = TRUE)) {
        z <- substr(x,1,which(strsplit(x, "")[[1]]=="$")-1)
        secondSplit <- handleSplit(z)
        handleWPS(rev(secondSplit),Win,Place,Show)
        y <- substr(x,which(strsplit(x, "")[[1]]=="$"),nchar(x))
        thirdSplit <- handleSplit(y)
        Trifecta <- c(Trifecta, rev(thirdSplit)[1])
      }
      else if (grepl('DimeSuper ' , x , ignore.case = TRUE)) {
        if(which(strsplit(x, " ")[[1]]=="DimeSuper") == 1)  {
          myValuesPos <- unlist(gregexpr(pattern = ',', x))
          ds <- trimws(substr(x,myValuesPos[1]+1 ,myValuesPos[1]+7))
          DimeSuper <- c(DimeSuper, ds)
        }
        else {
          z <- substr(x,1,which(strsplit(x, " ")[[1]]=="DimeSuper")-1)
          secondSplit <- handleSplit(z)
          handleWPS(rev(secondSplit),Win,Place,Show)
        }
      }
      
    }
    
  }
  
  if (max(nchar(lines) > 130)) {
    #processing each line
    for (line in lines) {
      x <- substr(line , limit / 2 + 2 , limit)
      if (!is.null(x) && !nchar(trimws(x)) == 0 && substr(x,1,1) != '$') {
        if (grepl("[A-z]$", trimws(x))) {
          if (!grepl('/' , x , ignore.case = TRUE)) {
            # filter for main criteria
            secondSplit <- handleSplit(x)
            handleName(secondSplit,Name)
            handleStartPos(secondSplit,StartingPosition)
            handleFoRtFp(rev(secondSplit),FinishingPosition,Time,FinalOdds)
          }
        }
        else if (grepl('RACE' , x, ignore.case = TRUE)) {
          handleDistanceSO(x,RaceNumber,Distance,Grade)
          temp <- c(temp,length(Name))
          if (length(RaceNumber) - length(Quinella) >= 2 ) {
            Quinella <- c(Quinella,' ')
          }
          if(length(RaceNumber) - length(Exacta) >= 2 ) {
            Exacta <- c(Exacta,' ')
          }
          if(length(RaceNumber) - length(Trifecta) >= 2 ) {
            Trifecta <- c(Trifecta,' ')
          }
          if(length(RaceNumber) - length(DimeSuper) >= 2 ) {
            DimeSuper <- c(DimeSuper,' ')
          }
        }
        else if (grepl('Quiniela ' , x , ignore.case = TRUE) | grepl('Q1' , x) ){
          #Q <- c(Q, trimws(substr(x, length(x), )))
          z <- substr(x,1,which(strsplit(x, "")[[1]]=="$")-1)
          secondSplit <- handleSplit(z)
          handleWPS(rev(secondSplit),Win,Place,Show)
          y <- substr(x,which(strsplit(x, "")[[1]]=="$"),nchar(x))
          thirdSplit <- handleSplit(y)
          Quinella <- c(Quinella, rev(thirdSplit)[1])
        }
        else if (grepl('Exacta ' , x , ignore.case = TRUE)) {
          z <- substr(x,1,which(strsplit(x, "")[[1]]=="$")-1)
          secondSplit <- handleSplit(z)
          handleWPS(rev(secondSplit),Win,Place,Show)
          y <- substr(x,which(strsplit(x, "")[[1]]=="$"),nchar(x))
          thirdSplit <- handleSplit(y)
          Exacta <- c(Exacta, rev(thirdSplit)[1])
        }
        else if (grepl('Trifecta ' , x , ignore.case = TRUE)) {
          z <- substr(x,1,which(strsplit(x, "")[[1]]=="$")-1)
          secondSplit <- handleSplit(z)
          handleWPS(rev(secondSplit),Win,Place,Show)
          y <- substr(x,which(strsplit(x, "")[[1]]=="$"),nchar(x))
          thirdSplit <- handleSplit(y)
          Trifecta <- c(Trifecta, rev(thirdSplit)[1])
        }
        else if (grepl('DimeSuper ' , x , ignore.case = TRUE)) {
          if(which(strsplit(x, " ")[[1]]=="DimeSuper") == 1)  {
            myValuesPos <- unlist(gregexpr(pattern = ',', x))
            ds <- trimws(substr(x,myValuesPos[1]+1 ,myValuesPos[1]+7))
            DimeSuper <- c(DimeSuper, ds)
          }
          else {
            z <- substr(x,1,which(strsplit(x, " ")[[1]]=="DimeSuper")-1)
            secondSplit <- handleSplit(z)
            handleWPS(rev(secondSplit),Win,Place,Show)
          }
        }
      }
    }
    
  }
  
}


tempry <- diff(temp)
tempry <- c(tempry, length(Name) - sum(tempry))

for (r in 1 : length(tempry)) {

  elemr <- as.list(rep(RaceNumber[temp[r]+1], tempry[r]-1))
  elemg <- as.list(rep(Grade[temp[r]+1], tempry[r]-1))
  elemd <- as.list(rep(Distance[temp[r]+1], tempry[r]-1))
  
  RaceNumber <- append(RaceNumber,elemr,temp[r]+1)
  Grade <- append(Grade,elemg,temp[r]+1)
  Distance <- append(Distance,elemd,temp[r]+1)
  
}

for (r in 1 : length(tempry)) {
  elem  <-  as.list(rep(' ', tempry[r]-1))
  Quinella <- append(Quinella,elem,temp[r]+1)
  Exacta <- append(Exacta,elem,temp[r]+1)
  Trifecta <- append(Trifecta,elem,temp[r]+1)
  DimeSuper <- append(DimeSuper,elem,temp[r]+1)
}

#only for SOUTHLAND-Aug18-Friday-Evening-Charts
if(length(Quinella) > length(Name)){
  Quinella <- Quinella[-length(Name)]
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


Quinella <- c(do.call("cbind",Quinella))
Exacta <- c(do.call("cbind",Exacta))
Trifecta <- c(do.call("cbind",Trifecta))
DimeSuper <- c(do.call("cbind",DimeSuper))
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
exportDataToExcel <- data.frame(Name,UniqueIdentifier,RaceDate,Track,RaceNumber,Grade,StartingPosition,FinishingPosition,Distance,Time,Win,Place,Show,Quinella,Exacta,Trifecta,DimeSuper, check.rows= FALSE)
write.xlsx(exportDataToExcel,"D:/dummy2.xlsx",sheetName = "Newdata2")