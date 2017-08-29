library(pdftools)
file <-
  pdf_text('C://abhiimpdata//R//ORANGE-PARK-Aug18-Friday-Evening-Charts.pdf')
info <-
  pdf_info('C://abhiimpdata//R//ORANGE-PARK-Aug18-Friday-Evening-Charts.pdf')
pages <- xxx$pages
#file <- pdf_text('C://abhiimpdata//R//pdfs//SOUTHLAND-Fri-Twi-8-18-charts.pdf')
racevector <-
  distancevector <-
  gradevector <-
  Name <-
  StartingPosition <-
  FinishingPosition <- Time <- FinalOdds <- Win <-
  Place <-
  Show <- Quinella <- Perfecta <- Trifecta <- Superfecta <- c()

track =TRUE

for (k in 1:pages) {
  pdfTextFile <- strsplit(file[[k]], "\n")[[1]]
  lines <- strsplit(pdfTextFile, "\n")
  
  if (max(nchar(lines)) > 130) {
    limit <- max(nchar(lines))
  }
  else if (!exists("limit")) {
    limit <- max(nchar(lines)) * 2
  }
  
  
  #Track
  if (track) {
   tracktail <- substr(trimws(lines[1]) , unlist(gregexpr(pattern = ' ', trimws(lines[1])))[1]+1,unlist(gregexpr(pattern = ' ', trimws(lines[1])))[1]+1)
   trackhead <- substr(trimws(lines[1]),1,1)
   Track <- paste(trackhead,tracktail, sep = '') 
   track = !track
  }
  
  #loop for the dates
  for (i in 1:2) {
    if (grepl('charts' , lines[i] , ignore.case = TRUE)) {
      breakPoints <- unlist(gregexpr(pattern = ',', lines[i]))
      
      if (length(breakPoints) > 1) {
        RaceDate <- trimws(substr(lines[i], breakPoints[1] + 1, breakPoints[2]))
      }
      else {
        RaceDate <- trimws(substr(lines[i], breakPoints[1] + 1, nchar(lines[i])))
      }
    }
  }
  
  #for everything else
  for (line in lines) {
    x <- substr(line , 1 , limit / 2)
    if (!is.null(x) && !nchar(trimws(x)) == 0) {
      #get the RN , Grade , Distance elements
      if (grepl('Distance' , x , ignore.case = TRUE))
      {
        RaceNumber <- trimws(substr(x, 1, 4))
        Grade <- trimws(substr(x, 12, 13))
        Distance <- trimws(substr(x, 24, 27))
        racevector <- c(racevector, RaceNumber)
        distancevector <- c(distancevector, Distance)
        gradevector <- c(gradevector, Grade)
        
      }
      #same thing as above but for another pdf
      else if (grepl('RACE' , x , ignore.case = TRUE))
      {
        RaceNumber <- trimws(substr(x, 5, 7))
        Grade <- trimws(substr(x, 14, 17))
        Distance <- trimws(substr(x, 18, 22))
        racevector <- c(racevector, RaceNumber)
        distancevector <- c(distancevector, Distance)
        gradevector <- c(gradevector, Grade)
      }
      
      #get the rest of the data
      else if (grepl('Exotics: Quinella' , x , ignore.case = TRUE)) {
        # working with Exotics: Quinella , (1-4): 12.80 Perfecta , (1-4): 42.00 Trifecta , (1-4-5): 111.60 Superfecta , (1-4-5-3): 103.50
        
        
        Quinella <- c(Quinella, trimws(substr(x, 27, 33)))
        Perfecta <- c(Perfecta, trimws(substr(x, 55, 61)))
        #logic to make work for T & S
        myValuesPos <- unlist(gregexpr(pattern = ':', x))
        myValuesPos <- myValuesPos[3:length(myValuesPos)]
        
        S <- unlist(gregexpr(pattern = 'Superfecta', x))
        T <- unlist(gregexpr(pattern = 'Trifecta', x))
        
        if (S != -1) {
          posS <- min(which(myValuesPos > S))
          Superfecta <-
            c(Superfecta, trimws(substr(
              x, myValuesPos[posS] + 1, myValuesPos[posS] + 7
            )))
        }
        if (T != -1) {
          posT <- min(which(myValuesPos > T))
          Trifecta <-
            c(Trifecta, trimws(substr(
              x, myValuesPos[posT] + 1, myValuesPos[posT] + 7
            )))
        }
      }
      # for next line superfecta/Trifecta
      else if (grepl('fecta' , x , ignore.case = TRUE)) {
        myValuesPosition <- unlist(gregexpr(pattern = ':', x))
        S <- unlist(gregexpr(pattern = 'Superfecta', x))
        T <- unlist(gregexpr(pattern = 'Trifecta', x))
        
        if (S != -1) {
          Superfecta <-
            c(Superfecta, trimws(
              substr(x, myValuesPosition[1] + 1, myValuesPosition[1] + 7)
            ))
        }
        if (T != -1) {
          Trifecta <-
            c(Trifecta, trimws(
              substr(x, myValuesPosition[1] + 1, myValuesPosition[1] + 7)
            ))
        }
      }
      
      #ignoring the middle line like Lashmet Kennel, Bd, F, 11/28/15, Barcelona Boss - Lk's Cherokee
      else if (grepl('-' , x , ignore.case = TRUE) |
               grepl('/' , x , ignore.case = TRUE)) {
        
      }
      else {
        firstSplit <- strsplit(x[[1]], " ")[[1]]
        secondSplit <- strsplit(firstSplit, " ")
        secondSplit[lapply(secondSplit, length) > 0]
        secondSplit <- Filter(length, secondSplit)
        if (!(grepl('NULL' ,secondSplit[1]) | grepl("\\d", secondSplit[1]))) {
          nameCol <- paste(secondSplit[1], secondSplit[2])
          Name <- c(Name, nameCol)
        }
        for(l in 4:6) {
          if (nchar(secondSplit[l]) == 1){
          StartingPosition <- c(StartingPosition, secondSplit[l])
          break
          }
        }
        revSecondSplit <- rev(secondSplit)
        
        #loop to get the Finish Position	Time	Final Odds
        # 10 because i have reverse the line and i am extracting from the other side
        # so by 10 i am expecting all 3 of them like 2    2    1  .5  31.45 2.30 Bmpd, 1st To Task
        if (!grepl("." , revSecondSplit[1] , fixed = TRUE)) {
          for (i in 1:10) {
            # this cheks where each element of revSecondSplit contains any character or & or ,
            # if yes it will return true and hence !(negate) is returing false
            #2    1  .5  31.45 2.30 Bmpd, Kept To TasK  ==> these are 10 elemnts of revSecondSplit
            if (!grepl("^[A-Za-z,&1st\r]+$", revSecondSplit[i], perl = T)) {
              fo <- revSecondSplit[i]
              rt <- revSecondSplit[i + 1]
              fp <- revSecondSplit [i + 3]
              break
            }
          }
          if (exists("fp") && exists("rt") && exists("fo")) {
            FinishingPosition <- c(FinishingPosition, fp)
            Time <- c(Time, rt)
            FinalOdds <- c(FinalOdds, fo)
          }
        }
        #logic for win place show
        else {
          for (j in 1:3) {
            if (grepl("." , revSecondSplit[j] , fixed = TRUE)) {
              if (j == 1) {
                Show  <- c(Show, revSecondSplit[j])
              }
              if (j == 3) {
                Win <- c(Win, revSecondSplit[j])
              }
              if (j == 2) {
                Place <- c(Place, revSecondSplit[j])
              }
            }
          }
        }
      }
    }
  }
  
  
  ###############################################################################
  
  
  #Get the remaing part of the pdf if only it exists
  if (max(nchar(lines) > 130)) {
    for (line in lines) {
      # if (!is.null(line) && !nchar(trimws(line)) == 0) {
        x <- substr(line , limit / 2 + 2 , limit)
        if (!is.null(x) && !nchar(trimws(x)) == 0) {
        if (grepl('Distance' , x , ignore.case = TRUE))
        {
          RaceNumber <- trimws(substr(x, 1, 4))
          Grade <- trimws(substr(x, 12, 13))
          Distance <- trimws(substr(x, 24, 27))
          racevector <- c(racevector, RaceNumber)
          distancevector <- c(distancevector, Distance)
          gradevector <- c(gradevector, Grade)
          
        }
        #same thing as above but for another pdf
        else if (grepl('RACE' , x , ignore.case = TRUE))
        {
          RaceNumber <- trimws(substr(x, 5, 7))
          Grade <- trimws(substr(x, 14, 17))
          Distance <- trimws(substr(x, 18, 22))
          racevector <- c(racevector, RaceNumber)
          distancevector <- c(distancevector, Distance)
          gradevector <- c(gradevector, Grade)
        }
        
        #get the rest of the data
        else if (grepl('Exotics: Quinella' , x , ignore.case = TRUE)) {
          # working with Exotics: Quinella , (1-4): 12.80 Perfecta , (1-4): 42.00 Trifecta , (1-4-5): 111.60 Superfecta , (1-4-5-3): 103.50
          
          
          Quinella <- c(Quinella, trimws(substr(x, 27, 33)))
          Perfecta <- c(Perfecta, trimws(substr(x, 55, 61)))
          #logic to make work for T & S
          myValuesPos <- unlist(gregexpr(pattern = ':', x))
          myValuesPos <- myValuesPos[3:length(myValuesPos)]
          
          S <- unlist(gregexpr(pattern = 'Superfecta', x))
          T <- unlist(gregexpr(pattern = 'Trifecta', x))
          
          if (S != -1) {
            posS <- min(which(myValuesPos > S))
            Superfecta <-
              c(Superfecta, trimws(substr(
                x, myValuesPos[posS] + 1, myValuesPos[posS] + 7
              )))
          }
          if (T != -1) {
            posT <- min(which(myValuesPos > T))
            Trifecta <-
              c(Trifecta, trimws(substr(
                x, myValuesPos[posT] + 1, myValuesPos[posT] + 7
              )))
          }
        }
        # for next line superfecta/Trifecta
        else if (grepl('fecta' , x , ignore.case = TRUE)) {
          myValuesPosition <- unlist(gregexpr(pattern = ':', x))
          S <- unlist(gregexpr(pattern = 'Superfecta', x))
          T <- unlist(gregexpr(pattern = 'Trifecta', x))
          
          if (S != -1) {
            Superfecta <-
              c(Superfecta, trimws(
                substr(x, myValuesPosition[1] + 1, myValuesPosition[1] + 7)
              ))
          }
          if (T != -1) {
            Trifecta <-
              c(Trifecta, trimws(
                substr(x, myValuesPosition[1] + 1, myValuesPosition[1] + 7)
              ))
          }
        }
        
        #ignoring the middle line like Lashmet Kennel, Bd, F, 11/28/15, Barcelona Boss - Lk's Cherokee
        else if (grepl('-' , x , ignore.case = TRUE) |
                 grepl('/' , x , ignore.case = TRUE)) {
          
        }
        else {
          firstSplit <- strsplit(x[[1]], " ")[[1]]
          secondSplit <- strsplit(firstSplit, " ")
          secondSplit[lapply(secondSplit, length) > 0]
          secondSplit <- Filter(length, secondSplit)
          if (!(grepl('NULL' ,secondSplit[1]) | grepl("\\d", secondSplit[1]))) {
            nameCol <- paste(secondSplit[1], secondSplit[2])
            Name <- c(Name, nameCol)
          }
          
          for(l in 4:6) {
            if (nchar(secondSplit[l]) == 1){
              StartingPosition <- c(StartingPosition, secondSplit[l])
              break
            }
          }
          revSecondSplit <- rev(secondSplit)
          
          #loop to get the Finish Position	Time	Final Odds
          # 10 because i have reverse the line and i am extracting from the other side
          # so by 10 i am expecting all 3 of them like 2    2    1  .5  31.45 2.30 Bmpd, 1st To Task
          if (!grepl("." , revSecondSplit[1] , fixed = TRUE)) {
            for (i in 1:10) {
              # this cheks where each element of revSecondSplit contains any character or & or ,
              # if yes it will return true and hence !(negate) is returing false
              #2    1  .5  31.45 2.30 Bmpd, Kept To TasK  ==> these are 10 elemnts of revSecondSplit
              if (!grepl("^[A-Za-z,&1st\r]+$", revSecondSplit[i], perl = T)) {
                fo <- revSecondSplit[i]
                rt <- revSecondSplit[i + 1]
                fp <- revSecondSplit [i + 3]
                break
              }
            }
            if (exists("fp") && exists("rt") && exists("fo")) {
              FinishingPosition <- c(FinishingPosition, fp)
              Time <- c(Time, rt)
              FinalOdds <- c(FinalOdds, fo)
            }
          }
          #logic for win place show
          else {
            for (j in 1:3) {
              if (grepl("." , revSecondSplit[j] , fixed = TRUE)) {
                if (j == 1) {
                  Show  <- c(Show, revSecondSplit[j])
                }
                if (j == 3) {
                  Win <- c(Win, revSecondSplit[j])
                }
                if (j == 2) {
                  Place <- c(Place, revSecondSplit[j])
                }
              }
            }
          }
        }
      }
    }
    
  }
  
}



###########################################Filtering out the remaining items
Name <- Name[-c(1, length(Name))]
FinalOdds <- FinalOdds[-1]
FinishingPosition <- FinishingPosition[-1]


############################################Creating Data Frame
#exportDataToExcel <- data.frame(Name,)
