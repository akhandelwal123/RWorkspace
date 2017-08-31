library(pdftools)
library(rJava)
library(xlsxjars)
library(xlsx)
library(WriteXLS)
file <-
  pdf_text('C://abhiimpdata//R//ORANGE-PARK-Aug18-Friday-Evening-Charts.pdf')
info <-
  pdf_info('C://abhiimpdata//R//ORANGE-PARK-Aug18-Friday-Evening-Charts.pdf')
pages <- info$pages
#file <- pdf_text('C://abhiimpdata//R//pdfs//SOUTHLAND-Fri-Twi-8-18-charts.pdf')

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

#WPSMaps
wps <- new.env(hash=T)


#Fileds in excel
RaceNumber <-
  Distance <-
  Grade <-
  Name <-
  StartingPosition <-
  FinishingPosition <- Time <- FinalOdds <- Win <-
  Place <-
  Show <- Quinella <- Perfecta <- Trifecta <- Superfecta <- Winplaceshow <- c()

track =TRUE

# function to filter the characters 
filterChar <- function(c){
  y <- unique(na.omit(unlist(strsplit(unlist(c), "[^a-zA-Z's]+"))))
  z <- paste(y, collapse = ' ')
  return (trimws(z))
}


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
      #SL
      if (length(breakPoints) > 1) {
        RaceDate <- trimws(substr(lines[i], breakPoints[1] + 1, breakPoints[2]))
      }
      #OP
      else {
        RaceDate <- trimws(substr(lines[i], breakPoints[1] + 1, nchar(lines[i])))
      }
      #logic for friEve
      for (v in ls(racemap)) {
        if (grepl(v , lines[i] , ignore.case = TRUE)) {
          RaceDate <- paste(RaceDate,racemap[[v]],sep = '')
        }
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
        rn <- trimws(substr(x, 1, 4))
        gd <- trimws(substr(x, 12, 13))
        dist <- trimws(substr(x, 24, 27))
        RaceNumber <- c(RaceNumber, rn)
        Distance <- c(Distance, dist)
        Grade <- c(Grade, gd)
        
      }
      #same thing as above but for another pdf
      else if (grepl('RACE' , x , ignore.case = TRUE))
      {
        rn <- trimws(substr(x, 5, 7))
        gd <- trimws(substr(x, 14, 17))
        dist <- trimws(substr(x, 18, 22))
        RaceNumber <- c(RaceNumber, rn)
        Distance <- c(Distance, dist)
        Grade <- c(Grade, gd)
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
          nameCol <- filterChar(c(secondSplit[1],secondSplit[2],secondSplit[3],secondSplit[4]))
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
          myflag = TRUE
          for (j in 1:3) {
            if (j == 1) {
              if (grepl("." , revSecondSplit[j] , fixed = TRUE)) {
                show  <-  revSecondSplit[j]
              }
              else {
                show  <- ' '
                winperson <- filterChar(revSecondSplit)
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
          rn <- trimws(substr(x, 1, 4))
          gd <- trimws(substr(x, 12, 13))
          dist <- trimws(substr(x, 24, 27))
          RaceNumber <- c(RaceNumber, rn)
          Distance <- c(Distance, dist)
          Grade <- c(Grade, gd)
          
        }
        #same thing as above but for another pdf
        else if (grepl('RACE' , x , ignore.case = TRUE))
        {
          rn <- trimws(substr(x, 5, 7))
          gd <- trimws(substr(x, 14, 17))
          dist <- trimws(substr(x, 18, 22))
          RaceNumber <- c(RaceNumber, rn)
          Distance <- c(Distance, dist)
          Grade <- c(Grade, gd)
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
            nameCol <- filterChar(c(secondSplit[1],secondSplit[2],secondSplit[3],secondSplit[4]))
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
        }
      }
    }
    
  }
  
}



###########################################Filtering out the remaining items
Name <- Name[-c(1, length(Name))]
FinalOdds <- FinalOdds[-1]
FinishingPosition <- FinishingPosition[-1]
Time <- Time[-1]


#Refactoring Grade , RaceNumber , Distance 
div <- c(1,9,17,25,33,41,49,57,64,72,80,88,96,104,112)
h <- 1
for (r in div) {
  elemr <- as.list(rep(RaceNumber[r], 7))
  elemg <- as.list(rep(Grade[r], 7))
  elemd <- as.list(rep(Distance[r], 7))
  RaceNumber <- append(RaceNumber,elemr,div[h])
  Grade <- append(Grade,elemg,div[h])
  Distance <- append(Distance,elemd,div[h])
  h <- h+1
}
# h <- 1
# for (r in 1 : length(Grade)) {
#   elem <- as.list(rep(Grade[r], 7))
#   Grade <- append(Grade,elem,div[r])
# }
# h <- 1
# for (r in 1 : length(Distance)) {
#   elem <- as.list(rep(Distance[r], 7))
#   Distance <- append(Distance,elem,div[r])
# }
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

##converting list into data frames
mylist <- c(Quinella,Perfecta,Trifecta,Superfecta,RaceNumber,Track,Grade,StartingPosition,FinishingPosition,Distance,Time,Win,Place,Show)


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

############################################Creating Data Frame
exportDataToExcel <- data.frame(Name,UniqueIdentifier,RaceDate,Track,RaceNumber,Grade,StartingPosition,FinishingPosition,Distance,Time,Win,Place,Show,Quinella,Perfecta,Trifecta,Superfecta)
# write(exportDataToExcel, file = 'C:\\abhiimpdata\\R\\excel\\test.xlsx')
#WriteXLS(exportDataToExcel , ExcelFileName = 'C:\\abhiimpdata\\R\\excel\\test.xlsx')
write.xlsx(exportDataToExcel,"D:/dummy.xlsx",sheetName = "Newdata")
#exportDataToExcel <- data.frame(Name,RaceDate,Track,Perfecta)
head(exportDataToExcel)