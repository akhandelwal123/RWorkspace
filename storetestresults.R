
#4. 
# x <- "Lk's Marcella    67.5 1 1 2    2    1  .5  31.45 2.30 Bmpd, 1st To Task "
# z <- strsplit(x[[1]], " ")[[1]]
# y <- strsplit(z, " ")
# y[lapply(y,length)>0] 
# y <- Filter(length, y)
# paste(y[1],y[2])
# 
# print(!grepl("^[A-Za-z,&,1st]+$", y[12], perl = T))
# flag = TRUE
# 
# h <- rev(y)
# for (i in 1 : 10) {
#   if (!grepl("^[A-Za-z,&1st]+$", h[i], perl = T)) {
#     a <- h[i]
#     b <- h[i+1]
#     c <- h [i+3]
#     break
#   }
#   
# }
# a
# b
# c

# 3.
# x <- c()
#  for (i in 1:5) {
#    
#    x <- c(x, i)
#  }
# 
# for (i in 6:7) {
#   
#   x <- c(x, i)
# }
# x

# 1. Test for date string

# x <- " Charts for Friday Evening, 08/18/17"
#  grepl('hel' , x , ignore.case = TRUE )
#   print ("hello")
# 
#  y <- unlist(gregexpr(pattern =',', x))
#  typeof(y[1])
# 
#  nchar(x)
# trimws(substr(x,y+1,nchar(x)))

#2.test for grade data
# x <- "RACE 1  Grade A   334 Yards   F - 17.57"
# x <- "1st Grade: M Distance:  550 Condition: Fast"

# if (grepl('RACE' , x , ignore.case = TRUE )) {
#   RaceNumber <- trimws(substr(x,5,7))
#   Grade <- trimws(substr(x,14,17))
#   Distance <- trimws(substr(x,18,22))
# }
# 
# print (RaceNumber)
# print( Grade)
# print(Distance )



