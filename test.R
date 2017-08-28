#4. 
x <- "Lk's Marcella    67.5 1 1 2    2    1  .5  31.45 2.30 Bmpd, Kept To Task "
 z <- strsplit(x[[1]], " ")[[1]]
 y <- strsplit(z, " ")
 y[lapply(y,length)>0] 
 y <- Filter(length, y)
 paste(y[1],y[2])
#a<-gsub(" ", ",", x, fixed=TRUE)

# y[4]
# y[10]
# y[12]
# y[13]
 
#length(y)
 print(!grepl("^[A-Za-z,]+$", y[12], perl = T))
 flag = TRUE

for (i in length(y):length(y)-10) {
    if (!grepl("^[A-Za-z,]+$", y[i], perl = T)) {
      a <- y[i]
      b <- y[i-1]
      c <- y [i-3]
      browse()
    }
  break
}
a
b
c