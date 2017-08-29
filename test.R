x <- "5 Lee Banshee                       2.2 "


z <- strsplit(x[[1]], " ")[[1]]
y <- strsplit(z, " ")
y[lapply(y,length)>0]
y <- Filter(length, y)


h <- rev(y)
for (i in 1 : 10) {
  if (!grepl("^[A-Za-z,&1st]+$", h[i], perl = T)) {
    a <- h[i]
    b <- h[i+1]
    c <- h [i+2]
    break
  }

}
a
b
c

