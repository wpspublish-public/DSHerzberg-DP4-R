library(tidyverse)

A<-seq(1:5)
B<-seq(6,10)
C<-c("x","y","x","y","x")
dat<-data.frame(A,B,C)
cols<-names(dat)

list <- map(names(dat), ~dat[.x])
