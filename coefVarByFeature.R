# x is input matrix (samples x features)
coef.var.mets <- function(x, dstart=1){
  x <-as.data.frame(t(x))
  mylist <- vector(mode="list", length = dim(x)[1])
  rnames <- rownames(x)
  for (i in 1:dim(x)[1]) {
    x1 <- t(x)[dstart:dim(x)[2],i]
    cov1 <- 100*sd(x1)/mean(x1)
    mylist[i] <- cov1}
  write.table((t(rbind(rnames, mylist))),"CoefVarFeatures.txt",row.names = TRUE, col.names = FALSE)
  for (k in c(100,90,80,70,60,50,45,40,35,30,25,20,15,10,5)){
    l <- round((length(which(k >= mylist))/(dim(x)[1])*100),1) ## %
    m <- (length(which(k >= mylist))) ## count
    print(c(k,l,m))
  }
}

