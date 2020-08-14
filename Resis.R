#open and connect IB 
#ressources : https://cran.r-project.org/web/packages/IBrokers/IBrokers.pdf
library(IBrokers)
tws = twsConnect(port=7497)
tws

#Prepare vairbales to use in our code
symbol <- twsSTK("AAPL")
bars <- "1 day"
dur <- "6 M"
long <- 5

#import data
dat = reqHistoricalData(tws, symbol,barSize =  bars ,duration = dur)
dat = round(dat,0)
colnames(dat)[1:8]= c("Open","High","Low","Close","Volume","WAP","HasGaps","Count")

#find where we have more than 2 points of contact
tabf=as.data.frame(table(dat$Close))
tabf = tabf[!(tabf$Freq == 1),]
tabfs = sort(tabf$Var1, decreasing = T)

res = as.numeric(levels(tabfs))[tabfs] #in res we have our target prices but those prices are too confused we shloud treat them to made one price emerge
res=sort(res)

if ( nchar(round(res[1],0)) == 1) {
  saut <- 0.01
} else if ( nchar(round(res[1],0)) == 2) {
  saut <- 1
} else if ( nchar(round(res[1],0)) == 3) {
  saut <- 10
} else {
  saut <- 100
}

seq_to_check <- as.data.frame(seq(from = res[1], to= res[length(res)], by = saut))

groups <- c()
for (i in 1: nrow(seq_to_check)){
  groups <- as.data.frame(c(groups,paste("G", i, sep = "")))
}

groups <- as.data.frame(t(groups))
groups <- cbind(seq_to_check,groups)

res <- as.data.frame(res)
id <- as.data.frame(c(1:nrow(res)))
id <- 1
res <- cbind(res, id)


for (j in 2:nrow(groups)){
  for (i in 1: nrow(res)){
    if (res[i,1] > groups[j,1]){
      res[i,2] <- j
    }
  } 
}

#First way to treat our prices is to delete where we have two contact ot because of a resistance presence
#but because of unvariabiliy due to consequent days. If our price doesn't we'll have two points of contac
#but it's not mean that we have a support there. So we will delete from out target prices observations where prices are in the same period

to_check = unique(res$id)

for (i1 in unique(to_check[1:length(to_check)])){
  a <- c()
  for (i2 in 1:nrow(res)){
    if (res[i2,2] == i1){
      a <- c(a,which(dat$Close==res[i2,1]))
      a <- sort(a)
    }
  }
  for (i3 in 1:length(a)){
    tryCatch({
      (
        if (((a[i3+1]-a[i3]) <long)) {
          res = res [-i3,]
        }
      )
    }, error = function(e) {
      cat("ERROR :", conditionMessage(e), "\n")
    })  
  }
}

#we will make an ajustment if prices is less than one (forex case by example)
mean_price <- aggregate(res[,1], list(res$id), mean)
mean_price

#place order
for (i in 1:nrow(mean_price)){
  price <- mean_price[i,2]
  id = as.numeric(reqIds(tws))
  price = 108.7
  myorder = twsOrder(id, orderType="STP LMT", lmtPrice= price,
                     auxPrice="108.10",action="SELL",totalQuantity="10",
                     transmit=FALSE)    
  
  placeOrder(tws, twsSTK("AAPL"), myorder) 
}

