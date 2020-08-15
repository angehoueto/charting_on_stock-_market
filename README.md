# Technical Analysis


  Thechinical Analysis is a style of trading opposed to fundamentals. One of the main techincal analysis strategy is chart analysis.That project will try to find keys level to take a trade on the stock market. That project will evlute and we will slowly add more charts figures.
   We will first try to find one of the most weel known wat to trade with chart analysis: _SUPPORT AND RESISTANCE_
   You got to buy a stock on a support level and to sell it on a resistance level. You can also wait the support (resitance) to be broke before selling(buying). As we always say. The trend is your friend.To know more about technical analysis click just right [here](https://www.investopedia.com/articles/active-trading/102914/technical-analysis-strategies-beginners.asp)
  The project will run on trader workstation from Interractive broker; why? Just because they offer a well integreated api services with Rstudio, VBA and Excel. But don't run if you don't work with Rstudio just download the Excel file and run to the setion named _find keys level_
  
## DATA IMPORTATION

  We must firstly open trader workstation on your desktop and enable your api to run. Click [here](https://interactivebrokers.github.io/tws-api/index.html)for more information. You will now have to connect your Rstudio application to trader workstation. You will have to use the _twsConnect_ (with they talked about on the link) function to.

```{r}
library(IBrokers)
tws = twsConnect(port=7497)
tws
```
  
 We will now use some variables that we will use futher in our code. You gotta change the *symbol* in "" for the ticker of your choice. You must also change for the variables *bars* and *duration* to match with your holding time period. The variable *long* purpose is to determine wich kind on price is really a resistance or not; if our chart come to hit a price located at the same level to a price one month ago and go back we got a resistance. But if our price just don't move in a week (or a day depending to your trading style) it won't be a resistance, this is just a lack of fluctuation. So be sure to change the variables in the right way to got some persistance in the prices that will find the model. If you're not sure about how to set the variables on the right level just let them as they are.
 
```{r}
symbol <- twsSTK("AAPL")
bars <- "1 day"
duration <- "6 M"
long <- 5
```
 
 We will now import our data directly from Ibroker server:
 
```{r}
dat = reqHistoricalData(tws, symbol,barSize =  bars ,duration = duration)
dat = round(dat,0)
colnames(dat)[1:8]= c("Open","High","Low","Close","Volume","WAP","HasGaps","Count")
```
 
  If you're not with Ibroker option just download the Excel file and import it:
  
```{r}
dat <- read_excel("dat.xlsx")
```


## Find keys levels
  
  To find our keys levels, we will try to see which prices will come twice in our prices distribution. You could also use the mode to find the main support level. But we want to have a set of prices to got our trade as fast as possible on the right level. So we will try to determine some prices and we will clean our prices to got more precise levels.

```{r}
tabf=as.data.frame(table(dat$Close))
tabf = tabf[!(tabf$Freq == 1),]
tabfs = sort(tabf$Var1, decreasing = T)
res = as.numeric(levels(tabfs))[tabfs]
res=sort(res)
```

 Okay now stay tuned. We will go to the dark side of the strength. I will explain you what we want to do now. In res we got some prices. They are already sorted. But we can have prices like *245,246* or *265,267* by example. We're all okay that we won't take a trade at 246 dollars and another trade at 246 dollars. So we got to find a way to make our model understand that prices that are closed are the same. *Don't worry* we won't use Deepl models here. We will keep them for others project.
   Here we go. We will first try to group our prices into intervals.We will set our intervals. The length of our interval depend of our price level, We will choose a 0.01 length for instruments like forex with prices less than 1$. For other instruments like stock, futures etc 1 dollar length of interval for other instruments with a prince between 1 dollar and a 10 dollar length interval for more. With a price more than more than 999 we will use a 100 length interval. _*Notice that we test that model on stocks between 100 and 300 dollars, so we're not sure bout the precision on stock with more or less than those prices*_
   We will define our intervals borders directly from our prices. We will just take our minimum price and we will make a sequence of number by jumping from *skip* to *skip*:

```{r}

if ( nchar(round(res[1],0)) == 1) {
  skip <- 0.01
} else if ( nchar(round(res[1],0)) == 2) {
  skip <- 1
} else if ( nchar(round(res[1],0)) == 3) {
  skip <- 10
} else {
  skip <- 100
}

seq_to_check <- as.data.frame(seq(from = res[1], to= res[length(res)], by = skip))
```

  We will now group our prices together in our intervalls.

```{r}
groups <- c()
for (i in 1: nrow(seq_to_check)){
  groups <- as.data.frame(c(groups,paste("G", i, sep = "")))
}

groups <- as.data.frame(t(groups))
groups <- cbind(seq_to_check,groups)
```

  Now we will match res and group to make our res into different groups
  
```{r}
res <- as.data.frame(res)
id <- as.data.frame(c(1:nrow(res)))
res <- cbind(res, id)


for (j in 2:nrow(groups)){
  for (i in 1: nrow(res)){
    if (res[i,1] > groups[j,1]){
      res[i,2] <- j
    }
  } 
}
colnames(res)[2]="id"
```

  Make a small preview of your data before going futher:
  
```{r}
View(res)
```

  We will now try to eliminate support due to lack from fluctuation from our list of prices: the next loop is long but simple to understand. We will just try to find by group, in our list of prices which one come into one *long* variable interval. For exemple if long equal 5 as in our code, if we got prices in the same group in that interval of time, we will consider that it's just a lack of fluctuation. We will verify our dates directly from our main source of data : the dataframe _dat_.
  
```{r}
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
  
```

  Now check res values
  
```{r}
View(res)
```

 Far away better unh! We'll now continue by finding a mean price for each interval (groups) 

```{r}
mean_price <- aggregate(res[,1], list(res$id), mean)
mean_price
```


## Place your order
  
  You can now take your trades at the prices found with your code. _*WARNING*_ *Be careful, your good old human eyes and your experience on the stock market will (i hope to) never go to be replaced by a machine; so you should verify your prices before going to a trade*
  Of course te next code will only execute itself if you're with Ibrokers and if you set up the api correctly. If not just enter your trade with hands.
  
```{r}
for (i in 1:nrow(mean_price)){
  price <- mean_price[i,2]
  id = as.numeric(reqIds(tws))
  price = 108.7
  myorder = twsOrder(id, orderType="STP LMT", lmtPrice= price,
                     auxPrice="108.10",action="SELL",totalQuantity="10",
                     transmit=FALSE)    
  
  placeOrder(tws, twsSTK("AAPL"), myorder) 
}
```

  We hope that you found that small model useful. We will try to make our best to find more patterns on the stock market. We'll voluntary make our models with hands without Deep Learning models even if Deepl models will work more efficiently. But our goal here is to test our mathematics, informatics and statistitical skills.

