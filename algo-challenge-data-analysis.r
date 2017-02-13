library(data.table)
require(bit64)
library(ggplot2)

## Data loading

## Creates a smaller subset of samples, for easy rmarkdown creation
load.all.data.and.create.sample.dataset <- function () {
  all.data2 <- fread("/Users/dputer/Documents/University/Topics\ in\ data\ science/exercises/final-project/algorithmic-trading-challenge-training.csv", 
                    header = T)
  
  create.sample.subset <- function(data.to.subset) { 
    data.subset <- data.to.subset[1:50]
    passed.ids <- c(security.ids[1])
    for (i in security.ids[-1]) {
      if (i == security.ids[length(security.ids)]) {
        break
      }
      passed.ids <- c(passed.ids, i)
      row.start <- min(as.integer(data.to.subset[!(security_id %in% passed.ids), head(row_id)]))
      data.subset <- rbind(data.subset, data.to.subset[row.start:(row.start+50)])
    }
    return(data.subset)
  }
  data.subset <- create.sample.subset(all.data)
  write.csv(data.subset, "/Users/dputer/Documents/University/Topics\ in\ data\ science/exercises/final-project/training-subset.csv", sep = ",")
  remove(all.data)
}

## Utility Methods for training data

## Code for getting the columns with missing values, if any, for a given data table/ data frame.
### Parameter: {data frame}: The data for which to check missing values.
get.cols.with.missing <- function(data) {
  cols.with.missing = c()
  for (c in colnames(data)) {
    if (sum(is.na(data[,c]))) {
      cols.with.missing <- c(cols.with.missing, c)
    } 
  }
  return(cols.with.missing)
}


## Code for finding how many trading days we have for each security, assuming trade data 
## sorted in days->security->time.
##
## Parameter: 
##    data {data.table}: The securities data. We assume 2 cols exist: "security_id" and "time1".
##    required.security.ids {list/vector}:  The list of ids of the securities for which we want 
##      to find the trading days count.
## Return: {data.table} containing 2 cols: "security_id" and "days".
get.securities.days.info <- function(data, required.security.ids) {
  securities.days.per.hour <- data.table(security_id=character(), hour=character(), days=integer())
  # The idea, for each security, search for the number of "batches" of a given hour, 
  # this will provide the number of days for each stock.
  for (s in required.security.ids) {
    security.specific.data <- data[security_id == s, .(time1)]
    security.specific.data[, hour:=as.integer(substr(time1,0,2))]
    for (h in 8:16) {
      days <- 0
      i <- 1
      while(i <= nrow(security.specific.data)) {
        # Once we find the first row of the hour we're searching for, increment the number of days, 
        # and skip rows till the end of the current batch.
        if (security.specific.data[i, hour] == h) {
          days <- days + 1
          while((security.specific.data[i, hour] == h) && (i <= nrow(security.specific.data))) {
            i <- i+1
          }
        } 
        i <- i+1
      }
      # Add the number of days to our current security/hours/days matrix
      securities.days.per.hour <- rbind(securities.days.per.hour, list(s, h, days))
      print(paste("Security: ", s, " hour: ", h, "days: ", days))
    }
  }
  
  # Now find the max of each one
  securities.days.info <- securities.days.per.hour[, .(days=max(days)), security_id]
  return(securities.days.info)
}

split.data.as.competition <- function(all.data) {
  # In case we want to look at the split point
  # print(all.data[(nrow(all.data)-50000-5):(nrow(all.data)-50000+5), .(row_id,security_id,time1)])
  
  # Okay, let's split the train and test (i.e., the train and test data as declared by the data providers)
  test.data <- all.data[(nrow(all.data) - 50000 + 1):(nrow(all.data))]
  training.data <- all.data[1:(nrow(all.data) - 50000)]
  return(list(training.data=training.data, test.data=test.data))
}

## Parameter: 
##    security_price {numeric}: The price of the security
## Return: The minimum price incerement for orders, according to the security price.
getMinPriceIncerement <- function (security_price) {
  if (security_price <= 0.9999)	{
    return(0.0001)
  } else if (security_price <= 4.9995) {
    return(0.0005)
  } else if (security_price <= 9.999) {
    return(0.001)
  } else if (security_price <= 49.995) {
    return(0.005)
  } else if (security_price <= 99.99) {
    return(0.01)
  } else if (security_price <= 499.95) {
    return(0.05)
  } else if (security_price <= 999.9) {
    return(0.1)
  } else if (security_price <= 4999) {
    return(0.5)
  } else if (security_price <= 9999) {
    return(1)
  } else {
    return(5)
  }
}


## Main

############ Loading the data and first look

## Loading just the sample data (for quick rmarkdown handling)
all.data <- fread("/Users/dputer/Documents/University/Topics\ in\ data\ science/exercises/final-project/training-subset.csv", header = T)

# Full data
# all.data <- fread("/Users/dputer/Documents/University/Topics\ in\ data\ science/exercises/final-project/algorithmic-trading-challenge-training.csv",
#                   header = T)

# s<-split.data.as.competition(all.data) 
# remove(all.data)
# security.ids <- levels(factor(s$training.data[,security_id]))
# print(paste("# of securities in data:", length(security.ids)))
# days.info <- get.securities.days.info(s$training.data, security.ids)

# # First look at the data! Wooho :)
# str(all.data)
# 
# # The data description mentioned the last 50K samples in the data are actually "test" data of the competition, 
# # which was actually appended to the training (a new test data for the competition was created later). 
# # The appneded test data was randomly sampled, as opposed to the oredered training data. Let's have a quick look
# head(all.data)
# tail(all.data)


############ Split the data to training and test data (based on competition data)

# # Let's verify that the switch indeed startes in the last 50K
# print(all.data[(nrow(all.data)-50000-5):(nrow(all.data)-50000+5), .(row_id,security_id,time1)])
# 
# # Okay, let's split the train and test (i.e., the train and test data as declared by the data providers)
# test.data <- all.data[(nrow(all.data) - 50000 + 1):(nrow(all.data))]
# training.data <- all.data[1:(nrow(all.data) - 50000)]
# # Now let's free up some memory
# remove(all.data)
# 
# # Training Data seems to be ascendingly sorted by (1)days => (2)security_id => (3)time1. 
# # Let's see how many days we have per security. Result: 6 days.
# securities.days.info <- getSecuritiesDaysInfo(training.data, security.ids)
# print(securities.days.info)
# 
# # Let's merge the data back (doesn't depend on days)
# all.data <- rbind(training.data, test.data)
# remove(training.data, test.data)


############ Exploring training data stucture

# # The head of training data returns the same security ID. Let's find out the index for the first row which includes 
# # a different security 
# all.data[security_id!=1, head(row_id)]
# # Got 3196 3197 3198 3199 3200 3201, let's see records a bit before and after 3196, 
# # (probably switches from security id 1 to 2). Result: indeed switches from 1 to 2.
# all.data[3193:3198, .(row_id, security_id, time1)]

# Securities are identified by an ID, let's see how many are there :)  Result=102
security.ids <- levels(factor(all.data[,security_id]))
print(paste("# of securities in data:", length(security.ids)))


############ Missing data
cols.with.missing <- get.cols.with.missing(all.data)
if (length(cols.with.missing) > 0) {
  print(paste("Found cols with missing values! Handle them first:", cols.with.missing))
  quit()
} else {
  print("No missing values found")
}

############ Setting correct types for the data
# The data description mentions a data error where bid/ask #50 is the same as #51. Let's verify.
if (sum(!all.data[,bid50==bid51 & ask50==ask51]) > 0) {
  print("Data NOT the same for bid/ask 50 and 51!")
  quit()
} else {
  print("bid/ask50==51, removing bid/ask51")
  all.data[, bid51:=NULL]
  all.data[, ask51:=NULL]
}

if(length(all.data[transtype49 != TRUE, row_id])) {
  print("Shock trade has an unexpected type!")
  quit()
} 

# So trnastype49 is always "trade", and transtype50 is always quote. We'll remove them.
all.data[,transtype49:=NULL]
all.data[,transtype50:=NULL]

# A bit of metaprogramming to save some time when transforming transtype :)
for (i in 1:48) {
  assign("temp", eval(substitute(paste("transtype", i, sep = ""))))
  all.data[, eval(temp):=as.factor(get(temp))]
}
remove(temp)

# "initiator" is also Factor
all.data[,initiator:=as.factor(initiator)]

# Convert string times to milliseconds from 8am ("epoch")
library(lubridate)
epoch <- hms("08:00:00")
for (i in 1:50) {
  assign("temp", eval(substitute(paste("time", i, sep = ""))))
  all.data[, eval(temp):=as.integer(second(hms(get(temp)) - epoch)*1000)]
}
remove(temp)


############ Adding relevant features (normalized spread, )

# Adding normalized spread (i.e, spread in units of the minimum increment, which is dependent of the stock's price).
for (i in 1:49) {
  assign("newfeature", eval(substitute(paste("nspread", i, sep = ""))))
  assign("existingbid", eval(substitute(paste("bid", i, sep = ""))))
  assign("existingask", eval(substitute(paste("ask", i, sep = ""))))
  all.data[, eval(newfeature):=as.numeric((get(existingask) - get(existingbid))/ getMinPriceIncerement(all.data[i, get(existingbid)]))]
}


all.data[i, "mean_nspread"] = mean(unlist(all.data[i, .(nspread1,nspread2,nspread3,nspread4,nspread5,
                                                        nspread6,nspread7,nspread8,nspread9,nspread10,
                                                        nspread11,nspread12,nspread13,nspread14,nspread15,
                                                        nspread16,nspread17,nspread18,nspread19,nspread20,nspread21,
                                                        nspread22,nspread23,nspread24,nspread25,nspread26,nspread27,
                                                        nspread28,nspread29,nspread30,nspread31,nspread32,nspread33,
                                                        nspread34,nspread35,nspread36,nspread37,nspread38,nspread39,
                                                        nspread40,nspread41,nspread42,nspread43,nspread44,nspread45,
                                                        nspread46,nspread47,nspread48,nspread49)]))
head(all.data, n = 2)
for (i in 1:nrow(all.data)) {
  all.data[i, "mean_nspread"] = mean(unlist(all.data[i, .(nspread44,nspread45,nspread46,nspread47,nspread48,nspread49)]))
}


l <- unlist(t(all.data[security_id==1, .(bid1,bid2,bid3,bid4,bid5,bid6,bid7,bid8,bid9,bid10,bid11,bid12,bid13,bid14,bid15,bid16,bid17,bid18,bid19,bid20,bid21,bid22,bid23,bid24,bid25,bid26,bid27,bid28,bid29,bid30,bid31,bid32,bid33,bid34,bid35,bid36,bid37,bid38,bid39,bid40,bid41,bid42,bid43,bid44,bid45,bid46,bid47,bid48,bid49,bid52,bid53,bid54,bid55,bid56,bid57,bid58,bid59,bid60,bid61,bid62,bid63,bid64,bid65,bid66,bid67,bid68,bid69,bid70,bid71,bid72,bid73,bid74,bid75,bid76,bid77,bid78,bid79,bid80,bid81,bid82,bid83,bid84,bid85,bid86,bid87,bid88,bid89,bid90,bid91,bid92,bid93,bid94,bid95,bid96,bid97,bid98,bid99,bid100)]))
l[1:20]

############ Outlier detection


############ Result calculation

## Parameters:
##    predictions.df {data.frame}: A DF which includes only(!) the results (i.e., bid/ask52..100, 
##        or both, but only results). 
##    actual.df {data.frame}: Same as results.prediction, only with true labels. Order should be 
##        exactly the same as the results.prediction DF.
##    score.func {function}: A function which accepts "predictions" and "actual" vectors, and returns a match score.
## Return: {numeric} The result of score.func
calculateScore <- function(predictions.df, actual.df, score.func) {
  predictions <- unlist(t(predictions.df))
  actual <- unlist(t(actual.df))
  if (length(predictions) != length(actual)) {
    print(paste("Different number of cells for predictions:", length(predictions), " and actual:", length(actual)))
    quit()
  }
  return(score.func(predictions, actual))
}

calculateRMSE <- function(predictions.df, actual.df) {
  return(calculateScore(predictions.df, actual.df, function (p, a) { return(sqrt(sum((p-a)^2)/length(p))) }))
}

calculateSMAPE <- function(predictions.df, actual.df) {
  return(calculateScore(predictions.df, actual.df, function(p,a) {return(sum(abs(p-a)/(abs(p) + abs(a)))/length(p))}))
}

calculateRMSEVec <- function(p, a) {
  return(sqrt(sum((p-a)^2)/length(p)))
}

calculateSMAPE1000Vec <- function(p, a) {
  return((sum(abs(p-a)/(abs(p) + abs(a)))/length(p))*1000)
}

install.packages("e1071")
library(e1071)
test.data <- all.data[, .(bid10, bid20,bid52)]

# Required libraries
library(scatterplot3d)
library(rgl)
p3d<-scatterplot3d(x = test.data$bid10, y = test.data$bid20, 
       z = test.data$bid52, xlab = "Bid10", ylab = "Bid20", 
       zlab = "Forcast"  ,color = "red", highlight.3d = TRUE)


run.svr.model <- function(train, test) {
  # Cols bid49, bid50, nspread49, nspread48, 
  model <- svm(result ~ bid49 + bid50 + nspread49 + nspread48 + p_tcount + p_value + trade_vwap + trade_volume + initiator, train)
  return(predict(model, test))
}

do.cross.validation <- function (d, k) {
  # We need a copy of the DF, so we can remove parts from it.
  d2 <- d[1:nrow(d)]
  chunk.size <- nrow(d2) %/% k
  chunks <- list()
  for (i in 1:(k-1)) {
    test.rows <- sample(nrow(d2), chunk.size)
    chunks[[i]] <- d2[test.rows]
    d2 <- d2[-test.rows]
  }
  # The leftover in d2 is actually the last chunk.
  chunks[[k]] <- d2
  # Use each chunk once as a test, while the rest are training.
  test.scores = c()
  for (i in 1:length(chunks)) {
    test.chunk <- chunks[[i]][,-c("result")]
    training.chunk <- rbindlist(chunks[-i])
    current.score <- run.svr.model(training.chunk, test.chunk)
    print(paste("Current svr score:", current.score))
    test.scores <- c(test.scores, current.score)
  }
  return(mean(test.scores))
}

validate.model <- function(training, test) {
  j <- 52
  training <- all.data.old
  test <- all.data
  result.col <- paste("ask", j, sep = "")
  training[,result:=get(result.col)]
  predictions <- run.svr.model(training, test)
  results <- data.table(ask52=predictions)
  for(j in 53:100) {
    result.col <- paste("ask", j, sep = "")
    training[,result:=ask56]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask56=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask57]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask57=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask58]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask58=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask59]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask59=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask60]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask60=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask61]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask61=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask62]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask62=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask63]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask63=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask64]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask64=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask65]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask65=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask66]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask66=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask67]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask67=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask68]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask68=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask69]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask69=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask70]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask70=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask71]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask71=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask72]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask72=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask73]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask73=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask74]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask74=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask75]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask75=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask76]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask76=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask77]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask77=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask78]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask78=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask79]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask79=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask80]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask80=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask81]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask81=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask82]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask82=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask83]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask83=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask84]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask84=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask85]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask85=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask86]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask86=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask87]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask87=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask88]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask88=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask89]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask89=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask90]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask90=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask91]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask91=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask92]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask92=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask93]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask93=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask94]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask94=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask95]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask95=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask96]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask96=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask97]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask97=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask98]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask98=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask99]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask99=predictions)
    results <- cbind(results, current.dt)
    training[,result:=ask100]
    predictions <- run.svr.model(training, test)
    current.dt <- data.table(ask100=predictions)
    results <- cbind(results, current.dt)
    
  }
  calculateSMAPE(results, test[, .(ask52,ask53,ask54,ask55,ask56,ask57,ask58,ask59,ask60,ask61,ask62,ask63,ask64,ask65,ask66,ask67,ask68,ask69,ask70,ask71,ask72,ask73,ask74,ask75,ask76,ask77,ask78,ask79,ask80,ask81,ask82,ask83,ask84,ask85,ask86,ask87,ask88,ask89,ask90,ask91,ask92,ask93,ask94,ask95,ask96,ask97,ask98,ask99,ask100)])
  calculateRMSE(results, test.data.ask)
    
  test.data.ask <- all.data[, .(ask50,ask52,ask53,ask54,ask55,ask56,ask57,ask58,ask59,ask60,ask61,ask62,ask63,ask64,ask65,ask66,ask67,ask68,ask69,ask70,ask71,ask72,ask73,ask74,ask75,ask76,ask77,ask78,ask79,ask80,ask81,ask82,ask83,ask84,ask85,ask86,ask87,ask88,ask89,ask90,ask91,ask92,ask93,ask94,ask95,ask96,ask97,ask98,ask99,ask100)]
  naive.prediction.ask <- test.data.ask[,.(ask50)]
  test.data.ask[, ask50:=NULL]
  naive.prediction.ask <- naive.prediction.ask[, `:=`(ask52=ask50, ask53=ask50,ask54=ask50,ask55=ask50,ask56=ask50,ask57=ask50,ask58=ask50,ask59=ask50,ask60=ask50,ask61=ask50,ask62=ask50,ask63=ask50,ask64=ask50,ask65=ask50,ask66=ask50,ask67=ask50,ask68=ask50,ask69=ask50,ask70=ask50,ask71=ask50,ask72=ask50,ask73=ask50,ask74=ask50,ask75=ask50,ask76=ask50,ask77=ask50,ask78=ask50,ask79=ask50,ask80=ask50,ask81=ask50,ask82=ask50,ask83=ask50,ask84=ask50,ask85=ask50,ask86=ask50,ask87=ask50,ask88=ask50,ask89=ask50,ask90=ask50,ask91=ask50,ask92=ask50,ask93=ask50,ask94=ask50,ask95=ask50,ask96=ask50,ask97=ask50,ask98=ask50,ask99=ask50,ask100=ask50)]
  naive.prediction.ask <- naive.prediction.ask[, ask50:=NULL]
  calculateSMAPE(naive.prediction.ask, test.data.ask)
  calculateRMSE(naive.prediction.ask, test.data.ask)
}

all.data.old <- all.data
all.data <- test.data2

bid.ask.avg <- all.data2[,.(bid_avg=mean(bid49), ask_avg=mean(ask49)), by=.(security_id)]
boxplot(bid.ask.avg$bid_avg, bid.ask.avg$ask_avg)
hist(bid.ask.avg$bid_avg, breaks = 20, col = "green", xlab = "Average Security Price", ylab = "# of Securities", main = "Averge Security price")

final.price.shock.ratio <- all.data2[,.(ratio=mean(100*ask100/ask49)), by=.(security_id)]
boxplot(final.price.shock.ratio$ratio, names = c("% last of change ask"))
hist(final.price.shock.ratio$ratio, breaks = 20, col = "green", xlab = "% last of change ask", ylab = "# of Securities", main = "% change of last ask")

middle.price.shock.ratio <- all.data2[,.(ratio=mean(100*ask77/ask49)), by=.(security_id)]
boxplot(middle.price.shock.ratio$ratio, names = c("% change of middle ask"))
hist(middle.price.shock.ratio$ratio, breaks = 20, col = "green", xlab = "% change of middle ask", ylab = "# of Securities", main = "% change of middle ask")

str(all.data) <- all.data2

b523 <- all.data[, .(bid52, bid53)]
str(unlist(t(b523)))
unlist(t(all.data[, .(bid52, bid53)]))[2]

