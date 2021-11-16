library(xts)
library(xgboost)

data <- read.csv('C:/Users/Max/Desktop/CSGO/maindata_fresh.csv')

# Removing flash columns (Too many NAs to work with)
data <- data[, -grep('Flash', colnames(data))]
# Removing all drawn games (Too few to matter)
data <- data[data$Outcome != 0,]

# Removing insignificant teams that have less than 30 games on record
teams <- c(data$Team.1, data$Team.2)

freq <- as.data.frame(table(teams), stringsAsFactors = FALSE)
freq <- freq[order(freq$Freq, decreasing = TRUE),]

keep <- freq[freq$Freq >= 30, 1]

data <- data[data$Team.1 %in% keep,]
data <- data[data$Team.2 %in% keep,]

# The usual transformation process should not apply to the odds
odds <- data[, c('ID', 'Team.1.Odds', 'Team.2.Odds', 'Outcome')]
odds <- na.omit(odds)
# Retaining the odds seperately for pratical purposes
# One data_frame is meant to be processed as a feature, the other is used for return calculation and simulation purposes
decimal_odds <- odds

# Seperating the odds from the rest of the features 
data[, c('Date', 'Event', 'Team.1.Odds', 'Team.2.Odds', 'Team.1.Score', 'Team.2.Score')] <- NULL

# Processing the historic performance for each team
process_past_performance <- function(teams, data, lookback_window){
  
  stats_mean <- list()
  stats_sd <- list()
  
  for(teamname in teams){
    
    print(teamname)
    
    team1 <- data[data$Team.1 == teamname,]
    team2 <- data[data$Team.2 == teamname,]
    
    team1 <- team1[, -grep('Team.2.', colnames(team1))]
    team1$Team.1 <- NULL
    team1$Team.2 <- NULL
    colnames(team1) <- gsub('Team.1.', '', colnames(team1))
    
    Side <- rep(1, nrow(team1))
    team1 <- cbind(team1, Side)
    
    team2 <- team2[, -grep('Team.1.', colnames(team2))]
    team2$Team.1 <- NULL
    team2$Team.2 <- NULL
    colnames(team2) <- gsub('Team.2.', '', colnames(team2))
    
    Side <- rep(2, nrow(team2))
    team2 <- cbind(team2, Side)
    
    team <- rbind(team1, team2)
    team <- team[order(team$ID),]
    
    i <- match(c('ID', 'Side'), colnames(team))
    identifiers <- team[, i]
    team_stats <- team[, -i]
    
    n <- lookback_window
    mean_values <- rollapplyr(team_stats, width = list(-1:-n), mean, fill = NA)
    sd_values <- rollapplyr(team_stats, width = list(-1:-n), sd, fill = NA)
    
    mean_values <- cbind(identifiers, mean_values)
    sd_values <- cbind(identifiers, sd_values)
    
    stats_mean[teamname] <- list(mean_values)
    stats_sd[teamname] <- list(sd_values)
    
  }
  
  stats_mean <- as.data.frame(do.call(rbind, stats_mean))
  stats_mean <- na.omit(stats_mean)
  stats_sd <- as.data.frame(do.call(rbind, stats_sd))
  stats_sd <- na.omit(stats_sd)
  
  freq <- as.data.frame(table(stats_mean$ID), stringsAsFactors = FALSE)
  IDs <- as.numeric(freq[freq$Freq == 1, ]$Var1)
  
  stats_mean <- stats_mean[!stats_mean$ID %in% IDs,]
  stats_sd <- stats_sd[!stats_sd$ID %in% IDs,]
  
  stats <- list(stats_mean, stats_sd)
  names(stats) <- c('mean', 'sd')
  return(stats)
  
}

teams <- unique(c(data$Team.1, data$Team.2))
stats <- process_past_performance(teams, data, lookback_window = 16)

monte_carlo_sim <- function(stats_mean, stats_sd, stat_names){
  
  output <- data.frame()
  IDs <- unique(stats_mean$ID)
  
  for(ID in IDs){
    
    match_mean <- stats_mean[stats_mean$ID == ID,]
    match_sd <- stats_sd[stats_sd$ID == ID,]
    
    probs <- c(ID)
    
    for(stat in stat_names){
      
      stat1 <- match_mean[match_mean$Side == 1, stat]
      stat1_sd <- match_sd[match_sd$Side == 1, stat]
      
      stat2 <- match_mean[match_mean$Side == 2, stat]
      stat2_sd <- match_sd[match_sd$Side == 2, stat]
      
      # Normal distribution
      perf1 <- rnorm(10000, mean = stat1, sd = stat1_sd)
      perf2 <- rnorm(10000, mean = stat2, sd = stat2_sd)
      
      perf_diff <- perf2 - perf1 
      implied_prob <- pnorm(0, mean = mean(perf_diff), sd = sd(perf_diff))
      
      # Logisitic distribtution
      #perf1 <- rlogis(10000, stat1, stat1_sd)
      #perf2 <- rlogis(10000, stat2, stat2_sd)
      
      #perf_diff <- perf2 - perf1
      #ds <- ecdf(perf_diff)
      #implied_prob <- ds(0)
      
      probs <- c(probs, implied_prob)
      
    }
    
    output <- rbind(output, probs)
    print(which(IDs == ID))
    
  }
  
  colnames(output) <- c('ID', stat_names)
  return(output)
    
}

stat_names <- colnames(stats$mean)[-c(1,2,3)]
mainframe <- monte_carlo_sim(stats_mean = stats$mean, stats_sd = stats$sd, stat_names)

mainframe <- merge(mainframe, odds, by = 'ID')
mainframe$Outcome <- as.factor(mainframe$Outcome)

mainframe$Deaths.mean <- 1 - mainframe$Deaths.mean
mainframe$Deaths.sd <- 1 - mainframe$Deaths.sd
mainframe$Deaths.min <- 1 - mainframe$Deaths.min
mainframe$Deaths.max <- 1 - mainframe$Deaths.max

# Processing complete

# Prediction algo simulation

xgboost_sim <- function(data, decimal_odds, iterations){
  
  pathways <- list()
  eval <- data.frame()
  
  for(i in 1:iterations){
    
    train_sample <- sample(nrow(data), 0.5*nrow(data))
    train <- data[train_sample, ]
    test <- data[-train_sample, ]
    
    xgbtrain <- train[, -1]
    xgbtest <- test[, -1]
    
    testlabel <- factor(xgbtest$Outcome)
    testlabel <- as.integer(testlabel) - 1
    xgbtest$Outcome <- NULL
    
    trainlabel <- factor(xgbtrain$Outcome)
    trainlabel <- as.integer(trainlabel) - 1
    xgbtrain$Outcome <- NULL
    
    #PCA <- prcomp(xgbtrain, scale = TRUE)
    #xgbtrain <- as.data.frame(predict(PCA, xgbtrain)[, 1:40])
    #xgbtest <- as.data.frame(predict(PCA, xgbtest)[, 1:40])
    
    xgbtrain <- as.matrix(xgbtrain)
    xgb.train = xgb.DMatrix(data = xgbtrain, label = trainlabel)
    xgbtest <- as.matrix(xgbtest)
    xgb.test = xgb.DMatrix(data = xgbtest, label = testlabel)
    
    params <- list(
      booster = 'gblinear',
      eta = 0.01,
      objective = 'binary:logistic',
      eval_metric = 'error'
    )
    
    xgb.fit = xgb.train(
      params = params,
      data = xgb.train,
      nrounds = 1000,
      early_stopping_rounds = 100,
      watchlist = list(train = xgb.train, test = xgb.test),
      verbose = 0
    )
    
    xgb.pred <- predict(xgb.fit, xgbtest, reshape = TRUE)
    xgb.pred <- as.data.frame(xgb.pred)
    colnames(xgb.pred) <- c('Prob2')
    
    xgb.pred$Prob1 <- 1 - xgb.pred$Prob2
    xgb.pred <- xgb.pred[c('Prob1', 'Prob2')]
    
    xgb.pred$Prediction <- ifelse(xgb.pred$Prob1 > xgb.pred$Prob2, 1, 2)
    
    results <- test[, c(1, ncol(test))]
    results <- cbind(results, xgb.pred)
    
    confusion <- data.frame(target = results$Outcome, prediction = results$Prediction)
    xgbacc <- sum(diag(table(confusion))) / nrow(confusion)
    
    results <- merge(results, decimal_odds[, c(1, 2,3)], by = 'ID')
    results <- results[, c('ID', 'Outcome', 'Prediction', 'Prob1', 'Prob2', 'Team.1.Odds', 'Team.2.Odds')]
    results <- na.omit(results)
    
    kelly_criterion <- function(odds, prob, frac){
      output <- (prob * (odds - 1) - (1 - prob)) / (odds - 1)
      output <- output / frac
      return(output)
    }
    
    kelly <- apply(results[, 4:ncol(results)], 1, function(x){
      probs <- c(x[1], x[2])
      odds <- c(x[3], x[4])
      output <- kelly_criterion(odds = odds, prob = probs, frac = 10)
      output <- round(output, 3)
      names(output) <- c('K1', 'K2')
      return(output)
    })
    
    kelly <- t(kelly)
    kelly[kelly < 0] <- 0
    results <- cbind(results, kelly)
    
    returncalc <- results[, c('Outcome', 'Team.1.Odds', 'Team.2.Odds', 'K1', 'K2')]
    
    returns <- apply(returncalc, 1, function(x){
      
      x <- as.numeric(x)
      
      loss <- x[4] +  x[5]
      outcome <- x[1]
      
      # Team 1 wins
      if(outcome == 1){
        gain <- x[2] * x[4]
      }
      
      if(outcome == 2){
        gain <- x[3] * x[5]
      }
      
      profit <- gain - loss
      return(profit)
    })
    
    cumret <- cumprod(1 + returns) - 1
    
    metrics <- data.frame(accuracy = xgbacc,
                          return = as.numeric(cumret[length(cumret)]),
                          maximum = max(cumret),
                          mimum = min(cumret))
    
    eval <- rbind(eval, metrics)
    pathways[i] <- list(cumret)
    
    print(i)
    
  }
  
  output <- list(eval, pathways)
  names(output) <- c('stats', 'pathways')
  return(output)
  
}


output <- xgboost_sim(data = mainframe, decimal_odds = decimal_odds, iterations = 300)
stats <- output$stats
pathways <- output$pathways
pathways <- do.call(cbind, pathways)

# Raw prediction Accuracy
boxplot(stats$accuracy)
mean(stats$accuracy)

# Accuracy vs return
plot(stats$accuracy, stats$return, pch = 19)
abline(h = 0)

# Distribution of returns
d <- density(stats$return)
plot(d, xlab = 'x', main = 'Distribution of returns', 
     cex.lab = 0.8, cex.axis = 0.8)
polygon(x = d$x, y = d$y,
        col = adjustcolor('steelblue', alpha.f = .5), border = FALSE)
abline(v = median(stats$return))

# Risk of loss and expected value
risk <- ecdf(stats$return)
risk(0)
mean(stats$return)

# Plot all possible pathways
plot(pathways[, 1], type = 'l', ylim = c(-1, max(stats$maximum)), col = 'gray')

for(i in 2:ncol(pathways)){
  lines(pathways[, i], col = 'gray')
}

abline(h = 0, lwd = 1)
abline(h = -1, lwd = 1, lty = 2)

lines(1:nrow(pathways), 
      seq(0, mean(stats$return), 
      length.out = nrow(pathways)),
      lty = 2, 
      lwd = 2)

path_mean <- rowMeans(pathways)
path_sd <- apply(pathways, 1, sd)

# Averaged pathway
lines(1:nrow(pathways),
      path_mean,
      lwd = 2, 
      col = 'steelblue')

# 95% Confidence interval
lines(1:nrow(pathways),
      path_mean + 2 * path_sd,
      lwd = 2, lty = 2,
      col = 'steelblue')

lines(1:nrow(pathways),
      path_mean - 2 * path_sd,
      lwd = 2, lty = 2,
      col = 'steelblue')

polygon(x = c(1:nrow(pathways), rev(1:nrow(pathways))), 
        y = c(path_mean - 2 * path_sd, rev(path_mean + 2 * path_sd)),
        col = adjustcolor('steelblue', alpha.f = .1), border = FALSE)
