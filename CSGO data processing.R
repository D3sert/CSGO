library(jsonlite)
setwd('C:/Users/Max/Desktop/CSGO')
data <- fromJSON("main.json")

bool <- grepl('2015', data)
data <- data[!bool]

bool <- (grepl('January', data) | grepl('February', data) | grepl('March', data)) & grepl('2016', data)
data <- data[!bool]

# Fucked up game (example ID == 2302287 and 2305027)
bool <- lapply(data, function(data){
  output <- lapply(data$Match, function(match){
    scoreboard <- match$Scoreboard
    bool <- lapply(scoreboard, function(stats){
      dimensions <- dim(stats)
      bool <- sum(dimensions == c(5, 9))
    })
    bool <- ifelse(bool == 2, TRUE, FALSE)
  })
  output <- do.call(rbind, output)
  all(output)
})

bool <- do.call(rbind, bool)
data <- data[bool]

data <- lapply(data, function(data){
  
  odds <- data$Odds
  if(length(odds) == 1){
    odds <- c(NA, NA)
  }
  
  score <- as.numeric(data$Score)
  if(score[1] > score[2]){
    outcome <- 1
  }
  if(score[1] < score[2]){
    outcome <- 2
  }
  if(score[1] == score[2]){
    outcome <- 0
  }
  
  gamestats <- lapply(data$Match, function(x){
    list(x$Scoreboard, x$Entries, x$Clutches)
  })
  
  gamestats <- lapply(gamestats, function(gamestats){
    
    # Clutches and entries
    misc_stats <- sapply(gamestats[2:3], strsplit, ':')
    misc_stats <- do.call(rbind, misc_stats)
    misc_stats <- apply(misc_stats, 2, as.numeric)
    rownames(misc_stats) <- c('Entries', 'Clutches')
    
    # Scoreboard stats
    scoreboard <- lapply(gamestats[[1]], function(teams){
      out <- apply(teams[,2:3], 2, function(x){
        out <- strsplit(x, ' ') 
        out <- do.call(rbind, out)
        if(ncol(out) == 1){
          out <- cbind(out, NA)
        } else {
          out <- gsub('\\(|\\)', '', out)
        }
        return(out)
      })
      
      kill_hs <- out[1:5,]
      assist_f <- out[6:10,]
      
      teams[,5] <- as.numeric(sub('%', '', teams[,5], fixed = TRUE))/100
      teams <- cbind(kill_hs, assist_f, teams[,4:ncol(teams)])
      teams <- suppressWarnings(apply(teams, 2, as.numeric))
      colnames(teams) <- c('Kills', 'Assists', 'HS', 'Flashes', 'Deaths', 'KAST', 'K/D Diff', 'ADR', 'FK Diff', 'Rating')
      
      KDA <- (teams[,1]+teams[,2])/teams[,5]
      HSpc <- teams[,3]/teams[,1]
      teams <- cbind(teams, KDA, HSpc)
      
      stats <- apply(teams, 2, function(x){
        c(mean(x), sd(x), max(x), min(x))
      })
      
      rownames(stats) <- c('mean', 'sd', 'max', 'min')
      
      columns <- sapply(colnames(stats), function(x){
        paste(x, names(stats[,1]))
      })
      
      statnames <- as.vector(columns)
      tempstats <- as.vector(stats)
      names(tempstats) <- statnames
      return(tempstats)
    })
    output <- do.call(rbind, scoreboard)
    output <- cbind(t(misc_stats), output)
    return(output)
  })
  
  gamestats <- Reduce('+', gamestats)/length(gamestats)
  
  names(gamestats[,1])
  columns <- sapply(colnames(gamestats), function(x){
    paste(names(gamestats[,1]), x)
  })
  statnames <- as.vector(columns)
  gamestats <- as.vector(gamestats)
  names(gamestats) <- statnames
  
  metastats <- c(data$Date, data$Event, data$Teams, odds, score, outcome)
  names(metastats) <- c('Date', 'Event', 'Team 1', 'Team 2', 'Team 1 Odds', 'Team 2 Odds', 'Team 1 Score', 'Team 2 Score', 'Outcome')
  
  output <- c(metastats, gamestats)
  return(output)
})

data <- do.call(rbind, data)
data <- as.data.frame(data)

data[5:ncol(data)] <- apply(data[5:ncol(data)], 2, as.numeric)
data[5:ncol(data)] <- round(data[5:ncol(data)], 3)
data <- cbind(ID = as.numeric(rownames(data)), data)
rownames(data) <- NULL

data$Date <- gsub("(\\d)(st|nd|rd|th)\\b", "\\1", data$Date)
data$Date <- as.Date(data$Date, format = '%B %d %Y')

write.csv(data, 'C:/Users/Max/Desktop/CSGO/maindata_fresh.csv', row.names = FALSE)



