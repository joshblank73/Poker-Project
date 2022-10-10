# setup
install.packages('tidyr')
install.packages('dplyr')
install.packages('data.table')
install.packages('stringr')
install.packages('tibble')
install.packages('randomForest')
install.packages('caTools')

library('tidyr')
library('dplyr')
library('data.table')
library('stringr')
library('tibble')
library('randomForest')
library('caTools')

setwd('/Users/JoshBlank/poker1')

# importing and cleaning data

## creating poker table
poker_colnames = c('id', 'table', 'pocket', 'flop', 'turn', 'river',
                   'show down', 'summary')
poker <- as_tibble(data.frame(matrix(nrow=0,ncol=length(poker_colnames))))
colnames(poker) <- poker_colnames

for (i in 1:67) {
  file <- paste0('abs NLH handhq_', as.character(i), '-OBFUSCATED.txt')
  raw1 <- readChar(file, nchars = 1e7)
  lst1 <- strsplit(x = raw1, split = '\r\n\r\n\r\n\r\n')
  vec1 <- lst1[[1]]
  tbl1 <- as_tibble(vec1)
  tbl1 <- tbl1[!grepl(pattern = 'Ante', tbl1$value),]
  tbl1 <- tbl1[!grepl(pattern = '1 on 1', tbl1$value),]
  tbl1 <- tbl1 %>%
    separate(value, c('id', 'rest'), sep = '\r\nTable: ', extra = 'merge') %>%
    separate(rest, c('table', 'rest'),
             sep = '\\*\\*\\* POCKET CARDS \\*\\*\\*\r\n', extra = 'merge') %>%
    separate(rest, c('action', 'rest'),
             sep = '\\*\\*\\* SHOW DOWN \\*\\*\\*\r\n', extra = 'merge') %>%
    separate(rest, c('show down', 'summary'),
             sep = '\\*\\*\\* SUMMARY \\*\\*\\*\r\n')
  poker <- rbind(poker, tbl1)
}

poker <- poker[-c(1255, 1826, 3788, 8780, 8811, 9955, 10302, 11050, 11464,
                  12040, 12341, 12933, 16576, 18330, 18668, 19394, 22388,
                  23700),]
poker <- poker %>%
  separate(table, c('table', paste0('p', c(1:6)), 'small', 'big', 'sitout', 'e'),
           sep = '\r\n', remove = T)
poker <- subset(poker, select = -e)
poker <- poker[!(is.na(poker$big) | nchar(poker$big) == 0),]
poker <- poker[!grepl(pattern = 'sitout', poker$big),]
poker <- poker[!grepl(pattern = 'small', poker$p6),]
poker <- poker[nchar(poker$sitout) == 0,]
poker <- subset(poker, select = -sitout)

for (i in 1:6){
  col <- paste0('p', as.character(i))
  name <- paste0('name', as.character(i))
  stack <- paste0('stack', as.character(i))
  trash1 <- paste0('trash1', as.character(i))
  trash2 <- paste0('trash2', as.character(i))
  poker <- poker %>%
    separate(col, c(name, stack), sep = ' \\(\\$', extra = 'merge') %>%
    separate(name, c(trash1, name), sep = '\\- ', extra = 'merge') %>%
    separate(stack, c(stack, trash2), sep = ' in')
}

poker <- poker[,-c(3, 6, 7, 10, 11, 14, 15, 18, 19, 22, 23, 26, 27, 28)]
poker$stack1 <- as.numeric(gsub(',', '', poker$stack1))
poker$stack2 <- as.numeric(gsub(',', '', poker$stack2))
poker$stack3 <- as.numeric(gsub(',', '', poker$stack3))
poker$stack4 <- as.numeric(gsub(',', '', poker$stack4))
poker$stack5 <- as.numeric(gsub(',', '', poker$stack5))
poker$stack6 <- as.numeric(gsub(',', '', poker$stack6))
poker$id <- as.numeric(substr(poker$id, 8, 17))
for (i in 1:nrow(poker)){
  poker$table[i] <- str_split(poker$table[i], ' \\(')[[1]][1]
}

poker1 <- poker
poker <- poker1

## creating start table
start <- poker[,c(1:14)]
colnames(start) <- c('id', 'table', 'd', 'd_stack', 'sb', 'sb_stack', 'bb',
                     'bb_stack', 'utg', 'utg_stack', 'p4', 'p4_stack', 'p5',
                     'p5_stack')
start <- start[,c(1,2,5:14,3,4)]

## creating action table
action <- poker[,c(1,15)] %>%
  add_column(folds_pocket = 0, folds_flop = 0, folds_turn = 0, folds_river = 0,
             checks_pocket = 0, checks_flop = 0, checks_turn = 0, checks_river = 0,
             calls_pocket = 0, calls_flop = 0, calls_turn = 0, calls_river = 0,
             bets_pocket = 0, bets_flop = 0, bets_turn = 0, bets_river = 0,
             raises_pocket = 0, raises_flop = 0, raises_turn = 0, raises_river = 0)

action <- action %>%
  separate(action, c('pocket', 'action'), sep = 'FLOP') %>%
  separate(action, c('flop', 'action'), sep = 'TURN') %>%
  separate(action, c('turn', 'river'), sep = 'RIVER')

for (i in 1:nrow(action)) {
  for (j in 2:5) {
    action[i,j+4] <- lengths(regmatches(action[i,j],
                                        gregexpr('Folds', action[i,j])))
    action[i,j+8] <- lengths(regmatches(action[i,j],
                                        gregexpr('Checks', action[i,j])))
    action[i,j+12] <- lengths(regmatches(action[i,j],
                                         gregexpr('Calls', action[i,j])))
    action[i,j+16] <- lengths(regmatches(action[i,j],
                                         gregexpr('Bets', action[i,j])))
    action[i,j+20] <- lengths(regmatches(action[i,j],
                                         gregexpr('Raises', action[i,j])))
  }
}

action_full <- action
action <- action[,c(1,6:25)]

## creating summary table
summary <- poker[,c(1,17)] %>%
  add_column(pot_board = NA, rest = NA)

for (i in 1:nrow(summary)){
  summary$pot_board[i] <- str_split(summary$summary[i],
                                    '\r\nSeat', n = 2)[[1]][1]
  summary$rest[i] <- paste0('Seat', str_split(summary$summary[i],
                                              '\r\nSeat', n = 2)[[1]][2])
}

summary <- summary %>%
  separate(pot_board, c('pot', 'board'), sep = '\r\n') %>%
  separate(rest, c(paste0('s', 1:6)), sep = '\r\n')

for (i in 1:nrow(summary)){
  summary$pot[i] <- str_split(summary$pot[i], '\\)', n = 2)[[1]][1]
}

summary <- subset(summary, select = -summary)
special_games <- summary$id[nchar(summary$pot) > 19]
summary$pot <- gsub(',', '', substr(summary$pot, 12,
                                    nchar(summary$pot)))
summary$pot[summary$id == 3034333436] <- 4421
summary$pot[summary$id == 3037849306] <- 105
summary$pot[summary$id == 3037896232] <- 2808.75
summary$pot[summary$id == 3037982442] <- 930.95
summary$pot[summary$id == 3055736828] <- 1035.75
summary$pot[summary$id == 3069842890] <- 1908.50
summary$pot[summary$id == 3072886242] <- 4917
summary$pot[summary$id == 3081541030] <- 2056
summary$pot[summary$id == 3086126762] <- 1068
summary$pot <- as.numeric(summary$pot)

summary$board <- substr(summary$board, 8, nchar(summary$board) - 1)

summary_action <- summary[,c(4:9)]
summary_action$s1 <- substr(summary_action$s1, 9, nchar(summary_action$s1))
summary_action$s2 <- substr(summary_action$s2, 9, nchar(summary_action$s2))
summary_action$s3 <- substr(summary_action$s3, 9, nchar(summary_action$s3))
summary_action$s4 <- substr(summary_action$s4, 9, nchar(summary_action$s4))
summary_action$s5 <- substr(summary_action$s5, 9, nchar(summary_action$s5))
summary_action$s6 <- substr(summary_action$s6, 9, nchar(summary_action$s6))

summary <- summary[,c(1:3)] %>%
  add_column(sb = NA, bb = NA, utg = NA, p4 = NA, p5 = NA, d = NA)

for (i in 1:nrow(summary)) {
  if (grepl(start$sb[i], summary_action$s1[i], fixed = T)) {
    summary$sb[i] <- summary_action$s1[i]
  } else if (grepl(start$bb[i], summary_action$s1[i], fixed = T)) {
    summary$bb[i] <- summary_action$s1[i]
  } else if (grepl(start$utg[i], summary_action$s1[i], fixed = T)) {
    summary$utg[i] <- summary_action$s1[i]
  } else if (grepl(start$p4[i], summary_action$s1[i], fixed = T)) {
    summary$p4[i] <- summary_action$s1[i]
  } else if (grepl(start$p5[i], summary_action$s1[i], fixed = T)) {
    summary$p5[i] <- summary_action$s1[i]
  } else if (grepl(start$d[i], summary_action$s1[i], fixed = T)) {
    summary$d[i] <- summary_action$s1[i]
  }
  
  if (grepl(start$sb[i], summary_action$s2[i], fixed = T)) {
    summary$sb[i] <- summary_action$s2[i]
  } else if (grepl(start$bb[i], summary_action$s2[i], fixed = T)) {
    summary$bb[i] <- summary_action$s2[i]
  } else if (grepl(start$utg[i], summary_action$s2[i], fixed = T)) {
    summary$utg[i] <- summary_action$s2[i]
  } else if (grepl(start$p4[i], summary_action$s2[i], fixed = T)) {
    summary$p4[i] <- summary_action$s2[i]
  } else if (grepl(start$p5[i], summary_action$s2[i], fixed = T)) {
    summary$p5[i] <- summary_action$s2[i]
  } else if (grepl(start$d[i], summary_action$s2[i], fixed = T)) {
    summary$d[i] <- summary_action$s2[i]
  }
  
  if (grepl(start$sb[i], summary_action$s3[i], fixed = T)) {
    summary$sb[i] <- summary_action$s3[i]
  } else if (grepl(start$bb[i], summary_action$s3[i], fixed = T)) {
    summary$bb[i] <- summary_action$s3[i]
  } else if (grepl(start$utg[i], summary_action$s3[i], fixed = T)) {
    summary$utg[i] <- summary_action$s3[i]
  } else if (grepl(start$p4[i], summary_action$s3[i], fixed = T)) {
    summary$p4[i] <- summary_action$s3[i]
  } else if (grepl(start$p5[i], summary_action$s3[i], fixed = T)) {
    summary$p5[i] <- summary_action$s3[i]
  } else if (grepl(start$d[i], summary_action$s3[i], fixed = T)) {
    summary$d[i] <- summary_action$s3[i]
  }
  
  if (grepl(start$sb[i], summary_action$s4[i], fixed = T)) {
    summary$sb[i] <- summary_action$s4[i]
  } else if (grepl(start$bb[i], summary_action$s4[i], fixed = T)) {
    summary$bb[i] <- summary_action$s4[i]
  } else if (grepl(start$utg[i], summary_action$s4[i], fixed = T)) {
    summary$utg[i] <- summary_action$s4[i]
  } else if (grepl(start$p4[i], summary_action$s4[i], fixed = T)) {
    summary$p4[i] <- summary_action$s4[i]
  } else if (grepl(start$p5[i], summary_action$s4[i], fixed = T)) {
    summary$p5[i] <- summary_action$s4[i]
  } else if (grepl(start$d[i], summary_action$s4[i], fixed = T)) {
    summary$d[i] <- summary_action$s4[i]
  }
  
  if (grepl(start$sb[i], summary_action$s5[i], fixed = T)) {
    summary$sb[i] <- summary_action$s5[i]
  } else if (grepl(start$bb[i], summary_action$s5[i], fixed = T)) {
    summary$bb[i] <- summary_action$s5[i]
  } else if (grepl(start$utg[i], summary_action$s5[i], fixed = T)) {
    summary$utg[i] <- summary_action$s5[i]
  } else if (grepl(start$p4[i], summary_action$s5[i], fixed = T)) {
    summary$p4[i] <- summary_action$s5[i]
  } else if (grepl(start$p5[i], summary_action$s5[i], fixed = T)) {
    summary$p5[i] <- summary_action$s5[i]
  } else if (grepl(start$d[i], summary_action$s5[i], fixed = T)) {
    summary$d[i] <- summary_action$s5[i]
  }
  
  if (grepl(start$sb[i], summary_action$s6[i], fixed = T)) {
    summary$sb[i] <- summary_action$s6[i]
  } else if (grepl(start$bb[i], summary_action$s6[i], fixed = T)) {
    summary$bb[i] <- summary_action$s6[i]
  } else if (grepl(start$utg[i], summary_action$s6[i], fixed = T)) {
    summary$utg[i] <- summary_action$s6[i]
  } else if (grepl(start$p4[i], summary_action$s6[i], fixed = T)) {
    summary$p4[i] <- summary_action$s6[i]
  } else if (grepl(start$p5[i], summary_action$s6[i], fixed = T)) {
    summary$p5[i] <- summary_action$s6[i]
  } else if (grepl(start$d[i], summary_action$s6[i], fixed = T)) {
    summary$d[i] <- summary_action$s6[i]
  }
}

summary$sb <- substr(summary$sb, 38, nchar(summary$sb))
summary$bb <- substr(summary$bb, 36, nchar(summary$bb))
summary$utg <- substr(summary$utg, 24, nchar(summary$utg))
summary$p4 <- substr(summary$p4, 24, nchar(summary$p4))
summary$p5 <- substr(summary$p5, 24, nchar(summary$p5))
summary$d <- substr(summary$d, 33, nchar(summary$d))

summary <- summary %>%
  add_column(end_stage = NA, winner = NA)

summary$end_stage <- ifelse(is.na(action_full$flop), 'pocket',
                            ifelse(is.na(action_full$turn), 'flop',
                                   ifelse(is.na(action_full$river), 'turn',
                                          'river')))

for (i in 1:nrow(summary)) {
  for (j in 4:9) {
    if (grepl('collected|won', summary[i,j])) {
      summary$winner[i] <- colnames(summary)[j]
      summary[i,j] <- 'Won'
    }
    if (grepl('Mucked', summary[i,j])) {
      summary[i,j] <- 'Folded on the RIVER'
    }
    if (grepl('lost', summary[i,j])) {
      summary[i,j] <- 'Lost'
    }
  }
}

summary$board <- ifelse(is.na(summary$board), '', summary$board)

# joining tables
poker_wide <- start %>%
  full_join(action, by = 'id') %>%
  full_join(summary, by = 'id', suffix = c('_name', '_end'))

# converting to long format
summary_long <- gather(summary, player, result, sb:d, factor_key=T)

poker_long <- gather(poker_wide, player, result, sb_end:d_end, factor_key=T)

# removing NA
holdem <- na.omit(poker_wide)
holdem$winner <- as.factor(holdem$winner)
holdem$sb_win <- as.factor(ifelse(holdem$winner == 'sb', 'yes', 'no'))
holdem$bb_win <- as.factor(ifelse(holdem$winner == 'bb', 'yes', 'no'))
holdem$utg_win <- as.factor(ifelse(holdem$winner == 'utg', 'yes', 'no'))
holdem$p4_win <- as.factor(ifelse(holdem$winner == 'p4', 'yes', 'no'))
holdem$p5_win <- as.factor(ifelse(holdem$winner == 'p5', 'yes', 'no'))
holdem$d_win <- as.factor(ifelse(holdem$winner == 'd', 'yes', 'no'))

# random forest analysis
set.seed(73)
split <- sample.split(1:nrow(holdem), SplitRatio = 0.75)
train <- subset(holdem, split == T)
test <- subset(holdem, split == F)

rf.holdem <- randomForest(winner ~ table + sb_stack + bb_stack + utg_stack +
                            p4_stack + p5_stack + d_stack,
                          data = train, importance = T)

holdem.win_pred <- predict(rf.holdem, newdata = test)
holdem.cm <- table(test$winner, holdem.win_pred)
sum(holdem.cm)
correct <- 0
for (i in 1:6) {
  correct <- correct + holdem.cm[i,i]
}
correct / sum(holdem.cm)

holdem.cm
