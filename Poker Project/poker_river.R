# setup
install.packages('tidyr')
install.packages('dplyr')
install.packages('data.table')
install.packages('stringr')
install.packages('tibble')

library('tidyr')
library('dplyr')
library('data.table')
library('stringr')
library('tibble')

setwd('/Users/JoshBlank/poker1')

# importing and cleaning data
poker_colnames = c('game', 'table', 'pocket', 'flop', 'turn', 'river',
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
  tbl1 <- tbl1[grepl(pattern = '\\*\\*\\* RIVER \\*\\*\\*', tbl1$value),]
  tbl1 <- tbl1[!grepl(pattern = '1 on 1', tbl1$value),]
  tbl1 <- tbl1 %>%
    separate(value, c('game', 'rest'), sep = '\r\nTable: ', extra = 'merge') %>%
    separate(rest, c('table', 'rest'),
             sep = '\\*\\*\\* POCKET CARDS \\*\\*\\*\r\n', extra = 'merge') %>%
    separate(rest, c('pocket', 'rest'),
             sep = '\\*\\*\\* FLOP \\*\\*\\* ', extra = 'merge') %>%
    separate(rest, c('flop', 'rest'),
             sep = '\\*\\*\\* TURN \\*\\*\\* ') %>%
    separate(rest, c('turn', 'rest'),
             sep = '\\*\\*\\* RIVER \\*\\*\\* ', extra = 'merge') %>%
    separate(rest, c('river', 'rest'),
             sep = '\\*\\*\\* SHOW DOWN \\*\\*\\*\r\n', extra = 'merge') %>%
    separate(rest, c('show down', 'summary'),
             sep = '\\*\\*\\* SUMMARY \\*\\*\\*\r\n')
  poker <- rbind(poker, tbl1)
}

poker <- poker[-3266,]
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
poker <- subset(poker, select = -table)
poker$game <- as.numeric(substr(poker$game, 8, 17))
colnames(poker)[1] <- 'id'

poker1 <- poker
poker <- poker1

# creating new tables

## creating start table
start <- poker[,c(1:13)]
colnames(start) <- c('id', 'd', 'd_stack', 'sb', 'sb_stack', 'bb', 'bb_stack',
                     'utg', 'utg_stack', 'p4', 'p4_stack', 'p5', 'p5_stack')
start <- start[,c(1,4:13,2,3)]

## creating summary table
summary <- poker[,c(1,19)]
summary <- summary %>%
  separate(summary, c('pot', 'board', paste0('s', 1:6)), sep = '\r\n',
           remove = T) %>%
  separate(pot, c('pot', 'trash'), sep = '\\) \\| ')

summary <- subset(summary, select = -trash)
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
colnames(summary)[1] <- 'id'

for (i in 1:2683) {
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

