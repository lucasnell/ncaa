

library(tidyverse)

foo <- function(x, pos) {
    seed <- {str_replace(x[1], "Overall tournament record of #", "") %>% 
            str_replace("^\\s+", "") %>% 
            str_split("\\s+")}[[1]][1] %>% 
        as.integer
    win_loss <- x[2:length(x)] %>% 
        str_replace("%", "") %>% 
        str_split("\\s+") %>% 
        keep(~ length(.x) > 1) %>% 
        map(~ {tail(.x, 2)[1] %>% 
                str_replace_all("\\(|\\)", "") %>% 
                str_split("-")}[[1]] %>% 
                as.integer) %>% 
        do.call(what = rbind)
    if (nrow(win_loss) != 16) stop("Win-loss matrix has incorrect number of rows")
    data_frame(seed = seed, opp_seed = 1:16, wins = win_loss[,1], losses = win_loss[,2])
}



win_loss <- read_lines_chunked("history.txt", callback = DataFrameCallback$new(foo), 
                               chunk_size = 19) %>% 
    mutate(p = wins / (losses + wins),
           p = ifelse(p == 0, 1/250, p),
           p = ifelse(p == 1, 1 - 1/250, p),
           p = ifelse(is.na(p), 0.5, p))

winm <- matrix(NA, 16, 16)
lossm <- matrix(NA, 16, 16)
for (i in 1:16) {
    for (j in i:16) {
        winm[i,j] <- win_loss$wins[win_loss$seed == i & win_loss$opp_seed == j]
        lossm[i,j] <- win_loss$losses[win_loss$seed == i & win_loss$opp_seed == j]
        winm[j,i] <- win_loss$wins[win_loss$seed == j & win_loss$opp_seed == i]
        lossm[j,i] <- win_loss$losses[win_loss$seed == j & win_loss$opp_seed == i]
    }
}

pmat <- winm / (winm + lossm)
# pmat[,16] <- 

pmat[16,] <- (1:16 - 1) / 30
pmat[,16] <- 1 - (1:16 - 1) / 30
pmat[1,14:15] <- 1
# Interpolate missing values across columns:
for (i in 1:ncol(pmat)) {
    f <- approxfun(pmat[,i])
    for (j in which(is.nan(pmat[,i]))) {
        pmat[j,i] <- f(j)
    }
}
# 50% chance against same seed
diag(pmat) <- 0.5
# Making no p == 1 or 0
pmat[pmat == 0] <- 1/250
pmat[pmat == 1] <- 1 - 1/250



# Returns which of x wins
sim_win <- function(x) {
    prob  <- pmat[x[1],x[2]]
    winner <- rbinom(1,1,prob)
    if (winner == 1) {
        return(x[1])
    } else {
        return(x[2])
    }
}


sim_location <- function() {
    all_rounds <- replicate(4, NULL)
    round_num <- 1
    round0 <- lapply(c(1,8, 5,4, 6,3, 7,2), function(x) c(x, 17-x)) %>% 
        do.call(what = rbind)
    round1 <- numeric(nrow(round0))
    while (length(round1) > 1) {
        round1 <- numeric(nrow(round0))
        j = 1
        for (i in 1:nrow(round0)) {
            winner <- sim_win(round0[i,])
            round1[j] <- winner
            j = j + 1
        }
        round0 <- matrix(round1, length(round1)/2,2, byrow = TRUE)
        all_rounds[[round_num]] <- round1
        round_num <- round_num + 1
    }
    return(all_rounds)
}


sim_brackets <- function(){
    bracks <- replicate(4, sim_location(), simplify = FALSE)
    names(bracks) <- c("South", "West", "East", "Midwest")
    p <- pmat[bracks[[1]][[4]], bracks[[2]][[4]]]
    if (rbinom(1,1,p) == 1) {
        last_round <- c(bracks[[1]][[4]])
        names(last_round) <- names(bracks)[1]
    } else {
        last_round <- c(bracks[[2]][[4]])
        names(last_round) <- names(bracks)[2]
    }
    
    p <- pmat[bracks[[3]][[4]], bracks[[4]][[4]]]
    if (rbinom(1,1,p) == 1) {
        last_round <- c(last_round, c = bracks[[3]][[4]])
        names(last_round)[2] <- names(bracks)[3]
    } else {
        last_round <- c(last_round, d = bracks[[4]][[4]])
        names(last_round)[2] <- names(bracks)[4]
    }
    
    
    p <- pmat[last_round[1], last_round[2]]
    if (rbinom(1,1,p) == 1) {
        champ <- names(last_round)[1]
    } else {
        champ <- names(last_round)[2]
    }
    champ
    
    return(list(first = bracks, second = last_round, champ = champ))
}


sim_brackets()
