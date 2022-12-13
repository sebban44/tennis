library(dplyr)
library(zoo)

# Assign probability of serve and return for p1 and p2
p_s1 <- .6
p_r1 <- .4
p_s2 <- .6
p_r2 <- .4

# Calculate probability of winning serve,return and tiebreak game - https://chewthestat.com/finding-the-winning-formula/
serve_game <- function(s) {
  return(s^4 + 4 * s^4 * (1 - s) + 10 * s^4 * (1 - s)^2 + (20 * s^5 * (1 - s)^3) / (1 - 2 * s + 2 * s^2))
}

return_game <- function(r) {
  return(r^4 + 4 * r^4 * (1 - r) + 10 * r^4 * (1 - r)^2 + (20 * r^5 * (1 - r)^3) / (1 - 2 * r + 2 * r^2))
}

tb_game <- function(s, r) {
  t <- 0.5 * s + 0.5 * r
  return(t^7 + 7 * t^7 * (1 - t) + 28 * t^7 * (1 - t)^2 + 84 * t^7 * (1 - t)^3 + 210 * t^7 * (1 - t)^4 + 462 * t^7 * (1 - t)^5 + (924 * t^8 * (1 - t)^6) / (1 - 2 * t + 2 * t^2))
}

pA <- serve_game(p_s1)
qA <- return_game(p_r1)

pB <- serve_game(p_s2)
qB <- return_game(p_r2)

tA <- tb_game(p_s1, p_r1)
tB <- 1 - tA

# Randomise a coin toss to set who serves first
coin_toss <- runif(1)
coin_w <- ifelse(coin_toss < 0.5, 1, 0)

# Simulate a set
set <- function(pA, pB, c_t) {
  g <- 0
  gA <- 0
  gB <- 0

  while ((max(gA, gB) < 6 || abs(gA - gB) < 2) && gA + gB < 12) {
    g <- g + 1

    if (c_t == 1) {
      if (g %% 2 == 0) {
        if (runif(1) < pA) {
          gA <- gA + 1
        } else {
          gB <- gB + 1
        }
      }

      if (g %% 2 == 1) {
        if (runif(1) < pB) {
          gB <- gB + 1
        } else {
          gA <- gA + 1
        }
      }

      if (gA == 6 && gB == 6) {
        g <- g + 1
        if (runif(1) < tA) {
          gA <- gA + 1
          gB <- gB + 0
        } else {
          gA <- gA + 0
          gB <- gB + 1
        }
      }
    }

    if (c_t == 0) {
      if (g %% 2 == 0) {
        if (runif(1) < pB) {
          gB <- gB + 1
        } else {
          gA <- gA + 1
        }
      }

      if (g %% 2 == 1) {
        if (runif(1) < pA) {
          gA <- gA + 1
        } else {
          gB <- gB + 1
        }
      }

      if (gA == 6 && gB == 6) {
        g <- g + 1
        if (runif(1) < tB) {
          gB <- gB + 1
        } else {
          gA <- gA + 1
        }
      }
    }
  }

  return(c(g, gA, gB))
}

# Reason for setting up a set_2 function is because we are assuming p1 is serving first - this function takes p2 as first server
# A better approach would be to randomise who is serving first and then just calling a function for winning set


sim_match <- function(pA, pB, c_t) {
  # we play 2 sets
  sim_set_1 <- set(pA, pB, coin_w)
  sim_set_2 <- set(pA, pB, coin_w)

  # if tied we play a 3rd set & checks coin for who should start serve 3rd set
  if (sim_set_1[2] > sim_set_1[3] && sim_set_2[2] < sim_set_2[3] || sim_set_1[2] < sim_set_1[3] && sim_set_2[2] > sim_set_2[3]) {
    if (c_t == 1) {
      sim_set_3 <- set(pA, pB, 1)
    } else {
      sim_set_3 <- set(pA, pB, 0)
    }
    result <- data.frame(
      Set_1_Games = sim_set_1[1],
      Set_1_A = sim_set_1[2],
      Set_1_B = sim_set_1[3],
      Set_2_Games = sim_set_2[1],
      Set_2_A = sim_set_2[2],
      Set_2_B = sim_set_2[3],
      Set_3_Games = sim_set_3[1],
      Set_3_A = sim_set_3[2],
      Set_3_B = sim_set_3[3]
    )
    return(result)
  } else {
    result <- data.frame(
      Set_1_Games = sim_set_1[1],
      Set_1_A = sim_set_1[2],
      Set_1_B = sim_set_1[3],
      Set_2_Games = sim_set_2[1],
      Set_2_A = sim_set_2[2],
      Set_2_B = sim_set_2[3],
      Set_3_Games = NA,
      Set_3_A = NA,
      Set_3_B = NA
    )
    return(result)
  }
}

#Simulate matches 100k times
n_sims <- 100000
sim <- do.call(rbind, replicate(n_sims, coredata(sim_match(pA, pB, coin_w)), simplify = FALSE))

sim <- sim %>%
  rowwise() %>%
  mutate(
    Total_Set_1 = sum(Set_1_A, Set_1_B),
    Total_Games_A = sum(Set_1_A, Set_2_A, Set_3_A, na.rm = TRUE),
    Total_Games_B = sum(Set_1_B, Set_2_B, Set_3_B, na.rm = TRUE),
    Game_Diff_A = Total_Games_A - Total_Games_B,
    Game_Diff_B = Total_Games_B - Total_Games_A,
    Total_Games = Total_Games_A + Total_Games_B,
    W_Set_1_A = ifelse(Set_1_A > Set_1_B, 1, 0),
    W_Set_1_B = ifelse(Set_1_B > Set_1_A, 1, 0),
    W_Set_2_A = ifelse(Set_2_A > Set_2_B, 1, 0),
    W_Set_2_B = ifelse(Set_2_B > Set_2_A, 1, 0),
    W_Set_3_A = ifelse(Set_3_A > Set_3_B, 1, 0),
    W_Set_3_B = ifelse(Set_3_B > Set_3_A, 1, 0),
    Set_A = sum(W_Set_1_A, W_Set_2_A, W_Set_3_A, na.rm = TRUE),
    Set_B = sum(W_Set_1_B, W_Set_2_B, W_Set_3_B, na.rm = TRUE),
    Total_Sets = ifelse((Set_A + Set_B) > 2, 3, 2),
    Winner = ifelse(Set_A > Set_B, 1, 0)
  )

#Example
#Distribution of total games in a match
hist(sim$Total_Games)


