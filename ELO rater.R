library(readxl)
library(xlsx)

# imports ratings table
ratings <- read_excel("ratings.xlsx", sheet = "ELO", col_types = c("numeric", "text", "numeric", "numeric", "numeric", "numeric"))

# Picks a random number between 1 and total number of movies
movA <- as.numeric(sample(1:length(ratings$Number), 1))

# identifies the ELO of the random movie chosen
movA_ELO_old <- as.numeric(ratings[movA,"ELO"])

# picks a second movie from the subset of movies that are within 250 ELO points of movA
movB_options <- subset(ratings, (ratings$ELO > (movA_ELO_old - 250)) & ratings$ELO < (movA_ELO_old + 250))
movB <- as.numeric(sample(movB_options$Number, 1))

# Finds the second movies identifier number and ELO rating
movB_ELO_old <- as.numeric(ratings[movB, "ELO"])

# calculates the expected win probabilty for each movie
EA <- 1/(1+10^((movB_ELO_old - movA_ELO_old)/400))
EB <- 1/(1+10^((movA_ELO_old - movB_ELO_old)/400))

# asks user to choose between two movies
result <- readline(prompt= paste("A.", ratings[movA, "Title"], ratings[movA, "Year"], "versus", "B.", ratings[movB, "Title"], ratings[movB, "Year"], "? "))

# depending on result, calculates new ELO ratings
if (result == "A") {
  movA_ELO_new <- movA_ELO_old + 32 * (1 - EA)
  movB_ELO_new <- movB_ELO_old + 32 * (0 - EB)
} else if (result == "B") {
  movA_ELO_new <- movA_ELO_old + 32 * (0 - EA)
  movB_ELO_new <- movB_ELO_old + 32 * (1 - EB)
} else if (result == "D") {
  movA_ELO_new <- movA_ELO_old + 32 * (.5 - EA)
  movB_ELO_new <- movB_ELO_old + 32 * (.5 - EB)
} else {
 stop
}

# Updates the ELO value in the ratings table
ratings[movA,"ELO"] <- movA_ELO_new
ratings[movB,"ELO"] <- movB_ELO_new

# Adds one to the counter for both movies

ratings[movA, "Counter"] <- ratings [movA, "Counter"] + 1
ratings[movB, "Counter"] <- ratings [movB, "Counter"] + 1

# Exports new data to excel
ratings <- as.data.frame(ratings)
write.xlsx(ratings, "ratings.xlsx", sheetName = "ELO", row.names = FALSE)




