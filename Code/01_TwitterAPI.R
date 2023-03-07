# Load libraries
install.packages("academictwitteR")

library(academictwitteR)
library(stringr)
library(openxlsx)

# Load Data # # # # #
#   Still Missing   #
#                   #
# # # # # # # # # # #

# Get Twitter IDs of media accounts
twitter_ids <- get_user_id(
  media_accs$Twitter.Handle,
  bearer_token = get_bearer(),
  all = FALSE,
  keep_na = TRUE
)

twitter_ids <- data.frame(twitter_ids)
twitter_ids$handle <- rownames(twitter_ids)

for(i in 1:nrow(twitter_ids)){
  handle <- twitter_ids$handle[i]
  id <- twitter_ids$twitter_ids[i]

  tweets <-
    get_user_timeline(
      x = id,
      start_tweets = "2022-11-01T10:00:00Z",
      end_tweets = "2023-02-08T10:00:00Z",
      data_path = paste0("../Data/",User,"/")
      n = Inf,
    )
}

