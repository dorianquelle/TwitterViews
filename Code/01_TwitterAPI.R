# Load libraries
# Print Current Working Directory
# set Working Directory to Code

install.packages("academictwitteR")
library(academictwitteR)
library(stringr)
library(openxlsx)

# Load "../Data/Clean_MediaRatingTwitter.csv"
media_accs <- read.csv("../Data/Clean_MediaRatingTwitter.csv")
# Remove "@" from Twitter handles
media_accs$Twitter.Handle <- str_replace_all(media_accs$Twitter.Handle, "@", "")
media_accs$Twitter.ID <- NA

# Sequentially get Twitter IDs for each media account
for(i in 1:nrow(media_accs)){
  tryCatch({
    handle <- media_accs$Twitter.Handle[i]
    id <- get_user_id(handle, bearer_token = get_bearer())
    # Print which handle is being processed
    print(paste("Processing", handle))
    media_accs$Twitter.ID[i] <- id
  }, error = function(e) {
    print(paste("Error for", handle))
    print("Error",e)
  })
}

for(i in 1:nrow(media_accs)){
  handle <- media_accs$Twitter.Handle[i]
  id <- media_accs$Twitter.ID[i]

  # Download all Tweets for each media account Between 2022-11-01 and 2023-03-08
  print(paste0("Downloading Tweets for ", handle, " with ID ", id, ".", " Account: ", i, "/", nrow(media_accs)))
  print(paste("Start Time:", Sys.time()))

  # Check whether "../Data/{handle}/" exists
  if(!dir.exists(paste0("../Data/",handle,"/"))){
    tweets <- get_user_timeline(
                x = id,
                start_tweets = "2022-11-01T00:00:00Z",
                end_tweets = "2023-03-08T00:00:00Z",
                data_path = paste0("../Data/",handle,"/"),
                n = Inf
              )
  } else {
     print(paste0("Directory for ", handle, " already exists."))
     print(paste0("Skipping ", handle, "."))
  }
}

