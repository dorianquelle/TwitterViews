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
  tryCatch({
    if(!dir.exists(paste0("../Data/",handle,"/"))){
        tweets <- get_all_tweets(
                    users = handle,
                    start_tweets = "2022-11-01T00:00:00Z",
                    end_tweets = "2023-03-07T00:00:00Z",
                    data_path = paste0("../Data/",handle,"/"),
                    n = Inf
                  )
      } else {
          print(paste0("Directory for ", handle, " already exists."))
          print(paste0("Skipping ", handle, "."))
     } 
    },
  error = function(e) {
    print(paste("Error for", handle))
    print(paste("Error",e))
  })
}

# Repeat for State Media "../Data/Clean_StateMediaTwitter.csv"
state <- read.csv("../Mediadatabases/Clean_StateMediaTwitter.csv")
handles <- state$Twitter.Handle
# Remove leading or trailing whitespace
handles <- str_trim(handles)

for(i in 1:nrow(state)){
  tryCatch({
  print(paste0("Downloading Tweets for ", handles[i], ".", " Account: ", i, "/", nrow(state)))
  print(paste("Start Time:", Sys.time()))
  if(!dir.exists(paste0("../StateMedia/",handles[i],"/"))){
      tweets <- get_all_tweets(
                  users = handles[i],
                  start_tweets = "2022-11-01T00:00:00Z",
                  end_tweets = "2023-03-07T00:00:00Z",
                  data_path = paste0("../StateMedia/",handles[i],"/"),
                  n = Inf
                )
    } else {
        print(paste0("Directory for ", handles[i], " already exists."))
    }}, error = function(e) {
    print(paste("Error for", handle))
    print("Error",e)
    }
  )
  }

