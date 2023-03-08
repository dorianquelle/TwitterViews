library(tidyverse)
library(lubridate)
library(academictwitteR)
library(parallel)

# Set up API Tokens
# Read tokens.txt
tokens <- readLines("../tokens.txt")
# Get followers of Media Accounts
media_accs <- read.csv("../Mediadatabases/Clean_MediaRatingTwitter.csv")
media_accs$Twitter.Handle <- str_replace_all(media_accs$Twitter.Handle, "@", "")


get_followers <- function(id, handle, token){
        # Print random number to check if function is running
        print(paste("Start Time:", Sys.time(), "for", handle, runif(1, 0, 100)))
        tryCatch({
            # Check if directory exists 
            # If it does, skip
            if(!dir.exists(paste0("../Followers/Media/",handle,"/"))){ 
                followers <- get_user_followers(
                            x = id,
                            bearer_token = token
                            )

                # Add Twitter Handle and Twitter ID to followers
                followers["Twitter.Handle"] <- as.character(handle)
                followers["Twitter.ID"] <- as.character(id)

                if(!dir.exists(paste0("../Followers/Media/",handle,"/"))){
                    dir.create(paste0("../Followers/Media/",handle,"/"))
                }

                saveRDS(followers, paste0("../Followers/Media/",handle,"/",handle,"_followers.rds"))

            } else {
            # This is invoked if the directory already exists; Skip!
            print(paste0("Directory for ", handle, " already exists."))
            print(paste0("Skipping ", handle, "."))
            return 
                            }            
                },
        error = function(e) {
            print(paste("Error for", handle))
            print(paste("Error",e))
            return
        })
        return 
}


# Spawn a pool of n workers
# Where n is the number of tokens that we have
# Get the number of tokens
n_token <- length(tokens)
# Create Test Run

# Create a list of arguments for each worker
args <- lapply(1:nrow(media_accs), function(i){
    list(id = media_accs$Twitter.ID[i],
         handle = media_accs$Twitter.Handle[i])
})

# Split args into n_token subsets
args_list <- split(args, rep(1:n_token, length.out = length(args)))
for(i in 1:n_token){
    for(j in 1:length(args_list[[i]])){
        args_list[[i]][[j]]$token <- tokens[i]
    }
}

# Define a function to run get_followers on each element of args
run_get_followers <- function(arg) {
  get_followers(arg[[1]]$id, arg[[1]]$handle, arg[[1]]$token)
}

# Create a cluster with three nodes
cl <- makeCluster(n_token, outfile = "/Users/dorianquelle/Desktop/zurich/TwitterViews/Code/MultiProcessingLog.txt")
clusterExport(cl, c("run_get_followers", "get_followers","get_user_followers"))
# Run run_get_followers on each subset of args in parallel
results <- parLapplyLB(cl = cl, args_list, run_get_followers)

# Stop the cluster
stopCluster(cl)

