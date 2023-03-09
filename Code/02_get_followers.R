j = commandArgs(trailingOnly=TRUE)
# if j is list take first element
if (is.list(j)) {
    j = as.numeric(j[[1]])
}else{
    j = as.numeric(j)
}
j = j[2]

library(tidyverse)
library(lubridate)
library(academictwitteR)
library(parallel)
library(foreach)
library(doParallel)

# import all functions from 02_util_get_followers.R
setwd("/Users/dorianquelle/Desktop/zurich/TwitterViews/Code")
source("02_util_get_followers.R")

# Set up API Tokens
# Read tokens.txt
tokens <- readLines("../tokens.txt")
# Ignore lines that start with #
tokens <- tokens[!grepl("^#", tokens)]
# check for duplicates
if (length(tokens) != length(unique(tokens))){
    print("Duplicate tokens found!")
    print("Removing duplicates...")
    # Print prior and post length
    print(paste("Prior length:", length(tokens)))
    tokens <- unique(tokens)
    print(paste("Post length:", length(tokens)))
}

#token_valid <- rep(FALSE, length(tokens))
## check if token works 
#for(i in 1:length(tokens)){
#    tryCatch({
#        a <- get_user_followers(x = "1336014519160758278", bearer_token = tokens[i], verbose = FALSE)
#        token_valid[i] <- TRUE
#        cat("\r", i, "/", length(tokens), " tokens valid")
#    }, error = function(e){
#        tokens_valid[i] <- FALSE
#        cat("\r", i, "/", length(tokens), " tokens NOT valid")
#    })
#}
## Remove invalid tokens (Print Prior and Post length)
#print("Removing invalid tokens...")
print(paste("Prior length:", length(tokens)))
#tokens <- tokens[token_valid]
#print(paste("Post length:", length(tokens)))
tokens <- tokens[seq(j, length(tokens), 5)]
print(paste("Post length:", length(tokens)))

# Get followers of Media Accounts
media_accs <- read.csv("../Mediadatabases/small_Clean_MediaRatingTwitter.csv")
media_accs <- media_accs[seq(j, nrow(media_accs), 5), ]

# Filter media_accs to only include accounts with Twitter.ID
# Create 5 groups of tokens
# Each group will be used by a worker
token_grouped <- split(tokens, 1:length(tokens) %% 5)

get_followers <- function(handle, tokens){
    id <- get_user_id(handle, bearer_token = tokens[1])
        # Print random number to check if function is running
        print(paste("Start Time:", Sys.time(), "for", handle))
        tryCatch({
            # Check if directory exists 
            # If it does, skip
            if(!dir.exists(paste0("../Followers/Media/",handle,"/"))){ 
                followers <- get_user_followers_pimped(
                            x = id,
                            bearer_token = tokens
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

for(i in 1:nrow(media_accs)){
    get_followers(media_accs$Twitter.Handle[i], tokens)
}


# Spawn a pool of n workers
# Where n is the number of tokens that we have
# Get the number of tokens
#n_token <- length(token_grouped)
## Create Test Run
#
## Create a list of arguments for each worker
#args <- lapply(1:nrow(media_accs), function(i){
#    list(handle = media_accs$Twitter.Handle[i])
#})
#
## Split args into n_token subsets
#args_list <- split(args, rep(1:n_token, length.out = length(args)))
#for(i in 1:n_token){
#    for(j in 1:length(args_list[[i]])){
#        args_list[[i]][[j]]$token <- unlist(token_grouped[i])
#    }
#}

# Define a function to run get_followers on each element of args
#run_get_followers <- function(arg) {
#    # assert that id is a character, handle is a character, and token is a list
#    tryCatch({
#          get_followers(arg[[1]]$handle, arg[[1]]$token)
#    }, error = function(e){
#        print(paste("Error in run_get_followers", arg[[1]]$handle,"at", Sys.time()))
#        print(paste("Error",e))
#        # Print current time
#    })
#}

# Set up a parallel backend
#cl <- makeCluster(c("localhost", "localhost", "localhost","localhost","localhost"), type = "PSOCK",outfile = paste0(getwd(),"/MultiProcessingLog.txt"))
#clusterExport(cl, c("run_get_followers", "get_followers","get_user_followers","get_user_followers_pimped","get_user_edges_pimped","get_user_id", "make_query",".check_header_rate_limit","add_context_annotations", "add_query_prefix", ".process_qparam", ".trigger_sleep", ".check_reset",".vwarn",".vcat",".gen_random_dir"))
#clusterApply(cl, args_list, run_get_followers)
#
## Close the cluster
#stopCluster(cl)




#media_accs$Twitter.Handle <- str_replace_all(media_accs$Twitter.Handle, "@", "")
#
##Check if Twitter.ID is in column names
#if(!"Twitter.ID" %in% colnames(media_accs)){
#    media_accs$Twitter.ID <- NA
#    for(i in 1:nrow(media_accs)){
#        if(is.na(media_accs[i, "Twitter.ID"])){
#            media_accs[i, "Twitter.ID"] <- get_user_id(media_accs[i, "Twitter.Handle"])
#        }
#    cat("\r", i, "/", nrow(media_accs), " Twitter.IDs added")
#    }
#}
## save the file
#write.csv(media_accs, "../Mediadatabases/small_Clean_MediaRatingTwitter.csv", row.names = FALSE)

