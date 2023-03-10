# List Dirs in Followers/Media/*
setwd("./Code")
library(tidyverse)
library(ggplot2)
library(readr)

clean_rds <- function(path) { 
    followers <- readRDS(path)
    followers <- cbind(followers, followers$public_metrics %>% unnest())
    followers <- followers %>% select(-"public_metrics", -"entities")
    # If withheld is in data frame, remove it
    if ("withheld" %in% colnames(followers)) {
        followers <- followers %>% select(-"withheld")
    }
    followers$id <- as.character(followers$id)
    followers$follow_order <- nrow(followers):1
    format = "%Y-%m-%dT%H:%M:%S.000Z"
    followers$created_at <- as.POSIXct(followers$created_at, format)
    # Write CSV to New Folder Media_Clean
    new_path <- paste0(str_replace(str_replace(path, ".rds", ".csv"),"Media","Media_clean"),".gz") %>% 
                strsplit(., "/") %>% unlist %>% .[-4] %>% paste0(.,collapse = "/")
    write_csv(followers, new_path)
    return
}

# Get all directories in Followers/Media
users = list.dirs("../Followers/Media/", recursive = FALSE, full.names = FALSE)
# Lapply clean_rds on all files that are not hidden
lapply(users, function(x) {
    files = list.files(paste0("../Followers/Media/", x), full.names = TRUE)
    lapply(files, function(y) {
        if (substr(x, 1, 1) != ".") {
            new_path <- paste0(str_replace(str_replace(y, ".rds", ".csv"),"Media","Media_clean"),".gz") %>% 
                strsplit(., "/") %>% unlist %>% .[-4] %>% paste0(.,collapse = "/")
            if(!file.exists(new_path)){
                clean_rds(y)    
            }
        }
    })
})

# Create all follower-order plots
for(file in list.files("../Followers/Media_clean/")){
    # Check whether png already exists
    if("rds" %in% file | "gz" %in% file){
        print(paste0("File malformed: ", file))
        next
    }
    path <- paste0("../Results/FollowerOrder/",str_replace(file,".csv.gz",".png"))
    if(file.exists(path)){
        print(paste0("File already exists: ", file))
        next
    }
    # Make load silent
    tryCatch({
        suppressMessages(followers <- read_csv(paste0("../Followers/Media_clean/",file)))
        followers <- followers %>% filter(created_at > "2005-01-01" & created_at < "2023-04-01")
        print(paste("Successfully read file:", file))

        followers <- followers %>% distinct(id, .keep_all = TRUE)
        # Create new follower_order2
        followers$follow_order2 <- nrow(followers):1

        p <- ggplot(data = followers, aes(x = follow_order2, y = created_at))+
        geom_bin_2d(bins = 250)+
        theme_bw()+
        theme(legend.position = "none", 
                legend.margin=margin(0,0,0,0),
                legend.box.margin=margin(-5,-5,0,0))+
        xlab("Follower Order")+
        ylab("Creation Date")+
        scale_fill_viridis_c()

        ggsave(
            filename = path, 
            plot = p,
            width = 5,
            height = 5,
            dpi = 150
            )
        print(paste0("Successfully saved plot for: ", file))
    }, error = function(e) {
        print(paste0("Error in file: ", file))
    })
}
