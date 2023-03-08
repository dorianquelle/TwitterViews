# List Dirs in Followers/Media/*
library(tidyverse)
library(ggplot2)
library(readr)

clean_rds <- function(path) {
    followers <- readRDS(path)
    followers <- cbind(followers, followers$public_metrics %>% unnest())
    followers <- followers %>% select(-public_metrics, -entities)
    followers$id <- as.character(followers$id)
    followers$follow_order <- nrow(followers):1
    followers$created_at <- as.POSIXct(followers$created_at, format = "%Y-%m-%dT%H:%M:%S.000Z")
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
            clean_rds(y)
        } 
    })
})



