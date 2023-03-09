# List Dirs in Followers/Media/*
setwd("./Code")
library(tidyverse)
library(ggplot2)
library(readr)

clean_rds <- function(path) { # nolint: object_usage_linter.
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
            clean_rds(y)
        }
    })
})
library(ggplot2)
# Create all follower-order plots
for(file in list.files("../Followers/Media_clean/")){
    # Make load silent
    suppressMessages(followers <- read_csv(paste0("../Followers/Media_clean/",file)))
    followers <- followers %>% filter(created_at > "2005-01-01" & created_at < "2023-04-01")
    print(paste("Successfully read file:", file))

    p <- ggplot(data = followers, aes(x = follow_order, y = created_at))+
      geom_bin_2d(bins = 250)+
      theme_bw()+
      theme(legend.position = "none", 
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-5,-5,0,0))+
      xlab("Follower Order")+
      ylab("Creation Date")+
      scale_fill_viridis_c()
    path <- paste0("../Results/FollowerOrder/",str_replace(file,".csv.gz",".png"))
    ggsave(
        filename = path, 
        plot = p,
        width = 10,
        height = 10,
        dpi = 300
        )
    print(paste0("Successfully saved plot for: ", file))
}

# Load YahooNews_followers.csv.gz
followers <- read_csv("../Followers/Media_clean/YahooNews_followers.csv.gz")
# Filter out all users that were created before 2005
followers <- followers %>% filter(created_at > "2005-01-01" & created_at < "2023-04-01")
# Create a plot
followers %>%
mutate(followers_col = ifelse(followers_count == 0, "red", "black")) %>%
ggplot(data = , aes(x = follow_order, y = created_at))+
geom_point(aes(col = followers_col), alpha = 0.1, cex = 0.001)+
  theme_bw()+
  theme(legend.position = "none", 
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-5,-5,0,0))+
  xlab("Follower Order")+
  ylab("Creation Date")+
  scale_fill_viridis_c()+
  xlim(500000,NA)

