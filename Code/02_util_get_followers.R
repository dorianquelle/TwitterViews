get_user_followers_pimped <- function(x, bearer_tokens){
  get_user_edges_pimped(x = x, wt = "followers", bearer_tokens)
}

get_user_edges_pimped <- function(x, bearer_tokens, wt, verbose = TRUE){  
  if(!dir.exists(paste0(".temp/",x[i],"/"))){ # see bind_rows comment below. 
        dir.create(paste0(".temp/",x[i],"/"))
      }

  all_bearer_tokens = bearer_tokens
  url <- "https://api.twitter.com/2/users/"
  
endpoint <- "/followers"
params <- list(
    "max_results" = 1000,
    "user.fields" = "created_at,description,entities,id,location,name,pinned_tweet_id,profile_image_url,protected,public_metrics,url,username,verified,withheld"
)
  
  new_df <- data.frame()
  for(i in seq_along(x)){
    cat(paste0("Processing ",x[i],"\n"))
    count_pages <- 0
    next_token <- ""
    while (!is.null(next_token)) {
      count_pages <- count_pages + 1
      requrl <- paste0(url,x[i],endpoint)
      
      if(next_token!=""){
        params[["pagination_token"]] <- next_token
      }
      c_bearer_token = bearer_tokens[count_pages %% length(bearer_tokens) + 1]
      # Try Catch the query, If error remove the current token. 
      tryCatch({
        dat <- make_query(url = requrl, params = params, bearer_token = c_bearer_token, verbose = verbose)   
      }, error = function(e){
        if(length(bearer_tokens) > 1){
          bearer_tokens = bearer_tokens[-which(bearer_tokens == c_bearer_token)]
        } else {
          print("No more tokens to try.")
          # Here we should just assume that something went wrong and re-use all tokens
          bearer_tokens <- all_bearer_tokens
        }
      })

      next_token <- dat$meta$next_token #this is NULL if there are no pages left
      new_rows <- dat$data
      new_rows$from_id <- x[i]
      # This line becomes incredibly inefficient if we have a lot of users
      #new_df <- dplyr::bind_rows(new_df, new_rows) # add new rows

      # We need to create a temp folder with the outputs. 
      # Export new_rows to the temp folder with name f"{x[i]}_{count_pages}_{next_token}.csv"
      # This asssures that we always have a well ordered list of files and incase of an error
      # We can extract the next token to continue from.

      # Export new_rows
      write.csv(new_rows, paste0(".temp/",x[i],"/",x[i],"_",count_pages,"_",next_token,".csv"), row.names = FALSE)


      
      cat("Total data points: ",nrow(new_df), "\n")
      Sys.sleep(1)
      if (is.null(next_token)) {
        if(verbose) {
          cat("This is the last page for ",
              x[i],
              ": finishing collection. \n")
        }
        break
      }
      Sys.sleep(1)
    }
  }
  # Bind all the files in the temp folder to a single dataframe
  # Delete the temp folder
  tryCatch({
    temp_files <- list.files(paste0(".temp/",x[i],"/"), pattern = "*.csv", full.names = TRUE)
    # Sort by inverse creation_time
    temp_files <- temp_files[order(file.info(temp_files)$ctime, decreasing = TRUE)]

    new_df <-  temp_files %>%  
        map_df(read_csv, col_types = cols()) %>% 
        bind_rows()
    # Delete the temp folder using unlink
    unlink(paste0(".temp/",x[i],"/"), recursive = TRUE)
  }, error = function(e){
    print("Error in bind_rows. So we do not delete")
    # Check whether new_df exists in the environment
    if(exists("new_df")){
      return(new_df)
    } else {
      print("new_df does not exist")
      print(paste("FATAL ERROR in ", x[i]))
    }
  }
 
  return(new_df)
}




##### UTILS

make_query <- function(url, params, bearer_token, max_error = 4, verbose = TRUE) {
  bearer_token <- paste0("Bearer ",bearer_token)
  count <- 0
  while (TRUE) {
    if (count >= max_error) {
      stop("Too many errors.")
    }
    pre_time <- Sys.time()
    r <- httr::GET(url, httr::add_headers(Authorization = bearer_token), query = params)
    time_diff <- as.numeric(Sys.time() - pre_time)
    if (time_diff < 1) { ## To prevent #231
      Sys.sleep(1)
    }
    status_code <- httr::status_code(r)
    if (!status_code %in% c(200, 429, 503)) {
      stop(paste("something went wrong. Status code:", httr::status_code(r)))
    }
    if (.check_header_rate_limit(r, verbose = verbose)) {
      count <- count + 1
    }
    if (status_code == 200) {
      break()
    }
    if (status_code == 503) {
      count <- count + 1
      Sys.sleep(count * 5)
    }
    if (status_code == 429) {
      .trigger_sleep(r, verbose = verbose)
      count <- count + 1
    }
  }
  jsonlite::fromJSON(httr::content(r, "text"))
}


.check_header_rate_limit <- function(r, verbose) {
  if (is.null(httr::headers(r)$`x-rate-limit-remaining`)) {
    return(FALSE)
  }
  if (httr::headers(r)$`x-rate-limit-remaining` == "1") {
    .vwarn(verbose, paste("x-rate-limit-remaining=1. Resets at", .check_reset(r)))
    return(TRUE)
  } else {
    return(FALSE)
  }
}

.gen_random_dir <- function() {
  file.path(tempdir(), paste0(sample(letters, 20), collapse = ""))
}


.vcat <- function(bool, ...) {
  if (bool) {
    cat(...)
  }
}

.vwarn <- function(bool, ...) {
  if (bool) {
    warning(..., call. = FALSE)
  }
}

.check_reset <- function(r, tzone = "") {
  lubridate::with_tz(lubridate::as_datetime(as.numeric(httr::headers(r)$`x-rate-limit-reset`), tz = tzone), tzone)
}


.trigger_sleep <- function(r, verbose = TRUE, really_sleep = TRUE, ref_time = Sys.time(), tzone = "") {
  reset_time <- .check_reset(r, tzone = tzone)
  ## add 1s as buffer
  sleep_period <- ceiling(as.numeric(reset_time - ref_time, units = "secs")) + 1
  if (sleep_period < 0) {
    ## issue #213
    .vcat(verbose, "Rate limit reached. Cannot estimate adaptive sleep time. Sleeping for 900 seconds. \n")
    sleep_period <- 900
  } else {
    .vcat(verbose, "Rate limit reached. Rate limit will reset at", as.character(reset_time) ,"\nSleeping for", sleep_period ,"seconds. \n")
  }
  if (verbose) {
    pb <- utils::txtProgressBar(min = 0, max = sleep_period, initial = 0)
    for (i in seq_len(sleep_period)) {
      utils::setTxtProgressBar(pb, i)
      if (really_sleep) {
        Sys.sleep(1)
      }
    }
  } else {
    if (really_sleep) {
      Sys.sleep(sleep_period)
    }
  }
  invisible(r)
}


.process_qparam <- function(param, param_str,query) {
  if(!is.null(param)){
    if(isTRUE(param)) {
      query <- paste(query, param_str)
    } else if(param == FALSE) {
      query <- paste(query, paste0("-", param_str))
    }
  }
  return(query)
}


add_query_prefix <- function(x, prefix){
  q <- paste0(prefix, x)
  q <- paste(q, collapse = " OR ")
  q <- paste0("(",q,")")
  return(q)
}

add_context_annotations <- function(params, verbose){
  if(params[["max_results"]] > 100){
    params[["max_results"]] <- 100
    .vcat(verbose, "page_n is limited to 100 due to the restriction imposed by Twitter API\n")
  }
  params[["tweet.fields"]] <- "attachments,author_id,context_annotations,conversation_id,created_at,entities,geo,id,in_reply_to_user_id,lang,public_metrics,possibly_sensitive,referenced_tweets,source,text,withheld"
  params
}