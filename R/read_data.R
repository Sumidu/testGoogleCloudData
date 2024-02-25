load_incidences <- function(filename){
  incidences <- readr::read_csv(filename)
  incidences |> 
    filter(Altersgruppe == "00+")
}


read_twitter_data <- function(filename){
  all_data <- fread(filename, stringsAsFactors = FALSE)
  
  all_data |> 
    #sample_n(limit_rows) |>
    mutate(created_at = ymd_hms(created_at)) |> 
    write_rds(here::here("data", "all_data.rds"))
  
  read_rds(here::here("data", "all_data.rds"))
}

connect_twitter_db <- function(db_filename){
  ### SQLite Version START
  con <- dbConnect(RSQLite::SQLite(), db_filename)
  tweets_db <- tbl(con, "tweets")
  tweets_db
}


create_twitter_db <- function(all_data, db_filename){
  
  ### SQLite Version START
  con <- dbConnect(RSQLite::SQLite(), db_filename)
  copy_to(con, all_data, "tweets", temporary = FALSE, indexes = list(
    c("id_str", "created_at"),
    "id_str",
    "created_at"
  ))
}