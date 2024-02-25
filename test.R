library(data.table)
library(tidyverse)
library(stringr)
library(DBI)
library(zoo)
library(tidytext)
library(stopwords)
library(collapse)


source("R/read_data.R")
source("R/utils.R")

inc_file_name <- here::here("data", "COVID-19-Faelle_7-Tage-Inzidenz_Deutschland.csv")
raw_csv_datafile <- here::here("data/merged.csv")
db_file_name <- here::here("data", "my-db.sqlite")


incidences <- load_incidences(inc_file_name)
all_data <- read_twitter_data(raw_csv_datafile, 100000)

#create_twitter_db(all_data,db_file_name)
#tweets_db <- connect_twitter_db(db_file_name)


#tweets_db |> filter(lang == "de") |> select(text) |> head(10)


tweets_db %>% 
  select(created_at) %>% 
  collect() %>% 
  mutate(created_at = ymd_hms(created_at)) %>% 
  ggplot(aes(x = created_at)) + geom_histogram()
#### SQLITE Version END


filter_word <- "impfung"


# average tweet counts over a week
all_data %>% 
  dplyr::filter(lang == "de") %>% 
  mutate(text = str_to_lower(text)) %>% 
  filter(str_detect(text, fixed(filter_word))) %>%
  select(created_at) %>% 
  collect() %>% 
  mutate(created_at = ymd_hms(created_at)) %>% 
  mutate(created_at_day = as_date(floor_date(created_at, "day"))) %>%
  group_by(created_at_day) %>%
  summarize(tweet_count = n()) %>% 
  mutate(tweet_count = rollmean(tweet_count, 7, fill = NA, align = "right")) %>%
  # find starting day
  #filter(tweet_roll > 100) %>%
  #pull(created_at_day) %>% min()
  #select(created_at_day, tweet_count) %>%
  #mutate(keyowrd = filter_word) %>% 
  noop() -> res_data 

# plot tweet weekly averages ----
res_data %>% 
  ggplot() + 
    aes(x = created_at_day, y = tweet_count) + 
    geom_line() +
  labs(title = filter_word)
    
    
res_inc <- incidences %>% 
  filter(Meldedatum > ymd("2022-08-05")) %>% 
  #filter(Meldedatum < ymd("2022-08-01")) %>%
  filter(Meldedatum < ymd("2023-07-01")) %>%
  # altersgruppen zusammenführen
  filter(Altersgruppe == "00+") %>% 
  #group_by(Meldedatum) %>%
  #summarise(`Faelle_7-Tage` = sum(`Faelle_7-Tage`)) %>%
  #mutate(roll_sum = rollmean(Faelle_neu, 7, fill = NA, align = "right")) %>%
  noop() 
  
res_inc %>% 
  ggplot() +
  aes(x = Meldedatum, y = `Faelle_7-Tage`, group = 1) + 
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%b %y") +
  labs(y = "7-Tage-Inzidenz", x = "Meldedatum")
  
ggsave("output/incidence.pdf")


res_data %>% rename(coronatweets = tweet_count) %>% 
  left_join(res_inc, by = c("created_at_day" = "Meldedatum")) %>% 
  select(created_at_day, coronatweets, f0 = `Faelle_7-Tage`) %>% 
  mutate(f1 = lag(f0, 2)) %>% 
  mutate(f2 = lag(f0, 4)) %>% 
  mutate(f3 = lag(f0, 6)) %>% 
  mutate(f4 = lag(f0, 8)) %>% 
  mutate(b1 = lead(f0, 2)) %>% 
  mutate(b2 = lead(f0, 4)) %>% 
  mutate(b3 = lead(f0, 6)) %>% 
  mutate(b4 = lead(f0, 8)) %>% 
  select(-created_at_day) %>% 
  noop() -> test_data

options(digits = 2)

cor(test_data, use = "pairwise.complete.obs")^2


# wordbased approach ----




# tokenization of all tweets
lower_de_data <- all_data %>% 
  filter(lang == "de") %>% 
  mutate(text = str_to_lower(text)) %>% 
  mutate(created_at = ymd_hms(created_at)) %>% 
  mutate(created_at_day = as_date(floor_date(created_at, "day"))) %>% 
  noop()



# generate a stopwords list
custom_stop_words <- c("rt", "https", "t.co", "mehr", "schon", "mal", "ja", "seit", "wurde", "wer", "warum",
                       "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "wäre", "wurden", "würde", "würden",
                       "wegen", "gibt", "ab", "d", "2020", "ganz", "viele", "beim", "amp", "bitte", "immer", "nie",
                       "lassen", "kommt", "müssen", "eigentlich", "wohl", "endlich", "hätte", "herr", "erst", "u",
                       "fast", "gar", "dafür", "nein", "klar", "tun")
stop_words <- tibble(word = c(stopwords::data_stopwords_nltk$de, custom_stop_words))


lower_de_data %>% 
  #sample_n(100000) %>% 
  select(created_at_day, text) %>% 
  unnest_tokens(word, text) %>% 
  group_by(created_at_day, word) %>%
  summarize(word_count = n()) %>% 
  ungroup() %>% 
  arrange(desc(word_count)) %>% 
  anti_join(stop_words, by = c("word" = "word")) %>%
  noop() -> tokenized_data

# test for all words
tokenized_data %>% 
  group_by(word) %>% 
  summarize(tweet_count = sum(word_count) ) %>% 
  arrange(desc(tweet_count)) -> top_words

top_words %>%
  head(100) %>%
  ggplot() +
  aes(x = reorder(word, tweet_count), y = tweet_count) +
  geom_col() +
  coord_flip()

ggsave("output/top_words.pdf")


cor_table <- data.frame()
# plot the time series for the top words
for(i in 1:1000){
  
  idx <- i
  fname <- paste0("output/word_", idx, "_", top_words$word[idx], ".pdf")

roll_avg <- tokenized_data %>% 
  filter(word == top_words$word[idx]) %>% 
  mutate(word_count = rollmean(word_count, 7, fill = NA, align = "right")) %>% 
  noop()

tmax <- roll_avg %>% pull(word_count) %>% max(na.rm = TRUE)
imax <- res_inc %>% 
  filter(Altersgruppe == "00+") %>% 
  select(datum = Meldedatum, cases = `Faelle_7-Tage`) %>% 
  pull(cases) %>% max(na.rm = TRUE)

max_ratio <- imax/tmax

c_table <- res_inc %>% 
  filter(Altersgruppe == "00+") %>% 
  select(datum = Meldedatum, cases = `Faelle_7-Tage`) %>%
  left_join(roll_avg, by = c(datum = "created_at_day")) %>% 
  select(cases, word_count) %>% 
  cor(use = "pairwise.complete.obs")

# add correlation data to table
cor_table <- bind_rows(cor_table, 
                       data.frame(word = c(top_words$word[idx]), cor = c(c_table[1,2]))
                       )


res_inc %>% 
  filter(Altersgruppe == "00+") %>% 
  select(datum = Meldedatum, cases = `Faelle_7-Tage`) %>%
  left_join(roll_avg, by = c(datum = "created_at_day")) %>% 
  select(-word) %>% 
  ggplot() +
  aes(x = datum) +
  geom_line(aes(y = cases, color = "red", name = "cases")) +
  geom_line(aes(y = word_count*max_ratio, color = "blue")) +
  scale_y_continuous(
  
    # Features of the first axis
    name = "cases",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./max_ratio, name="term frequency")
  ) +
  labs(title = paste("Rolling 7-day average for term:",top_words$word[idx], "- cor:", round(c_table[1,2], 2)),
       x = "date", y = "term frequency") 
  
  ggsave(fname)
}

cor_table %>% arrange(desc(cor)) %>% head(100) 
