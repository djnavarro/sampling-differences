library(tidyverse)

parse_data <- function(demo_file, data_file) {
  
  # read the data
  demo_table <- read_csv(demo_file)
  data_table <- read_csv(data_file)
  join_table <- full_join(demo_table, data_table)
  
  # pivot, clean names and separate
  experiment <- join_table %>% 
    pivot_longer(
      cols = matches("^[CP]"), 
      names_to = "measure", 
      values_to = "response"
    ) %>% 
    janitor::clean_names() %>%
    separate(
      col = "measure", 
      into = c("sampling_frame", "sample_size", "test_item"),
      sep = "_"
    )
  
  # tidy values
  experiment <- experiment %>% 
    mutate(
      sampling_frame = case_when(
        sampling_frame == "C" ~ "category",
        sampling_frame == "P" ~ "property",
        TRUE ~ NA_character_
      ),
      sample_size = as.numeric(str_remove_all(sample_size, "SS")),
      test_item = as.numeric(test_item), 
      gender = str_to_lower(gender)
    )
  
  
  
}

exp1 <- parse_data(
  demo_file = here::here("inputs", "Exp 1 - demographics.csv"),
  data_file = here::here("inputs", "Exp 1 - generalisation ratings.csv")
)

exp2 <- parse_data(
  demo_file = here::here("inputs", "Exp 2 - demographics.csv"),
  data_file = here::here("inputs", "Exp 2 - generalisation ratings.csv")
)

# just in case, store the experiment number
exp1$exp_num <- 1
exp2$exp_num <- 2

# save the data
write_csv(exp1, here::here("data", "exp1.csv"))
write_csv(exp2, here::here("data", "exp2.csv"))
