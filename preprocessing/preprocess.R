
parse_data <- function(demo_file, data_file) {

  # read the data
  demo_table <- readr::read_csv(demo_file)
  data_table <- readr::read_csv(data_file)
  join_table <- dplyr::full_join(demo_table, data_table, by = "ID")

  # pivot, clean names and separate
  experiment <- join_table %>%
    tidyr::pivot_longer(
      cols = tidyselect::matches("^[CP]"),
      names_to = "measure",
      values_to = "response"
    ) %>%
    janitor::clean_names() %>%
    tidyr::separate(
      col = "measure",
      into = c("sampling_frame", "sample_size", "test_item"),
      sep = "_"
    )

  # tidy values
  experiment <- experiment %>%
    dplyr::mutate(
      sampling_frame = dplyr::case_when(
        sampling_frame == "C" ~ "category",
        sampling_frame == "P" ~ "property",
        TRUE ~ NA_character_
      ),
      sample_size = as.numeric(stringr::str_remove_all(sample_size, "SS")),
      test_item = as.numeric(test_item),
      gender = stringr::str_to_lower(gender)
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
readr::write_csv(exp1, here::here("data", "exp1.csv"))
readr::write_csv(exp2, here::here("data", "exp2.csv"))
