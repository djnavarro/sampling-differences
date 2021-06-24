

# helper functions to plot model and aggregate ----------------------------

plot_curves <- function(dat) {
  ggplot(
    data = dat,
    mapping = aes(
      x = test_item,
      y = response,
      colour = factor(sample_size)
    )
  ) + 
    facet_grid(rows = vars(source), cols = vars(sampling_frame)) + 
    geom_path(show.legend = FALSE) +
    geom_point(size = 3) + 
    scale_color_scico_d(
      name = "sample size", 
      palette = "bilbao", 
      begin = .3, 
      end = .8
    ) + 
    ylim(0, 1) +
    NULL
}

plot_points <- function(dat) {
  
  dat <- dat %>%
    pivot_wider(names_from = "source", values_from = "response")
  
  ggplot(
    data = dat,
    mapping = aes(
      x = model,
      y = human,
      colour = factor(sample_size)
    )
  ) + 
    facet_wrap(~sampling_frame) + 
    geom_abline(slope = 1, intercept = 0) + 
    geom_point(
      #  show.legend = FALSE, 
      size = 3
    ) + 
    scale_color_scico_d(
      name = NULL, 
      palette = "bilbao", 
      begin = .3, 
      end = .8
    ) + 
    coord_fixed(xlim = c(0,1), ylim = c(0,1)) + 
    NULL
}



# constraint checking helpers ---------------------------------------------


check_value <- function(higher, lower) {
  
  if(is.na(higher)) return(NA)
  if(is.na(lower)) return(NA)
  
  if(higher >  lower) return(1)
  if(higher == lower) return(.5)
  if(higher <  lower) return(0)
}

is_met <- function(dat, which) {
  
  # A  - (test items 1-2) > (test items 3-6) overall
  # B  - (test items 1-2) > (test items 3-6) category
  # C  - (test items 1-2) > (test items 3-6) property
  if(which == "A") {
    test_sml <- dat %>% filter(test_item %in% 1:2) %>% pull(response) %>% mean()
    test_big <- dat %>% filter(test_item %in% 3:6) %>% pull(response) %>% mean()
    return(check_value(higher = test_sml, lower = test_big))
  }
  if(which == "B") {
    test_sml <- dat %>% filter(test_item %in% 1:2, sampling_frame == "category") %>% pull(response) %>% mean()
    test_big <- dat %>% filter(test_item %in% 3:6, sampling_frame == "category") %>% pull(response) %>% mean()
    return(check_value(higher = test_sml, lower = test_big))
  }
  if(which == "C") {
    test_sml <- dat %>% filter(test_item %in% 1:2, sampling_frame == "property") %>% pull(response) %>% mean()
    test_big <- dat %>% filter(test_item %in% 3:6, sampling_frame == "property") %>% pull(response) %>% mean()
    return(check_value(higher = test_sml, lower = test_big))
  }
  
  
  # D  - (ss 20 > ss 8)  test items 1-2 category
  # E  - (ss 20 > ss 2)  test items 1-2 category
  # F  - (ss 8 > ss 2)   test items 1-2 category
  if(which == "D") {
    gen_big <- dat %>% filter(test_item %in% 1:2, sampling_frame == "category", sample_size == 20) %>% pull(response) %>% mean()
    gen_sml <- dat %>% filter(test_item %in% 1:2, sampling_frame == "category", sample_size == 8) %>% pull(response) %>% mean()
    return(check_value(higher = gen_big, lower = gen_sml))
  }
  if(which == "E") {
    gen_big <- dat %>% filter(test_item %in% 1:2, sampling_frame == "category", sample_size == 20) %>% pull(response) %>% mean()
    gen_sml <- dat %>% filter(test_item %in% 1:2, sampling_frame == "category", sample_size == 2) %>% pull(response) %>% mean()
    return(check_value(higher = gen_big, lower = gen_sml))
  }
  if(which == "F") {
    gen_big <- dat %>% filter(test_item %in% 1:2, sampling_frame == "category", sample_size == 8) %>% pull(response) %>% mean()
    gen_sml <- dat %>% filter(test_item %in% 1:2, sampling_frame == "category", sample_size == 2) %>% pull(response) %>% mean()
    return(check_value(higher = gen_big, lower = gen_sml))
  }
  
  
  # G  - (ss 20 > ss 8)  test items 1-2 property
  # H  - (ss 20 > ss 2)  test items 1-2 property
  # I  - (ss 8 > ss 2)   test items 1-2 property
  if(which == "G") {
    gen_big <- dat %>% filter(test_item %in% 1:2, sampling_frame == "property", sample_size == 20) %>% pull(response) %>% mean()
    gen_sml <- dat %>% filter(test_item %in% 1:2, sampling_frame == "property", sample_size == 8) %>% pull(response) %>% mean()
    return(check_value(higher = gen_big, lower = gen_sml))
  }
  if(which == "H") {
    gen_big <- dat %>% filter(test_item %in% 1:2, sampling_frame == "property", sample_size == 20) %>% pull(response) %>% mean()
    gen_sml <- dat %>% filter(test_item %in% 1:2, sampling_frame == "property", sample_size == 2) %>% pull(response) %>% mean()
    return(check_value(higher = gen_big, lower = gen_sml))
  }
  if(which == "I") {
    gen_big <- dat %>% filter(test_item %in% 1:2, sampling_frame == "property", sample_size == 8) %>% pull(response) %>% mean()
    gen_sml <- dat %>% filter(test_item %in% 1:2, sampling_frame == "property", sample_size == 2) %>% pull(response) %>% mean()
    return(check_value(higher = gen_big, lower = gen_sml))
  }
  
  
  # J  - (ss 20 > ss 8)  test items 3-6 category
  # K  - (ss 20 > ss 2)  test items 3-6 category
  # L  - (ss 8 > ss 2)   test items 3-6 category
  if(which == "J") {
    gen_big <- dat %>% filter(test_item %in% 3:6, sampling_frame == "category", sample_size == 20) %>% pull(response) %>% mean()
    gen_sml <- dat %>% filter(test_item %in% 3:6, sampling_frame == "category", sample_size == 8) %>% pull(response) %>% mean()
    return(check_value(higher = gen_big, lower = gen_sml))
  }
  if(which == "K") {
    gen_big <- dat %>% filter(test_item %in% 3:6, sampling_frame == "category", sample_size == 20) %>% pull(response) %>% mean()
    gen_sml <- dat %>% filter(test_item %in% 3:6, sampling_frame == "category", sample_size == 2) %>% pull(response) %>% mean()
    return(check_value(higher = gen_big, lower = gen_sml))
  }
  if(which == "L") {
    gen_big <- dat %>% filter(test_item %in% 3:6, sampling_frame == "category", sample_size == 8) %>% pull(response) %>% mean()
    gen_sml <- dat %>% filter(test_item %in% 3:6, sampling_frame == "category", sample_size == 2) %>% pull(response) %>% mean()
    return(check_value(higher = gen_big, lower = gen_sml))
  }
  
  # M  - (ss 20 < ss 8)  test items 3-6 property
  # N  - (ss 20 < ss 2)  test items 3-6 property
  # O  - (ss 8 < ss 2)   test items 3-6 property
  if(which == "M") {
    gen_big <- dat %>% filter(test_item %in% 3:6, sampling_frame == "property", sample_size == 20) %>% pull(response) %>% mean()
    gen_sml <- dat %>% filter(test_item %in% 3:6, sampling_frame == "property", sample_size == 8) %>% pull(response) %>% mean()
    return(check_value(higher = gen_sml, lower = gen_big)) # note reversal!
  }
  if(which == "N") {
    gen_big <- dat %>% filter(test_item %in% 3:6, sampling_frame == "property", sample_size == 20) %>% pull(response) %>% mean()
    gen_sml <- dat %>% filter(test_item %in% 3:6, sampling_frame == "property", sample_size == 2) %>% pull(response) %>% mean()
    return(check_value(higher = gen_sml, lower = gen_big)) # note reversal!
  }
  if(which == "O") {
    gen_big <- dat %>% filter(test_item %in% 3:6, sampling_frame == "property", sample_size == 8) %>% pull(response) %>% mean()
    gen_sml <- dat %>% filter(test_item %in% 3:6, sampling_frame == "property", sample_size == 2) %>% pull(response) %>% mean()
    return(check_value(higher = gen_sml, lower = gen_big)) # note reversal!
  }
  
}


constraints_met <- function(dat) {
  
  # set up data frame
  constraints <- tibble(
    id = first(dat$id),
    name = LETTERS[1:15],
    met = FALSE
  )
  
  # regularity list:
  
  # A  - (test items 1-2) > (test items 3-6) overall
  # B  - (test items 1-2) > (test items 3-6) category
  # C  - (test items 1-2) > (test items 3-6) property
  constraints$met[1] <- is_met(dat, "A")
  constraints$met[2] <- is_met(dat, "B")
  constraints$met[3] <- is_met(dat, "C")
  
  # D  - (ss 20 > ss 8)  test items 1-2 category
  # E  - (ss 20 > ss 2)  test items 1-2 category
  # F  - (ss 8 > ss 2)   test items 1-2 category
  constraints$met[4] <- is_met(dat, "D")
  constraints$met[5] <- is_met(dat, "E")
  constraints$met[6] <- is_met(dat, "F")
  
  # G  - (ss 20 > ss 8)  test items 1-2 property
  # H  - (ss 20 > ss 2)  test items 1-2 property
  # I  - (ss 8 > ss 2)   test items 1-2 property
  constraints$met[7] <- is_met(dat, "G")
  constraints$met[8] <- is_met(dat, "H")
  constraints$met[9] <- is_met(dat, "I")
  
  # J  - (ss 20 > ss 8)  test items 3-6 category
  # K  - (ss 20 > ss 2)  test items 3-6 category
  # L  - (ss 8 > ss 2)   test items 3-6 category
  constraints$met[10] <- is_met(dat, "J")
  constraints$met[11] <- is_met(dat, "K")
  constraints$met[12] <- is_met(dat, "L")
  
  # M  - (ss 20 < ss 8)  test items 3-6 property
  # N  - (ss 20 < ss 2)  test items 3-6 property
  # O  - (ss 8 < ss 2)   test items 3-6 property
  constraints$met[13] <- is_met(dat, "M")
  constraints$met[14] <- is_met(dat, "N")
  constraints$met[15] <- is_met(dat, "O")
  
  return(constraints)
}



plot_constraint_checks <- function(filename) {
  
  human_raw <- read_csv(here::here("data", filename)) 
  
  human_list <- human_raw %>% group_split(id)
  
  human_qual <- human_list %>% 
    map_dfr(constraints_met)
  
  qual_summary <- human_qual %>% 
    group_by(name) %>% 
    summarise(
      prop_met = mean(met), 
      n_met = sum(met == 1), 
      n_fail = sum(met == 0), 
      n_idk = sum(met == .5)
    ) %>%
    ungroup()
  
  # ---------------- i kind of don't like these tests --------------
  # qual_summary$p_val <- NA
  # for(i in 1:15) {
  #   if(!is.na(qual_summary$n_met[i])) {
  #     qual_summary$p_val[i] <- binom.test(
  #       x = qual_summary$n_met[i], 
  #       n = qual_summary$n_met[i] + qual_summary$n_fail[i], 
  #       p = .5, 
  #       alternative = "greater"
  #     )$p.value
  #   }
  # }
  # qual_summary$p_val[qual_summary$p_val < .001] <- 0 
  
  gap <- 3
  unique_pos <- c(1:3, gap + (4:6), 2*gap + (7:9), 3*gap + (10:12), 4*gap + (13:15))
  long_names <- c(
    "overall",
    "category sampling",
    "property sampling",
    
    "20 v 8 items",
    "20 v 2 items",
    "8 v 2 items",
    
    "20 v 8 items",
    "20 v 2 items",
    "8 v 2 items",
    
    "20 v 8 items",
    "20 v 2 items",
    "8 v 2 items",
    
    "20 v 8 items",
    "20 v 2 items",
    "8 v 2 items"
  )
  
  group_lab <- tibble(
    x = 2,
    y = -((00:4) * (3 + gap) - gap * .2),
    lab = c(
      "(a) Test items are CLOSER",
      "(b) Sample size is LARGER: near tests, category sampling",
      "(c) Sample size is LARGER: near tests, property sampling",
      "(d) Sample size is LARGER: far tests, category sampling",
      "(e) Sample size is SMALLER: far tests, property sampling"
    )
  )
  
  sz <- 7
  pic <- ggplot(
    data = qual_summary %>% mutate(pos = -unique_pos, long_name = long_names),
    mapping = aes(y = pos, yend = pos, group = name)
  ) + 
    geom_segment(aes(x = 0, xend = n_met), color = "orange", size = sz) + 
    geom_segment(aes(x = -n_fail -2, xend = -2), color = "skyblue", size = sz) + 
    geom_segment(aes(x = 70, xend = 70 + n_idk), color = "grey", size = sz) + 
    geom_text(aes(x = n_met + 3, label = n_met), size = 3) +
    geom_text(aes(x = -n_fail - 5, label = n_fail), size = 3) +
    geom_text(aes(x = 73 + n_idk, label = n_idk), size = 3) +
    geom_text(aes(x = 2, label = long_name), color = "white", hjust = "left", size = 3) +
    geom_text(data = group_lab, mapping = aes(x = x, y = y, label = lab), inherit.aes = FALSE, hjust = "left") + 
    theme_void() + 
    ggtitle("Are participants more willing to generalize when...") + 
    annotate("text", x = 5, y = -28, label = "Yes") +
    annotate("text", x =-5, y = -28, label = "No") +
    annotate("text", x =76, y = -28, label = "Same")
  
  #ggsave(here::here("model", "constraint_checking.png"), pic, height = 8, width = 6)
  
  return(pic)
  
}




# helper for the individual variation -------------------------------------


plot_individual_differences <- function(filename) {
  
  
  human_raw <- read_csv(here::here("data", filename)) 
  
  max_ss <- max(human_raw$sample_size)
  min_ss <- min(human_raw$sample_size)
  
  
  human_sml <- human_raw %>%
    mutate(test_type = ifelse(test_item %in% 1:2, "near", "far")) %>%
    group_by(id, test_type, sample_size, sampling_frame) %>%
    summarise(response = mean(response/10)) %>%
    ungroup() %>%
    pivot_wider(id_cols = c(id, test_type, sample_size), names_from = sampling_frame, values_from = response)
  
  movement <- function(df) {
    mv <- abs(df$category[2] - df$category[1]) + 
      abs(df$category[3] - df$category[2]) + 
      abs(df$category[5] - df$category[4]) +  
      abs(df$category[6] - df$category[5]) +
      abs(df$property[2] - df$property[1]) + 
      abs(df$property[3] - df$property[2]) + 
      abs(df$property[5] - df$property[4]) +  
      abs(df$property[6] - df$property[5])
    
    df$mv <- mv
    return(df)
  }
  
  directions <- function(df) {
    max_ss <- max(df$sample_size)
    min_ss <- min(df$sample_size)
    tibble(
      id = first(df$id),
      category_near = sign(df$category[df$sample_size == max_ss & df$test_type == "near"] - df$category[df$sample_size == min_ss & df$test_type == "near"]),
      category_far =  sign(df$category[df$sample_size == max_ss & df$test_type == "far"]  - df$category[df$sample_size == min_ss & df$test_type == "far"]),
      property_near = sign(df$property[df$sample_size == max_ss & df$test_type == "near"] - df$property[df$sample_size == min_ss & df$test_type == "near"]),
      property_far =  sign(df$property[df$sample_size == max_ss & df$test_type == "far"]  - df$property[df$sample_size == min_ss & df$test_type == "far"]),
    )
  }
  
  human_sgn <- human_sml %>%
    group_split(id) %>%
    map_dfr(~directions(.)) %>%
    mutate(
      near_predicted = (category_near == 1) & (property_near == 1), 
      far_predicted =  (category_far == 1) & (property_far == -1),
      is_predicted = near_predicted & far_predicted
    )
  
  
  human_sml <- human_sml %>%
    left_join(human_sgn) %>%
    group_split(id) %>%
    map_dfr(~movement(.)) %>%
    mutate(id = factor(id) %>% reorder(mv))
  
  
  
  # unite(condition, test_type, sample_size)
  
  p <- ggplot(
    data = human_sml, # %>% filter(id < 20),
    mapping = aes(category, property, colour = test_type)
  ) +
    geom_rect(
      data = human_sml %>% filter(is_predicted),
      mapping = aes(xmin = 0, ymin = 0, xmax = 1, ymax = 1),
      inherit.aes = FALSE,
      show.legend = FALSE,
      color = "grey70",
      fill = NA,
      alpha = .1
    ) +
    geom_abline(slope = 1, intercept = 0, color = "grey80", size = .2) + 
    geom_path(show.legend = FALSE) +
    geom_point(show.legend = FALSE, size = .25) +
    geom_point(data = human_sml %>% filter(sample_size == 20), size = 1.5) +
    geom_text(aes(x = .15, y = .85, label = id), color = "grey50", size = 2) + 
    facet_wrap(~id) + 
    theme(
      strip.background = element_blank(), 
      strip.text = element_blank()
    ) + 
    scale_color_discrete("Test item type", breaks = c("near", "far"), labels = c("Nearby test items", "Distant test items")) + 
    scale_x_continuous("Category sampling", NULL, limits = c(0,1), expand = c(0, 0)) + 
    scale_y_continuous("Property sampling", NULL, limits = c(0,1), expand = c(0, 0)) + 
    #scale_fill_brewer() + 
    coord_fixed(clip = "off") +
    NULL
  
  #ggsave(here::here("model", "indiv_plots.png"), p)
  
  
  human_sml <- human_sml %>% mutate(
    this_predicted = case_when(
      test_type == "near" ~ near_predicted,
      test_type == "far" ~ far_predicted
    )
  )
  
  human_sml <- human_sml %>%
    group_by(id, test_type) %>%
    mutate(category_n = category - category[sample_size == min_ss],
           property_n = property - property[sample_size == min_ss]
    )
  
  mutate_direction <- function(data, filename) {
    if(filename == "exp1.csv") {
      data$direction <- sign(data$`20` - data$`2`)
    } else {
      data$direction <- sign(data$`20` - data$`8`)
    }
    return(data)
  }
  
  counts <- human_sml %>%
    filter(sample_size %in% c(min_ss, max_ss)) %>%
    pivot_longer(cols = c(category, property), names_to = "sample_type", values_to = "response") %>%
    select(id, test_type, sample_size, sample_type, response) %>%
    pivot_wider(names_from = sample_size, values_from = response) %>%
    mutate_direction(filename) %>%
    pivot_wider(id_cols = c(id, test_type), names_from = sample_type, values_from = direction) %>%
    mutate(category = as.factor(category), property = as.factor(property)) %>%
    group_by(test_type, category, property, .drop = FALSE) %>%
    summarise(n = n()) %>%
    mutate(
      category = as.numeric(as.character(category)), 
      property = as.numeric(as.character(property))) %>%
    ungroup()
  
  p3 <- ggplot(data = human_sml,
               mapping = aes(category_n, property_n, group = id, color = this_predicted)
  ) +
    geom_path(show.legend = FALSE, alpha = .25) +
    geom_point(show.legend = FALSE, alpha = .5, data = human_sml %>% filter(sample_size == 20)) +
    geom_label(data = counts, mapping = aes(x=category*.9, y = property*.9, label = n), inherit.aes = FALSE) + 
    facet_wrap(~relevel(as.factor(test_type), "near"), labeller = as_labeller(c(near = "Nearby test items", far = "Distant test items"))) + 
    scale_x_continuous("Effect of sample size, category sampling", NULL, limits = c(-1,1), expand = c(1, 1)*.01) + 
    scale_y_continuous("Effect of sample size, property sampling", NULL, limits = c(-1,1), expand = c(1, 1)*.01) + 
    scico::scale_color_scico_d(palette = "bilbao", begin = .5) + 
    coord_fixed(clip = "off") +
    NULL
  
  
  #ggsave(here::here("model", "indiv_plots3.png"), p3, width = 6, height = 4)
  
  
  return(list(p, p3))
  
}






