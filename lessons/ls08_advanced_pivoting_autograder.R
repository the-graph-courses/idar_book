if(!require('pacman')) install.packages('pacman')
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(here, 
               glue,
               praise,
               janitor,
               tidyverse)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 5)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_adult_long ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_adult_long <-
  function() {
    
    .problem_number <<- 1
    
    .adult_stats <- 
      tibble::tribble(
        ~adult,  ~year1_BMI,  ~year2_BMI,  ~year1_HIV,  ~year2_HIV,
        "A",          25,          30,  "Positive",  "Positive",
        "B",          34,          28,  "Negative",  "Positive",
        "C",          19,          17,  "Negative",  "Negative"
      )
    
    .Q_adult_long <- 
      .adult_stats %>%
      pivot_longer(cols = 2:5,
                   names_sep = "_",
                   names_to = c("year", ".value"))
    
    .autograder <<-
      function() {
        if (!exists("Q_adult_long"))
          .na("You have not yet defined the answer object, `Q_adult_long`.")
        
        if (!is.data.frame(Q_adult_long))
          .na("Invalid answer. Your answer should be a data frame")
        
        if (ncol(Q_adult_long) != 4)
          .fail("Wrong. Your answer should have four columns.")
        
        if (!all(names(Q_adult_long) %in% names(.Q_adult_long)) )
          .fail("Wrong. Your answer should have the following columns: adult, year, BMI, HIV")
        
        if (isTRUE(all.equal(.Q_adult_long,
                             Q_adult_long, ignore_col_order = T, ignore_row_order = T)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_adult_long <- function() {
  '
HINT.
  
  Use the `pivot_longer` function from the `tidyverse`. 
  - Specify `cols = 2:5` to select the columns to pivot. 
  - Use `names_sep = "_"` and `names_to = c("year", ".value")` to format the names.
  ' -> out
  cat(out)
}


.SOLUTION_Q_adult_long <- function() {
  '

  
  adult_stats %>%
      pivot_longer(cols = 2:5,
                   names_sep = "_",
                   names_to = c("year", ".value"))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_growth_stats_long ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_growth_stats_long <-
  function() {
    
    .problem_number <<- 2
    
    .growth_stats <- 
      tibble::tribble(
        ~child,~yr1_head,~yr2_head,~yr1_neck,~yr2_neck,~yr1_hip,~yr2_hip,
        "a",       45,       48,       23,       24,      51,      52,
        "b",       48,       50,       24,       26,      52,      52,
        "c",       50,       52,       24,       27,      53,      54
      )
    
    .Q_growth_stats_long <- 
      .growth_stats %>%
      pivot_longer(cols = 2:7,
                   names_to = c("year", ".value"),
                   names_sep = "_")
    
    .autograder <<-
      function() {
        if (!exists("Q_growth_stats_long"))
          .na("You have not yet defined the answer object, `Q_growth_stats_long`.")
        
        if (!is.data.frame(Q_growth_stats_long))
          .na("Invalid answer. Your answer should be a data frame")
        
        if (ncol(Q_growth_stats_long) != 5)
          .fail("Wrong. Your answer should have five columns.")
        
        if (!all(names(Q_growth_stats_long) %in% names(.Q_growth_stats_long)) )
          .fail("Wrong. Your answer should have the following columns: child, year, head, neck, hip")
        
        if (isTRUE(all.equal(.Q_growth_stats_long,
                             Q_growth_stats_long, ignore_col_order = T, ignore_row_order = T)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_growth_stats_long <- function() {
  '
HINT.
  
  Use the `pivot_longer` function from the `tidyverse`. 
  - Specify `cols = 2:7` to select the columns to pivot. 
  - Use `names_to = c("year", ".value")` and `names_sep = "_"` to format the names.
  ' -> out
  cat(out)
}

.SOLUTION_Q_growth_stats_long <- function() {
  '

  
  growth_stats %>%
     pivot_longer(cols = 2:7,
                   names_to = c("year", ".value"),
                   names_sep = "_")' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_adult2_long ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_adult2_long <-
  function() {
    
    .problem_number <<- 3
    
    .adult_stats_dot_sep <- 
      tibble::tribble(
        ~adult,  ~`BMI.year1`,  ~`BMI.year2`,  ~`HIV.year1`,  ~`HIV.year2`,
        "A",            25,            30,    "Positive",   "Positive",
        "B",            34,            28,    "Negative",   "Positive",
        "C",            19,            17,    "Negative",   "Negative"
      )
    
    .Q_adult2_long <- 
      .adult_stats_dot_sep %>%
      pivot_longer(cols = 2:5,
                   names_sep = "\\.",
                   names_to = c(".value", "year"))
    
    .autograder <<-
      function() {
        if (!exists("Q_adult2_long"))
          .na("You have not yet defined the answer object, `Q_adult2_long`.")
        
        if (!is.data.frame(Q_adult2_long))
          .na("Invalid answer. Your answer should be a data frame")
        
        if (ncol(Q_adult2_long) != 4)
          .fail("Wrong. Your answer should have four columns.")
        
        if (!all(names(Q_adult2_long) %in% names(.Q_adult2_long)) )
          .fail("Wrong. Your answer should have the following columns: adult, BMI, HIV, year")
        
        if (isTRUE(all.equal(.Q_adult2_long,
                             Q_adult2_long, ignore_col_order = T, ignore_row_order = T)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_adult2_long <- function() {
  '
HINT.
  
  Use the `pivot_longer` function from the `tidyverse`. 
  - Specify `cols = 2:5` to select the columns to pivot. 
  - Remember to correctly escape the dot character using `names_sep = "\\."` 
  and use `names_to = c(".value", "year")` to format the names.
  ' -> out
  cat(out)
}


.SOLUTION_Q_adult2_long <- function() {
  '

  
  adult_stats_dot_sep %>%
      pivot_longer(cols = 2:5,
                   names_sep = "\\.",
                   names_to = c(".value", "year"))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_diet_diversity_vietnam_long ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_diet_diversity_vietnam_long <-
  function() {
    
    .problem_number <<- 4
    
    .diet_diversity_vietnam_wide <- suppressMessages( read_csv(here("data/diet_diversity_vietnam_wide.csv")))
    
    .Q_diet_diversity_vietnam_long <- 
      .diet_diversity_vietnam_wide%>%
      rename(
        enerc_kcal_w__1 = enerc_kcal_w_1,
        enerc_kcal_w__2 = enerc_kcal_w_2,
        dry_w__1 = dry_w_1,
        dry_w__2 = dry_w_2,
        water_w__1 = water_w_1,
        water_w__2 = water_w_2,
        fat_w__1 = fat_w_1,
        fat_w__2 = fat_w_2
      ) %>%  
      pivot_longer(2:9, names_sep = "__", names_to = c(".value", "visit"))
    
    .autograder <<-
      function() {
        if (!exists("Q_diet_diversity_vietnam_long"))
          .na("You have not yet defined the answer object, `Q_diet_diversity_vietnam_long`.")
        
        if (!is.data.frame(Q_diet_diversity_vietnam_long))
          .na("Invalid answer. Your answer should be a data frame")
        
        if (ncol(Q_diet_diversity_vietnam_long) != 6)
          .fail("Wrong. Your answer should have six columns.")
        
        if (!all(names(Q_diet_diversity_vietnam_long) %in% names(.Q_diet_diversity_vietnam_long)) )
          .fail("Wrong. Your answer should have the following columns: household_id, enerc_kcal_w, dry_w, water_w, fat_w, visit")
        
        if (isTRUE(all.equal(.Q_diet_diversity_vietnam_long,
                             Q_diet_diversity_vietnam_long, ignore_col_order = T, ignore_row_order = T)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_diet_diversity_vietnam_long <- function() {
  '
HINT.
  
  Begin by renaming the columns using the `rename` function from the `tidyverse` so that they have a double underscore separator. 
  - Then, use `pivot_longer` and specify the column range `2:9`.
  - Use `names_sep = "__"` and `names_to = c(".value", "visit")` to format the names.
  ' -> out
  cat(out)
}
.SOLUTION_Q_diet_diversity_vietnam_long <- function() {
  '

  
  diet_diversity_vietnam_wide%>%
      rename(
        enerc_kcal_w__1 = enerc_kcal_w_1,
        enerc_kcal_w__2 = enerc_kcal_w_2,
        dry_w__1 = dry_w_1,
        dry_w__2 = dry_w_2,
        water_w__1 = water_w_1,
        water_w__2 = water_w_2,
        fat_w__1 = fat_w_1,
        fat_w__2 = fat_w_2
      ) %>%  
      pivot_longer(2:9, names_sep = "__", names_to = c(".value", "visit"))
  ' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_tb_visit_wide ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_tb_visit_wide <-
  function() {
    
    .problem_number <<- 5
    
    .tb_visits_long <- 
      suppressMessages(read_csv(here("data/india_tb_pathways_and_costs_data.csv"))) %>% 
      clean_names() %>% 
      select(id, first_visit_location, first_visit_cost, 
             second_visit_location, second_visit_cost, 
             third_visit_location, third_visit_cost) %>% 
      rename(first__visit_location = first_visit_location, 
             first__visit_cost = first_visit_cost, 
             second__visit_location = second_visit_location, 
             second__visit_cost= second_visit_cost, 
             third__visit_location = third_visit_location, 
             third__visit_cost = third_visit_cost) %>% 
      pivot_longer(2:7, 
                   names_to = c("visit_count", ".value"), 
                   names_sep = "__")
    
    
    .Q_tb_visit_wide <- 
      .tb_visits_long %>%
      pivot_wider(names_from = visit_count,
                  values_from = c(visit_location, visit_cost)) %>%
      rename(
        c1 = 1,
        c2 = 2,
        c3 = 3,
        c4 = 4,
        c5 = 5,
        c6 = 6,
        c7 = 7
      )
    
    .autograder <<-
      function() {
        
        
        Q_tb_visit_wide_renamed <- 
          Q_tb_visit_wide %>% 
          rename(
            c1 = 1,
            c2 = 2,
            c3 = 3,
            c4 = 4,
            c5 = 5,
            c6 = 6,
            c7 = 7
          )
        
        
        
        
        if (!exists("Q_tb_visit_wide"))
          .na("You have not yet defined the answer object, `Q_tb_visit_wide`.")
        
        if (!is.data.frame(Q_tb_visit_wide_renamed))
          .na("Invalid answer. Your answer should be a data frame")
        
        if (!ncol(Q_tb_visit_wide_renamed) == ncol(.Q_tb_visit_wide))
          .fail(glue::glue("Wrong. Your answer should have {ncol(.Q_tb_visit_wide)} columns."))
        
        if (! all(names(Q_tb_visit_wide_renamed) %in% names(.Q_tb_visit_wide)) )
          .fail(paste0("Wrong. Your answer should have the following columns:", 
                       paste0(names(.Q_tb_visit_wide), collapse = ", ")
          ))
        
        if (isTRUE(suppressMessages(all.equal(Q_tb_visit_wide_renamed,
                                              .Q_tb_visit_wide, ignore_col_order = T, ignore_row_order = T))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_tb_visit_wide <- function() {
  '
HINT.
  
  Begin by using the `pivot_wider` function from the `tidyverse` package on the `tb_visits_long` dataset. 
  You"ll want to specify which columns will provide the new column names (`names_from`) and from which columns to get the values (`values_from`).
  After the pivot operation, use the `rename` function to change the column names as required.
  
  tb_visits_long %>%
    pivot_wider(_________)
  ' -> out
  cat(out)
}


.SOLUTION_Q_tb_visit_wide <- function() {
  '

  
  tb_visits_long %>%
      pivot_wider(names_from = visit_count,
                  values_from = c(visit_location, visit_cost))
      )' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 5)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_adult_long ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_adult_long <-
  function() {
    
    .problem_number <<- 1
    
    .adult_stats <- 
      tibble::tribble(
        ~adult,  ~year1_BMI,  ~year2_BMI,  ~year1_HIV,  ~year2_HIV,
        "A",          25,          30,  "Positive",  "Positive",
        "B",          34,          28,  "Negative",  "Positive",
        "C",          19,          17,  "Negative",  "Negative"
      )
    
    .Q_adult_long <- 
      .adult_stats %>%
      pivot_longer(cols = 2:5,
                   names_sep = "_",
                   names_to = c("year", ".value"))
    
    .autograder <<-
      function() {
        if (!exists("Q_adult_long"))
          .na("You have not yet defined the answer object, `Q_adult_long`.")
        
        if (!is.data.frame(Q_adult_long))
          .na("Invalid answer. Your answer should be a data frame")
        
        if (ncol(Q_adult_long) != 4)
          .fail("Wrong. Your answer should have four columns.")
        
        if (!all(names(Q_adult_long) %in% names(.Q_adult_long)) )
          .fail("Wrong. Your answer should have the following columns: adult, year, BMI, HIV")
        
        if (isTRUE(all.equal(.Q_adult_long,
                             Q_adult_long, ignore_col_order = T, ignore_row_order = T)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_adult_long <- function() {
  '
HINT.
  
  Use the `pivot_longer` function from the `tidyverse`. 
  - Specify `cols = 2:5` to select the columns to pivot. 
  - Use `names_sep = "_"` and `names_to = c("year", ".value")` to format the names.
  ' -> out
  cat(out)
}


.SOLUTION_Q_adult_long <- function() {
  '

  
  adult_stats %>%
      pivot_longer(cols = 2:5,
                   names_sep = "_",
                   names_to = c("year", ".value"))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_growth_stats_long ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_growth_stats_long <-
  function() {
    
    .problem_number <<- 2
    
    .growth_stats <- 
      tibble::tribble(
        ~child,~yr1_head,~yr2_head,~yr1_neck,~yr2_neck,~yr1_hip,~yr2_hip,
        "a",       45,       48,       23,       24,      51,      52,
        "b",       48,       50,       24,       26,      52,      52,
        "c",       50,       52,       24,       27,      53,      54
      )
    
    .Q_growth_stats_long <- 
      .growth_stats %>%
      pivot_longer(cols = 2:7,
                   names_to = c("year", ".value"),
                   names_sep = "_")
    
    .autograder <<-
      function() {
        if (!exists("Q_growth_stats_long"))
          .na("You have not yet defined the answer object, `Q_growth_stats_long`.")
        
        if (!is.data.frame(Q_growth_stats_long))
          .na("Invalid answer. Your answer should be a data frame")
        
        if (ncol(Q_growth_stats_long) != 5)
          .fail("Wrong. Your answer should have five columns.")
        
        if (!all(names(Q_growth_stats_long) %in% names(.Q_growth_stats_long)) )
          .fail("Wrong. Your answer should have the following columns: child, year, head, neck, hip")
        
        if (isTRUE(all.equal(.Q_growth_stats_long,
                             Q_growth_stats_long, ignore_col_order = T, ignore_row_order = T)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_growth_stats_long <- function() {
  '
HINT.
  
  Use the `pivot_longer` function from the `tidyverse`. 
  - Specify `cols = 2:7` to select the columns to pivot. 
  - Use `names_to = c("year", ".value")` and `names_sep = "_"` to format the names.
  ' -> out
  cat(out)
}

.SOLUTION_Q_growth_stats_long <- function() {
  '

  
  growth_stats %>%
     pivot_longer(cols = 2:7,
                   names_to = c("year", ".value"),
                   names_sep = "_")' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_adult2_long ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_adult2_long <-
  function() {
    
    .problem_number <<- 3
    
    .adult_stats_dot_sep <- 
      tibble::tribble(
        ~adult,  ~`BMI.year1`,  ~`BMI.year2`,  ~`HIV.year1`,  ~`HIV.year2`,
        "A",            25,            30,    "Positive",   "Positive",
        "B",            34,            28,    "Negative",   "Positive",
        "C",            19,            17,    "Negative",   "Negative"
      )
    
    .Q_adult2_long <- 
      .adult_stats_dot_sep %>%
      pivot_longer(cols = 2:5,
                   names_sep = "\\.",
                   names_to = c(".value", "year"))
    
    .autograder <<-
      function() {
        if (!exists("Q_adult2_long"))
          .na("You have not yet defined the answer object, `Q_adult2_long`.")
        
        if (!is.data.frame(Q_adult2_long))
          .na("Invalid answer. Your answer should be a data frame")
        
        if (ncol(Q_adult2_long) != 4)
          .fail("Wrong. Your answer should have four columns.")
        
        if (!all(names(Q_adult2_long) %in% names(.Q_adult2_long)) )
          .fail("Wrong. Your answer should have the following columns: adult, BMI, HIV, year")
        
        if (isTRUE(all.equal(.Q_adult2_long,
                             Q_adult2_long, ignore_col_order = T, ignore_row_order = T)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_adult2_long <- function() {
  '
HINT.
  
  Use the `pivot_longer` function from the `tidyverse`. 
  - Specify `cols = 2:5` to select the columns to pivot. 
  - Remember to correctly escape the dot character using `names_sep = "\\."` 
  and use `names_to = c(".value", "year")` to format the names.
  ' -> out
  cat(out)
}


.SOLUTION_Q_adult2_long <- function() {
  '

  
  adult_stats_dot_sep %>%
      pivot_longer(cols = 2:5,
                   names_sep = "\\.",
                   names_to = c(".value", "year"))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_diet_diversity_vietnam_long ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_diet_diversity_vietnam_long <-
  function() {
    
    .problem_number <<- 4
    
    .diet_diversity_vietnam_wide <- suppressMessages( read_csv(here("data/diet_diversity_vietnam_wide.csv")))
    
    .Q_diet_diversity_vietnam_long <- 
      .diet_diversity_vietnam_wide%>%
      rename(
        enerc_kcal_w__1 = enerc_kcal_w_1,
        enerc_kcal_w__2 = enerc_kcal_w_2,
        dry_w__1 = dry_w_1,
        dry_w__2 = dry_w_2,
        water_w__1 = water_w_1,
        water_w__2 = water_w_2,
        fat_w__1 = fat_w_1,
        fat_w__2 = fat_w_2
      ) %>%  
      pivot_longer(2:9, names_sep = "__", names_to = c(".value", "visit"))
    
    .autograder <<-
      function() {
        if (!exists("Q_diet_diversity_vietnam_long"))
          .na("You have not yet defined the answer object, `Q_diet_diversity_vietnam_long`.")
        
        if (!is.data.frame(Q_diet_diversity_vietnam_long))
          .na("Invalid answer. Your answer should be a data frame")
        
        if (ncol(Q_diet_diversity_vietnam_long) != 6)
          .fail("Wrong. Your answer should have six columns.")
        
        if (!all(names(Q_diet_diversity_vietnam_long) %in% names(.Q_diet_diversity_vietnam_long)) )
          .fail("Wrong. Your answer should have the following columns: household_id, enerc_kcal_w, dry_w, water_w, fat_w, visit")
        
        if (isTRUE(all.equal(.Q_diet_diversity_vietnam_long,
                             Q_diet_diversity_vietnam_long, ignore_col_order = T, ignore_row_order = T)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_diet_diversity_vietnam_long <- function() {
  '
HINT.
  
  Begin by renaming the columns using the `rename` function from the `tidyverse` so that they have a double underscore separator. 
  - Then, use `pivot_longer` and specify the column range `2:9`.
  - Use `names_sep = "__"` and `names_to = c(".value", "visit")` to format the names.
  ' -> out
  cat(out)
}
.SOLUTION_Q_diet_diversity_vietnam_long <- function() {
  '

  
  diet_diversity_vietnam_wide%>%
      rename(
        enerc_kcal_w__1 = enerc_kcal_w_1,
        enerc_kcal_w__2 = enerc_kcal_w_2,
        dry_w__1 = dry_w_1,
        dry_w__2 = dry_w_2,
        water_w__1 = water_w_1,
        water_w__2 = water_w_2,
        fat_w__1 = fat_w_1,
        fat_w__2 = fat_w_2
      ) %>%  
      pivot_longer(2:9, names_sep = "__", names_to = c(".value", "visit"))
  ' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_tb_visit_wide ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_tb_visit_wide <-
  function() {
    
    .problem_number <<- 5
    
    .tb_visits_long <- 
      suppressMessages(read_csv(here("data/india_tb_pathways_and_costs_data.csv"))) %>% 
      clean_names() %>% 
      select(id, first_visit_location, first_visit_cost, 
             second_visit_location, second_visit_cost, 
             third_visit_location, third_visit_cost) %>% 
      rename(first__visit_location = first_visit_location, 
             first__visit_cost = first_visit_cost, 
             second__visit_location = second_visit_location, 
             second__visit_cost= second_visit_cost, 
             third__visit_location = third_visit_location, 
             third__visit_cost = third_visit_cost) %>% 
      pivot_longer(2:7, 
                   names_to = c("visit_count", ".value"), 
                   names_sep = "__")
    
    
    .Q_tb_visit_wide <- 
      .tb_visits_long %>%
      pivot_wider(names_from = visit_count,
                  values_from = c(visit_location, visit_cost)) %>%
      rename(
        c1 = 1,
        c2 = 2,
        c3 = 3,
        c4 = 4,
        c5 = 5,
        c6 = 6,
        c7 = 7
      )
    
    .autograder <<-
      function() {
        
        
        Q_tb_visit_wide_renamed <- 
          Q_tb_visit_wide %>% 
          rename(
            c1 = 1,
            c2 = 2,
            c3 = 3,
            c4 = 4,
            c5 = 5,
            c6 = 6,
            c7 = 7
          )
        
        
        
        
        if (!exists("Q_tb_visit_wide"))
          .na("You have not yet defined the answer object, `Q_tb_visit_wide`.")
        
        if (!is.data.frame(Q_tb_visit_wide_renamed))
          .na("Invalid answer. Your answer should be a data frame")
        
        if (!ncol(Q_tb_visit_wide_renamed) == ncol(.Q_tb_visit_wide))
          .fail(glue::glue("Wrong. Your answer should have {ncol(.Q_tb_visit_wide)} columns."))
        
        if (! all(names(Q_tb_visit_wide_renamed) %in% names(.Q_tb_visit_wide)) )
          .fail(paste0("Wrong. Your answer should have the following columns:", 
                       paste0(names(.Q_tb_visit_wide), collapse = ", ")
          ))
        
        if (isTRUE(suppressMessages(all.equal(Q_tb_visit_wide_renamed,
                                              .Q_tb_visit_wide, ignore_col_order = T, ignore_row_order = T))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_tb_visit_wide <- function() {
  '
HINT.
  
  Begin by using the `pivot_wider` function from the `tidyverse` package on the `tb_visits_long` dataset. 
  You"ll want to specify which columns will provide the new column names (`names_from`) and from which columns to get the values (`values_from`).
  After the pivot operation, use the `rename` function to change the column names as required.
  
  tb_visits_long %>%
    pivot_wider(_________)
  ' -> out
  cat(out)
}


.SOLUTION_Q_tb_visit_wide <- function() {
  '

  
  tb_visits_long %>%
      pivot_wider(names_from = visit_count,
                  values_from = c(visit_location, visit_cost))
      )' -> out
  cat(out)
}
