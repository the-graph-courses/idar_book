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
## ~  Q_data_type ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_data_type <-
  function() {
    
    .problem_number <<- 1
    
    correct_answer <- "wide"
    
    .autograder <<-
      function() {
        if (!exists("Q_data_type"))
          .na("You have not yet defined the answer object, `Q_data_type`.")
        
        if (!is.character(Q_data_type))
          .na("Invalid answer. Your answer should be a single character string")
        
        if (isTRUE(identical(tolower(trimws(Q_data_type)),
                             correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_data_type <- function() {
  '
HINT.
  Each observational unit (each country) occupies just one row
' -> out
  cat(out)
}

.SOLUTION_Q_data_type <- function() {
  '

  "Wide"' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_euro_births_long ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_euro_births_long <-
  function() {
    
    .problem_number <<- 2
    
    .euro_births_wide <- suppressMessages(read_csv(here("data/euro_births_wide.csv")))
    
    .Q_euro_births_long <<- 
      .euro_births_wide %>% 
      pivot_longer(2:8, 
                   names_to = "year", 
                   values_to = "births_count")
    
    
    .autograder <<-
      function() {
        if (!exists("Q_euro_births_long"))
          .na("You have not yet defined the answer object, `Q_euro_births_long`.")
        
        if (!is.data.frame(Q_euro_births_long))
          .na("Invalid answer. Your answer should be a data frame")
        
        if (ncol(Q_euro_births_long) != 3)
          .fail("Wrong. Your answer should have three columns.")
        
        if (!all(names(Q_euro_births_long) %in% names(.Q_euro_births_long)) )
          .fail("Wrong. Your answer should have the following columns: country, year and births_count")
        
        if (isTRUE(all.equal(.Q_euro_births_long,
                             Q_euro_births_long, ignore_col_order = T, ignore_row_order = T)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_euro_births_long <- function() {
  '
HINT.
  
  Your code should look like this:
  
  euro_births_wide %>% 
      pivot_longer(COLS_TO_PIVOT, 
                   names_to = NAMES_COL_NAME, 
                   values_to = VALUES_COL_NAME)' -> out
  cat(out)
}

.SOLUTION_Q_euro_births_long <- function() {
  '

  euro_births_wide %>% 
      pivot_longer(2:8, 
                   names_to = "year", 
                   values_to = "births_count")
' -> out
  cat(out)
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_population_widen ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_population_widen <-
  function() {
    
    .problem_number <<- 3
    
    
    .Q_population_widen <- 
      tidyr::population %>% 
      pivot_wider(names_from = year, 
                  values_from = population)
    
    
    .autograder <<-
      function() {
        if (!exists("Q_population_widen"))
          .na("You have not yet defined the answer object, `Q_population_widen`.")
        
        if (!is.data.frame(Q_population_widen))
          .na("Invalid answer. Your answer should be a data frame")
        
        if (!ncol(Q_population_widen) == ncol(.Q_population_widen))
          .fail(glue::glue("Wrong. Your answer should have {ncol(.Q_population_widen)} columns."))
        
        if (! all(names(Q_population_widen) %in% names(.Q_population_widen)) )
          .fail(paste0("Wrong. Your answer should have the following columns:", 
                       paste0(names(.Q_population_widen), collapse = ", ")
          ))
        
        if (isTRUE(suppressMessages(all.equal(Q_population_widen,
                                              .Q_population_widen, ignore_col_order = T, ignore_row_order = T))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_population_widen <- function() {
  '
HINT.
  
  Your code should look like this:
  
  tidyr::population %>% 
      pivot_wider(names_from = COLUMN_CONTAINING_FUTURE_COLUMN_NAMES,
                   values_from = COLUMN_CONTAINING_THE_VALUES_TO_BE_PIVOTED)' -> out
  cat(out)
}

.SOLUTION_Q_population_widen <- function() {
  '

  
  tidyr::population %>% 
      pivot_wider(names_from = year,
                   values_from = population)' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_population_max ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_population_max <-
  function() {
    
    .problem_number <<- 4
    
    
    .Q_population_max <- 
      tidyr::population %>% 
      group_by(country) %>% 
      filter(population == max(population)) %>% 
      ungroup()
    
    
    .autograder <<-
      function() {
        if (!exists("Q_population_max"))
          .na("You have not yet defined the answer object, `Q_population_max`.")
        
        if (!is.data.frame(Q_population_max))
          .na("Invalid answer. Your answer should be a data frame")
        
        if (!nrow(Q_population_max) == nrow(.Q_population_max))
          .fail(glue::glue("Wrong. Your answer should have {nrow(.Q_population_max)} rows"))
        
        if (! all(names(Q_population_max) %in% names(.Q_population_max)) )
          .fail(paste0("Wrong. Your answer should have the following columns:", 
                       paste0(names(.Q_population_max), collapse = ", ")
          ))
        
        if (isTRUE(all.equal(Q_population_max,
                             .Q_population_max, ignore_col_order = T, ignore_row_order = T)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_population_max <- function() {
  '
HINT.
  
  Your code should look like this:
  
  tidyr::population %>% 
    group_by(country) %>% 
    FILTER_FOR_THE_MAX_POPULATION_PER_GROUP
    ungroup()' -> out
  cat(out)
}

.SOLUTION_Q_population_max <- function() {
  '

  
  tidyr::population %>% 
    group_by(country) %>% 
    filter(population == max(population)) %>% 
    ungroup()' -> out
  cat(out)
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_population_summaries ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_population_summaries <-
  function() {
    
    .problem_number <<- 5
    
    .Q_population_summaries <- 
      population %>% 
      group_by(country) %>% 
      summarise(max_population = max(population), 
                min_population = min(population), 
                mean_population = mean(population))
    
    
    .autograder <<-
      function() {
        if (!exists("Q_population_summaries"))
          .na("You have not yet defined the answer object, `Q_population_summaries`.")
        
        if (!is.data.frame(Q_population_summaries))
          .na("Invalid answer. Your answer should be a data frame")
        
        if (!nrow(Q_population_summaries) == nrow(.Q_population_summaries))
          .fail(glue::glue("Wrong. Your answer should have {nrow(.Q_population_summaries)} rows"))
        
        if (! all(names(Q_population_summaries) %in% names(.Q_population_summaries)) )
          .fail(paste0("Wrong. Your answer should have the following columns:", 
                       paste0(names(.Q_population_summaries), collapse = ", ")
          ))
        
        if (isTRUE(all.equal(Q_population_summaries,
                             .Q_population_summaries, ignore_col_order = T, ignore_row_order = T)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_population_summaries <- function() {
  '
HINT.
  
  Your code should look like this:
  
  population %>% 
  group_by(country) %>% 
  summarise(max_population = , 
            min_population = , 
            mean_population = )' -> out
  cat(out)
}

.SOLUTION_Q_population_summaries <- function() {
  '

  
  population %>% 
  group_by(country) %>% 
  summarise(max_population = max(population), 
            min_population = min(population), 
            mean_population = mean(population))' -> out
  cat(out)
}

