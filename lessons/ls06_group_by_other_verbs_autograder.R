if(!require('pacman')) install.packages('pacman')
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(praise,
               tidyverse,
               dplyr)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  DATA ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.sarcopenia <- read_csv(here::here('data/sarcopenia_elderly.csv'))


.CHECK_Q_grip_strength_arranged <- function() invisible(NULL)
.HINT_Q_grip_strength_arranged<- function() invisible(NULL)

.CHECK_Q_age_group_height <- function() invisible(NULL)
.HINT_Q_age_group_height <- function() invisible(NULL)

.CHECK_Q_max_skeletal_muscle_index <- function() invisible(NULL)
.HINT_Q_max_skeletal_muscle_index <- function() invisible(NULL)

.CHECK_Q_rank_grip_strength <- function() invisible(NULL)
.HINT_Q_rank_grip_strength <- function() invisible(NULL)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 4)   # Put total number of questions as `times` argument

.NUM_Q_grip_strength_arranged <- 1
.NUM_Q_age_group_height <- 2
.NUM_Q_max_skeletal_muscle_index <- 3
.NUM_Q_rank_grip_strength <- 4

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  ANSWERS ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_grip_strength_arranged ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_grip_strength_arranged <-
  function() {
    
    .problem_number <<- .NUM_Q_grip_strength_arranged
    correct_answer <- .sarcopenia %>%
      select(sex_male_1_female_0, grip_strength_kg) %>%
      arrange(sex_male_1_female_0, grip_strength_kg)
    
    Q_grip_strength_arranged <- .Last.value
    
    .autograder <<-
      function(){
        if(!exists("Q_grip_strength_arranged"))
          .na("You have not yet defined the answer object, `Q_grip_strength_arranged`.")
        if (!is.data.frame(Q_grip_strength_arranged))
          .na("Invalid answer. Your answer should be a data frame.")
        
        if (isTRUE(all.equal(Q_grip_strength_arranged, correct_answer, ignore_row_order = F)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_grip_strength_arranged <- function(){
  '
HINT.
Your answer should look this: 

Q_grip_strength_arranged <- 
  sarcopenia %>% 
  select(VARIABLES) %>%
  arrange(VARIABLES)' -> out
  cat(out)
}

.SOLUTION_Q_grip_strength_arranged <- function(){
  '

Q_grip_strength_arranged <- 
  sarcopenia %>%
  select(sex_male_1_female_0, grip_strength_kg) %>%
  arrange(sex_male_1_female_0, grip_strength_kg)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_age_group_height ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_age_group_height <-
  function() {
    
    .problem_number <<- .NUM_Q_age_group_height
    correct_answer <- .sarcopenia %>%
      mutate(age_group = factor(age_group, levels = c("Sixties",
                                                      "Seventies",
                                                      "Eighties"))) %>%
      arrange(age_group, height_meters)
    
    .autograder <<-
      function(){
        if(!exists("Q_age_group_height"))
          .na("You have not yet defined the answer object, `Q_age_group_height`.")
        if (!is.data.frame(Q_age_group_height))
          .na("Invalid answer. Your answer should be a data frame.")
        
        if (isTRUE(all.equal(Q_age_group_height, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_age_group_height <- function(){
  '
HINT.
Your answer should look this: 

Q_age_group_height <- 
  sarcopenia %>% 
  mutate(age_group = TRANSFORMATION) %>%
  arrange(VARIABLES)' -> out
  cat(out)
}

.SOLUTION_Q_age_group_height <- function(){
  '

Q_age_group_height <- 
  sarcopenia %>%
  mutate(age_group = factor(age_group, levels = c("Sixties",
                                                  "Seventies",
                                                  "Eighties"))) %>%
  arrange(age_group, height_meters)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_max_skeletal_muscle_index ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_max_skeletal_muscle_index <-
  function() {
    
    .problem_number <<- .NUM_Q_max_skeletal_muscle_index
    correct_answer <- .sarcopenia %>%
      group_by(age_group,sex_male_1_female_0) %>%
      filter(skeletal_muscle_index == max(skeletal_muscle_index))
      
    
    .autograder <<-
      function(){
        if(!exists("Q_max_skeletal_muscle_index"))
          .na("You have not yet defined the answer object, `Q_max_skeletal_muscle_index`.")
        if (!is.data.frame(Q_max_skeletal_muscle_index))
          .na("Invalid answer. Your answer should be a data frame.")
        
        if (isTRUE(all.equal(Q_max_skeletal_muscle_index, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_max_skeletal_muscle_index <- function(){
  '
HINT.
Your answer should look this: 

Q_max_skeletal_muscle_index <- 
  sarcopenia %>% 
  group_by(VARIABLES) %>%
  filter(FORMULA)' -> out
  cat(out)
}

.SOLUTION_Q_max_skeletal_muscle_index <- function(){
  '

Q_max_skeletal_muscle_index <- 
  sarcopenia %>%
  group_by(age_group,sex_male_1_female_0) %>%
  filter(skeletal_muscle_index == max(skeletal_muscle_index))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_rank_grip_strength ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_rank_grip_strength <-
  function() {
    
    .problem_number <<- .NUM_Q_rank_grip_strength
    correct_answer <- .sarcopenia %>%
      group_by(age_group) %>%
      mutate(grip_strength_rank = rank(grip_strength_kg))
    
    
    .autograder <<-
      function(){
        if(!exists("Q_rank_grip_strength"))
          .na("You have not yet defined the answer object, `Q_rank_grip_strength`.")
        if (!is.data.frame(Q_rank_grip_strength))
          .na("Invalid answer. Your answer should be a data frame.")
        
        if (isTRUE(all.equal(Q_rank_grip_strength, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_rank_grip_strength <- function(){
  '
HINT.
Your answer should look this: 

Q_rank_grip_strength <- 
  sarcopenia %>% 
  group_by(VARIABLE) %>%
  mutate(grip_strength_rank = FORMULA)' -> out
  cat(out)
}

.SOLUTION_Q_rank_grip_strength <- function(){
  '

Q_rank_grip_strength <- 
  sarcopenia %>%
  group_by(age_group) %>%
  mutate(grip_strength_rank = rank(grip_strength_kg))' -> out
  cat(out)
}
