if(!require('pacman')) install.packages('pacman')

pacman::p_install_gh('graph-courses/autograder')
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(here, 
               glue,
               praise,
               janitor,
               tidyverse)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  DATA ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.sarcopenia <- read_csv(here('data/sarcopenia_elderly.csv'))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 6)   # Put total number of questions as `times` argument

.NUM_Q_weight_to_g <- 1
.NUM_Q_sarcopenia_resp_id <- 2
.NUM_Q_women_low_grip_strength <- 3
.NUM_Q_prop_women_low_grip_strength <- 4
.NUM_Q_asm_calculation <- 5
.NUM_Q_age_integer <- 6

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_weight_to_g ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_weight_to_g <-
  function() {
    
    .problem_number <<- .NUM_Q_weight_to_g
    correct_answer <- .sarcopenia %>% mutate(weight_grams = weight_kg * 1000)
    
    .autograder <<-
      function(){
        if(!exists("Q_weight_to_g"))
          .na("You have not yet defined the answer object, `Q_weight_to_g`.")
        if (!is.data.frame(Q_weight_to_g))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"weight_grams" %in% names(Q_weight_to_g))
          .fail("Your answer should have a column called 'weight_grams'.")
        if (isTRUE(all.equal(Q_weight_to_g$weight_grams, correct_answer$weight_grams/1000000)))
          .fail("It seems you used the formula `weight_kg/1000` instead of `weight_kg * 1000`")
        
        
        if (isTRUE(all.equal(select(Q_weight_to_g, weight_grams) , 
                             select(correct_answer, weight_grams))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_weight_to_g <- function(){
'
HINT.
Your answer should look this: 

Q_weight_to_g <- 
  sarcopenia %>% 
  mutate(weight_grams = FORMULA_HERE)' -> out
  cat(out)
}

.SOLUTION_Q_weight_to_g <- function(){
'

Q_weight_to_g <- 
  sarcopenia %>% 
  mutate(weight_grams = weight_kg*1000)' -> out
  cat(out)
}

# Tests
# 
# Q_weight_to_g <-
#   sarcopenia %>%
#   _____________________
# .CHECK_Q_weight_to_g()
# 
# Q_weight_to_g <-
#   "sarcopenia"
# .CHECK_Q_weight_to_g()
# 
# Q_weight_to_g <-
#   sarcopenia %>%
#   select(weight_kg) %>%
#   mutate(weight_grams = weight_kg * 1000)
# .CHECK_Q_weight_to_g()
# 
# Q_weight_to_g <-
#   sarcopenia %>%
#   mutate(weight_grammes = weight_kg * 1000)
# .CHECK_Q_weight_to_g()
# 
# Q_weight_to_g <-
#   sarcopenia %>%
#   mutate(weight_grams = weight_kg / 1000)
# .CHECK_Q_weight_to_g()
# 
# Q_weight_to_g <-
#   sarcopenia %>%
#   mutate(weight_grams = weight_kg * 1000)
# .CHECK_Q_weight_to_g()
# 
# 
# .HINT_Q_weight_to_g()
# .SOLUTION_Q_weight_to_g()


# 
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## ~  Q_sarcopenia_resp_id ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.CHECK_Q_sarcopenia_resp_id <-
  function() {

    .problem_number <<- .NUM_Q_sarcopenia_resp_id
    correct_answer <- .sarcopenia %>% mutate(respondent_id = row_number())

    .autograder <<-
      function(){
        if(!exists("Q_sarcopenia_resp_id"))
          .na("You have not yet defined the answer object, `Q_sarcopenia_resp_id`.")
        if (!is.data.frame(Q_sarcopenia_resp_id))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"respondent_id" %in% names(Q_sarcopenia_resp_id))
          .fail("Your answer should have a column called 'respondent_id'.")

        if (isTRUE(all.equal(select(Q_sarcopenia_resp_id, respondent_id) ,
                             select(correct_answer, respondent_id))))
          .pass()

        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_sarcopenia_resp_id <- function(){
  '
HINT
Your answer should look like this:

Q_sarcopenia_resp_id <-
  sarcopenia %>%
  mutate(respondent_id = FORMULA_HERE)' -> out
  cat(out)
}

.SOLUTION_Q_sarcopenia_resp_id  <- function(){
  '

Q_sarcopenia_resp_id <-
  sarcopenia %>%
  mutate(respondent_id = 1:n())' -> out
  cat(out)
}


# Tests

# Q_sarcopenia_resp_id <-
#   sarcopenia %>%
#   _____________________
# .CHECK_Q_sarcopenia_resp_id()
# 
# Q_sarcopenia_resp_id <-
#   "sarcopenia"
# .CHECK_Q_sarcopenia_resp_id()
# 
# Q_sarcopenia_resp_id <-
#   sarcopenia %>%
#   mutate(respondent_id = 1:n())
# .CHECK_Q_sarcopenia_resp_id()
# 
# .HINT_Q_sarcopenia_resp_id()
# .SOLUTION_Q_sarcopenia_resp_id()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_sarcopenia_grip_strength ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.CHECK_Q_sarcopenia_grip_strength <-
  function() {
    
    .problem_number <<- .NUM_Q_sarcopenia_grip_strength
    correct_answer <- .sarcopenia %>% mutate(grip_strength_rank = rank(desc(grip_strength_kg), ties.method = "min"))
    incorrect_forgot_ties <-  .sarcopenia %>% mutate(grip_strength_rank = rank(desc(grip_strength_kg))) 
    incorrect_forgot_descending <-  .sarcopenia %>% mutate(grip_strength_rank = rank(grip_strength_kg, ties.method = "min")) 
    incorrect_forgot_descending_and_ties <-  .sarcopenia %>% mutate(grip_strength_rank = rank(grip_strength_kg))
    
    .autograder <<-
      function(){
        if(!exists("Q_sarcopenia_grip_strength"))
          .na("You have not yet defined the answer object, `Q_sarcopenia_grip_strength`.")
        
        if (!is.data.frame(Q_sarcopenia_grip_strength))
          .na("Invalid answer. Your answer should be a data frame.")
        
        if (!"grip_strength_rank" %in% names(Q_sarcopenia_grip_strength))
          .fail("Your answer should have a column called 'grip_strength_rank'.")
        
        if (isTRUE(all.equal(select(Q_sarcopenia_grip_strength, grip_strength_rank) ,
                             select(incorrect_forgot_ties, grip_strength_rank))))
          .fail("Wrong answer. It seems you forgot to specify `ties_method = 'min'`")
        
        if (isTRUE(all.equal(select(Q_sarcopenia_grip_strength, grip_strength_rank) ,
                             select(incorrect_forgot_descending, grip_strength_rank))))
          .fail("Wrong answer. It seems you forgot to wrap the `grip_strength_kg` variable in `desc()`")
        
        if (isTRUE(all.equal(select(Q_sarcopenia_grip_strength, grip_strength_rank) ,
                             select(incorrect_forgot_descending_and_ties, grip_strength_rank))))
          .fail("Wrong answer. It seems you forgot to wrap the `grip_strength_kg` variable in `desc()` and you forgot to specify `ties_method = 'min'`")
        
        if (isTRUE(all.equal(select(Q_sarcopenia_grip_strength, grip_strength_rank) ,
                             select(correct_answer, grip_strength_rank))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_sarcopenia_grip_strength <- function(){
  '
HINT
Your answer should have the form:

Q_sarcopenia_grip_strength <-
  sarcopenia %>%
  mutate(grip_strength_rank = RANKING_CODE)
  
where RANKING_CODE should rank `grip_strength_kg` in descending order  with `ties.method` set to "min"
' -> out
  cat(out) 
}

.SOLUTION_Q_sarcopenia_grip_strength <- function(){
  '

Q_sarcopenia_grip_strength <-
  sarcopenia %>%
  mutate(grip_strength_rank = rank(desc(grip_strength_kg)), ties.method = "min")' -> out
  cat(out)
}


# # Tests
# 
# Q_sarcopenia_grip_strength <-
#   sarcopenia %>%
#   _____________________
# .CHECK_Q_sarcopenia_grip_strength()
# 
# Q_sarcopenia_grip_strength <-
#   "sarcopenia"
# .CHECK_Q_sarcopenia_grip_strength()
# 
# Q_sarcopenia_grip_strength <-
#   sarcopenia %>%
#   select(weight_kg) %>%
#   mutate(grip_strength_rank = weight_kg * 1000)
# .CHECK_Q_sarcopenia_grip_strength()
# 
# Q_sarcopenia_grip_strength <-
#   sarcopenia %>%
#   mutate(grip_strength_rank = rank(grip_strength_kg))
# .CHECK_Q_sarcopenia_grip_strength()
# 
# Q_sarcopenia_grip_strength <-
#   sarcopenia %>%
#   mutate(grip_strength_rank = rank(desc(grip_strength_kg)))
# .CHECK_Q_sarcopenia_grip_strength()
# 
# Q_sarcopenia_grip_strength <-
#   sarcopenia %>%
#   mutate(grip_strength_rank = rank(grip_strength_kg, ties.method = "min"))
# .CHECK_Q_sarcopenia_grip_strength()
# 
# Q_sarcopenia_grip_strength <-
#   sarcopenia %>%
#   mutate(grip_strength_rank = rank(desc(grip_strength_kg), ties.method = "min"))
# .CHECK_Q_sarcopenia_grip_strength()
# 
# .HINT_Q_sarcopenia_grip_strength()
# .SOLUTION_Q_sarcopenia_grip_strength()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_women_low_grip_strength ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.CHECK_Q_women_low_grip_strength <-
  function() {

    .problem_number <<- .NUM_Q_women_low_grip_strength
    correct_answer <- 
      .sarcopenia %>%
      filter(sex_male_1_female_0 == 0) %>%
      mutate(low_grip_strength = grip_strength_kg < 20)
    
    incorrect_less_than_equal_to <-  
      .sarcopenia %>%
      filter(sex_male_1_female_0 == 0) %>%
      mutate(low_grip_strength = grip_strength_kg <= 20)
    
    incorrect_greater_than <-  
      .sarcopenia %>%
      filter(sex_male_1_female_0 == 0) %>%
      mutate(low_grip_strength = grip_strength_kg > 20)
    
    incorrect_greater_than_equal_to <-  
      .sarcopenia %>%
      filter(sex_male_1_female_0 == 0) %>%
      mutate(low_grip_strength = grip_strength_kg >= 20)

    .autograder <<-
      function(){
        if(!exists("Q_women_low_grip_strength"))
          .na("You have not yet defined the answer object, `Q_women_low_grip_strength`.")

        if (!is.data.frame(Q_women_low_grip_strength))
          .na("Invalid answer. Your answer should be a data frame.")

        if (!"low_grip_strength" %in% names(Q_women_low_grip_strength))
          .fail("Your answer should have a column called 'low_grip_strength'.")
        
        if (nrow(Q_women_low_grip_strength) == nrow(.sarcopenia))
          .fail("Wrong answer. It seems you did not filter for just women in the dataset. 
                To filter for women, the code `filter(sex_male_1_female_0 == 0)` should be in your pipeline.")

        if (isTRUE(all.equal(select(Q_women_low_grip_strength, low_grip_strength),
                             select(incorrect_less_than_equal_to, low_grip_strength))) |
            isTRUE(all.equal(select(Q_women_low_grip_strength, low_grip_strength),
                             select(incorrect_greater_than, low_grip_strength))) |
            isTRUE(all.equal(select(Q_women_low_grip_strength, low_grip_strength),
                             select(incorrect_greater_than_equal_to, low_grip_strength))))
          .fail("Wrong answer. It seems you are using the wrong sign comparison operator. You should have `grip_strength_kg < 20`")

        if (isTRUE(all.equal(select(Q_women_low_grip_strength, low_grip_strength) ,
                             select(correct_answer, low_grip_strength))))
          .pass()

        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_women_low_grip_strength <- function(){
  '
HINT
Your answer should have the form:

Q_women_low_grip_strength <-
  sarcopenia %>%
  filter(sex_male_1_female_0 == 0) %>%
  mutate(low_grip_strength = COMPARISON_CODE)
  
where COMPARISON_CODE should check whether `grip_strength_kg` is less than 20
' -> out
  cat(out)
}

.SOLUTION_Q_women_low_grip_strength <- function(){
  '


Q_women_low_grip_strength <-
  sarcopenia %>%
  filter(sex_male_1_female_0 == 0) %>%
  mutate(low_grip_strength = grip_strength_kg < 20)
  ' -> out
  cat(out)
}

# # Tests
# rm(Q_women_low_grip_strength)
# .CHECK_Q_women_low_grip_strength()
# 
# Q_women_low_grip_strength <- 
# "sarcopenia"
# .CHECK_Q_women_low_grip_strength()
# 
# Q_women_low_grip_strength <- 
#   sarcopenia %>% 
#   filter(sex_male_1_female_0 == 0)
# .CHECK_Q_women_low_grip_strength()
# 
# Q_women_low_grip_strength <- 
#   sarcopenia %>% 
#   filter(sex_male_1_female_0 == 0) %>% 
#   mutate(low_grip_strength = grip_strength_kg > 20)
# .CHECK_Q_women_low_grip_strength()
# 
# Q_women_low_grip_strength <- 
#   sarcopenia %>% 
#   filter(sex_male_1_female_0 == 0) %>% 
#   mutate(low_grip_strength = grip_strength_kg < 20)
# .CHECK_Q_women_low_grip_strength()
# 
# .HINT_Q_women_low_grip_strength()
# .SOLUTION_Q_women_low_grip_strength()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_prop_women_low_grip_strength ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.CHECK_Q_prop_women_low_grip_strength <- 
  function(){
    .problem_number <<- .NUM_Q_prop_women_low_grip_strength
    
    .autograder <<- function(){
      
      if(!exists("Q_prop_women_low_grip_strength"))
        .na("You have not yet defined the answer object, `Q_prop_women_low_grip_strength`.")
      
      if(! (is.numeric(Q_prop_women_low_grip_strength) & 
           (length(Q_prop_women_low_grip_strength) == 1)))
        .na("Invalid answer. Your answer should be a single number without quotes")
      
      if(between(Q_prop_women_low_grip_strength, 0, 1))
        .fail("You have provided a decimal answer. Your answer should be a percentage between 1 and 100.")
      
      if(between(Q_prop_women_low_grip_strength, 17, 18))
        .fail("Wrong answer. It seems you are counting the percentage that DON'T have low grip strength. You actually want the opposite of that." )
      
      if(between(Q_prop_women_low_grip_strength, 82, 83))
        .pass()
      
      else
        .fail()
      
    }
    .run_autograder()
    
  }

.HINT_Q_prop_women_low_grip_strength <- function(){
  '
HINT
Pass the output of the previous question into the `janitor::tabyl()` function, with `low_grip_strength` as an argument to the function.
' -> out
  cat(out)
}

.SOLUTION_Q_prop_women_low_grip_strength <- function(){
  '


Q_prop_women_low_grip_strength <- 
  sarcopenia %>% 
  filter(sex_male_1_female_0 == 0) %>% 
  mutate(low_grip_strength = grip_strength_kg < 20) %>% 
  tabyl(low_grip_strength) %>% 
  .[2,3] * 100
  ' -> out
  cat(out)
}

# Tests
# rm(Q_prop_women_low_grip_strength)
# .CHECK_Q_prop_women_low_grip_strength()
# 
# Q_prop_women_low_grip_strength <- "YOUR ANSWER HERE"
# .CHECK_Q_prop_women_low_grip_strength()
# 
# Q_prop_women_low_grip_strength <- 0.4
# .CHECK_Q_prop_women_low_grip_strength()
# 
# Q_prop_women_low_grip_strength <- 17.5
# .CHECK_Q_prop_women_low_grip_strength()
# 
# Q_prop_women_low_grip_strength <- 82.6
# .CHECK_Q_prop_women_low_grip_strength()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_asm_calculation ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.CHECK_Q_asm_calculation <-
  function() {
    
    .problem_number <<- .NUM_Q_asm_calculation
    
    correct_answer <- 
      .sarcopenia %>%
      mutate(asm = 0.244 * weight_kg + 7.8 * height_meters + 6.6 * sex_male_1_female_0 - 0.098 * age - 4.5)
    
    .autograder <<-
      function(){
        if(!exists("Q_asm_calculation"))
          .na("You have not yet defined the answer object, `Q_asm_calculation`.")
        
        if (!is.data.frame(Q_asm_calculation))
          .na("Invalid answer. Your answer should be a data frame.")
        
        if (!"asm" %in% names(Q_asm_calculation))
          .fail("Your answer should have a column called 'asm'.")
        
        if (isTRUE(all.equal(select(Q_asm_calculation, asm) ,
                             select(correct_answer, asm))))
          .pass()
        
        else
          .fail(glue("Wrong answer. The first five values of the `asm` column should be: \n{paste0(head(correct_answer$asm, 5), collapse = '\n') }"))
      }
    .run_autograder()
  }

.HINT_Q_asm_calculation <- function(){
  "HINT
The last three elements of the formula are: 6.6 * sex_male_1_female_0 - 0.098 * age - 4.5
Now all you're missing are the `weight_kg` and `height_meters` terms." -> out
  cat(out)
}

.SOLUTION_Q_asm_calculation <- function(){
  '


Q_asm_calculation <-
   sarcopenia %>%
   mutate(asm = 0.244 * weight_kg + 7.8 * height_meters + 6.6 * sex_male_1_female_0 - 0.098 * age - 4.5)
  ' -> out
  cat(out)
}

# # Tests
# rm(Q_asm_calculation)
# .CHECK_Q_asm_calculation()
# 
# Q_asm_calculation <- 
#   .sarcopenia
# .CHECK_Q_asm_calculation()
# 
# Q_asm_calculation <- 
#   .sarcopenia %>%
#   mutate(asm = 0.244 * weight_kg + 7.8 * height_meters + 6.6 * sex_male_1_female_0 - 0.098)
# .CHECK_Q_asm_calculation()
# 
# Q_asm_calculation <- 
#   .sarcopenia %>%
#   mutate(asm = 0.244 * weight_kg + 7.8 * height_meters + 6.6 * sex_male_1_female_0 - 0.098  * age - 4.5)
# .CHECK_Q_asm_calculation()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_age_integer ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_age_integer <-
  function() {
    
    .problem_number <<- .NUM_Q_age_integer
    
    correct_answer <- 
      .sarcopenia %>%
      mutate(age_integer = as.integer(age))
    
    .autograder <<-
      function(){
        if(!exists("Q_age_integer"))
          .na("You have not yet defined the answer object, `Q_age_integer`.")
        
        if (!is.data.frame(Q_age_integer))
          .na("Invalid answer. Your answer should be a data frame.")
        
        if (!"age_integer" %in% names(Q_age_integer))
          .fail("Your answer should have a column called 'age_integer'.")
        
        if (isTRUE(all.equal(select(Q_age_integer, age_integer) ,
                             select(correct_answer, age_integer))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_age_integer <- function(){
  "HINT
You should have `as.integer(age)` somewhere in your code" -> out
  cat(out)
}

.SOLUTION_Q_age_integer <- function(){
  '


Q_age_integer <-
   sarcopenia %>%
   mutate(age_integer = as.integer(age))
  ' -> out
  cat(out)
}


# # # Tests
# rm(Q_age_integer)
# .CHECK_Q_age_integer()
# 
# Q_age_integer <-
#   .sarcopenia
# .CHECK_Q_age_integer()
# 
# Q_age_integer <-
#   .sarcopenia %>%
#   mutate(age_integer = age)
# .CHECK_Q_age_integer()
# 
# Q_age_integer <-
#   .sarcopenia %>%
#   mutate(age_integer = as.integer(age))
# .CHECK_Q_age_integer()

