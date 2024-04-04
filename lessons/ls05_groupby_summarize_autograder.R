if(!require('pacman')) install.packages('pacman')
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(praise,
               tidyverse,
               dplyr)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  DATA ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.yaounde <- read_csv(here::here('data/yaounde_data.csv'))
.yao <- .yaounde %>% 
  select(
    age, age_category_3, sex, weight_kg, height_cm,
    neighborhood, is_smoker, is_pregnant, occupation,
    treatment_combinations, symptoms, n_days_miss_work, n_bedridden_days,
    highest_education, igg_result)
# 
# .CHECK_Q_weight_summary <- function() invisible(NULL)
# .HINT_Q_weight_summary<- function() invisible(NULL)
# 
# .CHECK_Q_height_summary <- function() invisible(NULL)
# .HINT_Q_height_summary<- function() invisible(NULL)
# 
# .CHECK_Q_weight_by_smoking_status<- function() invisible(NULL)
# .HINT_Q_weight_by_smoking_status<- function() invisible(NULL)
# 
# .CHECK_Q_min_max_height_by_sex<- function() invisible(NULL)
# .HINT_Q_min_max_height_by_sex<- function() invisible(NULL)
# 
# .CHECK_Q_sum_bedridden_days<- function() invisible(NULL)
# .HINT_Q_sum_bedridden_days<- function() invisible(NULL)
# 
# .CHECK_Q_weight_by_sex_treatments <- function() invisible(NULL)
# .HINT_Q_weight_by_sex_treatments <- function() invisible(NULL)
# 
# .CHECK_Q_bedridden_by_age_sex_iggresult<- function() invisible(NULL)
# .HINT_Q_bedridden_by_age_sex_iggresult<- function() invisible(NULL)
# 
# .CHECK_Q_occupation_summary <- function() invisible(NULL)
# .HINT_Q_occupation_summary <- function() invisible(NULL)
# 
# .CHECK_Q_symptoms_adults <- function() invisible(NULL)
# .HINT_Q_symptoms_adults <- function() invisible(NULL)
# 
# .CHECK_Q_count_iggresults_stratified_by_sex_agecategories <- function() invisible(NULL)
# .HINT_Q_count_iggresults_stratified_by_sex_agecategories <- function() invisible(NULL)
# 
# .CHECK_Q_count_bedridden_age_categories <- function() invisible(NULL)
# .HINT_Q_count_bedridden_age_categories <- function() invisible(NULL)
# 
# .CHECK_Q_median_age_by_neighborhood_agecategory_sex <- function() invisible(NULL)
# .HINT_Q_median_age_by_neighborhood_agecategory_sex <- function() invisible(NULL)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 11)   # Put total number of questions as `times` argument

.NUM_Q_weight_summary <- 1
.NUM_Q_height_summary <- 2
.NUM_Q_weight_by_smoking_status <- 3
.NUM_Q_min_max_height_by_sex <- 4
.NUM_Q_sum_bedridden_days <- 5
.NUM_Q_weight_by_sex_treatments <- 6
.NUM_Q_bedridden_by_age_sex_iggresult <- 7
.NUM_Q_occupation_summary <- 8
.NUM_Q_count_iggresults_stratified_by_sex_agecategories <- 9
.NUM_Q_count_bedridden_age_categories <- 10
.NUM_Q_median_age_by_neighborhood_agecategory_sex <- 11


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  ANSWERS ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_weight_summary ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_weight_summary <-
  function() {
    
    .problem_number <<- .NUM_Q_weight_summary
    correct_answer <- .yao %>%
      summarize(mean_weight_kg = mean(weight_kg),
                median_weight_kg = median(weight_kg),
                sd_weight_kg = sd(weight_kg))
    
    .autograder <<-
      function(){
        if(!exists("Q_weight_summary"))
          .na("You have not yet defined the answer object, `Q_weight_summary`.")
        if (!is.data.frame(Q_weight_summary))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"mean_weight_kg" %in% names(Q_weight_summary))
          .fail("Your answer should have a column called 'mean_weight_kg'.")
        if (!"median_weight_kg" %in% names(Q_weight_summary))
          .fail("Your answer should have a column called 'median_weight_kg'.")
        if (!"sd_weight_kg" %in% names(Q_weight_summary))
          .fail("Your answer should have a column called 'sd_weight_kg'.")
        
        
        if (isTRUE(all.equal(Q_weight_summary, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_weight_summary <- function(){
  '
HINT.
Your answer should look this: 

Q_weight_summary <- 
  yao %>% 
  summarize(mean_weight_kg = FORMULA_HERE,
            median_weight_kg = FORMULA_HERE,
            sd_weight_kg = FORMULA_HERE)' -> out
  cat(out)
}

.SOLUTION_Q_weight_summary <- function(){
  '

Q_weight_summary <- 
  yao %>%
      summarize(mean_weight_kg = mean(weight_kg),
                median_weight_kg = median(weight_kg),
                sd_weight_kg = sd(weight_kg))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_height_summary ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_height_summary <-
  function() {
    
    .problem_number <<- .NUM_Q_height_summary
    correct_answer <- .yao %>%
      summarize(min_height_cm = min(height_cm),
                max_height_cm = max(height_cm))
    
    .autograder <<-
      function(){
        if(!exists("Q_height_summary"))
          .na("You have not yet defined the answer object, `Q_height_summary`.")
        if (!is.data.frame(Q_height_summary))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"min_height_cm" %in% names(Q_height_summary))
          .fail("Your answer should have a column called 'min_height_cm'.")
        if (!"max_height_cm" %in% names(Q_height_summary))
          .fail("Your answer should have a column called 'max_height_cm'.")
        
        if (isTRUE(all.equal(Q_height_summary, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_height_summary <- function(){
  '
HINT.
Your answer should look this: 

Q_height_summary <- 
  yao %>% 
  summarize(min_height_cm = FORMULA_HERE,
            max_height_cm = FORMULA_HERE)' -> out
  cat(out)
}

.SOLUTION_Q_height_summary <- function(){
  '

Q_height_summary <- 
  yao %>%
      summarize(min_height_cm = min(height_cm),
                max_height_cm = max(height_cm))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_weight_by_smoking_status ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_weight_by_smoking_status <-
  function() {
    
    .problem_number <<- .NUM_Q_weight_by_smoking_status
    correct_answer <- .yao %>% 
      group_by(is_smoker) %>% 
      summarise(weight_mean = mean(weight_kg))
    
    .autograder <<-
      function(){
        if(!exists("Q_weight_by_smoking_status"))
          .na("You have not yet defined the answer object, `Q_weight_by_smoking_status`.")
        if (!is.data.frame(Q_weight_by_smoking_status))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"weight_mean" %in% names(Q_weight_by_smoking_status))
          .fail("Your answer should have a column called 'weight_mean'.")
        
        if (isTRUE(all.equal(Q_weight_by_smoking_status, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_weight_by_smoking_status <- function(){
  '
HINT.
Your answer should look this: 

Q_weight_by_smoking_status <- 
  yao %>% 
  group_by(VARIABLE) %>%
  summarize(weight_mean = FORMULA_HERE)' -> out
  cat(out)
}

.SOLUTION_Q_weight_by_smoking_status <- function(){
  '

Q_weight_by_smoking_status <- 
  yao %>% 
  group_by(is_smoker) %>% 
  summarise(weight_mean = mean(weight_kg))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_min_max_height_by_sex ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_min_max_height_by_sex <-
  function() {
    
    .problem_number <<- .NUM_Q_min_max_height_by_sex
    correct_answer <- .yao %>% 
      group_by(sex) %>% 
      summarise(min_height_cm = min(height_cm), 
                max_height_cm = max(height_cm))
    
    .autograder <<-
      function(){
        if(!exists("Q_min_max_height_by_sex"))
          .na("You have not yet defined the answer object, `Q_min_max_height_by_sex`.")
        if (!is.data.frame(Q_min_max_height_by_sex))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"max_height_cm" %in% names(Q_min_max_height_by_sex))
          .fail("Your answer should have a column called 'max_height_cm'.")
        if (!"min_height_cm" %in% names(Q_min_max_height_by_sex))
          .fail("Your answer should have a column called 'min_height_cm'.")
        
        if (isTRUE(all.equal(Q_min_max_height_by_sex, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_min_max_height_by_sex <- function(){
  '
HINT.
Your answer should look this: 

Q_min_max_height_by_sex <- 
  yao %>% 
  group_by(VARIABLE) %>%
  summarize(min_height_cm = FORMULA_HERE,
            max_height_cm = FORMULA_HERE)' -> out
  cat(out)
}

.SOLUTION_Q_min_max_height_by_sex <- function(){
  '

Q_min_max_height_by_sex <- 
  yao %>% 
  group_by(sex) %>%
  summarise(min_height_cm = min(height_cm),
            max_height_cm = max(height_cm))' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_sum_bedridden_days ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
.CHECK_Q_sum_bedridden_days <-
  function() {
    
    .problem_number <<- .NUM_Q_sum_bedridden_days
    correct_answer <- .yao %>% 
      group_by(sex) %>% 
      summarise(total_bedridden_days = sum(n_bedridden_days, na.rm = T))
    
    forgot_na <-   .yao %>% 
      group_by(sex) %>% 
      summarise(total_bedridden_days = sum(n_bedridden_days))
    
    
    .autograder <<-
      function(){
        if(!exists("Q_sum_bedridden_days"))
          .na("You have not yet defined the answer object, `Q_sum_bedridden_days`.")
        if (!is.data.frame(Q_sum_bedridden_days))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"total_bedridden_days" %in% names(Q_sum_bedridden_days))
          .fail("Your answer should have a column called 'total_bedridden_days'.")
        
        if (isTRUE(all.equal(Q_sum_bedridden_days, forgot_na)))
          .fail("Wrong. You may have forgotten to exclude NAs, with `na.rm = T`")
        
        
        if (isTRUE(all.equal(Q_sum_bedridden_days, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_sum_bedridden_days <- function(){
  '
HINT.
Your answer should look this: 

Q_sum_bedridden_days <- 
  yao %>% 
  group_by(VARIABLE) %>%
  summarize(total_bedridden_days = FORMULA_HERE)' -> out
  cat(out)
}

.SOLUTION_Q_sum_bedridden_days <- function(){
  '

Q_sum_bedridden_days <- 
  yao %>% 
  group_by(sex) %>% 
  summarise(total_bedridden_days = sum(n_bedridden_days, na.rm = T))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_weight_by_sex_treatments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_weight_by_sex_treatments <-
  function() {
    
    .problem_number <<- .NUM_Q_weight_by_sex_treatments
    correct_answer <- .yao %>% 
      group_by(sex, treatment_combinations) %>% 
      summarise(mean_weight_kg = mean(weight_kg, na.rm = T))
    
    .autograder <<-
      function(){
        if(!exists("Q_weight_by_sex_treatments"))
          .na("You have not yet defined the answer object, `Q_weight_by_sex_treatments`.")
        if (!is.data.frame(Q_weight_by_sex_treatments))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"mean_weight_kg" %in% names(Q_weight_by_sex_treatments))
          .fail("Your answer should have a column called 'mean_weight_kg'.")
        
        if (isTRUE(all.equal(Q_weight_by_sex_treatments, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_weight_by_sex_treatments <- function(){
  '
HINT.
Your answer should look this: 

Q_weight_by_sex_treatments <- 
  yao %>% 
  group_by(VARIABLE1, VARIABLE2) %>%
  summarize(mean_weight_kg = FORMULA_HERE)' -> out
  cat(out)
}

.SOLUTION_Q_weight_by_sex_treatments <- function(){
  '

  
Q_weight_by_sex_treatments <- 
  yao %>% 
  group_by(sex, treatment_combinations) %>%
  summarise(mean_weight_kg = mean(weight_kg, na.rm = T))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_bedridden_by_age_sex_iggresult  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_bedridden_by_age_sex_iggresult <-
  function() {
    
    .problem_number <<- .NUM_Q_bedridden_by_age_sex_iggresult
    correct_answer <- .yao %>% 
      group_by(age_category_3, sex, igg_result) %>% 
      summarise(mean_n_bedridden_days = mean(n_bedridden_days, na.rm = T))
    
    
    forgot_nas <- 
      .yao %>%
      group_by(age_category_3, sex, igg_result) %>% 
      summarise(mean_n_bedridden_days = mean(n_bedridden_days))
    
    .autograder <<-
      function(){
        if(!exists("Q_bedridden_by_age_sex_iggresult"))
          .na("You have not yet defined the answer object, `Q_bedridden_by_age_sex_iggresult`.")
        if (!is.data.frame(Q_bedridden_by_age_sex_iggresult))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"mean_n_bedridden_days" %in% names(Q_bedridden_by_age_sex_iggresult))
          .fail("Your answer should have a column called 'mean_n_bedridden_days'.")
        
        if (isTRUE(all.equal(Q_bedridden_by_age_sex_iggresult, forgot_nas)))
          .fail("It seems you forgot to remove the NAs!")
        
        
        if (isTRUE(all.equal(Q_bedridden_by_age_sex_iggresult, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_bedridden_by_age_sex_iggresult <- function(){
  '
HINT.
Your answer should look this: 

Q_bedridden_by_age_sex_iggresult <- 
  yao %>% 
  group_by(VARIABLES) %>%
  summarize(mean_n_bedridden_days = FORMULA_HERE)' -> out
  cat(out)
}

.SOLUTION_Q_bedridden_by_age_sex_iggresult <- function(){
  '

Q_bedridden_by_age_sex_iggresult <- 
  yao %>% 
  group_by(age_category_3, sex, igg_result) %>% 
  summarise(mean_n_bedridden_days = mean(n_bedridden_days, na.rm = T))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_occupation_summary  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_occupation_summary <-
  function() {
    
    .problem_number <<- .NUM_Q_occupation_summary
    correct_answer <- .yao %>% 
      group_by(occupation) %>% 
      summarise(count = n(),
                mean_n_days_miss_work = mean(n_days_miss_work, na.rm=TRUE))
    
    .autograder <<-
      function(){
        if(!exists("Q_occupation_summary"))
          .na("You have not yet defined the answer object, `Q_occupation_summary`.")
        if (!is.data.frame(Q_occupation_summary))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"mean_n_days_miss_work" %in% names(Q_occupation_summary))
          .fail("Your answer should have a column called 'mean_n_days_miss_work'.")
        
        if (isTRUE(all.equal(Q_occupation_summary, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_occupation_summary <- function(){
  '
HINT.
Your answer should look this: 

Q_occupation_summary <- 
  yao %>% 
  group_by(VARIABLE) %>%
  summarize(count = FORMULA_HERE,
            mean_n_days_miss_work = FORMULA_HERE)' -> out
  cat(out)
}

.SOLUTION_Q_occupation_summary <- function(){
  '

Q_occupation_summary <- 
  yao %>% 
  group_by(occupation) %>%
  summarise(count = n(),
            mean_n_days_miss_work = mean(n_days_miss_work, na.rm=TRUE))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_symptoms_adults  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_symptoms_adults <-
  function() {

    .problem_number <<- .NUM_Q_symptoms_adults
    correct_answer <- .yao %>%
      group_by(symptoms) %>%
      summarise(sum_adults = sum(age_category_3 == "Adult"))

    .autograder <<-
      function(){
        if(!exists("Q_symptoms_adults"))
          .na("You have not yet defined the answer object, `Q_symptoms_adults`.")
        if (!is.data.frame(Q_symptoms_adults))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"sum_adults" %in% names(Q_symptoms_adults))
          .fail("Your answer should have a column called 'sum_adults'.")

        if (isTRUE(all.equal(Q_symptoms_adults, correct_answer)))
          .pass()

        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_symptoms_adults <- function(){
  '
HINT.

  The condition you need is `age_category_3 == "Adult"`' -> out
  cat(out)
}

.SOLUTION_Q_symptoms_adults <- function(){
  '

Q_symptoms_adults <-
  yao %>%
  group_by(symptoms) %>%
      summarise(sum_adults = sum(age_category_3 == "Adult"))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_count_iggresults_stratified_by_sex_agecategories  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_count_iggresults_stratified_by_sex_agecategories <-
  function() {
    
    .problem_number <<- .NUM_Q_count_iggresults_stratified_by_sex_agecategories
    correct_answer <- .yao %>% 
      count(sex, age_category_3, igg_result)
    
    .autograder <<-
      function(){
        if(!exists("Q_count_iggresults_stratified_by_sex_agecategories"))
          .na("You have not yet defined the answer object, `Q_count_iggresults_stratified_by_sex_agecategories`.")
        if (!is.data.frame(Q_count_iggresults_stratified_by_sex_agecategories))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"n" %in% names(Q_count_iggresults_stratified_by_sex_agecategories))
          .fail("Your answer should have a column called 'n'.")
        
        if (isTRUE(all.equal(Q_count_iggresults_stratified_by_sex_agecategories, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_count_iggresults_stratified_by_sex_agecategories <- function(){
  '
HINT.
Your answer should look this: 

Q_count_iggresults_stratified_by_sex_agecategories <- 
  yao %>% 
  count(VARIABLE)' -> out
  cat(out)
}

.SOLUTION_Q_count_iggresults_stratified_by_sex_agecategories <- function(){
  '

Q_count_iggresults_stratified_by_sex_agecategories <- 
  yao %>% 
  count(sex, age_category_3, igg_result)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_count_bedridden_age_categories  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_count_bedridden_age_categories <-
  function() {
    
    .problem_number <<- .NUM_Q_count_bedridden_age_categories
    correct_answer <- .yao %>% 
      count(age_category_3, n_bedridden_days)
    
    .autograder <<-
      function(){
        if(!exists("Q_count_bedridden_age_categories"))
          .na("You have not yet defined the answer object, `Q_count_bedridden_age_categories`.")
        if (!is.data.frame(Q_count_bedridden_age_categories))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"n" %in% names(Q_count_bedridden_age_categories))
          .fail("Your answer should have a column called 'n'.")
        
        if (isTRUE(all.equal(Q_count_bedridden_age_categories, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_count_bedridden_age_categories <- function(){
  '
HINT.
Your answer should look this: 

Q_count_bedridden_age_categories <- 
  yao %>% 
  count(VARIABLE)' -> out
  cat(out)
}

.SOLUTION_Q_count_bedridden_age_categories <- function(){
  '

Q_count_bedridden_age_categories <- 
  yao %>% 
  count(age_category_3, n_bedridden_days)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_median_age_by_neighborhood_agecategory_sex  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_median_age_by_neighborhood_agecategory_sex <-
  function() {
    
    .problem_number <<- .NUM_Q_median_age_by_neighborhood_agecategory_sex
    correct_answer <- .yao %>% 
      mutate(neighborhood = as.factor(neighborhood),
             age_category_3 = as.factor(age_category_3),
             sex = as.factor(sex)) %>%
      group_by(neighborhood, age_category_3, sex, .drop=FALSE) %>%
      summarize(median_age = median(age, na.rm=TRUE))
    
    .autograder <<-
      function(){
        if(!exists("Q_median_age_by_neighborhood_agecategory_sex"))
          .na("You have not yet defined the answer object, `Q_median_age_by_neighborhood_agecategory_sex`.")
        if (!is.data.frame(Q_median_age_by_neighborhood_agecategory_sex))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"median_age" %in% names(Q_median_age_by_neighborhood_agecategory_sex))
          .fail("Your answer should have a column called 'median_age'.")
        
        if (isTRUE(all.equal(Q_median_age_by_neighborhood_agecategory_sex, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_median_age_by_neighborhood_agecategory_sex <- function(){
  '
HINT.
Your answer should look this: 

Q_median_age_by_neighborhood_agecategory_sex <- 
  yao %>% 
  mutate(VARIABLES) %>%
  group_by(VARIABLES) %>%
  summarize(median_age = FORMULA)' -> out
  cat(out)
}

.SOLUTION_Q_median_age_by_neighborhood_agecategory_sex <- function(){
  '

Q_median_age_by_neighborhood_agecategory_sex <- 
  yao %>% 
  mutate(neighborhood = as.factor(neighborhood),
             age_category_3 = as.factor(age_category_3),
             sex = as.factor(sex)) %>%
  group_by(neighborhood, age_category_3, sex, .drop=FALSE) %>%
  summarize(median_age = median(age, na.rm=TRUE))' -> out
  cat(out)
}

