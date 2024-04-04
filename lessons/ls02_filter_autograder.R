if(!require('pacman')) install.packages('pacman')

pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(praise)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  DATA ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.yaounde <- read_csv(here::here('data/yaounde_data.csv'))

.yao <-
  .yaounde %>% 
  select(age, sex, weight_kg, highest_education, neighborhood, 
         occupation, is_smoker, is_pregnant, 
         igg_result, igm_result)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 8)   # Put total number of questions as `times` argument

.NUM_Q_is_pregnant <- 1
.NUM_Q_female_nrow <- 2
.NUM_Q_under_18 <- 3
.NUM_Q_tsinga_messa<- 4
.NUM_Q_male_positive <- 5
.NUM_Q_child_primary <- 6
.NUM_Q_not_tsinga_messa <- 7
.NUM_Q_na_smoker <- 8

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_is_pregnant ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_is_pregnant <-
  function() {
    .problem_number <<- .NUM_Q_is_pregnant
    
    .Q_is_pregnant <- .yao %>% filter(is_pregnant == "Yes")
    
    .autograder <<- 
      function(){
        if (!is.data.frame(Q_is_pregnant)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all.equal(Q_is_pregnant, .Q_is_pregnant))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.HINT_Q_is_pregnant <- function(){ 
  'HINT: 
  YOURDATA %>% filter(CONDITION)' -> out
  cat(out)
}

.SOLUTION_Q_is_pregnant <- function(){ 
  " 
  yao %>% filter(is_pregnant == 'Yes')" -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_female_nrow ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_female_nrow <-
  function() {
    .problem_number <<- .NUM_Q_female_nrow
    
    .Q_female_nrow <- .yao %>%
      filter(sex == "Female") %>%
      nrow()
    
    .autograder <<- 
      function(){
        if (!is.numeric(Q_female_nrow)) return(c(value = -1, message = "Your result should be numeric."))
        
        if (identical(Q_female_nrow, .Q_female_nrow)) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.HINT_Q_female_nrow <- function(){ 
'HINT: 
  YOURDATA %>% 
  filter(sex == "YOURSTRING") %>%
  nrow()' -> out
  cat(out)
}

.SOLUTION_Q_female_nrow <- function(){ 
  " 
      yao %>%
      filter(sex == 'Female') %>%
      nrow()" -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_under_18 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_under_18 <-
  function() {
    .problem_number <<- .NUM_Q_under_18
    
    .Q_under_18 <- .yao %>% filter(age < 18)
    
    .autograder <<- 
      function(){
        if (!is.data.frame(Q_under_18)) return(c(value = -1, message = "Your result should be a data frame."))
        
        if (isTRUE(all.equal(Q_under_18, .Q_under_18))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.HINT_Q_under_18 <- function(){ 
'YOURDATA %>% 
  filter(CONDITION)' -> out
  cat(out)
}

.SOLUTION_Q_under_18 <- function(){ 
  " 
  yao %>% filter(age < 18)" -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_tsinga_messa ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_tsinga_messa <-
  function() {
    .problem_number <<- .NUM_Q_tsinga_messa
    
    .Q_tsinga_messa <- .yao %>%
      filter(neighborhood %in% c("Tsinga", "Messa")) 
    
    .autograder <<- 
      function(){
        if (!is.data.frame(Q_tsinga_messa)) return(c(value = -1, message = "Your result should be a data frame."))
        
        if (isTRUE(all.equal(Q_tsinga_messa, .Q_tsinga_messa))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.HINT_Q_tsinga_messa <- function(){ 
'HINT: 
  YOURDATA %>% 
  filter(neighborhood %in% c("STRING1", "STRING2"))' -> out
  cat(out)
}

.SOLUTION_Q_tsinga_messa <- function(){ 
  ' 
  yao %>%
  filter(neighborhood %in% c("Tsinga", "Messa"))' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_male_positive ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_male_positive <-
  function() {
    .problem_number <<- .NUM_Q_male_positive
    
    .Q_male_positive <- .yao %>%
      filter(sex == "Male" & igg_result == "Positive") 
    
    .autograder <<- 
      function(){
        if (!is.data.frame(Q_male_positive)) return(c(value = -1, message = "Your result should be a data frame."))
        
        if (isTRUE(all.equal(Q_male_positive, .Q_male_positive))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.HINT_Q_male_positive <- function(){ 
'HINT: 
  YOURDATA %>% 
  filter(sex == "Male" & CONDITION2) %>%
  nrow()' -> out
  cat(out)
}

.SOLUTION_Q_male_positive <- function(){ 
  ' 
  yao %>%
  filter(sex == "Male" & igg_result == "Positive")' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_child_primary ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_child_primary <-
  function() {
    .problem_number <<- .NUM_Q_child_primary
    
    .Q_child_primary <- .yao %>% filter(age < 18 | highest_education == "Primary")
    
    .Q_child_primary_wrong_operator <- .yao %>% filter(age < 18 & highest_education == "Primary")
    
    .autograder <<- 
      function(){
        if (!is.data.frame(Q_child_primary)) return(c(value = -1, message = "Your result should be a data frame."))
        
        if (isTRUE(all.equal(Q_child_primary, .Q_child_primary_wrong_operator))) return(c(value = 0, message = paste("Wrong. You should use the `|` operator.",
                                                                                "We actually want children OR anyone with a 'Primary' highest_education",
                                                                                "(despite it saying 'and' in the question). Think for some time about this.", 
                                                                                "It is quite counterintuitive!")))
        
        if (isTRUE(all.equal(Q_child_primary, .Q_child_primary))) return(c(value = 1, message = paste("Correct!", 
                                                                                                      "We actually want children OR anyone with a 'Primary' highest_education",
                                                                                                      "(despite it saying 'and' in the question).", 
                                                                                                      "It is quite counterintuitive!")))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.HINT_Q_child_primary <- function(){ 
  'HINT: 
  YOURDATA %>% filter(CONDITION1 | CONDITION2)' -> out
  cat(out)
}

.SOLUTION_Q_child_primary <- function(){ 
  ' 
  yao %>% filter(age < 18 | highest_education == "Primary")' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_not_tsinga_messa ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_not_tsinga_messa <-
  function() {
    .problem_number <<- .NUM_Q_not_tsinga_messa
    
    .Q_not_tsinga_messa <- .yao %>%
      filter(!(neighborhood %in% c("Tsinga", "Messa"))) 
    
    .autograder <<- 
      function(){
        if (!is.data.frame(Q_not_tsinga_messa)) return(c(value = -1, message = "Your result should be dataframe."))
        
        if (isTRUE(all.equal(Q_not_tsinga_messa, .Q_not_tsinga_messa))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.HINT_Q_not_tsinga_messa <- function(){ 
'HINT: 
  YOURDATA %>% 
  filter(neighborhood %in% c("STRING1", "STRING2") )' -> out
  cat(out)
}

.SOLUTION_Q_not_tsinga_messa <- function(){ 
  ' 
  yao %>%
  filter(!(neighborhood %in% c("Tsinga", "Messa")))' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_na_smoker ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_na_smoker <-
  function() {
    .problem_number <<- .NUM_Q_na_smoker
    
    .Q_na_smoker <- .yao %>% filter(is.na(is_smoker)) 
    
    .autograder <<- 
      function(){
        if (!is.data.frame(Q_na_smoker)) return(c(value = -1, message = "Your result should be dataframe."))
        
        if (isTRUE(all.equal(Q_na_smoker, .Q_na_smoker))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.HINT_Q_na_smoker <- function(){ 
'HINT: 
  YOURDATA %>% 
  filter(is.na(VARIABLE))' -> out
  cat(out)
}

.SOLUTION_Q_na_smoker <- function(){ 
  " 
  yao %>% filter(is.na(is_smoker))" -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_drop_resp_under_20 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_drop_resp_under_20 <-
  function() {
    .problem_number <<- .NUM_Q_drop_resp_under_20
    
    .Q_drop_resp_under_20 <- .yaounde %>% filter(respiration_frequency >= 20 | is.na(respiration_frequency))
    
    .Q_drop_resp_under_20_wrong_forgot_na <- .yaounde %>% filter(respiration_frequency >= 20)
    
    .autograder <<- 
      function(){
        if (!is.data.frame(Q_drop_resp_under_20))
          return(c(value = -1, message = "Invalid. Your result should be dataframe."))
        if (isTRUE(all.equal(Q_drop_resp_under_20, .Q_drop_resp_under_20_wrong_forgot_na))) 
          return(c(value = 0, message = "Wrong. Remember to also keep those with NA values."))
        
        if (isTRUE(all.equal(Q_drop_resp_under_20, .Q_drop_resp_under_20))) 
          return(c(value = 1, message = paste("Correct!", praise())))
        
        else
          return(c(value = 0, message = "Wrong. Please try again."))
        
      }
    .apply_autograder()
  }

.HINT_Q_drop_resp_under_20 <- function(){ 
  'HINT: 
  YOURDATA %>% filter(CONDITION1 | NA_CONDITION )' -> out
  cat(out)
}

.SOLUTION_Q_drop_resp_under_20 <- function(){ 
  " 
  yaounde %>% filter(respiration_frequency >= 20 | is.na(respiration_frequency))" -> out
  cat(out)
}


# 
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## ~  Q_is_pregnant0 ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .CHECK_Q_10 <-
#   function() {
#     .problem_number <<- 10
#     
#     .Q_is_pregnant0 <- .yao %>% filter(row_number() %in% c(8:20, 80))
#     
#     .autograder <<- 
#       function(){
#         if (!is.data.frame(Q_is_pregnant0)) return(c(value = -1, message = "Your result should be a data frame."))
#         
#         if (isTRUE(all.equal(Q_is_pregnant0, .Q_is_pregnant0))) return(c(value = 1, message = paste("Correct!", praise())))
#         
#         return(c(value = 0, message = "Wrong. Please try again."))
#       }
#     .apply_autograder()
#   }
# 
# .HINT_Q_10 <- function(){ 
#   'YOURDATA %>% filter(row_number() %in% SELECTION)' -> out
#   cat(out)
# }
# 
# 
# 
