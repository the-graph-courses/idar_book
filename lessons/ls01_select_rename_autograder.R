if(!require('pacman')) install.packages('pacman')

pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(praise)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  DATA ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.yaounde <- read_csv(here::here('data/yaounde_data.csv'))

.yao <-
  .yaounde %>% select(age,
                      sex,
                      highest_education,
                      occupation,
                      is_smoker,
                      is_pregnant,
                      igg_result,
                      igm_result)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 6)   # Put total number of questions as `times` argument


.NUM_Q_weight_height <- 1
.NUM_Q_cols_16_22 <- 2
.NUM_Q_symp_to_sequel <- 3
.NUM_Q_educ_consult <- 4
.NUM_Q_starts_with_is <- 5
.NUM_Q_rearrange <- 6

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_weight_height ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_weight_height <-
  function() {
    .problem_number <<- .NUM_Q_weight_height
    
    .Q_weight_height <- .yaounde %>% select(weight_kg, height_cm)
    
    .autograder <<- 
      function(){
        if (!is.data.frame(Q_weight_height)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all.equal(Q_weight_height, .Q_weight_height))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.HINT_Q_weight_height <- function(){ 
  "HINT: 
  YOURDATA %>% select(COL1, COL2)" -> out
  cat(out)
}

.SOLUTION_Q_weight_height <- function(){ 
  " 
  yaounde %>% select(weight_kg, height_cm)" -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_cols_16_22 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_cols_16_22 <-
  function() {
    .problem_number <<- .NUM_Q_cols_16_22
    
    .Q_cols_16_22 <- .yaounde %>% select(16, 22)

    .autograder <<- 
      function(){
        if (!is.data.frame(Q_cols_16_22)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all.equal(Q_cols_16_22, .Q_cols_16_22))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.HINT_Q_cols_16_22 <- function(){ 
  "HINT: 
  YOURDATA %>% select(NUM1, NUM2)" -> out
  cat(out)
}

.SOLUTION_Q_cols_16_22 <- function(){ 
  " 
  yaounde %>% select(16, 22)" -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_symp_to_sequel ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_symp_to_sequel <-
  function() {
    .problem_number <<- .NUM_Q_symp_to_sequel
    
    .Q_symp_to_sequel <- .yaounde %>% select(symptoms:sequelae)
    
    .autograder <<- 
      function(){
        if (!is.data.frame(Q_symp_to_sequel)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all.equal(Q_symp_to_sequel, .Q_symp_to_sequel))) return(c(value = 1, message = paste("Correct!", praise()) ))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    
    .apply_autograder()
  }

.HINT_Q_symp_to_sequel <- function(){ 
  "HINT: 
  YOURDATA %>% select(STARTCOL : ENDCOL)" -> out
  cat(out)
}

.SOLUTION_Q_symp_to_sequel <- function(){ 
  " 
  yaounde %>% select(symptoms:sequelae)" -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_educ_consult ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_educ_consult <-
  function() {
    .problem_number <<- .NUM_Q_educ_consult
    
    .Q_educ_consult <- .yaounde %>% select(!c(highest_education:consultation))
    
    .autograder <<- 
      function(){
        if (!is.data.frame(Q_educ_consult)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all.equal(Q_educ_consult, .Q_educ_consult))) return(c(value = 1, message = paste("Correct!", praise()) ))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    
    .apply_autograder()
  }

.HINT_Q_educ_consult <- function(){ 
  "HINT: 
  You can use `:` to select a range, and `!` to negate that range." -> out
  cat(out)
}

.SOLUTION_Q_educ_consult <- function(){ 
  " 
  yaounde %>% select(!c(highest_education:consultation))" -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_starts_with_is  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.CHECK_Q_starts_with_is <-
  function() {
    .problem_number <<- .NUM_Q_starts_with_is
    
    .Q_starts_with_is  <- .yaounde %>% select(starts_with("is"))
    
    .autograder <<- 
      function(){
        if (!is.data.frame(Q_starts_with_is )) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all.equal(Q_starts_with_is , .Q_starts_with_is ))) return(c(value = 1, message = paste("Correct!", praise()) ))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    
    .apply_autograder()
  }

.HINT_Q_starts_with_is <- function(){ 
  "HINT: YOURDATA %>% select(starts_with('YOURSTRING'))" -> out
  cat(out)
}

.SOLUTION_Q_starts_with_is <- function(){ 
  ' 
  yaounde %>% select(starts_with("is"))' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_rearrange ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_rearrange <-
  function() {
    .problem_number <<- .NUM_Q_rearrange
    
    .Q_rearrange <- .yaounde %>% select(starts_with("is"), everything())
    
    .autograder <<- 
      function(){
        if (!is.data.frame(Q_rearrange)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all.equal(Q_rearrange, .Q_rearrange))) return(c(value = 1, message = paste("Correct!", praise()) ))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    
    .apply_autograder()
  }

.HINT_Q_rearrange <- function(){ 
  "HINT: 
  YOURDATA %>% select(starts_with('is_'), REMAINING_COLS)" -> out
  cat(out)
}

.SOLUTION_Q_rearrange <- function(){ 
  ' 
  yaounde %>% select(starts_with("is_"), everything())' -> out
  cat(out)
}


