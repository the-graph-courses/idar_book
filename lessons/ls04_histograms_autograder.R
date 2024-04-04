# Ch03 Ls04 Histograms
## Joy Vaz
## 2023-02-11

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load_gh("KO112/KO")
pacman::p_load(tidyverse,
               praise,
               here)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Init ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 5)   # Put total number of questions as `times` argument


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.malidd <- read.csv(here::here("data/clean/malidd.csv"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Plot a histogram showing the distribution of age (`age_months`) in `malidd`. 
#' Make the borders and fill of the bars "seagreen", and reduce opacity to 40%.

# [backend]
.CHECK_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = age_months)) +
      geom_histogram(fill = "seagreen",
                     color = "seagreen",
                     alpha = 0.4) 
    
    gg_req <- .q1_correct
    gg_ans <- q1
    
    .autograder <<-
      function(){
        if(!is.ggplot(gg_ans)) return(c(value = -1, message = "Wrong. Your result should be a ggplot2 object. Please try again."))
        
        # Use compare_ggplots to check if answer is perfectly correct
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = "Wrong. Please check the hint and try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.HINT_q1 <- function(){
  'First, supply the correct data frame to the data layer of `ggplot()`.
  Then put the variable you want to map on your x axis inside `mapping = aes()`. 
  Then, add the correct geometry function for a histogram.
  You need to assign the correct aesthetic arguments for color, fill, and transparency (alpha). These are FIXED aesthetics, so be sure to put these inside the geom_*() function and NOT inside the aes() function.
  If all these are correct: look for typos, missing commas or plus signs, or unmatched brackets/parentheses.' -> out
  cat(out)
}
# solution of question
.SOLUTION_q1 <- function(){
  'ggplot(data = malidd, 
         mapping = aes(x = age_months)) +
    geom_histogram(fill = "seagreen",
                   color = "seagreen",
                   alpha = 0.4)`' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Building on the code above, visualize the relationship between time the GPD per capita `gap_US` data frame with both points and lines. Change the color of the points as desired.

# [backend]
.CHECK_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = age_months)) +
      geom_histogram(fill = "seagreen",
                     color = "seagreen",
                     alpha = 0.4) +
      labs(x = "Age (months)",
           y = "Number of children") 
    
    gg_req <- .q2_correct
    gg_ans <- q2
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Wrong. Your result should be a ggplot2 object. Please try again."))
        
        # Use compare_ggplots to check if answer is perfectly correct
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = "Wrong. Please check the hint and try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.HINT_q2 <- function(){
  'Axis labels in ggplot2 are modified using the labs() function, which was introduced in our lesson on line graphs.
  The text for each axis label should be within quotes. Check that it is identical to the text asked for in the question.
  If all these are correct: look for typos, missing commas or plus signs, or unmatched brackets/parentheses.' -> out
  cat(out)
}
# solution of question
.SOLUTION_q2 <- function(){
  'ggplot(data = .malidd, 
             mapping = aes(x = age_months)) +
      geom_histogram(fill = "seagreen",
                     color = "seagreen",
                     alpha = 0.4) +
      labs(x = "Age (months)",
           y = "Number of children")' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Make a histogram of frequency of respiration (`freqrespi`), 
#' which is measured in breaths per minute. 
#' Set the interior color to "indianred3", and border color to "lightgray".
#' Low the number of bins until there are no empty intervals. 
#' You should choose the highest value of bins for which there are no empty spaces.

# [backend]
.CHECK_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = freqrespi)) +
      geom_histogram(fill = "indianred3",
                     color = "lightgray",
                     bins = 20)
    
    gg_req <- .q3_correct
    gg_ans <- q3
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Wrong. Your result should be a ggplot2 object. Please try again."))
        
        # Use compare_ggplots to check if answer is perfectly correct
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        if(!isTRUE(perfect_match)) return(c(value = 0, 
                                            message = "Wrong. Please check the hint and try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.HINT_q3 <- function(){
  "First, supply the malidd data frame to the data layer of `ggplot()`.
  Then assign the correct variable to the x axis inside `aes()`. 
  Then, add the appropiety geometry function for a histogram.
  You need to assign the correct aesthetic arguments for color, fill, and number of bins. These are FIXED aesthetics, so be sure to put these inside the geom_*() function and NOT inside the aes() function.
  To get the number of bins that gives you no empty spaces, you will need to try different numbers. Try starting at a low number of bins like 15, and increase in small increments.
  If all these are correct: look for typos, missing commas or plus signs, or unmatched brackets/parentheses." -> out
  cat(out)
}
# solution of question
.SOLUTION_q3 <- function(){
  'ggplot(data = malidd, 
       mapping = aes(x = freqrespi)) +
    geom_histogram(fill = "indianred3",
                   color = "lightgray",
                   bins = 20)' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#'Create the same freqrespi histogram from the last practice question, 
#'but this time set the bin width to to a value that results in 18 bins. 
#'Then align the bars to the x axis breaks by adjusting the bin boundaries.

# [backend]
.CHECK_q4 <-
  function() {
    .problem_number <<- 4
    
    .q4_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = freqrespi)) +
      geom_histogram(binwidth = 2,
                     fill = "indianred3",
                     color = "lightgray",
                     boundary = 24)
    
    gg_req <- .q4_correct
    gg_ans <- q4
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Wrong. Your result should be a ggplot2 object. Please try again."))
        
        # Use compare_ggplots to check if answer is perfectly correct
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        if(!isTRUE(perfect_match)) return(c(value = 0, 
                                            message = "Wrong. Please check the hint and try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.HINT_q4 <- function(){
  "The freqrespi variable ranges from 24 to 60. To find the bin width needed to yield 18 bins, you can calculate (60-24)/18.
  To align the breaks to the x-axis marks, you can set the boundary argument to the lowest value." -> out
  cat(out)
}
# solution of question
.SOLUTION_q4 <- function(){
  'ggplot(data = malidd, 
       mapping = aes(x = freqrespi)) +
    geom_histogram(binwidth = 2,
                   fill = "indianred3",
                   color = "lightgray",
                   boundary = 24)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q5 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Plot the freqrespi histogram with bin breaks that range from the 
#' lowest value of freqrespi to the highest, with intervals of 4.
#' Next, adjust the x-axis scale breaks by adding a scale_*() function. 
#' Set the range to 24-60, with an intervals of 8.

# [backend]
.CHECK_q5 <-
  function() {
    .problem_number <<- 5
    
    .q5_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = freqrespi)) +
      geom_histogram(fill = "indianred3",
                     color = "lightgray", 
                     binwidth = 4) +
      scale_x_continuous(breaks = seq(24, 60, 8))
    
    gg_req <- .q5_correct
    gg_ans <- q5
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Wrong. Your result should be a ggplot2 object. Please try again."))
        
        # Use compare_ggplots to check if answer is perfectly correct
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        if(!isTRUE(perfect_match)) return(c(value = 0, 
                                            message = "Wrong. Please check the hint and try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.HINT_q5 <- function(){
  "Keep the same fill and color from the previous practice question.
  Use the binwidth argument to set the intervals. 
  Remember that these are FIXED aesthetics and should go in geom_histogram().
  Then use the seq() function to set breaks inside scale_x_continuous()." -> out
  cat(out)
}
# solution of question
.SOLUTION_q5 <- function(){
  'ggplot(data = malidd, 
       mapping = aes(x = freqrespi)) +
    geom_histogram(fill = "indianred3",
                   color = "lightgray", 
                   binwidth = 4) +
  scale_x_continuous(breaks = seq(24, 60, 8))' -> out
  cat(out)
}