# Ch03 Ls06 Boxplots
## Joy Vaz
## 2023-03-13

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load_gh("KO112/KO")
pacman::p_load(tidyverse,
               gapminder,
               praise,
               here)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Init ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 6)   # Put total number of questions as `times` argument

set.seed(1024)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.gapminder <- gapminder::gapminder

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
      ggplot(gapminder, aes(continent, gdpPercap, fill = continent)) +
      geom_boxplot(linewidth = 1) 
    
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
  Then put the variables you want to map on your x and y axes inside `mapping = aes()`. 
  Then, add the correct geometry function for a boxplot.
  You need to assign the correct aesthetic arguments for fill color (fill) and line width (linewidth). 
  Remember that aesthetics that are MAPPED to a VARIABLE go inside the aes(), but
  aesthetics that are set to a FIXED  value should go directly inside the geom_*() function.
  If all these are correct: look for typos, missing commas or plus signs, or unclosed brackets.' -> out
  cat(out)
}
# solution of question
.SOLUTION_q1 <- function(){
  'ggplot(data = gapminder,
  mapping = aes(x = continent, y = gdpPercap, fill = continent)) +
  geom_boxplot(linewidth = 1)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Building on your code from the last question, add a **`scale_*()`** function 
#' that transforms the y-axis to a **logarithmic scale**.
#' 
# [backend]
.CHECK_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- 
      ggplot(data = gapminder,
             mapping = aes(x = continent, y = gdpPercap, fill = continent)) +
      geom_boxplot(linewidth = 1) +
      scale_y_log10() 
    
    gg_req <- .q2_correct
    gg_ans <- q2
    
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
.HINT_q2 <- function(){
  'Use the correct code from the previous practice question, and add a new layer with a plus sign.
  Think about which scale_*() function you need to log transform the axis. This was taught in the line graphs lesson. 
  If all these are correct: look for typos, missing commas or plus signs, or unclosed brackets.' -> out
  cat(out)
}

# solution of question
.SOLUTION_q2 <- function(){
  'ggplot(data = gapminder,
  mapping = aes(x = continent, y = gdpPercap, fill = continent)) +
  geom_boxplot(linewidth = 1) +
  scale_y_log10()' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Create the boxplot showing the distribution of GDP per capita for each continent, 
#' like you did in practice question 2. Retain the fill, line width, and scale from that plot.
#' Now, **reorder** the boxes by **mean** `gdpPercap`, in **descending** order.

# [backend]
.CHECK_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- 
      ggplot(gapminder, 
             aes(x = reorder(continent, -gdpPercap),
                 y = gdpPercap,
                 fill = continent)) +
      geom_boxplot(linewidth = 1) +
      scale_y_log10()
    
    gg_req <- .q3_correct
    gg_ans <- q3
    gg_req_alt <- ggplot(gapminder, 
                         aes(x = reorder(continent, -gdpPercap, mean),
                             y = gdpPercap,
                             fill = continent)) +
      geom_boxplot(linewidth = 1) +
      scale_y_log10()
    
    .autograder <<-
      function(){
        if(!is.ggplot(gg_ans)) return(c(value = -1, message = "Wrong. Your result should be a ggplot2 object. Please try again."))
        
        # Use compare_ggplots to check if answer is perfectly correct
        perfect_match1 <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        perfect_match2 <- suppressWarnings(compare_ggplots(gg_req_alt, gg_ans))
        if(isTRUE(perfect_match1) | isTRUE(perfect_match2)) return(c(value = 1, message = paste("Correct!", praise::praise())))
        if(!isTRUE(perfect_match1) & !isTRUE(perfect_match2)) return(c(value = 0, message = "Wrong. Please check the hint and try again."))
        
      }
    .apply_autograder()
  }

# create one hint per question
.HINT_q3 <- function(){
  'Use the correct code from the previous practice question, and edit the x aesthetic mapping.
  You will need to use the reorder() function and specify two arguments in this order:
  (1) The categorical variable to be reordered (your x axis variable)
  (2) The numeric variable you want to reorder by
  To sort boxes in boxplot in DESCENDING order, add a minus sign to the numeric variable.
  Remember to retain all the previous layers and arguments from question 2.
  If all these are correct: look for typos, missing commas or plus signs, or unclosed brackets.' -> out
  cat(out)
}

# solution of question
.SOLUTION_q3 <- function(){
  'ggplot(data = gapminder,
  mapping = aes(
  x = reorder(continent, -gdpPercap), 
  y = gdpPercap, 
  fill = continent)) +
  geom_boxplot(linewidth = 1) +
  scale_y_log10()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Building on the code from the previous question, add **labels** to your plot.
#' -   Set the **main title** to "Variation in GDP per capita across continents (1952-2007)"
#' -   Change the **x-axis title** to "Continent", and
#' -   Change the **y-axis title** to "Income per person (USD)".

# [backend]
.CHECK_q4 <-
  function() {
    .problem_number <<- 4
    
    .q4_correct <- 
      ggplot(data = gapminder,
             mapping = aes(
               x = reorder(continent, -gdpPercap), 
               y = gdpPercap, 
               fill = continent)) +
      geom_boxplot(linewidth = 1) +
      scale_y_log10() +
      labs(title = "Variation in GDP per capita across continents (1952-2007)",
           x = "Continent",
           y = "Income per person (USD)") 
    
    gg_req <- .q4_correct
    gg_ans <- q4
    
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

# create one hint per question
.HINT_q4 <- function(){
  'Use the correct code from the previous practice question, and add a new layer with a plus sign.
  Use the labs() function to change the change the three labels asked for in the question.
  Make sure they match the text from the question exactly (copy and paste them to minimize chances of error).
  Remember to retain all the previous layers and arguments from previous practice questions, and do not make changes that are not asked for in the question.
  If all these are correct: look for typos, missing commas or plus signs, or unclosed brackets.' -> out
  cat(out)
}

# solution of question
.SOLUTION_q4 <- function(){
  'ggplot(data = gapminder,
  mapping = aes(
  x = reorder(continent, -gdpPercap), 
  y = gdpPercap, 
  fill = continent)) +
  geom_boxplot(linewidth = 1) +
  scale_y_log10() +
  labs(title = "Variation in GDP per capita across continents (1952-2007)",
    x = "Continent",
    y = "Income per person (USD)")' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q5 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' -   Take your plot from practice question 4, and add a layer of jittered points.

## [backend]
#.CHECK_q5 <-
#  function() {
#    .problem_number <<- 5
#    
#    .q5_correct <- 
#      ggplot(data = gapminder,
#        mapping = aes(
#          x = reorder(continent, -gdpPercap), 
#          y = gdpPercap, 
#          fill = continent)) +
#      geom_boxplot(linewidth = 1) +
#      scale_y_log10() +
#      labs(title = "Variation in GDP per capita across continents (1952-2007)",
#        x = "Continent",
#        y = "Income per person (USD)") + 
#      geom_jitter()
#    
#    gg_req <- .q5_correct
#    gg_ans <- q5
#    
#    .autograder <<-
#      function(){
#        if(!is.ggplot(gg_ans)) return(c(value = -1, message = "Wrong. Your result should be a ggplot2 object. Please try again."))
#        
#        # Use compare_ggplots to check if answer is perfectly correct
#        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
#        
#        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
#        if(!isTRUE(perfect_match)) return(c(value = 0, message = "Wrong. Please check the hint and try again."))
#      }
#    .apply_autograder()
#  }

# temporary check function
.CHECK_q5 <- function(){
  'Unfortunately this plot cannot be auto-checked because of
  the randomness introduced by the geom_jitter() function. 
  Please use the hint to help you answer this question, and
  compare your plot to the answer shown in the lesson video.
  If you cannot reproduce the answer, run `.SOLUTION_Q5()` 
  to get the answer.' -> out
  cat(out)
}

# create one hint per question
.HINT_q5 <- function(){
  'Use the correct code from the previous practice question, and add a new layer with a plus sign.
  Choose the correct ggplot function you need to add a layer of jittered points.
  Remember to retain all the previous layers and arguments from previous practice questions, and do not make changes that are not asked for in the question.
  If all these are correct: look for typos, missing commas or plus signs, or unclosed brackets.' -> out
  cat(out)
}

# solution of question
.SOLUTION_q5 <- function(){
  'ggplot(data = gapminder,
  mapping = aes(
  x = reorder(continent, -gdpPercap), 
  y = gdpPercap, 
  fill = continent)) +
  geom_boxplot(linewidth = 1) +
  scale_y_log10() +
  labs(title = "Variation in GDP per capita across continents (1952-2007)",
    x = "Continent",
    y = "Income per person (USD)") + 
  geom_jitter()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q6 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' -   Adapt your plot to make the points 45% transparent and change the width of the jitter to 0.3mm.
#' 

# [backend]
#.CHECK_q6 <-
#  function() {
#    .problem_number <<- 6
#    
#    .q6_correct <- 
#      ggplot(data = gapminder, 
#        mapping = aes(x = reorder(continent, -gdpPercap),
#          y = gdpPercap,
#          fill = continent)) +
#      geom_boxplot(linewidth = 1) +
#      scale_y_log10() +
#      labs(title = "Variation in GDP per capita across continents (1952-2007)",
#        x = "Continent",
#        y = "Income per person (USD)") + 
#      geom_jitter(width = 0.3, alpha = 0.55)
#    
#    gg_req <- .q6_correct
#    gg_ans <- q6
#    
#    .autograder <<-
#      function(){
#        if(!is.ggplot(gg_ans)) return(c(value = -1, message = "Wrong. Your result should be a ggplot2 object. Please try again."))
#        
#        # Use compare_ggplots to check if answer is perfectly correct
#        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
#        
#        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
#        if(!isTRUE(perfect_match)) return(c(value = 0, message = "Wrong. Please check the hint and try again."))
#      }
#    .apply_autograder()
#  }
# temporary check function
.CHECK_q6 <- function(){
  'Unfortunately this plot cannot be auto-checked because of
  the randomness introduced by the geom_jitter() function. 
  Please use the hint to help you answer this question, and
  compare your plot to the answer shown in the lesson video.
  If you cannot reproduce the answer, run `.SOLUTION_Q6()` 
  to get the answer.' -> out
  cat(out)
}

# create one hint per question
.HINT_q6 <- function(){
  'Use the correct code from the previous practice question, and add fixed aesthetics to the geom_jitter() layer.
  Remember that the alpha aesthetic controls OPACITY not TRANSPARENCY. 
  To plot points that are 45% percent transparent, they are 55% opaque. Set alpha to 0.55.' -> out
  cat(out)
}

# solution of question
.SOLUTION_q6 <- function(){
  'ggplot(data = gapminder,
  mapping = aes(
  x = reorder(continent, -gdpPercap), 
  y = gdpPercap, 
  fill = continent)) +
  geom_boxplot(linewidth = 1) +
  scale_y_log10() +
  labs(title = "Variation in GDP per capita across continents (1952-2007)",
    x = "Continent",
    y = "Income per person (USD)") + 
  geom_jitter(width = 0.3, alpha = 0.55)' -> out
  cat(out)
}