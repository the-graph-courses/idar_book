# Data Viz Ls02 Scatterplots
## Laure Vancauwenberghe, Joy Vaz, Kene Nwosu
## 2022-07-08

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
.scores <- rep(-1, times = 6)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.malidd <- read_csv(here::here("data/clean/malidd.csv"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q1 age_height ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Using the `malidd` data frame, create a scatterplot showing the relationship between age and height (`height_cm`).

# [backend]
.CHECK_age_height <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- 
      ggplot(data = .malidd,
             mapping = aes(x = age_months, 
                           y = height_cm)) +
      geom_point()
    
    gg_req <- .q1_correct
    gg_ans <- age_height
    
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
.HINT_age_height <- function(){
  'You can examine the code from the last example to help you with this question.
  The required inputs for a correct answer are:
  (1) data argument - Did you spell the data frame name correctly?
  (2) x and y mappings - Check that you are mapping the correct in aes().
  (3) geom function -  Think about geometric shape you need for a scatter plot.
  Lastly, look for typos, unlosed brackets, missing commas, missing plus signs, and closely read any error messages.'  -> out
  cat(out)
}
# solution of question
.SOLUTION_age_height <- function(){
  'ggplot(data = malidd,
             mapping = aes(x = age_months, 
                           y = height_cm)) +
      geom_point()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q2 v1 age_height_respi (misnomer) ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Using the `malidd` data frame, create a scatterplot showing the relationship between age and viral load, and map a third variable, `freqrespi`, to color:

# [backend]
.CHECK_age_height_respi <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = age_months, 
                           y = viral_load)) + 
      geom_point(mapping = aes(color = freqrespi))
    
    gg_req <- .q2_correct
    gg_ans <- age_height_respi
    
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
.HINT_age_height_respi <- function(){
  'HINT: Examine the code from and earlier example where where we mapped height to color, and do the same with frequency of respiration.
  The required inputs for a correct answer are:
  (1) data argument - Did you spell the data frame name correctly?
  (2) x and y mappings - Check that you are mapping the right variables to x and y inside aes().
  (3) color mapping -  Check that you are mapping the right variable to color inside aes().
  (4) geom function -  Think about geometric shapes you need for a scatter plot.
  Lastly, look for typos, unlosed brackets, missing commas, missing plus signs, and closely read any error messages.'  -> out
  cat(out)
}
# solution of question
.SOLUTION_age_height_respi <- function(){
  'ggplot(data = malidd, 
             mapping = aes(x = age_months, 
                           y = viral_load)) + 
      geom_point(mapping = aes(color = freqrespi))' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q2 v2 age_viral_respi (renamed) ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Using the `malidd` data frame, create a scatterplot showing the relationship between age and viral load, and map a third variable, `freqrespi`, to color:

# [backend]
.CHECK_age_viral_respi <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = age_months, 
                           y = viral_load)) + 
      geom_point(mapping = aes(color = freqrespi))
    
    gg_req <- .q2_correct
    gg_ans <- age_viral_respi
    
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
.HINT_age_viral_respi <- function(){'HINT: Examine the code for the plot where where we mapped height to color, and do the same with frequency of respiration.
  The required inputs for a correct answer are:
  (1) data argument - Did you spell the data frame name correctly?
  (2) x and y mappings - Check that you are mapping the right variables to x and y inside aes().
  (3) color mapping -  Check that you are mapping the color inside aes() to the right variable.
  (4) geom function -  Think about geometric shapes you need for a scatter plot.
  Lastly, look for typos, unlosed brackets, missing commas, missing plus signs, and closely read any error messages.'   -> out
  cat(out)
}
# solution of question
.SOLUTION_age_viral_respi <- function(){
  'ggplot(data = malidd, 
             mapping = aes(x = age_months, 
                           y = viral_load)) + 
      geom_point(mapping = aes(color = freqrespi))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q3 age_height_fever ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_age_height_fever <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = age_months, 
                           y = height_cm)) + 
      geom_point(mapping = aes(color = factor(fever)))
    
    gg_req <- .q3_correct
    gg_ans <- age_height_fever
    
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
## try to fix merge conflict 

# [backend]
# create one hint per question
.HINT_age_height_fever <- function(){
  'HINT: Examine the code for the plot where where we mapped breastfeeding to color, and do the same with fever instead.
  The required inputs for a correct answer are:
  (1) data argument - Did you spell the data frame name correctly? 
  (2) x and y mappings - Check that you are mapping the correct variables to x and y inside aes().
  (3) color mapping -  Check that you are mapping the color inside aes() to the right variable. Keep in mind that ggplot will treat the binay variable `fever` as a continuous variable, but here we want you to give it two distinct colors.
  (4) geom function -  Think about geometric shapes you need for a scatter plot.
  Lastly, look for typos, unlosed brackets, missing commas, or missing plus signs.  
  ' -> out
  cat(out)
}
# solution of question
.SOLUTION_age_height_fever <- function(){
  'ggplot(data = malidd, 
             mapping = aes(x = age_months, 
                           y = height_cm)) + 
      geom_point(mapping = aes(color = factor(fever)))' -> out
  cat(out)
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q4 age_viral_blue ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create a scatterplot with the same variables as the previous example, but change the color of the points to `cornflowerblue`, increase the size of points to 3mm and set the opacity at 60%.

# [backend]
.CHECK_age_viral_blue <-
  function() {
    .problem_number <<- 4
    
    .q4_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = age_months, 
                           y = viral_load)) + 
      geom_point(color = "cornflowerblue",
                 size = 3,
                 alpha = 0.6) 
    
    gg_req <- .q4_correct
    gg_ans <- age_viral_blue
    
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
.HINT_age_viral_blue <- function(){
  'The required inputs for a correct answer are:
  (1) data argument - Did you spell the data frame name correctly?
  (2) x and y mappings - Check that you are mapping the correct variables to x and y inside aes().
  (3) geom function -  Think about geometric shapes you need for a scatter plot.
  (4) fixed color aesthetic -  Check that you are setting the color to a fixed value OUTSIDE aes(). Keep in mind that the color name should be in quotes.
  (5) fixed size aesthetic -  Check that you are setting the size of points to a fixed numeric value OUTSIDE aes().
  (6) fixed alpha aesthetic for opactiy -  Check that you are setting alpha a fixed numeric value between 0 and 1.
  Lastly, look for typos, unlosed brackets, missing commas, missing plus signs, and closely read any error messages.' -> out
  cat(out)
}
# solution of question
.SOLUTION_age_viral_blue <- function(){
  'ggplot(data = malidd, 
             mapping = aes(x = age_months, 
                           y = viral_load)) + 
      geom_point(color = "cornflowerblue",
                 size = 3,
                 alpha = 0.6)' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q5 age_height_2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create a scatterplot with the same variables as the previous example, but change the color of the points to "steelblue", the size to 2.5mm, the transparency to 80%, and add trend line with the smoothing method `lm` (linear model). To make the trend line stand out, change it's color "indianred3".

# [backend]
.CHECK_age_height_2 <-
  function() {
    .problem_number <<- 5
    
    .q5_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = age_months, 
                           y = height_cm)) + 
      geom_point(color = "steelblue",
                 size = 2.5,
                 alpha = 0.8) +
      geom_smooth(method = "lm", color = "indianred3")
    
    gg_req <- .q5_correct
    gg_ans <- age_height_2
    
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
.HINT_age_height_2 <- function(){
  'The required inputs for a correct answer are:
  (1) A data frame, 
  (2) x and y variables
  (3) Two geom functions - one for scatter plot points and one for a smoothing line. 
  (4) fixed color aesthetic for points -  Check that you are setting the color to a fixed value inside the right geom function. 
  (5) fixed size aesthetic for points-  Check that you are setting the size of points to a fixed numeric value OUTSIDE aes().
  (6) fixed alpha aesthetic for point opactiy -  Check that you are setting alpha a fixed numeric value between 0 and 1.
  (7) fixed method for the smoothing line. Keep in mind that this could be in quotes.
  Lastly, look for typos, unlosed brackets, missing commas, missing plus signs, and closely read any error messages.'  -> out
  cat(out)
}
# solution of question
.SOLUTION_age_height_2 <- function(){
  'ggplot(data = malidd, 
          mapping = aes(x = age_months, 
                        y = height_cm)) + 
      geom_point(color = "steelblue",
                 size = 2.5,
                 alpha = 0.8) +
      geom_smooth(method = "lm", color = "indianred3")' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## age_height_3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Recreate the plot above, but this time change the shape to point to tilted rectangles (number 23), and map the body temperature variable (`temp`) to fill color.


# [backend]
.CHECK_age_height_3 <-
  function() {
    .problem_number <<- 6
    
    .q6_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = age_months, 
                           y = height_cm)) + 
      geom_point(color = "steelblue",
                 size = 2.5,
                 alpha = 0.8,
                 shape = 23,
                 mapping = aes(fill = temp)) +
      geom_smooth(method = "lm", color = "indianred3")
    
    gg_req <- .q6_correct
    gg_ans <- age_height_3
    
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
.HINT_age_height_3 <- function(){
  'HINT: First, copy the correct answer from the last practice question, and then add the two aesthetics asked for in the question.
  In addtion to the 7 inputs from the last question, the 2 additional aesthetics you need to add are:
  (1) fixed shape aesthetic for points-  Check that you are setting the shape of points to a fixed numeric value OUTSIDE aes() and INSIDE the geom function for points. 
  (2) fill mappping - Add fill equal to a data variable INSIDE aes(). Note that we want the fill color to only apply to points, NOT the smoothing line  ```````````. So add `mapping = aes()` inside `geom_point()` and map temperature to fever.
  Lastly, look for typos, unlosed brackets, missing commas, missing plus signs, and closely read any error messages.'  -> out
  cat(out)
}
# solution of question
.SOLUTION_age_height_3 <- function(){
  'ggplot(data = malidd, 
          mapping = aes(x = age_months, y = height_cm)) + 
      geom_point(color = "steelblue",
                 size = 2.5,
                 alpha = 0.8,
                 shape = 23,
                 mapping = aes(fill = temp)) +
      geom_smooth(method = "lm", color = "indianred3")' -> out
  cat(out)
}