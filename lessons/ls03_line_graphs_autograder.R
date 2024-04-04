# Ch03 Ls07 5NG Line graphs
## Joy Vaz
## 2022-07-11

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load_gh("KO112/KO")
pacman::p_load(tidyverse,
               praise,
               gapminder,
               here)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Init ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 8)   # Put total number of questions as `times` argument


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

gapminder <- gapminder::gapminder

gap_US <- dplyr::filter(gapminder, country == "United States")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create a time series plot of the GPD per capita (`gdpPercap`) recorded in the `gap_US` data frame by using `geom_line()` to create a linegraph.

# [backend]
.CHECK_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- 
      ggplot(gap_US, 
             mapping = aes(x = year, 
                           y = gdpPercap)) +
      geom_line() 
    
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
  Then put the variables you want to map on your x and your y axis inside `mapping = aes()`. 
  Then, add the correct geometry function a line graph.
  If all these are correct, look for typos, missing commas, or unclosed brackets.' -> out
  cat(out)
}
# solution of question
.SOLUTION_q1 <- function(){
  'ggplot(gap_US, 
          mapping = aes(x = year, 
                        y = gdpPercap)) +
      geom_line()' -> out
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
      ggplot(gap_US, 
             mapping = aes(x = year, 
                           y = gdpPercap)) +
      geom_line(lty = "dotdash") +
      geom_point(color = "aquamarine") 
    
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
  'We want to plot a line and points: you need to use two different geometry functions.
  Then we want to make the line dot-dashed and points "aquamarine" by adding fixed aesthetics inside each geom layer.' -> out
  cat(out)
}
# solution of question
.SOLUTION_q2 <- function(){
  'ggplot(gap_US, 
          mapping = aes(x = year, 
                           y = gdpPercap)) +
      geom_line(lty = "dotdash") +
      geom_point(color = "aquamarine")' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Using the `gap_mini` data frame, create a **population** growth chart with these aesthetic mappings:

# [backend]
.CHECK_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- 
      ggplot(gap_mini,
             aes(x = year,
                 y = pop,
                 color = country,
                 linetype = country)) +
      geom_line()
    
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
  "See axis labels to check which variables are mapped to x and y.
  See the guide on the right side of the plot to check which additional 
  aesthetics were mapped to `country`. There are two shown in the plot.
  Make sure you don't add additional modifications that the question did not ask for, or the CHECK function will deem it incorrect." -> out
  cat(out)
}
# solution of question
.SOLUTION_q3 <- function(){
  'ggplot(gap_mini,
             aes(x = year,
                 y = pop,
                 color = country,
                 linetype = country)) +
      geom_line()' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Next, add a layer of points to the previous plot, and add the required aesthetic mappings to produce a plot that looks like this:

# [backend]
.CHECK_q4 <-
  function() {
    .problem_number <<- 4
    
    .q4_correct <- 
      ggplot(gap_mini,
             aes(x = year,
                 y = pop,
                 color = country,
                 shape = continent,
                 lty = country)) +
      geom_line() +
      geom_point()
    
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
  "Take the correct code from q3 and add one additional mapping to make the plot for q4.
  Make sure you don't add additional modifications that the question did not ask for, or the CHECK function will deem it incorrect." -> out
  cat(out)
}
# solution of question
.SOLUTION_q4 <- function(){
  'ggplot(gap_mini,
             aes(x = year,
                 y = pop,
                 color = country,
                 shape = continent,
                 lty = country)) +
      geom_line() +
      geom_point()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q5 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Next, add a layer of points to the previous plot, and add the required aesthetic mappings to produce a plot that looks like this:

# [backend]
.CHECK_q5 <-
  function() {
    .problem_number <<- 5
    
    .q5_correct <- 
      ggplot(data = gap_mini2, 
             mapping = aes(x = year, 
                           y = gdpPercap, 
                           color = country)) +
      geom_line(linewidth = 1) +
      scale_x_continuous(breaks = gap_years) +
      scale_y_continuous(breaks = seq(from = 1000, to = 7000, by = 1000))
    
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
  "
  Make sure you don't add additional modifications that the question did not ask for, or the CHECK function will deem it incorrect." -> out
  cat(out)
}
# solution of question
.SOLUTION_q5 <- function(){
  'ggplot(data = gap_mini2, 
       mapping = aes(x = year, 
                     y = gdpPercap, 
                     color = country)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = gap_years) +
  scale_y_continuous(breaks = seq(from = 1000, to = 7000, by = 1000))' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q6 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# First subset `gapminder` to only the rows containing data for **Uganda:**
# use **`gap_Uganda`** to create a time series plot of population (**`pop`**) over time (**`year`**). Transform the y axis to a log scale, edit the scale breaks to **`gap_years`**, change the line color to `forestgreen` and the line width to 1mm.

# [backend]
.CHECK_q6 <-
  function() {
    .problem_number <<- 6
    
    .q6_correct <- 
      ggplot(data = gap_Uganda, aes(x = year, y = pop)) + 
      geom_line(linewidth = 1, color = "forestgreen")+
      scale_x_continuous(breaks = gap_years) +
      scale_y_log10()
    
    gg_req <- .q6_correct
    gg_ans <- q6
    
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
.HINT_q6 <- function(){
  'First, check that your essential layers are correct dataframe (gap_Uganda), aesthetic mappings, and geom function.
   You need to make two scale modifications: x-axis scale breaks set to match the years in the dataset, 
  and transform the y-axis scale to logarithmic (use the scale function we just learned).
   Add fixed aesthetics color = "foestgreen", and  "linewidth = 1" in geom_line(), but make sure not to add additional modifications that the question did not ask for, or the CHECK function will deem it incorrect.' -> out
  cat(out)
}
# solution of question
.SOLUTION_q6 <- function(){
  'ggplot(data = gap_Uganda, mapping = aes(x = year, y = pop)) + 
      geom_line(linewidth = 1, color = "forestgreen")+
      scale_x_continuous(breaks = gap_years) +
      scale_y_log10()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q7 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.CHECK_q7 <-
  function() {
    .problem_number <<- 7
    
    .q7_correct <- 
      ggplot(data = my_gap_mini, 
             mapping = aes(y = lifeExp, 
                           x = year, 
                           color = country)) +
      geom_line(linewidth = 1, alpha = 0.5) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = gap_years)
    
    gg_req <- .q7_correct
    gg_ans <- q7
    
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
.HINT_q7 <- function(){
  "
  Make sure you don't add additional modifications that the question did not ask for, or the CHECK function will deem it incorrect." -> out
  cat(out)
}
# solution of question
.SOLUTION_q7 <- function(){
  'ggplot(data = my_gap_mini, 
       mapping = aes(y = lifeExp, 
                     x = year, 
                     color = country)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = gap_years)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q8 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.CHECK_q8 <-
  function() {
    .problem_number <<- 8
    
    .q8_correct <- 
      ggplot(data = my_gap_mini, 
             mapping = aes(y = lifeExp, 
                           x = year, 
                           color = country)) +
      geom_line(linewidth = 1, alpha = 0.5) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = gap_years) +
      labs(x = "Year", 
           y = "Longevity",
           title = "Health & wealth of nations",
           color = "Color")
    
    gg_req <- .q8_correct
    gg_ans <- q8
    
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
.HINT_q8 <- function(){
  "
  Make sure you don't add additional modifications that the question did not ask for, or the CHECK function will deem it incorrect." -> out
  cat(out)
}
# solution of question
.SOLUTION_q8 <- function(){
  'ggplot(data = my_gap_mini, 
       mapping = aes(y = lifeExp, 
                     x = year, 
                     color = country)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = gap_years) +
  labs(x = "Year", 
       y = "Longevity",
       title = "Health & wealth of nations",
       color = "Color")' -> out
  cat(out)
}
