# Ch03 Ls06 5NG Scatterplots
## Joy Vaz
## 2022-09-03

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load_gh("KO112/KO")

#pacman::p_load(tidyverse,
#               OpenImageR,
#               here,
#               png,
#               jpeg,
#               svglite)
#

pacman::p_load(tidyverse,
               digest,
               here,
               praise)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ggplot_digest function ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Custom ggplot digest function
## Joy Vaz
## 2022-11-17

#' Function to generate hash function digests of ggplot objects.
#' Intended for use in data viz data quizzes. 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ggplot_digest function ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot_digest <- function(myplot) {
  # Create temp file name, save plot, digest file, and remove temp file
  plotfile <- tempfile(pattern = "ggplot_", fileext = ".png")
  suppressWarnings(suppressMessages(ggplot2::ggsave(filename = plotfile, plot = myplot, type = "cairo", device = "png")))
  plot_crypt <- digest::digest(file = plotfile)
  file.remove(plotfile)
  return(plot_crypt)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load Data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load nigerm into hidden environment
.my.env <- new.env()

load(here("data/clean/nigerm_cases_rgn.RData"),
     envir = .my.env)

.nigerm <- local(nigerm, envir = .my.env)

# Create nigerm04
.nigerm04 <- .nigerm %>%
  # filter data frame to only include data from 1996
  filter(year == 1996)  %>% 
  # remove the year column
  select(-year)

# Create nigerm04
.nigerm04 <- .nigerm %>%
  # filter data frame to only include data from 2004
  filter(year == 2004)  %>% 
  # remove the year column
  select(-year)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Init ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 4)   # Put total number of questions as `times` argument



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1: nigerm04_scatter ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Using the `nigerm04` data frame, write `ggplot` code that will create a 
#' scatter plot displaying the relationship between `cases` on the y-axis and 
#' `week` on the x-axis.

# [backend]
.CHECK_nigerm04_scatter <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- 
      ggplot(data = .nigerm04,
             mapping = aes(x = week, 
                           y = cases)) +
      geom_point()
    
    .autograder <<-
      function(){
        if (!is.ggplot(nigerm04_scatter)) return(c(value = -1, message = "Your result should be a ggplot2 object."))
        
        # test 1
        # that data used is correct
        .q1_test1 <- all.equal(
          target = as_tibble(nigerm04_scatter$data), 
          current = as_tibble(.q1_correct$data))
        
        # test 2
        # that learner used geom_point()
        .q1_test2 <- any(stringr::str_detect(capture.output(nigerm04_scatter$layers), 
                                             "geom_point"))
        # test 3
        # check the x mapping
        .q1_test3 <- "* `x` -> `week`" %in% capture.output(nigerm04_scatter$mapping)
        
        # test 4
        # check the y mapping
        .q1_test4 <- "* `y` -> `cases`" %in% capture.output(nigerm04_scatter$mapping)
        
        if (isTRUE(!(.q1_test1))) 
          return(c(value = 0, message = "! Check that you are plotting the correct data."))
        if (isTRUE(!(.q1_test2))) 
          return(c(value = 0, message = "! Make sure you're using geometric function `geom_point()`."))
        if (isTRUE(!(.q1_test3 && .q1_test4))) 
          return(c(value = 0, message = "! Check your mapping arguments for x and y: are you putting the right variables?"))
        if (isTRUE(.q1_test1 && .q1_test2 && .q1_test3 && .q1_test4)) 
          return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong answer. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.HINT_nigerm04_scatter <- function(){
  'First, identify which data frame to supply to the data layer.
  Then put the variables you want to map on your x and your y axis inside `mapping = aes()`. 
  Then, think about which geometry function you need for a scatter plot.
  If all these are correct, look for typos, missing commas, or unclosed brackets.' -> out
  cat(out)
}
# solution of question
.SOLUTION_nigerm04_scatter <- function(){
  'ggplot(data = nigerm04,
             mapping = aes(x = week, 
                           y = cases)) +
      geom_point()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2: nigerm04_bar ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Use the `nigerm04` data frame to create a bar plot of weekly cases with the 
#' `geom_col()` function. Map `cases` on the y-axis and `week` on the x-axis.

# [backend]
.CHECK_nigerm04_bar <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- 
      ggplot(data = .nigerm04, 
             mapping = aes(x = week, 
                           y = cases)) + 
      geom_col()
    
    .autograder <<-
      function(){
        if (!is.ggplot(nigerm04_bar)) return(c(value = -1, message = "Your result should be a ggplot2 object."))
        
        # test 1
        # that data used is correct
        .q2_test1 <- all.equal(
          target = as_tibble(nigerm04_bar$data), 
          current = as_tibble(.q2_correct$data))
        
        # test 2
        # that learner used geom_point()
        .q2_test2 <- any(stringr::str_detect(capture.output(nigerm04_bar$layers), 
                                             "geom_col"))
        # test 3
        # check the x mapping
        .q2_test3 <- "* `x` -> `week`" %in% capture.output(nigerm04_bar$mapping)
        
        # test 4
        # check the y mapping
        .q2_test4 <- "* `y` -> `cases`" %in% capture.output(nigerm04_bar$mapping)
        
        if (isTRUE(!(.q2_test1))) return(c(value = 0, message = "! Check that you are plotting the correct dataframe."))
        if (isTRUE(!(.q2_test2))) return(c(value = 0, message = "! Do not forget to use ggplot2 geometry function `geom_col()`."))
        
        
        # KENE EDIT:
        if (ggplot_digest(.q2_correct) == ggplot_digest(nigerm04_bar)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        
        # 
        # 
        # if (isTRUE(!(.q2_test3 && .q2_test4))) return(c(value = 0, message = "! Check your mapping arguments for x and y: are you putting the right variables?"))
        # if (isTRUE(.q2_test1 && .q2_test2 && .q2_test3 && .q2_test4 && .q2_test5)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong answer. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.HINT_nigerm04_bar <- function(){
  'First, identify which data frame to supply to the data layer.
  Then put the variables you want to map on your x and your y axis inside `mapping = aes()`. 
  Then, think about which geometry function you need for this bar plot (same as what we used earlier in this lesson).
  If all these are correct, look for typos, missing commas, or unclosed brackets.' -> out
  cat(out)
}
# solution of question
.SOLUTION_nigerm04_bar <- function(){
  'ggplot(data = nigerm04, 
          mapping = aes(x = week, 
                        y = cases)) + 
     geom_col()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3: nigerm04_line ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Use the `nigerm04` data frame to create a line graph of weekly cases, colored
#'  by `region`. The geom function for a line graph is called `geom_line()`. Map
#'  `cases` on the y-axis, `week` on the x-axis, and `region` to color.

# [backend]
.CHECK_nigerm04_line <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- 
      ggplot(data = .nigerm04, 
             mapping = aes(x = week, 
                           y = cases,
                           color = region)) + 
      geom_line()
    
    .autograder <<-
      function(){
        if (!is.ggplot(nigerm04_line)) return(c(value = -1, message = "Your result should be a ggplot2 object."))
        
        # test 1
        # that data used is correct
        .q3_test1 <- all.equal(
          target = as_tibble(nigerm04_line$data), 
          current = as_tibble(.q3_correct$data))
        
        # test 2
        # that learner used geom_point()
        .q3_test2 <- any(stringr::str_detect(capture.output(nigerm04_line$layers), 
                                             "geom_line"))
        # test 3
        # check the x mapping
        .q3_test3 <- "* `x` -> `week`" %in% capture.output(nigerm04_line$mapping)
        
        # test 4
        # check the y mapping
        .q3_test4 <- "* `y` -> `cases`" %in% capture.output(nigerm04_line$mapping)
        
        # test 5
        # check the color argument: UK spelling
        .q3_test5 <- any(stringr::str_detect(capture.output(nigerm04_line$layers), 
                                             "colour = ~region"))
        
        if (isTRUE(!(.q3_test1))) return(c(value = 0, message = "! Check that you are plotting the correct data."))
        
        # KENE EDIT:
        if (ggplot_digest(.q3_correct) == ggplot_digest(nigerm04_line)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        
        
        # 
        # if (isTRUE(!(.q3_test2))) return(c(value = 0, message = "! Do not forget to use ggplot2 geometry function geom_line."))
        # if (isTRUE(!(.q3_test3 && .q3_test4))) return(c(value = 0, message = "! Check your mapping arguments for x and y: are you putting the right variables?"))
        # if (isTRUE(!(.q3_test5))) return(c(value = 0, message = "! Do not forget to color the points using a mapping argument in your layers."))
        # 
        # if (isTRUE(.q3_test1 && .q3_test2 && .q3_test3 && .q3_test4 && .q3_test5)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        
        return(c(value = 0, message = "Wrong answer. Please try again."))
      }
    .apply_autograder()
  }


# [backend]
# create one hint per question
.HINT_nigerm04_line <- function(){
  'First, identify which data frame to supply to the data layer.
  Then put the variables you want to map on your x axis, y axis, and color inside `mapping = aes()`. 
  Then, use the correct geometry function needed for a line graph: `geom_line()`.
  If all these are correct, look for typos, missing commas, or unclosed brackets.' -> out
  cat(out)
}
# solution of question
.SOLUTION_nigerm04_line <- function(){
  "ggplot(data = nigerm04,
         mapping = aes(x = week,
                       y = cases,
                       color = region)) + 
    geom_line()" -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Section label ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Use the `nigerm04` data frame to create a bar graph of weekly cases, and fill
#'  all bars with the color "hotpink". Map `cases` on the y-axis, `week` on the 
#'  x-axis, and set the bars to constant color.


# ggplot(data = nigerm04, 
#        mapping = aes(x = week, 
#                      y = cases)) +
#   geom_col(fill = "hotpink")



# [backend]
.CHECK_nigerm04_pinkbar <-
  function() {
    .problem_number <<- 4
    
    .nigerm04_pinkbar <- 
      ggplot(data = .nigerm04, 
             mapping = aes(x = week, 
                           y = cases)) +
      geom_col(fill = "hotpink")
    
    
    .autograder <<-
      function(){
        if (!is.ggplot(nigerm04_pinkbar)) return(c(value = -1, message = "Your result should be a ggplot2 object."))
        
        if (ggplot_digest(.nigerm04_pinkbar) == ggplot_digest(nigerm04_pinkbar)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        
        return(c(value = 0, message = "Wrong answer. Please try again."))
      }
    .apply_autograder()
  }


# [backend]
# create one hint per question
.HINT_nigerm04_pinkbar <- function(){
  'Put the variables you want to map on your x axis, y axis inside `mapping = aes()`. 
  Then, use the correct geometry function, `geom_col()` with the right `fill` argument.' -> out
  cat(out)
}
# solution of question
.SOLUTION_nigerm04_pinkbar <- function(){
  'ggplot(data = nigerm04, 
          mapping = aes(x = week, 
                        y = cases)) +
      geom_col(fill = "hotpink")' -> out
  cat(out)
}
