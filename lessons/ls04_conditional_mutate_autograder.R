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
.flu_linelist <- read_csv(here::here('data/flu_h7n9_china_2013.csv'))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 9)   # Put total number of questions as `times` argument

.NUM_Q_age_group <- 1
.NUM_Q_age_group_percentage <- 2
.NUM_Q_age_group_nas <- 3
.NUM_Q_gender_recode <- 4
.NUM_Q_recode_recovery <- 5
.NUM_Q_adolescent_grouping <- 6
.NUM_Q_age_province_grouping <- 7
.NUM_Q_priority_groups <- 8
.NUM_Q_age_group_if_else <- 9


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  ANSWERS ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_age_group ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_age_group <-
  function() {
    
    .problem_number <<- .NUM_Q_age_group
    correct_answer <- .flu_linelist %>% 
      mutate(age_group = case_when(age < 50 ~ "Below 50", 
                                   age >= 50 ~ "50 and above"))
    
    .autograder <<-
      function(){
        if(!exists("Q_age_group"))
          .na("You have not yet defined the answer object, `Q_age_group`.")
        if (!is.data.frame(Q_age_group))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"age_group" %in% names(Q_age_group))
          .fail("Your answer should have a column called 'age_group'.")
        
        
        if (isTRUE(all.equal(select(Q_age_group, age_group) , 
                             select(correct_answer, age_group))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_age_group <- function(){
  '
HINT.
Your answer should look this: 

Q_age_group <- 
  flu_linelist %>% 
  mutate(age_group = FORMULA_HERE)' -> out
  cat(out)
}

.SOLUTION_Q_age_group <- function(){
  '

Q_age_group <- 
  flu_linelist %>% 
  mutate(age_group = case_when(age < 50 ~ "Below 50", 
                                   age >= 50 ~ "50 and above"))' -> out
  cat(out)
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_age_group_percentage ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_age_group_percentage <-
  function() {
    
    .problem_number <<- .NUM_Q_age_group_percentage
    
    correct_answer <- 
      .flu_linelist %>% 
      mutate(age_group_percentage = case_when(age < 60 ~ "Below 60", 
                                              age >= 60 ~ "60 and above")) %>% 
      tabyl(age_group_percentage) %>% 
      filter(age_group_percentage == "Below 60") %>% 
      pull(percent) * 100
      
    
    .autograder <<-
      function(){
        if(!exists("Q_age_group_percentage"))
          .na("You have not yet defined the answer object, `Q_age_group_percentage`.")
        if (!is.numeric(Q_age_group_percentage))
          .na("Invalid answer. Your answer should be a number")
        if (between(Q_age_group_percentage, correct_answer/100 - 0.1, correct_answer/100 + 0.1))
          .fail("You may be entering a decimal fraction, rather than a percentage")
        if (between(Q_age_group_percentage, correct_answer - 1, correct_answer + 1))
          .pass()
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_age_group_percentage <- function(){
  '
HINT
  Ideally, write a case_when() call that contains the conditions `age < 60`, then call janitor::tabyl() on your created variable.
' -> out
  cat(out)
}


.SOLUTION_Q_age_group_percentage <- function(){
  '
 Here is one way (not the only way) to get it:
  
Q_age_group_percentage <- 
  flu_linelist %>% 
      mutate(age_group_percentage = case_when(age < 60 ~ "Below 60", 
                                              age >= 60 ~ "60 and above")) %>% 
      tabyl(age_group_percentage) %>% 
      filter(age_group_percentage == "Below 60") %>% 
      pull(percent) * 100' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_age_group_nas ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_age_group_nas <-
  function() {
    
    .problem_number <<- .NUM_Q_age_group_nas
    correct_answer <- .flu_linelist %>%  
      mutate(age_group = case_when(age < 60 ~ "Below 60", 
                                   age >= 18 ~ "60 and above", 
                                   is.na(age) ~ "Missing age"))
    
    .autograder <<-
      function(){
        if(!exists("Q_age_group_nas"))
          .na("You have not yet defined the answer object, `Q_age_group_nas`.")
        if (!is.data.frame(Q_age_group_nas))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"age_group" %in% names(Q_age_group_nas))
          .fail("Your answer should have a column called 'age_group'.")
        
        
        if (isTRUE(all.equal(select(Q_age_group_nas, age_group) , 
                             select(correct_answer, age_group))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_age_group_nas <- function(){
  '
HINT.
Your answer should look this: 

Q_age_group_nas <- 
  flu_linelist %>% 
  mutate(age_group = FORMULA_HERE)' -> out
  cat(out)
}

.SOLUTION_Q_age_group_nas <- function(){
  '

Q_age_group_nas <- 
  flu_linelist %>% 
  mutate(age_group = case_when(age < 60 ~ "Below 60", 
                               age >= 60 ~ "60 and above", 
                               is.na(age) ~ "Missing age"))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_gender_recode ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_gender_recode <-
  function() {
    
    .problem_number <<- .NUM_Q_gender_recode
    correct_answer <- .flu_linelist %>%  
      mutate(gender = case_when(gender == "f" ~ "Female",
                                gender == "m" ~ "Male", 
                                is.na(gender) ~ "Missing gender"))
    
    .autograder <<-
      function(){
        if(!exists("Q_gender_recode"))
          .na("You have not yet defined the answer object, `Q_gender_recode`.")
        if (!is.data.frame(Q_gender_recode))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"gender" %in% names(Q_gender_recode))
          .fail("Your answer should have a column called 'gender'.")
        
        
        if (isTRUE(all.equal(select(Q_gender_recode, gender) , 
                             select(correct_answer, gender))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_gender_recode <- function(){
  '
HINT.
Your answer should look this: 

Q_gender_recode <- 
  flu_linelist %>% 
  mutate(gender = case_when(STATEMENTS HERE))' -> out
  cat(out)
}

.SOLUTION_Q_gender_recode <- function(){
  '

Q_gender_recode <- 
  flu_linelist %>%  
      mutate(gender = case_when(gender == "f" ~ "Female",
                                gender == "m" ~ "Male",
                                is.na(gender) ~ "Missing gender"))' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_recode_recovery ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_recode_recovery <-
  function() {
    
    .problem_number <<- .NUM_Q_recode_recovery
    correct_answer <- .flu_linelist %>%  
      mutate(outcome = case_when(outcome == "Recover" ~ "Recovery", 
                                 TRUE ~ outcome))
    
    .autograder <<-
      function(){
        if(!exists("Q_recode_recovery"))
          .na("You have not yet defined the answer object, `Q_recode_recovery`.")
        if (!is.data.frame(Q_recode_recovery))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"outcome" %in% names(Q_recode_recovery))
          .fail("Your answer should have a column called 'outcome'.")
        
        
        if (isTRUE(all.equal(select(Q_recode_recovery, outcome) , 
                             select(correct_answer, outcome))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_recode_recovery <- function(){
  '
HINT.
Your answer should look this: 

Q_recode_recovery <- 
  flu_linelist %>% 
  mutate(outcome = FORMULA_HERE)' -> out
  cat(out)
}

.SOLUTION_Q_recode_recovery <- function(){
  '

Q_recode_recovery <- 
  flu_linelist %>% 
  mutate(outcome = case_when(outcome == "Recover" ~ "Recovery", 
                                 TRUE ~ outcome))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_adolescent_grouping ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_adolescent_grouping <-
  function() {
    
    .problem_number <<- .NUM_Q_adolescent_grouping
    correct_answer <- .flu_linelist %>% 
      mutate(adolescent = case_when(
        age >= 10 & age < 20 ~ "Yes",
        TRUE ~ "No"
      ))
    
    mistake1 <- .flu_linelist %>% 
      mutate(adolescent = case_when(
        age >= 10 & age < 19 ~ "Yes",
        TRUE ~ "No"
      ))
    
    .autograder <<-
      function(){
        if(!exists("Q_adolescent_grouping"))
          .na("You have not yet defined the answer object, `Q_adolescent_grouping`.")
        if (!is.data.frame(Q_adolescent_grouping))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"adolescent" %in% names(Q_adolescent_grouping))
          .fail("Your answer should have a column called 'adolescent'.")
        
           if (isTRUE(all.equal(select(Q_adolescent_grouping, adolescent) , 
                                 select(mistake1, adolescent))))
          .fail("Your condition should be `age >= 10 & age < 20`")
        
        if (isTRUE(all.equal(select(Q_adolescent_grouping, adolescent) , 
                             select(correct_answer, adolescent))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_adolescent_grouping <- function(){
  '
HINT.
THe LHS condition should take the form `age >= 10 & age < 20`' -> out
  cat(out)
}

.SOLUTION_Q_adolescent_grouping <- function(){
  '

Q_adolescent_grouping <- 
  flu_linelist %>% 
      mutate(adolescent = case_when(
        age >= 10 & age < 20 ~ "Yes",
        TRUE ~ "No"))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_age_province_grouping ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_age_province_grouping <-
  function() {
    
    .problem_number <<- .NUM_Q_age_province_grouping
    correct_answer <- .flu_linelist %>% 
      mutate(recruit = case_when(
        province == "Jiangsu" & (age >= 30 & age < 60) ~ "Recruit to Jiangsu study",
        province == "Zhejiang" & (age >= 30 & age < 60) ~ "Recruit to Zhejiang study",
        TRUE ~ "Do not recruit"
      ))
    
    .autograder <<-
      function(){
        if(!exists("Q_age_province_grouping"))
          .na("You have not yet defined the answer object, `Q_age_province_grouping`.")
        if (!is.data.frame(Q_age_province_grouping))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"recruit" %in% names(Q_age_province_grouping))
          .fail("Your answer should have a column called 'recruit'.")
        
        
        if (isTRUE(all.equal(select(Q_age_province_grouping, recruit) , 
                             select(correct_answer, recruit))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_age_province_grouping <- function(){
  '
HINT.
Here is an example LHS condition: 
  `age >= 30 & age <= 59 & province == "Zhejiang" ~ "Recruit to Zhejiang study"` ' -> out
  cat(out)
}

.SOLUTION_Q_age_province_grouping <- function(){
  '

Q_age_province_grouping <- 
  flu_linelist %>% 
      mutate(recruit = case_when(
        province == "Jiangsu" & (age >= 30 & age < 60) ~ "Recruit to Jiangsu study",
        province == "Zhejiang" & (age >= 30 & age < 60) ~ "Recruit to Zhejiang study",
        TRUE ~ "Do not recruit"
      ))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_priority_groups ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.CHECK_Q_priority_groups <-
  function() {
    
    .problem_number <<- .NUM_Q_priority_groups
    correct_answer <- .flu_linelist %>% 
      mutate(follow_up_priority = case_when(
        age < 18 ~ "Highest priority", 
        gender == "f" ~ "High priority", 
        TRUE ~ "No priority"
      ))
    
    mistake1 <- .flu_linelist %>% 
      mutate(follow_up_priority = case_when(gender == "f" ~ "High priority",
                                            age < 18 ~ "Highest priority", 
                                            TRUE ~ "No priority"
      ))
    
    
    .autograder <<-
      function(){
        if(!exists("Q_priority_groups"))
          .na("You have not yet defined the answer object, `Q_priority_groups`.")
        if (!is.data.frame(Q_priority_groups))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"follow_up_priority" %in% names(Q_priority_groups))
          .fail("Your answer should have a column called 'follow_up_priority'.")
        
        
        if (isTRUE(all.equal(select(Q_priority_groups, follow_up_priority) , 
                             select(mistake1, follow_up_priority))))
          .fail(paste0("Wrong. It seems you put the gender condition first. This means it takes precedence over the age condition.", 
                       "That is, with what you have coded, female children won't get 'highest priority', only male children!",
                       "But the question says that children of any gender should be highest priority.",
                       "So this condition needs to come first in your case_when() statement."))
        
        
        if (isTRUE(all.equal(select(Q_priority_groups, follow_up_priority) , 
                             select(correct_answer, follow_up_priority))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_priority_groups <- function(){
  '
HINT.

The age condition needs to come first, in order to take priority over the others.' -> out
  cat(out)
}

.SOLUTION_Q_priority_groups <- function(){
  '

Q_priority_groups <- 
  flu_linelist %>% 
      mutate(follow_up_priority = case_when(
        age < 18 ~ "Highest priority", 
        gender == "f" ~ "High priority", 
        TRUE ~ "No priority"
      ))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_age_group_if_else ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_age_group_if_else <-
  function() {
    
    .problem_number <<- .NUM_Q_age_group_if_else
    correct_answer <- .flu_linelist %>% 
      mutate(age_group = if_else(age < 50, "Below 50", "50 and above"))
    
    .autograder <<-
      function(){
        if(!exists("Q_age_group_if_else"))
          .na("You have not yet defined the answer object, `Q_age_group_if_else`.")
        if (!is.data.frame(Q_age_group_if_else))
          .na("Invalid answer. Your answer should be a data frame.")
        if (!"age_group" %in% names(Q_age_group_if_else))
          .fail("Your answer should have a column called 'age_group'.")
        
        
        if (isTRUE(all.equal(select(Q_age_group_if_else, age_group) , 
                             select(correct_answer, age_group))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_age_group_if_else <- function(){
  '
HINT.
Your answer should look this: 

Q_age_group_if_else <- 
  flu_linelist %>% 
  mutate(age_group = if_else(outcome = FORMULA_HERE))' -> out
  cat(out)
}

.SOLUTION_Q_age_group_if_else <- function(){
  '

Q_age_group_if_else <- 
  flu_linelist %>% 
  mutate(age_group = if_else(age < 50, "Below 50", "50 and above"))' -> out
  cat(out)
}

