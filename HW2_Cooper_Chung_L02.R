# Cooper Chung (Redacted Student ID)
# Fall 2022 BTMA 431 HW2

### Question 1 ###
# Pass midterm.retake the pre-retake grade and the P, B and R values. It will apply the logic (using if statements)
# detailed in the assignment to calculate the post-retake midterm scores with the corresponding
# P, B and R values
midterm.retake <- function(pretake.grade, p.value, b.value, r.value){
  if(pretake.grade < p.value){
    if(r.value >= (p.value + b.value)){
      final.grade <- (p.value + b.value)
    }
    else if(r.value < (p.value + b.value)){
      final.grade <- r.value
    }
  }
  
  if(pretake.grade >= p.value){
    if(r.value >= (pretake.grade + b.value)){
      final.grade <- (pretake.grade + b.value)
    }
    else if(r.value < (pretake.grade + b.value)){
      final.grade <- r.value
    }
  }
  return(final.grade)
}

# Pass highest.retake your pre-retake grade and the P and B values. It will apply the logic (using if statements)
# detailed in the assignment to calculate the highest possible post-retake score
highest.retake <- function(pretake.grade, p.value, b.value){
  if(pretake.grade >= p.value){
    max.grade <- pretake.grade + b.value
  }
  
  if(pretake.grade < p.value){
    max.grade <- p.value + b.value
  }
  return(max.grade)
}

# Similar to highest.retake, it will calculate your highest possible post-retake score,
# but includes 50 extra bonus marks, and uniformly distributes it to generate the R value
highest.retake.bonus <- function(pretake.grade, p.value, b.value){
  if(pretake.grade >= p.value){
    max.grade <- pretake.grade + b.value
    final.grade <- (pretake.grade + b.value)
    r.value <- runif(1, min = 0, max = 150)
    
    if(r.value >= max.grade){
      final.grade <- max.grade
    }
    else if(r.value < max.grade){
      final.grade <- r.value
    }
  }
  
  if(pretake.grade < p.value){
    max.grade <- p.value + b.value
    
    if(r.value >= max.grade){
      final.grade <- max.grade
    }
    else if(r.value < max.grade){
      final.grade <- r.value
    }
  }
  return(max.grade)
}

### Question 1.1 ###
midterm.retake(21, 30, 30, 50)
# Post-retake midterm score would be 50

### Quetion 1.2 ###
midterm.retake(21, 30, 30, 60)
# Post-retake midterm score would be 60

### Question 1.3 ###
midterm.retake(21, 30, 30, 70)
# Post-retake midterm score would be 60

### Question 1.4 ###
highest.retake(21, 30, 30)
# Highest possible post-retake score would be 60

### Question 1.5 ###
midterm.retake(54, 22, 30, 67)
# Post-retake midterm score would be 67

### Question 1.6 ###
midterm.retake(54, 22, 30, 78)
# Post-retake midterm score would be 78

### Question 1.7 ###
midterm.retake(54, 22, 30, 89)
# Post-retake midterm score would be 84

### Question 1.8 ###
highest.retake(54, 22, 30)
# Highest possible post-retake score would be 84

### Question 1.9 ###
highest.retake(22, 30, 30)
# Highest possible post-retake score would be 60

### Question 1.10 ###
highest.retake.bonus(86, 30, 30)
# Highest possible post-retake score with 50 possible bonus marks would be 116

### Question 1 Part 2 ###
# Be sure to load() the exam grades first before using midterm.retake.calculator.
# Pass midterm.retake.calculator the name of the .rda file. Given the grades contained in the
# .rda file, it will determine an appropriate P and B value to use so that the post-retake class average
# will always be between 70 and 75.
midterm.retake.calculator <- function(pretake.grades){
  p.value <- 0:60 # All possible values of P
  b.value <- 0:60 # All possible values of B
  
  p.values <- rep(c(p.value[p.value%%2 == 0]), each = 31) # For all positive multiples of 2 from 0:60, repeat each one 31 times
  value.data.frame <- data.frame(p.values)                # Turn these multiples of 2 into a dataframe, use them as the possible P values
  value.data.frame$b.values <- b.value[b.value%%2 == 0]   # Same as above, but now used for B values
  
  value.data.frame$s.b.summed <- value.data.frame$p.values + value.data.frame$b.values # For all possible permutations of P and B values, create a new column of summed P and B values
  values.filtered <- subset(value.data.frame, s.b.summed >= 60)                        # Create a new dataframe that is a subset of the above dataframe, but filtered to summed values of 60 or above
  post.retake.averages <- data.frame()                                                 # Create an empty dataframe to store the post-retake averages using all possible permutations of P and B
  
  for(i in 1:nrow(values.filtered)){           # For all possible permutations of P and B, and for all grades in the grading file
    for(mark in pretake.grades){
      if(mark >= values.filtered$p.values[i]){ # if the grade is greater than the P value in the permutation
        max.mark <- mark + values.filtered$b.values[i]
        post.retake.averages <- append(post.retake.averages, max.mark) # Append the maximum mark achievable with a certain grade, using a permutation of P and B, into the empty dataframe created earlier
      }
      else if(mark < values.filtered$p.values[i]){                     # if the grade is less than the P value in the permutation
        max.mark <- values.filtered$p.values[i] + values.filtered$b.values[i]
        post.retake.averages <- append(post.retake.averages, max.mark)
      }
    }
  }
  post.retake.averages <- split(post.retake.averages, rep(1:496, each = 30)) # For the list of possible maximum grades, split by every 30 occurrences to isolate for a specific P and B permutation
  post.retake.averages.meaned <- list() # Create an empty list to store the mean of the possible maximum grades
  post.retake.averages.sd <- list()     # Create an empty list to store the standard deviation of the possible maximum grades
  
  for(average in 1:length(post.retake.averages)){ # Append to the empty mean dataframe created earlier the mean of all the maximum grades for each P and B value
    post.retake.averages.meaned <- append(post.retake.averages.meaned, mean(unlist(post.retake.averages[average])))
  }
  values.filtered$max.post.retake.avg <- append(values.filtered$max.post.retake.avg, post.retake.averages.meaned) # Append to original dataframe the means
  
  for(average in 1:length(post.retake.averages)){ # Append to the empty sd dataframe created earlier the sd of all the maximum grades for each P and B value
    post.retake.averages.sd <- append(post.retake.averages.sd, sd(unlist(post.retake.averages[average])))
  }
  values.filtered$sd.post.retake.avg <- append(values.filtered$sd.post.retake.avg, post.retake.averages.sd) # Append to original dataframe the standard deviations
  
  values.filtered <- subset(values.filtered, max.post.retake.avg >= 70 & max.post.retake.avg <= 75)         # Create another subset of the original dataframe containing only values of maximum grades between 70 and 75
  
  min.choice <- which.min(abs(sd(pretake.grades) - unlist(values.filtered$sd.post.retake.avg)))             # Due to multiple possibilities, choose the one whose SD is the closest to the SD of the grading file
  
  p.b.choice <- data.frame(p.value = values.filtered$p.values[min.choice],
                           b.value = values.filtered$b.values[min.choice]) # Create a new dataframe with only the chosen P and B value that minimizes the SD
  
  return(p.b.choice) # Returns the final dataframe
}

### exam_scores1.rda ###
load("exam_scores1.rda")
midterm.retake.calculator(exam_scores1)
# For exam_scores1.rda, the P Value is 26, and the B Value is 34

### exam_scores2.rda ###
load("exam_scores2.rda")
midterm.retake.calculator(exam_scores2)
# For exam_scores2.rda, the P Value is 30, and the B Value is 30

### exam_scores3.rda ###
load("exam_scores3.rda")
midterm.retake.calculator(exam_scores3)
# For exam_scores3.rda, the P Value is 38, and the B Value is 22

### exam_scores4.rda ###
load("exam_scores4.rda")
midterm.retake.calculator(exam_scores4)
# For exam_scores4.rda, the P Value is 42, and the B Value is 18

### exam_scores5.rda ###
load("exam_scores5.rda")
midterm.retake.calculator(exam_scores5)
# For exam_scores5.rda, the P Value is 22, and the B Value is 38

### Question 2 ###
# When calling the student.randomizer function, pass it a whole number of students to choose as the first variable
# and the file name of the class list csv in text (in "quotations") as the second variable. The function will return a list
# containing a randomized selection of the specified number of students in the class. Please ensure the class list csv is
# in the same directory as this file, and the number is equal to or below the count of students in the class.
student.randomizer <- function(number.of.students, class.list){
  class.csv <- read.csv(class.list, stringsAsFactors = FALSE)
  selected.students <- class.csv[sample(nrow(class.csv), number.of.students, replace = FALSE),]
  return(selected.students)
}

# Example
student.randomizer(5, "BTMA 431 L02 (Fall 2022).csv")

### Question 3 ###
# When calling student.marking, pass it the name of the csv in text (in "quotations"). The function will return a dataframe
# containing a student set as a marker, and 3 students they will be marking. It will go through the entire csv until there are
# no more students left to mark. Please ensure the class list csv is in the same directory as this file
student.marking <- function(class.list){
  class.csv <- read.csv(class.list, stringsAsFactors = FALSE)
  marker <- 1
  class.size <- nrow(class.csv)
  marking.sheet <- data.frame(Marker = character(), # Create initial dataframe with empty columns to populate with students
                              Student.1 = character(),
                              Student.2 = character(),
                              Student.3 = character())
  if(nrow(class.csv) >= 4){ # If the class size is greater than 4 constraint
    while(marker <= class.size){
      if((marker+1) <= class.size & (marker+2) <= class.size & (marker+3) > class.size){ # Checking for edge case 1 where there's not enough students to create even groups.
        overflow <- marker - (marker-1)                                                  # 3rd last group will be 1 student short so must wrap back to beginning to ensure 
        marking.sheet[marker,] <- class.csv$First.Name[marker:(marker+3)]                # all students are marked 3 times
        marking.sheet[marker, 4] <- class.csv$First.Name[overflow]
        marker <- marker + 1
      }
      else if((marker+1) <= class.size & (marker+2) > class.size & (marker+3) > class.size){ # Checking for edge case 2 where there's not enough students to create even groups.
        overflow <- marker - (marker-1)                                                      # 2nd last group will be 2 students short. Same reasoning as above
        marking.sheet[marker,] <- class.csv$First.Name[marker:(marker+3)]
        marking.sheet[marker, 3] <- class.csv$First.Name[overflow]
        marking.sheet[marker, 4] <- class.csv$First.Name[overflow + 1]
        marker <- marker + 1
      }
      else if((marker+1) > class.size & (marker+2) > class.size & (marker+3) > class.size){ # Checking for edge case 3 where there's not enough students to create even groups.
        overflow <- marker - (marker-1)                                                     # Final group will be 3 students short. Same reasoning as above
        marking.sheet[marker,] <- class.csv$First.Name[marker:(marker+3)]
        marking.sheet[marker, 2] <- class.csv$First.Name[overflow]
        marking.sheet[marker, 3] <- class.csv$First.Name[overflow + 1]
        marking.sheet[marker, 4] <- class.csv$First.Name[overflow + 2]
        marker <- marker + 1
      }  
      else{
        marking.sheet[marker,] <- class.csv$First.Name[marker:(marker+3)] # If no edge cases are found, select a student, and populate 3 students to mark with the next 3 students in
        marker <- marker + 1                                              # the class list
      }
    }
  }
  return(marking.sheet) # Returns the dataframe created in the beginning, now populated with the required information
}

# Example
student.marking("BTMA 431 L02 (Fall 2022).csv")

### Question 4 ###
simulation.attempts <- 10000 # Change to 1,000,000 for more accurate results

# Pass difference.calculator your chosen number to play the game. It will simulate the second number a certain amount of times to
# determine the mean payout if your chosen number to play the game
difference.calculator <- function(chosen.number){
  second.number <- sample(0:1000, simulation.attempts, replace = TRUE)
  payout <- (abs(chosen.number - second.number))^2
  return(mean(payout))
}

### Question 4a ###
difference.calculator(30)
# Expected payout (loss) for selecting 30 is $305,000

### Question 4b ###
difference.calculator(950)
# Expected payout (loss) for selecting 950 is $285,000

### Question 4c ###
difference.calculator(450)
# Expected payout (loss) for selecting 450 is $85,000

### Question 4d ###
number.chosen <- 0:1000     # Possible range of numbers to choose from
expected.loss <- numeric(0) # Empty numeric vector to store numbers

for(i in 1:length(number.chosen)){ # For every single number in the possible range, calculate the expected payout (loss) and add it to the empty numeric vector
  expected.loss <- c(expected.loss, difference.calculator(i))
}

which.min(expected.loss) # For every single number in the possible range, which one has the lowest expected payout (loss)
# The number that gives the lowest expected payout (loss) would be 500 

### Question 4e ###
difference.calculator(500)
# At 500, the expected payout (loss) is $85,000

### Question 4f ###
# Did not attempt