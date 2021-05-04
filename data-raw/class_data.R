## Generate data for three instructors
##
## The instructors will have means such that C > B > A, but the difference is only
## clearly significant for C > A, and borderline for the others.
##
## You can set the random seed explicitly or let R generate a random one. If you find
## one you like (R will print out the random ones so you can see which was used), save
## it so you can use it later.
generate_data <- function(seed) {
  if (missing(seed)) {
    seed <- ceiling(runif(1, 1, 10000))
    cat("Random seed: ", seed, "\n")
  }

  set.seed(seed)

  students_per_teacher <- 35
  number_of_teachers <- 3
  teachers <- c("A", "B", "C")

  tibble::tibble(
    outcome = c(
      rnorm(students_per_teacher, 11.7, 3),
      rnorm(students_per_teacher, 12.7, 3),
      rnorm(students_per_teacher, 13.5, 3)
    ),
    teacher = rep(teachers, each = students_per_teacher)
  )
}

class_data <- generate_data(9900)
class_data$outcome <- round(class_data$outcome, 0)
usethis::use_data(class_data, overwrite = TRUE)
