#' Data from introductory statistics students at a university.
#'
#' Students at a university taking an introductory statistics course were asked to complete this
#' survey as part of their homework.
#'
#' @format A data frame with 157 observations on the following 16 variables:
#' \describe{
#'   \item{Sex}{Sex of participant.}
#'   \item{RaceEthnic}{Racial or ethnic background.}
#'   \item{FamilyMembers}{Members of immediate family (excluding self).}
#'   \item{SSLast}{Last digit of social security number (NA if no SSN).}
#'   \item{Year}{Year in school: 1=First, 2=Second, 3=Third, 4=Fourth, 5=Other}
#'   \item{Job}{Current employment status: 0=not working, 1=part-time job, 2=full-time job}
#'   \item{MathAnxious}{Agreement with this statement "In general I tend to feel very anxious about mathematics": 2=Strongly Agree, 1=Agree, 0=Neither Agree nor Disagree, -1=Disagree, -2=Strongly Disagree}
#'   \item{Interest}{Interest in statistics and the course: 2=Very Interested, 1=Somewhat Interested, 0=No Interest, -1=Dread the Course.}
#'   \item{GradePredict}{Prediction for final grade in the course from the university's grade points per unit: 4.0=A, 3.7=A-, 3.3=B+, 3.0=B, 2.7=B-, 2.3=C+, 2.0=C, 1.7=C-, 1.3=Below C- }
#'   \item{Thumb}{Length in mm from tip of thumb to the crease between the thumb and palm.}
#'   \item{Index}{Length in mm from tip of index finger to the crease between the index finger and palm.}
#'   \item{Middle}{Length in mm from tip of middle finger to the crease between the middle finger and palm.}
#'   \item{Ring}{Length in mm from tip of ring finger to the crease between the middle finger and palm.}
#'   \item{Pinkie}{Length in mm from tip of pinkie finger to the crease between the pinkie finger and palm }
#'   \item{Height}{Height in inches.}
#'   \item{Weight}{Weight in pounds.}
#' }
"Fingers"


#' Tables data
#'
#' Data about tips collected from an experiment with 44 tables at a restaurant.
#'
#' @format A data frame with 44 observations on the following 2 variables.
#' \describe{
#'   \item{TableID}{A number assigned to each table.}
#'   \item{Tip}{How much the tip was.}
#' }
"Tables"


#' Servers data
#'
#' Data about tips collected from an experiment with 44 servers at a restaurant.
#'
#' Note: these data will be removed in future versions in favor of \code{\link{Tables}}.
#'
#' @format A data frame with 44 observations on the following 2 variables.
#' \describe{
#'   \item{ServerID}{A number assigned to each server.}
#'   \item{Tip}{How much the tip was.}
#' }
"Servers"


#' Data from an experiment about smiley faces and tips
#'
#' Tables were randomly assigned to receive checks that either included or did not include a drawing
#' of a smiley face. Data was collected from 44 tables in an effort to examine whether the added
#' smiley face would cause more generous tipping.
#'
#' @format A data frame with 44 observations on the following 3 variables.
#' \describe{
#'   \item{TableID}{A number assigned to each table.}
#'   \item{Tip}{How much the tip was.}
#'   \item{Condition}{Which experimental condition the table was randomly assigned to.}
#' }
"TipExperiment"


#' Students at a university were asked to enter a random number between 1-20 into a survey.
#'
#' Students at a university taking an introductory statistics course were asked to complete this
#' survey as part of their homework.
#'
#' @format A data frame with 211 observations on the following 1 variable:
#' \describe{
#'   \item{Any1_20}{The random number between 1 and 20 that a student thought of.}
#' }
"Survey"


#' Teacher effectiveness data
#'
#' These are hypothetical data for a small study comparing the effectiveness of three different
#' teachers. Each teacher was randomly assigned a group of 35 high school students and asked to
#' teach a 5-day unit on a new science topic for which none of the students had any prior knowledge.
#' All three teachers used the same curriculum materials and lesson plans, and student learning was
#' assessed using a common 30-item test at the end of the unit. The research question was: Were some
#' teachers more effective than others? Did teachers differ in the amount of learning they were able
#' to produce in their students?
#'
#' @format A data frame with 105 observations on the following 2 variables:
#' \describe{
#'   \item{teacher}{coded as A, B, or C}
#'   \item{outcome}{each student's score on the outcome test}
#' }
"class_data"
