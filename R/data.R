#' Data from introductory statistics students at a university.
#'
#' Students at a university taking an introductory statistics course were asked to complete this
#' survey as part of their homework.
#'
#' @format A data frame with 157 observations on the following 16 variables:
#' \describe{
#'   \item{\code{Sex}}{Sex of participant.}
#'   \item{\code{RaceEthnic}}{Racial or ethnic background.}
#'   \item{\code{FamilyMembers}}{Members of immediate family (excluding self).}
#'   \item{\code{SSLast}}{Last digit of social security number (\code{NA} if no SSN).}
#'   \item{\code{Year}}{Year in school: \code{1}=First, \code{2}=Second, \code{3}=Third,
#'     \code{4}=Fourth, \code{5}=Other}
#'   \item{\code{Job}}{Current employment status: \code{1}=Not Working, \code{2}=Part-time Job,
#'     \code{3}=Full-time Job}
#'   \item{\code{MathAnxious}}{Agreement with the statement "In general I tend to feel very anxious
#'     about mathematics": \code{1}=Strongly Disagree, \code{2}=Disagree, \code{3}=Neither Agree nor
#'     Disagree, \code{4}=Agree, \code{5}=Strongly Agree}
#'   \item{\code{Interest}}{Interest in statistics and the course: \code{1}=No Interest,
#'     \code{2}=Somewhat Interested, \code{3}=Very Interested}
#'   \item{\code{GradePredict}}{Numeric prediction for final grade in the course. The value is
#'     converted from the student's letter grade prediction. \code{4.0}=A, \code{3.7}=A-,
#'     \code{3.3}=B+, \code{3.0}=B, \code{2.7}=B-, \code{2.3}=C+, \code{2.0}=C, \code{1.7}=C-,
#'     \code{1.3}=Below C-}
#'   \item{\code{Thumb}}{Length in mm from tip of thumb to the crease between the thumb and palm.}
#'   \item{\code{Index}}{Length in mm from tip of index finger to the crease between the index
#'     finger and palm.}
#'   \item{\code{Middle}}{Length in mm from tip of middle finger to the crease between the middle
#'     finger and palm.}
#'   \item{\code{Ring}}{Length in mm from tip of ring finger to the crease between the middle finger
#'     and palm.}
#'   \item{\code{Pinkie}}{Length in mm from tip of pinkie finger to the crease between the pinkie
#'     finger and palm.}
#'   \item{\code{Height}}{Height in inches.}
#'   \item{\code{Weight}}{Weight in pounds.}
#' }
"Fingers"


#' Tables data
#'
#' Data about tips collected from an experiment with 44 tables at a restaurant.
#'
#' @format A data frame with 44 observations on the following 2 variables.
#' \describe{
#'   \item{\code{TableID}}{A number assigned to each table.}
#'   \item{\code{Tip}}{How much the tip was.}
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
#'   \item{\code{ServerID}}{A number assigned to each server.}
#'   \item{\code{Tip}}{How much the tip was.}
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
#'   \item{\code{TableID}}{A number assigned to each table.}
#'   \item{\code{Tip}}{How much the tip was.}
#'   \item{\code{Condition}}{Which experimental condition the table was randomly assigned to.}
#' }
"TipExperiment"


#' Students at a university were asked to enter a random number between 1-20 into a survey.
#'
#' Students at a university taking an introductory statistics course were asked to complete this
#' survey as part of their homework.
#'
#' @format A data frame with 211 observations on the following 1 variable:
#' \describe{
#'   \item{\code{Any1_20}}{The random number between 1 and 20 that a student thought of.}
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
#'   \item{\code{teacher}}{coded as A, B, or C}
#'   \item{\code{outcome}}{each student's score on the outcome test}
#' }
"class_data"
