#' Titanic Data
#'
#' The sinking of the Titanic is one of the most infamous shipwrecks in history.
#' On April 15, 1912, during her maiden voyage, the widely considered “unsinkable” RMS Titanic sank after colliding with an iceberg. Unfortunately, there weren’t enough lifeboats for everyone onboard, resulting in the death of 1502 out of 2224 passengers and crew.
#' While there was some element of luck involved in surviving, it seems some groups of people were more likely to survive than others.
#' In this challenge, we ask you to build a predictive model that answers the question: “what sorts of people were more likely to survive?” using passenger data (ie name, age, gender, socio-economic class, etc).
#'
#' @docType data
#'
#' @usage data(titanic)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{survival}{Survival}
#'  \item{pclass}{Ticket class}
#'  \item{sex}{Sex}
#'  \item{Age}{Age in years}
#'  \item{sibsp}{number of siblings / spouses}
#'  \item{parch}{number of parents / children}
#'  \item{ticket}{Ticket number}
#'  \item{fare}{Passenger fare}
#'  \item{cabin}{Cabin Number}
#'  \item{cabintype}{Type of cabin}
#'  \item{embarked}{Port of Embarkation}
#' }
#' @references This data set sourced from https://www.kaggle.com/c/titanic/data?select=train.csv
#' @keywords datasets
#' @examples
#'
#' data(titanic)
#' head(titanic)
'titanic'
