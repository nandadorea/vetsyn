#' @name lab.daily
#' @title Daily submissions to an animal diagnostic laboratory
#' @description This dataset was extracted from a database of laboratory test
#' submissions. The actual dates have been altered and the animal species 
#' is not given, in order to 
#' comply with the privacy policies. Further, all test information
#' has been removed and only the final syndromic classification is given. Only
#' the minimal nuber of columns needed to demonstrate the functionalities of the package
#' have been kept. These are: a column with the syndromic classification, a column 
#' specifying the date of submission, and one or multiple columns used to 
#' identify unique cases. In this particular dataset a case is defined as a unique 
#' combination of date, herdID and submissionID. HerdID and SubmissionID in this 
#' dataset have been altered to ensure privacy.
#' @docType data
#' @usage lab.daily
#' @format a \code{data.frame} 
#' @source package \code{vetsyn}
#' @author National Veterinary Institute of Sweden (SVA), 2013
#' 