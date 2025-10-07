########################################################
# Learning by parenting #
########################################################

# Author: Alicia García-Sierra
# Data used: NLSY and NLSY-CYA
# Date: November 2023


#######################################################

#######################################################
######### 0) PREPARE THE ENVIRONMENT ################
######################################################

# CLEAN

rm(list=ls()) 

# LOAD PACKAGES

library(tidyverse)
library(haven)
library(stringr)
library(tidyr)
library(labelled)
library(readxl)
library(lme4)
library(fixest)
library(panelView)
library(fect)
library(plm)
library(did)

# SET WD 

setwd("/Users/agarcias/Library/CloudStorage/OneDrive-UniversitédeLausanne/DPHIL/SECOND PAPER/R&R/METHODOLOGY")

#########################################
######## 1)  IMPORT DATA ##################
#########################################




# Set working directory
# setwd()


new_data <- read.table('LEARNING.dat', sep=' ')
names(new_data) <- c('C0000100',
                     'C0000200',
                     'C0005300',
                     'C0005400',
                     'C0005700',
                     'C0005800',
                     'C0006500',
                     'C0006800',
                     'C0007000',
                     'C0007010',
                     'C0007030',
                     'C0007041',
                     'C0007043',
                     'C0007045',
                     'C0007047',
                     'C0007049',
                     'C0007052',
                     'C0007055',
                     'C0007800',
                     'C0008000',
                     'C0008020',
                     'C0008040',
                     'C0008042',
                     'C0008043',
                     'C0008044',
                     'C0008045',
                     'C0008046',
                     'C0008047',
                     'C0008048',
                     'C0059901',
                     'C0061134',
                     'C0061136',
                     'C0061138',
                     'C0061140',
                     'C0061141',
                     'C0061142',
                     'C0061143',
                     'C0061144',
                     'C0061145',
                     'C0061146',
                     'C0128000',
                     'C0571600',
                     'C0580100',
                     'C0580400',
                     'C0792000',
                     'C0799600',
                     'C0799900',
                     'C0992000',
                     'C0998800',
                     'C0999100',
                     'C1192300',
                     'C1198800',
                     'C1199100',
                     'C1500100',
                     'C1507800',
                     'C1508100',
                     'C1557000',
                     'C1564700',
                     'C1565000',
                     'C1792700',
                     'C1800100',
                     'C1800400',
                     'C2502600',
                     'C2503700',
                     'C2503900',
                     'C2531100',
                     'C2532200',
                     'C2532400',
                     'C2802400',
                     'C2803000',
                     'C2803200',
                     'C3110400',
                     'C3111500',
                     'C3111700',
                     'C3601100',
                     'C3601600',
                     'C3602401',
                     'C3614100',
                     'C3615200',
                     'C3615400',
                     'C3981100',
                     'C3981600',
                     'C3982401',
                     'C3992700',
                     'C3993800',
                     'C3994000',
                     'C5524800',
                     'C5525400',
                     'C5526300',
                     'C5536700',
                     'C5537800',
                     'C5538000',
                     'C5801100',
                     'C5801700',
                     'C5802600',
                     'C5812500',
                     'C5813600',
                     'C5813800',
                     'S0065500',
                     'S0065600',
                     'S0065700',
                     'Y2267000')


# Handle missing values

new_data[new_data == -1] = NA  # Refused
new_data[new_data == -2] = NA  # Dont know
new_data[new_data == -3] = NA  # Invalid missing
new_data[new_data == -7] = NA  # Missing


# If there are values not categorized they will be represented as NA

vallabels = function(data) {
  data$C0005300 <- factor(data$C0005300,
                          levels=c(1.0,2.0,3.0),
                          labels=c("HISPANIC",
                                   "BLACK",
                                   "NON-BLACK, NON-HISPANIC"))
  data$C0005400 <- factor(data$C0005400,
                          levels=c(1.0,2.0),
                          labels=c("MALE",
                                   "FEMALE"))
  data$C0007800 <- factor(data$C0007800,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
                          labels=c("IN HOUSEHOLD OF MOTHER",
                                   "WITH FATHER",
                                   "WITH OTHER RELATIVE(S)",
                                   "WITH FOSTER CARE",
                                   "WITH ADOPTIVE PARENT(S)",
                                   "IN LONG TERM CARE INSTITUTION",
                                   "AWAY AT SCHOOL",
                                   "DECEASED",
                                   "PART TIME W/ MOTHER, PART TIME W/ FATHER",
                                   "PART TIME W/ MOTHER, PART TIME W/ OTHER",
                                   "OTHER"))
  data$C0008000 <- factor(data$C0008000,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
                          labels=c("IN HOUSEHOLD OF MOTHER",
                                   "WITH FATHER",
                                   "WITH OTHER RELATIVE(S)",
                                   "WITH FOSTER CARE",
                                   "WITH ADOPTIVE PARENT(S)",
                                   "IN LONG TERM CARE INSTITUTION",
                                   "AWAY AT SCHOOL",
                                   "DECEASED",
                                   "PART TIME W/ MOTHER, PART TIME W/ FATHER",
                                   "PART TIME W/ MOTHER, PART TIME W/ OTHER",
                                   "OTHER"))
  data$C0008020 <- factor(data$C0008020,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
                          labels=c("IN HOUSEHOLD OF MOTHER",
                                   "WITH FATHER",
                                   "WITH OTHER RELATIVE(S)",
                                   "WITH FOSTER CARE",
                                   "WITH ADOPTIVE PARENT(S)",
                                   "IN LONG TERM CARE INSTITUTION",
                                   "AWAY AT SCHOOL",
                                   "DECEASED",
                                   "PART TIME W/ MOTHER, PART TIME W/ FATHER",
                                   "PART TIME W/ MOTHER, PART TIME W/ OTHER",
                                   "OTHER"))
  data$C0008040 <- factor(data$C0008040,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
                          labels=c("IN HOUSEHOLD OF MOTHER",
                                   "WITH FATHER",
                                   "WITH OTHER RELATIVE(S)",
                                   "WITH FOSTER CARE",
                                   "WITH ADOPTIVE PARENT(S)",
                                   "IN LONG TERM CARE INSTITUTION",
                                   "AWAY AT SCHOOL",
                                   "DECEASED",
                                   "PART TIME W/ MOTHER, PART TIME W/ FATHER",
                                   "PART TIME W/ MOTHER, PART TIME W/ OTHER",
                                   "OTHER"))
  data$C0008042 <- factor(data$C0008042,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
                          labels=c("IN HOUSEHOLD OF MOTHER",
                                   "WITH FATHER",
                                   "WITH OTHER RELATIVE(S)",
                                   "WITH FOSTER CARE",
                                   "WITH ADOPTIVE PARENT(S)",
                                   "IN LONG TERM CARE INSTITUTION",
                                   "AWAY AT SCHOOL",
                                   "DECEASED",
                                   "PART TIME W/ MOTHER, PART TIME W/ FATHER",
                                   "PART TIME W/ MOTHER, PART TIME W/ OTHER",
                                   "OTHER"))
  data$C0008043 <- factor(data$C0008043,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
                          labels=c("IN HOUSEHOLD OF MOTHER",
                                   "WITH FATHER",
                                   "WITH OTHER RELATIVE(S)",
                                   "WITH FOSTER CARE",
                                   "WITH ADOPTIVE PARENT(S)",
                                   "IN LONG TERM CARE INSTITUTION",
                                   "AWAY AT SCHOOL",
                                   "DECEASED",
                                   "PART TIME W/ MOTHER, PART TIME W/ FATHER",
                                   "PART TIME W/ MOTHER, PART TIME W/ OTHER",
                                   "OTHER"))
  data$C0008044 <- factor(data$C0008044,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0),
                          labels=c("IN HOUSEHOLD OF MOTHER",
                                   "WITH HIS/HER FATHER",
                                   "WITH OTHER RELATIVE(S)",
                                   "WITH FOSTER CARE",
                                   "WITH ADOPTIVE PARENT(S)",
                                   "IN LONG TERM CARE INSTITUTION",
                                   "AWAY AT SCHOOL",
                                   "DECEASED",
                                   "PART TIME W/MOTHER, PART TIME W/FATHER",
                                   "PART TIME W/MOTHER, PART TIME W/OTHER PERSON",
                                   "OTHER",
                                   "OWN INDEPENDENT RESIDENCE"))
  data$C0008045 <- factor(data$C0008045,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0),
                          labels=c("IN HOUSEHOLD OF MOTHER",
                                   "WITH FATHER",
                                   "WITH OTHER RELATIVE(S)",
                                   "WITH FOSTER CARE",
                                   "WITH ADOPTIVE PARENT(S)",
                                   "IN LONG TERM CARE INSTITUTION",
                                   "AWAY AT SCHOOL",
                                   "DECEASED",
                                   "PART TIME W/ MOTHER, PART TIME W/ FATHER",
                                   "PART TIME W/ MOTHER, PART TIME W/ OTHER",
                                   "OTHER",
                                   "OWN INDEPENDENT RESIDENCE"))
  data$C0008046 <- factor(data$C0008046,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0),
                          labels=c("IN HOUSEHOLD OF MOTHER",
                                   "WITH FATHER",
                                   "WITH OTHER RELATIVE(S)",
                                   "WITH FOSTER CARE",
                                   "WITH ADOPTIVE PARENT(S)",
                                   "IN LONG TERM CARE INSTITUTION",
                                   "AWAY AT SCHOOL",
                                   "DECEASED",
                                   "PART TIME W/ MOTHER, PART TIME W/ FATHER",
                                   "PART TIME W/ MOTHER, PART TIME W/ OTHER",
                                   "OTHER",
                                   "OWN INDEPENDENT RESIDENCE"))
  data$C0008047 <- factor(data$C0008047,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0),
                          labels=c("IN HOUSEHOLD OF MOTHER",
                                   "WITH FATHER",
                                   "WITH OTHER RELATIVE(S)",
                                   "WITH FOSTER CARE",
                                   "WITH ADOPTIVE PARENT(S)",
                                   "IN LONG TERM CARE INSTITUTION",
                                   "AWAY AT SCHOOL",
                                   "DECEASED",
                                   "PART TIME W/ MOTHER, PART TIME W/ FATHER",
                                   "PART TIME W/ MOTHER, PART TIME W/ OTHER",
                                   "OTHER",
                                   "OWN INDEPENDENT RESIDENCE"))
  data$C0008048 <- factor(data$C0008048,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0),
                          labels=c("IN HOUSEHOLD OF MOTHER",
                                   "WITH FATHER",
                                   "WITH OTHER RELATIVE(S)",
                                   "WITH FOSTER CARE",
                                   "WITH ADOPTIVE PARENT(S)",
                                   "IN LONG TERM CARE INSTITUTION",
                                   "AWAY AT SCHOOL",
                                   "DECEASED",
                                   "PART TIME W/ MOTHER, PART TIME W/ FATHER",
                                   "PART TIME W/ MOTHER, PART TIME W/ OTHER",
                                   "OTHER",
                                   "OWN INDEPENDENT RESIDENCE"))
  data$C0059901 <- factor(data$C0059901,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
                          labels=c("NONE",
                                   "1ST GRADE",
                                   "2ND GRADE",
                                   "3RD GRADE",
                                   "4TH GRADE",
                                   "5TH GRADE",
                                   "6TH GRADE",
                                   "7TH GRADE",
                                   "8TH GRADE",
                                   "9TH GRADE",
                                   "10TH GRADE",
                                   "11TH GRADE",
                                   "12TH GRADE",
                                   "1ST YR COL",
                                   "2ND YR COL",
                                   "3RD YR COL",
                                   "4TH YR COL",
                                   "5TH YR COL",
                                   "6TH YR COL",
                                   "7TH YR COL",
                                   "8TH YR COL OR MORE",
                                   "UNGRADED"))
  data$C0061134 <- factor(data$C0061134,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
                          labels=c("NONE",
                                   "1ST GRADE",
                                   "2ND GRADE",
                                   "3RD GRADE",
                                   "4TH GRADE",
                                   "5TH GRADE",
                                   "6TH GRADE",
                                   "7TH GRADE",
                                   "8TH GRADE",
                                   "9TH GRADE",
                                   "10TH GRADE",
                                   "11TH GRADE",
                                   "12TH GRADE",
                                   "1ST YR COL",
                                   "2ND YR COL",
                                   "3RD YR COL",
                                   "4TH YR COL",
                                   "5TH YR COL",
                                   "6TH YR COL",
                                   "7TH YR COL",
                                   "8TH YR COL OR MORE",
                                   "UNGRADED"))
  data$C0061136 <- factor(data$C0061136,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
                          labels=c("NONE",
                                   "1ST GRADE",
                                   "2ND GRADE",
                                   "3RD GRADE",
                                   "4TH GRADE",
                                   "5TH GRADE",
                                   "6TH GRADE",
                                   "7TH GRADE",
                                   "8TH GRADE",
                                   "9TH GRADE",
                                   "10TH GRADE",
                                   "11TH GRADE",
                                   "12TH GRADE",
                                   "1ST YR COL",
                                   "2ND YR COL",
                                   "3RD YR COL",
                                   "4TH YR COL",
                                   "5TH YR COL",
                                   "6TH YR COL",
                                   "7TH YR COL",
                                   "8TH YR COL OR MORE",
                                   "UNGRADED"))
  data$C0061138 <- factor(data$C0061138,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
                          labels=c("NONE",
                                   "1ST GRADE",
                                   "2ND GRADE",
                                   "3RD GRADE",
                                   "4TH GRADE",
                                   "5TH GRADE",
                                   "6TH GRADE",
                                   "7TH GRADE",
                                   "8TH GRADE",
                                   "9TH GRADE",
                                   "10TH GRADE",
                                   "11TH GRADE",
                                   "12TH GRADE",
                                   "1ST YR COL",
                                   "2ND YR COL",
                                   "3RD YR COL",
                                   "4TH YR COL",
                                   "5TH YR COL",
                                   "6TH YR COL",
                                   "7TH YR COL",
                                   "8TH YR COL OR MORE",
                                   "UNGRADED"))
  data$C0061140 <- factor(data$C0061140,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
                          labels=c("NONE",
                                   "1ST GRADE",
                                   "2ND GRADE",
                                   "3RD GRADE",
                                   "4TH GRADE",
                                   "5TH GRADE",
                                   "6TH GRADE",
                                   "7TH GRADE",
                                   "8TH GRADE",
                                   "9TH GRADE",
                                   "10TH GRADE",
                                   "11TH GRADE",
                                   "12TH GRADE",
                                   "1ST YR COL",
                                   "2ND YR COL",
                                   "3RD YR COL",
                                   "4TH YR COL",
                                   "5TH YR COL",
                                   "6TH YR COL",
                                   "7TH YR COL",
                                   "8TH YR COL OR MORE",
                                   "UNGRADED"))
  data$C0061141 <- factor(data$C0061141,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
                          labels=c("NONE",
                                   "1ST GRADE",
                                   "2ND GRADE",
                                   "3RD GRADE",
                                   "4TH GRADE",
                                   "5TH GRADE",
                                   "6TH GRADE",
                                   "7TH GRADE",
                                   "8TH GRADE",
                                   "9TH GRADE",
                                   "10TH GRADE",
                                   "11TH GRADE",
                                   "12TH GRADE",
                                   "1ST YR COL",
                                   "2ND YR COL",
                                   "3RD YR COL",
                                   "4TH YR COL",
                                   "5TH YR COL",
                                   "6TH YR COL",
                                   "7TH YR COL",
                                   "8TH YR COL OR MORE",
                                   "UNGRADED"))
  data$C0061142 <- factor(data$C0061142,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
                          labels=c("NONE",
                                   "1ST GRADE",
                                   "2ND GRADE",
                                   "3RD GRADE",
                                   "4TH GRADE",
                                   "5TH GRADE",
                                   "6TH GRADE",
                                   "7TH GRADE",
                                   "8TH GRADE",
                                   "9TH GRADE",
                                   "10TH GRADE",
                                   "11TH GRADE",
                                   "12TH GRADE",
                                   "1ST YR COL",
                                   "2ND YR COL",
                                   "3RD YR COL",
                                   "4TH YR COL",
                                   "5TH YR COL",
                                   "6TH YR COL",
                                   "7TH YR COL",
                                   "8TH YR COL OR MORE",
                                   "UNGRADED"))
  data$C0061143 <- factor(data$C0061143,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
                          labels=c("NONE",
                                   "1ST GRADE",
                                   "2ND GRADE",
                                   "3RD GRADE",
                                   "4TH GRADE",
                                   "5TH GRADE",
                                   "6TH GRADE",
                                   "7TH GRADE",
                                   "8TH GRADE",
                                   "9TH GRADE",
                                   "10TH GRADE",
                                   "11TH GRADE",
                                   "12TH GRADE",
                                   "1ST YR COL",
                                   "2ND YR COL",
                                   "3RD YR COL",
                                   "4TH YR COL",
                                   "5TH YR COL",
                                   "6TH YR COL",
                                   "7TH YR COL",
                                   "8TH YR COL OR MORE",
                                   "UNGRADED"))
  data$C0061144 <- factor(data$C0061144,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
                          labels=c("NONE",
                                   "1ST GRADE",
                                   "2ND GRADE",
                                   "3RD GRADE",
                                   "4TH GRADE",
                                   "5TH GRADE",
                                   "6TH GRADE",
                                   "7TH GRADE",
                                   "8TH GRADE",
                                   "9TH GRADE",
                                   "10TH GRADE",
                                   "11TH GRADE",
                                   "12TH GRADE",
                                   "1ST YR COL",
                                   "2ND YR COL",
                                   "3RD YR COL",
                                   "4TH YR COL",
                                   "5TH YR COL",
                                   "6TH YR COL",
                                   "7TH YR COL",
                                   "8TH YR COL OR MORE",
                                   "UNGRADED"))
  data$C0061145 <- factor(data$C0061145,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
                          labels=c("NONE",
                                   "1ST GRADE",
                                   "2ND GRADE",
                                   "3RD GRADE",
                                   "4TH GRADE",
                                   "5TH GRADE",
                                   "6TH GRADE",
                                   "7TH GRADE",
                                   "8TH GRADE",
                                   "9TH GRADE",
                                   "10TH GRADE",
                                   "11TH GRADE",
                                   "12TH GRADE",
                                   "1ST YR COL",
                                   "2ND YR COL",
                                   "3RD YR COL",
                                   "4TH YR COL",
                                   "5TH YR COL",
                                   "6TH YR COL",
                                   "7TH YR COL",
                                   "8TH YR COL OR MORE",
                                   "UNGRADED"))
  data$C0061146 <- factor(data$C0061146,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
                          labels=c("NONE",
                                   "1ST GRADE",
                                   "2ND GRADE",
                                   "3RD GRADE",
                                   "4TH GRADE",
                                   "5TH GRADE",
                                   "6TH GRADE",
                                   "7TH GRADE",
                                   "8TH GRADE",
                                   "9TH GRADE",
                                   "10TH GRADE",
                                   "11TH GRADE",
                                   "12TH GRADE",
                                   "1ST YR COL",
                                   "2ND YR COL",
                                   "3RD YR COL",
                                   "4TH YR COL",
                                   "5TH YR COL",
                                   "6TH YR COL",
                                   "7TH YR COL",
                                   "8TH YR COL OR MORE",
                                   "UNGRADED"))
  data$C0128000 <- factor(data$C0128000,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0),
                          labels=c("IN HOUSEHOLD OF MOTHER",
                                   "WITH FATHER",
                                   "WITH OTHER RELATIVE(S)",
                                   "WITH FOSTER CARE",
                                   "WITH ADOPTIVE PARENT(S)",
                                   "IN LONG TERM CARE INSTITUTION",
                                   "AWAY AT SCHOOL",
                                   "DECEASED",
                                   "PART TIME W/ MOTHER, PART TIME W/ FATHER",
                                   "PART TIME W/ MOTHER, PART TIME W/ OTHER",
                                   "OTHER",
                                   "OWN INDEPENDENT RESIDENCE"))
  data$C3601600 <- factor(data$C3601600,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0),
                          labels=c("IN HOUSEHOLD OF MOTHER",
                                   "WITH FATHER",
                                   "WITH OTHER RELATIVE(S)",
                                   "WITH FOSTER CARE",
                                   "WITH ADOPTIVE PARENT(S)",
                                   "IN LONG TERM CARE INSTITUTION",
                                   "AWAY AT SCHOOL",
                                   "DECEASED",
                                   "PART TIME W/ MOTHER, PART TIME W/ FATHER",
                                   "PART TIME W/ MOTHER, PART TIME W/ OTHER",
                                   "OTHER",
                                   "OWN INDEPENDENT RESIDENCE",
                                   "PRISON",
                                   "MILITARY",
                                   "TEMPORARY LIVING ARRANGEMENT",
                                   "WITH IN-LAWS",
                                   "WITH PARTNERS FAMILY"))
  data$C3602401 <- factor(data$C3602401,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
                          labels=c("NONE",
                                   "1ST GRADE",
                                   "2ND GRADE",
                                   "3RD GRADE",
                                   "4TH GRADE",
                                   "5TH GRADE",
                                   "6TH GRADE",
                                   "7TH GRADE",
                                   "8TH GRADE",
                                   "9TH GRADE",
                                   "10TH GRADE",
                                   "11TH GRADE",
                                   "12TH GRADE",
                                   "1ST YR COL",
                                   "2ND YR COL",
                                   "3RD YR COL",
                                   "4TH YR COL",
                                   "5TH YR COL",
                                   "6TH YR COL",
                                   "7TH YR COL",
                                   "8TH YR COL OR MORE",
                                   "UNGRADED"))
  data$C3981600 <- factor(data$C3981600,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0),
                          labels=c("IN HOUSEHOLD OF MOTHER",
                                   "WITH FATHER",
                                   "WITH OTHER RELATIVE(S)",
                                   "WITH FOSTER CARE",
                                   "WITH ADOPTIVE PARENT(S)",
                                   "IN LONG TERM CARE INSTITUTION",
                                   "AWAY AT SCHOOL",
                                   "DECEASED",
                                   "PART TIME W/ MOTHER, PART TIME W/ FATHER",
                                   "PART TIME W/ MOTHER, PART TIME W/ OTHER",
                                   "OTHER",
                                   "OWN INDEPENDENT RESIDENCE",
                                   "PRISON",
                                   "MILITARY",
                                   "TEMPORARY LIVING ARRANGEMENT",
                                   "WITH IN-LAWS",
                                   "WITH PARTNERS FAMILY"))
  data$C3982401 <- factor(data$C3982401,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
                          labels=c("NONE",
                                   "1ST GRADE",
                                   "2ND GRADE",
                                   "3RD GRADE",
                                   "4TH GRADE",
                                   "5TH GRADE",
                                   "6TH GRADE",
                                   "7TH GRADE",
                                   "8TH GRADE",
                                   "9TH GRADE",
                                   "10TH GRADE",
                                   "11TH GRADE",
                                   "12TH GRADE",
                                   "1ST YR COL",
                                   "2ND YR COL",
                                   "3RD YR COL",
                                   "4TH YR COL",
                                   "5TH YR COL",
                                   "6TH YR COL",
                                   "7TH YR COL",
                                   "8TH YR COL OR MORE",
                                   "UNGRADED"))
  data$C5525400 <- factor(data$C5525400,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0),
                          labels=c("IN HOUSEHOLD OF MOTHER",
                                   "WITH FATHER",
                                   "WITH OTHER RELATIVE(S)",
                                   "WITH FOSTER CARE",
                                   "WITH ADOPTIVE PARENT(S)",
                                   "IN LONG TERM CARE INSTITUTION",
                                   "AWAY AT SCHOOL",
                                   "DECEASED",
                                   "PART TIME W/ MOTHER, PART TIME W/ FATHER",
                                   "PART TIME W/ MOTHER, PART TIME W/ OTHER",
                                   "OTHER",
                                   "OWN INDEPENDENT RESIDENCE",
                                   "PRISON",
                                   "MILITARY",
                                   "TEMPORARY LIVING ARRANGEMENT",
                                   "WITH IN-LAWS",
                                   "WITH PARTNERS FAMILY"))
  data$C5526300 <- factor(data$C5526300,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
                          labels=c("NONE",
                                   "1ST GRADE",
                                   "2ND GRADE",
                                   "3RD GRADE",
                                   "4TH GRADE",
                                   "5TH GRADE",
                                   "6TH GRADE",
                                   "7TH GRADE",
                                   "8TH GRADE",
                                   "9TH GRADE",
                                   "10TH GRADE",
                                   "11TH GRADE",
                                   "12TH GRADE",
                                   "1ST YR COL",
                                   "2ND YR COL",
                                   "3RD YR COL",
                                   "4TH YR COL",
                                   "5TH YR COL",
                                   "6TH YR COL",
                                   "7TH YR COL",
                                   "8TH YR COL OR MORE",
                                   "UNGRADED"))
  data$C5801700 <- factor(data$C5801700,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0),
                          labels=c("IN HOUSEHOLD OF MOTHER",
                                   "WITH FATHER",
                                   "WITH OTHER RELATIVE(S)",
                                   "WITH FOSTER CARE",
                                   "WITH ADOPTIVE PARENT(S)",
                                   "IN LONG TERM CARE INSTITUTION",
                                   "AWAY AT SCHOOL",
                                   "DECEASED",
                                   "PART TIME W/ MOTHER, PART TIME W/ FATHER",
                                   "PART TIME W/ MOTHER, PART TIME W/ OTHER",
                                   "OTHER",
                                   "OWN INDEPENDENT RESIDENCE",
                                   "PRISON",
                                   "MILITARY",
                                   "TEMPORARY LIVING ARRANGEMENT",
                                   "WITH IN-LAWS",
                                   "WITH PARTNERS FAMILY",
                                   "HOTEL, MOTEL, ROOMING, OR BOARDING HOUSE",
                                   "SHELTER (FOR HOMELESS OR ABUSED) OR ON STREET"))
  data$C5802600 <- factor(data$C5802600,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
                          labels=c("NONE",
                                   "1ST GRADE",
                                   "2ND GRADE",
                                   "3RD GRADE",
                                   "4TH GRADE",
                                   "5TH GRADE",
                                   "6TH GRADE",
                                   "7TH GRADE",
                                   "8TH GRADE",
                                   "9TH GRADE",
                                   "10TH GRADE",
                                   "11TH GRADE",
                                   "12TH GRADE",
                                   "1ST YR COL",
                                   "2ND YR COL",
                                   "3RD YR COL",
                                   "4TH YR COL",
                                   "5TH YR COL",
                                   "6TH YR COL",
                                   "7TH YR COL",
                                   "8TH YR COL OR MORE",
                                   "UNGRADED"))
  data$Y2267000 <- factor(data$Y2267000,
                          levels=c(532.0),
                          labels=c("532"))
  return(data)
}


# If there are values not categorized they will be represented as NA

vallabels_continuous = function(data) {
  data$C0000100[1.0 <= data$C0000100 & data$C0000100 <= 9999999.0] <- 1.0
  data$C0000100 <- factor(data$C0000100,
                          levels=c(1.0),
                          labels=c("1 TO 9999999: See Min & Max values below for range as of this release"))
  data$C0000200[1.0 <= data$C0000200 & data$C0000200 <= 12686.0] <- 1.0
  data$C0000200 <- factor(data$C0000200,
                          levels=c(1.0),
                          labels=c("1 TO 12686: NLSY79 Public ID"))
  data$C0005700[1970.0 <= data$C0005700 & data$C0005700 <= 1978.0] <- 1970.0
  data$C0005700 <- factor(data$C0005700,
                          levels=c(1970.0,1979.0,1980.0,1981.0,1982.0,1983.0,1984.0,1985.0,1986.0,1987.0,1988.0,1989.0,1990.0,1991.0,1992.0,1993.0,1994.0,1995.0,1996.0,1997.0,1998.0,1999.0,2000.0,2001.0,2002.0,2003.0,2004.0,2005.0,2006.0,2007.0,2008.0,2009.0,2010.0,2011.0,2012.0,2013.0,2014.0,2015.0,2016.0,2017.0,2018.0,2019.0,2020.0,2021.0),
                          labels=c("1970 TO 1978: < before 1979",
                                   "1979",
                                   "1980",
                                   "1981",
                                   "1982",
                                   "1983",
                                   "1984",
                                   "1985",
                                   "1986",
                                   "1987",
                                   "1988",
                                   "1989",
                                   "1990",
                                   "1991",
                                   "1992",
                                   "1993",
                                   "1994",
                                   "1995",
                                   "1996",
                                   "1997",
                                   "1998",
                                   "1999",
                                   "2000",
                                   "2001",
                                   "2002",
                                   "2003",
                                   "2004",
                                   "2005",
                                   "2006",
                                   "2007",
                                   "2008",
                                   "2009",
                                   "2010",
                                   "2011",
                                   "2012",
                                   "2013",
                                   "2014",
                                   "2015",
                                   "2016",
                                   "2017",
                                   "2018",
                                   "2019",
                                   "2020",
                                   "2021"))
  data$C0005800[16.0 <= data$C0005800 & data$C0005800 <= 99999.0] <- 16.0
  data$C0005800 <- factor(data$C0005800,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16 TO 99999: 16+"))
  data$C0006500[0.0 <= data$C0006500 & data$C0006500 <= 11.0] <- 0.0
  data$C0006500[12.0 <= data$C0006500 & data$C0006500 <= 23.0] <- 12.0
  data$C0006500[24.0 <= data$C0006500 & data$C0006500 <= 35.0] <- 24.0
  data$C0006500[36.0 <= data$C0006500 & data$C0006500 <= 47.0] <- 36.0
  data$C0006500[48.0 <= data$C0006500 & data$C0006500 <= 59.0] <- 48.0
  data$C0006500[60.0 <= data$C0006500 & data$C0006500 <= 71.0] <- 60.0
  data$C0006500[72.0 <= data$C0006500 & data$C0006500 <= 83.0] <- 72.0
  data$C0006500[84.0 <= data$C0006500 & data$C0006500 <= 95.0] <- 84.0
  data$C0006500[96.0 <= data$C0006500 & data$C0006500 <= 107.0] <- 96.0
  data$C0006500[108.0 <= data$C0006500 & data$C0006500 <= 119.0] <- 108.0
  data$C0006500[120.0 <= data$C0006500 & data$C0006500 <= 131.0] <- 120.0
  data$C0006500[132.0 <= data$C0006500 & data$C0006500 <= 143.0] <- 132.0
  data$C0006500[144.0 <= data$C0006500 & data$C0006500 <= 155.0] <- 144.0
  data$C0006500[156.0 <= data$C0006500 & data$C0006500 <= 167.0] <- 156.0
  data$C0006500[168.0 <= data$C0006500 & data$C0006500 <= 179.0] <- 168.0
  data$C0006500[180.0 <= data$C0006500 & data$C0006500 <= 999.0] <- 180.0
  data$C0006500 <- factor(data$C0006500,
                          levels=c(0.0,12.0,24.0,36.0,48.0,60.0,72.0,84.0,96.0,108.0,120.0,132.0,144.0,156.0,168.0,180.0),
                          labels=c("0 TO 11: 0-11 MONTHS",
                                   "12 TO 23: 12-23 MONTHS",
                                   "24 TO 35: 24-35 MONTHS",
                                   "36 TO 47: 36-47 MONTHS",
                                   "48 TO 59: 48-59 MONTHS",
                                   "60 TO 71: 60-71 MONTHS",
                                   "72 TO 83: 72-83 MONTHS",
                                   "84 TO 95: 84-95 MONTHS",
                                   "96 TO 107: 96-107 MONTHS",
                                   "108 TO 119: 108-119 MONTHS",
                                   "120 TO 131: 120-131 MONTHS",
                                   "132 TO 143: 132-143 MONTHS",
                                   "144 TO 155: 144-155 MONTHS",
                                   "156 TO 167: 156-167 MONTHS",
                                   "168 TO 179: 168-179 MONTHS",
                                   "180 TO 999: 180 + MONTHS"))
  data$C0006800[0.0 <= data$C0006800 & data$C0006800 <= 11.0] <- 0.0
  data$C0006800[12.0 <= data$C0006800 & data$C0006800 <= 23.0] <- 12.0
  data$C0006800[24.0 <= data$C0006800 & data$C0006800 <= 35.0] <- 24.0
  data$C0006800[36.0 <= data$C0006800 & data$C0006800 <= 47.0] <- 36.0
  data$C0006800[48.0 <= data$C0006800 & data$C0006800 <= 59.0] <- 48.0
  data$C0006800[60.0 <= data$C0006800 & data$C0006800 <= 71.0] <- 60.0
  data$C0006800[72.0 <= data$C0006800 & data$C0006800 <= 83.0] <- 72.0
  data$C0006800[84.0 <= data$C0006800 & data$C0006800 <= 95.0] <- 84.0
  data$C0006800[96.0 <= data$C0006800 & data$C0006800 <= 107.0] <- 96.0
  data$C0006800[108.0 <= data$C0006800 & data$C0006800 <= 119.0] <- 108.0
  data$C0006800[120.0 <= data$C0006800 & data$C0006800 <= 131.0] <- 120.0
  data$C0006800[132.0 <= data$C0006800 & data$C0006800 <= 143.0] <- 132.0
  data$C0006800[144.0 <= data$C0006800 & data$C0006800 <= 155.0] <- 144.0
  data$C0006800[156.0 <= data$C0006800 & data$C0006800 <= 167.0] <- 156.0
  data$C0006800[168.0 <= data$C0006800 & data$C0006800 <= 179.0] <- 168.0
  data$C0006800[180.0 <= data$C0006800 & data$C0006800 <= 999.0] <- 180.0
  data$C0006800 <- factor(data$C0006800,
                          levels=c(0.0,12.0,24.0,36.0,48.0,60.0,72.0,84.0,96.0,108.0,120.0,132.0,144.0,156.0,168.0,180.0),
                          labels=c("0 TO 11: 0-11 MONTHS",
                                   "12 TO 23: 12-23 MONTHS",
                                   "24 TO 35: 24-35 MONTHS",
                                   "36 TO 47: 36-47 MONTHS",
                                   "48 TO 59: 48-59 MONTHS",
                                   "60 TO 71: 60-71 MONTHS",
                                   "72 TO 83: 72-83 MONTHS",
                                   "84 TO 95: 84-95 MONTHS",
                                   "96 TO 107: 96-107 MONTHS",
                                   "108 TO 119: 108-119 MONTHS",
                                   "120 TO 131: 120-131 MONTHS",
                                   "132 TO 143: 132-143 MONTHS",
                                   "144 TO 155: 144-155 MONTHS",
                                   "156 TO 167: 156-167 MONTHS",
                                   "168 TO 179: 168-179 MONTHS",
                                   "180 TO 999: 180 + MONTHS"))
  data$C0007000[1.0 <= data$C0007000 & data$C0007000 <= 4.0] <- 1.0
  data$C0007000[5.0 <= data$C0007000 & data$C0007000 <= 9.0] <- 5.0
  data$C0007000[10.0 <= data$C0007000 & data$C0007000 <= 14.0] <- 10.0
  data$C0007000[15.0 <= data$C0007000 & data$C0007000 <= 19.0] <- 15.0
  data$C0007000[20.0 <= data$C0007000 & data$C0007000 <= 24.0] <- 20.0
  data$C0007000[25.0 <= data$C0007000 & data$C0007000 <= 29.0] <- 25.0
  data$C0007000[30.0 <= data$C0007000 & data$C0007000 <= 34.0] <- 30.0
  data$C0007000[35.0 <= data$C0007000 & data$C0007000 <= 39.0] <- 35.0
  data$C0007000[40.0 <= data$C0007000 & data$C0007000 <= 44.0] <- 40.0
  data$C0007000[45.0 <= data$C0007000 & data$C0007000 <= 49.0] <- 45.0
  data$C0007000[50.0 <= data$C0007000 & data$C0007000 <= 52.0] <- 50.0
  data$C0007000[53.0 <= data$C0007000 & data$C0007000 <= 9999999.0] <- 53.0
  data$C0007000 <- factor(data$C0007000,
                          levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0,53.0),
                          labels=c("0",
                                   "1 TO 4",
                                   "5 TO 9",
                                   "10 TO 14",
                                   "15 TO 19",
                                   "20 TO 24",
                                   "25 TO 29",
                                   "30 TO 34",
                                   "35 TO 39",
                                   "40 TO 44",
                                   "45 TO 49",
                                   "50 TO 52",
                                   "53 TO 9999999: 53+"))
  data$C0007010[0.0 <= data$C0007010 & data$C0007010 <= 11.0] <- 0.0
  data$C0007010[12.0 <= data$C0007010 & data$C0007010 <= 23.0] <- 12.0
  data$C0007010[24.0 <= data$C0007010 & data$C0007010 <= 35.0] <- 24.0
  data$C0007010[36.0 <= data$C0007010 & data$C0007010 <= 47.0] <- 36.0
  data$C0007010[48.0 <= data$C0007010 & data$C0007010 <= 59.0] <- 48.0
  data$C0007010[60.0 <= data$C0007010 & data$C0007010 <= 71.0] <- 60.0
  data$C0007010[72.0 <= data$C0007010 & data$C0007010 <= 83.0] <- 72.0
  data$C0007010[84.0 <= data$C0007010 & data$C0007010 <= 95.0] <- 84.0
  data$C0007010[96.0 <= data$C0007010 & data$C0007010 <= 107.0] <- 96.0
  data$C0007010[108.0 <= data$C0007010 & data$C0007010 <= 119.0] <- 108.0
  data$C0007010[120.0 <= data$C0007010 & data$C0007010 <= 131.0] <- 120.0
  data$C0007010[132.0 <= data$C0007010 & data$C0007010 <= 143.0] <- 132.0
  data$C0007010[144.0 <= data$C0007010 & data$C0007010 <= 155.0] <- 144.0
  data$C0007010[156.0 <= data$C0007010 & data$C0007010 <= 167.0] <- 156.0
  data$C0007010[168.0 <= data$C0007010 & data$C0007010 <= 179.0] <- 168.0
  data$C0007010[180.0 <= data$C0007010 & data$C0007010 <= 999.0] <- 180.0
  data$C0007010 <- factor(data$C0007010,
                          levels=c(0.0,12.0,24.0,36.0,48.0,60.0,72.0,84.0,96.0,108.0,120.0,132.0,144.0,156.0,168.0,180.0),
                          labels=c("0 TO 11: 0-11 MONTHS",
                                   "12 TO 23: 12-23 MONTHS",
                                   "24 TO 35: 24-35 MONTHS",
                                   "36 TO 47: 36-47 MONTHS",
                                   "48 TO 59: 48-59 MONTHS",
                                   "60 TO 71: 60-71 MONTHS",
                                   "72 TO 83: 72-83 MONTHS",
                                   "84 TO 95: 84-95 MONTHS",
                                   "96 TO 107: 96-107 MONTHS",
                                   "108 TO 119: 108-119 MONTHS",
                                   "120 TO 131: 120-131 MONTHS",
                                   "132 TO 143: 132-143 MONTHS",
                                   "144 TO 155: 144-155 MONTHS",
                                   "156 TO 167: 156-167 MONTHS",
                                   "168 TO 179: 168-179 MONTHS",
                                   "180 TO 999: 180 + MONTHS"))
  data$C0007030[0.0 <= data$C0007030 & data$C0007030 <= 11.0] <- 0.0
  data$C0007030[12.0 <= data$C0007030 & data$C0007030 <= 23.0] <- 12.0
  data$C0007030[24.0 <= data$C0007030 & data$C0007030 <= 35.0] <- 24.0
  data$C0007030[36.0 <= data$C0007030 & data$C0007030 <= 47.0] <- 36.0
  data$C0007030[48.0 <= data$C0007030 & data$C0007030 <= 59.0] <- 48.0
  data$C0007030[60.0 <= data$C0007030 & data$C0007030 <= 71.0] <- 60.0
  data$C0007030[72.0 <= data$C0007030 & data$C0007030 <= 83.0] <- 72.0
  data$C0007030[84.0 <= data$C0007030 & data$C0007030 <= 95.0] <- 84.0
  data$C0007030[96.0 <= data$C0007030 & data$C0007030 <= 107.0] <- 96.0
  data$C0007030[108.0 <= data$C0007030 & data$C0007030 <= 119.0] <- 108.0
  data$C0007030[120.0 <= data$C0007030 & data$C0007030 <= 131.0] <- 120.0
  data$C0007030[132.0 <= data$C0007030 & data$C0007030 <= 143.0] <- 132.0
  data$C0007030[144.0 <= data$C0007030 & data$C0007030 <= 155.0] <- 144.0
  data$C0007030[156.0 <= data$C0007030 & data$C0007030 <= 167.0] <- 156.0
  data$C0007030[168.0 <= data$C0007030 & data$C0007030 <= 179.0] <- 168.0
  data$C0007030[180.0 <= data$C0007030 & data$C0007030 <= 999.0] <- 180.0
  data$C0007030 <- factor(data$C0007030,
                          levels=c(0.0,12.0,24.0,36.0,48.0,60.0,72.0,84.0,96.0,108.0,120.0,132.0,144.0,156.0,168.0,180.0),
                          labels=c("0 TO 11: 0-11 MONTHS",
                                   "12 TO 23: 12-23 MONTHS",
                                   "24 TO 35: 24-35 MONTHS",
                                   "36 TO 47: 36-47 MONTHS",
                                   "48 TO 59: 48-59 MONTHS",
                                   "60 TO 71: 60-71 MONTHS",
                                   "72 TO 83: 72-83 MONTHS",
                                   "84 TO 95: 84-95 MONTHS",
                                   "96 TO 107: 96-107 MONTHS",
                                   "108 TO 119: 108-119 MONTHS",
                                   "120 TO 131: 120-131 MONTHS",
                                   "132 TO 143: 132-143 MONTHS",
                                   "144 TO 155: 144-155 MONTHS",
                                   "156 TO 167: 156-167 MONTHS",
                                   "168 TO 179: 168-179 MONTHS",
                                   "180 TO 999: 180 + MONTHS"))
  data$C0007041[0.0 <= data$C0007041 & data$C0007041 <= 11.0] <- 0.0
  data$C0007041[12.0 <= data$C0007041 & data$C0007041 <= 23.0] <- 12.0
  data$C0007041[24.0 <= data$C0007041 & data$C0007041 <= 35.0] <- 24.0
  data$C0007041[36.0 <= data$C0007041 & data$C0007041 <= 47.0] <- 36.0
  data$C0007041[48.0 <= data$C0007041 & data$C0007041 <= 59.0] <- 48.0
  data$C0007041[60.0 <= data$C0007041 & data$C0007041 <= 71.0] <- 60.0
  data$C0007041[72.0 <= data$C0007041 & data$C0007041 <= 83.0] <- 72.0
  data$C0007041[84.0 <= data$C0007041 & data$C0007041 <= 95.0] <- 84.0
  data$C0007041[96.0 <= data$C0007041 & data$C0007041 <= 107.0] <- 96.0
  data$C0007041[108.0 <= data$C0007041 & data$C0007041 <= 119.0] <- 108.0
  data$C0007041[120.0 <= data$C0007041 & data$C0007041 <= 131.0] <- 120.0
  data$C0007041[132.0 <= data$C0007041 & data$C0007041 <= 143.0] <- 132.0
  data$C0007041[144.0 <= data$C0007041 & data$C0007041 <= 155.0] <- 144.0
  data$C0007041[156.0 <= data$C0007041 & data$C0007041 <= 167.0] <- 156.0
  data$C0007041[168.0 <= data$C0007041 & data$C0007041 <= 179.0] <- 168.0
  data$C0007041[180.0 <= data$C0007041 & data$C0007041 <= 999.0] <- 180.0
  data$C0007041 <- factor(data$C0007041,
                          levels=c(0.0,12.0,24.0,36.0,48.0,60.0,72.0,84.0,96.0,108.0,120.0,132.0,144.0,156.0,168.0,180.0),
                          labels=c("0 TO 11: 0-11 MONTHS",
                                   "12 TO 23: 12-23 MONTHS",
                                   "24 TO 35: 24-35 MONTHS",
                                   "36 TO 47: 36-47 MONTHS",
                                   "48 TO 59: 48-59 MONTHS",
                                   "60 TO 71: 60-71 MONTHS",
                                   "72 TO 83: 72-83 MONTHS",
                                   "84 TO 95: 84-95 MONTHS",
                                   "96 TO 107: 96-107 MONTHS",
                                   "108 TO 119: 108-119 MONTHS",
                                   "120 TO 131: 120-131 MONTHS",
                                   "132 TO 143: 132-143 MONTHS",
                                   "144 TO 155: 144-155 MONTHS",
                                   "156 TO 167: 156-167 MONTHS",
                                   "168 TO 179: 168-179 MONTHS",
                                   "180 TO 999: 180 + MONTHS"))
  data$C0007043[0.0 <= data$C0007043 & data$C0007043 <= 11.0] <- 0.0
  data$C0007043[12.0 <= data$C0007043 & data$C0007043 <= 23.0] <- 12.0
  data$C0007043[24.0 <= data$C0007043 & data$C0007043 <= 35.0] <- 24.0
  data$C0007043[36.0 <= data$C0007043 & data$C0007043 <= 47.0] <- 36.0
  data$C0007043[48.0 <= data$C0007043 & data$C0007043 <= 59.0] <- 48.0
  data$C0007043[60.0 <= data$C0007043 & data$C0007043 <= 71.0] <- 60.0
  data$C0007043[72.0 <= data$C0007043 & data$C0007043 <= 83.0] <- 72.0
  data$C0007043[84.0 <= data$C0007043 & data$C0007043 <= 95.0] <- 84.0
  data$C0007043[96.0 <= data$C0007043 & data$C0007043 <= 107.0] <- 96.0
  data$C0007043[108.0 <= data$C0007043 & data$C0007043 <= 119.0] <- 108.0
  data$C0007043[120.0 <= data$C0007043 & data$C0007043 <= 131.0] <- 120.0
  data$C0007043[132.0 <= data$C0007043 & data$C0007043 <= 143.0] <- 132.0
  data$C0007043[144.0 <= data$C0007043 & data$C0007043 <= 155.0] <- 144.0
  data$C0007043[156.0 <= data$C0007043 & data$C0007043 <= 167.0] <- 156.0
  data$C0007043[168.0 <= data$C0007043 & data$C0007043 <= 179.0] <- 168.0
  data$C0007043[180.0 <= data$C0007043 & data$C0007043 <= 999.0] <- 180.0
  data$C0007043 <- factor(data$C0007043,
                          levels=c(0.0,12.0,24.0,36.0,48.0,60.0,72.0,84.0,96.0,108.0,120.0,132.0,144.0,156.0,168.0,180.0),
                          labels=c("0 TO 11: 0-11 MONTHS",
                                   "12 TO 23: 12-23 MONTHS",
                                   "24 TO 35: 24-35 MONTHS",
                                   "36 TO 47: 36-47 MONTHS",
                                   "48 TO 59: 48-59 MONTHS",
                                   "60 TO 71: 60-71 MONTHS",
                                   "72 TO 83: 72-83 MONTHS",
                                   "84 TO 95: 84-95 MONTHS",
                                   "96 TO 107: 96-107 MONTHS",
                                   "108 TO 119: 108-119 MONTHS",
                                   "120 TO 131: 120-131 MONTHS",
                                   "132 TO 143: 132-143 MONTHS",
                                   "144 TO 155: 144-155 MONTHS",
                                   "156 TO 167: 156-167 MONTHS",
                                   "168 TO 179: 168-179 MONTHS",
                                   "180 TO 999: 180+ MONTHS"))
  data$C0007045[0.0 <= data$C0007045 & data$C0007045 <= 11.0] <- 0.0
  data$C0007045[12.0 <= data$C0007045 & data$C0007045 <= 23.0] <- 12.0
  data$C0007045[24.0 <= data$C0007045 & data$C0007045 <= 35.0] <- 24.0
  data$C0007045[36.0 <= data$C0007045 & data$C0007045 <= 47.0] <- 36.0
  data$C0007045[48.0 <= data$C0007045 & data$C0007045 <= 59.0] <- 48.0
  data$C0007045[60.0 <= data$C0007045 & data$C0007045 <= 71.0] <- 60.0
  data$C0007045[72.0 <= data$C0007045 & data$C0007045 <= 83.0] <- 72.0
  data$C0007045[84.0 <= data$C0007045 & data$C0007045 <= 95.0] <- 84.0
  data$C0007045[96.0 <= data$C0007045 & data$C0007045 <= 107.0] <- 96.0
  data$C0007045[108.0 <= data$C0007045 & data$C0007045 <= 119.0] <- 108.0
  data$C0007045[120.0 <= data$C0007045 & data$C0007045 <= 131.0] <- 120.0
  data$C0007045[132.0 <= data$C0007045 & data$C0007045 <= 143.0] <- 132.0
  data$C0007045[144.0 <= data$C0007045 & data$C0007045 <= 155.0] <- 144.0
  data$C0007045[156.0 <= data$C0007045 & data$C0007045 <= 167.0] <- 156.0
  data$C0007045[168.0 <= data$C0007045 & data$C0007045 <= 179.0] <- 168.0
  data$C0007045[180.0 <= data$C0007045 & data$C0007045 <= 999.0] <- 180.0
  data$C0007045 <- factor(data$C0007045,
                          levels=c(0.0,12.0,24.0,36.0,48.0,60.0,72.0,84.0,96.0,108.0,120.0,132.0,144.0,156.0,168.0,180.0),
                          labels=c("0 TO 11: 0-11 MONTHS",
                                   "12 TO 23: 12-23 MONTHS",
                                   "24 TO 35: 24-35 MONTHS",
                                   "36 TO 47: 36-47 MONTHS",
                                   "48 TO 59: 48-59 MONTHS",
                                   "60 TO 71: 60-71 MONTHS",
                                   "72 TO 83: 72-83 MONTHS",
                                   "84 TO 95: 84-95 MONTHS",
                                   "96 TO 107: 96-107 MONTHS",
                                   "108 TO 119: 108-119 MONTHS",
                                   "120 TO 131: 120-131 MONTHS",
                                   "132 TO 143: 132-143 MONTHS",
                                   "144 TO 155: 144-155 MONTHS",
                                   "156 TO 167: 156-167 MONTHS",
                                   "168 TO 179: 168-179 MONTHS",
                                   "180 TO 999: 180 + MONTHS"))
  data$C0007047[0.0 <= data$C0007047 & data$C0007047 <= 11.0] <- 0.0
  data$C0007047[12.0 <= data$C0007047 & data$C0007047 <= 23.0] <- 12.0
  data$C0007047[24.0 <= data$C0007047 & data$C0007047 <= 35.0] <- 24.0
  data$C0007047[36.0 <= data$C0007047 & data$C0007047 <= 47.0] <- 36.0
  data$C0007047[48.0 <= data$C0007047 & data$C0007047 <= 59.0] <- 48.0
  data$C0007047[60.0 <= data$C0007047 & data$C0007047 <= 71.0] <- 60.0
  data$C0007047[72.0 <= data$C0007047 & data$C0007047 <= 83.0] <- 72.0
  data$C0007047[84.0 <= data$C0007047 & data$C0007047 <= 95.0] <- 84.0
  data$C0007047[96.0 <= data$C0007047 & data$C0007047 <= 107.0] <- 96.0
  data$C0007047[108.0 <= data$C0007047 & data$C0007047 <= 119.0] <- 108.0
  data$C0007047[120.0 <= data$C0007047 & data$C0007047 <= 131.0] <- 120.0
  data$C0007047[132.0 <= data$C0007047 & data$C0007047 <= 143.0] <- 132.0
  data$C0007047[144.0 <= data$C0007047 & data$C0007047 <= 155.0] <- 144.0
  data$C0007047[156.0 <= data$C0007047 & data$C0007047 <= 167.0] <- 156.0
  data$C0007047[168.0 <= data$C0007047 & data$C0007047 <= 179.0] <- 168.0
  data$C0007047[180.0 <= data$C0007047 & data$C0007047 <= 999.0] <- 180.0
  data$C0007047 <- factor(data$C0007047,
                          levels=c(0.0,12.0,24.0,36.0,48.0,60.0,72.0,84.0,96.0,108.0,120.0,132.0,144.0,156.0,168.0,180.0),
                          labels=c("0 TO 11: 0-11 MONTHS",
                                   "12 TO 23: 12-23 MONTHS",
                                   "24 TO 35: 24-35 MONTHS",
                                   "36 TO 47: 36-47 MONTHS",
                                   "48 TO 59: 48-59 MONTHS",
                                   "60 TO 71: 60-71 MONTHS",
                                   "72 TO 83: 72-83 MONTHS",
                                   "84 TO 95: 84-95 MONTHS",
                                   "96 TO 107: 96-107 MONTHS",
                                   "108 TO 119: 108-119 MONTHS",
                                   "120 TO 131: 120-131 MONTHS",
                                   "132 TO 143: 132-143 MONTHS",
                                   "144 TO 155: 144-155 MONTHS",
                                   "156 TO 167: 156-167 MONTHS",
                                   "168 TO 179: 168-179 MONTHS",
                                   "180 TO 999: 180 + MONTHS"))
  data$C0007049[0.0 <= data$C0007049 & data$C0007049 <= 11.0] <- 0.0
  data$C0007049[12.0 <= data$C0007049 & data$C0007049 <= 23.0] <- 12.0
  data$C0007049[24.0 <= data$C0007049 & data$C0007049 <= 35.0] <- 24.0
  data$C0007049[36.0 <= data$C0007049 & data$C0007049 <= 47.0] <- 36.0
  data$C0007049[48.0 <= data$C0007049 & data$C0007049 <= 59.0] <- 48.0
  data$C0007049[60.0 <= data$C0007049 & data$C0007049 <= 71.0] <- 60.0
  data$C0007049[72.0 <= data$C0007049 & data$C0007049 <= 83.0] <- 72.0
  data$C0007049[84.0 <= data$C0007049 & data$C0007049 <= 95.0] <- 84.0
  data$C0007049[96.0 <= data$C0007049 & data$C0007049 <= 107.0] <- 96.0
  data$C0007049[108.0 <= data$C0007049 & data$C0007049 <= 119.0] <- 108.0
  data$C0007049[120.0 <= data$C0007049 & data$C0007049 <= 131.0] <- 120.0
  data$C0007049[132.0 <= data$C0007049 & data$C0007049 <= 143.0] <- 132.0
  data$C0007049[144.0 <= data$C0007049 & data$C0007049 <= 155.0] <- 144.0
  data$C0007049[156.0 <= data$C0007049 & data$C0007049 <= 167.0] <- 156.0
  data$C0007049[168.0 <= data$C0007049 & data$C0007049 <= 179.0] <- 168.0
  data$C0007049[180.0 <= data$C0007049 & data$C0007049 <= 999.0] <- 180.0
  data$C0007049 <- factor(data$C0007049,
                          levels=c(0.0,12.0,24.0,36.0,48.0,60.0,72.0,84.0,96.0,108.0,120.0,132.0,144.0,156.0,168.0,180.0),
                          labels=c("0 TO 11: MONTHS",
                                   "12 TO 23: MONTHS",
                                   "24 TO 35: MONTHS",
                                   "36 TO 47: MONTHS",
                                   "48 TO 59: MONTHS",
                                   "60 TO 71: MONTHS",
                                   "72 TO 83: MONTHS",
                                   "84 TO 95: MONTHS",
                                   "96 TO 107: MONTHS",
                                   "108 TO 119: MONTHS",
                                   "120 TO 131: MONTHS",
                                   "132 TO 143: MONTHS",
                                   "144 TO 155: MONTHS",
                                   "156 TO 167: MONTHS",
                                   "168 TO 179: MONTHS",
                                   "180 TO 999: MONTHS"))
  data$C0007052[0.0 <= data$C0007052 & data$C0007052 <= 11.0] <- 0.0
  data$C0007052[12.0 <= data$C0007052 & data$C0007052 <= 23.0] <- 12.0
  data$C0007052[24.0 <= data$C0007052 & data$C0007052 <= 35.0] <- 24.0
  data$C0007052[36.0 <= data$C0007052 & data$C0007052 <= 47.0] <- 36.0
  data$C0007052[48.0 <= data$C0007052 & data$C0007052 <= 59.0] <- 48.0
  data$C0007052[60.0 <= data$C0007052 & data$C0007052 <= 71.0] <- 60.0
  data$C0007052[72.0 <= data$C0007052 & data$C0007052 <= 83.0] <- 72.0
  data$C0007052[84.0 <= data$C0007052 & data$C0007052 <= 95.0] <- 84.0
  data$C0007052[96.0 <= data$C0007052 & data$C0007052 <= 107.0] <- 96.0
  data$C0007052[108.0 <= data$C0007052 & data$C0007052 <= 119.0] <- 108.0
  data$C0007052[120.0 <= data$C0007052 & data$C0007052 <= 131.0] <- 120.0
  data$C0007052[132.0 <= data$C0007052 & data$C0007052 <= 143.0] <- 132.0
  data$C0007052[144.0 <= data$C0007052 & data$C0007052 <= 155.0] <- 144.0
  data$C0007052[156.0 <= data$C0007052 & data$C0007052 <= 167.0] <- 156.0
  data$C0007052[168.0 <= data$C0007052 & data$C0007052 <= 179.0] <- 168.0
  data$C0007052[180.0 <= data$C0007052 & data$C0007052 <= 999.0] <- 180.0
  data$C0007052 <- factor(data$C0007052,
                          levels=c(0.0,12.0,24.0,36.0,48.0,60.0,72.0,84.0,96.0,108.0,120.0,132.0,144.0,156.0,168.0,180.0),
                          labels=c("0 TO 11: 0-11 MONTHS",
                                   "12 TO 23: 12-23 MONTHS",
                                   "24 TO 35: 24-35 MONTHS",
                                   "36 TO 47: 36-47 MONTHS",
                                   "48 TO 59: 48-59 MONTHS",
                                   "60 TO 71: 60-71 MONTHS",
                                   "72 TO 83: 72-83 MONTHS",
                                   "84 TO 95: 84-95 MONTHS",
                                   "96 TO 107: 96-107 MONTHS",
                                   "108 TO 119: 108-119 MONTHS",
                                   "120 TO 131: 120-131 MONTHS",
                                   "132 TO 143: 132-143 MONTHS",
                                   "144 TO 155: 144-155 MONTHS",
                                   "156 TO 167: 156-167 MONTHS",
                                   "168 TO 179: 168-179 MONTHS",
                                   "180 TO 999: 180 + MONTHS"))
  data$C0007055[0.0 <= data$C0007055 & data$C0007055 <= 11.0] <- 0.0
  data$C0007055[12.0 <= data$C0007055 & data$C0007055 <= 23.0] <- 12.0
  data$C0007055[24.0 <= data$C0007055 & data$C0007055 <= 35.0] <- 24.0
  data$C0007055[36.0 <= data$C0007055 & data$C0007055 <= 47.0] <- 36.0
  data$C0007055[48.0 <= data$C0007055 & data$C0007055 <= 59.0] <- 48.0
  data$C0007055[60.0 <= data$C0007055 & data$C0007055 <= 71.0] <- 60.0
  data$C0007055[72.0 <= data$C0007055 & data$C0007055 <= 83.0] <- 72.0
  data$C0007055[84.0 <= data$C0007055 & data$C0007055 <= 95.0] <- 84.0
  data$C0007055[96.0 <= data$C0007055 & data$C0007055 <= 107.0] <- 96.0
  data$C0007055[108.0 <= data$C0007055 & data$C0007055 <= 119.0] <- 108.0
  data$C0007055[120.0 <= data$C0007055 & data$C0007055 <= 131.0] <- 120.0
  data$C0007055[132.0 <= data$C0007055 & data$C0007055 <= 143.0] <- 132.0
  data$C0007055[144.0 <= data$C0007055 & data$C0007055 <= 155.0] <- 144.0
  data$C0007055[156.0 <= data$C0007055 & data$C0007055 <= 167.0] <- 156.0
  data$C0007055[168.0 <= data$C0007055 & data$C0007055 <= 179.0] <- 168.0
  data$C0007055[180.0 <= data$C0007055 & data$C0007055 <= 999.0] <- 180.0
  data$C0007055 <- factor(data$C0007055,
                          levels=c(0.0,12.0,24.0,36.0,48.0,60.0,72.0,84.0,96.0,108.0,120.0,132.0,144.0,156.0,168.0,180.0),
                          labels=c("0 TO 11: 0-11 MONTHS",
                                   "12 TO 23: 12-23 MONTHS",
                                   "24 TO 35: 24-35 MONTHS",
                                   "36 TO 47: 36-47 MONTHS",
                                   "48 TO 59: 48-59 MONTHS",
                                   "60 TO 71: 60-71 MONTHS",
                                   "72 TO 83: 72-83 MONTHS",
                                   "84 TO 95: 84-95 MONTHS",
                                   "96 TO 107: 96-107 MONTHS",
                                   "108 TO 119: 108-119 MONTHS",
                                   "120 TO 131: 120-131 MONTHS",
                                   "132 TO 143: 132-143 MONTHS",
                                   "144 TO 155: 144-155 MONTHS",
                                   "156 TO 167: 156-167 MONTHS",
                                   "168 TO 179: 168-179 MONTHS",
                                   "180 TO 999: 180 + MONTHS"))
  data$C0571600[1.0 <= data$C0571600 & data$C0571600 <= 99.0] <- 1.0
  data$C0571600[100.0 <= data$C0571600 & data$C0571600 <= 199.0] <- 100.0
  data$C0571600[200.0 <= data$C0571600 & data$C0571600 <= 299.0] <- 200.0
  data$C0571600[300.0 <= data$C0571600 & data$C0571600 <= 399.0] <- 300.0
  data$C0571600[400.0 <= data$C0571600 & data$C0571600 <= 499.0] <- 400.0
  data$C0571600[500.0 <= data$C0571600 & data$C0571600 <= 599.0] <- 500.0
  data$C0571600[600.0 <= data$C0571600 & data$C0571600 <= 699.0] <- 600.0
  data$C0571600[700.0 <= data$C0571600 & data$C0571600 <= 799.0] <- 700.0
  data$C0571600[800.0 <= data$C0571600 & data$C0571600 <= 899.0] <- 800.0
  data$C0571600[900.0 <= data$C0571600 & data$C0571600 <= 999.0] <- 900.0
  data$C0571600[1000.0 <= data$C0571600 & data$C0571600 <= 1099.0] <- 1000.0
  data$C0571600[1100.0 <= data$C0571600 & data$C0571600 <= 1199.0] <- 1100.0
  data$C0571600[1200.0 <= data$C0571600 & data$C0571600 <= 1299.0] <- 1200.0
  data$C0571600[1300.0 <= data$C0571600 & data$C0571600 <= 1399.0] <- 1300.0
  data$C0571600[1400.0 <= data$C0571600 & data$C0571600 <= 1499.0] <- 1400.0
  data$C0571600[1500.0 <= data$C0571600 & data$C0571600 <= 9999999.0] <- 1500.0
  data$C0571600 <- factor(data$C0571600,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0,1100.0,1200.0,1300.0,1400.0,1500.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 1099",
                                   "1100 TO 1199",
                                   "1200 TO 1299",
                                   "1300 TO 1399",
                                   "1400 TO 1499",
                                   "1500 TO 9999999: 1500+"))
  data$C0580100[0.0 <= data$C0580100 & data$C0580100 <= 64.0] <- 0.0
  data$C0580100[65.0 <= data$C0580100 & data$C0580100 <= 69.0] <- 65.0
  data$C0580100[70.0 <= data$C0580100 & data$C0580100 <= 84.0] <- 70.0
  data$C0580100[85.0 <= data$C0580100 & data$C0580100 <= 99.0] <- 85.0
  data$C0580100[100.0 <= data$C0580100 & data$C0580100 <= 114.0] <- 100.0
  data$C0580100[115.0 <= data$C0580100 & data$C0580100 <= 129.0] <- 115.0
  data$C0580100[130.0 <= data$C0580100 & data$C0580100 <= 144.0] <- 130.0
  data$C0580100[145.0 <= data$C0580100 & data$C0580100 <= 9999.0] <- 145.0
  data$C0580100 <- factor(data$C0580100,
                          levels=c(0.0,65.0,70.0,85.0,100.0,115.0,130.0,145.0),
                          labels=c("0 TO 64",
                                   "65 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 144",
                                   "145 TO 9999: 145 +"))
  data$C0580400[0.0 <= data$C0580400 & data$C0580400 <= 64.0] <- 0.0
  data$C0580400[65.0 <= data$C0580400 & data$C0580400 <= 69.0] <- 65.0
  data$C0580400[70.0 <= data$C0580400 & data$C0580400 <= 84.0] <- 70.0
  data$C0580400[85.0 <= data$C0580400 & data$C0580400 <= 99.0] <- 85.0
  data$C0580400[100.0 <= data$C0580400 & data$C0580400 <= 114.0] <- 100.0
  data$C0580400[115.0 <= data$C0580400 & data$C0580400 <= 129.0] <- 115.0
  data$C0580400[130.0 <= data$C0580400 & data$C0580400 <= 144.0] <- 130.0
  data$C0580400[145.0 <= data$C0580400 & data$C0580400 <= 9999.0] <- 145.0
  data$C0580400 <- factor(data$C0580400,
                          levels=c(0.0,65.0,70.0,85.0,100.0,115.0,130.0,145.0),
                          labels=c("0 TO 64",
                                   "65 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 144",
                                   "145 TO 9999: 145 +"))
  data$C0792000[1.0 <= data$C0792000 & data$C0792000 <= 99.0] <- 1.0
  data$C0792000[100.0 <= data$C0792000 & data$C0792000 <= 199.0] <- 100.0
  data$C0792000[200.0 <= data$C0792000 & data$C0792000 <= 299.0] <- 200.0
  data$C0792000[300.0 <= data$C0792000 & data$C0792000 <= 399.0] <- 300.0
  data$C0792000[400.0 <= data$C0792000 & data$C0792000 <= 499.0] <- 400.0
  data$C0792000[500.0 <= data$C0792000 & data$C0792000 <= 599.0] <- 500.0
  data$C0792000[600.0 <= data$C0792000 & data$C0792000 <= 699.0] <- 600.0
  data$C0792000[700.0 <= data$C0792000 & data$C0792000 <= 799.0] <- 700.0
  data$C0792000[800.0 <= data$C0792000 & data$C0792000 <= 899.0] <- 800.0
  data$C0792000[900.0 <= data$C0792000 & data$C0792000 <= 999.0] <- 900.0
  data$C0792000[1000.0 <= data$C0792000 & data$C0792000 <= 1099.0] <- 1000.0
  data$C0792000[1100.0 <= data$C0792000 & data$C0792000 <= 1199.0] <- 1100.0
  data$C0792000[1200.0 <= data$C0792000 & data$C0792000 <= 1299.0] <- 1200.0
  data$C0792000[1300.0 <= data$C0792000 & data$C0792000 <= 1399.0] <- 1300.0
  data$C0792000[1400.0 <= data$C0792000 & data$C0792000 <= 1499.0] <- 1400.0
  data$C0792000[1500.0 <= data$C0792000 & data$C0792000 <= 9999999.0] <- 1500.0
  data$C0792000 <- factor(data$C0792000,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0,1100.0,1200.0,1300.0,1400.0,1500.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 1099",
                                   "1100 TO 1199",
                                   "1200 TO 1299",
                                   "1300 TO 1399",
                                   "1400 TO 1499",
                                   "1500 TO 9999999: 1500+"))
  data$C0799600[0.0 <= data$C0799600 & data$C0799600 <= 64.0] <- 0.0
  data$C0799600[65.0 <= data$C0799600 & data$C0799600 <= 69.0] <- 65.0
  data$C0799600[70.0 <= data$C0799600 & data$C0799600 <= 84.0] <- 70.0
  data$C0799600[85.0 <= data$C0799600 & data$C0799600 <= 99.0] <- 85.0
  data$C0799600[100.0 <= data$C0799600 & data$C0799600 <= 114.0] <- 100.0
  data$C0799600[115.0 <= data$C0799600 & data$C0799600 <= 129.0] <- 115.0
  data$C0799600[130.0 <= data$C0799600 & data$C0799600 <= 144.0] <- 130.0
  data$C0799600[145.0 <= data$C0799600 & data$C0799600 <= 9999.0] <- 145.0
  data$C0799600 <- factor(data$C0799600,
                          levels=c(0.0,65.0,70.0,85.0,100.0,115.0,130.0,145.0),
                          labels=c("0 TO 64",
                                   "65 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 144",
                                   "145 TO 9999: 145 +"))
  data$C0799900[0.0 <= data$C0799900 & data$C0799900 <= 64.0] <- 0.0
  data$C0799900[65.0 <= data$C0799900 & data$C0799900 <= 69.0] <- 65.0
  data$C0799900[70.0 <= data$C0799900 & data$C0799900 <= 84.0] <- 70.0
  data$C0799900[85.0 <= data$C0799900 & data$C0799900 <= 99.0] <- 85.0
  data$C0799900[100.0 <= data$C0799900 & data$C0799900 <= 114.0] <- 100.0
  data$C0799900[115.0 <= data$C0799900 & data$C0799900 <= 129.0] <- 115.0
  data$C0799900[130.0 <= data$C0799900 & data$C0799900 <= 144.0] <- 130.0
  data$C0799900[145.0 <= data$C0799900 & data$C0799900 <= 9999.0] <- 145.0
  data$C0799900 <- factor(data$C0799900,
                          levels=c(0.0,65.0,70.0,85.0,100.0,115.0,130.0,145.0),
                          labels=c("0 TO 64",
                                   "65 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 144",
                                   "145 TO 9999: 145 +"))
  data$C0992000[1.0 <= data$C0992000 & data$C0992000 <= 99.0] <- 1.0
  data$C0992000[100.0 <= data$C0992000 & data$C0992000 <= 199.0] <- 100.0
  data$C0992000[200.0 <= data$C0992000 & data$C0992000 <= 299.0] <- 200.0
  data$C0992000[300.0 <= data$C0992000 & data$C0992000 <= 399.0] <- 300.0
  data$C0992000[400.0 <= data$C0992000 & data$C0992000 <= 499.0] <- 400.0
  data$C0992000[500.0 <= data$C0992000 & data$C0992000 <= 599.0] <- 500.0
  data$C0992000[600.0 <= data$C0992000 & data$C0992000 <= 699.0] <- 600.0
  data$C0992000[700.0 <= data$C0992000 & data$C0992000 <= 799.0] <- 700.0
  data$C0992000[800.0 <= data$C0992000 & data$C0992000 <= 899.0] <- 800.0
  data$C0992000[900.0 <= data$C0992000 & data$C0992000 <= 999.0] <- 900.0
  data$C0992000[1000.0 <= data$C0992000 & data$C0992000 <= 1099.0] <- 1000.0
  data$C0992000[1100.0 <= data$C0992000 & data$C0992000 <= 1199.0] <- 1100.0
  data$C0992000[1200.0 <= data$C0992000 & data$C0992000 <= 1299.0] <- 1200.0
  data$C0992000[1300.0 <= data$C0992000 & data$C0992000 <= 1399.0] <- 1300.0
  data$C0992000[1400.0 <= data$C0992000 & data$C0992000 <= 1499.0] <- 1400.0
  data$C0992000[1500.0 <= data$C0992000 & data$C0992000 <= 9999999.0] <- 1500.0
  data$C0992000 <- factor(data$C0992000,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0,1100.0,1200.0,1300.0,1400.0,1500.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 1099",
                                   "1100 TO 1199",
                                   "1200 TO 1299",
                                   "1300 TO 1399",
                                   "1400 TO 1499",
                                   "1500 TO 9999999: 1500+"))
  data$C0998800[0.0 <= data$C0998800 & data$C0998800 <= 64.0] <- 0.0
  data$C0998800[65.0 <= data$C0998800 & data$C0998800 <= 69.0] <- 65.0
  data$C0998800[70.0 <= data$C0998800 & data$C0998800 <= 84.0] <- 70.0
  data$C0998800[85.0 <= data$C0998800 & data$C0998800 <= 99.0] <- 85.0
  data$C0998800[100.0 <= data$C0998800 & data$C0998800 <= 114.0] <- 100.0
  data$C0998800[115.0 <= data$C0998800 & data$C0998800 <= 129.0] <- 115.0
  data$C0998800[130.0 <= data$C0998800 & data$C0998800 <= 144.0] <- 130.0
  data$C0998800[145.0 <= data$C0998800 & data$C0998800 <= 9999.0] <- 145.0
  data$C0998800 <- factor(data$C0998800,
                          levels=c(0.0,65.0,70.0,85.0,100.0,115.0,130.0,145.0),
                          labels=c("0 TO 64",
                                   "65 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 144",
                                   "145 TO 9999: 145 +"))
  data$C0999100[0.0 <= data$C0999100 & data$C0999100 <= 64.0] <- 0.0
  data$C0999100[65.0 <= data$C0999100 & data$C0999100 <= 69.0] <- 65.0
  data$C0999100[70.0 <= data$C0999100 & data$C0999100 <= 84.0] <- 70.0
  data$C0999100[85.0 <= data$C0999100 & data$C0999100 <= 99.0] <- 85.0
  data$C0999100[100.0 <= data$C0999100 & data$C0999100 <= 114.0] <- 100.0
  data$C0999100[115.0 <= data$C0999100 & data$C0999100 <= 129.0] <- 115.0
  data$C0999100[130.0 <= data$C0999100 & data$C0999100 <= 144.0] <- 130.0
  data$C0999100[145.0 <= data$C0999100 & data$C0999100 <= 9999.0] <- 145.0
  data$C0999100 <- factor(data$C0999100,
                          levels=c(0.0,65.0,70.0,85.0,100.0,115.0,130.0,145.0),
                          labels=c("0 TO 64",
                                   "65 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 144",
                                   "145 TO 9999: 145 +"))
  data$C1192300[1.0 <= data$C1192300 & data$C1192300 <= 99.0] <- 1.0
  data$C1192300[100.0 <= data$C1192300 & data$C1192300 <= 199.0] <- 100.0
  data$C1192300[200.0 <= data$C1192300 & data$C1192300 <= 299.0] <- 200.0
  data$C1192300[300.0 <= data$C1192300 & data$C1192300 <= 399.0] <- 300.0
  data$C1192300[400.0 <= data$C1192300 & data$C1192300 <= 499.0] <- 400.0
  data$C1192300[500.0 <= data$C1192300 & data$C1192300 <= 599.0] <- 500.0
  data$C1192300[600.0 <= data$C1192300 & data$C1192300 <= 699.0] <- 600.0
  data$C1192300[700.0 <= data$C1192300 & data$C1192300 <= 799.0] <- 700.0
  data$C1192300[800.0 <= data$C1192300 & data$C1192300 <= 899.0] <- 800.0
  data$C1192300[900.0 <= data$C1192300 & data$C1192300 <= 999.0] <- 900.0
  data$C1192300[1000.0 <= data$C1192300 & data$C1192300 <= 1099.0] <- 1000.0
  data$C1192300[1100.0 <= data$C1192300 & data$C1192300 <= 1199.0] <- 1100.0
  data$C1192300[1200.0 <= data$C1192300 & data$C1192300 <= 1299.0] <- 1200.0
  data$C1192300[1300.0 <= data$C1192300 & data$C1192300 <= 1399.0] <- 1300.0
  data$C1192300[1400.0 <= data$C1192300 & data$C1192300 <= 1499.0] <- 1400.0
  data$C1192300[1500.0 <= data$C1192300 & data$C1192300 <= 9999999.0] <- 1500.0
  data$C1192300 <- factor(data$C1192300,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0,1100.0,1200.0,1300.0,1400.0,1500.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 1099",
                                   "1100 TO 1199",
                                   "1200 TO 1299",
                                   "1300 TO 1399",
                                   "1400 TO 1499",
                                   "1500 TO 9999999: 1500+"))
  data$C1198800[0.0 <= data$C1198800 & data$C1198800 <= 64.0] <- 0.0
  data$C1198800[65.0 <= data$C1198800 & data$C1198800 <= 69.0] <- 65.0
  data$C1198800[70.0 <= data$C1198800 & data$C1198800 <= 84.0] <- 70.0
  data$C1198800[85.0 <= data$C1198800 & data$C1198800 <= 99.0] <- 85.0
  data$C1198800[100.0 <= data$C1198800 & data$C1198800 <= 114.0] <- 100.0
  data$C1198800[115.0 <= data$C1198800 & data$C1198800 <= 129.0] <- 115.0
  data$C1198800[130.0 <= data$C1198800 & data$C1198800 <= 144.0] <- 130.0
  data$C1198800[145.0 <= data$C1198800 & data$C1198800 <= 9999.0] <- 145.0
  data$C1198800 <- factor(data$C1198800,
                          levels=c(0.0,65.0,70.0,85.0,100.0,115.0,130.0,145.0),
                          labels=c("0 TO 64",
                                   "65 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 144",
                                   "145 TO 9999: 145 +"))
  data$C1199100[0.0 <= data$C1199100 & data$C1199100 <= 64.0] <- 0.0
  data$C1199100[65.0 <= data$C1199100 & data$C1199100 <= 69.0] <- 65.0
  data$C1199100[70.0 <= data$C1199100 & data$C1199100 <= 84.0] <- 70.0
  data$C1199100[85.0 <= data$C1199100 & data$C1199100 <= 99.0] <- 85.0
  data$C1199100[100.0 <= data$C1199100 & data$C1199100 <= 114.0] <- 100.0
  data$C1199100[115.0 <= data$C1199100 & data$C1199100 <= 129.0] <- 115.0
  data$C1199100[130.0 <= data$C1199100 & data$C1199100 <= 144.0] <- 130.0
  data$C1199100[145.0 <= data$C1199100 & data$C1199100 <= 9999.0] <- 145.0
  data$C1199100 <- factor(data$C1199100,
                          levels=c(0.0,65.0,70.0,85.0,100.0,115.0,130.0,145.0),
                          labels=c("0 TO 64",
                                   "65 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 144",
                                   "145 TO 9999: 145 +"))
  data$C1500100[1.0 <= data$C1500100 & data$C1500100 <= 99.0] <- 1.0
  data$C1500100[100.0 <= data$C1500100 & data$C1500100 <= 199.0] <- 100.0
  data$C1500100[200.0 <= data$C1500100 & data$C1500100 <= 299.0] <- 200.0
  data$C1500100[300.0 <= data$C1500100 & data$C1500100 <= 399.0] <- 300.0
  data$C1500100[400.0 <= data$C1500100 & data$C1500100 <= 499.0] <- 400.0
  data$C1500100[500.0 <= data$C1500100 & data$C1500100 <= 599.0] <- 500.0
  data$C1500100[600.0 <= data$C1500100 & data$C1500100 <= 699.0] <- 600.0
  data$C1500100[700.0 <= data$C1500100 & data$C1500100 <= 799.0] <- 700.0
  data$C1500100[800.0 <= data$C1500100 & data$C1500100 <= 899.0] <- 800.0
  data$C1500100[900.0 <= data$C1500100 & data$C1500100 <= 999.0] <- 900.0
  data$C1500100[1000.0 <= data$C1500100 & data$C1500100 <= 1099.0] <- 1000.0
  data$C1500100[1100.0 <= data$C1500100 & data$C1500100 <= 1199.0] <- 1100.0
  data$C1500100[1200.0 <= data$C1500100 & data$C1500100 <= 1299.0] <- 1200.0
  data$C1500100[1300.0 <= data$C1500100 & data$C1500100 <= 1399.0] <- 1300.0
  data$C1500100[1400.0 <= data$C1500100 & data$C1500100 <= 1499.0] <- 1400.0
  data$C1500100[1500.0 <= data$C1500100 & data$C1500100 <= 9999999.0] <- 1500.0
  data$C1500100 <- factor(data$C1500100,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0,1100.0,1200.0,1300.0,1400.0,1500.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 1099",
                                   "1100 TO 1199",
                                   "1200 TO 1299",
                                   "1300 TO 1399",
                                   "1400 TO 1499",
                                   "1500 TO 9999999: 1500+"))
  data$C1507800[0.0 <= data$C1507800 & data$C1507800 <= 64.0] <- 0.0
  data$C1507800[65.0 <= data$C1507800 & data$C1507800 <= 69.0] <- 65.0
  data$C1507800[70.0 <= data$C1507800 & data$C1507800 <= 84.0] <- 70.0
  data$C1507800[85.0 <= data$C1507800 & data$C1507800 <= 99.0] <- 85.0
  data$C1507800[100.0 <= data$C1507800 & data$C1507800 <= 114.0] <- 100.0
  data$C1507800[115.0 <= data$C1507800 & data$C1507800 <= 129.0] <- 115.0
  data$C1507800[130.0 <= data$C1507800 & data$C1507800 <= 144.0] <- 130.0
  data$C1507800[145.0 <= data$C1507800 & data$C1507800 <= 9999.0] <- 145.0
  data$C1507800 <- factor(data$C1507800,
                          levels=c(0.0,65.0,70.0,85.0,100.0,115.0,130.0,145.0),
                          labels=c("0 TO 64",
                                   "65 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 144",
                                   "145 TO 9999: 145 +"))
  data$C1508100[0.0 <= data$C1508100 & data$C1508100 <= 64.0] <- 0.0
  data$C1508100[65.0 <= data$C1508100 & data$C1508100 <= 69.0] <- 65.0
  data$C1508100[70.0 <= data$C1508100 & data$C1508100 <= 84.0] <- 70.0
  data$C1508100[85.0 <= data$C1508100 & data$C1508100 <= 99.0] <- 85.0
  data$C1508100[100.0 <= data$C1508100 & data$C1508100 <= 114.0] <- 100.0
  data$C1508100[115.0 <= data$C1508100 & data$C1508100 <= 129.0] <- 115.0
  data$C1508100[130.0 <= data$C1508100 & data$C1508100 <= 144.0] <- 130.0
  data$C1508100[145.0 <= data$C1508100 & data$C1508100 <= 9999.0] <- 145.0
  data$C1508100 <- factor(data$C1508100,
                          levels=c(0.0,65.0,70.0,85.0,100.0,115.0,130.0,145.0),
                          labels=c("0 TO 64",
                                   "65 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 144",
                                   "145 TO 9999: 145 +"))
  data$C1557000[1.0 <= data$C1557000 & data$C1557000 <= 99.0] <- 1.0
  data$C1557000[100.0 <= data$C1557000 & data$C1557000 <= 199.0] <- 100.0
  data$C1557000[200.0 <= data$C1557000 & data$C1557000 <= 299.0] <- 200.0
  data$C1557000[300.0 <= data$C1557000 & data$C1557000 <= 399.0] <- 300.0
  data$C1557000[400.0 <= data$C1557000 & data$C1557000 <= 499.0] <- 400.0
  data$C1557000[500.0 <= data$C1557000 & data$C1557000 <= 599.0] <- 500.0
  data$C1557000[600.0 <= data$C1557000 & data$C1557000 <= 699.0] <- 600.0
  data$C1557000[700.0 <= data$C1557000 & data$C1557000 <= 799.0] <- 700.0
  data$C1557000[800.0 <= data$C1557000 & data$C1557000 <= 899.0] <- 800.0
  data$C1557000[900.0 <= data$C1557000 & data$C1557000 <= 999.0] <- 900.0
  data$C1557000[1000.0 <= data$C1557000 & data$C1557000 <= 9.9999999E7] <- 1000.0
  data$C1557000 <- factor(data$C1557000,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999: 1000+"))
  data$C1564700[0.0 <= data$C1564700 & data$C1564700 <= 69.0] <- 0.0
  data$C1564700[70.0 <= data$C1564700 & data$C1564700 <= 84.0] <- 70.0
  data$C1564700[85.0 <= data$C1564700 & data$C1564700 <= 99.0] <- 85.0
  data$C1564700[100.0 <= data$C1564700 & data$C1564700 <= 114.0] <- 100.0
  data$C1564700[115.0 <= data$C1564700 & data$C1564700 <= 129.0] <- 115.0
  data$C1564700[130.0 <= data$C1564700 & data$C1564700 <= 999999.0] <- 130.0
  data$C1564700 <- factor(data$C1564700,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C1565000[0.0 <= data$C1565000 & data$C1565000 <= 69.0] <- 0.0
  data$C1565000[70.0 <= data$C1565000 & data$C1565000 <= 84.0] <- 70.0
  data$C1565000[85.0 <= data$C1565000 & data$C1565000 <= 99.0] <- 85.0
  data$C1565000[100.0 <= data$C1565000 & data$C1565000 <= 114.0] <- 100.0
  data$C1565000[115.0 <= data$C1565000 & data$C1565000 <= 129.0] <- 115.0
  data$C1565000[130.0 <= data$C1565000 & data$C1565000 <= 999999.0] <- 130.0
  data$C1565000 <- factor(data$C1565000,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C1792700[1.0 <= data$C1792700 & data$C1792700 <= 99.0] <- 1.0
  data$C1792700[100.0 <= data$C1792700 & data$C1792700 <= 199.0] <- 100.0
  data$C1792700[200.0 <= data$C1792700 & data$C1792700 <= 299.0] <- 200.0
  data$C1792700[300.0 <= data$C1792700 & data$C1792700 <= 399.0] <- 300.0
  data$C1792700[400.0 <= data$C1792700 & data$C1792700 <= 499.0] <- 400.0
  data$C1792700[500.0 <= data$C1792700 & data$C1792700 <= 599.0] <- 500.0
  data$C1792700[600.0 <= data$C1792700 & data$C1792700 <= 699.0] <- 600.0
  data$C1792700[700.0 <= data$C1792700 & data$C1792700 <= 799.0] <- 700.0
  data$C1792700[800.0 <= data$C1792700 & data$C1792700 <= 899.0] <- 800.0
  data$C1792700[900.0 <= data$C1792700 & data$C1792700 <= 999.0] <- 900.0
  data$C1792700[1000.0 <= data$C1792700 & data$C1792700 <= 9.9999999E7] <- 1000.0
  data$C1792700 <- factor(data$C1792700,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999"))
  data$C1800100[0.0 <= data$C1800100 & data$C1800100 <= 69.0] <- 0.0
  data$C1800100[70.0 <= data$C1800100 & data$C1800100 <= 84.0] <- 70.0
  data$C1800100[85.0 <= data$C1800100 & data$C1800100 <= 99.0] <- 85.0
  data$C1800100[100.0 <= data$C1800100 & data$C1800100 <= 114.0] <- 100.0
  data$C1800100[115.0 <= data$C1800100 & data$C1800100 <= 129.0] <- 115.0
  data$C1800100[130.0 <= data$C1800100 & data$C1800100 <= 999999.0] <- 130.0
  data$C1800100 <- factor(data$C1800100,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999"))
  data$C1800400[0.0 <= data$C1800400 & data$C1800400 <= 69.0] <- 0.0
  data$C1800400[70.0 <= data$C1800400 & data$C1800400 <= 84.0] <- 70.0
  data$C1800400[85.0 <= data$C1800400 & data$C1800400 <= 99.0] <- 85.0
  data$C1800400[100.0 <= data$C1800400 & data$C1800400 <= 114.0] <- 100.0
  data$C1800400[115.0 <= data$C1800400 & data$C1800400 <= 129.0] <- 115.0
  data$C1800400[130.0 <= data$C1800400 & data$C1800400 <= 999999.0] <- 130.0
  data$C1800400 <- factor(data$C1800400,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999"))
  data$C2502600[1.0 <= data$C2502600 & data$C2502600 <= 99.0] <- 1.0
  data$C2502600[100.0 <= data$C2502600 & data$C2502600 <= 199.0] <- 100.0
  data$C2502600[200.0 <= data$C2502600 & data$C2502600 <= 299.0] <- 200.0
  data$C2502600[300.0 <= data$C2502600 & data$C2502600 <= 399.0] <- 300.0
  data$C2502600[400.0 <= data$C2502600 & data$C2502600 <= 499.0] <- 400.0
  data$C2502600[500.0 <= data$C2502600 & data$C2502600 <= 599.0] <- 500.0
  data$C2502600[600.0 <= data$C2502600 & data$C2502600 <= 699.0] <- 600.0
  data$C2502600[700.0 <= data$C2502600 & data$C2502600 <= 799.0] <- 700.0
  data$C2502600[800.0 <= data$C2502600 & data$C2502600 <= 899.0] <- 800.0
  data$C2502600[900.0 <= data$C2502600 & data$C2502600 <= 999.0] <- 900.0
  data$C2502600[1000.0 <= data$C2502600 & data$C2502600 <= 9.9999999E7] <- 1000.0
  data$C2502600 <- factor(data$C2502600,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999: 1000+"))
  data$C2503700[0.0 <= data$C2503700 & data$C2503700 <= 69.0] <- 0.0
  data$C2503700[70.0 <= data$C2503700 & data$C2503700 <= 84.0] <- 70.0
  data$C2503700[85.0 <= data$C2503700 & data$C2503700 <= 99.0] <- 85.0
  data$C2503700[100.0 <= data$C2503700 & data$C2503700 <= 114.0] <- 100.0
  data$C2503700[115.0 <= data$C2503700 & data$C2503700 <= 129.0] <- 115.0
  data$C2503700[130.0 <= data$C2503700 & data$C2503700 <= 999999.0] <- 130.0
  data$C2503700 <- factor(data$C2503700,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C2503900[0.0 <= data$C2503900 & data$C2503900 <= 69.0] <- 0.0
  data$C2503900[70.0 <= data$C2503900 & data$C2503900 <= 84.0] <- 70.0
  data$C2503900[85.0 <= data$C2503900 & data$C2503900 <= 99.0] <- 85.0
  data$C2503900[100.0 <= data$C2503900 & data$C2503900 <= 114.0] <- 100.0
  data$C2503900[115.0 <= data$C2503900 & data$C2503900 <= 129.0] <- 115.0
  data$C2503900[130.0 <= data$C2503900 & data$C2503900 <= 999999.0] <- 130.0
  data$C2503900 <- factor(data$C2503900,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C2531100[1.0 <= data$C2531100 & data$C2531100 <= 99.0] <- 1.0
  data$C2531100[100.0 <= data$C2531100 & data$C2531100 <= 199.0] <- 100.0
  data$C2531100[200.0 <= data$C2531100 & data$C2531100 <= 299.0] <- 200.0
  data$C2531100[300.0 <= data$C2531100 & data$C2531100 <= 399.0] <- 300.0
  data$C2531100[400.0 <= data$C2531100 & data$C2531100 <= 499.0] <- 400.0
  data$C2531100[500.0 <= data$C2531100 & data$C2531100 <= 599.0] <- 500.0
  data$C2531100[600.0 <= data$C2531100 & data$C2531100 <= 699.0] <- 600.0
  data$C2531100[700.0 <= data$C2531100 & data$C2531100 <= 799.0] <- 700.0
  data$C2531100[800.0 <= data$C2531100 & data$C2531100 <= 899.0] <- 800.0
  data$C2531100[900.0 <= data$C2531100 & data$C2531100 <= 999.0] <- 900.0
  data$C2531100[1000.0 <= data$C2531100 & data$C2531100 <= 9.9999999E7] <- 1000.0
  data$C2531100 <- factor(data$C2531100,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999: 1000+"))
  data$C2532200[0.0 <= data$C2532200 & data$C2532200 <= 69.0] <- 0.0
  data$C2532200[70.0 <= data$C2532200 & data$C2532200 <= 84.0] <- 70.0
  data$C2532200[85.0 <= data$C2532200 & data$C2532200 <= 99.0] <- 85.0
  data$C2532200[100.0 <= data$C2532200 & data$C2532200 <= 114.0] <- 100.0
  data$C2532200[115.0 <= data$C2532200 & data$C2532200 <= 129.0] <- 115.0
  data$C2532200[130.0 <= data$C2532200 & data$C2532200 <= 999999.0] <- 130.0
  data$C2532200 <- factor(data$C2532200,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C2532400[0.0 <= data$C2532400 & data$C2532400 <= 69.0] <- 0.0
  data$C2532400[70.0 <= data$C2532400 & data$C2532400 <= 84.0] <- 70.0
  data$C2532400[85.0 <= data$C2532400 & data$C2532400 <= 99.0] <- 85.0
  data$C2532400[100.0 <= data$C2532400 & data$C2532400 <= 114.0] <- 100.0
  data$C2532400[115.0 <= data$C2532400 & data$C2532400 <= 129.0] <- 115.0
  data$C2532400[130.0 <= data$C2532400 & data$C2532400 <= 999999.0] <- 130.0
  data$C2532400 <- factor(data$C2532400,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C2802400[1.0 <= data$C2802400 & data$C2802400 <= 99.0] <- 1.0
  data$C2802400[100.0 <= data$C2802400 & data$C2802400 <= 199.0] <- 100.0
  data$C2802400[200.0 <= data$C2802400 & data$C2802400 <= 299.0] <- 200.0
  data$C2802400[300.0 <= data$C2802400 & data$C2802400 <= 399.0] <- 300.0
  data$C2802400[400.0 <= data$C2802400 & data$C2802400 <= 499.0] <- 400.0
  data$C2802400[500.0 <= data$C2802400 & data$C2802400 <= 599.0] <- 500.0
  data$C2802400[600.0 <= data$C2802400 & data$C2802400 <= 699.0] <- 600.0
  data$C2802400[700.0 <= data$C2802400 & data$C2802400 <= 799.0] <- 700.0
  data$C2802400[800.0 <= data$C2802400 & data$C2802400 <= 899.0] <- 800.0
  data$C2802400[900.0 <= data$C2802400 & data$C2802400 <= 999.0] <- 900.0
  data$C2802400[1000.0 <= data$C2802400 & data$C2802400 <= 9.9999999E7] <- 1000.0
  data$C2802400 <- factor(data$C2802400,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999: 1000+"))
  data$C2803000[0.0 <= data$C2803000 & data$C2803000 <= 69.0] <- 0.0
  data$C2803000[70.0 <= data$C2803000 & data$C2803000 <= 84.0] <- 70.0
  data$C2803000[85.0 <= data$C2803000 & data$C2803000 <= 99.0] <- 85.0
  data$C2803000[100.0 <= data$C2803000 & data$C2803000 <= 114.0] <- 100.0
  data$C2803000[115.0 <= data$C2803000 & data$C2803000 <= 129.0] <- 115.0
  data$C2803000[130.0 <= data$C2803000 & data$C2803000 <= 999999.0] <- 130.0
  data$C2803000 <- factor(data$C2803000,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C2803200[0.0 <= data$C2803200 & data$C2803200 <= 69.0] <- 0.0
  data$C2803200[70.0 <= data$C2803200 & data$C2803200 <= 84.0] <- 70.0
  data$C2803200[85.0 <= data$C2803200 & data$C2803200 <= 99.0] <- 85.0
  data$C2803200[100.0 <= data$C2803200 & data$C2803200 <= 114.0] <- 100.0
  data$C2803200[115.0 <= data$C2803200 & data$C2803200 <= 129.0] <- 115.0
  data$C2803200[130.0 <= data$C2803200 & data$C2803200 <= 999999.0] <- 130.0
  data$C2803200 <- factor(data$C2803200,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C3110400[1.0 <= data$C3110400 & data$C3110400 <= 99.0] <- 1.0
  data$C3110400[100.0 <= data$C3110400 & data$C3110400 <= 199.0] <- 100.0
  data$C3110400[200.0 <= data$C3110400 & data$C3110400 <= 299.0] <- 200.0
  data$C3110400[300.0 <= data$C3110400 & data$C3110400 <= 399.0] <- 300.0
  data$C3110400[400.0 <= data$C3110400 & data$C3110400 <= 499.0] <- 400.0
  data$C3110400[500.0 <= data$C3110400 & data$C3110400 <= 599.0] <- 500.0
  data$C3110400[600.0 <= data$C3110400 & data$C3110400 <= 699.0] <- 600.0
  data$C3110400[700.0 <= data$C3110400 & data$C3110400 <= 799.0] <- 700.0
  data$C3110400[800.0 <= data$C3110400 & data$C3110400 <= 899.0] <- 800.0
  data$C3110400[900.0 <= data$C3110400 & data$C3110400 <= 999.0] <- 900.0
  data$C3110400[1000.0 <= data$C3110400 & data$C3110400 <= 9.9999999E7] <- 1000.0
  data$C3110400 <- factor(data$C3110400,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999: 1000+"))
  data$C3111500[0.0 <= data$C3111500 & data$C3111500 <= 69.0] <- 0.0
  data$C3111500[70.0 <= data$C3111500 & data$C3111500 <= 84.0] <- 70.0
  data$C3111500[85.0 <= data$C3111500 & data$C3111500 <= 99.0] <- 85.0
  data$C3111500[100.0 <= data$C3111500 & data$C3111500 <= 114.0] <- 100.0
  data$C3111500[115.0 <= data$C3111500 & data$C3111500 <= 129.0] <- 115.0
  data$C3111500[130.0 <= data$C3111500 & data$C3111500 <= 999999.0] <- 130.0
  data$C3111500 <- factor(data$C3111500,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C3111700[0.0 <= data$C3111700 & data$C3111700 <= 69.0] <- 0.0
  data$C3111700[70.0 <= data$C3111700 & data$C3111700 <= 84.0] <- 70.0
  data$C3111700[85.0 <= data$C3111700 & data$C3111700 <= 99.0] <- 85.0
  data$C3111700[100.0 <= data$C3111700 & data$C3111700 <= 114.0] <- 100.0
  data$C3111700[115.0 <= data$C3111700 & data$C3111700 <= 129.0] <- 115.0
  data$C3111700[130.0 <= data$C3111700 & data$C3111700 <= 999999.0] <- 130.0
  data$C3111700 <- factor(data$C3111700,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C3601100[0.0 <= data$C3601100 & data$C3601100 <= 11.0] <- 0.0
  data$C3601100[12.0 <= data$C3601100 & data$C3601100 <= 23.0] <- 12.0
  data$C3601100[24.0 <= data$C3601100 & data$C3601100 <= 35.0] <- 24.0
  data$C3601100[36.0 <= data$C3601100 & data$C3601100 <= 47.0] <- 36.0
  data$C3601100[48.0 <= data$C3601100 & data$C3601100 <= 59.0] <- 48.0
  data$C3601100[60.0 <= data$C3601100 & data$C3601100 <= 71.0] <- 60.0
  data$C3601100[72.0 <= data$C3601100 & data$C3601100 <= 83.0] <- 72.0
  data$C3601100[84.0 <= data$C3601100 & data$C3601100 <= 95.0] <- 84.0
  data$C3601100[96.0 <= data$C3601100 & data$C3601100 <= 107.0] <- 96.0
  data$C3601100[108.0 <= data$C3601100 & data$C3601100 <= 119.0] <- 108.0
  data$C3601100[120.0 <= data$C3601100 & data$C3601100 <= 131.0] <- 120.0
  data$C3601100[132.0 <= data$C3601100 & data$C3601100 <= 143.0] <- 132.0
  data$C3601100[144.0 <= data$C3601100 & data$C3601100 <= 155.0] <- 144.0
  data$C3601100[156.0 <= data$C3601100 & data$C3601100 <= 167.0] <- 156.0
  data$C3601100[168.0 <= data$C3601100 & data$C3601100 <= 179.0] <- 168.0
  data$C3601100[180.0 <= data$C3601100 & data$C3601100 <= 999.0] <- 180.0
  data$C3601100 <- factor(data$C3601100,
                          levels=c(0.0,12.0,24.0,36.0,48.0,60.0,72.0,84.0,96.0,108.0,120.0,132.0,144.0,156.0,168.0,180.0),
                          labels=c("0 TO 11: 0-11 MONTHS",
                                   "12 TO 23: 12-23 MONTHS",
                                   "24 TO 35: 24-35 MONTHS",
                                   "36 TO 47: 36-47 MONTHS",
                                   "48 TO 59: 48-59 MONTHS",
                                   "60 TO 71: 60-71 MONTHS",
                                   "72 TO 83: 72-83 MONTHS",
                                   "84 TO 95: 84-95 MONTHS",
                                   "96 TO 107: 96-107 MONTHS",
                                   "108 TO 119: 108-119 MONTHS",
                                   "120 TO 131: 120-131 MONTHS",
                                   "132 TO 143: 132-143 MONTHS",
                                   "144 TO 155: 144-155 MONTHS",
                                   "156 TO 167: 156-167 MONTHS",
                                   "168 TO 179: 168-179 MONTHS",
                                   "180 TO 999: 180 + MONTHS"))
  data$C3614100[1.0 <= data$C3614100 & data$C3614100 <= 99.0] <- 1.0
  data$C3614100[100.0 <= data$C3614100 & data$C3614100 <= 199.0] <- 100.0
  data$C3614100[200.0 <= data$C3614100 & data$C3614100 <= 299.0] <- 200.0
  data$C3614100[300.0 <= data$C3614100 & data$C3614100 <= 399.0] <- 300.0
  data$C3614100[400.0 <= data$C3614100 & data$C3614100 <= 499.0] <- 400.0
  data$C3614100[500.0 <= data$C3614100 & data$C3614100 <= 599.0] <- 500.0
  data$C3614100[600.0 <= data$C3614100 & data$C3614100 <= 699.0] <- 600.0
  data$C3614100[700.0 <= data$C3614100 & data$C3614100 <= 799.0] <- 700.0
  data$C3614100[800.0 <= data$C3614100 & data$C3614100 <= 899.0] <- 800.0
  data$C3614100[900.0 <= data$C3614100 & data$C3614100 <= 999.0] <- 900.0
  data$C3614100[1000.0 <= data$C3614100 & data$C3614100 <= 9.9999999E7] <- 1000.0
  data$C3614100 <- factor(data$C3614100,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999: 1000+"))
  data$C3615200[0.0 <= data$C3615200 & data$C3615200 <= 69.0] <- 0.0
  data$C3615200[70.0 <= data$C3615200 & data$C3615200 <= 84.0] <- 70.0
  data$C3615200[85.0 <= data$C3615200 & data$C3615200 <= 99.0] <- 85.0
  data$C3615200[100.0 <= data$C3615200 & data$C3615200 <= 114.0] <- 100.0
  data$C3615200[115.0 <= data$C3615200 & data$C3615200 <= 129.0] <- 115.0
  data$C3615200[130.0 <= data$C3615200 & data$C3615200 <= 999999.0] <- 130.0
  data$C3615200 <- factor(data$C3615200,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C3615400[0.0 <= data$C3615400 & data$C3615400 <= 69.0] <- 0.0
  data$C3615400[70.0 <= data$C3615400 & data$C3615400 <= 84.0] <- 70.0
  data$C3615400[85.0 <= data$C3615400 & data$C3615400 <= 99.0] <- 85.0
  data$C3615400[100.0 <= data$C3615400 & data$C3615400 <= 114.0] <- 100.0
  data$C3615400[115.0 <= data$C3615400 & data$C3615400 <= 129.0] <- 115.0
  data$C3615400[130.0 <= data$C3615400 & data$C3615400 <= 999999.0] <- 130.0
  data$C3615400 <- factor(data$C3615400,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C3981100[0.0 <= data$C3981100 & data$C3981100 <= 11.0] <- 0.0
  data$C3981100[12.0 <= data$C3981100 & data$C3981100 <= 23.0] <- 12.0
  data$C3981100[24.0 <= data$C3981100 & data$C3981100 <= 35.0] <- 24.0
  data$C3981100[36.0 <= data$C3981100 & data$C3981100 <= 47.0] <- 36.0
  data$C3981100[48.0 <= data$C3981100 & data$C3981100 <= 59.0] <- 48.0
  data$C3981100[60.0 <= data$C3981100 & data$C3981100 <= 71.0] <- 60.0
  data$C3981100[72.0 <= data$C3981100 & data$C3981100 <= 83.0] <- 72.0
  data$C3981100[84.0 <= data$C3981100 & data$C3981100 <= 95.0] <- 84.0
  data$C3981100[96.0 <= data$C3981100 & data$C3981100 <= 107.0] <- 96.0
  data$C3981100[108.0 <= data$C3981100 & data$C3981100 <= 119.0] <- 108.0
  data$C3981100[120.0 <= data$C3981100 & data$C3981100 <= 131.0] <- 120.0
  data$C3981100[132.0 <= data$C3981100 & data$C3981100 <= 143.0] <- 132.0
  data$C3981100[144.0 <= data$C3981100 & data$C3981100 <= 155.0] <- 144.0
  data$C3981100[156.0 <= data$C3981100 & data$C3981100 <= 167.0] <- 156.0
  data$C3981100[168.0 <= data$C3981100 & data$C3981100 <= 179.0] <- 168.0
  data$C3981100[180.0 <= data$C3981100 & data$C3981100 <= 999.0] <- 180.0
  data$C3981100 <- factor(data$C3981100,
                          levels=c(0.0,12.0,24.0,36.0,48.0,60.0,72.0,84.0,96.0,108.0,120.0,132.0,144.0,156.0,168.0,180.0),
                          labels=c("0 TO 11: 0-11 MONTHS",
                                   "12 TO 23: 12-23 MONTHS",
                                   "24 TO 35: 24-35 MONTHS",
                                   "36 TO 47: 36-47 MONTHS",
                                   "48 TO 59: 48-59 MONTHS",
                                   "60 TO 71: 60-71 MONTHS",
                                   "72 TO 83: 72-83 MONTHS",
                                   "84 TO 95: 84-95 MONTHS",
                                   "96 TO 107: 96-107 MONTHS",
                                   "108 TO 119: 108-119 MONTHS",
                                   "120 TO 131: 120-131 MONTHS",
                                   "132 TO 143: 132-143 MONTHS",
                                   "144 TO 155: 144-155 MONTHS",
                                   "156 TO 167: 156-167 MONTHS",
                                   "168 TO 179: 168-179 MONTHS",
                                   "180 TO 999: 180 + MONTHS"))
  data$C3992700[1.0 <= data$C3992700 & data$C3992700 <= 99.0] <- 1.0
  data$C3992700[100.0 <= data$C3992700 & data$C3992700 <= 199.0] <- 100.0
  data$C3992700[200.0 <= data$C3992700 & data$C3992700 <= 299.0] <- 200.0
  data$C3992700[300.0 <= data$C3992700 & data$C3992700 <= 399.0] <- 300.0
  data$C3992700[400.0 <= data$C3992700 & data$C3992700 <= 499.0] <- 400.0
  data$C3992700[500.0 <= data$C3992700 & data$C3992700 <= 599.0] <- 500.0
  data$C3992700[600.0 <= data$C3992700 & data$C3992700 <= 699.0] <- 600.0
  data$C3992700[700.0 <= data$C3992700 & data$C3992700 <= 799.0] <- 700.0
  data$C3992700[800.0 <= data$C3992700 & data$C3992700 <= 899.0] <- 800.0
  data$C3992700[900.0 <= data$C3992700 & data$C3992700 <= 999.0] <- 900.0
  data$C3992700[1000.0 <= data$C3992700 & data$C3992700 <= 9.9999999E7] <- 1000.0
  data$C3992700 <- factor(data$C3992700,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999: 1000+"))
  data$C3993800[0.0 <= data$C3993800 & data$C3993800 <= 69.0] <- 0.0
  data$C3993800[70.0 <= data$C3993800 & data$C3993800 <= 84.0] <- 70.0
  data$C3993800[85.0 <= data$C3993800 & data$C3993800 <= 99.0] <- 85.0
  data$C3993800[100.0 <= data$C3993800 & data$C3993800 <= 114.0] <- 100.0
  data$C3993800[115.0 <= data$C3993800 & data$C3993800 <= 129.0] <- 115.0
  data$C3993800[130.0 <= data$C3993800 & data$C3993800 <= 999999.0] <- 130.0
  data$C3993800 <- factor(data$C3993800,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C3994000[0.0 <= data$C3994000 & data$C3994000 <= 69.0] <- 0.0
  data$C3994000[70.0 <= data$C3994000 & data$C3994000 <= 84.0] <- 70.0
  data$C3994000[85.0 <= data$C3994000 & data$C3994000 <= 99.0] <- 85.0
  data$C3994000[100.0 <= data$C3994000 & data$C3994000 <= 114.0] <- 100.0
  data$C3994000[115.0 <= data$C3994000 & data$C3994000 <= 129.0] <- 115.0
  data$C3994000[130.0 <= data$C3994000 & data$C3994000 <= 999999.0] <- 130.0
  data$C3994000 <- factor(data$C3994000,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C5524800[0.0 <= data$C5524800 & data$C5524800 <= 11.0] <- 0.0
  data$C5524800[12.0 <= data$C5524800 & data$C5524800 <= 23.0] <- 12.0
  data$C5524800[24.0 <= data$C5524800 & data$C5524800 <= 35.0] <- 24.0
  data$C5524800[36.0 <= data$C5524800 & data$C5524800 <= 47.0] <- 36.0
  data$C5524800[48.0 <= data$C5524800 & data$C5524800 <= 59.0] <- 48.0
  data$C5524800[60.0 <= data$C5524800 & data$C5524800 <= 71.0] <- 60.0
  data$C5524800[72.0 <= data$C5524800 & data$C5524800 <= 83.0] <- 72.0
  data$C5524800[84.0 <= data$C5524800 & data$C5524800 <= 95.0] <- 84.0
  data$C5524800[96.0 <= data$C5524800 & data$C5524800 <= 107.0] <- 96.0
  data$C5524800[108.0 <= data$C5524800 & data$C5524800 <= 119.0] <- 108.0
  data$C5524800[120.0 <= data$C5524800 & data$C5524800 <= 131.0] <- 120.0
  data$C5524800[132.0 <= data$C5524800 & data$C5524800 <= 143.0] <- 132.0
  data$C5524800[144.0 <= data$C5524800 & data$C5524800 <= 155.0] <- 144.0
  data$C5524800[156.0 <= data$C5524800 & data$C5524800 <= 167.0] <- 156.0
  data$C5524800[168.0 <= data$C5524800 & data$C5524800 <= 179.0] <- 168.0
  data$C5524800[180.0 <= data$C5524800 & data$C5524800 <= 191.0] <- 180.0
  data$C5524800[192.0 <= data$C5524800 & data$C5524800 <= 999.0] <- 192.0
  data$C5524800 <- factor(data$C5524800,
                          levels=c(0.0,12.0,24.0,36.0,48.0,60.0,72.0,84.0,96.0,108.0,120.0,132.0,144.0,156.0,168.0,180.0,192.0),
                          labels=c("0 TO 11: 0-11 MONTHS",
                                   "12 TO 23: 12-23 MONTHS",
                                   "24 TO 35: 24-35 MONTHS",
                                   "36 TO 47: 36-47 MONTHS",
                                   "48 TO 59: 48-59 MONTHS",
                                   "60 TO 71: 60-71 MONTHS",
                                   "72 TO 83: 72-83 MONTHS",
                                   "84 TO 95: 84-95 MONTHS",
                                   "96 TO 107: 96-107 MONTHS",
                                   "108 TO 119: 108-119 MONTHS",
                                   "120 TO 131: 120-131 MONTHS",
                                   "132 TO 143: 132-143 MONTHS",
                                   "144 TO 155: 144-155 MONTHS",
                                   "156 TO 167: 156-167 MONTHS",
                                   "168 TO 179: 168-179 MONTHS",
                                   "180 TO 191: 180-191 MONTHS",
                                   "192 TO 999: 192 + MONTHS"))
  data$C5536700[1.0 <= data$C5536700 & data$C5536700 <= 99.0] <- 1.0
  data$C5536700[100.0 <= data$C5536700 & data$C5536700 <= 199.0] <- 100.0
  data$C5536700[200.0 <= data$C5536700 & data$C5536700 <= 299.0] <- 200.0
  data$C5536700[300.0 <= data$C5536700 & data$C5536700 <= 399.0] <- 300.0
  data$C5536700[400.0 <= data$C5536700 & data$C5536700 <= 499.0] <- 400.0
  data$C5536700[500.0 <= data$C5536700 & data$C5536700 <= 599.0] <- 500.0
  data$C5536700[600.0 <= data$C5536700 & data$C5536700 <= 699.0] <- 600.0
  data$C5536700[700.0 <= data$C5536700 & data$C5536700 <= 799.0] <- 700.0
  data$C5536700[800.0 <= data$C5536700 & data$C5536700 <= 899.0] <- 800.0
  data$C5536700[900.0 <= data$C5536700 & data$C5536700 <= 999.0] <- 900.0
  data$C5536700[1000.0 <= data$C5536700 & data$C5536700 <= 9.9999999E7] <- 1000.0
  data$C5536700 <- factor(data$C5536700,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999: 1000+"))
  data$C5537800[0.0 <= data$C5537800 & data$C5537800 <= 69.0] <- 0.0
  data$C5537800[70.0 <= data$C5537800 & data$C5537800 <= 84.0] <- 70.0
  data$C5537800[85.0 <= data$C5537800 & data$C5537800 <= 99.0] <- 85.0
  data$C5537800[100.0 <= data$C5537800 & data$C5537800 <= 114.0] <- 100.0
  data$C5537800[115.0 <= data$C5537800 & data$C5537800 <= 129.0] <- 115.0
  data$C5537800[130.0 <= data$C5537800 & data$C5537800 <= 999999.0] <- 130.0
  data$C5537800 <- factor(data$C5537800,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C5538000[0.0 <= data$C5538000 & data$C5538000 <= 69.0] <- 0.0
  data$C5538000[70.0 <= data$C5538000 & data$C5538000 <= 84.0] <- 70.0
  data$C5538000[85.0 <= data$C5538000 & data$C5538000 <= 99.0] <- 85.0
  data$C5538000[100.0 <= data$C5538000 & data$C5538000 <= 114.0] <- 100.0
  data$C5538000[115.0 <= data$C5538000 & data$C5538000 <= 129.0] <- 115.0
  data$C5538000[130.0 <= data$C5538000 & data$C5538000 <= 999999.0] <- 130.0
  data$C5538000 <- factor(data$C5538000,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C5801100[0.0 <= data$C5801100 & data$C5801100 <= 11.0] <- 0.0
  data$C5801100[12.0 <= data$C5801100 & data$C5801100 <= 23.0] <- 12.0
  data$C5801100[24.0 <= data$C5801100 & data$C5801100 <= 35.0] <- 24.0
  data$C5801100[36.0 <= data$C5801100 & data$C5801100 <= 47.0] <- 36.0
  data$C5801100[48.0 <= data$C5801100 & data$C5801100 <= 59.0] <- 48.0
  data$C5801100[60.0 <= data$C5801100 & data$C5801100 <= 71.0] <- 60.0
  data$C5801100[72.0 <= data$C5801100 & data$C5801100 <= 83.0] <- 72.0
  data$C5801100[84.0 <= data$C5801100 & data$C5801100 <= 95.0] <- 84.0
  data$C5801100[96.0 <= data$C5801100 & data$C5801100 <= 107.0] <- 96.0
  data$C5801100[108.0 <= data$C5801100 & data$C5801100 <= 119.0] <- 108.0
  data$C5801100[120.0 <= data$C5801100 & data$C5801100 <= 131.0] <- 120.0
  data$C5801100[132.0 <= data$C5801100 & data$C5801100 <= 143.0] <- 132.0
  data$C5801100[144.0 <= data$C5801100 & data$C5801100 <= 155.0] <- 144.0
  data$C5801100[156.0 <= data$C5801100 & data$C5801100 <= 167.0] <- 156.0
  data$C5801100[168.0 <= data$C5801100 & data$C5801100 <= 179.0] <- 168.0
  data$C5801100[180.0 <= data$C5801100 & data$C5801100 <= 191.0] <- 180.0
  data$C5801100[192.0 <= data$C5801100 & data$C5801100 <= 999.0] <- 192.0
  data$C5801100 <- factor(data$C5801100,
                          levels=c(0.0,12.0,24.0,36.0,48.0,60.0,72.0,84.0,96.0,108.0,120.0,132.0,144.0,156.0,168.0,180.0,192.0),
                          labels=c("0 TO 11: 0-11 MONTHS",
                                   "12 TO 23: 12-23 MONTHS",
                                   "24 TO 35: 24-35 MONTHS",
                                   "36 TO 47: 36-47 MONTHS",
                                   "48 TO 59: 48-59 MONTHS",
                                   "60 TO 71: 60-71 MONTHS",
                                   "72 TO 83: 72-83 MONTHS",
                                   "84 TO 95: 84-95 MONTHS",
                                   "96 TO 107: 96-107 MONTHS",
                                   "108 TO 119: 108-119 MONTHS",
                                   "120 TO 131: 120-131 MONTHS",
                                   "132 TO 143: 132-143 MONTHS",
                                   "144 TO 155: 144-155 MONTHS",
                                   "156 TO 167: 156-167 MONTHS",
                                   "168 TO 179: 168-179 MONTHS",
                                   "180 TO 191: 180-191 MONTHS",
                                   "192 TO 999: 192 + MONTHS"))
  data$C5812500[1.0 <= data$C5812500 & data$C5812500 <= 99.0] <- 1.0
  data$C5812500[100.0 <= data$C5812500 & data$C5812500 <= 199.0] <- 100.0
  data$C5812500[200.0 <= data$C5812500 & data$C5812500 <= 299.0] <- 200.0
  data$C5812500[300.0 <= data$C5812500 & data$C5812500 <= 399.0] <- 300.0
  data$C5812500[400.0 <= data$C5812500 & data$C5812500 <= 499.0] <- 400.0
  data$C5812500[500.0 <= data$C5812500 & data$C5812500 <= 599.0] <- 500.0
  data$C5812500[600.0 <= data$C5812500 & data$C5812500 <= 699.0] <- 600.0
  data$C5812500[700.0 <= data$C5812500 & data$C5812500 <= 799.0] <- 700.0
  data$C5812500[800.0 <= data$C5812500 & data$C5812500 <= 899.0] <- 800.0
  data$C5812500[900.0 <= data$C5812500 & data$C5812500 <= 999.0] <- 900.0
  data$C5812500[1000.0 <= data$C5812500 & data$C5812500 <= 9.9999999E7] <- 1000.0
  data$C5812500 <- factor(data$C5812500,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999: 1000+"))
  data$C5813600[0.0 <= data$C5813600 & data$C5813600 <= 69.0] <- 0.0
  data$C5813600[70.0 <= data$C5813600 & data$C5813600 <= 84.0] <- 70.0
  data$C5813600[85.0 <= data$C5813600 & data$C5813600 <= 99.0] <- 85.0
  data$C5813600[100.0 <= data$C5813600 & data$C5813600 <= 114.0] <- 100.0
  data$C5813600[115.0 <= data$C5813600 & data$C5813600 <= 129.0] <- 115.0
  data$C5813600[130.0 <= data$C5813600 & data$C5813600 <= 999999.0] <- 130.0
  data$C5813600 <- factor(data$C5813600,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C5813800[0.0 <= data$C5813800 & data$C5813800 <= 69.0] <- 0.0
  data$C5813800[70.0 <= data$C5813800 & data$C5813800 <= 84.0] <- 70.0
  data$C5813800[85.0 <= data$C5813800 & data$C5813800 <= 99.0] <- 85.0
  data$C5813800[100.0 <= data$C5813800 & data$C5813800 <= 114.0] <- 100.0
  data$C5813800[115.0 <= data$C5813800 & data$C5813800 <= 129.0] <- 115.0
  data$C5813800[130.0 <= data$C5813800 & data$C5813800 <= 999999.0] <- 130.0
  data$C5813800 <- factor(data$C5813800,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$S0065500[0.0 <= data$S0065500 & data$S0065500 <= 9.0] <- 0.0
  data$S0065500[10.0 <= data$S0065500 & data$S0065500 <= 19.0] <- 10.0
  data$S0065500[20.0 <= data$S0065500 & data$S0065500 <= 29.0] <- 20.0
  data$S0065500[30.0 <= data$S0065500 & data$S0065500 <= 39.0] <- 30.0
  data$S0065500[40.0 <= data$S0065500 & data$S0065500 <= 49.0] <- 40.0
  data$S0065500[50.0 <= data$S0065500 & data$S0065500 <= 59.0] <- 50.0
  data$S0065500[60.0 <= data$S0065500 & data$S0065500 <= 69.0] <- 60.0
  data$S0065500[70.0 <= data$S0065500 & data$S0065500 <= 79.0] <- 70.0
  data$S0065500[80.0 <= data$S0065500 & data$S0065500 <= 89.0] <- 80.0
  data$S0065500[90.0 <= data$S0065500 & data$S0065500 <= 100.0] <- 90.0
  data$S0065500 <- factor(data$S0065500,
                          levels=c(0.0,10.0,20.0,30.0,40.0,50.0,60.0,70.0,80.0,90.0),
                          labels=c("0 TO 9: PERCENT",
                                   "10 TO 19: PERCENT",
                                   "20 TO 29: PERCENT",
                                   "30 TO 39: PERCENT",
                                   "40 TO 49: PERCENT",
                                   "50 TO 59: PERCENT",
                                   "60 TO 69: PERCENT",
                                   "70 TO 79: PERCENT",
                                   "80 TO 89: PERCENT",
                                   "90 TO 100: PERCENT"))
  data$S0065600[0.0 <= data$S0065600 & data$S0065600 <= 9.0] <- 0.0
  data$S0065600[10.0 <= data$S0065600 & data$S0065600 <= 19.0] <- 10.0
  data$S0065600[20.0 <= data$S0065600 & data$S0065600 <= 29.0] <- 20.0
  data$S0065600[30.0 <= data$S0065600 & data$S0065600 <= 39.0] <- 30.0
  data$S0065600[40.0 <= data$S0065600 & data$S0065600 <= 49.0] <- 40.0
  data$S0065600[50.0 <= data$S0065600 & data$S0065600 <= 59.0] <- 50.0
  data$S0065600[60.0 <= data$S0065600 & data$S0065600 <= 69.0] <- 60.0
  data$S0065600[70.0 <= data$S0065600 & data$S0065600 <= 79.0] <- 70.0
  data$S0065600[80.0 <= data$S0065600 & data$S0065600 <= 89.0] <- 80.0
  data$S0065600[90.0 <= data$S0065600 & data$S0065600 <= 100.0] <- 90.0
  data$S0065600 <- factor(data$S0065600,
                          levels=c(0.0,10.0,20.0,30.0,40.0,50.0,60.0,70.0,80.0,90.0),
                          labels=c("0 TO 9: PERCENT",
                                   "10 TO 19: PERCENT",
                                   "20 TO 29: PERCENT",
                                   "30 TO 39: PERCENT",
                                   "40 TO 49: PERCENT",
                                   "50 TO 59: PERCENT",
                                   "60 TO 69: PERCENT",
                                   "70 TO 79: PERCENT",
                                   "80 TO 89: PERCENT",
                                   "90 TO 100: PERCENT"))
  data$S0065700[0.0 <= data$S0065700 & data$S0065700 <= 9.0] <- 0.0
  data$S0065700[10.0 <= data$S0065700 & data$S0065700 <= 19.0] <- 10.0
  data$S0065700[20.0 <= data$S0065700 & data$S0065700 <= 29.0] <- 20.0
  data$S0065700[30.0 <= data$S0065700 & data$S0065700 <= 39.0] <- 30.0
  data$S0065700[40.0 <= data$S0065700 & data$S0065700 <= 49.0] <- 40.0
  data$S0065700[50.0 <= data$S0065700 & data$S0065700 <= 59.0] <- 50.0
  data$S0065700[60.0 <= data$S0065700 & data$S0065700 <= 69.0] <- 60.0
  data$S0065700[70.0 <= data$S0065700 & data$S0065700 <= 79.0] <- 70.0
  data$S0065700[80.0 <= data$S0065700 & data$S0065700 <= 89.0] <- 80.0
  data$S0065700[90.0 <= data$S0065700 & data$S0065700 <= 100.0] <- 90.0
  data$S0065700 <- factor(data$S0065700,
                          levels=c(0.0,10.0,20.0,30.0,40.0,50.0,60.0,70.0,80.0,90.0),
                          labels=c("0 TO 9: PERCENT",
                                   "10 TO 19: PERCENT",
                                   "20 TO 29: PERCENT",
                                   "30 TO 39: PERCENT",
                                   "40 TO 49: PERCENT",
                                   "50 TO 59: PERCENT",
                                   "60 TO 69: PERCENT",
                                   "70 TO 79: PERCENT",
                                   "80 TO 89: PERCENT",
                                   "90 TO 100: PERCENT"))
  return(data)
}

varlabels <- c("ID CODE OF CHILD",
               "ID CODE OF MOTHER OF CHILD",
               "RACE OF CHILD (FROM MOTHERS SCREENER 79)",
               "SEX OF CHILD",
               "DATE OF BIRTH OF CHILD - YEAR",
               "BIRTH ORDER OF CHILD",
               "CHILD AGE AT CHILD SUPP ASSESS DATE 86",
               "CHILD AGE AT CHILD SUPP ASSESS DATE 88",
               "AGE OF MOTHER AT BIRTH OF CHILD",
               "CHILD AGE AT CHILD SUPP ASSESS DATE 90",
               "CHILD AGE AT CHILD SUPP ASSESS DATE 92",
               "CHILD AGE AT CHILD SUPP ASSESS DATE 94",
               "CHILD AGE AT CHILD SUPP ASSESS DATE 96",
               "CHILD AGE AT CHILD SUPP ASSESS DATE 1998",
               "CHILD AGE AT CHILD SUPP ASSESS DATE 2000",
               "CHILD AGE AT CHILD SUPP ASSESS DATE 2002",
               "CHILD AGE AT CHILD SUPP ASSESS DATE 2004",
               "CHILD AGE AT CHILD SUPP ASSESS DATE 2006",
               "USUAL RESIDENCE OF CHILD 86",
               "USUAL RESIDENCE OF CHILD 88",
               "USUAL RESIDENCE OF CHILD 90",
               "USUAL RESIDENCE OF CHILD 92",
               "USUAL RESIDENCE OF CHILD 94",
               "USUAL RESIDENCE OF CHILD 96",
               "USUAL RESIDENCE OF CHILD 1998",
               "USUAL RESIDENCE OF CHILD 2000",
               "USUAL RESIDENCE OF CHILD 2002",
               "USUAL RESIDENCE OF CHILD 2004",
               "USUAL RESIDENCE OF CHILD 2006",
               "HGC OF MOTHER AS OF MAY 1 (REV)",
               "HGC OF MOTHER AS OF MAY 1 (REV)",
               "HGC OF MOTHER AS OF MAY 1 (REV)",
               "HGC OF MOTHER AS OF MAY 1 (REV)",
               "HGC OF MOTHER AS OF MAY 1 (REV)",
               "HGC OF MOTHER AS OF MAY 1 (REV)",
               "HGC OF MOTHER AS OF MAY 1 (REV) 1998",
               "HGC OF MOTHER AS OF MAY 1 (REV) 2000",
               "HGC OF MOTHER AS OF MAY 1 (REV) 2002",
               "HGC OF MOTHER AS OF MAY 1 (REV) 2004",
               "HGC OF MOTHER AS OF MAY 1 (REV) 2006",
               "USUAL RESIDENCE OF CHILD",
               "HOME: COG STIM STANDARD SCORE",
               "PIAT MATH: TOTAL STD SCORE 86",
               "PIAT READ REC: TOTAL STD SCORE 86",
               "HOME: COG STIM STANDARD SCORE",
               "PIAT MATH: TOTAL STD SCORE 88",
               "PIAT READ REC: TOTAL STD SCORE 88",
               "HOME: COG STIM STANDARD SCORE",
               "PIAT MATH: TOTAL STD SCORE 90",
               "PIAT READ REC: TOTAL STD SCORE 90",
               "HOME: COG STIM STANDARD SCORE",
               "PIAT MATH: TOTAL STANDARD SCORE - 92",
               "PIAT READ REC: TOTAL STNDRD SCORE - 92",
               "HOME: COG STIM STANDARD SCORE",
               "PIAT MATH: TOTAL STANDARD SCORE - 94",
               "PIAT READ REC: TOTAL STNDRD SCORE - 94",
               "HOME: COG STIM STANDARD SCORE",
               "PIAT MATH: TOTAL STANDARD SCORE 96",
               "PIAT READ REC: TOTAL STNDRD SCORE 96",
               "HOME: COG STIM STANDARD SCORE 1998",
               "PIAT MATH: TOTAL STANDARD SCORE 1998",
               "PIAT READ REC: TOTAL STNDRD SCORE 1998",
               "HOME: COG STIM STANDARD SCORE 2000",
               "PIAT MATH: TOTAL STANDARD SCORE 2000",
               "PIAT READ REC: TOTAL STNDRD SCORE 2000",
               "HOME: COG STIM STANDARD SCORE 2002",
               "PIAT MATH: TOTAL STANDARD SCORE 2002",
               "PIAT READ REC: TOTAL STNDRD SCORE 2002",
               "HOME: COG STIM STANDARD SCORE 2004",
               "PIAT MATH: TOTAL STANDARD SCORE 2004",
               "PIAT READ REC: TOTAL STNDRD SCORE 2004",
               "HOME: COG STIM STANDARD SCORE 2006",
               "PIAT MATH: TOTAL STANDARD SCORE 2006",
               "PIAT READ REC: TOTAL STNDRD SCORE 2006",
               "CHILD AGE AT CHILD SUPP ASSESS DATE 2008",
               "USUAL RESIDENCE OF CHILD 2008",
               "HGC OF MOTHER AS OF MAY 1 (REV) 2008",
               "HOME: COG STIM STANDARD SCORE 2008",
               "PIAT MATH: TOTAL STANDARD SCORE 2008",
               "PIAT READ REC: TOTAL STNDRD SCORE 2008",
               "CHILD AGE AT CHILD SUPP ASSESS DATE 2010",
               "USUAL RESIDENCE OF CHILD 2010",
               "HGC OF MOTHER AS OF MAY 1 (REV) 2010",
               "HOME: COG STIM STANDARD SCORE 2010",
               "PIAT MATH: TOTAL STANDARD SCORE 2010",
               "PIAT READ REC: TOTAL STNDRD SCORE 2010",
               "CHILD AGE AT CHILD SUPP ASSESS DATE 2012",
               "USUAL RESIDENCE OF CHILD 2012",
               "HGC OF MOTHER AS OF MAY 1 (REV) 2012",
               "HOME: COG STIM STANDARD SCORE 2012",
               "PIAT MATH: TOTAL STANDARD SCORE 2012",
               "PIAT READ REC: TOTAL STNDRD SCORE 2012",
               "CHILD AGE AT CHILD SUPP ASSESS DATE 2014",
               "USUAL RESIDENCE OF CHILD 2014",
               "HGC OF MOTHER AS OF MAY 1 (REV) 2014",
               "HOME: COG STIM STANDARD SCORE 2014",
               "PIAT MATH: TOTAL STANDARD SCORE 2014",
               "PIAT READ REC: TOTAL STNDRD SCORE 2014",
               "TEST 1: READING SCORE-PERCENTILE RANK",
               "TEST 1: LANGUAGE SCORE-PERCENTILE RANK",
               "TEST 1: MATH TOTAL SCORE-PERCENTILE RANK",
               "VERSION_R29 CHILD/YOUNG ADULT XRND"
)


# Use qnames rather than rnums

qnames = function(data) {
  names(data) <- c("CPUBID_XRND",
                   "MPUBID_XRND",
                   "CRACE_XRND",
                   "CSEX_XRND",
                   "CYRB_XRND",
                   "BTHORDR_XRND",
                   "CSAGE1986_1986",
                   "CSAGE1988_1988",
                   "MAGEBIR_XRND",
                   "CSAGE1990_1990",
                   "CSAGE1992_1992",
                   "CSAGE1994_1994",
                   "CSAGE1996_1996",
                   "CSAGE1998_1998",
                   "CSAGE2000_2000",
                   "CSAGE2002_2002",
                   "CSAGE2004_2004",
                   "CSAGE2006_2006",
                   "CRES1986_1986",
                   "CRES1988_1988",
                   "CRES1990_1990",
                   "CRES1992_1992",
                   "CRES1994_1994",
                   "CRES1996_1996",
                   "CRES1998_1998",
                   "CRES2000_2000",
                   "CRES2002_2002",
                   "CRES2004_2004",
                   "CRES2006_2006",
                   "HGCREV1986_1986",
                   "HGCREV1988_1988",
                   "HGCREV1990_1990",
                   "HGCREV1992_1992",
                   "HGCREV1994_1994",
                   "HGCREV1996_1996",
                   "HGCREV1998_1998",
                   "HGCREV2000_2000",
                   "HGCREV2002_2002",
                   "HGCREV2004_2004",
                   "HGCREV2006_2006",
                   "CRES1993_1993",
                   "COGNZ1986_1986",
                   "MATHZ1986_1986",
                   "RECOGZ1986_1986",
                   "COGNZ1988_1988",
                   "MATHZ1988_1988",
                   "RECOGZ1988_1988",
                   "COGNZ1990_1990",
                   "MATHZ1990_1990",
                   "RECOGZ1990_1990",
                   "COGNZ1992_1992",
                   "MATHZ1992_1992",
                   "RECOGZ1992_1992",
                   "COGNZ1994_1994",
                   "MATHZ1994_1994",
                   "RECOGZ1994_1994",
                   "COGNZ1996_1996",
                   "MATHZ1996_1996",
                   "RECOGZ1996_1996",
                   "COGNZ1998_1998",
                   "MATHZ1998_1998",
                   "RECOGZ1998_1998",
                   "COGNZ2000_2000",
                   "MATHZ2000_2000",
                   "RECOGZ2000_2000",
                   "COGNZ2002_2002",
                   "MATHZ2002_2002",
                   "RECOGZ2002_2002",
                   "COGNZ2004_2004",
                   "MATHZ2004_2004",
                   "RECOGZ2004_2004",
                   "COGNZ2006_2006",
                   "MATHZ2006_2006",
                   "RECOGZ2006_2006",
                   "CSAGE2008_2008",
                   "CRES2008_2008",
                   "HGCREV2008_2008",
                   "COGNZ2008_2008",
                   "MATHZ2008_2008",
                   "RECOGZ2008_2008",
                   "CSAGE2010_2010",
                   "CRES2010_2010",
                   "HGCREV2010_2010",
                   "COGNZ2010_2010",
                   "MATHZ2010_2010",
                   "RECOGZ2010_2010",
                   "CSAGE2012_2012",
                   "CRES2012_2012",
                   "HGCREV2012_2012",
                   "COGNZ2012_2012",
                   "MATHZ2012_2012",
                   "RECOGZ2012_2012",
                   "CSAGE2014_2014",
                   "CRES2014_2014",
                   "HGCREV2014_2014",
                   "COGNZ2014_2014",
                   "MATHZ2014_2014",
                   "RECOGZ2014_2014",
                   "TREATOT1_1995",
                   "TLANTOT1_1995",
                   "TMATTOT1_1995",
                   "VERSION_R29_XRND")
  return(data)
}


#********************************************************************************************************

# Remove the '#' before the following line to create a data file called "categories" with value labels.
categories <- vallabels(new_data)

# Remove the '#' before the following lines to rename variables using Qnames instead of Reference Numbers
new_data <- qnames(new_data)
categories <- qnames(categories)

# Produce summaries for the raw (uncategorized) data file
summary(new_data)

# Rename to data

data<-new_data

#************************************************************************************************************


#######################################################
######### 1) LABEL VARIABLES IN THE LONG FORMAT ################
######################################################


############# TIME INVARIANT VARIABLES ##################

data$ID<-data$CPUBID_XRND
data$motherID<-data$MPUBID_XRND
data$race<-data$CRACE_XRND
data$sex<-data$CSEX_XRND
data$birthyear<-data$CYRB_XRND
data$birthorder<-data$BTHORDR_XRND
data$motherbirthage<-data$MAGEBIR_XRND

data$readingtest<-data$TREATOT1_1995
data$languagetest<-data$TLANTOT1_1995
data$mathtest<-data$TMATTOT1_1995

############# TIME VARIANT VARIABLES ##################

#### COGNITIVE STIMULATION STANDARD SCORES ###

data$cognz_1986<-data$COGNZ1986_1986
data$cognz_1988<-data$COGNZ1988_1988
data$cognz_1990<-data$COGNZ1990_1990
data$cognz_1992<-data$COGNZ1992_1992
data$cognz_1994<-data$COGNZ1994_1994
data$cognz_1996<-data$COGNZ1996_1996
data$cognz_1998<-data$COGNZ1998_1998
data$cognz_2000<-data$COGNZ2000_2000
data$cognz_2002<-data$COGNZ2002_2002
data$cognz_2004<-data$COGNZ2004_2004
data$cognz_2006<-data$COGNZ2006_2006
data$cognz_2008<-data$COGNZ2008_2008
data$cognz_2010<-data$COGNZ2010_2010
data$cognz_2012<-data$COGNZ2012_2012
data$cognz_2014<-data$COGNZ2014_2014



#### PIAT MATH STD SCORES ###########

data$mathz_1986<-data$MATHZ1986_1986
data$mathz_1988<-data$MATHZ1988_1988
data$mathz_1990<-data$MATHZ1990_1990
data$mathz_1992<-data$MATHZ1992_1992
data$mathz_1994<-data$MATHZ1994_1994
data$mathz_1996<-data$MATHZ1996_1996
data$mathz_1998<-data$MATHZ1998_1998
data$mathz_2000<-data$MATHZ2000_2000
data$mathz_2002<-data$MATHZ2002_2002
data$mathz_2004<-data$MATHZ2004_2004
data$mathz_2006<-data$MATHZ2006_2006
data$mathz_2008<-data$MATHZ2008_2008
data$mathz_2010<-data$MATHZ2010_2010
data$mathz_2012<-data$MATHZ2012_2012
data$mathz_2014<-data$MATHZ2014_2014

#### PIAT READING REGONITION STD SCORES ######

data$recoz_1986<-data$RECOGZ1986_1986
data$recoz_1988<-data$RECOGZ1988_1988
data$recoz_1990<-data$RECOGZ1990_1990
data$recoz_1992<-data$RECOGZ1992_1992
data$recoz_1994<-data$RECOGZ1994_1994
data$recoz_1996<-data$RECOGZ1996_1996
data$recoz_1998<-data$RECOGZ1998_1998
data$recoz_2000<-data$RECOGZ2000_2000
data$recoz_2002<-data$RECOGZ2002_2002
data$recoz_2004<-data$RECOGZ2004_2004
data$recoz_2006<-data$RECOGZ2006_2006
data$recoz_2008<-data$RECOGZ2008_2008
data$recoz_2010<-data$RECOGZ2010_2010
data$recoz_2012<-data$RECOGZ2012_2012
data$recoz_2014<-data$RECOGZ2014_2014


### RESIDENCE ###

data$residence_1986<-data$CRES1986_1986
data$residence_1988<-data$CRES1988_1988
data$residence_1990<-data$CRES1990_1990
data$residence_1992<-data$CRES1992_1992
data$residence_1994<-data$CRES1994_1994
data$residence_1996<-data$CRES1996_1996
data$residence_1998<-data$CRES1998_1998
data$residence_2000<-data$CRES2000_2000
data$residence_2002<-data$CRES2002_2002
data$residence_2004<-data$CRES2004_2004
data$residence_2006<-data$CRES2006_2006
data$residence_2008<-data$CRES2008_2008
data$residence_2010<-data$CRES2010_2010
data$residence_2012<-data$CRES2012_2012
data$residence_2014<-data$CRES2014_2014

### EDUCATION ###

data$education_1986<-data$HGCREV1986_1986
data$education_1988<-data$HGCREV1988_1988
data$education_1990<-data$HGCREV1990_1990
data$education_1992<-data$HGCREV1992_1992
data$education_1994<-NA
data$education_1996<-data$HGCREV1996_1996
data$education_1998<-data$HGCREV1998_1998
data$education_2000<-data$HGCREV2000_2000
data$education_2002<-data$HGCREV2002_2002
data$education_2004<-data$HGCREV2004_2004
data$education_2006<-data$HGCREV2006_2006
data$education_2008<-data$HGCREV2008_2008
data$education_2010<-data$HGCREV2010_2010
data$education_2012<-data$HGCREV2012_2012
data$education_2014<-data$HGCREV2014_2014

### AGE OF THE CHILD ###


data$ageatint_1986<-data$CSAGE1986_1986
data$ageatint_1988<-data$CSAGE1988_1988
data$ageatint_1990<-data$CSAGE1990_1990
data$ageatint_1992<-data$CSAGE1992_1992
data$ageatint_1994<-data$CSAGE1994_1994
data$ageatint_1996<-data$CSAGE1996_1996
data$ageatint_1998<-data$CSAGE1998_1998
data$ageatint_2000<-data$CSAGE2000_2000
data$ageatint_2002<-data$CSAGE2002_2002
data$ageatint_2004<-data$CSAGE2004_2004
data$ageatint_2006<-data$CSAGE2006_2006
data$ageatint_2008<-data$CSAGE2008_2008
data$ageatint_2010<-data$CSAGE2010_2010
data$ageatint_2012<-data$CSAGE2012_2012
data$ageatint_2014<-data$CSAGE2014_2014


#######################################################
######### 2) EXTRACTING VARIABLES ################
######################################################


myvars <- c("ID", "motherID","race", "sex",
            "birthyear", "birthorder", "motherbirthage",
            "mathtest", "languagetest", "readingtest",
            "cognz_1986", "cognz_1988", "cognz_1990",
            "cognz_1992", "cognz_1994", "cognz_1996",
            "cognz_1998", "cognz_2000", "cognz_2002",
            "cognz_2004", "cognz_2006", "cognz_2008",
            "cognz_2010", "cognz_2012", "cognz_2014",
            "mathz_1986", "mathz_1988", "mathz_1990",
            "mathz_1992", "mathz_1994", "mathz_1996",
            "mathz_1998", "mathz_2000", "mathz_2002",
            "mathz_2004", "mathz_2006", "mathz_2008",
            "mathz_2010", "mathz_2012", "mathz_2014",
            "recoz_1986", "recoz_1988", "recoz_1990",
            "recoz_1992", "recoz_1994", "recoz_1996",
            "recoz_1998", "recoz_2000", "recoz_2002",
            "recoz_2004", "recoz_2006", "recoz_2008",
            "recoz_2010", "recoz_2012", "recoz_2014",
            "ageatint_1986", "ageatint_1988", "ageatint_1990",
            "ageatint_1992", "ageatint_1994", "ageatint_1996",
            "ageatint_1998", "ageatint_2000", "ageatint_2002",
            "ageatint_2004", "ageatint_2006", "ageatint_2008",
            "ageatint_2010", "ageatint_2012", "ageatint_2014",
            "residence_1986", "residence_1988", "residence_1990",
            "residence_1992", "residence_1994", "residence_1996",
            "residence_1998", "residence_2000", "residence_2002",
            "residence_2004", "residence_2006", "residence_2008",
            "residence_2010", "residence_2012", "residence_2014",
            "education_1986", "education_1988", "education_1990",
            "education_1992", "education_1994", "education_1996",
            "education_1998", "education_2000", "education_2002",
            "education_2004", "education_2006", "education_2008",
            "education_2010", "education_2012", "education_2014")
wide <- data[myvars]


#######################################################
######### 3) MERGE WITH MOTHERS INFO ################
######################################################


# Set working directory
# setwd()


new_data <- read.table('LEARNINGmothers.dat', sep=' ')
names(new_data) <- c('R0000100',
                     'R0173600',
                     'R0214700',
                     'R0214800',
                     'R2257410',
                     'R2257500',
                     'R2257700',
                     'R2257800',
                     'R2257901',
                     'R2444610',
                     'R2445100',
                     'R2445200',
                     'R2445301',
                     'R2870110',
                     'R2870200',
                     'R2870600',
                     'R2870800',
                     'R2871000',
                     'R3073910',
                     'R3074300',
                     'R3074500',
                     'R3074700',
                     'R3400600',
                     'R3400700',
                     'R3401000',
                     'R3401200',
                     'R3401400',
                     'R3656000',
                     'R3656400',
                     'R3656600',
                     'R3656800',
                     'R4006500',
                     'R4006600',
                     'R4006900',
                     'R4007100',
                     'R4007300',
                     'R4417600',
                     'R4418000',
                     'R4418200',
                     'R4418400',
                     'R5080600',
                     'R5080700',
                     'R5081000',
                     'R5081200',
                     'R5081400',
                     'R5165900',
                     'R5166300',
                     'R5166500',
                     'R5166700',
                     'R6478600',
                     'R6478700',
                     'R6478900',
                     'R6479100',
                     'R6479300',
                     'R7006400',
                     'R7006500',
                     'R7006800',
                     'R7007000',
                     'R7703600',
                     'R7703700',
                     'R7704100',
                     'R7704300',
                     'R8496000',
                     'R8496100',
                     'R8496500',
                     'R8496700',
                     'T0987600',
                     'T0987800',
                     'T0988100',
                     'T0988300',
                     'T0988500',
                     'T2209900',
                     'T2210000',
                     'T2210300',
                     'T2210500',
                     'T3107700',
                     'T3107800',
                     'T3108200',
                     'T3108400',
                     'T4112200',
                     'T4112300',
                     'T4112700',
                     'T4112900',
                     'T5022500',
                     'T5022600',
                     'T5023100',
                     'T5023300',
                     'T9900000')


# Handle missing values

new_data[new_data == -1] = NA  # Refused
new_data[new_data == -2] = NA  # Dont know
new_data[new_data == -3] = NA  # Invalid missing
new_data[new_data == -4] = NA  # Valid missing
new_data[new_data == -5] = NA  # Non-interview


# If there are values not categorized they will be represented as NA

vallabels = function(data) {
  data$R0173600 <- factor(data$R0173600,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0),
                          labels=c("CROSS MALE WHITE",
                                   "CROSS MALE WH. POOR",
                                   "CROSS MALE BLACK",
                                   "CROSS MALE HISPANIC",
                                   "CROSS FEMALE WHITE",
                                   "CROSS FEMALE WH POOR",
                                   "CROSS FEMALE BLACK",
                                   "CROSS FEMALE HISPANIC",
                                   "SUP MALE WH POOR",
                                   "SUP MALE BLACK",
                                   "SUP MALE HISPANIC",
                                   "SUP FEM WH POOR",
                                   "SUP FEMALE BLACK",
                                   "SUP FEMALE HISPANIC",
                                   "MIL MALE WHITE",
                                   "MIL MALE BLACK",
                                   "MIL MALE HISPANIC",
                                   "MIL FEMALE WHITE",
                                   "MIL FEMALE BLACK",
                                   "MIL FEMALE HISPANIC"))
  data$R0214700 <- factor(data$R0214700,
                          levels=c(1.0,2.0,3.0),
                          labels=c("HISPANIC",
                                   "BLACK",
                                   "NON-BLACK, NON-HISPANIC"))
  data$R0214800 <- factor(data$R0214800,
                          levels=c(1.0,2.0),
                          labels=c("MALE",
                                   "FEMALE"))
  data$R2257700 <- factor(data$R2257700,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0),
                          labels=c("WORKING",
                                   "WITH JOB NOT AT WORK",
                                   "UNEMPLOYED",
                                   "KEEPING HOUSE",
                                   "GOING TO SCHOOL",
                                   "UNABLE TO WORK",
                                   "OTHER",
                                   "IN ACTIVE FORCES"))
  data$R2257800 <- factor(data$R2257800,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("NORTHEAST",
                                   "NORTH CENTRAL",
                                   "SOUTH",
                                   "WEST"))
  data$R2257901 <- factor(data$R2257901,
                          levels=c(0.0,1.0,2.0,3.0,5.0,6.0),
                          labels=c("NEVER MARRIED",
                                   "MARRIED",
                                   "SEPARATED",
                                   "DIVORCED",
                                   "REMARRIED",
                                   "WIDOWED"))
  data$R2445100 <- factor(data$R2445100,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0),
                          labels=c("WORKING",
                                   "WITH JOB NOT AT WORK",
                                   "UNEMPLOYED",
                                   "KEEPING HOUSE",
                                   "GOING TO SCHOOL",
                                   "UNABLE TO WORK",
                                   "OTHER",
                                   "IN ACTIVE FORCES"))
  data$R2445200 <- factor(data$R2445200,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("NORTHEAST",
                                   "NORTH CENTRAL",
                                   "SOUTH",
                                   "WEST"))
  data$R2445301 <- factor(data$R2445301,
                          levels=c(0.0,1.0,2.0,3.0,5.0,6.0),
                          labels=c("NEVER MARRIED",
                                   "MARRIED",
                                   "SEPARATED",
                                   "DIVORCED",
                                   "REMARRIED",
                                   "WIDOWED"))
  data$R2870600 <- factor(data$R2870600,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0),
                          labels=c("WORKING",
                                   "WITH JOB NOT AT WORK",
                                   "UNEMPLOYED",
                                   "KEEPING HOUSE",
                                   "GOING TO SCHOOL",
                                   "UNABLE TO WORK",
                                   "OTHER",
                                   "IN ACTIVE FORCES"))
  data$R2870800 <- factor(data$R2870800,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("NORTHEAST",
                                   "NORTH CENTRAL",
                                   "SOUTH",
                                   "WEST"))
  data$R2871000 <- factor(data$R2871000,
                          levels=c(0.0,1.0,2.0,3.0,5.0,6.0),
                          labels=c("NEVER MARRIED",
                                   "MARRIED",
                                   "SEPARATED",
                                   "DIVORCED",
                                   "REMARRIED",
                                   "WIDOWED"))
  data$R3074300 <- factor(data$R3074300,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0),
                          labels=c("WORKING",
                                   "WITH JOB NOT AT WORK",
                                   "UNEMPLOYED",
                                   "KEEPING HOUSE",
                                   "GOING TO SCHOOL",
                                   "UNABLE TO WORK",
                                   "OTHER",
                                   "IN ACTIVE FORCES"))
  data$R3074500 <- factor(data$R3074500,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("NORTHEAST",
                                   "NORTH CENTRAL",
                                   "SOUTH",
                                   "WEST"))
  data$R3074700 <- factor(data$R3074700,
                          levels=c(0.0,1.0,2.0,3.0,5.0,6.0),
                          labels=c("NEVER MARRIED",
                                   "MARRIED",
                                   "SEPARATED",
                                   "DIVORCED",
                                   "REMARRIED",
                                   "WIDOWED"))
  data$R3401000 <- factor(data$R3401000,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0),
                          labels=c("WORKING",
                                   "WITH JOB NOT AT WORK",
                                   "UNEMPLOYED",
                                   "KEEPING HOUSE",
                                   "GOING TO SCHOOL",
                                   "UNABLE TO WORK",
                                   "OTHER",
                                   "IN ACTIVE FORCES"))
  data$R3401200 <- factor(data$R3401200,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("NORTHEAST",
                                   "NORTH CENTRAL",
                                   "SOUTH",
                                   "WEST"))
  data$R3401400 <- factor(data$R3401400,
                          levels=c(0.0,1.0,2.0,3.0,5.0,6.0),
                          labels=c("NEVER MARRIED",
                                   "MARRIED",
                                   "SEPARATED",
                                   "DIVORCED",
                                   "REMARRIED",
                                   "WIDOWED"))
  data$R3656400 <- factor(data$R3656400,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0),
                          labels=c("WORKING",
                                   "WITH JOB NOT AT WORK",
                                   "UNEMPLOYED",
                                   "KEEPING HOUSE",
                                   "GOING TO SCHOOL",
                                   "UNABLE TO WORK",
                                   "OTHER",
                                   "IN ACTIVE FORCES"))
  data$R3656600 <- factor(data$R3656600,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("NORTHEAST",
                                   "NORTH CENTRAL",
                                   "SOUTH",
                                   "WEST"))
  data$R3656800 <- factor(data$R3656800,
                          levels=c(0.0,1.0,2.0,3.0,5.0,6.0),
                          labels=c("NEVER MARRIED",
                                   "MARRIED",
                                   "SEPARATED",
                                   "DIVORCED",
                                   "REMARRIED",
                                   "WIDOWED"))
  data$R4006900 <- factor(data$R4006900,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0),
                          labels=c("WORKING",
                                   "WITH JOB NOT AT WORK",
                                   "UNEMPLOYED",
                                   "KEEPING HOUSE",
                                   "GOING TO SCHOOL",
                                   "UNABLE TO WORK",
                                   "OTHER",
                                   "IN ACTIVE FORCES"))
  data$R4007100 <- factor(data$R4007100,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("NORTHEAST",
                                   "NORTH CENTRAL",
                                   "SOUTH",
                                   "WEST"))
  data$R4007300 <- factor(data$R4007300,
                          levels=c(0.0,1.0,2.0,3.0,5.0,6.0),
                          labels=c("NEVER MARRIED",
                                   "MARRIED",
                                   "SEPARATED",
                                   "DIVORCED",
                                   "REMARRIED",
                                   "WIDOWED"))
  data$R4418000 <- factor(data$R4418000,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0),
                          labels=c("WORKING",
                                   "WITH JOB NOT AT WORK",
                                   "UNEMPLOYED",
                                   "KEEPING HOUSE",
                                   "GOING TO SCHOOL",
                                   "UNABLE TO WORK",
                                   "OTHER",
                                   "IN ACTIVE FORCES"))
  data$R4418200 <- factor(data$R4418200,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("NORTHEAST",
                                   "NORTH CENTRAL",
                                   "SOUTH",
                                   "WEST"))
  data$R4418400 <- factor(data$R4418400,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("NEVER MARRIED",
                                   "MARRIED",
                                   "SEPARATED",
                                   "DIVORCED",
                                   "WIDOWED"))
  data$R5081000 <- factor(data$R5081000,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0),
                          labels=c("1: 1  EMPLOYED",
                                   "2: 2  EMPLOYED - ABSENT FROM JOB",
                                   "3: 3  UNEMPLOYED - ON LAYOFF",
                                   "4: 4  UNEMPLOYED - LOOKING FOR WORK",
                                   "5: 5  NOT IN LABOR FORCE - RETIRED",
                                   "6: 6  NOT IN LABOR FORCE - DISABLED",
                                   "7: 7  NOT IN LABOR FORCE - OTHER",
                                   "8: 8  IN ACTIVE FORCES"))
  data$R5081200 <- factor(data$R5081200,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("1: 1  NORTHEAST",
                                   "2: 2  NORTH CENTRAL",
                                   "3: 3  SOUTH",
                                   "4: 4  WEST"))
  data$R5081400 <- factor(data$R5081400,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$R5166300 <- factor(data$R5166300,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0),
                          labels=c("1: EMPLOYED",
                                   "2: EMPLOYED - ABSENT FROM JOB",
                                   "3: UNEMPLOYED - ON LAYOFF",
                                   "4: UNEMPLOYED - LOOKING FOR WORK",
                                   "5: NOT IN LABOR FORCE - RETIRED",
                                   "6: NOT IN LABOR FORCE - DISABLED",
                                   "7: NOT IN LABOR FORCE - OTHER",
                                   "8: IN ACTIVE FORCES"))
  data$R5166500 <- factor(data$R5166500,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("1: NORTHEAST",
                                   "2: NORTH CENTRAL",
                                   "3: SOUTH",
                                   "4: WEST"))
  data$R5166700 <- factor(data$R5166700,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: NEVER MARRIED",
                                   "1: MARRIED",
                                   "2: SEPARATED",
                                   "3: DIVORCED",
                                   "6: WIDOWED"))
  data$R6478900 <- factor(data$R6478900,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0),
                          labels=c("1: 1  EMPLOYED",
                                   "2: 2  EMPLOYED - ABSENT FROM JOB",
                                   "3: 3  UNEMPLOYED - ON LAYOFF",
                                   "4: 4  UNEMPLOYED - LOOKING FOR WORK",
                                   "5: 5  NOT IN LABOR FORCE - RETIRED",
                                   "6: 6  NOT IN LABOR FORCE - DISABLED",
                                   "7: 7  NOT IN LABOR FORCE - OTHER",
                                   "8: 8  IN ACTIVE MILITARY FORCES"))
  data$R6479100 <- factor(data$R6479100,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("1: 1  NORTHEAST",
                                   "2: 2  NORTH CENTRAL",
                                   "3: 3  SOUTH",
                                   "4: 4  WEST"))
  data$R6479300 <- factor(data$R6479300,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("Never Married",
                                   "Married",
                                   "Separated",
                                   "Divorced",
                                   "Widowed"))
  data$R7006800 <- factor(data$R7006800,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("1: 1  NORTHEAST",
                                   "2: 2  NORTH CENTRAL",
                                   "3: 3  SOUTH",
                                   "4: 4  WEST"))
  data$R7007000 <- factor(data$R7007000,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("Never Married",
                                   "Married",
                                   "Separated",
                                   "Divorced",
                                   "Widowed"))
  data$R7704100 <- factor(data$R7704100,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("1: 1  NORTHEAST",
                                   "2: 2  NORTH CENTRAL",
                                   "3: 3  SOUTH",
                                   "4: 4  WEST"))
  data$R7704300 <- factor(data$R7704300,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$R8496500 <- factor(data$R8496500,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("1: 1  NORTHEAST",
                                   "2: 2  NORTH CENTRAL",
                                   "3: 3  SOUTH",
                                   "4: 4  WEST"))
  data$R8496700 <- factor(data$R8496700,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$T0988100 <- factor(data$T0988100,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0),
                          labels=c("1: EMPLOYED",
                                   "2: EMPLOYED - ABSENT FROM JOB",
                                   "3: UNEMPLOYED - ON LAYOFF",
                                   "4: UNEMPLOYED - LOOKING FOR WORK",
                                   "5: NOT IN LABOR FORCE - RETIRED",
                                   "6: NOT IN LABOR FORCE - DISABLED",
                                   "7: NOT IN LABOR FORCE - OTHER",
                                   "8: IN ACTIVE MILITARY FORCES"))
  data$T0988300 <- factor(data$T0988300,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("1: 1  NORTHEAST",
                                   "2: 2  NORTH CENTRAL",
                                   "3: 3  SOUTH",
                                   "4: 4  WEST"))
  data$T0988500 <- factor(data$T0988500,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$T2210300 <- factor(data$T2210300,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("1: NORTHEAST",
                                   "2: NORTH CENTRAL",
                                   "3: SOUTH",
                                   "4: WEST"))
  data$T2210500 <- factor(data$T2210500,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$T3108200 <- factor(data$T3108200,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("1: NORTHEAST",
                                   "2: NORTH CENTRAL",
                                   "3: SOUTH",
                                   "4: WEST"))
  data$T3108400 <- factor(data$T3108400,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$T4112700 <- factor(data$T4112700,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("1: NORTHEAST",
                                   "2: NORTH CENTRAL",
                                   "3: SOUTH",
                                   "4: WEST"))
  data$T4112900 <- factor(data$T4112900,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$T5023100 <- factor(data$T5023100,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("1: NORTHEAST",
                                   "2: NORTH CENTRAL",
                                   "3: SOUTH",
                                   "4: WEST"))
  data$T5023300 <- factor(data$T5023300,
                          levels=c(0.0,1.0,2.0,3.0,6.0),
                          labels=c("0: 0  NEVER MARRIED",
                                   "1: 1  MARRIED",
                                   "2: 2  SEPARATED",
                                   "3: 3  DIVORCED",
                                   "6: 6  WIDOWED"))
  data$T9900000 <- factor(data$T9900000,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,93.0,94.0,95.0),
                          labels=c("NONE",
                                   "1ST GRADE",
                                   "2ND GRADE",
                                   "3RD GRADE",
                                   "4TH GRADE",
                                   "5TH GRADE",
                                   "6TH GRADE",
                                   "7TH GRADE",
                                   "8TH GRADE",
                                   "9TH GRADE",
                                   "10TH GRADE",
                                   "11TH GRADE",
                                   "12TH GRADE",
                                   "1ST YEAR COLLEGE",
                                   "2ND YEAR COLLEGE",
                                   "3RD YEAR COLLEGE",
                                   "4TH YEAR COLLEGE",
                                   "5TH YEAR COLLEGE",
                                   "6TH YEAR COLLEGE",
                                   "7TH YEAR COLLEGE",
                                   "8TH YEAR COLLEGE OR MORE",
                                   "PRE-KINDERGARTEN",
                                   "KINDERGARTEN",
                                   "UNGRADED"))
  return(data)
}


# If there are values not categorized they will be represented as NA

vallabels_continuous = function(data) {
  data$R2257410[17.0 <= data$R2257410 & data$R2257410 <= 99999.0] <- 17.0
  data$R2257410 <- factor(data$R2257410,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0),
                          labels=c("0: < 1",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16",
                                   "17 TO 99999: 17+"))
  data$R2257500[1.0 <= data$R2257500 & data$R2257500 <= 999.0] <- 1.0
  data$R2257500[1000.0 <= data$R2257500 & data$R2257500 <= 1999.0] <- 1000.0
  data$R2257500[2000.0 <= data$R2257500 & data$R2257500 <= 2999.0] <- 2000.0
  data$R2257500[3000.0 <= data$R2257500 & data$R2257500 <= 3999.0] <- 3000.0
  data$R2257500[4000.0 <= data$R2257500 & data$R2257500 <= 4999.0] <- 4000.0
  data$R2257500[5000.0 <= data$R2257500 & data$R2257500 <= 5999.0] <- 5000.0
  data$R2257500[6000.0 <= data$R2257500 & data$R2257500 <= 6999.0] <- 6000.0
  data$R2257500[7000.0 <= data$R2257500 & data$R2257500 <= 7999.0] <- 7000.0
  data$R2257500[8000.0 <= data$R2257500 & data$R2257500 <= 8999.0] <- 8000.0
  data$R2257500[9000.0 <= data$R2257500 & data$R2257500 <= 9999.0] <- 9000.0
  data$R2257500[10000.0 <= data$R2257500 & data$R2257500 <= 14999.0] <- 10000.0
  data$R2257500[15000.0 <= data$R2257500 & data$R2257500 <= 19999.0] <- 15000.0
  data$R2257500[20000.0 <= data$R2257500 & data$R2257500 <= 24999.0] <- 20000.0
  data$R2257500[25000.0 <= data$R2257500 & data$R2257500 <= 49999.0] <- 25000.0
  data$R2257500[50000.0 <= data$R2257500 & data$R2257500 <= 9999999.0] <- 50000.0
  data$R2257500 <- factor(data$R2257500,
                          levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0,10000.0,15000.0,20000.0,25000.0,50000.0),
                          labels=c("0",
                                   "1 TO 999",
                                   "1000 TO 1999",
                                   "2000 TO 2999",
                                   "3000 TO 3999",
                                   "4000 TO 4999",
                                   "5000 TO 5999",
                                   "6000 TO 6999",
                                   "7000 TO 7999",
                                   "8000 TO 8999",
                                   "9000 TO 9999",
                                   "10000 TO 14999",
                                   "15000 TO 19999",
                                   "20000 TO 24999",
                                   "25000 TO 49999",
                                   "50000 TO 9999999: 50000+"))
  data$R2444610[17.0 <= data$R2444610 & data$R2444610 <= 99999.0] <- 17.0
  data$R2444610 <- factor(data$R2444610,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0),
                          labels=c("0: < 1",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16",
                                   "17 TO 99999: 17+"))
  data$R2870110[17.0 <= data$R2870110 & data$R2870110 <= 99999.0] <- 17.0
  data$R2870110 <- factor(data$R2870110,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0),
                          labels=c("0: < 1",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16",
                                   "17 TO 99999: 17+"))
  data$R2870200[1.0 <= data$R2870200 & data$R2870200 <= 999.0] <- 1.0
  data$R2870200[1000.0 <= data$R2870200 & data$R2870200 <= 1999.0] <- 1000.0
  data$R2870200[2000.0 <= data$R2870200 & data$R2870200 <= 2999.0] <- 2000.0
  data$R2870200[3000.0 <= data$R2870200 & data$R2870200 <= 3999.0] <- 3000.0
  data$R2870200[4000.0 <= data$R2870200 & data$R2870200 <= 4999.0] <- 4000.0
  data$R2870200[5000.0 <= data$R2870200 & data$R2870200 <= 5999.0] <- 5000.0
  data$R2870200[6000.0 <= data$R2870200 & data$R2870200 <= 6999.0] <- 6000.0
  data$R2870200[7000.0 <= data$R2870200 & data$R2870200 <= 7999.0] <- 7000.0
  data$R2870200[8000.0 <= data$R2870200 & data$R2870200 <= 8999.0] <- 8000.0
  data$R2870200[9000.0 <= data$R2870200 & data$R2870200 <= 9999.0] <- 9000.0
  data$R2870200[10000.0 <= data$R2870200 & data$R2870200 <= 14999.0] <- 10000.0
  data$R2870200[15000.0 <= data$R2870200 & data$R2870200 <= 19999.0] <- 15000.0
  data$R2870200[20000.0 <= data$R2870200 & data$R2870200 <= 24999.0] <- 20000.0
  data$R2870200[25000.0 <= data$R2870200 & data$R2870200 <= 49999.0] <- 25000.0
  data$R2870200[50000.0 <= data$R2870200 & data$R2870200 <= 9999999.0] <- 50000.0
  data$R2870200 <- factor(data$R2870200,
                          levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0,10000.0,15000.0,20000.0,25000.0,50000.0),
                          labels=c("0",
                                   "1 TO 999",
                                   "1000 TO 1999",
                                   "2000 TO 2999",
                                   "3000 TO 3999",
                                   "4000 TO 4999",
                                   "5000 TO 5999",
                                   "6000 TO 6999",
                                   "7000 TO 7999",
                                   "8000 TO 8999",
                                   "9000 TO 9999",
                                   "10000 TO 14999",
                                   "15000 TO 19999",
                                   "20000 TO 24999",
                                   "25000 TO 49999",
                                   "50000 TO 9999999: 50000+"))
  data$R3073910[17.0 <= data$R3073910 & data$R3073910 <= 99999.0] <- 17.0
  data$R3073910 <- factor(data$R3073910,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0),
                          labels=c("0: < 1",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16",
                                   "17 TO 99999: 17+"))
  data$R3400600[17.0 <= data$R3400600 & data$R3400600 <= 99999.0] <- 17.0
  data$R3400600 <- factor(data$R3400600,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0),
                          labels=c("0: < 1",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16",
                                   "17 TO 99999: 17+"))
  data$R3400700[1.0 <= data$R3400700 & data$R3400700 <= 999.0] <- 1.0
  data$R3400700[1000.0 <= data$R3400700 & data$R3400700 <= 1999.0] <- 1000.0
  data$R3400700[2000.0 <= data$R3400700 & data$R3400700 <= 2999.0] <- 2000.0
  data$R3400700[3000.0 <= data$R3400700 & data$R3400700 <= 3999.0] <- 3000.0
  data$R3400700[4000.0 <= data$R3400700 & data$R3400700 <= 4999.0] <- 4000.0
  data$R3400700[5000.0 <= data$R3400700 & data$R3400700 <= 5999.0] <- 5000.0
  data$R3400700[6000.0 <= data$R3400700 & data$R3400700 <= 6999.0] <- 6000.0
  data$R3400700[7000.0 <= data$R3400700 & data$R3400700 <= 7999.0] <- 7000.0
  data$R3400700[8000.0 <= data$R3400700 & data$R3400700 <= 8999.0] <- 8000.0
  data$R3400700[9000.0 <= data$R3400700 & data$R3400700 <= 9999.0] <- 9000.0
  data$R3400700[10000.0 <= data$R3400700 & data$R3400700 <= 14999.0] <- 10000.0
  data$R3400700[15000.0 <= data$R3400700 & data$R3400700 <= 19999.0] <- 15000.0
  data$R3400700[20000.0 <= data$R3400700 & data$R3400700 <= 24999.0] <- 20000.0
  data$R3400700[25000.0 <= data$R3400700 & data$R3400700 <= 49999.0] <- 25000.0
  data$R3400700[50000.0 <= data$R3400700 & data$R3400700 <= 9999999.0] <- 50000.0
  data$R3400700 <- factor(data$R3400700,
                          levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0,10000.0,15000.0,20000.0,25000.0,50000.0),
                          labels=c("0",
                                   "1 TO 999",
                                   "1000 TO 1999",
                                   "2000 TO 2999",
                                   "3000 TO 3999",
                                   "4000 TO 4999",
                                   "5000 TO 5999",
                                   "6000 TO 6999",
                                   "7000 TO 7999",
                                   "8000 TO 8999",
                                   "9000 TO 9999",
                                   "10000 TO 14999",
                                   "15000 TO 19999",
                                   "20000 TO 24999",
                                   "25000 TO 49999",
                                   "50000 TO 9999999: 50000+"))
  data$R3656000[17.0 <= data$R3656000 & data$R3656000 <= 99999.0] <- 17.0
  data$R3656000 <- factor(data$R3656000,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0),
                          labels=c("0: < 1",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16",
                                   "17 TO 99999: 17+"))
  data$R4006500[17.0 <= data$R4006500 & data$R4006500 <= 99999.0] <- 17.0
  data$R4006500 <- factor(data$R4006500,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0),
                          labels=c("0: < 1",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16",
                                   "17 TO 99999: 17+"))
  data$R4006600[1.0 <= data$R4006600 & data$R4006600 <= 999.0] <- 1.0
  data$R4006600[1000.0 <= data$R4006600 & data$R4006600 <= 1999.0] <- 1000.0
  data$R4006600[2000.0 <= data$R4006600 & data$R4006600 <= 2999.0] <- 2000.0
  data$R4006600[3000.0 <= data$R4006600 & data$R4006600 <= 3999.0] <- 3000.0
  data$R4006600[4000.0 <= data$R4006600 & data$R4006600 <= 4999.0] <- 4000.0
  data$R4006600[5000.0 <= data$R4006600 & data$R4006600 <= 5999.0] <- 5000.0
  data$R4006600[6000.0 <= data$R4006600 & data$R4006600 <= 6999.0] <- 6000.0
  data$R4006600[7000.0 <= data$R4006600 & data$R4006600 <= 7999.0] <- 7000.0
  data$R4006600[8000.0 <= data$R4006600 & data$R4006600 <= 8999.0] <- 8000.0
  data$R4006600[9000.0 <= data$R4006600 & data$R4006600 <= 9999.0] <- 9000.0
  data$R4006600[10000.0 <= data$R4006600 & data$R4006600 <= 14999.0] <- 10000.0
  data$R4006600[15000.0 <= data$R4006600 & data$R4006600 <= 19999.0] <- 15000.0
  data$R4006600[20000.0 <= data$R4006600 & data$R4006600 <= 24999.0] <- 20000.0
  data$R4006600[25000.0 <= data$R4006600 & data$R4006600 <= 49999.0] <- 25000.0
  data$R4006600[50000.0 <= data$R4006600 & data$R4006600 <= 9999999.0] <- 50000.0
  data$R4006600 <- factor(data$R4006600,
                          levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0,10000.0,15000.0,20000.0,25000.0,50000.0),
                          labels=c("0",
                                   "1 TO 999",
                                   "1000 TO 1999",
                                   "2000 TO 2999",
                                   "3000 TO 3999",
                                   "4000 TO 4999",
                                   "5000 TO 5999",
                                   "6000 TO 6999",
                                   "7000 TO 7999",
                                   "8000 TO 8999",
                                   "9000 TO 9999",
                                   "10000 TO 14999",
                                   "15000 TO 19999",
                                   "20000 TO 24999",
                                   "25000 TO 49999",
                                   "50000 TO 9999999: 50000+"))
  data$R4417600[16.0 <= data$R4417600 & data$R4417600 <= 99999.0] <- 16.0
  data$R4417600 <- factor(data$R4417600,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16 TO 99999: 16+"))
  data$R5080600[10.0 <= data$R5080600 & data$R5080600 <= 999.0] <- 10.0
  data$R5080600 <- factor(data$R5080600,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$R5080700[1.0 <= data$R5080700 & data$R5080700 <= 999.0] <- 1.0
  data$R5080700[1000.0 <= data$R5080700 & data$R5080700 <= 1999.0] <- 1000.0
  data$R5080700[2000.0 <= data$R5080700 & data$R5080700 <= 2999.0] <- 2000.0
  data$R5080700[3000.0 <= data$R5080700 & data$R5080700 <= 3999.0] <- 3000.0
  data$R5080700[4000.0 <= data$R5080700 & data$R5080700 <= 4999.0] <- 4000.0
  data$R5080700[5000.0 <= data$R5080700 & data$R5080700 <= 5999.0] <- 5000.0
  data$R5080700[6000.0 <= data$R5080700 & data$R5080700 <= 6999.0] <- 6000.0
  data$R5080700[7000.0 <= data$R5080700 & data$R5080700 <= 7999.0] <- 7000.0
  data$R5080700[8000.0 <= data$R5080700 & data$R5080700 <= 8999.0] <- 8000.0
  data$R5080700[9000.0 <= data$R5080700 & data$R5080700 <= 9999.0] <- 9000.0
  data$R5080700[10000.0 <= data$R5080700 & data$R5080700 <= 14999.0] <- 10000.0
  data$R5080700[15000.0 <= data$R5080700 & data$R5080700 <= 19999.0] <- 15000.0
  data$R5080700[20000.0 <= data$R5080700 & data$R5080700 <= 24999.0] <- 20000.0
  data$R5080700[25000.0 <= data$R5080700 & data$R5080700 <= 49999.0] <- 25000.0
  data$R5080700[50000.0 <= data$R5080700 & data$R5080700 <= 9.9999999E7] <- 50000.0
  data$R5080700 <- factor(data$R5080700,
                          levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0,10000.0,15000.0,20000.0,25000.0,50000.0),
                          labels=c("0",
                                   "1 TO 999",
                                   "1000 TO 1999",
                                   "2000 TO 2999",
                                   "3000 TO 3999",
                                   "4000 TO 4999",
                                   "5000 TO 5999",
                                   "6000 TO 6999",
                                   "7000 TO 7999",
                                   "8000 TO 8999",
                                   "9000 TO 9999",
                                   "10000 TO 14999",
                                   "15000 TO 19999",
                                   "20000 TO 24999",
                                   "25000 TO 49999",
                                   "50000 TO 99999999: 50000+"))
  data$R5165900[10.0 <= data$R5165900 & data$R5165900 <= 999.0] <- 10.0
  data$R5165900 <- factor(data$R5165900,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$R6478600[10.0 <= data$R6478600 & data$R6478600 <= 999.0] <- 10.0
  data$R6478600 <- factor(data$R6478600,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$R6478700[1.0 <= data$R6478700 & data$R6478700 <= 999.0] <- 1.0
  data$R6478700[1000.0 <= data$R6478700 & data$R6478700 <= 1999.0] <- 1000.0
  data$R6478700[2000.0 <= data$R6478700 & data$R6478700 <= 2999.0] <- 2000.0
  data$R6478700[3000.0 <= data$R6478700 & data$R6478700 <= 3999.0] <- 3000.0
  data$R6478700[4000.0 <= data$R6478700 & data$R6478700 <= 4999.0] <- 4000.0
  data$R6478700[5000.0 <= data$R6478700 & data$R6478700 <= 5999.0] <- 5000.0
  data$R6478700[6000.0 <= data$R6478700 & data$R6478700 <= 6999.0] <- 6000.0
  data$R6478700[7000.0 <= data$R6478700 & data$R6478700 <= 7999.0] <- 7000.0
  data$R6478700[8000.0 <= data$R6478700 & data$R6478700 <= 8999.0] <- 8000.0
  data$R6478700[9000.0 <= data$R6478700 & data$R6478700 <= 9999.0] <- 9000.0
  data$R6478700[10000.0 <= data$R6478700 & data$R6478700 <= 14999.0] <- 10000.0
  data$R6478700[15000.0 <= data$R6478700 & data$R6478700 <= 19999.0] <- 15000.0
  data$R6478700[20000.0 <= data$R6478700 & data$R6478700 <= 24999.0] <- 20000.0
  data$R6478700[25000.0 <= data$R6478700 & data$R6478700 <= 49999.0] <- 25000.0
  data$R6478700[50000.0 <= data$R6478700 & data$R6478700 <= 9.9999999E7] <- 50000.0
  data$R6478700 <- factor(data$R6478700,
                          levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0,10000.0,15000.0,20000.0,25000.0,50000.0),
                          labels=c("0",
                                   "1 TO 999",
                                   "1000 TO 1999",
                                   "2000 TO 2999",
                                   "3000 TO 3999",
                                   "4000 TO 4999",
                                   "5000 TO 5999",
                                   "6000 TO 6999",
                                   "7000 TO 7999",
                                   "8000 TO 8999",
                                   "9000 TO 9999",
                                   "10000 TO 14999",
                                   "15000 TO 19999",
                                   "20000 TO 24999",
                                   "25000 TO 49999",
                                   "50000 TO 99999999: 50000+"))
  data$R7006400[10.0 <= data$R7006400 & data$R7006400 <= 999.0] <- 10.0
  data$R7006400 <- factor(data$R7006400,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$R7006500[1.0 <= data$R7006500 & data$R7006500 <= 999.0] <- 1.0
  data$R7006500[1000.0 <= data$R7006500 & data$R7006500 <= 1999.0] <- 1000.0
  data$R7006500[2000.0 <= data$R7006500 & data$R7006500 <= 2999.0] <- 2000.0
  data$R7006500[3000.0 <= data$R7006500 & data$R7006500 <= 3999.0] <- 3000.0
  data$R7006500[4000.0 <= data$R7006500 & data$R7006500 <= 4999.0] <- 4000.0
  data$R7006500[5000.0 <= data$R7006500 & data$R7006500 <= 5999.0] <- 5000.0
  data$R7006500[6000.0 <= data$R7006500 & data$R7006500 <= 6999.0] <- 6000.0
  data$R7006500[7000.0 <= data$R7006500 & data$R7006500 <= 7999.0] <- 7000.0
  data$R7006500[8000.0 <= data$R7006500 & data$R7006500 <= 8999.0] <- 8000.0
  data$R7006500[9000.0 <= data$R7006500 & data$R7006500 <= 9999.0] <- 9000.0
  data$R7006500[10000.0 <= data$R7006500 & data$R7006500 <= 14999.0] <- 10000.0
  data$R7006500[15000.0 <= data$R7006500 & data$R7006500 <= 19999.0] <- 15000.0
  data$R7006500[20000.0 <= data$R7006500 & data$R7006500 <= 24999.0] <- 20000.0
  data$R7006500[25000.0 <= data$R7006500 & data$R7006500 <= 49999.0] <- 25000.0
  data$R7006500[50000.0 <= data$R7006500 & data$R7006500 <= 9.9999999E7] <- 50000.0
  data$R7006500 <- factor(data$R7006500,
                          levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0,10000.0,15000.0,20000.0,25000.0,50000.0),
                          labels=c("0",
                                   "1 TO 999",
                                   "1000 TO 1999",
                                   "2000 TO 2999",
                                   "3000 TO 3999",
                                   "4000 TO 4999",
                                   "5000 TO 5999",
                                   "6000 TO 6999",
                                   "7000 TO 7999",
                                   "8000 TO 8999",
                                   "9000 TO 9999",
                                   "10000 TO 14999",
                                   "15000 TO 19999",
                                   "20000 TO 24999",
                                   "25000 TO 49999",
                                   "50000 TO 99999999: 50000+"))
  data$R7703600[10.0 <= data$R7703600 & data$R7703600 <= 999.0] <- 10.0
  data$R7703600 <- factor(data$R7703600,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$R7703700[1.0 <= data$R7703700 & data$R7703700 <= 999.0] <- 1.0
  data$R7703700[1000.0 <= data$R7703700 & data$R7703700 <= 1999.0] <- 1000.0
  data$R7703700[2000.0 <= data$R7703700 & data$R7703700 <= 2999.0] <- 2000.0
  data$R7703700[3000.0 <= data$R7703700 & data$R7703700 <= 3999.0] <- 3000.0
  data$R7703700[4000.0 <= data$R7703700 & data$R7703700 <= 4999.0] <- 4000.0
  data$R7703700[5000.0 <= data$R7703700 & data$R7703700 <= 5999.0] <- 5000.0
  data$R7703700[6000.0 <= data$R7703700 & data$R7703700 <= 6999.0] <- 6000.0
  data$R7703700[7000.0 <= data$R7703700 & data$R7703700 <= 7999.0] <- 7000.0
  data$R7703700[8000.0 <= data$R7703700 & data$R7703700 <= 8999.0] <- 8000.0
  data$R7703700[9000.0 <= data$R7703700 & data$R7703700 <= 9999.0] <- 9000.0
  data$R7703700[10000.0 <= data$R7703700 & data$R7703700 <= 14999.0] <- 10000.0
  data$R7703700[15000.0 <= data$R7703700 & data$R7703700 <= 19999.0] <- 15000.0
  data$R7703700[20000.0 <= data$R7703700 & data$R7703700 <= 24999.0] <- 20000.0
  data$R7703700[25000.0 <= data$R7703700 & data$R7703700 <= 49999.0] <- 25000.0
  data$R7703700[50000.0 <= data$R7703700 & data$R7703700 <= 9.9999999E7] <- 50000.0
  data$R7703700 <- factor(data$R7703700,
                          levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0,10000.0,15000.0,20000.0,25000.0,50000.0),
                          labels=c("0",
                                   "1 TO 999",
                                   "1000 TO 1999",
                                   "2000 TO 2999",
                                   "3000 TO 3999",
                                   "4000 TO 4999",
                                   "5000 TO 5999",
                                   "6000 TO 6999",
                                   "7000 TO 7999",
                                   "8000 TO 8999",
                                   "9000 TO 9999",
                                   "10000 TO 14999",
                                   "15000 TO 19999",
                                   "20000 TO 24999",
                                   "25000 TO 49999",
                                   "50000 TO 99999999: 50000+"))
  data$R8496000[10.0 <= data$R8496000 & data$R8496000 <= 999.0] <- 10.0
  data$R8496000 <- factor(data$R8496000,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$R8496100[1.0 <= data$R8496100 & data$R8496100 <= 999.0] <- 1.0
  data$R8496100[1000.0 <= data$R8496100 & data$R8496100 <= 1999.0] <- 1000.0
  data$R8496100[2000.0 <= data$R8496100 & data$R8496100 <= 2999.0] <- 2000.0
  data$R8496100[3000.0 <= data$R8496100 & data$R8496100 <= 3999.0] <- 3000.0
  data$R8496100[4000.0 <= data$R8496100 & data$R8496100 <= 4999.0] <- 4000.0
  data$R8496100[5000.0 <= data$R8496100 & data$R8496100 <= 5999.0] <- 5000.0
  data$R8496100[6000.0 <= data$R8496100 & data$R8496100 <= 6999.0] <- 6000.0
  data$R8496100[7000.0 <= data$R8496100 & data$R8496100 <= 7999.0] <- 7000.0
  data$R8496100[8000.0 <= data$R8496100 & data$R8496100 <= 8999.0] <- 8000.0
  data$R8496100[9000.0 <= data$R8496100 & data$R8496100 <= 9999.0] <- 9000.0
  data$R8496100[10000.0 <= data$R8496100 & data$R8496100 <= 14999.0] <- 10000.0
  data$R8496100[15000.0 <= data$R8496100 & data$R8496100 <= 19999.0] <- 15000.0
  data$R8496100[20000.0 <= data$R8496100 & data$R8496100 <= 24999.0] <- 20000.0
  data$R8496100[25000.0 <= data$R8496100 & data$R8496100 <= 49999.0] <- 25000.0
  data$R8496100[50000.0 <= data$R8496100 & data$R8496100 <= 9.99999999E8] <- 50000.0
  data$R8496100 <- factor(data$R8496100,
                          levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0,10000.0,15000.0,20000.0,25000.0,50000.0),
                          labels=c("0",
                                   "1 TO 999",
                                   "1000 TO 1999",
                                   "2000 TO 2999",
                                   "3000 TO 3999",
                                   "4000 TO 4999",
                                   "5000 TO 5999",
                                   "6000 TO 6999",
                                   "7000 TO 7999",
                                   "8000 TO 8999",
                                   "9000 TO 9999",
                                   "10000 TO 14999",
                                   "15000 TO 19999",
                                   "20000 TO 24999",
                                   "25000 TO 49999",
                                   "50000 TO 999999999: 50000+"))
  data$T0987600[10.0 <= data$T0987600 & data$T0987600 <= 999.0] <- 10.0
  data$T0987600 <- factor(data$T0987600,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T0987800[1.0 <= data$T0987800 & data$T0987800 <= 999.0] <- 1.0
  data$T0987800[1000.0 <= data$T0987800 & data$T0987800 <= 1999.0] <- 1000.0
  data$T0987800[2000.0 <= data$T0987800 & data$T0987800 <= 2999.0] <- 2000.0
  data$T0987800[3000.0 <= data$T0987800 & data$T0987800 <= 3999.0] <- 3000.0
  data$T0987800[4000.0 <= data$T0987800 & data$T0987800 <= 4999.0] <- 4000.0
  data$T0987800[5000.0 <= data$T0987800 & data$T0987800 <= 5999.0] <- 5000.0
  data$T0987800[6000.0 <= data$T0987800 & data$T0987800 <= 6999.0] <- 6000.0
  data$T0987800[7000.0 <= data$T0987800 & data$T0987800 <= 7999.0] <- 7000.0
  data$T0987800[8000.0 <= data$T0987800 & data$T0987800 <= 8999.0] <- 8000.0
  data$T0987800[9000.0 <= data$T0987800 & data$T0987800 <= 9999.0] <- 9000.0
  data$T0987800[10000.0 <= data$T0987800 & data$T0987800 <= 14999.0] <- 10000.0
  data$T0987800[15000.0 <= data$T0987800 & data$T0987800 <= 19999.0] <- 15000.0
  data$T0987800[20000.0 <= data$T0987800 & data$T0987800 <= 24999.0] <- 20000.0
  data$T0987800[25000.0 <= data$T0987800 & data$T0987800 <= 49999.0] <- 25000.0
  data$T0987800[50000.0 <= data$T0987800 & data$T0987800 <= 9.9999999E7] <- 50000.0
  data$T0987800 <- factor(data$T0987800,
                          levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0,10000.0,15000.0,20000.0,25000.0,50000.0),
                          labels=c("0",
                                   "1 TO 999",
                                   "1000 TO 1999",
                                   "2000 TO 2999",
                                   "3000 TO 3999",
                                   "4000 TO 4999",
                                   "5000 TO 5999",
                                   "6000 TO 6999",
                                   "7000 TO 7999",
                                   "8000 TO 8999",
                                   "9000 TO 9999",
                                   "10000 TO 14999",
                                   "15000 TO 19999",
                                   "20000 TO 24999",
                                   "25000 TO 49999",
                                   "50000 TO 99999999: 50000+"))
  data$T2209900[10.0 <= data$T2209900 & data$T2209900 <= 999.0] <- 10.0
  data$T2209900 <- factor(data$T2209900,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T2210000[1.0 <= data$T2210000 & data$T2210000 <= 999.0] <- 1.0
  data$T2210000[1000.0 <= data$T2210000 & data$T2210000 <= 1999.0] <- 1000.0
  data$T2210000[2000.0 <= data$T2210000 & data$T2210000 <= 2999.0] <- 2000.0
  data$T2210000[3000.0 <= data$T2210000 & data$T2210000 <= 3999.0] <- 3000.0
  data$T2210000[4000.0 <= data$T2210000 & data$T2210000 <= 4999.0] <- 4000.0
  data$T2210000[5000.0 <= data$T2210000 & data$T2210000 <= 5999.0] <- 5000.0
  data$T2210000[6000.0 <= data$T2210000 & data$T2210000 <= 6999.0] <- 6000.0
  data$T2210000[7000.0 <= data$T2210000 & data$T2210000 <= 7999.0] <- 7000.0
  data$T2210000[8000.0 <= data$T2210000 & data$T2210000 <= 8999.0] <- 8000.0
  data$T2210000[9000.0 <= data$T2210000 & data$T2210000 <= 9999.0] <- 9000.0
  data$T2210000[10000.0 <= data$T2210000 & data$T2210000 <= 14999.0] <- 10000.0
  data$T2210000[15000.0 <= data$T2210000 & data$T2210000 <= 19999.0] <- 15000.0
  data$T2210000[20000.0 <= data$T2210000 & data$T2210000 <= 24999.0] <- 20000.0
  data$T2210000[25000.0 <= data$T2210000 & data$T2210000 <= 49999.0] <- 25000.0
  data$T2210000[50000.0 <= data$T2210000 & data$T2210000 <= 9.9999999E7] <- 50000.0
  data$T2210000 <- factor(data$T2210000,
                          levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0,10000.0,15000.0,20000.0,25000.0,50000.0),
                          labels=c("0",
                                   "1 TO 999",
                                   "1000 TO 1999",
                                   "2000 TO 2999",
                                   "3000 TO 3999",
                                   "4000 TO 4999",
                                   "5000 TO 5999",
                                   "6000 TO 6999",
                                   "7000 TO 7999",
                                   "8000 TO 8999",
                                   "9000 TO 9999",
                                   "10000 TO 14999",
                                   "15000 TO 19999",
                                   "20000 TO 24999",
                                   "25000 TO 49999",
                                   "50000 TO 99999999: 50000+"))
  data$T3107700[10.0 <= data$T3107700 & data$T3107700 <= 999.0] <- 10.0
  data$T3107700 <- factor(data$T3107700,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T3107800[1.0 <= data$T3107800 & data$T3107800 <= 999.0] <- 1.0
  data$T3107800[1000.0 <= data$T3107800 & data$T3107800 <= 1999.0] <- 1000.0
  data$T3107800[2000.0 <= data$T3107800 & data$T3107800 <= 2999.0] <- 2000.0
  data$T3107800[3000.0 <= data$T3107800 & data$T3107800 <= 3999.0] <- 3000.0
  data$T3107800[4000.0 <= data$T3107800 & data$T3107800 <= 4999.0] <- 4000.0
  data$T3107800[5000.0 <= data$T3107800 & data$T3107800 <= 5999.0] <- 5000.0
  data$T3107800[6000.0 <= data$T3107800 & data$T3107800 <= 6999.0] <- 6000.0
  data$T3107800[7000.0 <= data$T3107800 & data$T3107800 <= 7999.0] <- 7000.0
  data$T3107800[8000.0 <= data$T3107800 & data$T3107800 <= 8999.0] <- 8000.0
  data$T3107800[9000.0 <= data$T3107800 & data$T3107800 <= 9999.0] <- 9000.0
  data$T3107800[10000.0 <= data$T3107800 & data$T3107800 <= 14999.0] <- 10000.0
  data$T3107800[15000.0 <= data$T3107800 & data$T3107800 <= 19999.0] <- 15000.0
  data$T3107800[20000.0 <= data$T3107800 & data$T3107800 <= 24999.0] <- 20000.0
  data$T3107800[25000.0 <= data$T3107800 & data$T3107800 <= 49999.0] <- 25000.0
  data$T3107800[50000.0 <= data$T3107800 & data$T3107800 <= 9.9999999E7] <- 50000.0
  data$T3107800 <- factor(data$T3107800,
                          levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0,10000.0,15000.0,20000.0,25000.0,50000.0),
                          labels=c("0",
                                   "1 TO 999",
                                   "1000 TO 1999",
                                   "2000 TO 2999",
                                   "3000 TO 3999",
                                   "4000 TO 4999",
                                   "5000 TO 5999",
                                   "6000 TO 6999",
                                   "7000 TO 7999",
                                   "8000 TO 8999",
                                   "9000 TO 9999",
                                   "10000 TO 14999",
                                   "15000 TO 19999",
                                   "20000 TO 24999",
                                   "25000 TO 49999",
                                   "50000 TO 99999999: 50000+"))
  data$T4112200[10.0 <= data$T4112200 & data$T4112200 <= 999.0] <- 10.0
  data$T4112200 <- factor(data$T4112200,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T4112300[1.0 <= data$T4112300 & data$T4112300 <= 999.0] <- 1.0
  data$T4112300[1000.0 <= data$T4112300 & data$T4112300 <= 1999.0] <- 1000.0
  data$T4112300[2000.0 <= data$T4112300 & data$T4112300 <= 2999.0] <- 2000.0
  data$T4112300[3000.0 <= data$T4112300 & data$T4112300 <= 3999.0] <- 3000.0
  data$T4112300[4000.0 <= data$T4112300 & data$T4112300 <= 4999.0] <- 4000.0
  data$T4112300[5000.0 <= data$T4112300 & data$T4112300 <= 5999.0] <- 5000.0
  data$T4112300[6000.0 <= data$T4112300 & data$T4112300 <= 6999.0] <- 6000.0
  data$T4112300[7000.0 <= data$T4112300 & data$T4112300 <= 7999.0] <- 7000.0
  data$T4112300[8000.0 <= data$T4112300 & data$T4112300 <= 8999.0] <- 8000.0
  data$T4112300[9000.0 <= data$T4112300 & data$T4112300 <= 9999.0] <- 9000.0
  data$T4112300[10000.0 <= data$T4112300 & data$T4112300 <= 14999.0] <- 10000.0
  data$T4112300[15000.0 <= data$T4112300 & data$T4112300 <= 19999.0] <- 15000.0
  data$T4112300[20000.0 <= data$T4112300 & data$T4112300 <= 24999.0] <- 20000.0
  data$T4112300[25000.0 <= data$T4112300 & data$T4112300 <= 49999.0] <- 25000.0
  data$T4112300[50000.0 <= data$T4112300 & data$T4112300 <= 9.9999999E7] <- 50000.0
  data$T4112300 <- factor(data$T4112300,
                          levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0,10000.0,15000.0,20000.0,25000.0,50000.0),
                          labels=c("0",
                                   "1 TO 999",
                                   "1000 TO 1999",
                                   "2000 TO 2999",
                                   "3000 TO 3999",
                                   "4000 TO 4999",
                                   "5000 TO 5999",
                                   "6000 TO 6999",
                                   "7000 TO 7999",
                                   "8000 TO 8999",
                                   "9000 TO 9999",
                                   "10000 TO 14999",
                                   "15000 TO 19999",
                                   "20000 TO 24999",
                                   "25000 TO 49999",
                                   "50000 TO 99999999: 50000+"))
  data$T5022500[10.0 <= data$T5022500 & data$T5022500 <= 999.0] <- 10.0
  data$T5022500 <- factor(data$T5022500,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10 TO 999: 10+"))
  data$T5022600[1.0 <= data$T5022600 & data$T5022600 <= 999.0] <- 1.0
  data$T5022600[1000.0 <= data$T5022600 & data$T5022600 <= 1999.0] <- 1000.0
  data$T5022600[2000.0 <= data$T5022600 & data$T5022600 <= 2999.0] <- 2000.0
  data$T5022600[3000.0 <= data$T5022600 & data$T5022600 <= 3999.0] <- 3000.0
  data$T5022600[4000.0 <= data$T5022600 & data$T5022600 <= 4999.0] <- 4000.0
  data$T5022600[5000.0 <= data$T5022600 & data$T5022600 <= 5999.0] <- 5000.0
  data$T5022600[6000.0 <= data$T5022600 & data$T5022600 <= 6999.0] <- 6000.0
  data$T5022600[7000.0 <= data$T5022600 & data$T5022600 <= 7999.0] <- 7000.0
  data$T5022600[8000.0 <= data$T5022600 & data$T5022600 <= 8999.0] <- 8000.0
  data$T5022600[9000.0 <= data$T5022600 & data$T5022600 <= 9999.0] <- 9000.0
  data$T5022600[10000.0 <= data$T5022600 & data$T5022600 <= 14999.0] <- 10000.0
  data$T5022600[15000.0 <= data$T5022600 & data$T5022600 <= 19999.0] <- 15000.0
  data$T5022600[20000.0 <= data$T5022600 & data$T5022600 <= 24999.0] <- 20000.0
  data$T5022600[25000.0 <= data$T5022600 & data$T5022600 <= 49999.0] <- 25000.0
  data$T5022600[50000.0 <= data$T5022600 & data$T5022600 <= 9.9999999E7] <- 50000.0
  data$T5022600 <- factor(data$T5022600,
                          levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0,10000.0,15000.0,20000.0,25000.0,50000.0),
                          labels=c("0",
                                   "1 TO 999",
                                   "1000 TO 1999",
                                   "2000 TO 2999",
                                   "3000 TO 3999",
                                   "4000 TO 4999",
                                   "5000 TO 5999",
                                   "6000 TO 6999",
                                   "7000 TO 7999",
                                   "8000 TO 8999",
                                   "9000 TO 9999",
                                   "10000 TO 14999",
                                   "15000 TO 19999",
                                   "20000 TO 24999",
                                   "25000 TO 49999",
                                   "50000 TO 99999999: 50000+"))
  return(data)
}

varlabels <- c("ID# (1-12686) 79",
               "SAMPLE ID  79 INT",
               "RACL/ETHNIC COHORT /SCRNR 79",
               "SEX OF R 79",
               "FAMILY SIZE 86",
               "TOT NET FAMILY INC P-C YR 86",
               "EMPLOYMENT STATUS RECODE 86",
               "REGION OF CURRENT RESIDENCE 86",
               "MARITAL STATUS 86",
               "FAMILY SIZE 87",
               "EMPLOYMENT STATUS RECODE 87",
               "REGION OF CURRENT RESIDENCE 87",
               "MARITAL STATUS 87",
               "FAMILY SIZE 88",
               "TOT NET FAMILY INC P-C YR 88",
               "EMPLOYMENT STATUS RECODE 88",
               "REGION OF CURRENT RESIDENCE 88",
               "MARITAL STATUS 88",
               "FAMILY SIZE 89",
               "EMPLOYMENT STATUS RECODE 89",
               "REGION OF CURRENT RESIDENCE 89",
               "MARITAL STATUS 89",
               "FAMILY SIZE 90",
               "TOT NET FAMILY INC P-C YR 90",
               "EMPLOYMENT STATUS RECODE 90",
               "REGION OF CURRENT RESIDENCE 90",
               "MARITAL STATUS 90",
               "FAMILY SIZE 91",
               "EMPLOYMENT STATUS RECODE 91",
               "REGION OF CURRENT RESIDENCE 91",
               "MARITAL STATUS 91",
               "FAMILY SIZE 92",
               "TOT NET FAMILY INC P-C YR 92",
               "EMPLOYMENT STATUS RECODE 92",
               "REGION OF CURRENT RESIDENCE 92",
               "MARITAL STATUS 92",
               "FAMILY SIZE 93",
               "EMPLOYMENT STATUS RECODE 93",
               "REGION OF RESIDENCE 93",
               "MARITAL STATUS 93",
               "FAMILY SIZE 94",
               "TOTAL NET FAMILY INCOME 94",
               "EMPLOYMENT STATUS RECODE 94",
               "REGION OF RESIDENCE 94",
               "MARITAL STATUS 94",
               "FAMILY SIZE 96",
               "EMPLOYMENT STATUS RECODE 96",
               "REGION OF RESIDENCE 96",
               "MARITAL STATUS 96",
               "FAMILY SIZE 1998",
               "TOTAL NET FAMILY INCOME 1998",
               "EMPLOYMENT STATUS RECODE 1998",
               "REGION OF RESIDENCE 1998",
               "MARITAL STATUS 1998",
               "FAMILY SIZE 2000",
               "TOTAL NET FAMILY INCOME 2000",
               "REGION OF RESIDENCE 2000",
               "MARITAL STATUS 2000",
               "FAMILY SIZE 2002",
               "TOTAL NET FAMILY INCOME 2002",
               "REGION OF RESIDENCE 2002",
               "MARITAL STATUS 2002",
               "FAMILY SIZE 2004",
               "TOTAL NET FAMILY INCOME 2004",
               "REGION OF RESIDENCE 2004",
               "MARITAL STATUS 2004",
               "FAMILY SIZE 2006",
               "TOTAL NET FAMILY INCOME 2006",
               "EMPLOYMENT STATUS RECODE 2006",
               "REGION OF RESIDENCE 2006",
               "MARITAL STATUS 2006",
               "FAMILY SIZE 2008",
               "TOTAL NET FAMILY INCOME 2008",
               "REGION OF RESIDENCE 2008",
               "MARITAL STATUS 2008",
               "FAMILY SIZE 2010",
               "TOTAL NET FAMILY INCOME 2010",
               "REGION OF RESIDENCE 2010",
               "MARITAL STATUS 2010",
               "FAMILY SIZE 2012",
               "TOTAL NET FAMILY INCOME 2012",
               "REGION OF RESIDENCE 2012",
               "MARITAL STATUS 2012",
               "FAMILY SIZE 2014",
               "TOTAL NET FAMILY INCOME 2014",
               "REGION OF RESIDENCE 2014",
               "MARITAL STATUS 2014",
               "HIGHEST GRADE EVER COMPLETED XRND"
)


# Use qnames rather than rnums

qnames = function(data) {
  names(data) <- c("CASEID_1979",
                   "SAMPLE_ID_1979",
                   "SAMPLE_RACE_78SCRN",
                   "SAMPLE_SEX_1979",
                   "FAMSIZE_1986",
                   "TNFI_TRUNC_1986",
                   "ESR_KEY_1986",
                   "REGION_1986",
                   "MARSTAT-KEY_1986",
                   "FAMSIZE_1987",
                   "ESR_KEY_1987",
                   "REGION_1987",
                   "MARSTAT-KEY_1987",
                   "FAMSIZE_1988",
                   "TNFI_TRUNC_1988",
                   "ESR_KEY_1988",
                   "REGION_1988",
                   "MARSTAT-KEY_1988",
                   "FAMSIZE_1989",
                   "ESR_KEY_1989",
                   "REGION_1989",
                   "MARSTAT-KEY_1989",
                   "FAMSIZE_1990",
                   "TNFI_TRUNC_1990",
                   "ESR_KEY_1990",
                   "REGION_1990",
                   "MARSTAT-KEY_1990",
                   "FAMSIZE_1991",
                   "ESR_KEY_1991",
                   "REGION_1991",
                   "MARSTAT-KEY_1991",
                   "FAMSIZE_1992",
                   "TNFI_TRUNC_1992",
                   "ESR_KEY_1992",
                   "REGION_1992",
                   "MARSTAT-KEY_1992",
                   "FAMSIZE_1993",
                   "ESR_KEY_1993",
                   "REGION_1993",
                   "MARSTAT-KEY_1993",
                   "FAMSIZE_1994",
                   "TNFI_TRUNC_1994",
                   "ESR_KEY_1994",
                   "REGION_1994",
                   "MARSTAT-KEY_1994",
                   "FAMSIZE_1996",
                   "ESR_KEY_1996",
                   "REGION_1996",
                   "MARSTAT-KEY_1996",
                   "FAMSIZE_1998",
                   "TNFI_TRUNC_1998",
                   "ESR_KEY_1998",
                   "REGION_1998",
                   "MARSTAT-KEY_1998",
                   "FAMSIZE_2000",
                   "TNFI_TRUNC_2000",
                   "REGION_2000",
                   "MARSTAT-KEY_2000",
                   "FAMSIZE_2002",
                   "TNFI_TRUNC_2002",
                   "REGION_2002",
                   "MARSTAT-KEY_2002",
                   "FAMSIZE_2004",
                   "TNFI_TRUNC_2004",
                   "REGION_2004",
                   "MARSTAT-KEY_2004",
                   "FAMSIZE_2006",
                   "TNFI_TRUNC_2006",
                   "ESR_KEY_2006",
                   "REGION_2006",
                   "MARSTAT-KEY_2006",
                   "FAMSIZE_2008",
                   "TNFI_TRUNC_2008",
                   "REGION_2008",
                   "MARSTAT-KEY_2008",
                   "FAMSIZE_2010",
                   "TNFI_TRUNC_2010",
                   "REGION_2010",
                   "MARSTAT-KEY_2010",
                   "FAMSIZE_2012",
                   "TNFI_TRUNC_2012",
                   "REGION_2012",
                   "MARSTAT-KEY_2012",
                   "FAMSIZE_2014",
                   "TNFI_TRUNC_2014",
                   "REGION_2014",
                   "MARSTAT-KEY_2014",
                   "HGC_EVER_XRND")
  return(data)
}


#********************************************************************************************************

# Remove the '#' before the following line to create a data file called "categories" with value labels.
categories <- vallabels(new_data)

# Remove the '#' before the following lines to rename variables using Qnames instead of Reference Numbers
new_data <- qnames(new_data)
categories <- qnames(categories)

# Produce summaries for the raw (uncategorized) data file
summary(new_data)

mothers<-new_data

#************************************************************************************************************


# INCOME
mothers$income_1986<-mothers$`TNFI_TRUNC_1986`
mothers$income_1988<-mothers$`TNFI_TRUNC_1988`
mothers$income_1990<-mothers$`TNFI_TRUNC_1990`
mothers$income_1992<-mothers$`TNFI_TRUNC_1992`
mothers$income_1994<-mothers$`TNFI_TRUNC_1994`
mothers$income_1996<-NA
mothers$income_1998<-mothers$`TNFI_TRUNC_1998`
mothers$income_2000<-mothers$`TNFI_TRUNC_2000`
mothers$income_2002<-mothers$`TNFI_TRUNC_2002`
mothers$income_2004<-mothers$`TNFI_TRUNC_2004`
mothers$income_2006<-mothers$`TNFI_TRUNC_2006`
mothers$income_2008<-mothers$`TNFI_TRUNC_2008`
mothers$income_2010<-mothers$`TNFI_TRUNC_2010`
mothers$income_2012<-mothers$`TNFI_TRUNC_2012`
mothers$income_2014<-mothers$`TNFI_TRUNC_2014`

# EDUCATION
mothers$highestgrade<-mothers$HGC_EVER_XRND

# MARITAL STATUS

mothers$marstatus_1986<-mothers$`MARSTAT-KEY_1986`
mothers$marstatus_1988<-mothers$`MARSTAT-KEY_1988`
mothers$marstatus_1990<-mothers$`MARSTAT-KEY_1990`
mothers$marstatus_1992<-mothers$`MARSTAT-KEY_1992`
mothers$marstatus_1994<-mothers$`MARSTAT-KEY_1994`
mothers$marstatus_1996<-mothers$`MARSTAT-KEY_1996`
mothers$marstatus_1998<-mothers$`MARSTAT-KEY_1998`
mothers$marstatus_2000<-mothers$`MARSTAT-KEY_2000`
mothers$marstatus_2002<-mothers$`MARSTAT-KEY_2002`
mothers$marstatus_2004<-mothers$`MARSTAT-KEY_2004`
mothers$marstatus_2006<-mothers$`MARSTAT-KEY_2006`
mothers$marstatus_2008<-mothers$`MARSTAT-KEY_2008`
mothers$marstatus_2010<-mothers$`MARSTAT-KEY_2010`
mothers$marstatus_2012<-mothers$`MARSTAT-KEY_2012`
mothers$marstatus_2014<-mothers$`MARSTAT-KEY_2014`

# REGION


mothers$region_1986<-mothers$`REGION_1986`
mothers$region_1988<-mothers$`REGION_1988`
mothers$region_1990<-mothers$`REGION_1990`
mothers$region_1992<-mothers$`REGION_1992`
mothers$region_1994<-mothers$`REGION_1994`
mothers$region_1996<-mothers$`REGION_1996`
mothers$region_1998<-mothers$`REGION_1998`
mothers$region_2000<-mothers$`REGION_2000`
mothers$region_2002<-mothers$`REGION_2002`
mothers$region_2004<-mothers$`REGION_2004`
mothers$region_2006<-mothers$`REGION_2006`
mothers$region_2008<-mothers$`REGION_2008`
mothers$region_2010<-mothers$`REGION_2010`
mothers$region_2012<-mothers$`REGION_2012`
mothers$region_2014<-mothers$`REGION_2014`


# FAMILY SIZE

mothers$famsize_1986<-mothers$`FAMSIZE_1986`
mothers$famsize_1988<-mothers$`FAMSIZE_1988`
mothers$famsize_1990<-mothers$`FAMSIZE_1990`
mothers$famsize_1992<-mothers$`FAMSIZE_1992`
mothers$famsize_1994<-mothers$`FAMSIZE_1994`
mothers$famsize_1996<-mothers$`FAMSIZE_1996`
mothers$famsize_1998<-mothers$`FAMSIZE_1998`
mothers$famsize_2000<-mothers$`FAMSIZE_2000`
mothers$famsize_2002<-mothers$`FAMSIZE_2002`
mothers$famsize_2004<-mothers$`FAMSIZE_2004`
mothers$famsize_2006<-mothers$`FAMSIZE_2006`
mothers$famsize_2008<-mothers$`FAMSIZE_2008`
mothers$famsize_2010<-mothers$`FAMSIZE_2010`
mothers$famsize_2012<-mothers$`FAMSIZE_2012`
mothers$famsize_2014<-mothers$`FAMSIZE_2014`


# EMPLOYMENT STATUS

mothers$unempl_1986<-mothers$`ESR_KEY_1986`
mothers$unempl_1988<-mothers$`ESR_KEY_1988`
mothers$unempl_1990<-mothers$`ESR_KEY_1990`
mothers$unempl_1992<-mothers$`ESR_KEY_1992`
mothers$unempl_1994<-mothers$`ESR_KEY_1994`
mothers$unempl_1996<-mothers$`ESR_KEY_1996`
mothers$unempl_1998<-mothers$`ESR_KEY_1998`
mothers$unempl_2000<-NA
mothers$unempl_2002<-NA
mothers$unempl_2004<-NA
mothers$unempl_2006<-mothers$`ESR_KEY_2006`
mothers$unempl_2008<-NA
mothers$unempl_2010<-NA
mothers$unempl_2012<-NA
mothers$unempl_2014<-NA

# IDS

mothers$motherID<-mothers$CASEID_1979


# EXTRACT VARIABLES
myvars <- c( "motherID", "highestgrade",
             "income_1986", "income_1988", "income_1990",
             "income_1992", "income_1994", "income_1996",
             "income_1998", "income_2000", "income_2002",
             "income_2004", "income_2006", "income_2008",
             "income_2010", "income_2012", "income_2014" ,
             "famsize_1986", "famsize_1988", "famsize_1990",
             "famsize_1992", "famsize_1994", "famsize_1996",
             "famsize_1998", "famsize_2000", "famsize_2002",
             "famsize_2004", "famsize_2006", "famsize_2008",
             "famsize_2010", "famsize_2012", "famsize_2014",
             "region_1986", "region_1988", "region_1990",
             "region_1992", "region_1994", "region_1996",
             "region_1998", "region_2000", "region_2002",
             "region_2004", "region_2006", "region_2008",
             "region_2010", "region_2012", "region_2014",
             "unempl_1986", "unempl_1988", "unempl_1990",
             "unempl_1992", "unempl_1994", "unempl_1996",
             "unempl_1998", "unempl_2000", "unempl_2002",
             "unempl_2004", "unempl_2006", "unempl_2008",
             "unempl_2010", "unempl_2012", "unempl_2014",
             "marstatus_1986", "marstatus_1988", "marstatus_1990",
             "marstatus_1992", "marstatus_1994", "marstatus_1996",
             "marstatus_1998", "marstatus_2000", "marstatus_2002",
             "marstatus_2004", "marstatus_2006", "marstatus_2008",
             "marstatus_2010", "marstatus_2012", "marstatus_2014")

mothers<-mothers[myvars]

# MERGE WITH CHILDRENS DATA 

merged<-merge(wide, mothers, by="motherID",all.x=TRUE)
wide<-merged



#######################################################
######### 4) RESHAPE TO LONG ##########################
#######################################################

# RELOCATE ORDER VARIABLES

wide<-wide %>%
  relocate(highestgrade,.after=motherbirthage)

# RESHAPE 

long<-reshape(wide, idvar="ID", varying=12:176, timevar="year", sep="_", direction="long")


# ARRANGE ORDER

long<-long%>%  
  arrange(as.numeric(ID), as.numeric(year),as.numeric(motherID))

# RELOCATE ORDER VARIABLES

long<-long %>%
  relocate(year, motherID,.after=ID)

#################################################################
######### 5 ) CREATE TREATMENT VARIABLES ##########################
#################################################################


# ---------- MATHEMATICAL ABILITY #

#Create variable
long1<-long %>%
  filter(!is.na(mathz))%>%
  arrange(ID, year)%>%
  group_by(ID)%>%
  mutate(diff=c(NA, diff(mathz)))  %>%
  filter(!is.na(diff))

#Relabel
long1$dmathz<-long1$diff

#Extract from sample
myvars <- c("ID", "dmathz", "year")
merging<-long1[myvars]

#Merge with existent database
long<-merge(long, merging, by=c("ID","year"),all=TRUE)

# ---------- READING #

#Create variable
long1<-long %>%
  filter(!is.na(recoz))%>%
  arrange(ID, year)%>%
  group_by(ID)%>%
  mutate(diff=c(NA, diff(recoz)))%>%
  filter(!is.na(diff))

#Relabel
long1$drecoz<-long1$diff

#Extract from sample
myvars <- c("ID", "drecoz", "year")
merging<-long1[myvars]

#Merge with existent database
long<-merge(long, merging, by=c("ID","year"),all=TRUE)

#Rename dataset
data<-long

#Create binary treatment maths
data$mathdecr<-data$dmathz
data$mathdecr[data$mathdecr>=0]<-0
data$mathdecr[data$mathdecr<0]<-1
table(data$mathdecr)

#Create binary treatment reading
data$recodecr<-data$drecoz
data$recodecr[data$recodecr>=0]<-0
data$recodecr[data$recodecr<0]<-1
table(data$recodecr)


################################################################################
######### 6) PREPARE REST OF VARIABLES FOR THE ANALYSIS ##########################
################################################################################



# DESTRING THE DATA #

data$ID<-as.numeric(data$ID)
data$motherID<-as.numeric(data$motherID)
data$sex<-as.numeric(data$sex)
data$race<-as.numeric(data$race)
data$cognz<-as.numeric(data$cognz)
data$dmathz<-as.numeric(data$dmathz)
data$drecoz<-as.numeric(data$drecoz)
data$mathdecr<-as.factor(data$mathdecr)
data$recodecr<-as.factor(data$recodecr)

# SIMPLIFIFIED yearS VARIABLE #

data$yearsimp<-data$year
data$yearsimp[data$yearsimp==1986]<-1
data$yearsimp[data$yearsimp==1988]<-2
data$yearsimp[data$yearsimp==1990]<-3
data$yearsimp[data$yearsimp==1992]<-4
data$yearsimp[data$yearsimp==1994]<-5
data$yearsimp[data$yearsimp==1996]<-6
data$yearsimp[data$yearsimp==1998]<-7
data$yearsimp[data$yearsimp==2000]<-8
data$yearsimp[data$yearsimp==2002]<-9
data$yearsimp[data$yearsimp==2004]<-10
data$yearsimp[data$yearsimp==2006]<-11
data$yearsimp[data$yearsimp==2008]<-12
data$yearsimp[data$yearsimp==2010]<-13
data$yearsimp[data$yearsimp==2012]<-14
data$yearsimp[data$yearsimp==2014]<-15


# MOTHERES EDUCATION #
table(data$education)
table(data$highestgrade)
data$tertiary<-data$education
data$tertiary[data$tertiary<=12]<-0 # Less than Tertiary
data$tertiary[data$tertiary>=13]<-1 # Tertiary Education
table(data$tertiary)

# RACE # 

data$black<-data$race
data$black[data$black==1 | data$black==3]<-0
data$black[data$black==2]<-1
table(data$black)

data$hisp<-data$race
data$hisp[data$hisp==2 | data$hisp==3]<-0
data$hisp[data$hisp==1]<-1
table(data$hisp)

data$white<-data$race
data$white[data$white==1 | data$white==2]<-0
data$white[data$white==3]<-1
table(data$white)

# SEX variable #

table(data$sex)
data$female<-data$sex
data$female[data$female==1]<-0 #male
data$female[data$female==2]<-1 #female
table(data$female)

# MOTHERS' BIRTH AGE #
table(data$motherbirthage)
data$motherbirthage[data$motherbirthage<14]<-NA

# RESIDENCE #
table(data$residence)
data$residence[data$residence!=1]<-0 # different residence than "in household with mother"

#BIRTH ORDER#
table(data$birthorder)

#DATE of BIRTH#
table(data$birthyear)

# AGE in months AT INTERVIEW#
summary(data$ageatint)   
data$yearsatint<-(data$ageatint/12)
hist(data$yearsatint)

# INCOME QUINTILES # 
summary(data$income)
data$inc<-as.factor(ntile(data$income, 10))
table(data$inc)
data$inc<-as.numeric(data$inc)

data$topinc<-data$inc
data$topinc[data$topinc==1 | data$topinc==2 | data$topinc==3 | data$topinc==4 | data$topinc==5 | data$topinc==6 | data$topinc==7 | data$topinc==8 ]<-0
data$topinc[data$topinc==9 | data$topinc==10]<-1
table(data$topinc)

data$bottominc<-data$inc
data$bottominc[ data$bottominc==3 | data$bottominc==4 | data$bottominc==5 | data$bottominc==6 | data$bottominc==7 | data$bottominc==8 | data$bottominc==9 | data$bottominc==10]<-0
data$bottominc[data$bottominc==1 | data$bottominc==2]<-1
table(data$bottominc)


# PARENTAL SEPARATION #

table(data$marstatus)

data$separation<-data$marstatus
data$separation[data$separation ==1]<-0
data$separation[data$separation ==2 | data$separation ==3 | data$separation ==6]<-1

table(data$separation)
data$separation<-as.factor(data$separation)

################################################################################
######### 7) RESTRICT THE SAMPLE ###############################################
################################################################################


# RESTRICT TO AGE RANGE

data <-data[data$yearsatint>= 5 & data$yearsatint<= 15,]



################################################################################
######### 8) DESCRIPTIVES ##########################
################################################################################
  

# Graphs

data$dico<-as.factor(data$highestgrade)
table(data$dico)
data$dico <- factor(data$dico,
                    levels = c(0,1),
                    labels = c("Non-Tertiary Education", "Tertiary Education"))

# DEPENDENT VARIABLES

g<-ggplot(data=subset(data,!is.na(data$dico)), aes(cognz, colour=dico)) +
  geom_density(adjust=1.5, alpha=.4)+
  labs( x="Distribution of Cognitive Stimulation", y="Kernel Density") +
  scale_color_manual(labels = c("Non-Tertiary Education", "Tertiary Education"), values = c("blueviolet", "darkgoldenrod1")) +
  theme_bw() +
  guides(color=guide_legend(""))
g<-g + theme_bw()

g <- g + theme(axis.title.y = element_text(size = 14, angle = 90, face="bold"))
g <- g + theme(axis.title.x = element_text(size = 14, angle = 00, face="bold"))
g <- g + theme(legend.title =element_text(size = 14, angle = 00) )
g <- g + theme(legend.text  =element_text(size = 14, angle = 00) )
g<- g + theme(legend.position="bottom")
g


# PREDICTORS

a<-ggplot(data=subset(data,!is.na(data$dico)), aes(mathz, colour=dico)) +
  geom_density(adjust=1.5, alpha=.4)+
  labs(x="Distribution of Mathematical Ability", y="Kernel Density") +
  scale_color_manual(labels = c("Non-Tertiary Education", "Tertiary Education"), values = c("blueviolet", "darkgoldenrod1")) +
  theme_bw() +
  guides(color=guide_legend(""))+
  scale_x_continuous(limits = c(50, 130))
a<-a + theme_bw()
a<- a + theme(axis.title.y = element_text(size = 14, angle = 90, face="bold"))
a <- a + theme(axis.title.x = element_text(size = 14, angle = 00, face="bold"))
a <- a + theme(legend.title =element_text(size = 14, angle = 00) )
a <- a + theme(legend.text  =element_text(size = 14, angle = 00) )
a<- a + theme(legend.position="bottom")
a

b<-ggplot(data=subset(data,!is.na(data$dico)), aes(recoz, colour=dico)) +
  geom_density(adjust=1.5, alpha=.4)+
  labs(x="Distribution of Reading Ability", y="Kernel Density") +
  scale_color_manual(labels = c("Non-Tertiary Education", "Tertiary Education"), values = c("blueviolet", "darkgoldenrod1")) +
  theme_bw() +
  guides(color=guide_legend(""))+
  scale_x_continuous(limits = c(50, 130))
b<-b + theme_bw()

b <- b + theme(axis.title.y = element_text(size = 14, angle = 90, face="bold"))
b <- b + theme(axis.title.x = element_text(size = 14, angle = 00, face="bold"))
b <- b + theme(legend.title =element_text(size = 14, angle = 00) )
b<- b + theme(legend.text  =element_text(size = 14, angle = 00) )
b<- b + theme(legend.position="bottom")
b


library(lemon)
legend <- g_legend(a + theme(legend.position='bottom'))

library(gridExtra)
nt <- theme(legend.position='hidden')
grid_arrange_shared_legend(a, b+nt,ncol=2, nrow=1)


# COLUMNS CHART 

table(data$mathdecr)
12072 /(13153 +12072) #47.8%
table(tert$mathdecr) 
3118 / (3118 +3704) #45.7%
table(nontert$mathdecr)
8759/(8759+9240) # 48.6%

table(data$recodecr)
12917  /(12917  +12191) #51.4%
table(tert$recodecr) 
3384 / (3384 + 3417) #49.7%
table(nontert$recodecr)
9304 /(9304 +8601) # 51.9%


Group <- c("Average", "Non-Tertiary", "Tertiary")
Maths<-c("47.8","48.6", "45.7" )
Reading<-c("51.4","51.9", "49.7" )

df <- data.frame(Group, Maths, Reading)
print (df)
df$Maths<-as.numeric(df$Maths)
df$Reading<-as.numeric(df$Reading)
df$Group<-as.factor(df$Group)

ggplot(data = df, 
       aes(x = Group, 
           y = Maths, colour=)) +
  geom_col()



#################### GRAPH with missingness

library(mice)

ver<-c("mathdecr", "ID")
extract <-data[ver]

md.pattern(extract) 

id_counts <- data %>% group_by(ID) %>% summarise(Count = n())
print(id_counts)

summary(id_counts$Count)


# Calculate the number of time periods for each ID
id_periods <- data %>% group_by(ID) %>% summarise(NumPeriods = n_distinct(year))

# Calculate the average number of time periods
avg_periods <- mean(id_periods$NumPeriods)

# Create a bar plot using ggplot2
ggplot(id_periods, aes(x = ID, y = NumPeriods)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  ggtitle("Average Number of Time Periods per Individual") +
  xlab("ID") +
  ylab("Number of Time Periods") +
  geom_hline(yintercept = avg_periods, linetype = "dashed", color = "red") +
  annotate("text", x = 1, y = avg_periods + 0.1, label = sprintf("Average: %.2f", avg_periods), color = "red", size = 4)


# Calculate the number of time periods for each cohort
data$birthyear[data$birthyear<=1973 | data$birthyear>=2006]<-NA
table(data$birthyear)

cohort_periods <- data %>% group_by(birthyear) %>% summarise(AvgNumPeriods = mean(n_distinct(year)))

# Create a bar plot using ggplot2: Average Number of Time Periods per Cohort
g<-ggplot(cohort_periods, aes(x = birthyear, y = AvgNumPeriods)) +
  geom_bar(stat = "identity", fill = "darkseagreen") +
  ggtitle("") +
  xlab("Year of birth") +
  ylab("Average Number of Time Periods") +
  scale_y_continuous(breaks=seq(1974,2005,1)) + 
  theme_minimal() +
  theme_bw() +
  guides(color=guide_legend(""))
g<-g + theme_bw()

g <- g + theme(axis.title.y = element_text(size = 12, angle = 90, face="bold"))
g <- g + theme(axis.title.x = element_text(size = 12, angle = 00, face="bold"))
g <- g + theme(legend.title =element_text(size = 12, angle = 00) )
g <- g + theme(legend.text  =element_text(size = 12, angle = 00) )
g<- g + theme(legend.position="bottom")
g <- g + coord_flip()
g



################################################################################
######### 9) MODELS #############################################################
################################################################################


####### 1)  OLS MODELS ####################


m1<-feols(cognz~mathdecr+ female + factor(race) + year + highestgrade , data=data)
etable(m1)

m2<-feols(cognz~recodecr+ female + factor(race) + year + highestgrade, data=data)
etable(m2)

####### 2)  FE MODELS ####################

m3 <- feols(cognz~mathdecr  + highestgrade | ID + year , data)
etable(m3)

m4 <- feols(cognz~recodecr  + highestgrade| ID + year, data)
etable(m4)


######## 3) FECT ############

# Make sure variables are numeric class #
data$cognz<-as.numeric(data$cognz)
data$ID<-as.numeric(data$ID)
data$year<-as.numeric(data$year)
data$mathdecr<-as.numeric(as.character(data$mathdecr))
data$recodecr<-as.numeric(as.character(data$recodecr))

data <- data %>%
  filter(!is.na(mathdecr))%>%
  filter(!is.na(recodecr))%>%
  filter(!is.na(cognz))

# Treatment status graph #
panelview(cognz~ mathdecr, data = ver, index = c("ID","year"), 
          axis.lab = "time", xlab = "Time", ylab = "Unit", 
          background = "white", main = " ", display.all=TRUE,
          color=c("blue3", "red1", "white"))

panelview(cognz~ recodecr, data = data, index = c("ID","year"), 
          axis.lab = "time", xlab = "Time", ylab = "Unit", 
          background = "white", main = "", display.all=TRUE,
          color=c("lightblue1", "blue3", "white"))

m5 <- fect(cognz~ mathdecr, data = data,  index = c("ID","year"), 
           method = "fe", force = "two-way", se=TRUE, 
           nboots=200, na.rm=TRUE)

plot(m5, main = "Estimated ATT (FEct)", ylab = "Effect of mathematical decline in ability on maternal conitive stimulation", 
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8, proportion = 0, count=FALSE)

print(m5)

m6 <- fect(cognz~ recodecr , data = data,  index = c("ID","year"), 
           method = "fe", force = "two-way", se=TRUE, nboots=200, na.rm=TRUE)

plot(m6, main = "Estimated ATT (FEct)", ylab = "Effect of reading decline in ability on maternal conitive stimulation",  
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8, proportion=0)

print(m6)


####### 4)  STRATIFIED MODELS ####################

#######   with TWFE

# Education

table(data$highestgrade)

data$educat<-data$highestgrade
data$educat[data$educat<=12]<-0 #nontert
data$educat[data$educat>=13]<-1 #tertiary

table(data$educat)

split<-split(data, f=data$educat)
nontert<-split$`0`
tert<-split$`1`

m7 <- feols(cognz~mathdecr| ID + year ,nontert)
etable(m7)

m8 <- feols(cognz~mathdecr| ID + year , tert)
etable(m8)

m9 <- feols(cognz~recodecr| ID + year, nontert)
etable(m9)

m10 <- feols(cognz~recodecr| ID + year, tert)
etable(m10)


# Income

split<-split(data, f=data$topinc)
top<-split$`1`

split<-split(data, f=data$bottominc)
bottom<-split$`1`

m11 <- feols(cognz~mathdecr| ID + year ,bottom)
etable(m11)

m12 <- feols(cognz~mathdecr| ID + year , top)
etable(m12)

m13 <- feols(cognz~recodecr| ID + year, bottom)
etable(m13)

m14 <- feols(cognz~recodecr| ID + year, top)
etable(m14)


####### with FEct

# Education
m15<- fect(cognz~ mathdecr , data = tert,  index = c("ID","year"), 
           method = "fe", force = "two-way", se=TRUE, nboots=200, na.rm=TRUE)
print(m15)

m16 <- fect(cognz~ mathdecr, data = nontert,  index = c("ID","year"), 
            method = "fe", force = "two-way", se=TRUE, nboots=200, na.rm=TRUE)
print(m16)

m17 <- fect(cognz~ recodecr, data = tert,  index = c("ID","year"), 
            method = "fe", force = "two-way", se=TRUE, nboots=200, na.rm=TRUE)
print(m17)

m18 <- fect(cognz~ recodecr, data = nontert,  index = c("ID","year"), 
            method = "fe", force = "two-way", se=TRUE, nboots=200, na.rm=TRUE)
print(m18)

# Income

m19<- fect(cognz~ mathdecr , data = top,  index = c("ID","year"), 
           method = "fe", force = "two-way", se=TRUE, nboots=200, na.rm=TRUE)
print(m19)

m20 <- fect(cognz~ mathdecr, data = bottom,  index = c("ID","year"), 
            method = "fe", force = "two-way", se=TRUE, nboots=200, na.rm=TRUE)
print(m20)

m21<- fect(cognz~ recodecr, data = top,  index = c("ID","year"), 
            method = "fe", force = "two-way", se=TRUE, nboots=200, na.rm=TRUE)
print(m21)

m22<- fect(cognz~ recodecr, data = bottom,  index = c("ID","year"), 
            method = "fe", force = "two-way", se=TRUE, nboots=200, na.rm=TRUE)
print(m22)


################################################################################
######### 10) ROBUSTNESS #############################################################
################################################################################

################## GRAPH FOR VISUAL INSPECTION OF PRE-TRENDS ###############################

# MATHS

library(readxl)
parallel <- read_excel("/Users/agarcias/Library/CloudStorage/OneDrive-UniversitédeLausanne/DPHIL/SECOND PAPER/ANALYSES/New graphs/parallel.xlsx",sheet=2)

parallel$variable<-as.factor(parallel$variable)


figure<-ggplot(parallel,
               aes(variable, estimate, colour=group, group=group)) +
  geom_point(aes(variable, estimate, colour=group, group=group),
             position = position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x = variable, 
                    ymin = ci_low,
                    ymax = ci_high),
                position = position_dodge(width = 0.5),
                width=0.1,alpha=0.8, size=0.5) +
  theme_bw()+
  labs(x= "Years to treatment (mathematical ability) ",
       y = "Coefficients") +
  labs(color='Group')

figure <-figure + scale_color_manual(values=c("blueviolet", "darkgoldenrod1"))  
figure <- figure + theme(axis.title.y = element_text(size = 14, angle = 90, face="bold"))
figure <- figure + theme(axis.title.x = element_text(size = 14, angle = 00, face="bold"))
figure <- figure + theme(legend.title =element_text(size = 14, angle = 00) )
figure <- figure + theme(legend.text  =element_text(size = 14, angle = 00) )
figure <- figure + theme(legend.position="bottom")
figure


# READING
library(readxl)
parallel <- read_excel("/Users/agarcias/Library/CloudStorage/OneDrive-UniversitédeLausanne/DPHIL/SECOND PAPER/ANALYSES/New graphs/parallel.xlsx", sheet=1)

parallel$variable<-as.factor(parallel$variable)

figure1<-ggplot(parallel,
                aes(variable, estimate, colour=group, group=group)) +
  geom_point(aes(variable, estimate, colour=group, group=group),
             position = position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x = variable, 
                    ymin = ci_low,
                    ymax = ci_high),
                position = position_dodge(width = 0.5),
                width=0.1,alpha=0.8, size=0.5) +
  theme_bw()+
  labs(x= "Years to treatment (reading ability) ",
       y = "Coefficients") +
  labs(color='Group') 

figure1 <-figure1 + scale_color_manual(values=c("blueviolet", "darkgoldenrod1"))  
figure1 <- figure1 + theme(axis.title.y = element_text(size = 14, angle = 90, face="bold"))
figure1 <- figure1 + theme(axis.title.x = element_text(size = 14, angle = 00, face="bold"))
figure1 <- figure1 + theme(legend.title =element_text(size = 14, angle = 00) )
figure1 <- figure1 + theme(legend.text  =element_text(size = 14, angle = 00) )
figure1 <- figure1 + theme(legend.position="bottom")
figure1


# Combine both figures
library(lemon)
legend <- g_legend(figure + theme(legend.position='bottom'))
library(gridExtra)
nt <- theme(legend.position='hidden')
grid_arrange_shared_legend(figure, figure1+nt,ncol=2, nrow=1)


################## TEST FOR NO PRE-TRENDs LIU ET AL. ###############################

# Repeat the models to have them here

# basic models 
m5 <- fect(cognz~ mathdecr, data = data,  index = c("ID","year"), 
           method = "fe", force = "two-way", se=TRUE, 
           nboots=200, na.rm=TRUE)

m6 <- fect(cognz~ recodecr , data = data,  index = c("ID","year"), 
           method = "fe", force = "two-way", se=TRUE, 
           nboots=200, na.rm=TRUE)

#leave one out models
m5a <- fect(cognz~ mathdecr, data = data,  index = c("ID","year"), 
           method = "fe", force = "two-way", se=TRUE, 
           nboots=200, na.rm=TRUE, loo = TRUE)

m6a <- fect(cognz~ recodecr , data = data,  index = c("ID","year"), 
           method = "fe", force = "two-way", se=TRUE, nboots=200, na.rm=TRUE, loo=TRUE)


# maths

plot(m5, type = "equiv", 
     cex.legend = 0.6, main = "Testing Pre-Trend (FEct)", cex.text = 0.8)

plot(m5a, type = "equiv", loo = TRUE,
     cex.legend = 0.6, main = "Testing Pre-Trend (FEct)", cex.text = 0.8)

# reading

plot(m6, type = "equiv",  
     cex.legend = 0.6, main = "Testing Pre-Trend (FEct)", cex.text = 0.8)

plot(m6a, type = "equiv",  loo = TRUE,
     cex.legend = 0.6, main = "Testing Pre-Trend (FEct)", cex.text = 0.8)


################## PLACEBO TEST ###############################


# Also uses only data prior to the treatment. 
# Create fake treatment with randomization of 0 and 1.

random_binary <- function(n, p){
  # p is the proportion of 1s
  x <- c(rep(1, times=n * p), rep(0, times=n * (1 - p)))
  x[sample(length(x))] # or sample(x)
}

# Assign it to half of the sample
data$random_treatment<-random_binary(24910, 0.5)

# Run models with fake treatment
m1<- fect(cognz~ random_treatment, data = data,  index = c("ID","YEAR"), 
            method = "fe", force = "two-way", se=TRUE, nboots=1000, na.rm=TRUE)

print(m1)

################## LOG TRANSFORMATION ###############################
cognitive<-data$cognz
hist(cognitive)
log_cognitive<-data$cognzlog
hist(log_cognitive)
hist(data$cognz)
data$cognzlog<-log(data$cognz+1, base=exp(1))
data$cognzlogasinh<-asinh(data$cognz)
hist(data$cognz)
hist(data$cognzlog)
hist(data$cognzlogasinh)
summary(data$cognzlog)
sd(data$cognzlog, na.rm=TRUE)
# OLS
m1<-feols(cognzlog~mathdecr, data=data)
etable(m1)

m2<-feols(cognzlog~recodecr, data=data)
etable(m2)

#TWFE
m3 <- feols(cognzlog~mathdecr + highestgrade| ID + year , data)
etable(m3)

m4 <- feols(cognzlog~recodecr + highestgrade| ID + year, data)
etable(m4)

#FEct

m5 <- fect(cognzlog~ mathdecr, data = data,  index = c("ID","year"), 
           method = "fe", force = "two-way", se=TRUE, 
           nboots=200, na.rm=TRUE)

plot(m5, main = "Estimated ATT (FEct)", ylab = "Effect of mathematical decline in ability on maternal conitive stimulation", 
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8, proportion = 0, count=FALSE)

print(m5)

m6 <- fect(cognzlog~ recodecr , data = data,  index = c("ID","year"), 
           method = "fe", force = "two-way", se=TRUE, nboots=200, na.rm=TRUE)

plot(m6, main = "Estimated ATT (FEct)", ylab = "Effect of reading ecline in ability on maternal conitive stimulation",  
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8, proportion=0)

print(m6)

#################### AGE HETEROGENEITY #############################

summary(data$yearsatint)
data$agecat<-data$yearsatint
data$agecat[data$agecat<=9]<-0 # early years
data$agecat[data$agecat>9 & data$agecat<=10.84 ]<-1 # medium years
data$agecat[data$agecat>10.84 & data$agecat<=12.67 ]<-2 # medium-later years
data$agecat[data$agecat>12.67]<-3 # later years
table(data$agecat)


#TWFE with AGE INTERACTION
m7 <- feols(cognz~mathdecr*factor(agecat)+ highestgrade| ID + year, data)
etable(m7)

m8 <- feols(cognz~recodecr*factor(agecat)+ highestgrade| ID + year, data)
etable(m8)

# no heterogeneity 

#################### AGE instead of year FE #############################


m9 <- feols(cognz~mathdecr + highestgrade| ID + ageatint , data)
etable(m9)

m10 <- feols(cognz~recodecr + highestgrade| ID + ageatint, data)
etable(m10)

# doesn't change

#################### INCREASES INSTEAD OF DECREASES #############################

# Maths 
data<-data %>%
  arrange(ID, year)%>%
  group_by(ID)%>%
  mutate(diff=c(NA, diff(mathz))) 

data$dmathz<-data$diff

data$mathincr<-data$dmathz
data$mathincr[data$mathincr>0]<-1
data$mathincr[data$mathincr<=0]<-0
table(data$mathincr)

# Reading
data<-data %>%
  arrange(ID, year)%>%
  group_by(ID)%>%
  mutate(diff=c(NA, diff(recoz)))

data$drecoz<-data$diff

data$recoincr<-data$drecoz
data$recoincr[data$recoincr>0]<-1
data$recoincr[data$recoincr<=0]<-0
table(data$recoincr)

# Models 

m1 <- feols(cognz~mathincr  + highestgrade | ID + year , data)
etable(m1)

m2 <- feols(cognz~recoincr + highestgrade| ID + year, data)
etable(m2)

#################### LAGS OF INCREASES: REGRESSION TO THE MEAN   #############################

# Mathematics

data$ability<-rowMeans(cbind(data$dmathz, data$drecoz ))

data <- data %>%
  group_by(ID) %>%
  mutate(lagability1 = dplyr::lag(ability, 1))%>%
  mutate(lagability2 = dplyr::lag(ability, 2))

m3 <- feols(cognz~mathdecr + lagability1 | ID + year , data)
etable(m3)

m4 <- feols(cognz~mathdecr + lagability2| ID + year , data)
etable(m4)

m5 <- feols(cognz~recodecr+ lagability1| ID + year , data)
etable(m5)

m6 <- feols(cognz~recodecr + lagability2 | ID + year , data)
etable(m6)


#################### INCLUDING TIME VARYING VARIABLES   #############################

table(data$separation)
table(data$region)
summary(data$famsize)
data$nchildren<-data$famsize-2
summary(data$nchildren)
sd(data$nchildren, na.rm=TRUE)

# MARITAL STATUS


m3 <- feols(cognz~mathdecr  + highestgrade + separation | ID + year , data)
etable(m3)

m4 <- feols(cognz~recodecr  + highestgrade + separation| ID + year, data)
etable(m4)


# REGION

m3 <- feols(cognz~mathdecr  + highestgrade + factor(region) | ID + year , data)
etable(m3)

m4 <- feols(cognz~recodecr  + highestgrade + factor(region)| ID + year, data)
etable(m4)


# HAVING MORE CHILDREN

m3 <- feols(cognz~mathdecr  + highestgrade + famsize | ID + year , data)
etable(m3)

m4 <- feols(cognz~recodecr  + highestgrade + famsize| ID + year, data)
etable(m4)


################### INTERACTIONS WITH TOTAL LEVEL OF ABILITY #################

data$totalabmath<-as.factor(ntile(data$mathz, 4))
data$totalabreco<-as.factor(ntile(data$recoz, 4))

m3 <- feols(cognz~mathdecr*totalabmath | ID + year , data)
etable(m3)

m4 <- feols(cognz~recodecr*totalabreco| ID + year, data)
etable(m4)


################### CORRELATIONS WITH SCHOOLS TEST #################

cor(data$mathz, data$mathtest, use='pairwise.complete.obs')
cor(data$recoz, data$languagetest, use='pairwise.complete.obs')
cor(data$recoz, data$readingtest, use='pairwise.complete.obs')


################### DIFFERENT INTENSITIES OF THE TREATMENT #################

table(data$dmathz)
data$negdmath<-data$dmathz
data$negdmath[data$negdmath>=0]<-NA
data$negdmath<-data$negdmath*(-1)
table(data$negdmath)
data$quantilesmath<-as.factor(ntile(data$negdmath, 4))
table(data$quantilesmath)

m3 <- feols(cognz~quantilesmath | ID + year , data)
etable(m3)

table(data$drecoz)
data$negdreco<-data$drecoz
data$negdreco[data$negdreco>=0]<-NA
data$negdreco<-data$negdreco*(-1)
table(data$negdreco)
data$quantilesreco<-as.factor(ntile(data$negdreco, 4))
table(data$quantilesreco)

m4 <- feols(cognz~quantilesreco| ID + year , data)
etable(m4)

