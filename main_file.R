
rm(list=ls())

####################################################
# All three files
#  - main_file.R
#  - jd_calculation.R
#  - latex_compilation.R
# must be saved in the same directory. After user specific customisation, the execution of this script
# produces a LaTeX and a PDF file in the same directory with a summary of the comparison. Time series
# specific details are not reported but are available in the objects "tab2" and "tab3" in the R environment.



#####################################################
#### Necessary items to be specified by the user ####
#####################################################

# Set path for workspace
workspace   <- "Example.xml"                      # path to workspace
# data_file   <- "Example.rda"                       # Alternative: access data via rds-file (ts-format required)

# Information on author & institution
Country     <- "Germany"                          # Name of your country
Institute   <- "Deutsche Bundesbank"              # Name of your institution
Department  <- "General Economic Statistics"      # Department's name
Author      <- "Jan Heller"                       # Author's name

# Information on the investigated indicator
INDICATOR    <- "Output in Industry"              # The investigated indicator (ONLY alphanumeric characters and blanks)
addINFO      <- ""                                # Additional information
                                                    # Information, you would like to integrate in the pdf report for a better understanding
                                                    # of the comparision of your data

# Specification settings
METHOD       <- "X13"                             # Which Method is used in the Test: X13 or TS
SPEC         <- "RSA5"                            # Name of default specification
                                                    # Elective specifications for X13: {X11, RSA0, RSA1, RSA2, RSA3, RSA4, RSA5}
                                                    # Elective specifications for TS:  {RSA0, RSA1, RSA2, RSA3, RSA4, RSA5, RSAfull}
# Name of the pdf-output
OUTPUTNAME   <- "BBk_JD+3 comparison"            # First part of name of the LaTeX and PDF file



###################################################################################
#### Additional options for the calculation of the measures in the sumary table####
###################################################################################

TRIM     = 0.00     # trimmed mean
DIGI     = 2        # number of digits in tables



#################################################
#### Calculation & LaTeX and PDF compilation ####
#################################################

source("jd_calculation.R")                        # Calculation of the versions comparison
source("latex_compilation.R")                     # Compilation of LaTeX and PDF file
