

## load required packages ############################
    library(xtable)
    options(xtable.comment = FALSE)
##  Institution and time series profile  ############################

  # additional modules for institution profile (first table) not to be manually entered by user
    SoftwareOLD  <- "JD+ 2.2"                         # The old software used as benchmark
    SoftwareNEW  <- "JD+ 3.0"                         # The new software to be analysed
    SoftwareUsed <- paste0(SoftwareNEW," and ",SoftwareOLD)

   # Periodicity of the investigated series
    if (length(frequ)==1) {
      if (frequ==12){
        Periodicity  <-  "monthly"
      } else if (frequ==4){
        Periodicity  <-  "quarterly"
      } else if (frequ==2){
        Periodicity  <-  "biannualy"
      } else {
        Periodicity  <-  "other"
      }
    } else {
      Periodicity  <-  "mixed"
    }
    NoTS        <-  length(ts)                       # Nomber of time series that are seasonally adjusted
    Domain      <- INDICATOR  <-  gsub("[^0-9A-Za-z]","" , INDICATOR ,ignore.case = TRUE)

    # Transformation
    if(METHOD == "X13" & SPEC == "RSA2") SPEC <- "RSA2c"
    if(METHOD == "X13" & SPEC == "RSA4") SPEC <- "RSA4c"
    if(METHOD == "X13" & SPEC == "RSA5") SPEC <- "RSA5c"

    if (SPEC=="RSA0" | SPEC=="X11") {
      TRANSFORM    <- "NA"
    } else {
      TRANSFORM    <- "Auto"
    }

    # ARIMA
    if (SPEC=="X11") {
      ARIMAMDL     <- "NA"
    } else if (SPEC=="RSA1" | SPEC=="RSA2" | SPEC=="RSA2c" | SPEC=="RSA3"){
      ARIMAMDL     <- "Airline + mean"
    } else {
      ARIMAMDL     <- "Auto"
    }

    # Calendar adjustment
    if (SPEC=="X11" | SPEC=="RSA0" | SPEC=="RSA1" | SPEC=="RSA2" | SPEC=="RSA2c" | SPEC=="RSA3") {
      CALENDAR     <- "No"
    } else {
      CALENDAR     <- "Yes"
    }

    # Regressors
    if (SPEC=="X11" | SPEC=="RSA0" | SPEC=="RSA1" | SPEC=="RSA3") {
      REGRESSORS     <- "NA"
    } else if  (SPEC=="RSA2" | SPEC=="RSA2c" | SPEC=="RSA4" | SPEC=="RSA4c"){
      REGRESSORS     <- "2 TD + Easter"
    } else if (SPEC=="RSA5" | SPEC=="RSA5c"){
      REGRESSORS     <- "7 TD + Easter"
    } else {
      REGRESSORS     <- "Auto"
    }

    # Outlier
    OUTLIERSPRE  <- "No"                              # Pre-Specified outliers used?
    if (SPEC=="RSA0" | SPEC=="X11") {                 # Automatic outlier detection
      OUTLIERDET   <- "NA"
    } else {
      OUTLIERDET   <- "AO/LS/TC"
    }

    # Seasonal and trend filters applied in X 11
    if (METHOD=="X13") {
      SEASFIL      <- "Auto"                            # Seasonal filters applied in X 11
      HENDERSON    <- "Auto"                            # Henderson filter pre specified or AUTO
    } else {
      SEASFIL      <- "NA"                            # Seasonal filters applied in X 11
      HENDERSON    <- "NA"                            # Henderson filter pre specified or AUTO
    }


    if (TRIM > 0) {
    IndicatorsUsed1 <- paste0(100*TRIM,"% trimming: the ",100*TRIM,"% lowest and highest values are not taken into account in the")
    IndicatorsUsed2 <- c("computation of the mean")
    } else {
      IndicatorsUsed1 <- "None"
      IndicatorsUsed2 <- ""
    }
    Author2 <- paste0(Author, " (", Institute,"), ",
                      paste0(month.name[as.numeric(format(Sys.time(), '%m'))]," ",
                             format(Sys.time(), '%d'),", ",format(Sys.time(), '%Y')))



  # Institution and time series profile
    tabQu1 <- data.frame( Name = c("Country",
                                   "Domain",
                                   "Periodicity",
                                   "Number of series",
                                   "Method used",
                                   "Spec",
                                   "Transform",
                                   "ARIMA model",
                                   "Calendar adjustment",
                                   "Regressors",
                                   "Pre-specified outliers",
                                   "Outlier detection",
                                   "Seasonal filters",
                                   "Henderson filter",
                                   "Trimmed mean",
                                   " ",
                                   "Additional Information"
                                   ),
                         Character = c(Country,
                                       Domain,
                                       Periodicity,
                                       NoTS,
                                       METHOD,
                                       SPEC,
                                       TRANSFORM,
                                       ARIMAMDL,
                                       CALENDAR,
                                       REGRESSORS,
                                       OUTLIERSPRE,
                                       OUTLIERDET,
                                       SEASFIL,
                                       HENDERSON,
                                       IndicatorsUsed1,
                                       IndicatorsUsed2,
                                       addINFO))

    if (exists("ts_warn") & WARN == TRUE) {
      ts_warn_vector <- unlist(strsplit(ts_warn, "[.]"))
      tabQu1b <- data.frame(Name = c("WARNING", rep("", length(ts_warn_vector)-1)),
                            Character = ts_warn_vector)
      tabQu1 <- rbind(tabQu1, tabQu1b)
      }

    tabQu <- toLatex(xtable(tabQu1, caption="Details on procedure", align="ll|l"),caption.placement = "top",
                         include.rownames=F,include.colnames = F)


## tabSMRY: summary of seasonally as well as calendar adjusted results ############################
    add.to.row <- list(pos = list(0), command = NULL)
    command <- paste0("\\hline\n\\endhead\n", "\\hline\n", "\\multicolumn{", dim(tab1)[2], "}{l}",
                      "{\\footnotesize Continued on next page}\n", "\\endfoot\n", "\\endlastfoot\n")
    add.to.row$command <- command

    tabSMRYa<- toLatex(xtable(tab1, digits = DIGI, caption=paste0("Summary table for ",INDICATOR," using ", METHOD, " specification ", SPEC), align = "ll|r"),
                       caption.placement = "top",include.rownames=F,include.colnames = T)


## LaTEX Document ############################
  # output - as pdf
  options(xtable.comment = FALSE);  Sys.setlocale("LC_ALL", "German"); options(encoding = "UTF-8")
  latexname <- paste(format(Sys.time(), '%Y%m%d')," ",OUTPUTNAME," for ",INDICATOR,".tex", sep="")


  cat(paste("\\documentclass{article}", " ")                                                                 ,fill=T, file = latexname)
  cat(paste("\\usepackage{graphicx} ", " ")                                                                  ,fill=T, file = latexname, append=T)
  cat(paste("\\usepackage[hmargin=2.0cm,vmargin=2.0cm,bindingoffset=0.5cm]{geometry}")                       ,fill=T, file = latexname, append=T)
  cat(paste("\\usepackage{longtable}", " ")                                                                  ,fill=T, file = latexname, append=T)
  cat(paste("\\usepackage{caption} ", " ")                                                                   ,fill=T, file = latexname, append=T)
  cat(paste("\\captionsetup[table]{labelformat=empty, textfont=bf, singlelinecheck=off, aboveskip=1pt}", " "),fill=T, file = latexname, append=T)
  cat(paste("\\usepackage{amsmath}", " ")                                                                    ,fill=T, file = latexname, append=T)
  cat(paste("\\begin{document}", " ")                                                                        ,fill=T, file = latexname, append=T)
  cat(paste("\\renewcommand{\\arraystretch}{1.1}")                                                           ,fill=T, file = latexname, append=T)

  cat(paste0("\\begin{center}
             \\Large\\textbf{TIME SERIES ANALYSIS and SEASONAL ADJUSTMENT USER GROUP (TSAUG)}               \\newline
             \\end{center}
             \\begin{center}
             \\Large\\textbf{Comparison between ",paste0(SoftwareOLD," and ",SoftwareNEW),"}                \\newline
             \\end{center}
             \\begin{center}\\large{",Author2,"}\\end{center}
             \\vspace{1em}                                                                                 "),fill=T, file = latexname, append=T)

  # cat("\\vspace{1em}"                                                                                        ,fill=T, file = latexname, append=T)
  cat(tabQu                                                                                                  ,fill=T, file = latexname, append=T)
  # cat("\\vspace{1em}"                                                                                        ,fill=T, file = latexname, append=T)
  cat(tabSMRYa                                                                                               ,fill=T, file = latexname, append=T)

  cat("\\end{document}"                                                                                      ,fill=T, file = latexname, append=T)

  tools::texi2pdf(latexname, clean = TRUE)

