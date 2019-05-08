#=======================================================================

# Rattle is Copyright (c) 2006-2018 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2019-05-08 12:27:47 x86_64-w64-mingw32 

# Rattle version 5.2.0 user 'Acer'

# This log captures interactions with Rattle as an R script. 

# For repeatability, export this activity log to a 
# file, like 'model.R' using the Export button or 
# through the Tools menu. Th script can then serve as a 
# starting point for developing your own scripts. 
# After xporting to a file called 'model.R', for exmample, 
# you can type into a new R Console the command 
# "source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 
 
# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access the weather dataset and utilities.
library(magrittr) # Utilise %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#=======================================================================
# Rattle timestamp: 2019-05-08 12:28:13 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///C:/Users/Acer/Desktop/svyasa/R/Datasets/VapourLiquidEquilibrium.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2019-05-08 12:28:13 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=11 train=8 validate=2 test=1

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("XMeOH", "YMeOH")

crs$numeric   <- c("XMeOH", "YMeOH")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 12:28:17 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=11 train=8 validate=2 test=1

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- "XMeOH"

crs$numeric   <- "XMeOH"

crs$categoric <- NULL

crs$target    <- "YMeOH"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 12:28:21 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

#=======================================================================
# Rattle timestamp: 2019-05-08 12:28:36 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for XMeOH

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(XMeOH) %>%
  ggplot2::ggplot(ggplot2::aes(x=XMeOH)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("XMeOH\n\nRattle 2019-May-08 12:28:36 Acer") +
  ggplot2::ggtitle("Distribution of XMeOH (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for YMeOH

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(YMeOH) %>%
  ggplot2::ggplot(ggplot2::aes(x=YMeOH)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("YMeOH\n\nRattle 2019-May-08 12:28:36 Acer") +
  ggplot2::ggtitle("Distribution of YMeOH (sample)") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02)

#=======================================================================
# Rattle timestamp: 2019-05-08 12:28:48 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$train, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation VapourLiquidEquilibrium.csv using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2019-05-08 12:29:02 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(YMeOH ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs

# Plot the model evaluation.

ttl <- genPlotTitleCmd("Linear Model",crs$dataname,vector=TRUE)
plot(crs$glm, main=ttl[1])
