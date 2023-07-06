# This is a script that extracts crosstabs (contingency tables) of difficulties associated with narcolepsy w.r.t. moderators.

# list required packages into a character object
pkgs <- c( "rstudioapi", "dplyr", "tidyverse", "opnexlsx" )

# load or install packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# set working directory (works only in RStudio)
setwd( dirname(rstudioapi::getSourceEditorContext()$path) )

# read the data sets for analysis
d <- read.csv( "_nogithub/data/df.csv", sep = ";" )

# list all moderators of interest
mods <- names(d)[ c(3,5,8,9,11,14,16,17,19,21,23,25,27,28,30) ]


# ---- crosstabs extraction ----

# extract crosstabs for each issue per diagnosis and each moderator separately
ct <- lapply( setNames( mods, mods ), function(i) with( d, table( label, get(i), dg ) ) %>% array( . , dim = dim(.), dimnames = dimnames(.) ) )


# ---- plan ----

# 1) save ct as .rds
# 2) save each ct sublist to an excel
# 2.1) save a list for each one
# 2.1.1) marginal frequencies for diagnoses via apply( ct[[i]], c(1,3) sum )
# 2.1.2) marginal frequencies for the moderator via apply( ct[[i]], c(1,2), sum )
# 2.1.3) simple frequencies for the issue/moderator on each level of diagnosis (3 lists)
# 2.1.4) simple frequencies for the issue/diagnosis on each level of moderator ( dim(ct[[i]])[2] lists )
# 3) perform Fisher's (not really exact) test for each table from 2.1.1-2.1.4

# alternatively (and maybe preferably): comined dg and mod to create a new variable that will be compared via Fisher's test in 3) and
# saved before 2.1.3) via cbind( ct[[i]][ , 1 , ], ct[[i]][ , 2 , ] )
