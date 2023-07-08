# This is a script that extracts crosstabs (contingency tables) of difficulties associated with narcolepsy w.r.t. moderators.

# list required packages into a character object
pkgs <- c( "rstudioapi", "dplyr", "tidyverse", "openxlsx" )

# load or install packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# set working directory (works only in RStudio)
setwd( dirname(rstudioapi::getSourceEditorContext()$path) )

# read the data sets for analysis
d <- read.csv( "_nogithub/data/df.csv", sep = ";" )

# list all moderators of interest as weel as all diagnoses under study
mods <- names(d)[ c(3,5,8,9,11,14,16,17,19,21,23,25,27,28,30) ]
dg <- levels( as.factor( d$dg ) )


# ---- crosstabs extraction ----

# extract crosstabs for each issue per diagnosis and each moderator separately
ct <- lapply( setNames( mods, mods ), function(i) with( d, table( label, get(i), dg ) ) %>% array( . , dim = dim(.), dimnames = dimnames(.) ) )

# prepare a folder for crosstabs
if( !dir.exists("tabs/crossatabs") ) dir.create("tabs/crosstabs")

# save all crossatab arrays as .rds
saveRDS( ct, "tabs/crosstabs/crosstabs.rds" )

# loop through all crosstabs to create tabs for Fischer tests (i.e., marginal and combined crosstabs)
cross <- lapply( setNames(mods, mods),
                 function(i)
                   
                   # prepare a list with all marginal and combined contingency tables
                   list( marg_dia = apply( ct[[i]], c(1,3), sum ), # marginal diagnosis
                         marg_mod = apply( ct[[i]], c(1,2), sum ), # marginal moderator i
                         comb = lapply( 1:dim(ct[[i]])[2], function(j) ct[[i]][ , j , ] ) %>% # combined diagnosis/moderator columns
                           do.call( cbind, . ) %>% # glue each moderator level specific table together
                           `colnames<-`( expand.grid( dimnames(ct[[i]])[[3]], dimnames(ct[[i]])[[2]] ) %>% # rename columns
                                           apply( . , 1 , paste, collapse = "_" ) )
                         ) )

# add simple contrasts within diagnoses and moderator levels for further description and save resulting tables as .xlsx
for ( i in mods ) {
  
  # adding diagnosis-specific and moderator level-specific tables
  for ( j in dg ) cross[[i]][[ paste0("dia_",j) ]] <- ct[[i]][ , , j ]
  for ( j in dimnames(ct[[i]])[[2]] ) cross[[i]][[ paste0("mod_",j) ]] <- ct[[i]][ , j , ]
  
  # saving the table as .xlsx
  write.xlsx( cross[[i]], file = paste0("tabs/crosstabs/",i,".xlsx"), rowNames = T )
  
}


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
