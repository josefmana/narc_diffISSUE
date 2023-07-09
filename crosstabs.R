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
if( !dir.exists("tabs/crosstabs") ) dir.create("tabs/crosstabs")

# save all crossatab arrays as .rds
saveRDS( ct, "tabs/crosstabs/crosstabs.rds" )

# loop through all crosstabs to create tabs for Fischer tests (i.e., marginal and combined crosstabs)
cross <- lapply( setNames(mods, mods),
                 function(i)
                   
                   # prepare a list with all marginal and combined contingency tables
                   list( # marginal crosstabs
                         marg_dia = apply( ct[[i]], c(1,3), sum ), # marginal diagnosis
                         marg_mod = apply( ct[[i]], c(1,2), sum ), # marginal moderator "i"
                         
                         # combined crosstabs
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


# ---- statistical analysis ----

# if there are no p-values calculated yet, calculate Fisher test from a scratch
if( !file.exists("tabs/fisher_test.csv") ) {
  
  # calculate two hundred p-values based on one hundred thousands replicates of Monte Carlo Fisher test for count data
  # as implemented in base R
  p <- list( marg_dia = sapply( 1:200, function(i) fisher.test( cross$F.1..M.2.$marg_dia, simulate.p.value = T, B = 1e5 )$p.value ) %>% as.data.frame() %>% mutate( var = "dia", type = "marg" ) )
  
  # add p-values for marginal moderator crosstabs and combined moderator/diagnosis crosstabs
  for( i in mods ) p[[i]] <- lapply(
    
    # loop through all marginal and combined crosstabs
    setNames( c("marg_mod","comb"), c("marg_mod","comb") ),
    function(j)
      
      # for each moderator simulate 200 p-values based on one hundred thousands replicates of Monte Carlos Fisher rest for count data  
      sapply( 1:200, function(k) fisher.test( cross[[i]][[j]], simulate.p.value = T, B = 1e5 )$p.value ) %>%
      as.data.frame() %>%
      mutate( var = i, type = sub( "_.*", "", j ) )
    
  ) %>% do.call( rbind.data.frame, . ) # collapse results within each moderator
  
  # pull all evaluated variables into a single table
  p <- do.call( rbind.data.frame, p ) %>% rename( "p" = "." ) %>% `rownames<-`( 1:nrow(.) )
  
  # save the results
  write.table( p, "tabs/fisher_test.csv", sep = ",", quote = F, row.names = F )

# otherwise, read the saved results to save computational resources
} else p <- read.csv( "tabs/fisher_test.csv", sep = "," )

# prepare a table with maximal p-value (the most conservative one) for each tested variable
tab <- p %>%
  group_by( var, type ) %>% # split by crosstabs
  slice( which.max(p) ) %>% # keep only the maximum p-value
  mutate( 'p < .05' = ifelse( p < .05, "*", "" ), 'sig.' = ifelse( p < ( .05/nrow(.) ), "*", "" ) ) %>%
  relocate( p, .after = type )

# save the table
write.table( tab, "tabs/table_5_fisher_test_results.csv", sep = ",", quote = F, row.names = F )


# ---- write down session info ----

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = "sess/crosstabs.txt" )
