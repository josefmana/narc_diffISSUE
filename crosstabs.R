# This is a script that extracts crosstabs (contingency tables) of difficulties associated with narcolepsy w.r.t. moderators.

# list required packages into a character object
pkgs <- c( "rstudioapi", "dplyr", "tidyverse", "ggplot2", "openxlsx" )

# load or install packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# set working directory (works only in RStudio)
setwd( dirname(rstudioapi::getSourceEditorContext()$path) )

# read the data sets for analysis
d <- read.csv( "_nogithub/data/df.csv", sep = ";" ) %>%
  # re-format where needed
  mutate( `Lék...který` = as.character(`Lék...který`) ) %>%
  mutate( label = ifelse( label == "nic.neobtěžuje", "No problem reported", label ) )
n <- read.csv( "_nogithub/data/nms.csv", sep = ";" ) # names mapping

# list all moderators of interest as weel as all diagnoses under study
mods <- names(d)[ c(3,5,8,9,10,11,14,16,17,19,21,23,25,27,28,30) ]
dg <- levels( as.factor( d$dg ) )
is <- na.omit( unique( d$label ) )


# ---- crosstabs extraction ----

# extract crosstabs for each issue per diagnosis and each moderator separately
ct <- lapply( setNames( mods, mods ), function(i) with( d, table( label, get(i), dg ) ) %>% array( . , dim = dim(.), dimnames = dimnames(.) ) )

# get rid of columns that are not defined (such as cataplexy frequency in NT1 or IH)
ct$přítomnost.kataplexie <- ct$přítomnost.kataplexie[ , , "NT1"] # cataplexy defined for NT1 only
ct$frekvence.kataplexií <- ct$frekvence.kataplexií[ , , "NT1"] # ditto
ct$NSS.stupeň <- ct$NSS.stupeň[ , , c("NT1","NT2") ] # NSS ain't used in IH

# prepare a folder for crosstabs
if( !dir.exists("tabs/crosstabs") ) dir.create("tabs/crosstabs")

# save all crossatab arrays as .rds
saveRDS( ct, "tabs/crosstabs/crosstabs.rds" )

# loop through all crosstabs to create tabs for Fischer tests (i.e., marginal and combined crosstabs)
cross <- lapply( setNames(mods, mods),
                 function(i)
                   
                   # prepare a list with all marginal and combined contingency tables
                   list( # marginal crosstabs
                         if( grepl("kata",i) ) rowSums( ct[[i]] ) %>% as.data.frame() else apply( ct[[i]], c(1,3), sum ), # marginal diagnosis
                         if( grepl("kata",i) ) ct[[i]] else apply( ct[[i]], c(1,2), sum ), # marginal moderator "i"
                         
                         # combined crosstabs
                         if ( grepl("kata",i) ) ct[[i]] # for cataplexy marg_mod == comb
                         else lapply( 1:dim(ct[[i]])[2], function(j) ct[[i]][ , j , ] ) %>% # combined diagnosis/moderator columns
                           # glue each moderator level specific table together
                           do.call( cbind, . ) %>%
                           # rename columns
                           `colnames<-`( expand.grid( dimnames(ct[[i]])[[3]], dimnames(ct[[i]])[[2]] ) %>% apply( . , 1 , paste, collapse = "_" ) )
                         
                         # add names to the sublists
                         ) %>% `names<-`( c("marg_dia","marg_mod","comb") )
                 )

# add simple contrasts within diagnoses and moderator levels for further description
for ( i in mods ) {
  
  # for cataplexy continue right away
  if ( grepl("kata",i) ) next
  
  # for all remaining extract simple contrasts
  else {
    
    # adding diagnosis-specific and moderator level-specific tables
    for ( j in dimnames(ct[[i]])[[3]] ) cross[[i]][[ paste0("dia_",j) ]] <- ct[[i]][ , , j ]
    for ( j in dimnames(ct[[i]])[[2]] ) cross[[i]][[ paste0("mod_",j) ]] <- ct[[i]][ , j , ]
   
  }
}

# calculate column percentages for all tables
perc <- lapply( setNames( names(cross), names(cross) ),
                # loop through all moderators
                function(i)
                  # loop through all tables within each moderator
                  lapply( setNames( names(cross[[i]]), names(cross[[i]]) ),
                          # calculate column percentages
                          function(j) t( t( 100 *cross[[i]][[j]] ) / colSums( cross[[i]][[j]] ) )
                          )
                  )

# save combined "cross (perc %)" tables as .xlsx
for ( i in mods ) {
  
  write.xlsx(
    
    # prepare the table
    lapply( setNames( names(cross[[i]]), names(cross[[i]]) ),
            # loop through moderators
            function(j) sapply( colnames( cross[[i]][[j]] ),
                                # loop through columns
                                function(k)
                                  paste0( cross[[i]][[j]][ ,k], " (", sprintf( "%.2f", round( perc[[i]][[j]][ ,k], 2 ) ), " %)" )
                                )
            ),

    # save it
    file = paste0("tabs/crosstabs/",i,".xlsx"), rowNames = T
    
  )
}


# ---- crosstabs figures ---

# prepare an order for figures (the most common problem across diagnoses in the bottom)
o <- ( table(d$label) %>% as.data.frame() %>% arrange( desc(Freq) ) %>% select( Var1 ) %>% c() )$Var1

# set ggplot theme
theme_set( theme_minimal(base_size = 20) )

# start by saving the plot for marginal distribution of issues by diagnosis
as.data.frame(perc$F.1..M.2.$marg_dia) %>%
  rownames_to_column( var = "Issue" ) %>%
  pivot_longer( -1, names_to = "Predictor value", values_to = "Column\nperc." ) %>%
  mutate( Issue = factor( Issue, levels = o, ordered = T ) ) %>%
  ggplot( aes( x = `Predictor value`, y = Issue, fill = `Column\nperc.` ) ) +
  geom_tile( color = "white" ) +
  scale_fill_gradient( low = "white", high = "steelblue" ) +
  labs( y = NULL, x = "Diagnosis (NT1,NT2, IH)" ) + theme( axis.title.x = element_text( face = "bold" ) )

# save it
ggsave( "figs/Diagnosis (NT1,NT2, IH).jpg", dpi = 300, width = 13.1, height = 14.3 )

# next loop through all moderators and plot their marginal and interaction distributions
for ( i in mods ) {
  for ( j in c("marg_mod","comb") ) {
    
    # plot it
    as.data.frame( perc[[i]][[j]] ) %>%
      rownames_to_column( var = "Issue" ) %>%
      pivot_longer( -1, names_to = "Predictor value", values_to = "Column\nperc." ) %>%
      mutate( Issue = factor( Issue, levels = o, ordered = T ) ) %>%
      ggplot( aes( x = `Predictor value`, y = Issue, fill = `Column\nperc.` ) ) +
      geom_tile( color = "white" ) +
      scale_fill_gradient( low = "white", high = "steelblue" ) +
      labs( y = NULL, x = paste0( ifelse( j == "comb", "Diagnosis / ", "" ), n[ n$X2 == i , "X1" ] ) ) +
      theme( axis.title.x = element_text( face = "bold" ) )
    
    # save it
    ggsave( paste0( "figs/", i, "_", j, ".jpg" ), dpi = 300, width = 13.1, height = 14.3 )
    
  }
}


# ---- statistical analysis ----

# if there are no p-values calculated yet, calculate Fisher test from a scratch
if ( !file.exists("tabs/fisher_test.csv") ) {
  
  # calculate two hundred p-values based on one hundred thousands replicates of Monte Carlo Fisher test for count data
  # as implemented in base R
  p <- list( marg_dia = sapply( 1:200, function(i) fisher.test( cross$F.1..M.2.$marg_dia, simulate.p.value = T, B = 1e5 )$p.value ) %>% as.data.frame() %>% mutate( var = "dia", type = "marg" ) )
  
  # add p-values for marginal moderator crosstabs and combined moderator/diagnosis crosstabs
  for ( i in mods ) p[[i]] <- lapply(
    
    # loop through all marginal and combined crosstabs
    setNames( c("marg_mod","comb"), c("marg_mod","comb") ),
    function(j)
      
      # for each moderator simulate 200 p-values based on one hundred thousands replicates of Monte Carlo Fisher rest for count data  
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
  filter( !( grepl("kata",var) & type == "comb") ) %>% # keep only marginal cataplexy results (valid for NT1 only)
  group_by( var, type ) %>% # split by crosstabs
  slice( which.max(p) ) %>% # keep only the maximum p-value
  mutate( 'p < .05' = ifelse( p < .05, "*", "" ), 'sig.' = ifelse( p < ( .05/nrow(.) ), "*", "" ) ) %>%
  relocate( p, .after = type )

# save the table
write.table( tab, "tabs/table_5_fisher_test_results.csv", sep = ",", quote = F, row.names = F )


# ---- post-hoc exploration of issues distribution by diagnosis/sex ----

# prepare issue-specific crosstabs manually (because there's way too many )
crsexp <- lapply( setNames(is,is), function(i) data.frame( `1` = rep(NA,3), `2` = rep(NA,3), row.names = dg ) )

# fill-in the values
for ( i in is ) for ( j in dg ) for ( k in 1:2 ) crsexp[[i]][j,k] <- d[ with( d, label == i & dg == j & F.1..M.2. == k ) , 2 ] %>% na.omit() %>% length()

# save the crosstabs as xlsx
write.xlsx( crsexp %>% `names<-`( substr( names(crsexp), 1, 31 ) ), file = "tabs/crosstabs_diasex.xlsx", rowNames = T )

# prepare a tab for Fisher test results
tab2 <- sapply( is, function(i) c( issue = i, p = fisher.test( crsexp[[i]] )$p.value ) ) %>%
  t() %>% as.data.frame() %>%
  mutate( 'p < .05' = ifelse( p < .05, "*", "" ), 'sig.' = ifelse( p < ( .05/nrow(.) ), "*", "" ) )


# ---- saving Fisher tests results ----

# save both series of analyses into one excel file
write.xlsx( list( all = tab, diasex = tab2 ), "tabs/Fisher_tests.xlsx", rowNames = F )


# ---- write down session info ----

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = "sess/crosstabs.txt" )
