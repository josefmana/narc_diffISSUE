# set working directory (works only in RStudio)
setwd( dirname(rstudioapi::getSourceEditorContext()$path) )

# list packages to use
pkgs <- c( "tidyverse", "dplyr", # data wrangling
           "ggplot2", "patchwork", # plotting
           "performance", # model checking
           "openxlsx" # reading and writing .xlsx
           )

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# prepare a folder for tables, figures, models, and sessions info
sapply( c("tabs","figs","mods","sess"), function(i) if( !dir.exists(i) ) dir.create(i) )


# ---- import raw data ----

# read the data from .xlsx
d0 <- list(
  nt1 = read.xlsx( "_no_github/diff-issue.xlsx", sheet = "NT1" ),
  nt2 = read.xlsx( "_no_github/diff-issue.xlsx", sheet = "NT2" ),
  ih = read.xlsx( "_no_github/diff-issue.xlsx", sheet = "IH" )
)

# read the legend
d.leg <- read.xlsx( "_no_github/diff-issue.xlsx", sheet = "kódování - difficult issues" ) %>%
  t() %>% as.data.frame() %>% rownames_to_column( var = "0" ) %>% # add the null category
  t() %>% as.data.frame() %>% `colnames<-`( c("index","label") ) %>% # tidy
  mutate( index = sub( " ", "", index) %>% as.integer() ) # make sure the coding is in the right format


# ---- prepare data frame suitable for description ----

# rename NA IDs in d0$nt1 to dodge problems
d0$nt1[ is.na(d0$nt1$record.ID) , "record.ID" ] <- c("NA1","NA2")

# rename columns with difficulties mentioned by patients
# note, the order does not carry any meaning, it is just an index for different difficulties mentioned
for ( i in names(d0) ) {
  for ( j in ( which(colnames(d0[[i]]) == "FSS.stupeň") + 1 ):ncol(d0[[i]] ) ) {
    colnames(d0[[i]])[j] <- paste0( "diff_no.", j - which(colnames(d0[[i]]) == "FSS.stupeň") )
  }
}

# participant 832-39 (d0$nt1) has no difficulty reported but coded as NA, let's assume it should've been 0 instead
d0$nt1[ d0$nt1$record.ID == "832-39", "diff_no.1" ] <- 0

# prepare a data sets for the long-format data
d1 <- list()

# change format of all three data sets to long format for easier manipulation
for ( i in names(d0) ) d1[[i]] <- d0[[i]] %>%
  # pivot each to a longer format
  pivot_longer( cols = starts_with("diff"), values_to = "index", names_to = "difficulty" ) %>%
  mutate( label = sapply( 1:nrow(.),
                          function(j)
                            ifelse( !is.na(index[j]), d.leg[ d.leg$index == index[j], "label" ], NA )
                          ) )

# add empty columns to d1$nh2 and d1$ih
d1$nt2 <- d1$nt2 %>% mutate( `přítomnost.kataplexie` = NA, `frekvence.kataplexií` = NA, .before = "NSS" )
d1$ih <- d1$ih %>% mutate( `přítomnost.kataplexie` = NA, `frekvence.kataplexií` = NA, NSS = NA, `NSS.stupeň` = NA, .before = "FSS" )

# glue all data sets below each other to create a single one beautiful data frame
d1 <- do.call( rbind.data.frame, d1 )


# ---- tab 2 shinanigans: order the difficulties per diagnosis ----

# check whether the number of patients within each group corresponds with information in the article
sapply( unique(d1$`F(1)/M(2)`), # sex
        function(i) sapply( unique(d1$dg), # diagnosis
                            function(j) d1[ with( d1, dg == j & `F(1)/M(2)` == i ), ]$record.ID %>% unique() %>% length()
                            ) ) %>%
  # add total numbers
  as.data.frame() %>% add_column( tot = rowSums(.) ) %>% `colnames<-`( c("F","M","T") )

# NT1, n = 64 (38 F, 26 M); NT2, n = 17 (11 F, 6 F); IH, n = 33 (21 F, 12 M); A-OK

# prepare a table for per diagnosis rankings of difficulties frequencies
t2 <- list()

# fill-in
for ( i in unique(d1$dg) ) t2[[i]] <- cbind(
  
  # raw number of mentions per diagnosis
  ( table( ( d1[ d1$dg == i, ] %>% filter( !is.na(index) ) )$label ) %>% sort(decreasing = T) ) %>% as.data.frame(),
  # percentage
  ( ( table( ( d1[ d1$dg == i, ] %>% filter( !is.na(index) ) )$label ) %>% sort(decreasing = T) ) / # raw numbers
      length( d1[ d1$dg == i, ]$record.ID %>% unique() ) * 100 ) %>% # divide by number of patients|diagnosis
    round(1) %>% sprintf("%.1f",.) %>% as.data.frame() # tidy up
  
  ) %>%
  
  # clean the table
  mutate( freq = paste0( Freq, " (", ., " %)") ) %>% # prepare a column with output "N (%)"
  rownames_to_column( var = "placing" ) %>% # add placing for merging across diagnoses
  `colnames<-`( c( "placing", paste0(i, ".diff"), "freq", "perc", paste0( i, ".freq") ) ) %>% # name the columns
  select( -freq, -perc ) # keep only variables of interest

# merge the tables
t2 <- merge( t2$NT1, t2$NT2, by = "placing", all = T, sort = F ) %>% merge( ., t2$IH, by = "placing", all = T, sort = F )

# save the resulting table as .csv
write.table( t2, file = "tabs/table_2.csv", sep = ",", row.names = F, quote = F, na = "-" )


# ---- prepare data frame suitable for regressions ----

# list outcomes and predictors of interest
outs <- d.leg[c(2,3,6),"label"]
prds <- c("ESS","FSS","NSS")

# prepared the data frame by keeping only the first row for each patient in d1
d2 <- d1[ d1$difficulty == "diff_no.1", -which( colnames(d1) %in% c("difficulty","index","label") ) ] %>% as.data.frame()

# for each difficulty from d.leg add an indicator variable for each patient
for ( i in d.leg$label ) {
  for ( j in 1:nrow(d2) ) {
    # count rows that included non-NA patient/difficulty pair (will result in 0 or 1)
    d2[ j, i ] <- d1[ d1$record.ID == d2$record.ID[j] & d1$label == i, "label" ] %>% na.omit() %>% nrow()
  }
}

# split the data frame by diagnosis
d2 <- split( d2, d2$dg )

# prepare a table for scaling values of questionnaires
scl <- expand.grid( dg = unique(d1$dg), quest = prds, M = NA, SD = NA )

# add means and standard deviation
for ( i in 1:nrow(scl) ) scl[ i, c("M","SD") ] <- c( mean( d2[[ as.character(scl$dg[i]) ]][ , as.character(scl$quest[i]) ], na.rm = T ),
                                                     sd( d2[[ as.character(scl$dg[i]) ]][ , as.character(scl$quest[i]) ], na.rm = T ) )

# save the table of scaling values
write.table( scl, file = "tabs/scaling_table.csv", sep = ",", row.names = F, quote = F, na = "-" )

# scale the questionnaires before analyses
for ( i in 1:nrow(scl) ) d2[[ as.character(scl$dg[i]) ]][ , paste0( as.character(scl$quest[i]), "_sc" ) ] <- ( d2[[ as.character(scl$dg[i]) ]][ , as.character(scl$quest[i]) ] - scl[i,"M"] ) / scl[i,"SD"]


# ---- compute a set of logistic regressions ----


# ---- write down session info ----

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = "sess/narc_diffISSUE.txt" )
