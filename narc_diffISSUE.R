# set working directory (works only in RStudio)
setwd( dirname(rstudioapi::getSourceEditorContext()$path) )

# list packages to use
pkgs <- c( "tidyverse", "dplyr", # data wrangling
           "ggplot2", "patchwork", # plotting
           "performance", "emmeans", # stats
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

# participant 832-39 (d0$nt1) has no difficulty reported but is coded as NA, let's assume it should've been 0 instead
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
      #length( ( d1[ d1$dg == i, ] %>% filter( !is.na(index) ) )$label ) * 100 ) %>% # divide by number of difficulties/diagnosis
    round(1) %>% sprintf("%.1f",.) %>% as.data.frame() # tidy up
  
  ) %>%
  
  # clean the table
  mutate( freq = paste0( Freq, " (", ., " %)") ) %>% # prepare a column with output "N (%)"
  rownames_to_column( var = "placing" ) %>% # add placing for merging across diagnoses
  mutate( placing = as.integer(placing) ) %>%
  `colnames<-`( c( "placing", paste0(i, ".diff"), "freq", "perc", paste0( i, ".freq") ) ) %>% # name the columns
  select( -freq, -perc ) # keep only variables of interest

# merge the tables
t2 <- merge( t2$NT1, t2$NT2, by = "placing", all = T, sort = F ) %>%
  merge( ., t2$IH, by = "placing", all = T, sort = F ) %>%
  arrange( ., placing )

# save the resulting table as .csv
write.table( t2, file = "tabs/table_2.csv", sep = ",", row.names = F, quote = F, na = "-" )


# ---- prepare data frame suitable for regressions ----

# list outcomes and predictors of interest
out.nms <- d.leg[c(2,3,6),"label"]
out.vrs <- gsub( " ", "_", out.nms )
preds <- c("ESS","FSS","NSS")

# prepare the data frame by keeping only the first row for each patient in d1
d2 <- d1[ d1$difficulty == "diff_no.1", -which( colnames(d1) %in% c("difficulty","index","label") ) ] %>% as.data.frame()

# for each difficulty from d.leg add an indicator variable for each patient
for ( i in d.leg$label ) {
  for ( j in 1:nrow(d2) ) {
    # count rows that included non-NA patient/difficulty pair (will result in 0 or 1)
    d2[ j, i ] <- d1[ d1$record.ID == d2$record.ID[j] & d1$label == i, "label" ] %>% na.omit() %>% nrow()
  }
}

# add total number of difficulties reported by each patient
d2$diff_count <- d2[ , colnames(d2) %in% d.leg$label[-1] ] %>% rowSums()

# prepare columns with underscores instead of spaces for outcomes (such that formula in glm() doesn't have problems)
for (i in 1:length(out.nms) ) d2[ , out.vrs[i] ] <- d2[ , out.nms[i] ]

# split the data frame by diagnosis
d3 <- split( d2, d2$dg )

# prepare a table for scaling values of questionnaires
scl <- expand.grid( dg = unique(d1$dg), quest = preds, M = NA, SD = NA )

# add means and standard deviation
for ( i in 1:nrow(scl) ) scl[ i, c("M","SD") ] <- c( mean( d3[[ as.character(scl$dg[i]) ]][ , as.character(scl$quest[i]) ], na.rm = T ),
                                                     sd( d3[[ as.character(scl$dg[i]) ]][ , as.character(scl$quest[i]) ], na.rm = T ) )

# save the table of scaling values
write.table( scl, file = "tabs/scaling_table.csv", sep = ",", row.names = F, quote = F, na = "-" )

# scale the questionnaires before analyses
for ( i in 1:nrow(scl) ) d3[[ as.character(scl$dg[i]) ]][ , paste0( as.character(scl$quest[i]), "_sc" ) ] <- ( d3[[ as.character(scl$dg[i]) ]][ , as.character(scl$quest[i]) ] - scl[i,"M"] ) / scl[i,"SD"]


# ---- compute a set of logistic (multiple) regressions ----

# prepare a table for results of multiple regression analyses (all predictors at once)
t3 <- expand_grid( dg = names(d3), out = out.vrs, pred = paste0(preds,"_sc") ) %>%
  # add columns for the results
  mutate( b = NA, b_se = NA, z = NA, p = NA, OR = NA, OR_CI95 = NA ) %>% as.data.frame()

# prepare a list for models
m <- list()

# fit logistic (multiple) regressions
for ( i in names(d3) ) {
  for ( j in out.vrs ) m[[i]][[j]] <- glm( formula = paste0( j, " ~ 1 + ESS_sc + FSS_sc", if( i != "IH") " + NSS_sc" ) %>% as.formula,
                                        data = d3[[i]], family = binomial()
                                        )
}

# fill-in values into t3
for ( i in 1:nrow(t3) ) {
  
  # fill-in results on the logit scale
  tryCatch(
    t3[ i , c("b","b_se","z","p") ] <- summary( m[[t3$dg[i]]][[t3$out[i]]] )$coefficients[ t3$pred[i], ] %>% round(3) %>% sprintf( "%.3f", . ),
    error = function(e) { print("There ain't no NSS for IH") } # error message for missing parameters
  )
  
  # add odd ratios with their 95% CIs
  tryCatch(
    t3[ i , c("OR","OR_CI95") ] <- c(
      # estimate
      exp( summary( m[[t3$dg[i]]][[t3$out[i]]] )$coefficients[ t3$pred[i], 1 ] ) %>% round(2) %>% sprintf( "%.2f", . ),
      # 95 % CI
      paste0( "[", exp( summary( m[[t3$dg[i]]][[t3$out[i]]] )$coefficients[ t3$pred[i], 1 ] + qnorm( c(.025,.975) ) * summary( m[[t3$dg[i]]][[t3$out[i]]] )$coefficients[ t3$pred[i], 2 ] ) %>% round(2) %>% sprintf( "%.2f", . ) %>% paste( collapse = ", " ) , "]" )
    ), error = function(e) { print("There ain't no NSS for IH") } # error message for missing parameters
  )
  
}


# ---- compute a set of logistic (simple) regressions ----

# prepare a table for results of multiple regression analyses (all predictors at once)
t4 <- expand_grid( dg = names(d3), out = out.vrs, pred = paste0(preds,"_sc") ) %>%
  # add columns for the results
  mutate( b = NA, b_se = NA, z = NA, p = NA, OR = NA, OR_CI95 = NA ) %>% as.data.frame()

# prepare a list for models
m1 <- list()

# fit logistic (simple) regressions
for ( i in names(d3) ) {
  for ( j in out.vrs ) {
    for ( k in paste0(preds,"_sc") ) {
      
      # don't try to estimate outcome ~ NSS in IH (no data)
      if ( !(i == "IH" & k == "NSS_sc") ) {
        
        # otherwise go for it
        m1[[i]][[j]][[k]] <- glm( formula = paste0( j, " ~ 1 + ", k ) %>% as.formula() , data = d3[[i]], family = binomial() )
        
      }
    }
  }
}

# fill-in values into t4
for ( i in 1:nrow(t4) ) {
  
  # fill-in results on the logit scale
  tryCatch(
    t4[ i , c("b","b_se","z","p") ] <- summary( m1[[t4$dg[i]]][[t3$out[i]]][[t3$pred[i]]] )$coefficients[ t3$pred[i], ] %>% round(3) %>% sprintf( "%.3f", . ),
    error = function(e) { print("There ain't no NSS for IH") } # error message for missing parameters
  )
  
  # add odd ratios with their 95% CIs
  tryCatch(
    t4[ i , c("OR","OR_CI95") ] <- c(
      # estimate
      exp( summary( m1[[t3$dg[i]]][[t3$out[i]]][[t3$pred[i]]] )$coefficients[ t3$pred[i], 1 ] ) %>% round(2) %>% sprintf( "%.2f", . ),
      # 95 % CI
      paste0( "[", exp( summary( m1[[t3$dg[i]]][[t3$out[i]]][[t3$pred[i]]] )$coefficients[ t3$pred[i], 1 ] + qnorm( c(.025,.975) ) * summary( m[[t3$dg[i]]][[t3$out[i]]] )$coefficients[ t3$pred[i], 2 ] ) %>% round(2) %>% sprintf( "%.2f", . ) %>% paste( collapse = ", " ) , "]" )
    ), error = function(e) { print("There ain't no NSS for IH") } # error message for missing parameters
  )
  
}


# ---- add flags for a liberal p < .05 ----

# apply via assign(), use get(i) to call an object via its name written as a string - super handy!
for ( i in c("t3","t4") ) {
  assign( i, get(i) %>% mutate( flag = ifelse( p < .05, ":-)", "" ) ) ) # do the thing
  write.table( get(i), paste0("tabs/", ifelse( i == "t3", "table_3_multiple_regressions.csv", "table_4_simple_regressions.csv" ) ), sep = ";", row.names = F, quote = F, na = "" ) # write as .csv
}


# ---- compare mean number of difficulties across diagnoses ----

# first check means and SDs per group
sapply( names(d3), function(i) paste0( round( mean(d3[[i]]$diff_count), 2 ), " ± ", round( sd(d3[[i]]$diff_count), 2 ) ) )

# now compute ANOVA
anova( lm( diff_count ~ dg, data = d2 ) )


# ---- write down session info ----

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = "sess/narc_diffISSUE.txt" )
