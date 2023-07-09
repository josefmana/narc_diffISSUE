# This script is there for extracting descriptive stats regarding the data set used

# list required packages into a character object
pkgs <- c( "rstudioapi", "dplyr", "tidyverse" )

# load or install packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# set working directory (works only in RStudio)
setwd( dirname(rstudioapi::getSourceEditorContext()$path) )

# read the data sets for analysis
d <- read.csv( "_nogithub/data/df.csv", sep = ";" )

# extract diagnoses labels
dg <- unique(d$dg)


# ---- order difficulties per diagnosis ----

# check whether the number of patients within each group corresponds with information in the article
sapply( unique(d$F.1..M.2.), # sex
        function(i) sapply( unique(d$dg), # diagnosis
                            function(j) d[ with( d, dg == j & F.1..M.2. == i ), ]$record.ID %>% unique() %>% length()
        ) ) %>%
  # add total numbers
  as.data.frame() %>% add_column( tot = rowSums(.) ) %>% `colnames<-`( c("F","M","T") )
# NT1, n = 64 (38 F, 26 M); NT2, n = 17 (11 F, 6 F); IH, n = 33 (21 F, 12 M); A-OK

# prepare a table for per diagnosis rankings of difficulties frequencies
t.freq <- lapply( setNames(dg,dg),
                  function(i) cbind(
                    
                    # raw number of mentions per diagnosis
                    ( table( ( d[ d$dg == i, ] %>% filter( !is.na(index) ) )$label ) %>% sort(decreasing = T) ) %>% as.data.frame(),
                    # percentage
                    ( ( table( ( d[ d$dg == i, ] %>% filter( !is.na(index) ) )$label ) %>% sort(decreasing = T) ) / # raw numbers
                        length( d[ d$dg == i, ]$record.ID %>% unique() ) * 100 ) %>% # divide by number of patients|diagnosis
                        #length( ( d1[ d1$dg == i, ] %>% filter( !is.na(index) ) )$label ) * 100 ) %>% # divide by number of difficulties/diagnosis
                        round(1) %>% sprintf("%.1f",.) %>% as.data.frame() # tidy up
                    
                    ) %>%
                    
                    # clean the table
                    mutate( freq = paste0( Freq, " (", ., " %)") ) %>% # prepare a column with output "N (%)"
                    rownames_to_column( var = "placing" ) %>% # add placing for merging across diagnoses
                    mutate( placing = as.integer(placing) ) %>%
                    `colnames<-`( c( "placing", paste0(i, ".diff"), "freq", "perc", paste0( i, ".freq") ) ) %>% # name the columns
                    select( -freq, -perc ) # keep only variables of interest

                  )

# merge the tables
t.freq <- with( t.freq, merge( NT1, NT2, by = "placing", all = T, sort = F ) ) %>% merge( ., t.freq$IH, by = "placing", all = T, sort = F ) %>% arrange( ., placing )

# save the resulting table as .csv
write.table( t.freq, file = "tabs/table_2_percentages_of_difficulties_across_patients.csv", sep = ",", row.names = F, quote = F, na = "-" )


# ---- write down session info ----

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = "sess/desc.txt" )
