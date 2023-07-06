# Run this script first. It extracts and formats data from raw tables to analysis-ready outcomes.

# list required packages into a character object
pkgs <- c( "rstudioapi", "dplyr", "tidyverse", "openxlsx" )

# load or install packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# set working directory (works only in RStudio)
setwd( dirname(rstudioapi::getSourceEditorContext()$path) )


# ---- import raw data ----

# read the data from .xlsx
d0 <- list( nt1 = read.xlsx( "_nogithub/raw/diff issue podklady aktual.xlsx", sheet = "NT1" ),
            nt2 = read.xlsx( "_nogithub/raw/diff issue podklady aktual.xlsx", sheet = "NT2" ),
            ih = read.xlsx( "_nogithub/raw/diff issue podklady aktual.xlsx", sheet = "IH" )
            )

# read the legend
d.leg <-
  read.xlsx( "_nogithub/raw/diff issue podklady aktual.xlsx", sheet = "kódování - difficult issues" ) %>% # read the data
  t() %>% as.data.frame() %>% rownames_to_column( var = "0" ) %>% # add the null category
  t() %>% as.data.frame() %>% `colnames<-`( c("index","label") ) %>% # tidy
  mutate( index = sub( " ", "", index) %>% as.integer() ) # make sure the coding is in the right format


# ---- prepare data frame suitable for analyses ----

# rename NA IDs in d0$nt1 to dodge problems
d0$nt1[ is.na(d0$nt1$record.ID) , "record.ID" ] <- c("NA1","NA2")

# rename columns with difficulties mentioned by patients
# note, the order does not carry any meaning, it is just an index for different difficulties mentioned
for ( i in names(d0) ) {
  for ( j in ( which(colnames(d0[[i]]) == "FSS.stupeň") + 1 ):( which(colnames(d0[[i]]) == "HADS-D") - 1 ) ) {
    colnames(d0[[i]])[j] <- paste0( "diff_no.", j - which(colnames(d0[[i]]) == "FSS.stupeň") )
  }
}

# participant 832-39 (d0$nt1) reported no difficulty but is coded as NA, let's assume it should've been 0 instead
d0$nt1[ d0$nt1$record.ID == "832-39", "diff_no.1" ] <- 0

# rename "nejsou data" cells to "NA" and re-format difficulties from character to numeric where needed
#for ( i in names( d0 ) ) {
#  
#  d0[[i]][ d0[[i]] %in% c("nejsou data", "data nejsou")  , ] <- NA
#  d0[[i]] <- d0[[i]] %>% mutate( across( starts_with("diff"), as.numeric ) )
#
#}

# prepare a data sets for the long-format data
d1 <- lapply( setNames( names(d0), names(d0) ),
              function(i) d0[[i]] %>%
                # pivot each to a longer format
                pivot_longer( cols = starts_with("diff"), values_to = "index", names_to = "difficulty" ) %>%
                mutate( label = sapply( 1:nrow(.), function(j) ifelse( !is.na(index[j]), d.leg[ d.leg$index == index[j], "label" ], NA ) ) )
              )

# add empty columns to d1$nh2 and d1$ih
d1$nt1 <- d1$nt1 %>% mutate( IHSS = NA, .before = `přítomnost.kataplexie` )
d1$nt2 <- d1$nt2 %>% mutate( `přítomnost.kataplexie` = NA, `frekvence.kataplexií` = NA, .before = "NSS" ) %>% mutate( IHSS = NA, .before = `přítomnost.kataplexie` )
d1$ih <- d1$ih %>% mutate( `přítomnost.kataplexie` = NA, `frekvence.kataplexií` = NA, NSS = NA, `NSS.stupeň` = NA, .before = "FSS" )

# rename the "medikace" column
for ( i in names(d1) ) {
  names(d1[[i]])[ which( grepl( "Medikace.", names(d1[[i]]) ) ) ] <- "Medikace.ano/ne"
  names(d1[[i]])[ which( grepl( "NEO.FFI.neuroticismus", names(d1[[i]]) ) ) ] <- "NEO-FFI.neuroticismus.HS"
  names(d1[[i]])[7] <- "Doba.žití.s.nemocí.(in.years).=.Trvání.nemoci.(první.příznaky.subj.dle.pacienta)"
}

# glue all data sets below each other to create a single one beautiful data frame
d1 <- do.call( rbind.data.frame, d1 )

# save the outcome as a data frame ready for analyses
write.table( d1, "_nogithub/data/df.csv", sep = ";", row.names = F, quote = F )


# ---- write down session info ----

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = "sess/read_data.txt" )
