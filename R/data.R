#' Baseline socioeconomic information and BMIs of 500 individuals. 
#'
#' A dataset containing the body mass index (BMI) and baseline
#' socioeconomic information of 500 individuals from the National 
#' Longitudinal Survey of Youth 1979 (NLSY), a freely available longitudinal dataset. 
#' The 500 individuals were drawn randomly from among all NLSY respondents with at least 10
#' non-missing height/weight responses spread out over at least 20 years. 
#' This dataset is used for many of our code examples. 
#'
#' @format A data frame with 8083 rows and 18 columns. 
#' \describe{
#'   \item{ID}{jj}
#'   \item{News}{gj}
#'   \item{SEX}{gnegne}
#'   \item{AGE}{aa}
#'   \item{BLACK}{aa}
#'   \item{BMI}{aa}
#'   \item{Father_14}{aa}
#'   \item{HEIGHTIN}{aa}
#'   \item{HGC_FATHER}{aa}
#'   \item{HGC_MOTHER}{aa}
#'   \item{HISP}{aa}
#'   \item{Lib}{aa}
#'   \item{Mag}{aa}
#'   \item{Mother_14}{aa}
#'   \item{Num_sibs}{aa}
#'   \item{Two_Adults_14}{aa}
#'   \item{WEIGHT}{aa}
#'   \item{WHITE}{aa}
#'   
#'   ...
#' }
#' @source \url{https://www.bls.gov/nls/nlsy79.htm}
"nlsySample"



#' Additional socioeconomic information and BMIs of 1000 individuals. 
#'
#' See the description for the nlsySample data. This is an extended version of that dataset
#' with 1000 individuals drawn from the same population of survey respondents. There are also
#' covariates in this dataset that were excluded from the smaller version of the dataset. 
#' 
#'
#' @format A data frame with 8083 rows and 18 columns. 
#' \describe{
#'   \item{ID}{jj}
#'   \item{News}{gj}
#'   \item{SEX}{gnegne}
#'   \item{AGE}{aa}
#'   \item{BLACK}{aa}
#'   \item{BMI}{aa}
#'   \item{Father_14}{aa}
#'   \item{HEIGHTIN}{aa}
#'   \item{HGC_FATHER}{aa}
#'   \item{HGC_MOTHER}{aa}
#'   \item{HISP}{aa}
#'   \item{Lib}{aa}
#'   \item{Mag}{aa}
#'   \item{HGC}
#'   \item{Mother_14}{aa}
#'   \item{Num_sibs}{aa}
#'   \item{Two_Adults_14}{aa}
#'   \item{WEIGHT}{aa}
#'   \item{WHITE}{aa}
#'   \item{Dad_Full_Work}{}
#'   \item{Mom_Full_Work}{}
#'   \item{Age_first_alc}{}
#'   \item{POVSTAT}{}
#'   \item{Age_first_smoke}{}
#'   \item{STABLE_RESIDENCE}{}
#'   \item{Live_with_parents}{}
#'   \item{Age_first_weed}{}
#'   \item{Mother_on_record}{}
#'   \item{Father_on_record}{}
#'   \item{Mom_dad_household}{}
#'   \item{South_Birth}{}
#'   \item{URBAN_14}{}
#'   \item{HOUSEHOLD_14}{}
#'   \item{RACE}{}
#'   
#'  
#'   ...
#' }
#' @source \url{https://www.bls.gov/nls/nlsy79.htm}
"nlsySample_large"




#' A pre-built random forest (with no intercet) with 20 trees, built to the NLSY sample. 
#' 
#' The purpose of this pre-built forest is to demonstrate the forest evaluation functions without
#' needing to rebuild a forest (slow) every time. This forest does not use an intercept. 
"big_forest"

#' A pre-built random forest (with intercept) with 20 trees, built to the NLSY sample. 
#' 
#' The purpose of this pre-built forest is to demonstrate the forest evaluation functions without
#' needing to rebuild a forest (slow) every time. This forest uses an intercept. 
"big_intercept_forest"


#' A pre-computed variable importance matrix for the example forest.
#' 
#' Once again, this is saved simply so that examples can be shown without recomputing this matrix every time.
#' The Y-variable-importance function is relatively inefficient.
"Yimp"
