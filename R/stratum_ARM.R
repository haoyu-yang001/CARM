#'@import arrangements
#'@import dplyr
#'@importFrom MASS ginv
#'@importFrom stats median
#'@importFrom stats na.omit
#'@importFrom stats var
#'@export
#'
#'@title Stratified Adaptive Randomization via Mahalanobis Distance
#'
#'@description Update Strata for ARM
#'
#'@param covariate a data frame. A row of the dataframe
#'corresponds to the covariate profile of a patient.

#'@param assignment a vector. If partial patients had been allocated
#', please input their allocation. IF
#'all the patients are not be allocated, please input
#''assignment = NA' directly.
#'@param cont_var name of continuous covariates
#'@param cat_var name of categorical covariates
#'@param levels_list levels of each categorical covariate
#'@param q the biased coin probability.
#'\eqn{q} should be larger than 1/2 and less than 1, default = 0.75

#'@return
#'An object of class "ARM" is a list containing the following components:
#'\item{assignment}{Allocation of patients.}
#'\item{sample_size}{The number of patients in treatment 1 and treatment 2 respectively.}
#'\item{Mahalanobis_Distance}{Mahalanobis distance between treatment groups 1 and 2.}
#'


stratum_ARM<-function(covariate, assignment,
                      cont_var = c("C1"),
                      cat_var = c("N1","N2"),
                      levels_list = list(C1 = 0:1),
                      q=0.75){
  K=2
  p=length(cont_var)
  n = nrow(as.data.frame(covariate))
  method='none'
  covariate$originassign<-assignment
  enc <- make_strata_encoder(levels_list)

  covariate$fid <- sample(0:99999999, nrow(covariate), replace = FALSE)
  df_hist_enc <- enc$encode_df(as.data.frame(covariate))
  df_sorted <- df_hist_enc[order(df_hist_enc$stratum_id, decreasing = F), ]

  assign<-unsplit(
    lapply(split(seq_len(nrow(df_sorted)), df_sorted$stratum_id), function(idx) {
      ARM(covariate = df_sorted[idx, cont_var],
          assignment =df_sorted[idx, "originassign"],q=0.75)$assignment
    }),
    df_sorted$stratum_id)
  assignid <- data.frame(fid = df_sorted$fid, assignment = assign)
  assigndata <- left_join(covariate, assignid, by="fid")


  R = NULL
  R$assignment<-assigndata$assignment
  R$sample_size<-as.data.frame(assigndata%>%group_by(assignment)%>%count(assignment))
  R$Mahalanobis_Distance<-pairwise_dis(assigndata[,c("assignment",cont_var)],p,K,method)
  return(R)
}
