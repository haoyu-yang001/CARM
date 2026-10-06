#'@import arrangements
#'@importFrom MASS ginv
#'@importFrom stats median
#'@importFrom stats na.omit
#'@importFrom stats var
#'@import dplyr


pairwise_dis<-function(assigndata,p,K,method){
  dis<-dis_K<-NULL
  for (s in 1:K) {
    for (t in 1:K) {
      if (t>s)
      {

        pairwise<-assigndata[which(!is.na(assigndata$assignment)),]

        if (p>1){
          u<-colMeans((pairwise[which(pairwise$assignment==s),-1]))-
            colMeans(as.matrix(pairwise[which(pairwise$assignment==t),-1]))
          cov<-cov(as.matrix(pairwise[,-1]))}

        else{
          u<-mean((pairwise[which(pairwise$assignment==s),-1]))-
            mean(as.matrix(pairwise[which(pairwise$assignment==t),-1]))
          cov<-var(as.matrix(pairwise[,-1]))}

        dis<-2/K/K*nrow(pairwise)*(t(u)%*%ginv(cov)%*%u)
        dis_K<-c(dis_K,dis)


      }
    }
  }

  if (method=='mean'){
    all_dis<-mean(dis_K)
  }
  if(method=='max'){
    all_dis<-max(dis_K)
  }
  if(method=='median'){
    all_dis<-median(dis_K)
  }
  if(method=='none'){
    all_dis<-dis_K
  }
  return(all_dis)
}



circle_random<-function(assigndata,K,p,q,method,n){
MM<-NULL
ss=permutations(K)
had<-sum(!is.na(assigndata$assignment))
no<-sum(is.na(assigndata$assignment))
if (no %% K == 0){
  for (i in 1:(no/K)) {
    for (j in 1:factorial(K)) {
      assigndata$assignment[(K*(i-1)+1+had):(K*i+had)]<-ss[j,]
      MM[j]=pairwise_dis(assigndata,p,K,method)
    }
    u2=NULL
    for (u1 in 1:factorial(K)) {

      if(min(MM)==MM[u1])
      {
        u2=c(u2,u1)
      }
    }
    u=min(u2)
    c=rep(NA,factorial(K))
    c[u]=q
    c[-u]=(1-q)/(factorial(K)-1)
    x=sample(1:factorial(K),1,prob=c)
    assigndata$assignment[(K*(i-1)+1+had):(K*i+had)]<-ss[x,]
  }
}
else {
  remainder<-no %% K
  for (i in 1:((no-remainder)/K)) {
    for (j in 1:factorial(K)) {
      assigndata$assignment[(K*(i-1)+1+had):(K*i+had)]<-ss[j,]
      MM[j]=pairwise_dis(assigndata,p,K,method)
    }
    u2=NULL
    for (u1 in 1:factorial(K)) {

      if(min(MM)==MM[u1])
      {
        u2=c(u2,u1)
      }
    }
    u=min(u2)
    c=rep(NA,factorial(K))
    c[u]=q
    c[-u]=(1-q)/(factorial(K)-1)
    x=sample(1:factorial(K),1,prob=c)
    assigndata$assignment[(K*(i-1)+1+had):(K*i+had)]<-ss[x,]
  }
  for (r in (n-remainder+1):n) {
    assigndata$assignment[r]<-sample(c(1:K),prob=c(rep(1/K,K)),1,replace=TRUE)
  }
}
return(assigndata)
}

make_strata_encoder <- function(levels_list) {
  stopifnot(is.list(levels_list), length(levels_list) >= 1)

  key <- do.call(expand.grid, c(levels_list, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
  key$stratum_id <- seq_len(nrow(key))

  L <- vapply(levels_list, length, integer(1))
  offsets <- rev(cumprod(c(rev(L[-1]), 1)))


  index_in_levels <- function(vals, levels_list) {
    mapply(function(v, lv) {
      idx <- match(v, lv)
      if (is.na(idx)) stop(sprintf("value %s is not in level %s ", v, paste(lv, collapse = ",")))
      idx
    }, vals, levels_list, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  }


  encode_df <- function(df) {
    stopifnot(is.data.frame(df))
    merged <- merge(df, key, by = names(levels_list), all.x = TRUE, sort = FALSE)
    if (anyNA(merged$stratum_id)) {
      stop("It has been found that the values of some rows are not within the preset range. Please check the input.")
    }
    merged
  }

  list(
    levels_list = levels_list,
    key = key,
    encode_df = encode_df

  )
}

