library(quark)
#library(MASS)
library(parallel)
flexLinearAssoc11 <- function(Varpairs,map){
  ####all(varType == "continuous")
  #  corVal <- cor(map[ ,Varpairs[1] ],
  #                map[ ,Varpairs[2] ],
  #                method = "pearson",
  #                use = "pairwise")
  ####all(varType == "ordinal")
  corVal <- cor(as.numeric(map[ , Varpairs[1] ]),
                as.numeric(map[ , Varpairs[2] ]),
                method = "spearman",
                use = "pairwise")
  ####all(varType == "nominal" | varType == "binary")
  #  corVal <- assocstats( table(map[ , Varpairs]) )$cramer
  ####all(varType == "ordinal" |  varType == "nominal" |    varType == "binary")
  #  corVal <- assocstats( table(map[ , Varpairs]) )$cramer
  ####all(varType == "continuous" | varType == "ordinal")
  #  corVal <- cor(as.numeric(map[ , Varpairs[1] ]),
  #                as.numeric(map[ , Varpairs[2] ]),
  #                method = "spearman",
  #                use = "pairwise")
  ####all(varType == "continuous" |  varType == "nominal" |    varType == "binary")
  #  nomVar <- Varpairs[varType == "nominal" | varType == "binary"]
  #  contVar <- Varpairs[varType == "continuous"]
  #  corVal <- ICCbare(x = nomVar, y = contVar, data = map)
  return(corVal)
}
#n = 10
#x = rnorm(n,0,1)
#y = numeric(n)
#for (i in 1:n){
#  p = 1/(1+exp(-1-2*x[i]))
#  y[i] = rbinom(1,1,p)
#  print(i)
#}
##### Create Data set
#################################################
#id.df<-NULL
#for (i in 1:201)
#{
#  if(i == 1){
#    id.df <-cbind(id.df,seq(1,201,1))
#  }
#  else
#    id.df <-cbind(id.df,runif(201, 0, 10))
#}
#id.df <- as.data.frame(id.df)
#for (i in 1:201)
#{
#  if(i == 1){
#    names(id.df)[i] <-paste("id")
#  }
#  else
#    names(id.df)[i] <-paste("x",i, sep = "")
#}
################################################
#save(id.df, file = "exdata1.rda")
#Read in data
result.df<-NULL
coreNUM.df<-NULL
coreNUM.df <-rbind(coreNUM.df,1)
coreNUM.df <-rbind(coreNUM.df,2)
coreNUM.df <-rbind(coreNUM.df,4)
result.df <-cbind(result.df,coreNUM.df)




for( k in 1 : 10 ) {
  result11.df<-NULL
  result11.df <- as.data.frame(result11.df)
  str=sprintf("exdata%d.rda",k)
  load(str)
  varPairs <- NULL
  tmpVarNames <- (setdiff(colnames(id.df), "id"))
  # create pairs with col names
  #nVars <- length(tmpVarNames)

  #  for( i in 1 : (nVars - 1) ) {
  #  for( j in (i + 1) : nVars ) {
  #    varPairs <- rbind(varPairs, tmpVarNames[c(i, j)])
  #  }
  #}
  
  varPairs<-data.frame(t(combn(tmpVarNames,2)),stringsAsFactors = F)
  
  
  
  newVarPairs <-as.data.frame(varPairs)
  newVarPairs$coeff <-as.numeric(NA)
  names(newVarPairs)<-c("first","second","coeff")
  t0<-Sys.time()
  
  #linAssocFrame <- data.frame(varPairs, unlist(apply(varPairs,1, FUN = flexLinearAssoc11, map = id.df  )), stringsAsFactors = FALSE)
  colnames(linAssocFrame) <- c("var1", "var2", "coef")
  newVarPairs$coeff <-unlist(apply(varPairs,1, FUN = flexLinearAssoc11, map = id.df  ))
  t0<-Sys.time()-t0
  result11.df <-rbind(result11.df,t0)
  newVarPairs <-as.data.frame(varPairs)
  newVarPairs$coeff <-as.numeric(NA)
  names(newVarPairs)<-c("first","second","coeff")
  c1 <-makeCluster(getOption("c1.cores", 2))
  t1<-Sys.time()
  linAssocFrame <- data.frame(varPairs, unlist(apply(varPairs,1, FUN = flexLinearAssoc11, map = id.df  )), stringsAsFactors = FALSE)
  colnames(linAssocFrame) <- c("var1", "var2", "coef")
  
  newVarPairs$coeff <-unlist(parApply(c1, varPairs,1, FUN = flexLinearAssoc11, map = id.df  ))
  t1<-Sys.time()-t1
  stopCluster(c1)
  result11.df <-rbind(result11.df,t1)
  newVarPairs <-as.data.frame(varPairs)
  newVarPairs$coeff <-as.numeric(NA)
  names(newVarPairs)<-c("first","second","coeff")
  c1 <-makeCluster(getOption("c1.cores", 4))
  t2<-Sys.time()
  newVarPairs$coeff <-unlist(parApply(c1, varPairs,1, FUN = flexLinearAssoc11, map = id.df  ))
  t2<-Sys.time()-t2
  stopCluster(c1)
  result11.df <-rbind(result11.df,t2)
  result.df <-cbind(result.df,result11.df[1])
}
result.df <- as.data.frame(result.df)
for (i in 1:11)
{
  if(i == 1){
    names(result.df)[i] <-paste("cores")
  }
  else
    names(result.df)[i] <-paste("x",(i-1)*10, sep = "")
}
result.df

