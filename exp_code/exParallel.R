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

  load("exdata10.rda")
  varPairs <- NULL
  tmpVarNames <- (setdiff(colnames(id.df), "id"))
  varPairs<-data.frame(t(combn(tmpVarNames,2)),stringsAsFactors = F)
  linAssocFrame <- data.frame(varPairs, unlist(apply(varPairs,1, FUN = flexLinearAssoc11, map = id.df  )), stringsAsFactors = FALSE)
  colnames(linAssocFrame) <- c("var1", "var2", "coef")
  #names(linAssocFrame)<-c("first","second","coeff")
