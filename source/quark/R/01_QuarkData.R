### Title:    QuarkData Reference Class Definition
### Author:   Kyle M. Lang
### Created:  2015-OCT-30
### Modified: 2016-JUL-31
### Note:     QuarkData is the metadata class for the quark package.

### Copyright (C) 2016 Kyle M. Lang
###
### This program is free software: you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### (at your option) any later version.
###
### This program is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with this program.  If not, see <http://www.gnu.org/licenses/>.


#################################################################################
#####----------------------- DEFINE CLASS & FIELDS -------------------------#####
#################################################################################

QuarkData <- setRefClass("QuarkData",

                         fields = list(
                             call         = "list",
                             data         = "ANY",
                             seed         = "integer",
                             miceIters    = "integer",
                             miceRidge    = "numeric",
                             maxNetWts    = "integer",
                             forcePmm     = "logical",
                             typeVec      = "vector",
                             methVec      = "vector",
                             nComps       = "vector",
                             respCounts   = "vector",
                             initialPm    = "vector",
                             nomVars      = "vector",
                             ordVars      = "vector",
                             idVars       = "vector",
                             dropVars     = "matrix",
                             nomMaxLev    = "integer",
                             ordMaxLev    = "integer",
                             conMinLev    = "integer",
                             probNoms     = "vector",
                             probOrds     = "vector",
                             probCons     = "vector",
                             levelVec     = "vector",
                             simMode      = "logical",
                             highPmVars   = "vector",
                             emptyVars    = "vector",
                             constants    = "vector",
                             minRespCount = "integer",
                             verbose      = "logical",
                             groupVars    = "vector",
                             dummyVars    = "vector",
                             pcAux        = "list",
                             rSquared     = "list",
                             pcaMemLev    = "integer",
                             calcInteract = "logical",
                             calcPoly     = "logical",
                             maxPower     = "integer",
                             interact     = "ANY",
                             poly         = "ANY",
                             collinThresh = "numeric",
                             minPredCor   = "numeric",
                             nGVarCats    = "integer",
                             collinVars   = "data.frame",
                             impFails     = "list",
                             patterns     = "list",
                             frozenGVars  = "ANY",
                             idFills      = "ANY",
                             dummyId      = "logical",
                             nImps        = "integer",
                             compFormat   = "character",
                             miDatasets   = "ANY",
                             miceObject   = "ANY"
                         )# END fields
                         )# END QuarkData


#################################################################################
#####---------------------------- DEFINE METHODS ---------------------------#####
#################################################################################

QuarkData$methods(

    ##------------------------------ Constructor ------------------------------##

    initialize     = function(
        call         = list(
            prepData    = NULL,
            createPcAux = NULL,
            miWithPcAux = NULL
        ),
        data         = data.frame(NULL),
        seed         = 0L,
        miceIters    = 0L,
        miceRidge    = 0.0,
        maxNetWts    = 0L,
        forcePmm     = FALSE,
        typeVec      = vector("character"),
        methVec      = vector("character"),
        nComps       = vector("integer"),
        respCounts   = vector("numeric"),
        initialPm    = vector("numeric"),
        nomVars      = vector("character"),
        ordVars      = vector("character"),
        idVars       = vector("character"),
        dropVars     = matrix(NA, 1, 2),
        nomMaxLev    = 0L,
        ordMaxLev    = 0L,
        conMinLev    = 0L,
        probNoms     = vector("character"),
        probOrds     = vector("character"),
        probCons     = vector("character"),
        levelVec     = vector("integer"),
        simMode      = FALSE,
        highPmVars   = vector("character"),
        emptyVars    = vector("character"),
        constants    = vector("character"),
        minRespCount = 0L,
        verbose      = FALSE,
        groupVars    = vector("character"),
        dummyVars    = vector("character"),
        pcaMemLev    = 0L,
        calcInteract = FALSE,
        calcPoly     = FALSE,
        maxPower     = 0L,
        interact     = NULL,
        poly         = NULL,
        collinThresh = 0.0,
        minPredCor   = 0.0,
        nGVarCats    = 0L,
        collinVars   = data.frame(NULL),
        patterns     = list(),
        frozenGVars  = NULL,
        idFills      = list(),
        dummyId      = FALSE,
        pcAux        = list(
            lin    = data.frame(NULL),
            nonLin = data.frame(NULL)
        ),
        rSquared     = list(
            lin    = vector("numeric"),
            nonLin = vector("numeric")
        ),
        impFails     = list(
            firstPass = vector("character"),
            pmm       = vector("character"),
            groupMean = vector("character"),
            grandMean = vector("character")
        ),
        nImps      =  0L,
        compFormat = "",
        miDatasets = NULL,
        miceObject = NULL
    )                                                                           {
        "Initialize an object of class QuarkData"
        call         <<- call
        data         <<- data[ , setdiff(colnames(data), dropVars)]
        dropVars     <<- cbind(dropVars, "user_defined")
        seed         <<- seed
        miceIters    <<- miceIters
        miceRidge    <<- miceRidge
        maxNetWts    <<- maxNetWts
        forcePmm     <<- forcePmm
        typeVec      <<- typeVec
        methVec      <<- methVec
        nComps       <<- nComps
        respCounts   <<- respCounts
        initialPm    <<- initialPm
        nomVars      <<- nomVars
        ordVars      <<- ordVars
        idVars       <<- idVars
        nomMaxLev    <<- nomMaxLev
        ordMaxLev    <<- ordMaxLev
        conMinLev    <<- conMinLev
        probNoms     <<- probNoms
        probOrds     <<- probOrds
        probCons     <<- probCons
        levelVec     <<- levelVec
        simMode      <<- simMode
        highPmVars   <<- highPmVars
        emptyVars    <<- emptyVars
        constants    <<- constants
        minRespCount <<- minRespCount
        verbose      <<- verbose
        groupVars    <<- groupVars
        pcaMemLev    <<- pcaMemLev
        calcInteract <<- calcInteract
        calcPoly     <<- calcPoly
        maxPower     <<- maxPower
        poly         <<- poly
        pcAux        <<- pcAux
        rSquared     <<- rSquared
        impFails     <<- impFails
        dummyId      <<- dummyId
        dummyVars    <<- dummyVars
        interact     <<- interact
        collinThresh <<- collinThresh
        minPredCor   <<- minPredCor
        nGVarCats    <<- nGVarCats
        collinVars   <<- collinVars
        patterns     <<- patterns
        frozenGVars  <<- frozenGVars
        idFills      <<- idFills
        nImps        <<- nImps
        compFormat   <<- compFormat
        miDatasets   <<- miDatasets
        miceObject   <<- miceObject
    },

    ##------------------ "Overloaded" / Non-Standard Mutators -----------------##
    setCall         = function(x, parent)                                       {
        if     (parent == "prepData") call[[1]] <<- x
        else if(parent == "quark"   ) call[[2]] <<- x
        else if(parent == "rom"     ) call[[3]] <<- x
        else                          stop("Invalid 'parent' argument.")
    },

    setPoly         = function(x, power = NULL)                                 {
        "Modify the list of polynomial expansions of 'data'"
        if(is.null(power)) poly          <<- x
        else               poly[[power]] <<- x
    },

    setPcAux        = function(x, type = NULL)                                  {
        "Modify the list of principal component auxiliaries"
        if     (is.null(type)      ) pcAux        <<- x
        else if(type == "linear"   ) pcAux$lin    <<- x
        else if(type == "nonLinear") pcAux$nonLin <<- x
        else                         stop("Invalid pcAux type.")
    },

    setRSquared     = function(x, type = NULL)                                  {
        "Modify the list of R-Squared values for the PcAux scores"
        if     (is.null(type)      ) rSquared        <<- x
        else if(type == "linear"   ) rSquared$lin    <<- x
        else if(type == "nonLinear") rSquared$nonLin <<- x
        else                         stop("Invalid rSquared type.")
    },

    setControl      = function(x, parent = "dataPrep")                          {
        "Assign the control parameters"
        if(parent == "dataPrep") {# Populating a new QuarkData object?
            miceIters    <<- as.integer(x$miceIters)
            nGVarCats    <<- as.integer(x$nGVarCats)
            minPredCor   <<- x$minPredCor
            collinThresh <<- x$collinThresh
        }
        miceRidge    <<- x$miceRidge
        minRespCount <<- as.integer(x$minRespCount)
        maxNetWts    <<- as.integer(x$maxNetWts)
        nomMaxLev    <<- as.integer(x$nomMaxLev)
        ordMaxLev    <<- as.integer(x$ordMaxLev)
        conMinLev    <<- as.integer(x$conMinLev)
    },

    updateImpFails   = function(x, type)                                        {
        "Update the list of imputation failure records"
        impFails[[type]] <<- x
    },

    setMethVec     = function(x, index = NULL)                                  {
        "Update the elementary imputation method vector"
        if(is.null(index)) methVec        <<- x
        else               methVec[index] <<- x
    },

    ##------------------------- "Overloaded" Accessors ------------------------##

    getPoly         = function(power = NULL)                                    {
        "Retrieve the polynomial expansions of 'data'"
        if(is.null(power)) return(poly         )
        else               return(poly[[power]])
    },

    getPcAux        = function(type = NULL)                                     {
        "Retrieve the principal component auxiliary scores"
        if     (is.null(type)      ) return(pcAux                )
        else if(type == "linear"   ) return(pcAux$lin            )
        else if(type == "nonLinear") return(pcAux$nonLin         )
        else                         stop  ("Invalid pcAux type.")
    },

    getRSquared     = function(type = NULL)                                     {
        "Retrieve the R-Squareds for the PcAux scores"
        if     (is.null(type)      ) return(rSquared                )
        else if(type == "linear"   ) return(rSquared$lin            )
        else if(type == "nonLinear") return(rSquared$nonLin         )
        else                         stop  ("Invalid rSquared type.")
    },

    getControl      = function()                                                {
        "Retrieve the control parameters"
        list(
            miceIters    = miceIters,
            miceRidge    = miceRidge,
            collinThresh = collinThresh,
            minRespCount = minRespCount,
            minPredCor   = minPredCor,
            maxNetWts    = maxNetWts,
            nomMaxLev    = nomMaxLev,
            ordMaxLev    = ordMaxLev,
            conMinLev    = conMinLev,
            nGVarCats    = nGVarCats
        )
    },

    ##---------------- Data Screening and Manipulation Methods ----------------##

    removeVars     = function(x, reason, recordOnly = FALSE)                    {
        "Remove columns from 'data' and store their meta-data"
        dropCols <-  which(colnames(data) %in% x)
        dropVars <<- rbind(dropVars, cbind(colnames(data)[dropCols], reason))
        if(!recordOnly) data <<- data[ , -dropCols]
    },

    countVarLevels = function()                                                 {
        "Count the levels for each column in 'data'"
        levelVec <<- as.integer(
            do.call(c,
                    lapply(data,
                           FUN = function(x) length(unique(na.omit(x)))
                           )
                    )
        )
        names(levelVec) <<- colnames(data)
    },

    typeData       = function()                                                 {
        "Populate a vector containing each variable's type"
        cn <- colnames(data); nv <- ncol(data)
        ## Default type is continuous:
        typeVec                                  <<- rep("continuous", nv)
        typeVec[cn %in% nomVars & levelVec == 2] <<- "binary"
        typeVec[cn %in% nomVars & levelVec > 2 ] <<- "nominal"
        typeVec[cn %in% ordVars                ] <<- "ordinal"
        typeVec[cn %in% idVars                 ] <<- "id"
        typeVec[levelVec == 1                  ] <<- "constant"
        typeVec[initialPm == 1.0               ] <<- "empty"
        typeVec[cn %in% dropVars[ , 1]         ] <<- "drop"
        names(typeVec                          ) <<- colnames(data       )
    },

    castData       = function()                                                 {
        "Cast all variables to the appropriate measurement level"
        for(i in 1 : ncol(data)) {
            data[ , i] <<-
                switch(typeVec[i],
                       drop       =              data[ , i],
                       id         =              data[ , i],
                       empty      =              data[ , i],
                       constant   =              data[ , i],
                       binary     = as.factor  ( data[ , i] ),
                       nominal    = as.factor  ( data[ , i] ),
                       ordinal    = as.ordered ( data[ , i] ),
                       continuous = as.numeric ( data[ , i] )
                       )
        }
    },

    checkTypes     = function()                                                 {
        "Check each variable for a sensible number of levels"
        tmpN <- (typeVec == "nominal" | typeVec == "binary") &
            levelVec > nomMaxLev
        tmpC <- typeVec == "continuous" & levelVec < conMinLev
        tmpO <- typeVec == "ordinal"    & levelVec > ordMaxLev

        if(length(tmpN) > 0) probNoms <<- names(levelVec)[tmpN]
        if(length(tmpO) > 0) probOrds <<- names(levelVec)[tmpO]
        if(length(tmpC) > 0) probCons <<- names(levelVec)[tmpC]
    },

    countResponses = function(
        countMissing = FALSE,
        asProportion = FALSE,
        strict       = FALSE,
        initialPm    = FALSE
    )                                                                           {
        "Calculate the variable-wise response counts"
        if(asProportion) {
            if(countMissing) {
                respCounts <<- colMeans(is.na(data)            )
                noMissing  <-  all     (respCounts == 0.0      )
            } else {
                respCounts <<- colMeans(!is.na(data)           )
                noMissing  <-  all     (respCounts == 1.0      )
            }
        } else {
            if(countMissing) {
                respCounts <<- colSums(is.na(data)             )
                noMissing  <-  all    (respCounts == 0         )
            } else {
                respCounts <<- colSums(!is.na(data)            )
                noMissing  <-  all    (respCounts == nrow(data))
            }
        }

        if(strict & noMissing)
            stop(paste0("The data provided are completely ",
                        "observed.\nYou may wish to check ",
                        "your data for coding errors.")
                 )

        ## Store the inital proportion of missing data:
        if(initialPm) initialPm <<- colMeans(is.na(data))
    },

    findHighPmVars = function()                                                 {
        "Flag variables with few responses"
        tmpNames   <-  setdiff   (names(respCounts), dropVars[ , 1]       )
        tmpCounts  <-  respCounts[tmpNames                                ]
        highPmVars <<- tmpNames  [tmpCounts > 0 & tmpCounts < minRespCount]
        length(highPmVars) > 0 # Find any High PM variables?
    },

    findEmptyVars  = function(remove = TRUE)                                    {
        "Flag empty variables"
        tmpNames  <-  setdiff(names(typeVec), dropVars[ , 1])
        tmpTypes  <-  typeVec[tmpNames                      ]
        emptyVars <<- tmpNames[tmpTypes == "empty"          ]
        if(length(emptyVars) > 0) {# Find any empty variables?
            removeVars(x = emptyVars, reason = "empty", recordOnly = !remove)
            outFlag <- TRUE
        } else {
            outFlag <- FALSE
        }
        outFlag
    },

    findConstCols  = function(doingQuark = TRUE)                                {
        "Locate and fill constant columns in 'data'"
        tmpNames  <-  setdiff (names(typeVec), dropVars[ , 1])
        tmpTypes  <-  typeVec [tmpNames                      ]
        constants <<- tmpNames[tmpTypes == "constant"        ]

        if(length(constants) > 0) {# Find any constant columns?
            if(doingQuark) {
                removeVars(x = constants, reason = "constant")
            } else {
                fillConstants()
            }
            outFlag <- TRUE
        } else {
            outFlag <- FALSE
        }
        outFlag
    },

    fillConstants   = function()                                                {
        "Fill constant columns with the appropriate value"
        tmp1    <- names  (initialPm)[initialPm == 0]
        targets <- setdiff(constants, tmp1          )
        if(length(targets) > 1) {
            tmp2 <- data.frame(
                lapply(data[ , targets],
                       FUN = function(x) {
                           x[is.na(x)] <- unique(na.omit(x))
                           x
                       })
            )
        } else {
            tmp2              <- data  [ , targets   ]
            tmp2[is.na(tmp2)] <- unique(na.omit(tmp2))
        }
        data[ , targets] <<- tmp2
    },

    cleanCollinVars = function(x)                                               {
        "Remove one variable from all collinear pairs"
        collinVars <<- x
        removeVars(x = unique(collinVars$var1), reason = "collinear")
    },

    createMethVec  = function()                                                 {
        "Populate a vector of elementary imputation methods"
        if(forcePmm) {
            methVec        <<- rep     ("pmm", ncol(data))
            names(methVec) <<- colnames(data             )

            ## KML 2016-JUL-31: Don't use PMM for nominal variables
            binNames <- colnames(data)[typeVec[colnames(data)] == "binary"]
            nomNames <- colnames(data)[typeVec[colnames(data)] == "nominal"]
            
            tmpIndex <- names(methVec) %in% binNames
            setMethVec(x = "logreg", index = tmpIndex)

            tmpIndex <- names(methVec) %in% nomNames
            setMethVec(x = "polyreg", index = tmpIndex)
       
            ## Don't impute ID or Dropped Variables:
            tmpIndex <- names(methVec) %in% dropVars[ , 1] |
                names(methVec) %in% idVars
            setMethVec(x = "", index = tmpIndex)
         } else {
            methVec <<-
                sapply(typeVec[colnames(data)],
                       FUN = function(x) {
                           switch(x,
                                  continuous = "norm",
                                  ordinal    = "polr",
                                  nominal    = "polyreg",
                                  binary     = "logreg",
                                  "")
                       }
                       )
            ## Work Around: Use PMM for variables with only 1 missing datum:
            setMethVec(x = "pmm", index = colSums(is.na(data)) == 1)
        }
        ## Don't impute fully observed variables:
        setMethVec(x = "", index = colSums(is.na(data)) == 0)
    },

    addVars         = function(x, names = NULL)                                 {
        "Add columns to 'data'"
        if(is.null(names)) names <-  colnames  (x              )
        oldNames                 <-  colnames  (data           )
        data                     <<- data.frame(data, x        )
        colnames(data)           <<- c         (oldNames, names)
    },

    idToCharacter   = function()                                                {
        "If any IDs are factors, cast them as character objects"
        for(i in idVars) {
            if( any(class(data[ , i]) == "factor") )
                data[ , i] <<- as.character(data[ , i])
        }
    },

    binGroupVars    = function(undo = FALSE)                                    {
        "Discretize continuous grouping variables"
        gvTypes  <- typeVec[groupVars                       ]
        conGVars <- names  (gvTypes)[gvTypes == "continuous"]
        if(!undo) {
            ## Define a vector defining the cut-point percentiles:
            probVec <- c(0, (c(1 : (nGVarCats - 1)) / nGVarCats), 1)
            ## Define a function to create the cut-points:
            cutFun  <- function(x, nCats, probs) {
                cut(x,
                    breaks = quantile(x, probs = probs, na.rm = TRUE),
                    labels = c(1 : nCats)
                    )
            }
            ## Bin the variables:
            frozenGVars <<- data[ , conGVars]
            if(length(conGVars) > 1) {
                data[ , conGVars] <<- data.frame(lapply(X     = frozenGVars,
                                                        FUN   = cutFun,
                                                        nCats = nGVarCats,
                                                        probs = probVec)
                                                 )
            } else {
                data[ , conGVars] <<- cutFun(frozenGVars, nGVarCats, probVec)
            }
        } else {
            tmp               <-  data[ , conGVars]
            data[ , conGVars] <<- frozenGVars
            frozenGVars       <<- tmp
        }
    },

    createPatterns  = function()                                                {
        "Create patterns to use for group-mean substitution"
        ## Deal with any continuous grouping variables:
        gVarCheck <- any(typeVec[groupVars] == "continuous")
        if(gVarCheck) binGroupVars()

        ## 'patterns' list a list of vectors made up of
        ## cross-tabulated grouping variables:
        patterns <<-
            lapply(c(length(groupVars) : 2),
                   FUN = function(x, data) {
                       tmpData <- data[ , groupVars[1 : x]]
                       apply(# Merge columns of tmpData into a single vector
                           apply(tmpData, 2, as.character),# Typecast tmpData
                           1, paste, collapse = "")
                   },
                   data = data)

        patterns[[length(groupVars)]] <<- format(data[ , groupVars[1]])

        if(gVarCheck) binGroupVars(undo = TRUE)
    },

    completeMiData  = function()                                                {
        "Complete the multiply imputed data sets"
        specialComp <- compFormat %in% c("long", "broad", "repeated")
        if(specialComp) {
            miDatasets <<- mice::complete(miceObject, compFormat)
        } else {
            miDatasets <<- list()
            for(m in 1 : nImps)
                miDatasets[[m]] <<- mice::complete(miceObject, m)
        }
    },

    transformMiData = function()                                                {
        "Reformat list-formatted imputed data sets"
        if(compFormat == "long") {
            .imp <- rep(c(1 : nImps), each = nrow(data))
            .id  <- rep(c(1 : nrow(data)), nImps       )
            miDatasets <<-
                data.frame(.imp, .id, do.call(rbind.data.frame, miDatasets))
        } else {
            for(m in 1 : nImps) {
                colnames(miDatasets[[m]]) <<-
                    paste0(colnames(miDatasets[[m]]), ".", m)
            }

            miDatasets <<- do.call(cbind.data.frame, miDatasets)

            if(compFormat == "repeated") {
                tmp        <-  rep       (c(1 : ncol(data)), nImps)
                miDatasets <<- miDatasets[ , order(tmp)           ]
            }
        }
    }
)# END QuarkData$methods()
