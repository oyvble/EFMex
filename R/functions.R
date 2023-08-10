


#Helpfunction for input from user; numeric, logical or string (function)
isEmpty = function(x) !isTruthy(x) || is.null(x) || is.na(x) || x==""
inputNumeric = function(x, canBeEmpty=FALSE) {
  ret = NULL
  if(!isEmpty(x)) return(as.numeric(x))
  if(!canBeEmpty && is.null(ret)) stop("Input cannot be empty!")
  return(ret)
}
inputLogic = function(x) as.logical(x)
inputFun = function(string) eval( parse(text= paste("function(x)",string)) , envir=parent.frame() )


#Obtain results and store in table and create table frame:
getTableFormat = function(x,dec=2) {
  tab = round(x,dec)
  cbind(X=rownames(tab),tab)
}

#helpfunction to get table from file into list structure
importData = function(fn) {
  require(euroformix)
  euroformix::sample_tableToList(euroformix::tableReader(fn))
}

#x=logLikCalcs[indH1,lastIdx]
#Helpfunction for summing very small non-logged values (Avoiding underflow)
expsum2log = function(x, maxLog=500) { #
  mm = max(x) - maxLog #obtain minimum (most likely underlfor)
  log(sum(exp(x - mm))) + mm
}

#Helpfunction to get LR from two vectors (log10 scale)
getjointLR = function(x,y)  (expsum2log(x) - expsum2log(y))/log(10)


#Helpfunction to calculate the LR values from manual table
calcFromTable = function(tab) {
  nref = ncol(tab) - 1
  refNames = colnames(tab)[1:nref]
  logLiks = as.numeric(unlist(tab[,nref+1]))
  
  outtab <-  NULL 
  for(r in seq_along(refNames)) {
    #  r=1
    ref = refNames[r]
    H1ind = tab[,r]>0
    H2ind = tab[,r]==0
    jointLR = getjointLR(logLiks[H1ind],logLiks[H2ind])
    
    #calculate default LR hypothesis:
    H2ind = rowSums(tab[,1:nref]>0)==0 #no conditionals
    H1ind = rowSums(tab[,setdiff(1:nref,r)]>0)==0 & !H2ind #only ref is conditoned on
    pairLR = getjointLR(logLiks[H1ind],logLiks[H2ind])
    new <- c(pairLR,jointLR)
    
    outtab = rbind(outtab, new)
  }
  colnames(outtab) = c("conventional","exhaustive")
  rownames(outtab) = refNames
  return(outtab)  
}

#Main function for calculating likelihood for exhaustive set of hypotheses
calculateExhaustive = function(samples,refData, popFreq,NOC=3, kit="Fusion 6C",  condRefNames = NULL, #data settings
                               modelSetting = list(AT=100,fst=0.01, pC=0.05, lambda=0.01), #model settings
                               modelConfig = list(degrade=TRUE, stutterBW=FALSE,stutterFW=FALSE,priorBWS = NULL,priorFWS = NULL), #model config
                               optimConfig = list(seed=NULL,nDone=3, steptol=1e-4, minF=NULL, normalize=TRUE,adjQbp=FALSE),
                               storeModelFit=TRUE, verbose = FALSE) { #additional

  doCalcMLE = function(cond,nonCond=NULL) { #CALC QUAN MLE (helpfunctions)
    euroformix::calcMLE(NOC,samples,popFreq,refData,cond,nonCond,kit,
                        modelConfig$degrade,modelConfig$stutterBW,modelConfig$stutterFW,
                        modelSetting$AT, modelSetting$pC, modelSetting$lambda,modelSetting$fst,
                        nDone=optimConfig$nDone,seed=optimConfig$seed,steptol=optimConfig$steptol,
                        minF=optimConfig$minF, normalize=optimConfig$normalize, adjQbp=optimConfig$adjQbp,
                        priorBWS=modelConfig$priorBWS, priorFWS=modelConfig$priorFWS, verbose=FALSE)
  }

  refNames = names(refData) #names to consider as POI
  POIrefNames = setdiff(refNames,condRefNames) #names to consider as POI
#  print(samples)
#  print(refNames)
  nRefs = length(refNames) #number of refs
  nConds = length(condRefNames) #number of conds
  nPOIs = length(POIrefNames) #number of POIs
  mleFit = list() #store model fits
  
  if(nPOIs==0 || nConds==NOC) {
    print("There were no traversion to be done. Returning from function...")
    return(NULL)
  }
  
  #Calculate Hd first:
  isCond = which(refNames%in%condRefNames) #Indicate refernces that are always conditoned on
  cond0 = rep(0,nRefs) #default condition (always)
  cond0[isCond] = seq_len(nConds) #always conditoned like this
  nonCond0 = setdiff(seq_len(nRefs),isCond) #references not used (non-contributors)

  myprint = function(x) print(round(x,3))
  if(verbose) print("Calculate with no (POI) conditionals...")
  mleHd = doCalcMLE(cond0, nonCond0) #doCalcMLE(NULL, seq_len(nRefs))
  mleFit[[1]] = mleHd #always insert this (used to obtain info)
  logLikHd = mleHd$fit$loglik
  param_hd = mleHd$fit$thetahat2
  
  #CALCULATING HP fits
  hypCalcs = c(rep(0,nPOIs),logLik=logLikHd,param_hd) #store likelihoods in a matrix
  if(verbose) myprint(hypCalcs)
  #Traverse all possible candidates: Cant exceed number of contributors
  nCondRange = 1:min(nPOIs,NOC-nConds) #range to traverse
  for(nCondExtra in nCondRange) { #traverse different number of contributions
    # nCond=1
    refCombMat = gtools::combinations(nPOIs,nCondExtra) #possible ways to combine references
    if(verbose) print(paste0("Number of (POI) conditionals: ",nCondExtra, " (",nrow(refCombMat)," traverses)"))
    for(refComb_idx in seq_len(nrow(refCombMat))) {
      if(verbose) print(paste0("Iteration=",refComb_idx,":"))
      #    refComb_idx =1
      refUse = nonCond0[refCombMat[refComb_idx,]] #which to use (adjust index for non-contriutors)
      cond = cond0
      cond[refUse] = seq_along(refUse) + max(cond0)
      
      #Calculate Hp fit:
      nonCond = setdiff(seq_len(nRefs),which(cond>0)) #references not used
      if(length(nonCond)==0) nonCond = NULL
      mleHp = doCalcMLE(cond, nonCond)
      if(storeModelFit) mleFit[[length(mleFit)+1]] = mleHp #store model fit
      logLikHp = mleHp$fit$loglik
      param_hp = mleHp$fit$thetahat2
      row = c(cond[nonCond0],logLik=logLikHp,param_hp) #store conditonal of non-contributors only
      if(verbose) myprint(row) #show few decimals only
      hypCalcs = rbind(hypCalcs, row)
      #plotTopEPG2(mleHp)
    }
  }
  colnames(hypCalcs) = c(POIrefNames,"logLik",names(param_hd))
  rownames(hypCalcs) = paste0("Hyp",seq_len(nrow(hypCalcs)))
  #CALCULATIONS DONE

  lastIdx = nPOIs + 1
  #log10LR_simple = (hypCalcs[-1,lastIdx] - hypCalcs[1,lastIdx])/log(10) #obtain pairwise
  conventional = (hypCalcs[seq_len(nPOIs) + 1,lastIdx] - hypCalcs[1,lastIdx])/log(10) #obtain pairwise
  exhaustive = rep(NA,nPOIs) #obtain LR for each contributor
  for(refIdx in seq_len(nPOIs)) {
    # refIdx = 1
    indH1=hypCalcs[,refIdx]>0 #POI is contributors
    indH2=hypCalcs[,refIdx]==0 #POI is not contributors
    sumH1 = expsum2log(hypCalcs[indH1,lastIdx]) # log(sum(exp(hypCalcs[indH1,3])))
    sumH2 = expsum2log(hypCalcs[indH2,lastIdx])
    exhaustive[refIdx] = (sumH1 - sumH2)/log(10)
  }
  names(exhaustive)  <- names(conventional) <- POIrefNames
  LRtable = cbind(conventional, exhaustive)

  #obtain table also including Parameters
  return(list(LRtable=LRtable,hypCalcs=hypCalcs,mleFit=mleFit))
} #end for each sample

writeTable = function(tableList, outfn, sig=2) {
  format = function(x) {
    cn = colnames(x)
    rn = rownames(x)
    out = round(x,sig)
    return(cbind(X=rn,out))
  }
  for(i in seq_along(tableList)) {
    write.table(format(tableList),outfn,sep=";",quote=F,row.names = F,append=i>i, col.names = F)
  } 
}

