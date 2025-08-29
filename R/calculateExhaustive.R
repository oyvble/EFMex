
#Main function for calculating likelihood for exhaustive set of hypotheses
calculateExhaustive = function(samples,refData, popFreq,NOC=3, kit="Fusion 6C",  condRefNames = NULL, #data settings
                               modelSetting = list(AT=100,fst=0.01, pC=0.05, lambda=0.01), #model settings
                               modelConfig = list(degrade=TRUE, stutterBW=FALSE,stutterFW=FALSE,priorBWS = NULL,priorFWS = NULL), #model config
                               optimConfig = list(seed=NULL,nDone=3, steptol=1e-4, minF=NULL, normalize=TRUE,adjQbp=FALSE),
                               storeModelFit=TRUE, verbose = FALSE, LRthresh=NULL, adjustNumSubHyps=FALSE) { #additional
  
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
  #if(verbose) myprint(hypCalcs)
  peelOffRefIdx <- peeledOff <- NULL #indicate which reference to not include in exhaustive
  #Traverse all possible candidates: Cant exceed number of contributors
  nCondRange = 1:min(nPOIs,NOC-nConds) #range to traverse
  for(nCondExtra in nCondRange) { #traverse different number of contributions
    #  nCondExtra=3
    refCombMat = gtools::combinations(nPOIs,nCondExtra) #possible ways to combine references
    if(verbose) print(paste0("Number of (POI) conditionals: ",nCondExtra, " (",nrow(refCombMat)," traverses)"))
    for(refComb_idx in seq_len(nrow(refCombMat))) {
      #      refComb_idx=3
      if(verbose) print(paste0("Iteration=",refComb_idx,":"))
      refCombVec = refCombMat[refComb_idx,] #obtain vector with references to include
      if(!is.null(peelOffRefIdx) && any(peelOffRefIdx%in%refCombVec)) next
      refUse = nonCond0[refCombVec] #which to use (adjust index for non-contriutors)
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
      #if(verbose) myprint(row) #show few decimals only
      hypCalcs = rbind(hypCalcs, row)
      #plotTopEPG2(mleHp)
    }
    
    #Special handling when conditioning on 1 extra: possible to peel off situations when a reference  which does not fit is considered in the hypothesis
    if(!is.null(LRthresh) && nCondExtra==1) {
      #Obtain conventional LR for all POIs:
      logLiks = hypCalcs[,nPOIs+1] 
      pairwiseLRs = exp(logLiks[-1] - logLiks[1]) #calculate pairwise (get ordinary scale)
      peelOffRefIdx = which(pairwiseLRs<LRthresh) #dont continue with those with small LR
      if(length(peelOffRefIdx)>0) {
        peeledOff = POIrefNames[peelOffRefIdx]
        if(verbose) print(paste0("Pealing off following references: ",paste0(peeledOff,collapse="/")))
      }
    }
  }
  colnames(hypCalcs) = c(POIrefNames,"logLik",names(param_hd))
  rownames(hypCalcs) = paste0("Hyp",seq_len(nrow(hypCalcs)))
  #CALCULATIONS DONE
  
  lastIdx = nPOIs + 1 #obtain column with likvals
  #log10LR_simple = (hypCalcs[-1,lastIdx] - hypCalcs[1,lastIdx])/log(10) #obtain pairwise
  conventional = (hypCalcs[seq_len(nPOIs) + 1,lastIdx] - hypCalcs[1,lastIdx])/log(10) #obtain pairwise
  exhaustive = rep(NA,nPOIs) #obtain LR for each contributor
  GLR = rep(NA,nPOIs) #obtain LR for each contributor
  for(refIdx in seq_len(nPOIs)) {
    # refIdx = 1
    indH1=hypCalcs[,refIdx]>0 #POI is contributors
    indH2=hypCalcs[,refIdx]==0 #POI is not contributors
    nHypsH1 = sum(indH1) #number of hypotheses under H1
    nHypsH2 = sum(indH2) #number of hypotheses under H2
    exhaustive[refIdx] = getjointLR(hypCalcs[indH1,lastIdx],hypCalcs[indH2,lastIdx])  #returned LR on log10 scale
    GLR[refIdx] = getGLR(hypCalcs[indH1,lastIdx],hypCalcs[indH2,lastIdx])  #returned LR on log10 scale
    if(adjustNumSubHyps) exhaustive[refIdx] = exhaustive[refIdx] + log10(nHypsH2) - log10(nHypsH1) #adjust for number of hypotheses
  }
  names(exhaustive)  <- names(conventional) <- names(GLR) <- POIrefNames
  LRtable = cbind(conventional, exhaustive, GLR)
  colnames(LRtable) = c("Conventional","Exhaustive","GLR") #modify names
  
  #obtain table also including Parameters
  return(list(LRtable=LRtable,hypCalcs=hypCalcs,mleFit=mleFit,peeledOff=peeledOff))
} #end for each sample
