


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
getTableFormat = function(x,addRownames=TRUE,dec=2) {
  tab = round(x,dec)
  if(addRownames) tab = cbind(ID=rownames(tab),tab)
  rownames(tab) = NULL
  return(tab)
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
getGLR = function(x,y)  (max(x) - max(y))/log(10)

#Helpfunction to calculate the LR values from manual table (also used)
calcFromTable = function(tab,method="Exhaustive",addShaving=FALSE, adjustNumSubHyps=FALSE) {
   
  getOutTable = function(tab_sel) {
    nref = ncol(tab_sel) - 1
    refNames = colnames(tab_sel)[1:nref]
    logLiks = as.numeric(unlist(tab_sel[,nref+1]))
    
    outtab <-  NULL 
    for(r in seq_along(refNames)) {
      #  r=1
      H1ind = tab_sel[,r]>0
      H2ind = tab_sel[,r]==0
      
      GLR <- jointLR <- pairLR <- NaN
      nHypsH1 = sum(H1ind) #number of subhypotheses in H1
      nHypsH2 = sum(H2ind) #number of subhypotheses in H2
      if(nHypsH1>0 && nHypsH2>0) {
        GLR = getGLR(logLiks[H1ind],logLiks[H2ind])
        jointLR = getjointLR(logLiks[H1ind],logLiks[H2ind])
        if(adjustNumSubHyps) jointLR = jointLR + log10(nHypsH2) - log10(nHypsH1) #adjust for number of hypotheses
      }
      
      #calculate default LR hypothesis:
      H2ind = rowSums(tab_sel[,1:nref,drop=FALSE]>0)==0 #no conditionals
      H1ind = rowSums(tab_sel[,setdiff(1:nref,r),drop=FALSE]>0)==0 & !H2ind #only ref is conditoned on
      if(sum(H1ind)>0 && sum(H2ind)>0) pairLR = getjointLR(logLiks[H1ind],logLiks[H2ind])
      new <- c(pairLR,jointLR,GLR)
      outtab = rbind(outtab, new)
    }
    colnames(outtab) = c("Conventional","Exhaustive","GLR")
    rownames(outtab) = refNames
    return(outtab)
  }

  outtab = getOutTable(tab) #obtain original output table
  #Select columns to show in GUI (exhaustive or GLR)  
  selCols = colnames(outtab)[1:2] #selected columns to show
  selCols[2] = method #may be modified to GLR
  outtab = outtab[,selCols,drop=FALSE] #keep as matrix
  
  #adding shaving method:
  outtabExtended = outtab
  if(addShaving) {
    nref = nrow(outtab) #get number of references
    
    #Removing references with exhaustive/GLR LR<1 
    keepRefsBool = outtab[,2]>1 #only keep those with LR>1
    keepRefsInd = which(keepRefsBool) #get index of those to keep
    shaveRefsInd = setdiff(seq_len(nref),keepRefsInd) #get index of those to shave
    
    #Update table obtained from exhaustive calculations
    keepCols = c(keepRefsInd,ncol(tab)) #also keep last column (this is the log-likelihoods)
    keepRows = rowSums(tab[,shaveRefsInd,drop=FALSE]>0)==0  #indicate which rows to keep
    tabShaved = tab[keepRows,keepCols,drop=FALSE] #update table (shaved)
    outtabShaved = getOutTable(tabShaved) #obtain original output table
    outtabExtended = cbind(outtabExtended,Shaving=NA) #create extension of table
    outtabExtended[keepRefsInd,3] = outtabShaved[,method] #include the shaving results
  } 
  
  return(outtabExtended)  
}

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

