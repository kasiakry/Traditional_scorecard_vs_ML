iv.mult <- function(df,y,summary=FALSE,vars=NULL,verbose=FALSE,rcontrol=NULL) {
  if(verbose) {
    cat(paste("Started processing of data frame:", deparse(substitute(df)),"\n"))
  }
  
  if(is.null(vars)) {
    vars <- names(df)[names(df) !=y]
  }
  
  ivlist <- lapply(vars, function (x) {
    if(is.numeric(df[,x])) {
      if (verbose) cat(paste("Calling iv.num for variable:", x, "\n"))
      iv.num(df,x,y,verbose=verbose,rcontrol=rcontrol)
    } else {
      if (verbose) cat(paste("Calling iv.str for variable:", x, "\n"))
      iv.str(df,x,y,verbose=verbose)  
    }
  }
  )
  
  if (summary) {
    if (verbose) cat(paste("Preparing summary","\n"))
    ivlist <- rbind.fill(ivlist)
    ivlist <- sqldf("select 
                        variable as Variable,
                        sum(miv) as InformationValue, 
                        count(*) as Bins,
                        sum(case when outcome_0 = 0 or outcome_1 = 0 then 1 else 0 end) as ZeroBins
                     from ivlist 
                     group by variable 
                     order by InformationValue desc") 
    
    ivlist$Strength[ivlist$InformationValue >= 1] <- 1
    ivlist$Strength[ivlist$InformationValue >= .5 & ivlist$InformationValue < 1] <- 2
    ivlist$Strength[ivlist$InformationValue >= .2 & ivlist$InformationValue < .5] <- 3
    ivlist$Strength[ivlist$InformationValue >= .1 & ivlist$InformationValue < .2] <- 4
    ivlist$Strength[ivlist$InformationValue >= .02 & ivlist$InformationValue < .1] <- 5
    ivlist$Strength[ivlist$InformationValue < .02] <- 6
    ivlist$Strength <- factor(ivlist$Strength, levels=c(1,2,3,4,5,6), 
                              labels= c("Suspicious","Very strong","Strong","Average","Weak","Wery weak"))
  }
  ivlist
}



iv.str <- function(df,x,y,verbose=FALSE) {
  if (!(class(df)=="data.frame")) {
    stop("Parameter df has to be a data frame.")
  } 
  if (!(is.character(df[, x]) || is.factor(df[, x]))) {
    stop(paste("Input is not a character or factor! Variable:", x))
  } 
  if (!(is.numeric(df[, y]) || is.factor(df[, y]))) {
    stop("Outcome is not a number nor factor!")
  } 
  if (length(unique(df[, y])) != 2) {
    if(verbose) paste(cat(unique(df[,y])),"\n")
    stop("Not a binary outcome")
  }
  if (!(all(sort(unique(df[, y])) == c(0,1))) && is.numeric(df[,y])) {
    stop("Numeric outcome has to be encoded as 0 (good) and 1 (bad). \n")
  }
  if (is.factor(df[,y]) && all(levels(df[,y])[order(levels(df[,y]))]==c("bad","good"))) {
    if (verbose) cat("Assuming good = level 'good' and bad = level 'bad' \n")
    total_1 <- sum(df[,y]=="bad")
  } else if (is.factor(df[,y])) {
    if (verbose) cat("Factor: Assuming bad = level 2 and good = level 1 \n")
    total_1 <- sum(as.integer(df[, y])-1)
    
  } else {
    if (verbose) cat("Numeric: Assuming bad = 1 and good = 0 \n")
    total_1 <-sum(df[, y])
    
  }
  
  outcome_0 <- outcome_1 <- NULL # This is needed to avoid NOTES about not visible binding from R CMD check
  
  total_0 <- nrow(df) - total_1      
  iv_data <- data.frame(unclass(table(df[, x],df[, y])))
  
  if (all(names(iv_data)==c("bad","good"))) {
    iv_data <- iv_data[,c(2,1)]
  }
  
  
  names(iv_data) <- c("outcome_0","outcome_1")
  iv_data <-  within(iv_data, {
    class <- row.names(iv_data)
    variable <- x
    pct_0 <- outcome_0 / total_0
    pct_1 <- outcome_1 / total_1
    odds <-  pct_0 / pct_1
    woe <- log(odds)
    miv <- (pct_0 - pct_1) * woe    
  })
  
  if(is.factor(df[,x])) {
    iv_data$class <- factor(iv_data$class,levels=levels(df[,x]))
  }  
  
  iv_data <- iv_data[c("variable","class","outcome_0","outcome_1","pct_0","pct_1","odds","woe","miv")]
  
  if(any(iv_data$outcome_0 == 0) | any(iv_data$outcome_1 == 0)) {
    warning("Some group for outcome 0 has zero count. This will result in -Inf or Inf WOE. Replacing - ODDS=1, WoE=0, MIV=0. \n The bin is either too small or suspiciously predictive. \n You should fix this before running any model. It does not make any sense to keep WoE = 0 for such bin.")
    iv_data$woe <- ifelse(is.infinite(iv_data$woe),0,iv_data$woe)
    iv_data$miv <- ifelse(is.infinite(iv_data$miv),0,iv_data$miv)
    iv_data$odds <-ifelse(is.infinite(iv_data$odds),1,iv_data$odds)
  }
  
  rownames(iv_data) <- NULL
  cat (paste("Information Value",round(sum(iv_data$miv),2),"\n"))
  iv_data
}