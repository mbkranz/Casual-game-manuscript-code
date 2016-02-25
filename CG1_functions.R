#Casual Games GroupD (Adaptive Reasoning/WM group)--> 
#group d dependent terms: gamelist,cheat list (e.g.,cheatgude etc), reduction of data to variables section
###to check before running: 
#1.) excluded outliers in ralldata creation for entire subject exclusion
#2.) excluded game specific subs (e.g., if doing hlm with all subs do not exclude)
#3.) include aengie quest in final and first composite calc? see reduction of data variables

############packages############

#install.packages(c("reshape","ggplot2","ggthemes","foreign","plyr","pastecs","xlsx","XML","texreg","xtable","ggm","QuantPsych","car")) 
#library(reshape); library(ggplot2);  library(plyr); library(pastecs); library(xlsx); library(XML); library(texreg); library(xtable);library(QuantPsyc); library(car)

###functions#####

#transforms HTML objects created by htmlreg into list/data frame for each lm object in list
html2dataframefxn<-function(x){
  require(XML)
  listreg<-readHTMLTable(x)
listregdata<-as.data.frame(listreg)
return(listregdata)
}
View.xls <- function(data, autofilter=TRUE) {
  require(XLConnect)
  # data: data frame
  # autofilter: whether to apply a filter to make sorting and filtering easier
  open_command <- switch(Sys.info()[['sysname']],
                         Windows= 'open',
                         Linux  = 'xdg-open',
                         Darwin = 'open')
  require(XLConnect)
  temp_file <- paste0(tempfile(), '.xlsx')
  wb <- loadWorkbook(temp_file, create = TRUE)
  createSheet(wb, name = "temp")
  writeWorksheet(wb, data, sheet = "temp", startRow = 1, startCol = 1)
  if (autofilter) setAutoFilter(wb, 'temp', aref('A1', dim(data)))
  saveWorkbook(wb, )
  system(paste(open_command, temp_file))
}
corstarsl <- function(x){ 
  #corstarsl: from http://myowelt.blogspot.com/2008/04/beautiful-correlation-tables-in-r.html (to be used with xtable)
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}


outlierfxn<-function(data,variablelist,outlieroutput=FALSE,std=3){
  #outliers (outputs a list: [1] windsorized value data set and [2]makes a list of subject outliers per task)###
  wdata<-data
  outliers<-data.frame(subs=data[,1])
  for(variable in variablelist){
    #probablity values for outliers (for mean=0,sd=1-->i.e., zscore probablity)
    uwind<-pnorm(std)
    lwind<-pnorm(-std)
    #gets values of variable for probabilities
    cut_point_top <-qnorm(p=uwind,mean=mean(data[,variable],na.rm=TRUE),sd=sd(data[,variable],na.rm=TRUE))
    cut_point_bottom <-qnorm(p=lwind,mean=mean(data[,variable],na.rm=TRUE),sd=sd(data[,variable],na.rm=TRUE))
    #replaces outlier variable values within new wdata dataframe
    wdata[,variable]<-ifelse(data[,variable]>cut_point_top,cut_point_top,
                             ifelse(data[,variable]<cut_point_bottom,cut_point_bottom,data[,variable]))
    #makes dataset with outlier values that were replaced with windsorized values (calling from inputted data dataframe)
    outliers[,variable]<-ifelse((data[,variable]>cut_point_top |data[,variable]<cut_point_bottom),data[,variable],NA)}
  if(outlieroutput==TRUE){outputdata<-outliers
  }else{outputdata<-wdata}  
  return(outputdata)}



compscoresfxn<-function(tasklist,fdata){
  #calculates average standardized composite scores for all in list
  col<-0 
tempdata<-fdata[,tasklist] 
for(task in tasklist){col<-col+1
attach(tempdata)
tempdata[,col]<-scale(fdata[,task])
detach(tempdata)}
comp<-rowMeans(tempdata,na.rm=TRUE)
return(comp)}
corstarsmelt<-function(var1,var2){
  #creates correlation coefficient with signficance star (to be used in ddply for melted variables)
  require(Hmisc) 
  x<-cbind(var1,var2)
  R <- round(rcorr(x)$r,2)[2]
  p <- rcorr(x)$P  [2]
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  corr_withstars<-paste(R,mystars,sep="")
  return(corr_withstars)
}
bootcifxn<-function(data,boottest){
  #bootstrap function that uses same samples for multiple correlation coefficients
  for(var in 1:nrow(data)){
    bootci<-boot.ci(boottest,type="bca",index=var)
    cichar<-paste("[",round(bootci$bca[4],2),",",round(bootci$bca[5],2),"]",sep="")
    data$BCa[var]<-cichar
  }
  return(data)}
bootcorr<-function(idnames,data,i){
  #obtain bootstrap ci and put into data frame
  d<-data[i,]
  data_melt<-melt(d,id=idnames) 
  data_melt<-rename(data_melt,c(variable="gamemeas",value="gamescore"))
  data_melt2<-melt(data_melt,id=c("gamemeas","gamescore"))
  data_melt2<-rename(data_melt2,c(variable="cogmeas",value="cogscore"))
  data_mat<-ddply(data_melt2,.(gamemeas,cogmeas),summarise,corr=cor(gamescore,cogscore,use="pairwise.complete.obs"))
  data_corr<-data_mat$corr
  return(data_corr)}
bootlmbetafxn<-function(lmobject,data,i){
  #lm beta bootstrap function
  d<-data[i,]
  test<-as.character(lmobject$call)
  bootlmobj<-eval(parse(text=paste(test[1],"(formula=",test[2],",data=d)",sep="")))
  lmbetaobj<-lm.beta(bootlmobj)
  return(lmbetaobj)
}
regfxn<-function(lmobject){
  #regfxn gives dataframe in vertical format (rather than horizontal which texreg and stargazer does)
  #-->below are commented out variations of stats reported ###current version bootstraps standardized beta coefficients!!
  
  #bootstrapped beta coefficient
  #boot<-Boot(lmobject,R=1000)
  #summboot<-summary(boot) #summary of boot object
  #confboot<-confint(lmobject) #conf interval from boot object
  #bootstrapped standardized beta coefficient
  lmbeta_boot<-boot(data=lmobject$model,statistic=bootlmbetafxn,R=1000,lmobject=lmobject)
  confboot<-confint(lmbeta_boot,type="bca")
  summ<-summary(lmobject) #summary obj (summary on lm object)
  rsq<-round(summ$r.squared,2) #num (R squared value)                 
  adjrsq<-round(summ$adj.r.squared,2) #num (Adjusted R^2 from summary of lm object)
  fstat<-round(summ$fstatistic,2) #num (F stat from summary of lm object)
  se_norm<-round(summ$coefficients[,2],2) #standard error based on normal distribution
  tval_norm<-round(summ$coefficients[,3],2) #tvalue based on normality assump
  pval_norm<-round(summ$coefficients[,4],2) #pval based on norm assumption
  AIC<-round(AIC(lmobject),2) #num (AIC stat)
  BIC<-round(BIC(lmobject),2) #num (BIC stat)
  coeff<-round(summ$coefficients[,1],2) #num (coefficient values from Boot and lmobject are the same-- use summ$coefficients[,1] for same thing)
  #bootstrap stats
  bootlower<-round(confboot[,1],2) #lower confidence interval (2.5% is default)
  bootupper<-round(confboot[,2],2) #upper confidence interval (97.5% is default)
  #bootbias<-summboot[,3] #bias of bootstrap (original estimate- the average of bootstrap sample estimates)
  #bootSE<-summboot[,4] #standard deviation of the bootstrap replicates
  coeffnames<-row.names(summ$coefficients)  #fixed effects/predictor/coefficient names
  model<-as.character(lmobject$call)[2] #character (type in lm code)
  #standardized beta coefficients
  stdcoeff<-round(lm.beta(lmobject),2) #standardized beta coefficient from lm.beta package (need NA filler as it doesn't have intercept parameter estimate)
  filler<-rep(NA,length(coeff)-1) #filler for variables with only one value              
  #beta coefficient with confidence intervals (bootstrapped)
  bootconf<-paste("[",bootlower,",",bootupper,"]",sep="")
  betawbootconf<-paste(stdcoeff,bootconf,sep=" ")
  #model out: all stats in table format for one model 
  #                  modelout<-data.frame(model=c(model,filler),coefficients=coeffnames,
  #                             betaestimates=coeff, normSE=se_norm,tvalue=tval_norm,pvalue=pval_norm,
  #                             stdbetaestimates=c(NA,stdcoeff),
  #                             bootstrap_lowerconf=bootlower,bootstrap_upperconf=bootupper,
  #                             bootBias=bootbias, bootSE=bootSE,vif=c(NA,filler),
  #                             rsq=c(rsq,filler),adjrsq=c(adjrsq,filler),                         
  #                            AIC=c(AIC,filler),BIC=c(BIC,filler),Fchange=c(NA,filler))
  
  # #reduced stats
  #                   modelout<-data.frame(model=c(model,filler),coefficients=coeffnames,
  #                                           betaestimates=betawbootconf, stdbetaestimates=c(NA,stdcoeff),tvalue=tval_norm,pvalue=pval_norm,
  #                                           rsq=c(rsq,filler),adjrsq=c(adjrsq,filler),                         
  #                                        AIC=c(AIC,filler),BIC=c(BIC,filler),Fchange=c(NA,filler)) 
  
  
  #   #reduced stats for summary in paper
  #   redoxmodelout<-data.frame(model=model,coefficients=paste("Step ",length(coeffnames)-1,": ",
  #                                                            coeffnames[length(coeffnames)],sep=""),
  #                             betaestimates=coeff[length(coeff)],bootstrapci=bootconf[length(bootconf)], 
  #                             normSE=se_norm[length(se_norm)],tvalue=tval_norm[length(tval_norm)],
  #                             pvalue=pval_norm[length(pval_norm)],
  #                             stdbetaestimates=stdcoeff[length(stdcoeff)],
  #                             rsq=rsq,adjrsq=adjrsq,                         
  #                             AIC=AIC,BIC=BIC,Fchange=NA) 
  
  #super reduced reduced stats for summary in paper
  redoxmodelout<-data.frame(model=model,coefficients=paste("Step ",length(coeffnames)-1,": ",
                                                           coeffnames[length(coeffnames)],sep=""),stdbetaestimates=betawbootconf[length(betawbootconf)], adjrsq=adjrsq,Fchange=NA)
  
  modelout<-redoxmodelout
  return(modelout)}
hregfxn<-function(lmlist,dvarind){
  #hregfxn combines hierarchical models and also includes model comparison statistics (e.g., F change) 
  #and stats used for specifically multiple predictors(e.g.,variance inflation factor)
  #the dvarind loop places a row before first step in each model and writes title of dependent var predicting
  i=0
  for(lmobj in lmlist){
    i=i+1
    if(i==1){
      lmtable<-regfxn(lmobj)
      if(dvarind==1){
        firstrow<-regfxn(lmobj)[1,]
        firstrow[1,]<-as.character("NA")
        predictvar<-paste("Predicting",names(lmobj$model[1]),"session average game achievement",sep=" ")
        firstrow$coefficients<-as.character(firstrow$coefficients)
        firstrow$coefficients[1]<-predictvar
        firstrow$coefficients<-as.factor(firstrow$coefficients)
        lmtable<-rbind(firstrow,lmtable)
      }
    }else{
      lmtable2<-regfxn(lmobj)
      comp1<-anova(lmlist[[i-1]],lmlist[[i]])  #lmlist[[i]] same as lmobj (compares F stat of this model to previous nested model)
      vif1<-vif(lmobj) #variance inflation factor for each predictor--> p. 293 of Andy Fields book: average above 10=cause for concern, average above 1= substantially > than 1--> some bias
      #   lmtable2$vif<-c(NA,vif1) 
      Fchange<-comp1[2,"F"]
      Fsigchange<-comp1[2,"Pr(>F)"]
      lmtable2$Fchange[1]<-round(Fsigchange,3)
      lmtable<-rbind(lmtable,lmtable2) 
    }  
  }
  return(lmtable)}
avpfxn<-function(x){
  ####added variable plots (to show effect of each variable adjusting for other variables in models)######
  #makes a variable for plot name and for the variable name in an added variable plot (from a melted )
  xchar<-as.character(x)
  vars<-strsplit(xchar,"\\.")
  datavars<-data.frame(
    plotname=vars[[1]][1],
    plotvar=vars[[1]][2])
  return(datavars)
}
avpfxn2<-function(lmobject,DV,lmobjecttype){
  avPlotsobject<-car::avPlots(lmobject,id.n=2)
  avpdata<-as.data.frame(avPlotsobject)
  avpdata$subs<-row.names(avpdata)
  avpdata_melt<-reshape::melt(avpdata,id="subs")
  varsforplot<-plyr::ldply(avpdata_melt$variable,avpfxn)
  dataforplot<-cbind(avpdata_melt,varsforplot)
  dataforplot$plottype<-lmobjecttype
  dataforplot$plotvar<-ifelse(dataforplot$plotvar==DV,"highlevel","predictor")
  subsetdata<-subset(dataforplot,,c(subs,value:plottype))
  castforplot<-as.data.frame(cast(subsetdata,subs+plotname+plottype~plotvar))
  return(castforplot)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Multiple plot function
  #
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  #
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



