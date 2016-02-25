##################descriptive stats#############
library(QuantPsyc)
library(pastecs)
library(xlsx)
#means,stds of individual tasks (cognitive and games)
#skew,kurtosis of individual tasks,cognitive composites, and games
#descriptive stats (currently game and cognitive stat tables are exported to excel-->if LaTex is used (or want to use html), can use xtable
descstatnames<-c("mean","std.dev","skewness","kurtosis")
gamestatnames<-c("game","nbr.na")
#individual task measure stats
icogtasks<-c(gF,WM,PS) #pulls individual tasks from atomic vectors of task names in composite calculation section

#categorization of task measures for descriptive table (iandcomp_desc dataframe)
gFname<-rep("gF",length(gF))
WMname<-rep("WM",length(WM))
PSname<-rep("PSpeed",length(PS))
COMPname<-rep("Construct Composite",length(cogpred))
icogtaskconstructnames<-c(gFname,WMname,PSname,COMPname)
icogtasks_desc<-ddply(cog_game,.(Condition),function(x)as.data.frame(t(pastecs::stat.desc(cog_game[,5:15],norm=TRUE,basic=TRUE))))
icogtasks_desc<-ddply(cog_game,.(Condition),function(x)as.data.frame(t(pastecs::stat.desc(cog_game[,5:15],norm=TRUE,basic=TRUE))))
#t tests for group differences
cog_game.melt<-subset(cog_game,select=c(subs,Condition,MatrixReasoningACC:DigitSymbolCoding),subset=subs %in% compdata$subs)
cog_game.melt<-melt(cog_game.melt,id=c("subs","Condition"))
cog_game.ttest<-ddply(cog_game.melt,.(variable),function(x) {test<-t.test(value~Condition,data=x,var.equal=TRUE);difftest<-data.frame(tstat=test$statistic,df=test$parameter,pval=test$p.value)
                                                                    return(difftest)})
cog_game.ttest.welch<-ddply(cog_game.melt,.(variable),function(x) {test<-t.test(value~Condition,data=x);difftest<-data.frame(tstat=test$statistic,df=test$parameter,pval=test$p.value)
return(difftest)})

icogtasks_desc<-icogtasks_desc[descstatnames]
#individual game measure stats
game_desc<-as.data.frame(t(stat.desc(subset(cog_game_redox,,select=twothree1:sushi10),norm=TRUE,basic=TRUE)))
game_desc<-cbind(data.frame(games=row.names(game_desc)),game_desc[c("nbr.val",descstatnames)])
game_desc$mean<-round(game_desc$mean,2)
game_desc$std.dev<-round(game_desc$std.dev,2)
game_desc$skewness<-round(game_desc$skewness,2)
game_desc$kurtosis<-round(game_desc$kurtosis,2)

#composite game measure stats
gamecomp_desc<-as.data.frame(t(stat.desc(subset(compdata,,select=final:first),norm=TRUE,basic=TRUE)))
gamecomp_desc<-gamecomp_desc[descstatnames]


write.xlsx(game_desc,file="casualgames_learning_table.xlsx",sheetName="indgames_desc2",append=TRUE,row.names=FALSE)
#composite cognitive task measure stats
cogcomp_desc<-as.data.frame(t(stat.desc(subset(compdata,,select=gF:PSpeed),norm=TRUE,basic=TRUE)))
cogcomp_desc<-cogcomp_desc[descstatnames]

compdata.melt<-subset(compdata,select=c(subs,Condition,gF:PSpeed))
compdata.melt<-melt(compdata.melt,id=c("subs","Condition"))
compdata.ttest<-ddply(compdata.melt,.(variable),function(x) {test<-t.test(value~Condition,data=x,var.equal=TRUE);difftest<-data.frame(tstat=test$statistic,df=test$parameter,pval=test$p.value)
return(difftest)})
compdata.ttest.welch<-ddply(compdata.melt,.(variable),function(x) {test<-t.test(value~Condition,data=x);difftest<-data.frame(tstat=test$statistic,df=test$parameter,pval=test$p.value)
return(difftest)})

#individual and composite in one sheet
iandcomp_desc<-rbind(icogtasks_desc,cogcomp_desc)
iandcomp_desc<-data.frame(construct_name=icogtaskconstructnames,task=row.names(iandcomp_desc),iandcomp_desc)
write.xlsx(iandcomp_desc,file="casualgames_learning_table.xlsx",sheetName="cogtasks_desc",append=TRUE,row.names=FALSE)


#demogs
#Cheaters (excluded anyone who played any of the 4 games)-->from data of all people
cheaterdata<-subset(alldata,Condition=="A",select=cheattwothree:cheatsilv)
testcheat<-rowSums(cheaterdata,na.rm=TRUE)
cheatercount<-count(testcheat>0)
cheatnum<-cheatercount$freq[2]
#VGplay
VGplay<-subset(alldata,Condition=="A",select=PreScreenVGplay_sum)
VGplaycount<-count(VGplay>3)
VGplaynum<-VGplaycount$freq[2]
#male/female
demogdata<-join(subset(compdata,,subs),ralldata,by="subs",type="inner")
gender<-subset(demogdata,
               select=Gender) 
gendercount<-count(gender)
malenum<-gendercount$freq[2]
totalnum<-nrow(gender)
#age
age<-subset(demogdata,,
            select=Age) 
agecount<-round(stat.desc(age,norm=TRUE),digits=2)
meanage<-agecount[row.names(agecount)=="mean","Age"]
sdage<-agecount[row.names(agecount)=="std.dev","Age"]
agenum<-paste(meanage," (",sdage,")",sep="")

sink("casualgames_learning_demogs.csv")
demogs<-data.frame(cheaters=cheatnum,VGplaynum=VGplaynum,totalnum=totalnum,malenum=malenum,age=agenum)
demogs
sink()


