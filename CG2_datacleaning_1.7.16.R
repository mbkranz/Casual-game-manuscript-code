##################variables################
#directories
wd="/Users/michaelkranz/Documents/Analyses/casualgame_achievementpaper/paper_clean"
#wd="/Volumes/Data2/projects/cogtrainskillacquisition/CURRENTGroupD_learningrate"
library(reshape);library(plyr);
#tidyr is dev version that is currently installed (there was a problem with reshape/tidyr renaming melted variables)
library(tidyr);library(dplyr)
##################import data###############
setwd(wd)
#current data
alldata=read.csv(file="CognitiveData_all.csv",header=TRUE)
groupa<-xlsx::read.xlsx(file="./groupa_crosschecks/groupa_currentcorrected.xlsx",sheetName = "groupa")
groupd<-xlsx::read.xlsx(file="./groupd_checks_errorcorrection/groupd_currentcorrected.xlsx",sheetName = "groupd")
alldata.cogandgame<-join_all(list(alldata,groupa,groupd),by="subs")

#filter cheaters and only group a and d subs
ralldata.ad<-alldata.cogandgame %>%
  filter(Condition %in% c("A","D")) %>%
  filter(rowSums(cbind(cheatsilvd,cheataengie,cheatblock,cheatgude,cheatdigi,cheatsushi,cheattwothree),na.rm=TRUE)==0) %>%
  filter(PreScreenVGplay_sum<=3)

#individual cognitive predictors
cogpred=c("WM","gF","PSpeed")
gF<-c("MatrixReasoningACC","FormBoards_TOT","PaperFolding_TOT","SpatialRelations_TOT","LetterSets_TOT","ShipleyAbstract")
#WM<-c("SPWM_dprimeTOT","Nback_3back_dprimeTOT","VSTM_dprime_all_ACC","RunSpanScore","SymSpanScore")
WM<-c("SPWM_meanTOT_ACC","Nback_3back_generalACC" ,"VSTM_AllACC","RunSpanScore","SymSpanScore")
PS<-c("PatternCompAVG", "LetterCompAVG", "DigitSymbolCoding")
first.a<-c("twothree1","digi1","sushi1")
final.a<-c("twothree10","digi10","sushi10")
#first.d<-c("silvd1","blockdrop1","aengiequest1","gudeballs1") 
first.d<-c("silvd1","blockdrop1","gudeballs1") #without aengie analyses
#final.d<-c("silvd10","blockdrop10","aengiequest10","gudeballs10")
final.d<-c("silvd10","blockdrop10","gudeballs10") #without aengie analyses
#final.d<-c("silvd10","blockdrop10","aengiequest6","gudeballs10")
aqfinal<-grep("aengiequest",final.d,value=TRUE) #to be used when replacing values in next lines  
icogtasks<-c(gF,WM,PS)
final<-c(final.d,final.a)
first<-c(first.d,first.a)
igames<-c(first.d,first.a,final.a,final.d)

####group a#####

#filters data from subjects with missing session 1 or session 10 recorded scores
game_ralldata.a<-ralldata.ad %>%
  filter(Condition=="A") %>%
  select(subs,
         matches("twothree[0123456789]"),
         matches("sushi[0123456789]"),
         matches("digi[0123456789]")) %>%
  gather(gamesess,CGscore, -subs) %>%
  mutate(session=gsub(pattern="sushi|twothree|digi","",gamesess),
         game=gsub(pattern="[0123456789]","",gamesess)) %>%
  filter(!(game %in% c("sushi") & subs==5033)) %>%
  filter(!(game %in% c("twothree") & subs==5051)) %>%
  filter(!(game %in% c("twothree","digi") & subs==5063)) %>%
  filter(!(game %in% c("sushi") & subs==5071)) %>%
  filter(!(game %in% c("digi") & subs==5095)) %>%
  filter(!(game %in% c("twothree","digi","sushi") & subs==5107)) %>%
  select(subs,gamesess,CGscore) %>%
  spread(gamesess,CGscore)
        
######group d##########
gamenotes_cast<-cast(gamenotes_comb,subs~game,value="notes")
######individual game diagnostics (and error sub exclusion)##########
igamenames.d<-names(ralldata.ad)[grep(pattern="^silvd|^blockdrop|^aengiequest|^gudeballs",names(ralldata.ad))] #^=start string with
game_ralldata.d<-ralldata.ad[ralldata.ad$Condition=="D",c("subs",igamenames.d)] #not used (need to melt to filter bad subs)
#univariate individual game and session plots (histogram density plots and qq plots)-->now just used for filtering bad subs (deleted all plots etc)
game_unimelt<-melt(game_ralldata.d,id="subs")
game_unimelt<-plyr::rename(game_unimelt,c("variable"="game","value"="highlevel"))
game_unimelt$session<-gsub(pattern="silvd|blockdrop|aengiequest|gudeballs","",game_unimelt$game)
game_unimelt$game<-gsub(pattern="[0123456789]","",game_unimelt$game)
#silversphere:5428 ,aengie: 5412,5438,5440,5457,gude:5412,5418,5423,5426,5427,5428,5437,5440,5446,block:5412,5444
#see groupd_experimenterror.xlsx, "problem" sheet

game_unimelt<-subset(game_unimelt,
                     !((subs %in% c(5428) & game=="silvd") |
                         (subs %in% c(5412,5438,5440,5457) & game=="aengiequest") |
                         (subs %in% c(5412,5418,5423,5426,5427,5428,5437,5440,5446) & game=="gudeballs") |
                         (subs %in% c(5412,5444) & game=="blockdrop")))

game_ralldata.d<-as.data.frame(cast(game_unimelt,subs~game+session,value="highlevel"))
names(game_ralldata.d)<-lapply(names(game_ralldata.d),function(x){gsub("_","",x)}) #get rid of "_" that cast makes
row.names(game_ralldata.d)<-1:length(game_ralldata.d)
#add final level value for aengie quest session 6 (as this is my final session score for aengie)
#subs who beat aengie on final session (or determined final session of aengie quest): 
game_ralldata.d[,aqfinal]<-ifelse((is.na(game_ralldata.d[,aqfinal]) & !is.na(game_ralldata.d[,"aengiequest1"])),23,game_ralldata.d[,aqfinal])
#subs who beat silversphere: 5402,5431--> add 25 to 5402 and 5431
game_ralldata.d[,"silvd10"]<-ifelse(!is.na(game_ralldata.d[,"silvd1"]) & is.na(game_ralldata.d[,"silvd10"]),25,game_ralldata.d[,"silvd10"])


####NOTE: IN DPLYR: can use one_of() function with select() to select character vector list 
game_ralldata.ad<-join(game_ralldata.d,game_ralldata.a,type="full",match="all",by="subs")
cog_game<-join(ralldata.ad[,c("subs","Condition","Age","Gender",icogtasks)],game_ralldata.ad[,c("subs",igames)],type="left",match="all",by="subs")
cog_game_redox<-cog_game %>% filter(subs!=5412) %>% #only silvd game available (excluded)
  filter(subs!=5428) %>%  #aengie quest and block drop games available (excluded if not using aengie quest as only has one game)
  filter(subs!=5107) %>% #missing either session 1 or 10 from all three games
  filter(subs!=5063) #missing either sess 1 and 10 from twothree or digi
  
cog_game_redox.windsorized.outliers<-outlierfxn(data=cog_game_redox,variablelist=names(cog_game_redox)[-c(1:4)],outlieroutput=TRUE) #outliers and values based on outlierfxn prob values
cog_game_redox.windsorized.data<-outlierfxn(data=cog_game_redox,variablelist=names(cog_game_redox)[-c(1:4)],outlieroutput=FALSE) #data with windsorized cognitive tasks based on outlierfxn prob values

###############standardized composite construct scores#####################
compdata<-cog_game_redox.windsorized.data
#compdata<-cog_game_redox
compdata$gF<-compscoresfxn(gF,compdata)
compdata$WM<-compscoresfxn(WM,compdata)
compdata$PSpeed<-compscoresfxn(PS,compdata)
compdata$final<-compscoresfxn(final,compdata)
compdata$first<-compscoresfxn(first,compdata)
compdata<-subset(compdata,,select=c(subs,Condition,gF:first))



  