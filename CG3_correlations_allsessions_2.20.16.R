library(lme4)
library(boot)
####Table X and Supplemental Tables X,X and X correlation tables###########

#just to make sure using correct subs join with compdata subs
game_ralldata.a<-plyr::join(subset(compdata,select=subs),game_ralldata.a,type="inner",match="all",by="subs")
game_ralldata.a.windsorized.outliers<-outlierfxn(data=game_ralldata.a,variablelist=names(game_ralldata.a)[-c(1)],outlieroutput=TRUE) #outliers and values based on outlierfxn prob values
game_ralldata.a.windsorized.data<-outlierfxn(data=game_ralldata.a,variablelist=names(game_ralldata.a)[-c(1)],outlieroutput=FALSE) #data with windsorized cognitive tasks based on outlierfxn prob values
#just to make sure using correct subs join with compdata subs
game_ralldata.d<-plyr::join(subset(compdata,select=subs),game_ralldata.d,type="inner",match="all",by="subs")
game_ralldata.d.windsorized.outliers<-outlierfxn(data=game_ralldata.d,variablelist=names(game_ralldata.d)[-c(1)],outlieroutput=TRUE) #outliers and values based on outlierfxn prob values
game_ralldata.d.windsorized.data<-outlierfxn(data=game_ralldata.d,variablelist=names(game_ralldata.d)[-c(1)],outlieroutput=FALSE) #data with windsorized cognitive tasks based on outlierfxn prob values

#correlation plot across sessions###
#######all session std scores##########

compscores.game.fxn<-function(gamedata,gamelist){
  for(sess in 1:10){
    game.sess.list=paste(gamelist,sess,sep="")
    gamedata[,paste("compgamescore",sess,sep="_")]<-compscoresfxn(game.sess.list,gamedata)
  }
  return(gamedata[,grep("subs|compgamescore",names(gamedata))])}


compscores.game.a<-compscores.game.fxn(gamedata=game_ralldata.a,gamelist=c("sushi","twothree","digi"))
compscores.game.d<-compscores.game.fxn(gamedata=game_ralldata.d,gamelist=c("silvd","gudeballs","blockdrop"))

compscores.game.a<-compscores.game.fxn(gamedata=game_ralldata.a.windsorized.data,gamelist=c("sushi","twothree","digi"))
compscores.game.d<-compscores.game.fxn(gamedata=game_ralldata.d.windsorized.data,gamelist=c("silvd","gudeballs","blockdrop"))


compscores.game.ad<-rbind(compscores.game.a,compscores.game.d)
compscores.game.all<-join(compdata,compscores.game.ad,type="left",match="all",by="subs")

corstarsmelt.onlystars<-function(var1,var2){
  #creates correlation coefficient with signficance star (to be used in ddply for melted variables)
  require(Hmisc) 
  x<-cbind(var1,var2)
  Corr<-rcorr(x,type=c("spearman"))
  R <- round(Corr$r,2)[2]
  p <- Corr$P[2]
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  boot
  corr_withstars<-paste(R,mystars,sep="")
  #return(corr_withstars)
  return(mystars)
}
cormelt<-function(var1,var2){
  #creates correlation coefficient (commented out corstarsmelt code as this is the correlation coefficient fxn)(to be used in ddply for melted variables)
  require(Hmisc) 
  x<-cbind(var1,var2)
  Corr<-rcorr(x,type=c("pearson"))
  R <- round(Corr$r,2)[2]
  #p <- Corr$P[2]
  ## define notions for significance levels; spacing is important.
  #mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  #corr_withstars<-paste(R,mystars,sep="")
  return(R)
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
  #bootcorr just resamples the dataframe used to obtain the correlations and star significant values (see below for this data frame creation)
  #obtain bootstrap ci and put into data frame
d<-data[i,]
d.melt<-reshape::melt(d,id=c("subs","Condition","gF","WM","PSpeed"))
d.melt<-plyr::rename(d.melt,c(variable="CGsession",value="CGscore"))
d.melt<-reshape::melt(d.melt,id=c("subs","Condition","CGsession","CGscore"))
d.melt<-plyr::rename(d.melt,c(variable="cognitive",value="cognitivescore"))
compscores.game.corr<-d.melt %>% 
  group_by(Condition,CGsession,cognitive) %>%
  summarise(corr=cormelt(CGscore,cognitivescore),
            corrstar=corstarsmelt.onlystars(CGscore,cognitivescore)) %>%
  ungroup() %>%
  filter(!CGsession %in% c("first","final")) %>%
  mutate(CGsession=as.integer(gsub("compgamescore_","",CGsession))) %>%
  mutate(Condition=ifelse(Condition=="A","Non-adaptive","Adaptive")) %>%
  mutate(cognitive=ifelse(cognitive=="gF","Reasoning",
                          ifelse(cognitive=="WM","Working Memory",
                                 ifelse(cognitive=="PSpeed","Perceptual Speed","NA")))) %>%
  mutate(cognitive=factor(x=cognitive,levels=c("Reasoning","Working Memory","Perceptual Speed")))
data_corr<-compscores.game.corr$corr
return(data_corr)}


compscores.game.all.melt<-reshape::melt(compscores.game.all,id=c("subs","Condition","gF","WM","PSpeed"))
compscores.game.all.melt<-plyr::rename(compscores.game.all.melt,c(variable="CGsession",value="CGscore"))
compscores.game.all.melt<-reshape::melt(compscores.game.all.melt,id=c("subs","Condition","CGsession","CGscore"))
compscores.game.all.melt<-plyr::rename(compscores.game.all.melt,c(variable="cognitive",value="cognitivescore"))
compscores.game.corr<-compscores.game.all.melt %>% 
  group_by(Condition,CGsession,cognitive) %>%
  summarise(corr=cormelt(CGscore,cognitivescore),
            corrstar=corstarsmelt.onlystars(CGscore,cognitivescore)) %>%
  ungroup() %>%
  filter(!CGsession %in% c("first","final")) %>%
  mutate(CGsession=as.integer(gsub("compgamescore_","",CGsession))) %>%
  mutate(Condition=ifelse(Condition=="A","Non-adaptive","Adaptive")) %>%
  mutate(cognitive=ifelse(cognitive=="gF","Reasoning",
                          ifelse(cognitive=="WM","Working Memory",
                                 ifelse(cognitive=="PSpeed","Perceptual Speed","NA")))) %>%
  mutate(cognitive=factor(x=cognitive,levels=c("Reasoning","Working Memory","Perceptual Speed")))
#obtain bootstrap ci and put into data frame
gamenames<-grep("compgamescore",names(compscores.game.all),value=TRUE)
corr_boottest<-boot(data=compscores.game.all,statistic=bootcorr,R=2000,idnames=gamenames)
corr_mat<-bootcifxn(compscores.game.corr,corr_boottest)
corr_mat$StarSpace<-paste("\n\n",corr_mat$BCa,sep="")

ggplot(corr_mat,aes(CGsession,corr,group=Condition))+
  geom_line(aes(color=Condition),size=1.5)+
  facet_grid(cognitive~.)+
  geom_text(aes(label=ifelse(!((Condition=="Non-adaptive" & cognitive=="Reasoning" & CGsession %in% c(9))|
                               Condition=="Non-adaptive" & cognitive=="Working Memory" & CGsession %in% c(1,2,9)),
                             paste(corrstar,BCa,sep=" "),"")),size=5,hjust=.1)+
  geom_text(aes(label=ifelse((Condition=="Non-adaptive" & cognitive=="Reasoning" & CGsession %in% c(1,9))|
                               Condition=="Non-adaptive" & cognitive=="Working Memory" & CGsession %in% c(1,2,9),
                paste(corrstar,BCa,sep=" "),"")),size=5,vjust=2,hjust=.1)+
  labs(x="Session",y="Spearman Correlation Coefficient",group="Game Group")+
  scale_x_continuous(breaks=c(1:10))+
  theme_bw()+
  scale_colour_manual(values=c("skyblue","orange"),name="Game Group")+
  theme(text=element_text(family="Helvetica"),panel.grid=element_blank(),strip.text= element_text(size=14),
        strip.background = element_rect(colour="black",fill="white"),axis.text=element_text(size=14),
        legend.text=element_text(size=14,face="plain"),legend.position=c(.5,.75),axis.title=element_text(size=14),
        legend.title=element_text(size=14,face="plain"))


ggplot(corr_mat,aes(CGsession,corr,group=Condition))+
  geom_line(aes(color=Condition),size=1.5)+
  facet_grid(cognitive~.)+geom_text(aes(label=corrstar),size=5)+
  labs(x="Session",y="Spearman Correlation Coefficient",group="Game Group")+
  scale_x_continuous(breaks=c(1:10))+
  theme_bw()+
  scale_colour_manual(values=c("black","red"),name="Game Group",guide=FALSE)+
  theme(text=element_text(family="Helvetica"),panel.grid=element_blank(),strip.text= element_text(size=14),
        strip.background = element_rect(colour="black",fill="white"),axis.text=element_text(size=14),
        legend.text=element_text(size=14,face="plain"),legend.position=c(.5,.75),axis.title=element_text(size=14),
        legend.title=element_text(size=14,face="plain"))

BCAs<-cast(subset(corr_mat,select=c(Condition,CGsession,cognitive,BCa)),cognitive+Condition~CGsession,value="BCa")


setwd("~/Documents/Analyses/casualgame_achievementpaper/paper_clean/SupplementalFigure1_correlations_allsessions");ggsave(filename="SupplementalFigure2.png",width=8.5,height=11)



####linear mixed effects model with all games (for comparison with just first and final sessions as ran in the CG4_regression script)###
compscores.game.all.lme<-compscores.game.all %>% 
  gather(key=CGsession,value=CGscore,-c(subs,Condition,gF,WM,PSpeed,first,final)) %>%
  mutate(Condition=as.factor(ifelse(Condition==1,"A","D")),
         session=as.integer(gsub("compgamescore_","",CGsession)))

group.gF<-lmerTest::lmer(CGscore~gF*Condition*I(session-1)+(1|subs),data=compscores.game.all.lme)
group.WM<-lmerTest::lmer(CGscore~WM*Condition*I(session-1)+(1|subs),data=compscores.game.all.lme)
group.PSpeed<-lmerTest::lmer(CGscore~PSpeed*Condition*session+(1|subs),data=compscores.game.all.lme)

group.gF<-lme4::lmer(CGscore~gF*Condition*I(session-1)+(1|subs),data=compscores.game.all.lme)
group.WM<-lme4::lmer(CGscore~WM*Condition*I(session-1)+(1|subs),data=compscores.game.all.lme)
group.PSpeed<-lme4::lmer(CGscore~PSpeed*Condition*I(session-1)+(1|subs),data=compscores.game.all.lme)

html.gF<-html2dataframefxn(stargazer::stargazer(type="html",group.gF))
html.WM<-html2dataframefxn(stargazer::stargazer(type="html",group.WM))
html.PSpeed<-html2dataframefxn(stargazer::stargazer(type="html",group.PSpeed))
html.all<-as.data.frame(c(html.gF,html.WM,html.PSpeed))

group.gF.d<-lmerTest::lmer(CGscore~gF*I(session-1)+(1|subs),data=compscores.game.all.lme,subset=Condition=="D")
group.gF.a<-lmerTest::lmer(CGscore~gF*I(session-1)+(1|subs),data=compscores.game.all.lme,subset=Condition=="A")
group.WM.a<-lmerTest::lmer(CGscore~WM*I(session-1)+(1|subs),data=compscores.game.all.lme,subset=Condition=="A")
group.WM.d<-lmerTest::lmer(CGscore~WM*I(session-1)+(1|subs),data=compscores.game.all.lme,subset=Condition=="D")
group.PSpeed.a<-lmerTest::lmer(CGscore~PSpeed*session+(1|subs),data=compscores.game.all.lme,subset=Condition=="A")
group.PSpeed.d<-lmerTest::lmer(CGscore~PSpeed*session+(1|subs),data=compscores.game.all.lme,subset=Condition=="D")

#multiple cognitive measures
group.WMgF.d<-lmerTest::lmer(CGscore~WM*I(session-1)+gF*I(session-1)+(1|subs),data=compscores.game.all.lme,subset=Condition=="D")
group.WMgF.a<-lmerTest::lmer(CGscore~WM*I(session-1)+gF*I(session-1)+(1|subs),data=compscores.game.all.lme,subset=Condition=="A")





