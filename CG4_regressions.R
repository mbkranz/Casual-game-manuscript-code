library(lmerTest);library(texreg)
library(boot); library(car);library(QuantPsyc);
library(ggplot2)

###interaction of group and session
compdata.melt<-melt(compdata,measure=c("first","final"),variable_name="scoretype")
group.gF<-lme4::lmer(value~gF*Condition*scoretype+(1|subs),data=compdata.melt)
group.WM<-lme4::lmer(value~WM*Condition*scoretype+(1|subs),data=compdata.melt)
group.PSpeed<-lme4::lmer(value~PSpeed*Condition*scoretype+(1|subs),data=compdata.melt)

summary(lme1 <- nlme::lme(value ~ gF*Condition*scoretype, data = compdata.melt, random = ~1|subs))
summary(lme2 <- nlme::lme(value ~ gF*Condition*scoretype, data = compdata.melt, random = ~1|subs/Condition))
summary(lme2 <- nlme::lme(value ~ gF*Condition*scoretype, data = compdata.melt, random = ~scoretype|subs/Condition))

html.gF<-html2dataframefxn(stargazer::stargazer(type="html",group.gF))
html.WM<-html2dataframefxn(stargazer::stargazer(type="html",group.WM))
html.PSpeed<-html2dataframefxn(stargazer::stargazer(type="html",group.PSpeed))
html.all<-as.data.frame(c(html.gF,html.WM,html.PSpeed))


####linear regression (all subsets) for cognitive predictors (final session level~first sess + predictors) and (first session level~predictors)#### 
#predict first (gF and then WM)
compdata.a<-subset(compdata,Condition=="A")
compdata.d<-subset(compdata,Condition=="D")

reg.eachgroup.fxn<-function(compdatafxn,avpplots=FALSE,lmobj=FALSE){
first_gF<-lm(first~gF,data=compdatafxn)
first_gFWM<-lm(first~gF+WM,data=compdatafxn)
#predict first (WM and then gF)
first_WM<-lm(first~WM,data=compdatafxn)
first_WMgF<-update(first_WM,.~.+gF)
#predict final (gF and then WM)
final_gF<-lm(final~gF,data=compdatafxn)
final_gFWM<-update(final_gF,.~.+WM)
#predict final (WM and then gF)
final_WM<-lm(final~WM,data=compdatafxn)
final_WMgF<-update(final_WM,.~.+gF)
#predict final (first and then gF and then WM)
final1_3<-lm(final~first,data=compdatafxn)
final2_3<-update(final1_3,.~.+gF)
final3_3<-update(final2_3,.~.+WM)
#predict final (first and then WM and then gF)
final1_4<-lm(final~first,data=compdatafxn)
final2_4<-update(final1_4,.~.+WM)
final3_4<-update(final2_4,.~.+gF)
if(avpplots==FALSE){
  if (lmobj==TRUE){
  #OUTPUT IS JUST LIST OF LM OBJECTS
    allregtables<-list(first_gF=first_gF,first_gFWM=first_gFWM,
    first_WM=first_WM,first_WMgF=first_WMgF,
    final_gF=final_gF,final_gFWM=final_gFWM,
    final_WM=final_WM,final_WMgF=final_WMgF,
    final1_3=final1_3,final2_3=final2_3,final3_3=final3_3,
    final1_4=final1_4,final2_4=final2_4,final3_4=final3_4)
  }else{
  #OUTPUT IS HIERARCHICAL REGRESSIONS IN FORMATTED TABLE 
  first1<-hregfxn(list(first_gF,first_gFWM),1)
  first2<-hregfxn(list(first_WM,first_WMgF),1)
  final1<-hregfxn(list(final_gF,final_gFWM),1)
  final2<-hregfxn(list(final_WM,final_WMgF),1)
  final3<-hregfxn(list(final1_3,final2_3,final3_3),1)
  final4<-hregfxn(list(final1_4,final2_4,final3_4),1)
 allregtables<-rbind(first1,first2,final1,final2,final3,final4)}
return(allregtables)
}else{
  #OUTPUT ARE ADDED VARIABLE PLOTS AND ASSOCIATED RESIDUALS MAKING UP THESE PLOTS
  firstavp<-avpfxn2(first_gFWM,"first","first")
  finalavp1<-avpfxn2(final_gFWM,"final","final")
  finalavp2<-avpfxn2(final3_3,"final","final2")
  all_avp<-rbind(firstavp,finalavp1,finalavp2)
  all_avp$group<-unique(compdatafxn$Condition)
  return(all_avp)}
}

allregtables.a<-reg.eachgroup.fxn(compdata.a)
allregtables.d<-reg.eachgroup.fxn(compdata.d)

#group a speed
gFfirst<-lm(first~gF,data=compdata.a)
gFfinal<-lm(final~gF,data=compdata.a)
gFPSfirst<-lm(first~gF+PSpeed,data=compdata.a)
gFPSfinal<-lm(final~gF+PSpeed,data=compdata.a)
gFPSreg_first<-hregfxn(list(gFfirst,gFPSfirst),1)
gFPSreg_final<-hregfxn(list(gFfinal,gFPSfinal),1)

firstfinal<-lm(final~first,data=compdata.a)
firstPSfinal<-lm(final~first+PSpeed,data=compdata.a)
firstPSreg_final<-hregfxn(list(firstfinal,firstPSfinal),1)

#group d speed
gFfirst.d<-lm(first~gF,data=compdata.d)
gFfinal.d<-lm(final~gF,data=compdata.d)
gFPSfirst.d<-lm(first~gF+PSpeed,data=compdata.d)
gFPSfinal.d<-lm(final~gF+PSpeed,data=compdata.d)
gFPSreg_first.d<-hregfxn(list(gFfirst.d,gFPSfirst.d),1)
gFPSreg_final.d<-hregfxn(list(gFfinal.d,gFPSfinal.d),1)

firstPSfinal.d<-lm(final~first+PSpeed,data=compdata.d)
firstPSfinal.d<-lm(final~first+PSpeed,data=compdata.d)
firstPSreg_final.d<-hregfxn(list(firstfinal.d,firstPSfinal.d),1)

####Figure X: added variable plots (to show effect of each variable adjusting for other variables in models)######
groupaavp<-reg.eachgroup.fxn(compdata.a,avpplots=TRUE)
groupdavp<-reg.eachgroup.fxn(compdata.d,avpplots=TRUE)

avp<-rbind(groupaavp,groupdavp)
avp$plottype<-ifelse(avp$plottype=="first","First Session",
                     ifelse(avp$plottype=="final","Final Session",
                            ifelse(avp$plottype=="final2","Final Session ","None")))
avp$plotname<-plyr::revalue(avp$plotname,c(gF="REAS"))

groupdavpplot<-ggplot(subset(avp,group=="D"),aes(predictor,highlevel))+geom_point()+
  geom_smooth(method="lm",color="black")+
  facet_grid(plottype~plotname)+ggtitle("A. Adaptive Group")+
  labs(x="Predictor Score | others",y="Game Score | others")+
  theme_bw()+scale_y_continuous(limits=c(-2, 2))+scale_x_continuous(limits=c(-2, 2))+
  theme(panel.grid=element_blank(),strip.text= element_text(size=12, face="plain"),
        strip.background = element_rect(colour="black",fill="white"),axis.text=element_text(size=12))
groupaavpplot<-ggplot(subset(avp,group=="A"),aes(predictor,highlevel))+geom_point()+
  geom_smooth(method="lm",color="black")+
  facet_grid(plottype~plotname)+ggtitle("B. Non-Adaptive Group")+
  labs(x="Predictor Score | others",y="Game Score | others")+
  theme_bw()+scale_y_continuous(limits=c(-2, 2))+scale_x_continuous(limits=c(-2, 2))+
  theme(panel.grid=element_blank(),strip.text= element_text(size=12, face="plain"),
        strip.background = element_rect(colour="black",fill="white"),axis.text=element_text(size=12))
multiplot(groupdavpplot,groupaavpplot,cols=2)

#multiple regression plots
allregtables.a<-reg.eachgroup.fxn(compdata.a,lmobj=TRUE)
allregtables.d<-reg.eachgroup.fxn(compdata.d,lmobj=TRUE)

#see http://docs.ggplot2.org/dev/vignettes/themes.html for theme documentation
#final regression plots
multreg.plots.d.final<-data.frame(actual=compdata.d$final,predicted=predict(allregtables.d$final_gFWM),
                                  Group="Adaptive Games",
                          sess="Final Session CG Score")
multreg.plots.a.final<-data.frame(actual=compdata.a$final,predicted=predict(allregtables.a$final_gFWM),
                                  Group="Non-Adaptive Games",
                                  sess="Final Session CG Score")
multreg.plots.both.final<-rbind(multreg.plots.a.final,multreg.plots.d.final)
multreg.plots.both.final.ggplot<-ggplot(multreg.plots.both.final,aes(predicted,actual,color=Group))+
  geom_point()+
  geom_smooth(aes(color=Group,fill=Group),method="lm",alpha=.2)+
  scale_colour_manual(values = c("red","black"),guide=FALSE)+ scale_fill_manual(values = c("red","black"),guide=FALSE)+
  labs(x="Predicted with Reasoning and WM",y="Final Session CG Score")+
  scale_y_continuous(breaks=c(-1.5,-1,-.5,0,.5,1,1.5),limits=c(-1.5, 1.5))+scale_x_continuous(breaks=c(-1.5,-1,-.5,0,.5,1,1.5),limits=c(-1.5, 1.5))+
  theme(text=element_text(family="Helvetica"),
        strip.text= element_text(size=14),strip.background = element_rect(colour="black",fill="white"),
        axis.text=element_text(size=12,color="black"),axis.line = element_line(colour = "black"),
        legend.text=element_text(size=10,face="plain"),legend.position=c(.1,.9), legend.title=element_text(size=10,face="plain"),
        panel.background=element_rect(colour="white",fill="white"),panel.grid=element_blank())
setwd("~/Documents/Analyses/casualgame_achievementpaper/paper_clean/Figure3_MultipleRegPlots");ggsave(filename="MultRegPlots_final.png",width=5,height=5)
#first session regression plots
multreg.plots.d.first<-data.frame(actual=compdata.d$first,predicted=predict(allregtables.d$first_gFWM),
                                  Group="Adaptive Games",
                                  sess="First Session CG Score")
multreg.plots.a.first<-data.frame(actual=compdata.a$first,predicted=predict(allregtables.a$first_gFWM),
                                  Group="Non-Adaptive Games",
                                  sess="First Session CG Score")
multreg.plots.both.first<-rbind(multreg.plots.a.first,multreg.plots.d.first)
multreg.plots.both.first.ggplot<-ggplot(multreg.plots.both.first,aes(predicted,actual,color=Group))+geom_point()+
  geom_smooth(aes(color=Group,fill=Group),method="lm",alpha=.2)+
  scale_colour_manual(values = c("red","black"),guide=FALSE)+
  scale_fill_manual(values = c("red","black"),guide=FALSE)+
  labs(x="Predicted with Reasoning and WM",y="First Session CG Score")+
  scale_y_continuous(breaks=c(-1.5,-1,-.5,0,.5,1,1.5),limits=c(-1.5, 1.5))+scale_x_continuous(breaks=c(-1.5,-1,-.5,0,.5,1,1.5),limits=c(-1.5, 1.5))+
  theme(text=element_text(family="Helvetica"),
        strip.text= element_text(size=14),strip.background = element_rect(colour="black",fill="white"),
        axis.text=element_text(size=12,color="black"),axis.line = element_line(colour = "black"),
        legend.text=element_text(size=10,face="plain"),legend.position=c(.1,.9), legend.title=element_text(size=10,face="plain"),
        panel.background=element_rect(colour="white",fill="white"),panel.grid=element_blank())
setwd("~/Documents/Analyses/casualgame_achievementpaper/paper_clean/Figure3_MultipleRegPlots");ggsave(filename="MultRegPlots_first.png",width=5,height=5)

#multiplot(multreg.plots.both.first.ggplot,multreg.plots.both.final.ggplot,cols=2)   
#setwd("~/Documents/Analyses/casualgame_achievementpaper/paper_clean/Figure3_MultipleRegPlots"); pdf(file = "Combined_plots.pdf", width = 7, height = 9)
#####Final Model Diagnostics####
# Assessing Outliers of Final Models
# Influential observations: Cook's D plot
# identify D values > 4/(n-k-1) 
#see added variable plots above

#first model

#groupd
first_outlier.d<-lm(first~gF+WM,data=compdata.d)
outlierTest(first_outlier.d) # 5450
cutoff <- 4/((nrow(compdata.d)-length(first_outlier.d$coefficients)-2)) 
plot(first_outlier.d, which=1, cook.levels=cutoff)
plot(first_outlier.d, which=2, cook.levels=cutoff)
plot(first_outlier.d, which=3, cook.levels=cutoff)
plot(first_outlier.d, which=4, cook.levels=cutoff)
summary(lm(first~gF+WM,data=subset(compdata.d,subset=!(subs %in% c(5410))))) 
allregtables.d.outlier.first<-reg.eachgroup.fxn(subset(compdata.d,subset=!(subs %in% c(5410))))
#group a
first_outlier.a<-lm(first~gF+WM,data=compdata.a)
outlierTest(first_outlier.a) # 5450
cutoff <- 4/((nrow(compdata.a)-length(first_outlier.a$coefficients)-2)) 
plot(first_outlier.a, which=1, cook.levels=cutoff)
plot(first_outlier.a, which=2, cook.levels=cutoff)
plot(first_outlier.a, which=3, cook.levels=cutoff)
plot(first_outlier.a, which=4, cook.levels=cutoff) 

#final model
#groupd
final_outlier.d<-lm(final~gF+WM,data=compdata.d) 
outlierTest(final_outlier.d) # 5410
cutoff <- 4/((nrow(compdata.d)-length(final_outlier.d$coefficients)-2))
plot(final_outlier.d, which=1, cook.levels=cutoff)
plot(final_outlier.d, which=2, cook.levels=cutoff)
plot(final_outlier.d, which=3, cook.levels=cutoff)
plot(final_outlier.d, which=4, cook.levels=cutoff)

summary(lm(final~gF+WM,data=subset(compdata.d,subset=!(subs %in% c(5410)))))
allregtables.d.outlier.final<-reg.eachgroup.fxn(subset(compdata.d,subset=!(subs %in% c(5410))))


#groupa
final_outlier.a<-lm(final~gF+WM,data=compdata.a) 
outlierTest(final_outlier.a) # 5410
cutoff <- 4/((nrow(compdata.a)-length(final_outlier.a$coefficients)-2))
plot(final_outlier.a, which=1, cook.levels=cutoff)
plot(final_outlier.a, which=2, cook.levels=cutoff)
plot(final_outlier.a, which=3, cook.levels=cutoff)
plot(final_outlier.a, which=4, cook.levels=cutoff)

#final2 (with first predictor)
#groupd
final2_outlier.d<-lm(final~first+gF+WM,data=compdata.d)
outlierTest(final2_outlier.d) # none with Bonferroni correction but 5450 is p=.11
cutoff <- 4/((nrow(compdata.d)-length(final2_outlier.d$coefficients)-2))
plot(final2_outlier.d, which=1, cook.levels=cutoff)
plot(final2_outlier.d, which=2, cook.levels=cutoff)
plot(final2_outlier.d, which=3, cook.levels=cutoff)
plot(final2_outlier.d, which=4, cook.levels=cutoff)
summary(lm(final~first+gF+WM,data=compdata.d))
allregtables.d.outlier.final2<-reg.eachgroup.fxn(subset(compdata.d,subset=!(subs %in% c(5450))))


#groupa
final2_outlier.a<-lm(final~first+gF+WM,data=compdata.a)
outlierTest(final2_outlier.a) 
cutoff <- 4/((nrow(compdata.a)-length(final2_outlier.a$coefficients)-2))
plot(final2_outlier.a, which=1, cook.levels=cutoff)
plot(final2_outlier.a, which=2, cook.levels=cutoff)
plot(final2_outlier.a, which=3, cook.levels=cutoff)
plot(final2_outlier.a, which=4, cook.levels=cutoff)

#groupa speed

outlierTest(gFPSfinal) 
cutoff <- 4/((nrow(compdata.a)-length(gFPSfinal$coefficients)-2))
plot(gFPSfinal, which=1, cook.levels=cutoff)
plot(gFPSfinal, which=2, cook.levels=cutoff)
plot(gFPSfinal, which=3, cook.levels=cutoff)
plot(gFPSfinal, which=4, cook.levels=cutoff)

outlierTest(PSfirstfinalWM) 
cutoff <- 4/((nrow(compdata.a)-length(PSfirstfinalWM$coefficients)-2))
plot(PSfirstfinalWM, which=1, cook.levels=cutoff)
plot(PSfirstfinalWM, which=2, cook.levels=cutoff)
plot(PSfirstfinalWM, which=3, cook.levels=cutoff)
plot(PSfirstfinalWM, which=4, cook.levels=cutoff)




