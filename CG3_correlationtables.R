####Table X and Supplemental Tables X,X and X correlation tables###########
compdata.a<-subset(compdata,Condition=="A")
compdata.d<-subset(compdata,Condition=="D")

#correlation tables of all variables
#compdata=composite variables used in analyses (cognitive and game)
#individual cognitive and game variables making up composites used in analyses=cog_game_redox
#individual game data all sessions (casted)=game_ralldata 

#correlation tables with significance stars for composite data
corrtable.a<-corstarsl(subset(compdata.a,,gF:first))
corrtable.d<-corstarsl(subset(compdata.d,,gF:first))

compdata.melt<-melt(compdata,measure=c("final","first"),variable_name="scoretype")
cor_mat.first.a<-cor(subset(compdata.melt,Condition=="A" & scoretype=="first",c(gF:PSpeed)),use="pairwise.complete.obs")
cor_mat.final.a<-cor(subset(compdata.melt,Condition=="A" & scoretype=="final",c(gF:PSpeed)),use="pairwise.complete.obs")
cor_mat.first.d<-cor(subset(compdata.melt,Condition=="D" & scoretype=="first",c(gF:PSpeed)),use="pairwise.complete.obs")
cor_mat.final.d<-cor(subset(compdata.melt,Condition=="D" & scoretype=="final",c(gF:PSpeed)),use="pairwise.complete.obs")

#test of correlations between group d and group a
#Reasoning~first sess
psych::r.test(.58,.62,n=44,n2=46) ###Z=.29, p=.77
#WM~first sess
psych::r.test(.42,.49,n=44,n2=46) ###Z=.4, p=.69

#Reasoning~final sess
psych::r.test(.72,.43,n=44,n2=46) ###Z=2.05, p=.04
#WM~final sess
psych::r.test(.62,.37,n=44,n2=46) ###Z=1.54, p=.12


#adaptive paired correlation t tests
#Reasoning 
psych::r.test(.58,.72,.79,n=45) 
#WM
psych::r.test(.42,.62,.79,n=45)
#PSpeed
psych::r.test(-.05,.09,.79,n=45)

#non-adaptive paired correlation t tests
#Reasoning 
psych::r.test(.62,.43,.66,n=46)
#WM
psych::r.test(.49,.37,.66,n=46)
#PSpeed
psych::r.test(.39,.37,.66,n=46)

###correlation table with significance stars for supplemental indvidiual game and task table###

#group a
finaltablecorr1.a<-join(cog_game_redox,
                      compdata.a,by="subs",type="full")
finaltablecorr.a<-finaltablecorr1.a[,c(icogtasks,"gF","WM","PSpeed",final.a,first.a,"first","final")]
corr_melt.a<-melt(finaltablecorr.a,id=c(final.a,first.a,"first","final")); corr_melt.a<-plyr::rename(corr_melt.a,c(variable="gamemeas",value="gamescore")) 
corr_melt2.a<-melt(corr_melt.a,id=c("gamemeas","gamescore"));  corr_melt2.a<-plyr::rename(corr_melt2.a,c(variable="cogmeas",value="cogscore"))
corr_mat.a<-ddply(corr_melt2.a,.(gamemeas,cogmeas),summarise,corr=corstarsmelt(gamescore,cogscore))
corr_mat.a$group<-"Non-Adaptive"
#group d
finaltablecorr1.d<-join(cog_game_redox,
                        compdata.d,by="subs",type="full")
finaltablecorr.d<-finaltablecorr1.d[,c(icogtasks,"gF","WM","PSpeed",final.d,first.d,"first","final")]
corr_melt.d<-melt(finaltablecorr.d,id=c(final.d,first.d,"first","final")); corr_melt.d<-plyr::rename(corr_melt.d,c(variable="gamemeas",value="gamescore")) 
corr_melt2.d<-melt(corr_melt.d,id=c("gamemeas","gamescore"));  corr_melt2.d<-plyr::rename(corr_melt2.d,c(variable="cogmeas",value="cogscore"))
corr_mat.d<-ddply(corr_melt2.d,.(gamemeas,cogmeas),summarise,corr=corstarsmelt(gamescore,cogscore))
corr_mat.d$group<-"Adaptive"

##correlation table for paper (cog and game are switched around)
corr.all<-rbind(corr_mat.a,corr_mat.d)
corr.all<-arrange(corr.all,group,cogmeas)
corr.all$scoretype<-ifelse(grepl("10|6",corr.all$cogmeas),"final","first")
corr.allcast<-cast(corr.all,gamemeas~group+cogmeas,value=c("corr"))


