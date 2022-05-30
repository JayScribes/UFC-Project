## UFC Project
## Who is the best striker?
## Best striker will be defined by highest "Striking Metric"
## Boxing Metric = (Striking Offense + Striking Defense)
## Striking Offense = Striking Volume + Striking Precision + Striking Power - these will be differently weighted
## Weight of different metrics will be based off of winning variance explained by each metric after linear regression
## Fighters will also be weighted on the difficulty of their division based off of the number of unique champion-caliber fighters in the division






############################################################################### Background #################################################################################
################################################################################# Work #################################################################################





## Filtering only fights from 2011-2021

ufc_fights$outlier = ufc_fights$Date < "2011-01-01"
ufc_fights = filter(ufc_fights, outlier != TRUE)
ufc_fights <- subset(ufc_fights, , -c(outlier))

## Filtering only fights from male fighters
ufc_fights$outlier = ufc_fights$`Women's-Bout` > 0
ufc_fights = filter(ufc_fights, outlier != TRUE)
ufc_fights <- subset(ufc_fights, , -c(outlier))

## creating striker dataset
ufc_striking <- cbind.data.frame(ufc_fights$'Significant-Strike-Attempt', ufc_fights$'Significant-Strike-Land', ufc_fights$'Full Name', ufc_fights$'Fighter-2-Name',
                                 ufc_fights$'Knockdowns', ufc_fights$'KO/TKO', ufc_fights$W, ufc_fights$L, ufc_fights$D, ufc_fights$'Total-Fight-Time-Sec', ufc_fights$'Total-Strikes-Attempt',
                                 ufc_fights$'Total-Strikes-Land', ufc_fights$StrDef,ufc_fights$Belt,ufc_fights$Flyweight,ufc_fights$Bantamweight,ufc_fights$Featherweight,
                                 ufc_fights$Lightweight, ufc_fights$Welterweight,ufc_fights$Middleweight, ufc_fights$`Light-Heavyweight`,ufc_fights$Heavyweight)


## changing headders
ufc_striking<- edit(ufc_striking) 

## adding ground strikes to dataset to take them out of the equation

ufc_striking$ground.strikes.attempted <- (ufc_fights$`Ground-Attempt`) ## to be removed from tsa
ufc_striking$ground.strikes.landed <- (ufc_fights$`Ground-Land`) ## to be removed from tsl

# Building a new data frame with aggregates 


ufc.striking.offensive.aggregate <- ufc_striking%>% 
  group_by(fighter.name) %>% 
  summarise(avg.ssa = mean(significant.strike.attempt),
            tot.ssa = sum(significant.strike.attempt),
            tot.ssl = sum(significant.strike.land),
            tot.kd = sum(kd.in.fight),
            tot.ko = sum(ko.in.fight),
            win = sum(win),
            loss = sum(loss),
            draw = sum(draw),
            str.def = mean(str.def),
            belt.fights =sum(Belt),
            b.weight = sum(bantamweight),
            fl.weight = sum(flyweight),
            l.weight = sum(lightweight),
            ft.weight = sum(featherweight),
            w.weight = sum(welterweight),
            m.weight = sum(middleweight),
            lh.weight = sum(lheavyweight),
            h.weight = sum(heavyweight),
            tot.gsa = sum(ground.strikes.attempted),
            tot.gsl = sum(ground.strikes.landed),
            tot.fight.time = sum(fight.time.sec),
            tot.tsa = sum(total.strike.attempted),
            tot.tsl = sum(total.strikes.landed)) %>% 
  as.data.frame()

## Calculated Fields


ufc.striking.offensive.aggregate$total.fights <- (ufc.striking.offensive.aggregate$win + ufc.striking.offensive.aggregate$loss + ufc.striking.offensive.aggregate$draw)
ufc.striking.offensive.aggregate$total.fight.time.min <- (ufc.striking.offensive.aggregate$tot.fight.time /60)
ufc.striking.offensive.aggregate$SLPM <- ((ufc.striking.offensive.aggregate$tot.tsl -ufc.striking.offensive.aggregate$tot.gsl) / ufc.striking.offensive.aggregate$total.fight.time.min)
ufc.striking.offensive.aggregate$SAPM <- ((ufc.striking.offensive.aggregate$tot.tsa -ufc.striking.offensive.aggregate$tot.gsa) / ufc.striking.offensive.aggregate$total.fight.time.min)
ufc.striking.offensive.aggregate$SSLPM <- ((ufc.striking.offensive.aggregate$tot.ssl -ufc.striking.offensive.aggregate$tot.gsl) / ufc.striking.offensive.aggregate$total.fight.time.min)
ufc.striking.offensive.aggregate$SSAPM <- ((ufc.striking.offensive.aggregate$tot.ssa -ufc.striking.offensive.aggregate$tot.gsa)/ ufc.striking.offensive.aggregate$total.fight.time.min)
ufc.striking.offensive.aggregate$NSSLPM <- (ufc.striking.offensive.aggregate$SLPM - ufc.striking.offensive.aggregate$SSLPM)
ufc.striking.offensive.aggregate$NSSAPM <- (ufc.striking.offensive.aggregate$SAPM - ufc.striking.offensive.aggregate$SSAPM)
ufc.striking.offensive.aggregate$KOPM <- (ufc.striking.offensive.aggregate$tot.ko / ufc.striking.offensive.aggregate$total.fight.time.min)
ufc.striking.offensive.aggregate$KDPM <- (ufc.striking.offensive.aggregate$tot.kd / ufc.striking.offensive.aggregate$total.fight.time.min)

## Individual Striking Metrics

ufc.striking.offensive.aggregate$SVM <- ((ufc.striking.offensive.aggregate$SLPM)+(ufc.striking.offensive.aggregate$SAPM))
ufc.striking.offensive.aggregate$SPrM <- (((((ufc.striking.offensive.aggregate$SSLPM / ufc.striking.offensive.aggregate$SSAPM)*1.5) + (ufc.striking.offensive.aggregate$NSSLPM / ufc.striking.offensive.aggregate$NSSAPM*0.5)))/2)
ufc.striking.offensive.aggregate$SPwM <- ((ufc.striking.offensive.aggregate$tot.ko/ufc.striking.offensive.aggregate$tot.tsl)) + ((ufc.striking.offensive.aggregate$tot.kd/ufc.striking.offensive.aggregate$tot.tsl))
ufc.striking.offensive.aggregate$OSM <- (ufc.striking.offensive.aggregate$SVM + ufc.striking.offensive.aggregate$SPrM + ufc.striking.offensive.aggregate$SPwM)

## Are my metrics valid?

striking.volume.corr1 <- cor.test(ufc.striking.offensive.aggregate$tot.tsa, ufc.striking.offensive.aggregate$SVM)
striking.volume.corr2 <- cor.test(ufc.striking.offensive.aggregate$tot.tsl, ufc.striking.offensive.aggregate$SVM)
striking.volume.corr1
striking.volume.corr2 ## metric significantly correlated with both total stikes attempted and total stikes landed

ufc.striking.offensive.aggregate$pr <-  (ufc.striking.offensive.aggregate$tot.tsl / ufc.striking.offensive.aggregate$tot.tsa)
striking.precision.corr <- cor.test(ufc.striking.offensive.aggregate$SPrM, ufc.striking.offensive.aggregate$pr)
striking.precision.corr ## metric significantly correlated with 'general' striking precision 

striking.power.corr1 <- cor.test(ufc.striking.offensive.aggregate$tot.ko, ufc.striking.offensive.aggregate$SPwM)
striking.power.corr2 <- cor.test(ufc.striking.offensive.aggregate$tot.kd, ufc.striking.offensive.aggregate$SPwM)
striking.power.corr1
striking.power.corr2 ## metric significantly correlated with both total KD and total KO of fighters.

############################################################################### Background #################################################################################
################################################################################# Work #################################################################################



############################################################################### DIVISIONAL #################################################################################
############################################################################### Difficulty #################################################################################

division.difficulty <- ufc.striking.offensive.aggregate%>% 
  group_by(belt.fights) %>% 
  summarise(
    flyweight.t = sum(fl.weight),
    bantamweight.t = sum(b.weight),
    feather.t = sum(ft.weight),
    light.t = sum(l.weight),
    welter.t = sum(w.weight),
    middle.t = sum(m.weight),
    lheavy.t = sum(lh.weight),
    heavy.t = sum(h.weight))%>% 
  as.data.frame()

## measure of activity of a division
sum(division.difficulty$flyweight.t) ##462
sum(division.difficulty$bantamweight.t)## 958
sum(division.difficulty$feather.t) ## 1112
sum(division.difficulty$light.t)## 1580
sum(division.difficulty$welter.t)## 1524
sum(division.difficulty$middle.t)## 1068
sum(division.difficulty$lheavy.t)## 730
sum(division.difficulty$heavy.t)## 698

## Measure of championship fights in a division

div.diff.title <- edit(division.difficulty) ## removing non-title fights

sum(div.diff.title$flyweight.t) ## 149
sum(div.diff.title$bantamweight.t) ## 201
sum(div.diff.title$feather.t) ## 226
sum(div.diff.title$light.t) ## 281
sum(div.diff.title$welter.t) ## 295
sum(div.diff.title$middle.t) ## 257
sum(div.diff.title$lheavy.t) ## 218
sum(div.diff.title$heavy.t) ## 163

## Measure of championship-level fighters in a division

division.champ.fighters <- ufc.striking.offensive.aggregate%>% 
  group_by(fighter.name) %>% 
  summarise(
    tot.belt = sum(belt.fights),
    flyweight.t = sum(fl.weight),
    bantamweight.t = sum(b.weight),
    feather.t = sum(ft.weight),
    light.t = sum(l.weight),
    welter.t = sum(w.weight),
    middle.t = sum(m.weight),
    lheavy.t = sum(lh.weight),
    heavy.t = sum(h.weight))%>% 
  as.data.frame()


division.champ.fighters$outlier = division.champ.fighters$tot.belt < 0.00000000001   
division.champ.fighters = filter(division.champ.fighters, outlier != TRUE)
division.champ.fighters <- subset(division.champ.fighters, , -c(outlier))

division.champ.fighters <- edit(division.champ.fighters) ## manually inputting weight classes



champ.figters.by.weight <- division.champ.fighters %>% 
  group_by(weight.class) %>% 
  summarize(n = n_distinct(fighter.name))

## Combining total fights - championship fights - and championship fighters into one metri of divisional difficulty

weight.class <- c("Fly","Bantam","Feather","Light","Welter","Middle","LHeavy","Heavy")
fights <- c(462,958,1112,1580,1524,1068,730,698)
champ.fights <- c(149, 201, 226, 281, 295, 257, 218, 163)
champ.fighters <- c(12,21,27,27,25,21,18,12)
division.difficulty.df <- data.frame(weight.class, fights, champ.fights, champ.fighters)

## Proportional values per weight class

division.difficulty.df$weighted.fights <- (division.difficulty.df$fights)/(sum(division.difficulty.df$fights)/8)
division.difficulty.df$weighted.champ.fights <- (division.difficulty.df$champ.fights)/(sum(division.difficulty.df$champ.fights)/8)
division.difficulty.df$weighted.champ.fighter <- (division.difficulty.df$champ.fighter)/(sum(division.difficulty.df$champ.fighter)/8)
division.difficulty.df$weighted.difficulty <- ((division.difficulty.df$weighted.champ.fighter + division.difficulty.df$weighted.champ.fights + division.difficulty.df$weighted.fights)/3)
## weighted.difficulty will be used as a modifier to the final striking metric


G1 <- ggplot(data=division.difficulty.df, aes(x=weight.class, y=weighted.fights))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=c("Fly","Bantam","Feather","Light","Welter","Middle","LHeavy","Heavy"))+
  theme_classic() +labs(x= "Weight Class", y= "Weighted Fights")

G2 <- ggplot(data=division.difficulty.df, aes(x=weight.class, y=weighted.champ.fights))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=c("Fly","Bantam","Feather","Light","Welter","Middle","LHeavy","Heavy"))+
  theme_classic() +labs(x= "Weight Class", y= "Weighted Championship Fights")

G3 <- ggplot(data=division.difficulty.df, aes(x=weight.class, y=weighted.champ.fighter))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=c("Fly","Bantam","Feather","Light","Welter","Middle","LHeavy","Heavy"))+
  theme_classic() +labs(x= "Weight Class", y= "Weighted Championship Fighters")

G4 <- ggplot(data=division.difficulty.df, aes(x=weight.class, y=weighted.difficulty))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=c("Fly","Bantam","Feather","Light","Welter","Middle","LHeavy","Heavy"))+
  theme_classic() +labs(x= "Weight Class", y= "Weighted Difficulty")

plot_grid(G1,G2,G3,G4, labels = c("A","B","C","D"))



############################################################################### DIVISIONAL #################################################################################
############################################################################### Difficulty #################################################################################









############################################################################### Metric #################################################################################
############################################################################## Variance #################################################################################

## To what degree do my metrics explain variations in winning? Will assign weights based on this

ufc.striking.offensive.aggregate$win.prop <- (ufc.striking.offensive.aggregate$win /ufc.striking.offensive.aggregate$total.fights)

SVM.regression <- lm(win.prop ~ SVM, data = ufc.striking.offensive.aggregate)
summary(SVM.regression) ## SVM accounts for 5.5% of variation in winning

SPrM.regression <- lm(win.prop~ SPrM, data = ufc.striking.offensive.aggregate)
summary(SPrM.regression)  ## SPrM accounts for 5.5% of variation in winning

SPwM.regression <- lm(win.prop ~ SPwM, data = ufc.striking.offensive.aggregate)
summary(SPwM.regression)  ## SPwM not significant predictor of winning

SDM.regression <- lm(win.prop ~ str.def, data = ufc.striking.offensive.aggregate)
summary(SDM.regression)  ## str.def not significant predictor of winning







############################################################################### Metric #################################################################################
############################################################################## Variance #################################################################################









################################################################################ Final #################################################################################
############################################################################### Analysis #################################################################################



## Filtering out fighters without at least one fight with a belt on the line
ufc.striking.offensive.aggregate$outlier = ufc.striking.offensive.aggregate$belt.fights < 0.00000000001   
ufc.striking.offensive.aggregate = filter(ufc.striking.offensive.aggregate, outlier != TRUE)
ufc.striking.offensive.aggregate <- subset(ufc.striking.offensive.aggregate, , -c(outlier))

ufc.striking.offensive.aggregate <- merge(ufc.striking.offensive.aggregate, division.champ.fighters, by="fighter.name")
ufc.striking.offensive.aggregate$weight.class.modifier <-

## Filtering out all fighters with less than 10 UFC fights

ufc.striking.offensive.aggregate$outlier = ufc.striking.offensive.aggregate$total.fights < 10   
ufc.striking.offensive.aggregate = filter(ufc.striking.offensive.aggregate, outlier != TRUE)
ufc.striking.offensive.aggregate <- subset(ufc.striking.offensive.aggregate, , -c(outlier))

## Filtering out fighters with less than 250 total significant stikes attempted
ufc.striking.offensive.aggregate$outlier = ufc.striking.offensive.aggregate$tot.ssa < 250   
ufc.striking.offensive.aggregate = filter(ufc.striking.offensive.aggregate, outlier != TRUE)
ufc.striking.offensive.aggregate <- subset(ufc.striking.offensive.aggregate, , -c(outlier))


## Creating Weight Class Modifier Column
ufc.striking.offensive.aggregate<- edit(ufc.striking.offensive.aggregate)

## Assigning ranks by striking metrics
ufc.striking.offensive.aggregate$SPrRank <- rank(ufc.striking.offensive.aggregate$SPrM) 
ufc.striking.offensive.aggregate$SVRank <- rank (ufc.striking.offensive.aggregate$SVM)
ufc.striking.offensive.aggregate$SPwRank <- rank(ufc.striking.offensive.aggregate$SPwM)
ufc.striking.offensive.aggregate$SDefRank <- rank (ufc.striking.offensive.aggregate$str.def)

ufc.striking.offensive.aggregate <- edit(ufc.striking.offensive.aggregate)


## Modifying Ranks based on Weight Class Difficulty


## Calculating the total striking metric with sub metrics weighted by winning variance
ufc.striking.offensive.aggregate$OSMRank <- (((ufc.striking.offensive.aggregate$SPrRank*1.055) + (ufc.striking.offensive.aggregate$SVRank*1.055)) + (ufc.striking.offensive.aggregate$SPwRank)+ (ufc.striking.offensive.aggregate$SDefRank))/4



view1 <- cbind.data.frame (ufc.striking.offensive.aggregate$fighter.name, ufc.striking.offensive.aggregate$OSMRank, ufc.striking.offensive.aggregate$weight.class,
                           ufc.striking.offensive.aggregate$wc.modifier, ufc.striking.offensive.aggregate$belt.fights)

view1 <- edit(view1)

mean(view1$belt.fights)

view1$finalOSMRank <- (view1$OSMRank * view1$wc.modifier * view1$belt.mod) #### CHANGE THE BELT MOD TO REFLECT AVG AMT OF BELT FIGHTS



descriptive.stats <- cbind.data.frame(ufc.striking.offensive.aggregate$fighter.name, ufc.striking.offensive.aggregate$tot.ssl, ufc.striking.offensive.aggregate$tot.ssa, 
                                      ufc.striking.offensive.aggregate$KDPM, ufc.striking.offensive.aggregate$KOPM, ufc.striking.offensive.aggregate$str.def,
                                      ufc.striking.offensive.aggregate$belt.fights, ufc.striking.offensive.aggregate$weight.class,
                                      ufc.striking.offensive.aggregate$SSAPM, ufc.striking.offensive.aggregate$SSLPM)

descriptive.stats$SVM <- (ufc.striking.offensive.aggregate$SVM)
descriptive.stats$SPrM <- (ufc.striking.offensive.aggregate$SPrM)
descriptive.stats$SPwM <- (ufc.striking.offensive.aggregate$SPwM)

mean(descriptive.stats$str.def)














