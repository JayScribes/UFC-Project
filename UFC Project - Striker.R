## UFC Project
## Who is the best striker?
## Best striker will be defined by highest "Striking Metric"
## Boxing Metric = (Striking Offense + Striking Defense)
##Striking Offense = Striking Volume + Striking Precision + Striking Power - these will be differently weighted
## Striking Defense = inverse of opponent's Striking Offense - effectively how good were their opponent's striking against them?

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
                                 ufc_fights$'Total-Strikes-Land', ufc_fights$StrDef,ufc_fights$Belt)

## changing headders
ufc_striking<- edit(ufc_striking) 

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
            belt.fights =mean(Belt),
            tot.gsa = sum(ground.strikes.attempted),
            tot.gsl = sum(ground.strikes.landed),
            tot.fight.time = sum(fight.time.sec),
            tot.tsa = sum(total.strike.attempted),
            tot.tsl = sum(total.strikes.landed)) %>% 
  as.data.frame()

## adding ground strikes to dataset to take them out of the equation

ufc_striking$ground.strikes.attempted <- (ufc_fights$`Ground-Attempt`) ## to be removed from tsa
ufc_striking$ground.strikes.landed <- (ufc_fights$`Ground-Land`) ## to be removed from tsl


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

## Filtering out all fighters with less than 10 UFC fights

ufc.striking.offensive.aggregate$outlier = ufc.striking.offensive.aggregate$total.fights < 10   
ufc.striking.offensive.aggregate = filter(ufc.striking.offensive.aggregate, outlier != TRUE)
ufc.striking.offensive.aggregate <- subset(ufc.striking.offensive.aggregate, , -c(outlier))

## Filtering out fighters with less than 250 total significant stikes attempted
ufc.striking.offensive.aggregate$outlier = ufc.striking.offensive.aggregate$tot.ssa < 250   
ufc.striking.offensive.aggregate = filter(ufc.striking.offensive.aggregate, outlier != TRUE)
ufc.striking.offensive.aggregate <- subset(ufc.striking.offensive.aggregate, , -c(outlier))

## Filtering out fighters without at least one fight with a belt on the line
ufc.striking.offensive.aggregate$outlier = ufc.striking.offensive.aggregate$belt.fights < 0.00000000001   
ufc.striking.offensive.aggregate = filter(ufc.striking.offensive.aggregate, outlier != TRUE)
ufc.striking.offensive.aggregate <- subset(ufc.striking.offensive.aggregate, , -c(outlier))

## Individual Striking Metrics

ufc.striking.offensive.aggregate$SVM <- ((ufc.striking.offensive.aggregate$SLPM)+(ufc.striking.offensive.aggregate$SAPM)) 
ufc.striking.offensive.aggregate$SPrM <- (((((ufc.striking.offensive.aggregate$SSLPM / ufc.striking.offensive.aggregate$SSAPM)*1.5) + (ufc.striking.offensive.aggregate$NSSLPM / ufc.striking.offensive.aggregate$NSSAPM)*0.5))/2)
ufc.striking.offensive.aggregate$SPwM <- ((ufc.striking.offensive.aggregate$KOPM * 1.2) + (ufc.striking.offensive.aggregate$KDPM * 0.8))
ufc.striking.offensive.aggregate$OSM <- (ufc.striking.offensive.aggregate$SVM + ufc.striking.offensive.aggregate$SPrM + ufc.striking.offensive.aggregate$SPwM)


## Assigning ranks by striking metrics
ufc.striking.offensive.aggregate$SPrRank <- rank(ufc.striking.offensive.aggregate$SPrM)
ufc.striking.offensive.aggregate$SVRank <- rank (ufc.striking.offensive.aggregate$SVM)
ufc.striking.offensive.aggregate$SPwRank <- rank(ufc.striking.offensive.aggregate$SPwM)
ufc.striking.offensive.aggregate$SDefRank <- rank (ufc.striking.offensive.aggregate$str.def)


## Calculating the total striking metric with weighted sub-metrics
ufc.striking.offensive.aggregate$OSMRank <- ((ufc.striking.offensive.aggregate$SPrRank*1.35) + (ufc.striking.offensive.aggregate$SVRank) + (ufc.striking.offensive.aggregate$SPwRank*1.2)+ (ufc.striking.offensive.aggregate$SDefRank*1.15 )/4)







