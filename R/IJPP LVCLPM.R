names(Pnl3)

Pnl3$PressA <- (Pnl3$PrintA + Pnl3$DigA + Pnl3$CableA)/3
Pnl3$PressB <- (Pnl3$PrintB + Pnl3$DigB + Pnl3$CableB)/3
Pnl3$PressC <- (Pnl3$PrintC + Pnl3$DigC + Pnl3$CableC)/3

describe(Pnl3$PressA)
describe(Pnl3$PressB)
describe(Pnl3$PressC)

Pnl3$PTA <- (Pnl3$PT1a + Pnl3$PT2a + Pnl3$PT3a)/3
Pnl3$PTB <- (Pnl3$PT1b + Pnl3$PT2b + Pnl3$PT3b)/3
Pnl3$PTC <- (Pnl3$PT1c + Pnl3$PT2c + Pnl3$PT3c)/3

describe(Pnl3$PTA)
describe(Pnl3$PTB)
describe(Pnl3$PTC)

#Describe Sample# 
describe(Pnl3$Age)
table(Pnl3$Gender)
table(Pnl3$Party7)
table(Pnl3$Race)
table(Pnl3$BA)
table(Pnl3$Edu)
table(Pnl3$Inc)
table(Pnl3$Ideol)
table(Pnl3$PrtSt)
describe(Pnl3$PI1a)

describe(Pnl3$Age)
table(AugR$Gender)
table(AugR$Party7)
table(Pnl3$Race)
table(AugR$BA)
table(Pnl3$Edu)
table(AugR$Inc)
table(Pnl3$Ideol)
table(Pnl3$PrtSt)
describe(AugR$Party7)
describe(AugR$Ideol)
describe(AugR$PI_1)

describe(Pnl3$Age)
table(Pnl$Gender)
table(AugR$Party7)
table(Pnl$Race)
table(Pnl$BA)
table(Pnl$Edu)
table(Pnl$Inc)
table(Pnl3$Ideol)
table(Pnl3$PrtSt)
describe(Pnl$Party7)
describe(Pnl$Ideol)
describe(Pnl$PI_1a)

#Random Variable Working# 
Pnl3$Female <- recode(Pnl3$Gender, "1=0;2=1")
Pnl3$AfAm <- recode(Pnl3$Race, "1=1;2=0;3=0;4=0;5=0;6=0;7=0;8=0;9=0")
Pnl3$Asian <- recode(Pnl3$Race, "1=0;2=0;3=1;4=0;5=0;6=0;7=0;8=0;9=0")
Pnl3$Hisp <- recode(Pnl3$Race, "1=0;2=0;3=0;4=0;5=0;6=0;7=0;8=1;9=0")
Pnl3$OthRace <- recode(Pnl3$Race, "1=0;2=0;3=0;4=0;5=1;6=1;7=1;8=1;9=0")
Pnl3$BA <- recode(Pnl3$BA, "1=1;2=0")
Pnl3$Inc <- recode(Pnl3$Inc, "1=1;2=2;3=3;4=4;5=5;6=6;7=7;8=8;9=9;10=NA")
Pnl3$PrtSt <- recode(Pnl3$Party7, "1=3;2=2;3=1;4=0;5=1;6=2;7=3")


Pnl3$Party3 <- recode(Pnl3$Party7, "1=1;2=1;3=1;4=0;5=2;6=2;7=2")
Pnl3$PartyTie <- recode(Pnl3$PartyTie, "1=1;2=2;NA=0")
table(Pnl3$Party3)
table(Pnl3$PartyTie)

Pnl3$Party2 <- Pnl3$Party3 + Pnl3$PartyTie
table(Pnl3$Party2)

Pnl3$Dem <- recode(Pnl3$Party2, "1=1;2=0")
Pnl3$Rep <- recode(Pnl3$Party2, "1=0;2=1")

Pnl3$OGftA1 <- Pnl3$FT_3*Pnl3$Rep
Pnl3$OGftA2 <- Pnl3$FT_6*Pnl3$Dem
Pnl3$OGftA <- Pnl3$OGftA1 + Pnl3$OGftA2
describe(Pnl3$OGftA)

table(Pnl3$OGftA)

#Create Press Variables# 
Pnl3$PrintA <- (Pnl3$NwsPprLa + Pnl3$NwsPprCa)/2
Pnl3$DigA <- (Pnl3$ConBloga + Pnl3$LibBloga + Pnl3$OnLinea)/3
Pnl3$CableA <- (Pnl3$Foxa + Pnl3$MSNBCa + Pnl3$CNNa)/3 

Pnl3$PrintB <- (Pnl3$NwsPprLb + Pnl3$NwsPprCb)/2
Pnl3$DigB <- (Pnl3$ConBlogb + Pnl3$LibBlogb + Pnl3$OnLineb)/3
Pnl3$CableB <- (Pnl3$Foxb + Pnl3$MSNBCb + Pnl3$CNNb)/3 

Pnl3$PrintC <- (Pnl3$NwsPprLc + Pnl3$NswPprCc)/2
Pnl3$DigC <- (Pnl3$ConBlogc + Pnl3$LibBlogc + Pnl3$OnLinec)/3
Pnl3$CableC <- (Pnl3$Foxc + Pnl3$MSNBCc + Pnl3$CNNc)/3 

## Null Model ## 
IJPP0 <- '
## no change in variances over time, no covariances
PrintA ~~ V1*PrintA
DigA ~~ V2*DigA
CableA ~~ V3*CableA
PT1a ~~ V4*PT1a
PT2a ~~ V5*PT2a
PT3a ~~ V6*PT3a

PrintB ~~ V1*PrintB
DigB ~~ V2*DigB
CableB ~~ V3*CableB
PT1b ~~ V4*PT1b
PT2b ~~ V5*PT2b
PT3b ~~ V6*PT3b

PrintC ~~ V1*PrintC
DigC ~~ V2*DigC
CableC ~~ V3*CableC
PT1c ~~ V4*PT1c
PT2c ~~ V5*PT2c
PT3c ~~ V6*PT3c

## no change in means over time
PrintA ~ T1*1
DigA ~ T2*1
CableA ~ T3*1
PT1a ~ T4*1
PT2a ~ T5*1
PT3a ~ T6*1

PrintB ~ T1*1
DigB ~ T2*1
CableB ~ T3*1
PT1b ~ T4*1
PT2b ~ T5*1
PT3b ~ T6*1

PrintC ~ T1*1
DigC ~ T2*1
CableC ~ T3*1
PT1c ~ T4*1
PT2c ~ T5*1
PT3c ~ T6*1
'
fit0.0 <- lavaan(IJPP0, data=Pnl3, orthogonal=T, missing="fiml", , estimator="MLR")
summary(fit0.0, fit=T)


## Configural Invariance ##
IJPP0.1 <- '

#define latent variables

PressA =~ p11*PrintA + p12*DigA + p13*CableA
PressB =~ p21*PrintB + p22*DigB + p23*CableB
PressC =~ p31*PrintC +p32* DigC + p33*CableC

PTA =~ t11*PT1a + t12*PT2a + t13*PT3a
PTB =~ t21*PT1b + t22*PT2b + t23*PT3b
PTC =~ t31*PT1c + t32*PT2c + t33*PT3c

#residuals 

PrintA ~~ PrintA
PrintB ~~ PrintB
PrintC ~~ PrintC

DigA ~~ DigA
DigB ~~ DigB
DigC ~~ DigC

CableA ~~ CableA
CableB ~~ CableB
CableC ~~ CableC

PT1a ~~ PT1a
PT1b ~~ PT1b
PT1c ~~ PT1c

PT2a ~~ PT2a
PT2b ~~ PT2b
PT2c ~~ PT2c

PT3a ~~ PT3a
PT3b ~~ PT3b
PT3c ~~ PT3c

#correlated residuals accross time

PrintA ~~ PrintB + PrintC
DigA ~~ DigB + DigC
CableA ~~ CableB + CableC

PrintB ~~ PrintC 
DigB ~~ DigC
CableB ~~ CableC

PT1a ~~ PT1b + PT1c
PT2a ~~ PT2b + PT2c
PT3a ~~ PT3b + PT3c

PT1b ~~ PT1c
PT2b ~~ PT2c
PT3b ~~ PT3c

#intercepts
PrintA ~ P1*1
DigA ~ P2*1
CableA ~ P3*1
PT1a ~ T1*1
PT2a ~ T2*1
PT3a ~ T3*1

PrintB ~ P4*1
DigB ~ P5*1
CableB ~ P6*1
PT1b ~ T4*1
PT2b ~ T5*1
PT3b ~ T6*1

PrintC ~ P7*1
DigC ~ P8*1
CableC ~ P9*1
PT1c ~ T7*1
PT2c ~ T8*1
PT3c ~ T9*1

#latent variances
PressA ~~ PressA
PressB ~~ PressB
PressC ~~ PressC

PTA ~~ PTA
PTB ~~ PTB
PTC ~~ PTC

#latent co-variances
PressA ~~ PressB + PressC + PTA + PTB + PTC
PressB ~~ PressC + PTA + PTB + PTC
PressC ~~ PTA + PTB + PTC
PTA ~~ PTB + PTC 
PTB ~~ PTC 

#latent means
PressA ~ 1
PressB ~ 1
PressC ~ 1

PTA ~ 1
PTB ~ 1
PTC ~ 1

#constraints for effects coding
p11 == 3 - p12 - p13
t11 == 3 - t12 - t13

p21 == 3 - p22 - p23
t21 == 3 - t22 - t23

p31 == 3 - p32 - p33
t31 == 3 - t32 - t33

P1 == 0 - P2 - P3
T1 == 0 - T2 - T3

P4 == 0 - P5 - P6
T4 == 0 - T5 - T6

P7 == 0 - P8 - P9
T7 == 0 - T8 - T9
'
fit0.1 <- lavaan(IJPP0.1, data=Pnl3, std.lv=F, auto.fix.first=F, missing="fiml", estimator="MLR")
summary(fit0.1, standardized=T, fit=T)
#Adj CFI = .989
#Adj TLI = .980

## Loading Invariance ##

IJPP0.2 <- '

#define latent variables

PressA =~ p11*PrintA + p12*DigA + p13*CableA
PressB =~ p11*PrintB + p12*DigB + p13*CableB
PressC =~ p11*PrintC +p12* DigC + p13*CableC

PTA =~ t11*PT1a + t12*PT2a + t13*PT3a
PTB =~ t11*PT1b + t12*PT2b + t13*PT3b
PTC =~ t11*PT1c + t12*PT2c + t13*PT3c

#residuals 

PrintA ~~ PrintA
PrintB ~~ PrintB
PrintC ~~ PrintC

DigA ~~ DigA
DigB ~~ DigB
DigC ~~ DigC

CableA ~~ CableA
CableB ~~ CableB
CableC ~~ CableC

PT1a ~~ PT1a
PT1b ~~ PT1b
PT1c ~~ PT1c

PT2a ~~ PT2a
PT2b ~~ PT2b
PT2c ~~ PT2c

PT3a ~~ PT3a
PT3b ~~ PT3b
PT3c ~~ PT3c

#correlated residuals accross time

PrintA ~~ PrintB + PrintC
DigA ~~ DigB + DigC
CableA ~~ CableB + CableC

PrintB ~~ PrintC 
DigB ~~ DigC
CableB ~~ CableC

PT1a ~~ PT1b + PT1c
PT2a ~~ PT2b + PT2c
PT3a ~~ PT3b + PT3c

PT1b ~~ PT1c
PT2b ~~ PT2c
PT3b ~~ PT3c

#intercepts
PrintA ~ P1*1
DigA ~ P2*1
CableA ~ P3*1
PT1a ~ T1*1
PT2a ~ T2*1
PT3a ~ T3*1

PrintB ~ P4*1
DigB ~ P5*1
CableB ~ P6*1
PT1b ~ T4*1
PT2b ~ T5*1
PT3b ~ T6*1

PrintC ~ P7*1
DigC ~ P8*1
CableC ~ P9*1
PT1c ~ T7*1
PT2c ~ T8*1
PT3c ~ T9*1

#latent variances
PressA ~~ PressA
PressB ~~ PressB
PressC ~~ PressC

PTA ~~ PTA
PTB ~~ PTB
PTC ~~ PTC

#latent co-variances
PressA ~~ PressB + PressC + PTA + PTB + PTC
PressB ~~ PressC + PTA + PTB + PTC
PressC ~~ PTA + PTB + PTC
PTA ~~ PTB + PTC 
PTB ~~ PTC 

#latent means
PressA ~ 1
PressB ~ 1
PressC ~ 1

PTA ~ 1
PTB ~ 1
PTC ~ 1

#constraints for effects coding
p11 == 3 - p12 - p13
t11 == 3 - t12 - t13

P1 == 0 - P2 - P3
T1 == 0 - T2 - T3

P4 == 0 - P5 - P6
T4 == 0 - T5 - T6

P7 == 0 - P8 - P9
T7 == 0 - T8 - T9
'
fit0.2 <- lavaan(IJPP0.2, data=Pnl3, std.lv=F, auto.fix.first=F, missing="fiml", estimator="MLR")
summary(fit0.2, standardized=T, fit=T)

## Intercept Invariance ## 

IJPP0.3 <- '
#define latent variables

PressA =~ p11*PrintA + p12*DigA + p13*CableA
PressB =~ p11*PrintB + p12*DigB + p13*CableB
PressC =~ p11*PrintC +p12* DigC + p13*CableC

PTA =~ t11*PT1a + t12*PT2a + t13*PT3a
PTB =~ t11*PT1b + t12*PT2b + t13*PT3b
PTC =~ t11*PT1c + t12*PT2c + t13*PT3c

#residuals 

PrintA ~~ PrintA
PrintB ~~ PrintB
PrintC ~~ PrintC

DigA ~~ DigA
DigB ~~ DigB
DigC ~~ DigC

CableA ~~ CableA
CableB ~~ CableB
CableC ~~ CableC

PT1a ~~ PT1a
PT1b ~~ PT1b
PT1c ~~ PT1c

PT2a ~~ PT2a
PT2b ~~ PT2b
PT2c ~~ PT2c

PT3a ~~ PT3a
PT3b ~~ PT3b
PT3c ~~ PT3c

#correlated residuals accross time

PrintA ~~ PrintB + PrintC
DigA ~~ DigB + DigC
CableA ~~ CableB + CableC

PrintB ~~ PrintC 
DigB ~~ DigC
CableB ~~ CableC

PT1a ~~ PT1b + PT1c
PT2a ~~ PT2b + PT2c
PT3a ~~ PT3b + PT3c

PT1b ~~ PT1c
PT2b ~~ PT2c
PT3b ~~ PT3c

#intercepts
PrintA ~ P1*1
DigA ~ P2*1
CableA ~ P3*1
PT1a ~ T1*1
PT2a ~ T2*1
PT3a ~ T3*1

PrintB ~ P1*1
DigB ~ P2*1
CableB ~ P3*1
PT1b ~ T1*1
PT2b ~ T2*1
PT3b ~ T3*1

PrintC ~ P1*1
DigC ~ P2*1
CableC ~ P3*1
PT1c ~ T1*1
PT2c ~ T2*1
PT3c ~ T3*1

#latent variances
PressA ~~ PressA
PressB ~~ PressB
PressC ~~ PressC

PTA ~~ PTA
PTB ~~ PTB
PTC ~~ PTC

#latent co-variances
PressA ~~ PressB + PressC + PTA + PTB + PTC
PressB ~~ PressC + PTA + PTB + PTC
PressC ~~ PTA + PTB + PTC
PTA ~~ PTB + PTC 
PTB ~~ PTC 

#latent means
PressA ~ 1
PressB ~ 1
PressC ~ 1

PTA ~ 1
PTB ~ 1
PTC ~ 1

#constraints for effects coding
p11 == 3 - p12 - p13
t11 == 3 - t12 - t13

P1 == 0 - P2 - P3
T1 == 0 - T2 - T3
'
fit0.3 <- lavaan(IJPP0.3, data=Pnl3, std.lv=F, auto.fix.first=F, missing="fiml", estimator="MLR")
summary(fit0.3, standardized=T, fit=T)


## Regressions ## 
IJPP1.0 <- '
PTC ~ PTB + PressB 
PressC ~ PressB + PTB

PTB ~ PTA + PressA
PressB ~ PressA + PTA 

PressA ~~ PTA
PressB ~~ PTB
PressC ~~ PTC

#define latent variables

PressA =~ p11*PrintA + p12*DigA + p13*CableA
PressB =~ p11*PrintB + p12*DigB + p13*CableB
PressC =~ p11*PrintC +p12* DigC + p13*CableC

PTA =~ t11*PT1a + t12*PT2a + t13*PT3a
PTB =~ t11*PT1b + t12*PT2b + t13*PT3b
PTC =~ t11*PT1c + t12*PT2c + t13*PT3c

#residuals 

PrintA ~~ PrintA
PrintB ~~ PrintB
PrintC ~~ PrintC

DigA ~~ DigA
DigB ~~ DigB
DigC ~~ DigC

CableA ~~ CableA
CableB ~~ CableB
CableC ~~ CableC

PT1a ~~ PT1a
PT1b ~~ PT1b
PT1c ~~ PT1c

PT2a ~~ PT2a
PT2b ~~ PT2b
PT2c ~~ PT2c

PT3a ~~ PT3a
PT3b ~~ PT3b
PT3c ~~ PT3c

#correlated residuals accross time

PrintA ~~ PrintB + PrintC
DigA ~~ DigB + DigC
CableA ~~ CableB + CableC

PrintB ~~ PrintC 
DigB ~~ DigC
CableB ~~ CableC

PT1a ~~ PT1b + PT1c
PT2a ~~ PT2b + PT2c
PT3a ~~ PT3b + PT3c

PT1b ~~ PT1c
PT2b ~~ PT2c
PT3b ~~ PT3c

#intercepts
PrintA ~ P1*1
DigA ~ P2*1
CableA ~ P3*1
PT1a ~ T1*1
PT2a ~ T2*1
PT3a ~ T3*1

PrintB ~ P1*1
DigB ~ P2*1
CableB ~ P3*1
PT1b ~ T1*1
PT2b ~ T2*1
PT3b ~ T3*1

PrintC ~ P1*1
DigC ~ P2*1
CableC ~ P3*1
PT1c ~ T1*1
PT2c ~ T2*1
PT3c ~ T3*1

#latent variances
PressA ~~ PressA
PressB ~~ PressB
PressC ~~ PressC

PTA ~~ PTA
PTB ~~ PTB
PTC ~~ PTC

#latent means
PressA ~ 1
PressB ~ 1
PressC ~ 1

PTA ~ 1
PTB ~ 1
PTC ~ 1

#constraints for effects coding
p11 == 3 - p12 - p13
t11 == 3 - t12 - t13

P1 == 0 - P2 - P3
T1 == 0 - T2 - T3
'
fit1.0 <- lavaan(IJPP1.0, data=Pnl3, std.lv=F, auto.fix.first=F, missing="fiml", estimator="MLR")
summary(fit1.0, standardized=T, fit=T)
inspect(fit1.0, "modindices")


## Regression Model ## 

IJPP2.0 <- '
PTC ~ PTB + PressB 
PressC ~ PressB + PTB

PTB ~ PTA + PressA
PressB ~ PressA + PTA 

PressA ~ Age + Female + AfAm + Asian + Hisp + OthRace + Edu + Inc + BA + Ideol + PrtSt + PI1a
PTA ~ Age + Female + AfAm + Asian + Hisp + OthRace + Edu + Inc+ BA + Ideol + PrtSt + PI1a

PressA ~~ PTA
PressB ~~ PTB
PressC ~~ PTC

#define latent variables

PressA =~ p11*PrintA + p12*DigA + p13*CableA
PressB =~ p11*PrintB + p12*DigB + p13*CableB
PressC =~ p11*PrintC +p12* DigC + p13*CableC

PTA =~ t11*PT1a + t12*PT2a + t13*PT3a
PTB =~ t11*PT1b + t12*PT2b + t13*PT3b
PTC =~ t11*PT1c + t12*PT2c + t13*PT3c

#residuals 

PrintA ~~ PrintA
PrintB ~~ PrintB
PrintC ~~ PrintC

DigA ~~ DigA
DigB ~~ DigB
DigC ~~ DigC

CableA ~~ CableA
CableB ~~ CableB
CableC ~~ CableC

PT1a ~~ PT1a
PT1b ~~ PT1b
PT1c ~~ PT1c

PT2a ~~ PT2a
PT2b ~~ PT2b
PT2c ~~ PT2c

PT3a ~~ PT3a
PT3b ~~ PT3b
PT3c ~~ PT3c

#correlated residuals accross time

PrintA ~~ PrintB + PrintC 
DigA ~~ DigB + DigC
CableA ~~ CableB + CableC

PrintB ~~ PrintC 
DigB ~~ DigC
CableB ~~ CableC

PT1a ~~ PT1b + PT1c
PT2a ~~ PT2b + PT2c
PT3a ~~ PT3b + PT3c

PT1b ~~ PT1c
PT2b ~~ PT2c
PT3b ~~ PT3c

#intercepts
PrintA ~ P1*1
DigA ~ P2*1
CableA ~ P3*1
PT1a ~ T1*1
PT2a ~ T2*1
PT3a ~ T3*1

PrintB ~ P1*1
DigB ~ P2*1
CableB ~ P3*1
PT1b ~ T1*1
PT2b ~ T2*1
PT3b ~ T3*1

PrintC ~ P1*1
DigC ~ P2*1
CableC ~ P3*1
PT1c ~ T1*1
PT2c ~ T2*1
PT3c ~ T3*1

#latent variances
PressA ~~ PressA
PressB ~~ PressB
PressC ~~ PressC

PTA ~~ PTA
PTB ~~ PTB
PTC ~~ PTC

#latent means
PressA ~ 1
PressB ~ 1
PressC ~ 1

PTA ~ 1
PTB ~ 1
PTC ~ 1

#constraints for effects coding
p11 == 3 - p12 - p13
t11 == 3 - t12 - t13

P1 == 0 - P2 - P3
T1 == 0 - T2 - T3
'
fit2.0 <- lavaan(IJPP2.0, data=Pnl3, std.lv=F, auto.fix.first=F, missing="fiml", estimator="MLR")
summary(fit2.0, standardized=T, fit=T)
inspect(fit2.0, "modindices")

