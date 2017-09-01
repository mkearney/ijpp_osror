table(Pnl3$SocMed)
Pnl3SM <- subset(Pnl3, SocMed<2, select=ID:PrtSt)

Pnl3SM$SMuCom1 <- (Pnl3SM$SMu1a + Pnl3SM$SMu2a + Pnl3SM$SMu3a + Pnl3SM$SMu4a)/4
Pnl3SM$SMuCom2 <- (Pnl3SM$SMu1b + Pnl3SM$SMu2b + Pnl3SM$SMu3b + Pnl3SM$SMu4b)/4
Pnl3SM$SMuCom3 <- (Pnl3SM$SMu1c + Pnl3SM$SMu2c + Pnl3SM$SMu3c + Pnl3SM$SMu4c)/4

describe(Pnl3SM$SMuCom3)

## Null Model ## 
IJPP0sm <- '
## no change in variances over time, no covariances
PrintA ~~ V1*PrintA
DigA ~~ V2*DigA
CableA ~~ V3*CableA
PT1a ~~ V4*PT1a
PT2a ~~ V5*PT2a
PT3a ~~ V6*PT3a
SMu1a ~~ V7*SMu1a
SMu2a ~~ V8*SMu2a
SMu3a ~~ V9*SMu3a
SMu4a ~~ V10*SMu4a

PrintB ~~ V1*PrintB
DigB ~~ V2*DigB
CableB ~~ V3*CableB
PT1b ~~ V4*PT1b
PT2b ~~ V5*PT2b
PT3b ~~ V6*PT3b
SMu1b ~~ V7*SMu1b
SMu2b ~~ V8*SMu2b
SMu3b ~~ V9*SMu3b
SMu4b ~~ V10*SMu4b

PrintC ~~ V1*PrintC
DigC ~~ V2*DigC
CableC ~~ V3*CableC
PT1c ~~ V4*PT1c
PT2c ~~ V5*PT2c
PT3c ~~ V6*PT3c
SMu1c ~~ V7*SMu1c
SMu2c ~~ V8*SMu2c
SMu3c ~~ V9*SMu3c
SMu4c ~~ V10*SMu4c

## no change in means over time
PrintA ~ T1*1
DigA ~ T2*1
CableA ~ T3*1
PT1a ~ T4*1
PT2a ~ T5*1
PT3a ~ T6*1
SMu1a ~ T7*1
SMu2a ~ T8*1
SMu3a ~ T9*1
SMu4a ~ T10*1

PrintB ~ T1*1
DigB ~ T2*1
CableB ~ T3*1
PT1b ~ T4*1
PT2b ~ T5*1
PT3b ~ T6*1
SMu1b ~ T7*1
SMu2b ~ T8*1
SMu3b ~ T9*1
SMu4b ~ T10*1

PrintC ~ T1*1
DigC ~ T2*1
CableC ~ T3*1
PT1c ~ T4*1
PT2c ~ T5*1
PT3c ~ T6*1
SMu1c ~ T7*1
SMu2c ~ T8*1
SMu3c ~ T9*1
SMu4c ~ T10*1
'
fit0.0sm <- lavaan(IJPP0sm, data=Pnl3SM, orthogonal=T, missing="fiml", , estimator="MLR")
summary(fit0.0sm, fit=T)


## Configural Invariance ##
IJPP0.1sm <- '

#define latent variables

PressA =~ p11*PrintA + p12*DigA + p13*CableA
PressB =~ p21*PrintB + p22*DigB + p23*CableB
PressC =~ p31*PrintC +p32* DigC + p33*CableC

PTA =~ t11*PT1a + t12*PT2a + t13*PT3a
PTB =~ t21*PT1b + t22*PT2b + t23*PT3b
PTC =~ t31*PT1c + t32*PT2c + t33*PT3c

SMuA =~ s11*SMu1a + s12*SMu2a + s13*SMu3a + s14*SMu4a
SMuB =~ s21*SMu1b + s22*SMu2b + s23*SMu3b + s24*SMu4b
SMuC =~ s31*SMu1c + s32*SMu2c + s33*SMu3c + s34*SMu4c

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

SMu1a ~~ SMu1a
SMu1b ~~ SMu1b
SMu1c ~~ SMu1c

SMu2a ~~ SMu2a
SMu2b ~~ SMu2b
SMu2c ~~ SMu2c

SMu3a ~~ SMu3a
SMu3b ~~ SMu3b
SMu3c ~~ SMu3c

SMu4a ~~ SMu4a
SMu4b ~~ SMu4b
SMu4c ~~ SMu4c

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

SMu1a ~~ SMu1b + SMu1c
SMu2a ~~ SMu2b + SMu2c
SMu3a ~~ SMu3b + SMu3c
SMu4a ~~ SMu4b + SMu4c

SMu1b ~~ SMu1c
SMu2b ~~ SMu2c
SMu3b ~~ SMu3c
SMu4b ~~ SMu4c

#intercepts
PrintA ~ P1*1
DigA ~ P2*1
CableA ~ P3*1
PT1a ~ T1*1
PT2a ~ T2*1
PT3a ~ T3*1
SMu1a ~ S1*1
SMu2a ~ S2*1
SMu3a ~ S3*1
SMu4a ~ S4*1

PrintB ~ P4*1
DigB ~ P5*1
CableB ~ P6*1
PT1b ~ T4*1
PT2b ~ T5*1
PT3b ~ T6*1
SMu1b ~ S5*1
SMu2b ~ S6*1
SMu3b ~ S7*1
SMu4b ~ S8*1

PrintC ~ P7*1
DigC ~ P8*1
CableC ~ P9*1
PT1c ~ T7*1
PT2c ~ T8*1
PT3c ~ T9*1
SMu1c ~ S9*1
SMu2c ~ S10*1
SMu3c ~ S11*1
SMu4c ~ S12*1

#latent variances
PressA ~~ PressA
PressB ~~ PressB
PressC ~~ PressC

PTA ~~ PTA
PTB ~~ PTB
PTC ~~ PTC

SMuA ~~ SMuA
SMuB ~~ SMuB
SMuC ~~ SMuC

#latent co-variances
PressA ~~ PressB + PressC + PTA + PTB + PTC + SMuA + SMuB + SMuC
PressB ~~ PressC + PTA + PTB + PTC + SMuA + SMuB + SMuC
PressC ~~ PTA + PTB + PTC + SMuA + SMuB + SMuC
PTA ~~ PTB + PTC  + SMuA + SMuB + SMuC
PTB ~~ PTC  + SMuA + SMuB + SMuC
PTC ~~ SMuA + SMuB + SMuC
SMuA ~~ SMuB + SMuC
SMuB ~~ SMuC

#latent means
PressA ~ 1
PressB ~ 1
PressC ~ 1

PTA ~ 1
PTB ~ 1
PTC ~ 1

SMuA ~ 1
SMuB ~ 1
SMuC ~ 1

#constraints for effects coding
p11 == 3 - p12 - p13
t11 == 3 - t12 - t13
s11 == 4 - s12 - s13 - s14

p21 == 3 - p22 - p23
t21 == 3 - t22 - t23
s21 == 4 - s22 - s23 - s24

p31 == 3 - p32 - p33
t31 == 3 - t32 - t33
s31 == 4 - s32 - s33 - s34

P1 == 0 - P2 - P3
T1 == 0 - T2 - T3
S1 == 0 - S2 - S3 - S4

P4 == 0 - P5 - P6
T4 == 0 - T5 - T6
S5 == 0 - S6 - S7 - S8

P7 == 0 - P8 - P9
T7 == 0 - T8 - T9
S9 == 0 - S10 - S11 - S12
'
fit0.1sm <- lavaan(IJPP0.1sm, data=Pnl3SM, std.lv=F, auto.fix.first=F, missing="fiml", estimator="MLR")
summary(fit0.1sm, standardized=T, fit=T)
#Adj CFI = .981
#Adj TLI = .978

## Loading Invariance ##

IJPP0.2sm <- '

#define latent variables

PressA =~ p11*PrintA + p12*DigA + p13*CableA
PressB =~ p11*PrintB + p12*DigB + p13*CableB
PressC =~ p11*PrintC +p12* DigC + p13*CableC

PTA =~ t11*PT1a + t12*PT2a + t13*PT3a
PTB =~ t11*PT1b + t12*PT2b + t13*PT3b
PTC =~ t11*PT1c + t12*PT2c + t13*PT3c

SMuA =~ s11*SMu1a + s12*SMu2a + s13*SMu3a + s14*SMu4a
SMuB =~ s11*SMu1b + s12*SMu2b + s13*SMu3b + s14*SMu4b
SMuC =~ s11*SMu1c + s12*SMu2c + s13*SMu3c + s14*SMu4c


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

SMu1a ~~ SMu1a
SMu1b ~~ SMu1b
SMu1c ~~ SMu1c

SMu2a ~~ SMu2a
SMu2b ~~ SMu2b
SMu2c ~~ SMu2c

SMu3a ~~ SMu3a
SMu3b ~~ SMu3b
SMu3c ~~ SMu3c

SMu4a ~~ SMu4a
SMu4b ~~ SMu4b
SMu4c ~~ SMu4c

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

SMu1a ~~ SMu1b + SMu1c
SMu2a ~~ SMu2b + SMu2c
SMu3a ~~ SMu3b + SMu3c
SMu4a ~~ SMu4b + SMu4c

SMu1b ~~ SMu1c
SMu2b ~~ SMu2c
SMu3b ~~ SMu3c
SMu4b ~~ SMu4c

#intercepts
PrintA ~ P1*1
DigA ~ P2*1
CableA ~ P3*1
PT1a ~ T1*1
PT2a ~ T2*1
PT3a ~ T3*1
SMu1a ~ S1*1
SMu2a ~ S2*1
SMu3a ~ S3*1
SMu4a ~ S4*1

PrintB ~ P4*1
DigB ~ P5*1
CableB ~ P6*1
PT1b ~ T4*1
PT2b ~ T5*1
PT3b ~ T6*1
SMu1b ~ S5*1
SMu2b ~ S6*1
SMu3b ~ S7*1
SMu4b ~ S8*1

PrintC ~ P7*1
DigC ~ P8*1
CableC ~ P9*1
PT1c ~ T7*1
PT2c ~ T8*1
PT3c ~ T9*1
SMu1c ~ S9*1
SMu2c ~ S10*1
SMu3c ~ S11*1
SMu4c ~ S12*1

#latent variances
PressA ~~ PressA
PressB ~~ PressB
PressC ~~ PressC

PTA ~~ PTA
PTB ~~ PTB
PTC ~~ PTC

SMuA ~~ SMuA
SMuB ~~ SMuB
SMuC ~~ SMuC

#latent co-variances
PressA ~~ PressB + PressC + PTA + PTB + PTC + SMuA + SMuB + SMuC
PressB ~~ PressC + PTA + PTB + PTC + SMuA + SMuB + SMuC
PressC ~~ PTA + PTB + PTC + SMuA + SMuB + SMuC
PTA ~~ PTB + PTC  + SMuA + SMuB + SMuC
PTB ~~ PTC  + SMuA + SMuB + SMuC
PTC ~~ SMuA + SMuB + SMuC
SMuA ~~ SMuB + SMuC
SMuB ~~ SMuC

#latent means
PressA ~ 1
PressB ~ 1
PressC ~ 1

PTA ~ 1
PTB ~ 1
PTC ~ 1

SMuA ~ 1
SMuB ~ 1
SMuC ~ 1

#constraints for effects coding
p11 == 3 - p12 - p13
t11 == 3 - t12 - t13
s11 == 4 - s12 - s13 - s14

P1 == 0 - P2 - P3
T1 == 0 - T2 - T3
S1 == 0 - S2 - S3 - S4

P4 == 0 - P5 - P6
T4 == 0 - T5 - T6
S5 == 0 - S6 - S7 - S8

P7 == 0 - P8 - P9
T7 == 0 - T8 - T9
S9 == 0 - S10 - S11 - S12
'
fit0.2sm <- lavaan(IJPP0.2sm, data=Pnl3SM, std.lv=F, auto.fix.first=F, missing="fiml", estimator="MLR")
summary(fit0.2sm, standardized=T, fit=T)

## Intercept Invariance ## 

IJPP0.3sm <- '
#define latent variables

PressA =~ p11*PrintA + p12*DigA + p13*CableA
PressB =~ p11*PrintB + p12*DigB + p13*CableB
PressC =~ p11*PrintC +p12* DigC + p13*CableC

PTA =~ t11*PT1a + t12*PT2a + t13*PT3a
PTB =~ t11*PT1b + t12*PT2b + t13*PT3b
PTC =~ t11*PT1c + t12*PT2c + t13*PT3c

SMuA =~ s11*SMu1a + s12*SMu2a + s13*SMu3a + s14*SMu4a
SMuB =~ s11*SMu1b + s12*SMu2b + s13*SMu3b + s14*SMu4b
SMuC =~ s11*SMu1c + s12*SMu2c + s13*SMu3c + s14*SMu4c


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

SMu1a ~~ SMu1a
SMu1b ~~ SMu1b
SMu1c ~~ SMu1c

SMu2a ~~ SMu2a
SMu2b ~~ SMu2b
SMu2c ~~ SMu2c

SMu3a ~~ SMu3a
SMu3b ~~ SMu3b
SMu3c ~~ SMu3c

SMu4a ~~ SMu4a
SMu4b ~~ SMu4b
SMu4c ~~ SMu4c

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

SMu1a ~~ SMu1b + SMu1c
SMu2a ~~ SMu2b + SMu2c
SMu3a ~~ SMu3b + SMu3c
SMu4a ~~ SMu4b + SMu4c

SMu1b ~~ SMu1c
SMu2b ~~ SMu2c
SMu3b ~~ SMu3c
SMu4b ~~ SMu4c

#intercepts
PrintA ~ P1*1
DigA ~ P2*1
CableA ~ P3*1
PT1a ~ T1*1
PT2a ~ T2*1
PT3a ~ T3*1
SMu1a ~ S1*1
SMu2a ~ S2*1
SMu3a ~ S3*1
SMu4a ~ S4*1

PrintB ~ P1*1
DigB ~ P2*1
CableB ~ P3*1
PT1b ~ T1*1
PT2b ~ T2*1
PT3b ~ T3*1
SMu1b ~ S1*1
SMu2b ~ S2*1
SMu3b ~ S3*1
SMu4b ~ S4*1

PrintC ~ P1*1
DigC ~ P2*1
CableC ~ P3*1
PT1c ~ T1*1
PT2c ~ T2*1
PT3c ~ T3*1
SMu1c ~ S1*1
SMu2c ~ S2*1
SMu3c ~ S3*1
SMu4c ~ S4*1

#latent variances
PressA ~~ PressA
PressB ~~ PressB
PressC ~~ PressC

PTA ~~ PTA
PTB ~~ PTB
PTC ~~ PTC

SMuA ~~ SMuA
SMuB ~~ SMuB
SMuC ~~ SMuC

#latent co-variances
PressA ~~ PressB + PressC + PTA + PTB + PTC + SMuA + SMuB + SMuC
PressB ~~ PressC + PTA + PTB + PTC + SMuA + SMuB + SMuC
PressC ~~ PTA + PTB + PTC + SMuA + SMuB + SMuC
PTA ~~ PTB + PTC  + SMuA + SMuB + SMuC
PTB ~~ PTC  + SMuA + SMuB + SMuC
PTC ~~ SMuA + SMuB + SMuC
SMuA ~~ SMuB + SMuC
SMuB ~~ SMuC

#latent means
PressA ~ 1
PressB ~ 1
PressC ~ 1

PTA ~ 1
PTB ~ 1
PTC ~ 1

SMuA ~ 1
SMuB ~ 1
SMuC ~ 1

#constraints for effects coding
p11 == 3 - p12 - p13
t11 == 3 - t12 - t13
s11 == 4 - s12 - s13 - s14

P1 == 0 - P2 - P3
T1 == 0 - T2 - T3
S1 == 0 - S2 - S3 - S4
'
fit0.3sm <- lavaan(IJPP0.3sm, data=Pnl3SM, std.lv=F, auto.fix.first=F, missing="fiml", estimator="MLR")
summary(fit0.3sm, standardized=T, fit=T)


## Regressions ## 
IJPP1.0sm <- '
PTC ~ PTB + PressB + SMuB
PressC ~ PressB + PTB + SMuB
SMuC ~ SMuB + PressB + PTB

PTB ~ PTA + PressA + SMuA
PressB ~ PressA + PTA + SMuA
SMuB ~ SMuA + PressA + PTA

#define latent variables

PressA =~ p11*PrintA + p12*DigA + p13*CableA
PressB =~ p11*PrintB + p12*DigB + p13*CableB
PressC =~ p11*PrintC +p12* DigC + p13*CableC

PTA =~ t11*PT1a + t12*PT2a + t13*PT3a
PTB =~ t11*PT1b + t12*PT2b + t13*PT3b
PTC =~ t11*PT1c + t12*PT2c + t13*PT3c

SMuA =~ s11*SMu1a + s12*SMu2a + s13*SMu3a + s14*SMu4a
SMuB =~ s11*SMu1b + s12*SMu2b + s13*SMu3b + s14*SMu4b
SMuC =~ s11*SMu1c + s12*SMu2c + s13*SMu3c + s14*SMu4c


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

SMu1a ~~ SMu1a
SMu1b ~~ SMu1b
SMu1c ~~ SMu1c

SMu2a ~~ SMu2a
SMu2b ~~ SMu2b
SMu2c ~~ SMu2c

SMu3a ~~ SMu3a
SMu3b ~~ SMu3b
SMu3c ~~ SMu3c

SMu4a ~~ SMu4a
SMu4b ~~ SMu4b
SMu4c ~~ SMu4c

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

SMu1a ~~ SMu1b + SMu1c
SMu2a ~~ SMu2b + SMu2c
SMu3a ~~ SMu3b + SMu3c
SMu4a ~~ SMu4b + SMu4c

SMu1b ~~ SMu1c
SMu2b ~~ SMu2c
SMu3b ~~ SMu3c
SMu4b ~~ SMu4c

#intercepts
PrintA ~ P1*1
DigA ~ P2*1
CableA ~ P3*1
PT1a ~ T1*1
PT2a ~ T2*1
PT3a ~ T3*1
SMu1a ~ S1*1
SMu2a ~ S2*1
SMu3a ~ S3*1
SMu4a ~ S4*1

PrintB ~ P1*1
DigB ~ P2*1
CableB ~ P3*1
PT1b ~ T1*1
PT2b ~ T2*1
PT3b ~ T3*1
SMu1b ~ S1*1
SMu2b ~ S2*1
SMu3b ~ S3*1
SMu4b ~ S4*1

PrintC ~ P1*1
DigC ~ P2*1
CableC ~ P3*1
PT1c ~ T1*1
PT2c ~ T2*1
PT3c ~ T3*1
SMu1c ~ S1*1
SMu2c ~ S2*1
SMu3c ~ S3*1
SMu4c ~ S4*1

#latent variances
PressA ~~ PressA
PressB ~~ PressB
PressC ~~ PressC

PTA ~~ PTA
PTB ~~ PTB
PTC ~~ PTC

SMuA ~~ SMuA
SMuB ~~ SMuB
SMuC ~~ SMuC

#latent co-variances
PressA ~~ PTA + SMuA 
PressB ~~ PTB + SMuB
PressC ~~ PTC + SMuC
PTA ~~ SMuA
PTB ~~ SMuB
PTC ~~ + SMuC

#latent means
PressA ~ 1
PressB ~ 1
PressC ~ 1

PTA ~ 1
PTB ~ 1
PTC ~ 1

SMuA ~ 1
SMuB ~ 1
SMuC ~ 1

#constraints for effects coding
p11 == 3 - p12 - p13
t11 == 3 - t12 - t13
s11 == 4 - s12 - s13 - s14

P1 == 0 - P2 - P3
T1 == 0 - T2 - T3
S1 == 0 - S2 - S3 - S4


'
fit1.0sm <- lavaan(IJPP1.0sm, data=Pnl3SM, std.lv=F, auto.fix.first=F, missing="fiml", estimator="MLR")
summary(fit1.0sm, standardized=T, fit=T)
inspect(fit1.0, "modindices")


## Regression Model ## 


IJPP2.0sm <- '
PTC ~ PTB + PressB + SMuB
PressC ~ PressB + PTB + SMuB
SMuC ~ SMuB + PressB + PTB

PTB ~ PTA + PressA + SMuA
PressB ~ PressA + PTA + SMuA
SMuB ~ SMuA + PressA + PTA

PressA ~ Age + Female + AfAm + Asian + Hisp + OthRace + Edu + Inc + BA + Ideol + PrtSt + PI1a
PTA ~ Age + Female + AfAm + Asian + Hisp + OthRace + Edu + Inc+ BA + Ideol + PrtSt + PI1a
SMuA ~ Age + Female + AfAm + Asian + Hisp + OthRace + Edu + Inc+ BA + Ideol + PrtSt + PI1a

#define latent variables

PressA =~ p11*PrintA + p12*DigA + p13*CableA
PressB =~ p11*PrintB + p12*DigB + p13*CableB
PressC =~ p11*PrintC +p12* DigC + p13*CableC

PTA =~ t11*PT1a + t12*PT2a + t13*PT3a
PTB =~ t11*PT1b + t12*PT2b + t13*PT3b
PTC =~ t11*PT1c + t12*PT2c + t13*PT3c

SMuA =~ s11*SMu1a + s12*SMu2a + s13*SMu3a + s14*SMu4a
SMuB =~ s11*SMu1b + s12*SMu2b + s13*SMu3b + s14*SMu4b
SMuC =~ s11*SMu1c + s12*SMu2c + s13*SMu3c + s14*SMu4c


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

SMu1a ~~ SMu1a
SMu1b ~~ SMu1b
SMu1c ~~ SMu1c

SMu2a ~~ SMu2a
SMu2b ~~ SMu2b
SMu2c ~~ SMu2c

SMu3a ~~ SMu3a
SMu3b ~~ SMu3b
SMu3c ~~ SMu3c

SMu4a ~~ SMu4a
SMu4b ~~ SMu4b
SMu4c ~~ SMu4c

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

SMu1a ~~ SMu1b + SMu1c
SMu2a ~~ SMu2b + SMu2c
SMu3a ~~ SMu3b + SMu3c
SMu4a ~~ SMu4b + SMu4c

SMu1b ~~ SMu1c
SMu2b ~~ SMu2c
SMu3b ~~ SMu3c
SMu4b ~~ SMu4c

#intercepts
PrintA ~ P1*1
DigA ~ P2*1
CableA ~ P3*1
PT1a ~ T1*1
PT2a ~ T2*1
PT3a ~ T3*1
SMu1a ~ S1*1
SMu2a ~ S2*1
SMu3a ~ S3*1
SMu4a ~ S4*1

PrintB ~ P1*1
DigB ~ P2*1
CableB ~ P3*1
PT1b ~ T1*1
PT2b ~ T2*1
PT3b ~ T3*1
SMu1b ~ S1*1
SMu2b ~ S2*1
SMu3b ~ S3*1
SMu4b ~ S4*1

PrintC ~ P1*1
DigC ~ P2*1
CableC ~ P3*1
PT1c ~ T1*1
PT2c ~ T2*1
PT3c ~ T3*1
SMu1c ~ S1*1
SMu2c ~ S2*1
SMu3c ~ S3*1
SMu4c ~ S4*1

#latent variances
PressA ~~ PressA
PressB ~~ PressB
PressC ~~ PressC

PTA ~~ PTA
PTB ~~ PTB
PTC ~~ PTC

SMuA ~~ SMuA
SMuB ~~ SMuB
SMuC ~~ SMuC

#latent co-variances
PressA ~~ PTA + SMuA 
PressB ~~ PTB + SMuB
PressC ~~ PTC + SMuC
PTA ~~ SMuA
PTB ~~ SMuB
PTC ~~ + SMuC

#latent means
PressA ~ 1
PressB ~ 1
PressC ~ 1

PTA ~ 1
PTB ~ 1
PTC ~ 1

SMuA ~ 1
SMuB ~ 1
SMuC ~ 1

#constraints for effects coding
p11 == 3 - p12 - p13
t11 == 3 - t12 - t13
s11 == 4 - s12 - s13 - s14

P1 == 0 - P2 - P3
T1 == 0 - T2 - T3
S1 == 0 - S2 - S3 - S4


'
fit2.0sm <- lavaan(IJPP2.0sm, data=Pnl3SM, std.lv=F, auto.fix.first=F, missing="fiml", estimator="MLR")
summary(fit2.0sm, standardized=T, fit=T)
inspect(fit2.0, "modindices")

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