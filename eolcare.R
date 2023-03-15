
options(mc.cores=8)
library(BART3)
library(lattice)
los=read.csv('los.csv')
los$race=factor(los$race)

train=(los$strata<5)
pick=-(1:4)
x.train=los[train, pick]

## sample size of training set
(N=sum(train))

file.='loscrisk.rds'
if(!file.exists(file.)) {
    set.seed(21)
    ## use sparse DART prior and train the model with BART
    post=crisk.bart(x.train, times=los$days[train],
                    delta=los$tx[train], x.train2=x.train,
                    x.test=x.train, x.test2=x.train,
                    sparse=TRUE)
    saveRDS(post, file.)
} else {
    post=readRDS(file.)
}

source('ci.panel.R')
source('cif.R')
source('RR.R')

## create CIF plots
col.=c(1, 2, 4, 8, 3)
pred.(post, 'sex', 1:2, 'Sex',
      legend=c('Withdrawal', 'S: F', 'S: M',
               'Hospice', 'S: F', 'S: M'), 
      col=c(0, 2, 1, 0, 2, 1), lty=c(0, 1, 1, 0, 2, 2), lwd=2,
      inset=0.05)

race=c(4, 2, 3)
H=length(race)
pred.(post, 'race', race, 'Race',
      legend=c('Withdrawal', 'White', 'Black', 'Other', 
               'Hospice', 'White', 'Black', 'Other'), 
      lwd=2, lty=c(0, rep(1, H), 0, rep(2, H)),
      col=c(0, col.[1:H], 0, col.[1:H]),
      inset=0.05)

pred.(post, 'monitor', 0:1, 'Cerebral Monitor',
      legend=c('Withdrawal', 'CM: Yes', 'CM: No',
               'Hospice', 'CM: Yes', 'CM: No'),
      lwd=2, col=c(0, 2, 1, 0, 2, 1), lty=c(0, 1, 1, 0, 2, 2),
      inset=0.05)

pred.(post, 'cc_dementia', 0:1, 'Dementia',
      legend=c('Withdrawal', 'D: Yes', 'D: No',
               'Hospice', 'D: Yes', 'D: No'),
      lwd=2, col=c(0, 2, 1, 0, 2, 1), lty=c(0, 1, 1, 0, 2, 2),
      inset=0.05)

gcs=c(4, 6, 8, 10, 12)
pred.(post, 'gcs', gcs, 'Glasgow Coma Scale',
      legend=c('Withdrawal', paste0(gcs),
               'Hospice', paste0(gcs)), cex=0.95, 
      lwd=2, lty=c(0, rep(1, 5), 0, rep(2, 5)), col=c(0, col., 0, col.),
      inset=0.05)

age10=c(7.0, 7.5, 8.0, 8.5, 9.0)
age=age10*10
pred.(post, 'age10', age10, 'Age',
      legend=c('Withdrawal', paste0(age),
               'Hospice', paste0(age)), 
      lwd=2, lty=c(0, rep(1, 5), 0, rep(2, 5)), col=c(0, col., 0, col.),
      inset=0.05)

iss_05=c(10, 15, 20, 25, 30)
pred.(post, 'iss_05', iss_05, 'Injury Severity Score',
      legend=c('Withdrawal', paste0(iss_05),
               'Hospice', paste0(iss_05)), 
      lwd=2, lty=c(0, rep(1, 5), 0, rep(2, 5)), col=c(0, col., 0, col.),
      inset=0.05)

pred.(post, 'hispanic', 0:1, 'Hispanic',
      legend=c('Withdrawal', 'H: Yes', 'H: No',
               'Hospice', 'H: Yes', 'H: No'), 
      col=c(0, 2, 1, 0, 2, 1), lty=c(0, 1, 1, 0, 2, 2), lwd=2,
      inset=0.05)

pred.(post, 'cc_adlc', 0:1, 'ADLC',
      legend=c('Withdrawal', 'ADLC: Yes', 'ADLC: No',
               'Hospice', 'ADLC: Yes', 'ADLC: No'), 
      col=c(0, 2, 1, 0, 2, 1), lty=c(0, 1, 1, 0, 2, 2), lwd=2,
      inset=0.05)

pred.(post, 'nonprofit', 0:1, 'Non-profit',
      legend=c('Withdrawal', 'NP: Yes', 'NP: No',
               'Hospice', 'NP: Yes', 'NP: No'), 
      col=c(0, 2, 1, 0, 2, 1), lty=c(0, 1, 1, 0, 2, 2), lwd=2,
      inset=0.05)

pred.(post, 'cc_diabetes', 0:1, 'Diabetes',
      legend=c('Withdrawal', 'DM: Yes', 'DM: No',
               'Hospice', 'DM: Yes', 'DM: No'), 
      col=c(0, 2, 1, 0, 2, 1), lty=c(0, 1, 1, 0, 2, 2), lwd=2,
      inset=0.05)

pred.(post, 'fall', 0:1, 'Fall',
      legend=c('Withdrawal', 'F: Yes', 'F: No',
               'Hospice', 'F: Yes', 'F: No'),
      col=c(0, 2, 1, 0, 2, 1), lty=c(0, 1, 1, 0, 2, 2), lwd=2,
      inset=0.05)

for(i in 1:12) {
    k=c(2, 2, 5, 5, 5, 3, 2, 2, 2, 2, 2, 2)[i]
    var.=c('monitor', 'cc_dementia', 'gcs', 'age10', 'iss_05',
           'race', 'hispanic', 'sex', 'cc_adlc', 'nonprofit', 'cc_diabetes', 'fall')[i]
    file.=paste0('cif.', var., '.rds')
    cif.=readRDS(file.)
dimnames(cif.)[[1]]=rep(var., 6*k)
dimnames(cif.)[[2]]=c('Value', 'Day', 'Estimate', 'Lower', 'Upper')
    print(format(cif., digits=2))
}

## create RR plots
col.=c(1, 2, 4, 8, 3)
RR(post, 'sex', 1:2, 'Sex',
      legend=c('Withdrawal: F vs. M',
               'Hospice: F vs. M'), 
      col=c(2, 2), lty=c(1, 2), lwd=2,
      inset=0.02)

race=c(4, 2, 3)
H=length(race)
RR(post, 'race', race, 'Race',
      legend=c('Withdrawal', 'White', 'Black', 'Other',
               'Hospice', 'White', 'Black', 'Other'),
      lwd=2, lty=c(0, 0, 1, 1, 0, 0, 2, 2), col=c(0, 0, 2, 4, 0, 0, 2, 4),
      inset=0.02)

gcs=c(4, 6, 8, 10, 12)
RR(post, 'gcs', gcs, 'Glasgow Coma Scale',
      legend=c('Withdrawal', paste0(gcs),
               'Hospice', paste0(gcs)), 
      lwd=2, lty=c(0, rep(1, 5), 0, rep(2, 5)), col=c(0, 0, 2, 4, 8, 3, 0, 0, 2, 4, 8, 3),
      inset=0.02)

RR(post, 'monitor', 0:1, 'Cerebral Monitor',
      legend=c('Withdrawal: Yes vs. No',
               'Hospice: Yes vs. No'),
      lwd=2, col=c(2, 2), lty=c(1, 2),
      inset=0.02)

RR(post, 'cc_dementia', 0:1, 'Dementia', xy='bottomleft',
      legend=c('Withdrawal: Yes vs. No',
               'Hospice: Yes vs. No'),
      lwd=2, col=c(2, 2), lty=c(1, 2),
      inset=0.02)

age10=c(7.0, 7.5, 8.0, 8.5, 9.0)
age=age10*10
RR(post, 'age10', age10, 'Age', y=0.45,
      legend=c('Withdrawal', paste0(age),
               'Hospice', paste0(age)), 
      lwd=2, lty=c(0, rep(1, 5), 0, rep(2, 5)), col=c(0, 0, 2, 4, 8, 3, 0, 0, 2, 4, 8, 3),
      inset=0.02)

iss_05=c(10, 15, 20, 25, 30)
RR(post, 'iss_05', iss_05, 'Injury Severity Score',
      legend=c('Withdrawal', paste0(iss_05),
               'Hospice', paste0(iss_05)), 
      lwd=2, lty=c(0, rep(1, 5), 0, rep(2, 5)), col=c(0, 0, 2, 4, 8, 3, 0, 0, 2, 4, 8, 3),
      inset=0.02)

RR(post, 'hispanic', 0:1, 'Hispanic',
      legend=c('Withdrawal: Yes vs. No',
               'Hospice: Yes vs. No'), 
      col=c(2, 2), lty=c(1, 2), lwd=2,
      inset=0.02)

RR(post, 'cc_adlc', 0:1, 'ADLC',
      legend=c('Withdrawal: Yes vs. No',
               'Hospice: Yes vs. No'), 
      col=c(2, 2), lty=c(1, 2), lwd=2,
      inset=0.02)

RR(post, 'nonprofit', 0:1, 'Non-profit',
      legend=c('Withdrawal: Yes vs. No',
               'Hospice: Yes vs. No'), 
      col=c(2, 2), lty=c(1, 2), lwd=2,
      inset=0.02)

RR(post, 'cc_diabetes', 0:1, 'Diabetes',
      legend=c('Withdrawal: Yes vs. No',
               'Hospice: Yes vs. No'), 
      col=c(2, 2), lty=c(1, 2), lwd=2,
      inset=0.02)

RR(post, 'fall', 0:1, 'Fall',
      legend=c('Withdrawal: Yes vs. No',
               'Hospice: Yes vs. No'), 
      col=c(2, 2), lty=c(1, 2), lwd=2,
      inset=0.02)

for(i in 1:12) {
    k=c(2, 2, 5, 5, 5, 3, 2, 2, 2, 2, 2, 2)[i]-1
    var.=c('monitor', 'cc_dementia', 'gcs', 'age10', 'iss_05',
           'race', 'hispanic', 'sex', 'cc_adlc', 'nonprofit', 'cc_diabetes', 'fall')[i]
    file.=paste0('risk.', var., '.rds')
    RR.=readRDS(file.)
dimnames(RR.)[[1]]=rep(var., 6*k)
dimnames(RR.)[[2]]=c('Value', 'Day', 'Estimate', 'Lower', 'Upper', 'Prob>1')
    print(format(RR., digits=3))
}

