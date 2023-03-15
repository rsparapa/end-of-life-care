
pred. = function(post, var., vals, sub, legend, horiz=FALSE, cex=1,
                 col=NULL, lty=NULL, lwd=2, x=0.05, y=0.95,
                 inset=0, xy='topleft') {
K=post$K
NK = N*K
(M = nrow(post$yhat.test))
times.=c(0, post$times)

H = length(vals)
if(H==2)
    tx.test = rbind(post$tx.test, post$tx.test)
else if(H==3)
    tx.test = rbind(post$tx.test, post$tx.test, post$tx.test)
else if(H==5)
    tx.test = rbind(post$tx.test, post$tx.test, post$tx.test,
                    post$tx.test, post$tx.test)
if(var.=='race') { ## 4, 2, 3
    tx.test[ , 'race1'] = rep(c(0, 1, 0), each=NK)
    tx.test[ , 'race2'] = rep(c(0, 0, 1), each=NK)        
    tx.test[ , 'race3'] = rep(c(1, 0, 0), each=NK)        
    } else { tx.test[ , var.] = rep(vals, each=NK) }

file.=paste0('pred.', var., '.rds')
if(!file.exists(file.)) {
    pred=predict(post, tx.test, tx.test, mc.cores=8)
    saveRDS(pred, file.)
} else { pred = readRDS(file.) }

cif.test = matrix(nrow=M, ncol=K*H)
cif.test2 = matrix(nrow=M, ncol=K*H)
for(h in 1:H) {
    for(j in 1:K) {
        i=K*(h-1)+j
        k=NK*(h-1)+seq(j, NK, K)
        cif.test[ , i]=apply(pred$cif.test[ , k], 1, mean) 
        cif.test2[ , i]=apply(pred$cif.test2[ , k], 1, mean) 
        }
}

cif.test.mean =apply(cif.test, 2, mean)
cif.test2.mean=apply(cif.test2, 2, mean)
cif.test.025 =apply(cif.test, 2, quantile, probs=0.025)
cif.test2.025=apply(cif.test2, 2, quantile, probs=0.025)
cif.test.975 =apply(cif.test, 2, quantile, probs=0.975)
cif.test2.975=apply(cif.test2, 2, quantile, probs=0.975)

file.=paste0('cif.', var., '.rds')
cif.test. = cbind(vals[1], c(3, 7, 14), 
    matrix(c(cif.test.mean[3], cif.test.025[3], cif.test.975[3],
                     cif.test.mean[7], cif.test.025[7], cif.test.975[7],
                     cif.test.mean[14], cif.test.025[14], cif.test.975[14]),
                   byrow=TRUE, ncol=3, nrow=3))
cif.test2. = cbind(vals[1], c(3, 7, 14),
                   matrix(c(cif.test2.mean[3], cif.test2.025[3], cif.test2.975[3],
                      cif.test2.mean[7], cif.test2.025[7], cif.test2.975[7],
                      cif.test2.mean[14], cif.test2.025[14], cif.test2.975[14]),
                    byrow=TRUE, ncol=3, nrow=3))
for(j in 2:H) {
k=(j-1)*K
cif. = cbind(vals[j], c(3, 7, 14),
             matrix(c(cif.test.mean[k+3], cif.test.025[k+3], cif.test.975[k+3],
                cif.test.mean[k+7], cif.test.025[k+7], cif.test.975[k+7],
                cif.test.mean[k+14], cif.test.025[k+14], cif.test.975[k+14]),
              byrow=TRUE, ncol=3, nrow=3))
cif2. = cbind(vals[j], c(3, 7, 14),
              matrix(c( cif.test2.mean[k+3], cif.test2.025[k+3], cif.test2.975[k+3],
                  cif.test2.mean[k+7], cif.test2.025[k+7], cif.test2.975[k+7],
                  cif.test2.mean[k+14], cif.test2.025[k+14], cif.test2.975[k+14]),
               byrow=TRUE, ncol=3, nrow=3))
cif.test. =rbind(cif.test., cif.)
cif.test2.=rbind(cif.test2., cif2.)
}

saveRDS(rbind(cif.test., cif.test2.), file.)

df = data.frame(cif.test.mean=c(0, cif.test.mean[1:K]),
                times=times., group=1)
for(h in 2:H) {
    k = K*(h-1)
    df = rbind(df,data.frame(cif.test.mean=c(0, cif.test.mean[k+1:K]),
                times=times., group=h)
               )
    }

file.=paste0('cif-', var., '.pdf')
pdf(file=file.)
col.=c(1, 2, 4, 8, 3)
print(xyplot(cif.test.mean~times, groups=group, type='l', data=df, col=col.,
       lwd=2, xlim=0:14, ylim=0:1, xlab='t (days)', ylab='F(t|x)', sub=sub,
       key=list(x=x, y=y, border='black',
                text=list(legend, cex=cex),
                lines=list(col=col, lty=lty, lwd=2)),
       panel=function(...) {
           for(h in 1:H) {
               k = K*(h-1)+1:K
               j=((h%%2)==0)
               if(H==2)
               ci.panel(times., c(0, cif.test2.025[k]), c(0, cif.test2.975[k]),
                        j)
               llines(times., c(0, cif.test2.mean[k]), col=col.[h], lty=2, lwd=2) 
               if(H==2)
                   ci.panel(times., c(0, cif.test.025[k]), c(0, cif.test.975[k]),
                        j)
           }
           panel.xyplot(...)
       }))
dev.off()
}

