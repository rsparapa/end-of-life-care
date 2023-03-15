
RR = function(post, var., vals, sub, legend, horiz=FALSE, cex=1,
                 col=NULL, lty=NULL, lwd=2, inset=0, xy='topleft',
              x=0.05, y=0.95) {

K=post$K
NK = N*K
(M = nrow(post$yhat.test))
times=post$times
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

file.=paste0('RR.', var., '.rds')
if(!file.exists(file.)) {
    pred=predict(post, tx.test, tx.test, mc.cores=8)
    saveRDS(pred, file.)
} else { pred = readRDS(file.) }

RR.test = matrix(nrow=M, ncol=K*H)
RR.test2 = matrix(nrow=M, ncol=K*H)
for(h in 1:H) {
    for(j in 1:K) {
        i=K*(h-1)+j
        k=NK*(h-1)+seq(j, NK, K)
        l=seq(j, NK, K)
        RR.test[ , i]=apply(pred$prob.test[ , k]/pred$prob.test[ , l], 1, mean) 
        RR.test2[ , i]=apply(pred$prob.test2[ , k]/pred$prob.test2[ , l], 1, mean) 
        }
}

RR.test.mean=apply(RR.test, 2, mean)
RR.test2.mean=apply(RR.test2, 2, mean)
RR.test.prob=apply(RR.test>1, 2, mean)
RR.test2.prob=apply(RR.test2>1, 2, mean)
RR.test.025 =apply(RR.test, 2, quantile, probs=0.025)
RR.test2.025=apply(RR.test2, 2, quantile, probs=0.025)
RR.test.975 =apply(RR.test, 2, quantile, probs=0.975)
RR.test2.975=apply(RR.test2, 2, quantile, probs=0.975)

file.=paste0('risk.', var., '.rds')
RR.test. = cbind(vals[2], c(3, 7, 14), 
    matrix(c(RR.test.mean[K+3], RR.test.025[K+3], RR.test.975[K+3], RR.test.prob[K+3],
                     RR.test.mean[K+7], RR.test.025[K+7], RR.test.975[K+7], RR.test.prob[K+7],
                     RR.test.mean[K+14], RR.test.025[K+14], RR.test.975[K+14], RR.test.prob[K+14]),
                   byrow=TRUE, ncol=4, nrow=3))
RR.test2. = cbind(vals[2], c(3, 7, 14),
                   matrix(c(RR.test2.mean[K+3], RR.test2.025[K+3], RR.test2.975[K+3], RR.test2.prob[K+3],
                      RR.test2.mean[K+7], RR.test2.025[K+7], RR.test2.975[K+7], RR.test2.prob[K+7],
                      RR.test2.mean[K+14], RR.test2.025[K+14], RR.test2.975[K+14], RR.test2.prob[K+14]),
                    byrow=TRUE, ncol=4, nrow=3))
if(H>2) for(j in 3:H) {
k=(j-1)*K
RR. = cbind(vals[j], c(3, 7, 14),
             matrix(c(RR.test.mean[k+3], RR.test.025[k+3], RR.test.975[k+3], RR.test.prob[k+3],
                RR.test.mean[k+7], RR.test.025[k+7], RR.test.975[k+7], RR.test.prob[k+7],
                RR.test.mean[k+14], RR.test.025[k+14], RR.test.975[k+14], RR.test.prob[k+14]),
              byrow=TRUE, ncol=4, nrow=3))
RR2. = cbind(vals[j], c(3, 7, 14),
              matrix(c( RR.test2.mean[k+3], RR.test2.025[k+3], RR.test2.975[k+3], RR.test2.prob[k+3],
                  RR.test2.mean[k+7], RR.test2.025[k+7], RR.test2.975[k+7], RR.test2.prob[k+7],
                  RR.test2.mean[k+14], RR.test2.025[k+14], RR.test2.975[k+14], RR.test2.prob[k+14]),
               byrow=TRUE, ncol=4, nrow=3))
RR.test. =rbind(RR.test., RR.)
RR.test2.=rbind(RR.test2., RR2.)
}

saveRDS(rbind(RR.test., RR.test2.), file.)

df = data.frame(RR.test.mean=RR.test.mean[1:K], times=times, group=1)
for(h in 2:H) {
    k = K*(h-1)
    df = rbind(df,data.frame(RR.test.mean=RR.test.mean[k+1:K],
                times=times, group=h)
               )
    }


file.=paste0('risk-', var., '.pdf')
pdf(file=file.)
col.=c(1, 2, 4, 8, 3)
print(xyplot(RR.test.mean~times, groups=group, type='n', data=df, col=col.,
       lwd=2, xlim=1:14, xlab='t (days)', ylab='RR(t|x)', sub=sub,
       scales=list(y=list(limits=log(c(0.1, 10)), ##log=TRUE,
                          at=log(c(0.1, 0.2, 0.5, 1, 2, 5, 10)),
                          labels=paste(c(0.1, 0.2, 0.5, 1, 2, 5, 10)))),
       key=list(x=x, y=y, border='black',
                text=list(legend, cex=cex),
                lines=list(col=col, lty=lty, lwd=2)),
       panel=function(...) {
           for(h in 2:H) {
               k = K*(h-1)+1:K
               panel.abline(h=0, lty=3)
               if(H==2)
               ci.panel(times, log(RR.test2.025[k]), log(RR.test2.975[k]), TRUE)
               llines(times, log(RR.test2.mean[k]), col=col.[h], lty=2, lwd=2) 
               if(H==2)
                   ci.panel(times, log(RR.test.025[k]), log(RR.test.975[k]), FALSE)
               llines(times, log(RR.test.mean[k]), col=col.[h], lwd=2) 
           }
           panel.xyplot(...)
       }))
## plot(times, RR.test.mean[K+1:K], type='l', lwd=lwd, log='y',
##      xlab='t (days)', ylab='RR(t|x)',
##      xlim=c(1, 14), ylim=c(0.1, 10), sub=sub)
##      ##ylim=c(min., max.), sub=sub)
## lines(times, RR.test2.mean[K+1:K], lty=2, lwd=lwd)
## if(H>2) for(h in 3:H) {
##     lines(times, RR.test.mean[K*(h-1)+1:K], col=h-1, lwd=lwd)
##     lines(times, RR.test2.mean[K*(h-1)+1:K], col=h-1, lty=2, lwd=lwd)
## }
## if(length(col)>0 && length(lty)>0)
##     legend(xy, legend=legend, lwd=lwd, lty=lty, col=col,
##            inset=inset, horiz=horiz, cex=cex)
## abline(v=0, h=0:1, col=8)
dev.off()
}
