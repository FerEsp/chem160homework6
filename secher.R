sech<-read.table("secher.txt", header=T)
model<-lm(bwt~bpd, data=sech)
sum.model<-summary(model)
R2<-sum.model$r.squared
f<-sum.model$fstatistic
p.value<-pf(f[1],f[2],f[3],lower.tail=F)
output<-sprintf("R2 = %f and p-value=%f", R2, p.value)
cat(output,"\n")
intercept<-model$coefficients[1]
slope<-model$coefficients[2]
output<-sprintf("slope=%f  intercept=%f",slope, intercept)
cat(output,"\n")
png("bwt_bpd.png")
plot(bwt~bpd, data=sech)
abline(model)
dev.off()
model<-lm(bwt~ad, data=sech)
sum.model<-summary(model)
R2<-sum.model$r.squared
f<-sum.model$fstatistic
p.value<-pf(f[1],f[2],f[3],lower.tail=F)
output<-sprintf("R2 = %f and p-value=%f", R2, p.value)
cat(output,"\n")
intercept<-model$coefficients[1]
slope<-model$coefficients[2]
output<-sprintf("slope=%f  intercept=%f",slope, intercept)
cat(output,"\n")
png("bwt_ad.png")
plot(bwt~ad, data=sech)
abline(model)
dev.off()
model<-lm(bwt~bpd+ad, data=sech)
sum.model<-summary(model)
R2<-sum.model$r.squared
f<-sum.model$fstatistic
p.value<-pf(f[1],f[2],f[3],lower.tail=F)
output<-sprintf("R2 = %f and p-value=%f", R2, p.value)
cat(output,"\n")