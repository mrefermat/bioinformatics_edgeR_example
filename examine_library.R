require(edgeR)
dat<-read.csv('xxx')
x = new("DGEList")
x$counts <- as.matrix(dat[ , 4:9])
boxplot(x$counts)
boxplot(log2(x$counts))
plotMDS(logs2(x$counts+1))
plotMDS(logs2(x$counts))
cor(log2(X$counts+1))
1-cor(log2(X$counts+1))
as.dist(1-cor(log2(X$counts+1)))
hclust(as.dist(1-cor(log2(X$counts+1))))
plot(hclust(as.dist(1-cor(log2(X$counts+1)))))
sel=rowSums(cpm(x$counts)>1)>=2
dim(x$counts)
x$genes=dat[sel, 1:3]
x$counts = x$counts[sel, ]
rownames(x$counts)=dat[sel,2]
x$samples = data.frame(SampleID = colnames(x$counts),group=factor(c("T0",
	  "T0","T1","T1","T1","T1")),lib.size=colSums(x$counts))
dim(x)
x=calcNormFactors(x)
x$samples
des=models.matrix(~0+group,data=x$samples)
colnames(des) <- levels(x$samples$groups)
des
levels(x$samples$group)
plotMDS(log2(x$counts+1))
cor(log2(x$counts+1))
1-cor(log2(x$counts+1))
as.dist(1-cor(log2(x$counts+1)))
hclust(as.dist(1-cor(log2(X$counts+1))))
plot(hclust(as.dist(1-cor(log2(X$counts+1)))))
xglm = estimateDisp(x,des)
fit <- glmQLFit(xglm,des)
shRNA.T0_T1 <- glmQLFTest(test,contrast=c(0,-1,1))
topTags(shRNA.T0_T1)
genesymbols=x$genes[,1]
genesymbollist = list()
for(i in unique(genesymbols)) genesymbollist[[i]] = which(geneymbols==i)
head(T0_T1.camera)
shRNA.T1_PI <- glmQLFTest(fit,contrast=c(0,-1,1))
topTags(shRNA.T1_PI)
T1_PI.camera = camera(xglm,index=genesymbollist,des,contrast=c(0,-1,1))
head(T1_PI.camera)
sgRNA.T0_T1 <- topTags(shRNA.T0_T1,n=Inf)
write(sgRNA.T0_T1,XXXX)
write(T0_T1.camera,XXXX)



