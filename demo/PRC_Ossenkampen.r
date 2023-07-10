
data("Ossenkampen")

# Read example data ----------------------------------------------------



Design <- Ossenkampen[,c("A","B","Plot","Block")]
Design$A <- factor(Design$A);  Design$B <- factor(Design$B)
Design$B <- factor(Design$B, levels = c("Cntrl","PK","NPK","N"))
Design$Plot <- Design$Plot
Design$Block <- factor(Design$Block)

print(with(Design,table(A,B)))

# extract response variables
Y <- as.matrix(Ossenkampen[,-(1:5)]); rownames(Y)<-Ossenkampen$Sample
# transform as a multiplicative model is more logical than an additive one.
# But need to add pseudocount to avoid log(0)= infinite
Y <- log(Y+1)

# PRC via vegan -----------------------------------------------------------

myrda <- vegan::rda( Y~ A:B + Condition(A), data = Design)
print(myrda)
#SubtractReferenceValues has been replaced by the function PRC_scores
Design_w_PRCs <- PRC_scores(myrda, focal_factor_name= "B", referencelevel = 1, rank = 2, data= Design)
print(names(Design_w_PRCs))
print(names(Design_w_PRCs$PRCplus))
print(names(Design_w_PRCs$coefficients))
if (is.list(Design_w_PRCs$coefficients)) print(round(Design_w_PRCs$coefficients[[1]], 2)) else
  print(round(Design_w_PRCs$coefficients, 2))
# note that Design_w_PRCs$coefficients is not use for plotting


# prepare data for ggplot2. sc_ref sites and constraints axis  -----------

library(ggplot2)
# convert factor A to numeric and express as years after start
Design_w_PRCs$PRCplus$dyear <- as.numeric(levels(Design_w_PRCs$PRCplus$A))[Design_w_PRCs$PRCplus$A] -1958

pl <- ggplot(Design_w_PRCs$PRCplus,aes(x = dyear, y= PRC1 , color = B))
pl<- pl +
       geom_line(aes(x = dyear, y= PRC1, color = B, group = Plot),linewidth = 1.2)+
       # outcomment the next line if the data points of  'group' are independent
       geom_line(aes(x=dyear, y = PRC1_E, color = B, linetype = Block, group =Plot))+
       geom_point(aes(x=dyear, y = PRC1_E, color = B, shape = Block, group =Plot))

print(pl+
        ggtitle("Ossenkampen PRC1")+
        xlab("factor A: years after start of experiment") +
        ylab("sample scores (T with individual plot lines)")+scale_color_discrete(name = "factor B")
      )


#  species taxa bar plot, first axis --------------------------------------------------
sc_spec <- Design_w_PRCs$species
spec.df <- data.frame("Taxon"=rownames(sc_spec),as.data.frame(sc_spec))
logabu <- colSums(Y)
spec.df100 <- spec.df[logabu >100 & abs(spec.df$RDA1) > 1,]
id <- order(spec.df100$RDA1,decreasing = FALSE)

spec.df100sort <- spec.df100[id,]
spec.df100sort$Taxon <- factor(spec.df100sort$Taxon, levels = spec.df100$Taxon[id])
RDA1.spec <- ggplot(data=spec.df100sort, aes(x=Taxon, y=RDA1)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + coord_flip()
RDA1.spec
# many species scores are positive, while the PRC scores are mostly negative
# so that the conclusion is that many species decrease after N and NPK fertilization
