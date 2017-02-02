final_data <- read.csv("C:/Users/ahona/Desktop/R_folder/final_data.csv", row.names=1)
final_dataSet <- final_data
##View(final_data)

# load the plsdepot package
library(plsdepot)
##head(final_dataSet)

##names(final_dataSet)
##This data has 855 variables and 95 observations.  It is included in the plsdepot package.
##out of total 855 variables, first 841 variables are predictors and 
##last 14 variables are response variables
##response variable column(s) at the end of your dataframe. 
##Here is the code to build the model and in this case we 
##will examine it with 3 components (latent variables) by using comps=3. 
pls2 = plsreg2(final_dataSet[, 1:841], final_dataSet[, 842:855, drop = FALSE], comps = 3)
# correlations plot;
plot(pls2)

##components of the predictor variables(also knowns as T components)
pls2$x.scores

##loading of predictor variables
pls2$x.loads

##components of the response variables(also knowns as U components)
pls2$y.scores

##loading of response variables
pls2$y.loads

##correlation between X and T
pls2$cor.xt

##correlation between Y and T
pls2$cor.yt

##correlation between X and U
pls2$cor.xu

##correlation between Y and U
pls2$cor.yu

##correlation between T and U
pls2$cor.tu

##vector of standardized regression coefficient(used with scaled data)
pls2$std.coefs1 

##vector of regression coefficients(used with original data)
pls2$reg.coefs



##We can now get the variables for the plot:
  
xlim = c(-.5,.5) 
ylim = c(-.5,.5)
xlab = "axis 1"
ylab = "axis 2"
main = "Circle of correlations"

x = pls2$cor.xt[,1]
y = pls2$cor.xt[,2]

Mat = pls2$cor.xt

aux = which(Mat[,'t1'] < 0.0)
aux = aux[which(Mat[aux,'t2'] < 0.0)]
Rat = Mat[aux, ]

xx = Rat[,1]
yy = Rat[,2]

M = 14
labels.trait = rownames( pls2$cor.yt)
tx = pls2$cor.yt[,1]
ty = pls2$cor.yt[,2]
## create new pl
plot(NULL, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = main, asp = 1)
##add points for markers with specific color, size and type
points(x = x, y = y,
       pch = 16,
       cex = 1,
       col = "grey")

## add arrows for each trait
arrows(x0 = rep(0, M), y0 = rep(0, M), x1 = tx, y1 = ty, length = 0.1, col = "darkblue")

# create new plot
plot(NULL, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = main, asp = 1)
##add points for markers with specific color, size and type
points(x = x, y = y,
       pch = 16,
       cex = 1,
       col = "grey")

## add arrows for each trait
arrows(x0 = rep(0, M), y0 = rep(0, M), x1 = tx, y1 = ty, length = 0.1, col = "darkblue")

## add text for each trait
text(x = tx, y = ty, labels = labels.trait, pos = c(1,1,1,1), col = "darkblue")

### add the text for markers
markers = rownames(Rat)
text(x = xx , y = yy , labels = markers, cex=0.5) 
#Mg and all the markers
pls_Mg = plsreg1(final_dataSet[, 1:841], final_dataSet[, 853, drop = FALSE], comps = 3)
# correlations plot;
plot(pls_Mg)
pls_Mg$R2
#Co and all the markers
pls_Co = plsreg1(final_dataSet[, 1:841], final_dataSet[, 854, drop = FALSE], comps = 3)
# correlations plot;
plot(pls_Co)
pls_Co$R2

#As and all the markers
pls_As = plsreg1(final_dataSet[, 1:841], final_dataSet[, 852, drop = FALSE], comps = 3)
# correlations plot;
plot(pls_As)
pls_As$R2
#K and all the markers
pls_K = plsreg1(final_dataSet[, 1:841], final_dataSet[, 843, drop = FALSE], comps = 3)
# correlations plot;
plot(pls_K) 
pls_K$R2

attach(final_dataSet)
d <- cbind(final_dataSet[,1:841], Ni,Cu,Zn,Ca,Fe)

#Zn, Ca, Cu, Ni, Fe and all the markers

##will examine it with 3 components (latent variables) by using comps=3. 
pls_new = plsreg2(d[, 1:841], d[, 842:846, drop = FALSE], comps = 3)
# correlations plot;
plot(pls_new)

#Rb, Mo, S and P and all the markers
e <- cbind(final_dataSet[,1:841], Rb, Mo, S,P )

##will examine it with 3 components (latent variables) by using comps=3. 
pls_new_2 = plsreg2(e[, 1:841], e[, 842:845, drop = FALSE], comps = 3)
# correlations plot;
plot(pls_new_2)

