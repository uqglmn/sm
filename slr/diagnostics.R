# regression diagnotics plot 

regression_diagnostics <- function(x, y, scale=1) {

    # set the display size
    options(repr.plot.width=12*scale, repr.plot.height=6*scale)
    par(mfrow = c(2,4), mar=c(4*scale,4*scale,3,2))

    model = lm(y~x)           # fit a straight-line model
    haty  = fitted(model)     # get fitted values, hat-y
    r     = rstandard(model)  # standardised residuals

    # first row plots:
    
    # Regression model plot - 
    plot(y~x, main="Straight-line model", font.main=1, cex.lab=1.5*scale, 
         cex.axis=1.5*scale, cex.main=1.5*scale, cex.sub=1.5*scale)  # plot
    abline(model, col=2)                                             # add a fitted regression line

    # Constance of variance: r vs x
    plot(r~x, ylab="Std residuals", 
         main="Std residuals vs x", font.main=1, cex.lab=1.5*scale, 
         cex.axis=1.5*scale, cex.main=1.5*scale, cex.sub=1.5*scale)  # plot
    abline(h=0, lty=3, col=8)
    # the next two lines add a trend-line to the plot - loess fit - locally estimated scatterplot smoothing
    new.x = seq(min(x), max(x), length.out=20)
    lines(new.x, predict(loess(r~x), newdata=data.frame(x=new.x)), col=2) # col=2 - red colour

    # Constance of variance: r vs hat-y
    plot(r~haty, xlab="Fitted values", ylab="Std residuals",
         main="Std residuals vs Fitted", font.main=1, cex.lab=1.5*scale, 
         cex.axis=1.5*scale, cex.main=1.5*scale, cex.sub=1.5*scale)  # adjust the main title
    abline(h=0, lty=3, col=8)
    # the next two lines add a trend-line to the plot - loess fit - locally estimated scatterplot smoothing
    new.haty = seq(min(haty),max(haty),length.out=20)
    lines(new.haty, predict(loess(r~haty), newdata=data.frame(haty=new.haty)), col=2)

    # histogram of r
    hist(r, main="Std residuals", font.main=1, cex.lab=1.5*scale, 
         cex.axis=1.5*scale, cex.main=1.5*scale, cex.sub=1.5*scale)  # plot
    
    # second row plots:

    # built-in statistical plots
    plot(model, font.main=1, cex.lab=1.5*scale, 
         cex.axis=1.5*scale, cex.main=1.5*scale, cex.sub=1.5*scale)  # plot
    
}
