#########################################################################################################
##
## Masters of Spin: Unravelling the web
## Designed and developed by : Tinniam V Ganesh
## Date: 23 Feb 2015
## For more details on this project see: https://gigadom.wordpress.com/
##
#########################################################################################################

setwd("C:\\software\\R\\bowling")
# Read data

if(!file.exists("plots")) {
  dir.create("plots")
}

warne = read.csv("warne.csv")
bowlingPerf(warne,"Shane Warne")

murali = read.csv("murali.csv")
bowlingPerf(murali,"M Muralitharan")

kumble = read.csv("kumble.csv")
bowlingPerf(kumble,"Anil Kumble")

# Comparing wicket percentages of spin masters
# Save the plot
#setwd("./plots")
#relwktsfreqplot <- paste("relative-wkts-frequency.jpg")
#jpeg(relwktsfreqplot)

relativePerf(murali,name,"red",flag="TRUE")
relativePerf(warne,name,"green")
relativePerf(kumble,name,"darkmagenta")
legend(x="right",c("M Muralitharan","Shane Warne","Anil Kumble"), lty=c(1,1,1),   
       lwd=c(3,3,3),col=c( "red","green", "darkmagenta"),bty="n") 
#dev.off()
#setwd("..")

#Plot the relative economy rate
# Save the plot
#setwd("./plots")
#relwkteconomyrate <- paste("relative-wkts-economyrate.jpg")
#jpeg(relwkteconomyrate)

relativeER(murali,name,"red",flag="TRUE")
relativeER(warne,name,"green")
relativeER(kumble,name,"darkmagenta")
legend(x="bottomleft",c("M Muralitharan","Shane Warne","Anil Kumble"), lty=c(1,1,1),   
       lwd=c(3,3,3),col=c( "red","green", "darkmagenta"),bty="n") 
#dev.off()
#setwd("..")
