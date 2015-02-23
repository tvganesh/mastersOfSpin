#########################################################################################################
##
## Masters of Spin: Unravelling the web
## Designed and developed by : Tinniam V Ganesh
## Date: 23 Feb 2015
## For more details on this project see: https://gigadom.wordpress.com/
##
#########################################################################################################

bowlingPerf <- function(df, name) {
  
  # Remove rows with did not bowl
  a <- df$Overs != "DNB"
  bowler <- df[a,]
  
  # Remove rows with 'TDNB' - team did not bowl
  c <- bowler$Overs != "TDNB"
  bowler <- bowler[c,]
  
  # Create a table of wickets
  wktsTable <- table(bowler$Wkts)
  
  #Convert to dataframe for easy processing
  wktsDF <- as.data.frame(wktsTable)
  
  #Remove column with "-"
  wktsDF <- wktsDF[2:nrow(wktsDF),]
  
  #Rename columns
  colnames(wktsDF) <- c("Wickets","Freq")
  
  #Calculate wickets percentage
  wktsDF$freqPercent <- (wktsDF$Freq/sum(wktsDF$Freq)) * 100
  
  # Ensure ascending rrder of wickets
  wktsDF <- wktsDF[order(as.numeric(as.character(wktsDF$Wickets))),]
  
  # Save the plot
  #setwd("./plots")
  #wktsfreqplot <- paste(name,"-wkts-frequency.jpg")
  #jpeg(wktsfreqplot)
  
  
  atitle <- paste(name,"'s", " bowling career - Wicket percentage (%) vs wickets")
  plot(as.numeric(as.character(wktsDF$Wickets)), wktsDF$freqPercent, type="o", xlab="Wickets", ylab = "Wicket percentages (%)",
      main = atitle, ylim=c(0,50),pch=15, col="blue",lwd="3")
  #dev.off()
  #setwd("..")
  
  
  # Calculate mean economy rate versus number of wickets taken. Loop for 0 to max wickets
  econRate <- NULL
  for (i in 0: max(as.numeric(as.character(bowler$Wkts)))) {
 
    # Create a vector of Economy rate  for number of wickets 'i'
    a <- bowler[bowler$Wkts == i,]$Econ
    b <- as.numeric(as.character(a))
    
    # Compute the mean economy rate by using lapply on the list 
    econRate[i+1] <- lapply(list(b),mean)
    print(econRate[i])  
    
   
  }

  #Plot the economy rate vsersus wickets
  # Save the plot
  #setwd("./plots")
  #wktseconomyplot <- paste(name,"-wkts-economy.jpg")
  #jpeg(wktseconomyplot)
  
  wkts <- c(0:max(as.numeric(as.character(bowler$Wkts))))
  atitle <- paste(name,"'s", " Bowling career - Mean economy rate (%) vs wickets")
  plot(wkts,econRate,type="o",pch=13,col="red",lwd=3,xlab="Wickets",ylab="Economy rate",main=atitle)

  #dev.off()
  #setwd("..")
}

percentWkts <- function(df,name) {
  # Remove rows with did not bowl
  a <- df$Overs != "DNB"
  bowler <- df[a,]
  
  # Remove rows with 'TDNB' - team did not bowl
  c <- bowler$Overs != "TDNB"
  bowler <- bowler[c,]
  
  # Create a table of wickets
  wktsTable <- table(bowler$Wkts)
  
  #Convert to dataframe for easy processing
  wktsDF <- as.data.frame(wktsTable)
  
  #Remove column with "-"
  wktsDF <- wktsDF[2:nrow(wktsDF),]
  
  #Rename columns
  colnames(wktsDF) <- c("Wickets","Freq")
  
  #Calculate wickets percentage
  wktsDF$freqPercent <- (wktsDF$Freq/sum(wktsDF$Freq)) * 100
  
  # Ensure ascending rrder of wickets
  wktsDF <- wktsDF[order(as.numeric(as.character(wktsDF$Wickets))),]
  wktsDF
  
}
ER <- function(df,name){
  # Remove rows with did not bowl
  a <- df$Overs != "DNB"
  bowler <- df[a,]
  
  # Remove rows with 'TDNB' - team did not bowl
  c <- bowler$Overs != "TDNB"
  bowler <- bowler[c,]
  
  econRate <- NULL
  # Calculate mean economy rate versus number of wickets taken. Loop for 0 to max wickets
  for (i in 0: max(as.numeric(as.character(bowler$Wkts)))) {
    
    # Create a vector of Economy rate  for number of wickets 'i'
    a <- bowler[bowler$Wkts == i,]$Econ
    b <- as.numeric(as.character(a))
    
    # Compute the mean economy rate by using lapply on the list 
    econRate[i+1] <- lapply(list(b),mean)
    print(econRate[i])                  
  }
  econRate
}

# Compute the overall performanceof bowlers
relativeER <- function(frame, name,color,flag=FALSE) {
  
  name ="dummy"
  
  # Remove rows with did not bowl
  a <- frame$Overs != "DNB"
  bowler <- frame[a,]
  
  # Remove rows with 'TDNB' - team did not bowl
  c <- bowler$Overs != "TDNB"
  bowler <- bowler[c,]
  
  # Get the max wickets taken by bowler
  wkts <- c(0:max(as.numeric(as.character(bowler$Wkts))))
  
  #compute mean economy rate  for the bowler
  eRate <- ER(frame,name)
  
  # Plot the Economy Rate vs Wickets
  if(flag == TRUE) {
   
    plot(wkts,eRate,type="o",pch=13,col="red",lwd=3,
           xlab="wickets",ylab="Economy rate",main="Relative economy rate")
  }
  lines(wkts,eRate,col=color,lwd=3.0)
  
}

# Compute the overall performanceof bowlers
relativePerf <- function(frame, name,color,flag=FALSE) {

  name ="dummy"
  #compute percentage wickets for the bowler
  pWkts <- percentWkts(frame,name)
  
  # Plot the bowling performances
  if(flag == TRUE) {
    
    plot(as.numeric(as.character(pWkts$Wickets)), pWkts$freqPercent, type="o", xlab="Wickets", ylab = "Wicket percentages (%)",
         main = "Relative wickets percentage", ylim=c(0,50),pch=15, col=color,lwd="3")
  }
  lines(as.numeric(as.character(pWkts$Wickets)), pWkts$freqPercent,col=color,lwd=3.0)
  
}


