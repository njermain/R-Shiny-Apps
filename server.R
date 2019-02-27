library(shiny)
library(ggplot2)
library(shinythemes)
library(DT)
library(leaflet)


pizza <- jsonlite::fromJSON('FavoriteSpots.json') %>% tidyr::unnest()


shinyServer(function(input, output, 
                     session){


  output$contour.plot<-renderPlot({
    F. = seq(0,2,0.1)# no slider
    increment = -0.5# no slider
    b.val = 3.27 # no slider
    a.mat.val = 0.0047 # no slider
    L50.val = input$L50.val.
    k.val = input$k.val.
    Linf.val = input$Linf.val.
    mort.rate = input$mort.rate.
    min.L = 10 # no slider
    
    max.L = floor(Linf.val*0.95*0.0393700787)
    
    # Length of fishery entry vector
    L = seq(( Linf.val + (abs(increment)/2)),0,by=increment)
    L = c(0,L, Linf.val, Linf.val-0.01, Linf.val+0.01, c(seq(1.1,20.1)/0.0393700787,seq(0.9,19.9)/0.0393700787))
    L = L[order(L)]
    
    # Check to ensure that the last increment is not larger than the value of  Linf.val
    if (L[which.max(L)] >= L[(which.max(L) - 1)]) {
      L = L[seq(1,(which.max(L) - 1))]}
    
    # insert flanking 177.999 and 178.001 to ensure midpoint == 178
    # This can be deleted when contour plot is constructed
    L[intersect(which(L >  Linf.val-0.001),which(L <  Linf.val+0.001))] = NaN
    L = L[is.finite(L)]
    
    # Assume that the maximum L value is 0.2 mm smaller than  Linf.val
    # Thus the maximum size is  Linf.val - 0.2 
    L[which.max(L)] = L[which.max(L)] - 0.2
    L <- sort(L)
    
    # Determine the midpoints of the size classes based on the 
    midpoint = matrix(data = 0,1,(length(L) - 1))
    for (j in 1:(length(L) - 1)) {
      midpoint[j] = sum(c(L[j],L[j + 1])/2)  }
    midpoint <- sort(midpoint)
    
    # calculate Tm
    Tm = matrix(data = 0,1,(length(L) - 1))
    for (j in 1:(length(L) - 1)) {
      Tm[j] = -(1/ k.val)*log(1 - (L[j + 1] - L[j])/( Linf.val - L[j]))}
    Tm <- c(Tm)
    
    # calculate egg number for size classes that have
    # SSB.vect <- 1/(1 -  a.val*exp(- b.val *midpoint))
    
    length.entry <- seq(min.L,max.L,by = 1)/0.0393700787
    Y.mat <- SSB.mat <- matrix(NA, nrow = length(length.entry), ncol = length(F.))
    
    for (j in 1:length(length.entry)) {
      for (k in 1:length(F.)) {
        
        fish.length.ind <- which(midpoint >= length.entry[j])
        F.vect <- rep(0,length(midpoint))
        F.vect[fish.length.ind] <- F.[k]
        mm.rate <-  mort.rate*Tm + F.vect*Tm
        N.vect <- exp(-cumsum(mm.rate))
        SSB.mat[j,k] <- sum(N.vect*midpoint^ b.val *(1 + exp(- a.mat.val*(midpoint -  L50.val)))^(-1))    
      }}
    
    SSB.mat <- SSB.mat/max(SSB.mat)*100
    
    L.entry.in <- c(length.entry*0.0393700787)
    
    contour(t(SSB.mat), 
            xlab = "Instantaneous Fishing Mortality rate (F, 1/y)",
            ylab = "Length of entry to the fishery (TL, inch)", xaxt = 'n',yaxt = 'n')
    axis(labels = as.character(seq(10,29,1)), at = seq(0,1,length.out = length(seq(10,29,1))), side = 2, las = 2)
    axis(labels = F., at = seq(0,1,length.out = length(F.)), side = 1, las = 0)
    abline(h = seq(0,1,length.out = length(length.entry)), col = "gray")
    abline(v = seq(0,1,length.out = length(F.)), col = "gray")
    contour(t(SSB.mat), 
            xlab = "Instantaneous Fishing Mortality rate (F, 1/y)",
            ylab = "Length of entry to the fishery (TL, inch)", xaxt = 'n',yaxt = 'n', add = T, lwd = 2)
    
  })
  
  plotOutput(outputId = "contour.plot")

    output$DisplayState <- renderText(input$StateSelector)
    
    output$CarHist <- renderPlot({
        ggplot(mtcars, aes_string(x=input$CarColumn)) + 
            geom_histogram() 
    })
    

})