library(shiny)



  

# Black-Scholes Function
BS <-
  function(S, K, T, r, sig, type="C"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    if(type=="C"){
      value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
    }
    if(type=="P"){
      value <- K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
    }
    return(value)
  }


## Function to find BS Implied Vol using Bisection Method
#S <- 1082.74  stock price
#T <- 28/365  time
#r <- 0.01   risk free
# K strike price
# type "C" for CALL vs P put?
#implied.vol(S, dat$K[i], T, r, dat$C.Ask[i], "C")
### S  
implied.vol <-
  function(S, K, T, r, market, type){
    sig <- 0.20
    sig.up <- 1
    sig.down <- 0.001
    count <- 0
    err <- BS(S, K, T, r, sig, type) - market 
    
    ## repeat until error is sufficiently small or counter hits 1000
    while(abs(err) > 0.00001 && count<1000){
      if(err < 0){
        sig.down <- sig
        sig <- (sig.up + sig)/2
      }else{
        sig.up <- sig
        sig <- (sig.down + sig)/2
      }
      err <- BS(S, K, T, r, sig, type) - market
      count <- count + 1
    }
    
    ## return NA if counter hit 1000
    if(count==1000){
      return(NA)
    }else{
      return(sig)
    }
  }



ProbabilityStockPriceBelow <- function (CurrentPrice, TargetPrice, VolatilityPerPeriod, TimePeriod)
{
 # return StandardNormalPx(Math.log(TargetPrice / CurrentPrice) / (VolatilityPerPeriod * Math.sqrt(TimePeriod)));
  ## in r: 1-Qx = px ??
#return (pnorm(log(TargetPrice / CurrentPrice) /  (VolatilityPerPeriod * sqrt(TimePeriod))))
return (pnorm(log(TargetPrice / CurrentPrice) /  (VolatilityPerPeriod * sqrt(TimePeriod))))
}

ProbabilityStockPriceAbove <- function (CurrentPrice, TargetPrice, VolatilityPerPeriod, TimePeriod)
{

 #	return StandardNormalQx(Math.log(TargetPrice / CurrentPrice) / (VolatilityPerPeriod * Math.sqrt(TimePeriod)));   it is Qx instead of Px
  return( 1-pnorm(log(TargetPrice / CurrentPrice) /   (VolatilityPerPeriod * sqrt(TimePeriod) ) ) )
}


### this is new shit
#####################################################################################

 BlackScholesDen1 <- function(Current, Strike, TBillRate, Volatility, FractionalYear) 
{
  return( (log(Current / Strike) + ((TBillRate + ((Volatility * Volatility) / 2)) * FractionalYear)) / (Volatility * sqrt(FractionalYear)) )
}

BlackScholesDen2 <- function(Current, Strike, TBillRate, Volatility, FractionalYear)
{ 
    return( (log(Current / Strike) + ((TBillRate - ((Volatility * Volatility) / 2)) * FractionalYear)) / (Volatility * sqrt(FractionalYear)) ) 
  }
 
 
  BlackScholesCallHedgeRatio <-function(Current, Strike, TBillRate, Volatility, FractionalYear)
{
   return (pnorm(BlackScholesDen1(Current, Strike, TBillRate, Volatility, FractionalYear)))
 }

    BlackScholesPutHedgeRatio <- function(Current, Strike, TBillRate, Volatility, FractionalYear)
{
     return( BlackScholesCallHedgeRatio(Current, Strike, TBillRate, Volatility, FractionalYear) - 1)
   }

 
#####################
BlackScholesCallValue <- function (Current, Strike, TBillRate, Volatility, FractionalYear)
{
  a <- (Current * BlackScholesCallHedgeRatio(Current, Strike, TBillRate, Volatility, FractionalYear))
   b <- (pnorm(BlackScholesDen2(Current, Strike, TBillRate, Volatility, FractionalYear)))
   d <- (Strike * exp(TBillRate * (-FractionalYear)))
  return( a - (b * d) )
}
 BlackScholesPutValue <- function(Current, Strike, TBillRate, Volatility, FractionalYear)
 {
   a <- (Current * (pnorm(-BlackScholesDen1(Current, Strike, TBillRate, Volatility, FractionalYear))))
   b <- (pnorm(-BlackScholesDen2(Current, Strike, TBillRate, Volatility, FractionalYear)))
   d <-  (Strike * exp(-TBillRate * FractionalYear))
   return ((b * d) - a)
 }


 
##############################################################################
 
optionPrice <-function(K, type, priceList, currentPrice){
  
  vola <- 27.9 /100
  tBill <- 2.16 /100
  FractionalYear <- 71 / 365.25
  
  sum <- 0
  
  for(j in 1:length(priceList)){
    
    if(type == "Call"){
     sum <- sum +  BlackScholesCallValue(priceList[j], K, tBill, vola, FractionalYear) - currentPrice
    }
    else{
      sum <- sum +  BlackScholesPutValue(priceList[j], K, tBill, vola, FractionalYear) - currentPrice
    }
  }
  
  return(sum/length(priceList))
}


#####################################################################################

#path <- "C:/Users/fteschner/Desktop/"
#prices <- read.csv(paste(path, "OptionPrices.csv", sep=""), sep="|")

prices<- read.csv("OptionPrices.csv", sep="|")

prices <- prices[which(prices$ask > 0.001),]
prices$mid <- (prices$ask +prices$bid )/2

prices$type2 <- ifelse(prices$type =="Call", "C", "P")

prices$avgPrice <- NA

### clean dataset!

prices$impliedVola <- NA
T <- 35/365
r <- 0.01
  
## implied vola seems to work!
for (i in 1:nrow(prices)){  
  prices[i,]$impliedVola <-implied.vol(prices[i,]$stockprice, prices[i,]$strike , T, r, prices[i, ]$mid , prices[i, ]$type2)
}


prices$impliedProb <- NA
## lets calc probabilities!
for (i in 1:nrow(prices)){  
  prices[i,]$impliedProb <-ProbabilityStockPriceBelow(prices[i,]$stockprice, prices[i,]$strike ,prices[i, ]$impliedVola , T)
}



giveMeBeta <- function(min, ml, max){
return(1+4*(max-ml)/(max-min))
}
giveMeAlpha <- function(min, ml, max){
return ( 1+4*(ml-min)/(max-min))
}


calculateFuturePrices <- function(current ) {

	#current <- input$decimal
   # Strike <- 400
    vola <- 27.9 /100
    tBill <- 2.16 /100
    FractionalYear <- 71 / 365.25
	
	prices$fprice <- NA
	prices$differ <- NA
	
	for (i in 1:nrow(prices)){  
			if(prices[i,]$type2=="C"){
			   prices[i,]$fprice <<- BlackScholesCallValue(current, prices[i,]$strike, tBill, vola, FractionalYear)
			}
			else{
				 prices[i,]$fprice <<- BlackScholesPutValue(current, prices[i,]$strike, tBill, vola, FractionalYear)
			}
			#prices[i,]$impliedVola <-implied.vol(prices[i,]$stockprice, prices[i,]$strike , T, r, prices[i, ]$mid , prices[i, ]$type2)
			prices[i,]$differ <<- prices[i,]$fprice - prices[i,]$mid
	}
}

# Define server logic for slider examples
shinyServer(function(input, output) {
  
  # Reactive expression to compose a data frame containing all of the values
  sliderValues <- reactive({
    
    # Compose data frame
    
  #  data.frame(
  #    Name = c("Integer", 
  #             "Decimal",
  #             "Range",
  #             "Custom Format",
  #             "Animation"),
  #    Value = as.character(c(input$integer, 
  #                           input$decimal,
  #                           paste(input$range, collapse=' '),
  #                           input$format,
  #                           input$animation)), 
  #    stringsAsFactors=FALSE)
  }) 
  
  # Show the values using an HTML table
 # output$values <- renderTable({
 #  sliderValues()
 # })
  # Show the first "n" observations
  

    output$simple <- renderText({
   
		#HTML("Current Stock Price:", prices[2,]$stockprice,"<br> Date Scraped:", prices[2,]$date_scraped, " <br> Expiration Date 2013-08-13", "<br> interest r=0.01" )
		HTML("<br> <h3> Basic Info:</h3> Current Stock Price: 452 <br> Date Scraped: 2013-06-04 17:07:37.312  <br> Expiration Date 2013-08-13 <br> Interest r=0.01" )
		#cat(as.character(el))
   })

  
  output$regression2 <- renderTable({
    #summary(out)
    if(input$n_breaks == "Alle"){
      
      summary(lm(Punkte~Tore+MW+spiele+factor(Position), data=out))
    }
    else{
      summary(lm(Punkte~Tore+MW+spiele, data=out[which(out$Position == input$n_breaks),]))
    }
  })
  

  
  
  output$prices <- renderPlot({
    
	 if(input$type == "All"){
		plot(prices$mid~prices$strike, ylab="Option Price", xlab="Strike Price")
		
	}
	if(input$type == "Calls"){
	   plot(prices[which(prices$type2=="C"),]$mid~prices[which(prices$type2=="C"),]$strike, ylab="Option Price", xlab="Strike Price")
	}
    if(input$type == "Puts"){
	   plot(prices[which(prices$type2=="P"),]$mid~prices[which(prices$type2=="P"),]$strike, ylab="Option Price", xlab="Strike Price")
	}
	
})
	  
output$vola <- renderPlot({
    
	 if(input$type == "All"){
		plot(prices$impliedVola~prices$strike, ylab="Implied Volatility", xlab="Strike Price")
		
	}
	if(input$type == "Calls"){
	   plot(prices[which(prices$type2=="C"),]$impliedVola~prices[which(prices$type2=="C"),]$strike, ylab="Implied Volatility", xlab="Strike Price")
	}
    if(input$type == "Puts"){
	   plot(prices[which(prices$type2=="P"),]$impliedVola~prices[which(prices$type2=="P"),]$strike, ylab="Implied Volatility", xlab="Strike Price")
	}
  })

  output$prob <- renderPlot({
    
	 if(input$type == "All"){
		plot(prices$impliedProb~prices$strike, ylab="Implied Probability", xlab="Strike Price")
		
	}
	if(input$type == "Calls"){
	   plot(prices[which(prices$type2=="C"),]$impliedProb~prices[which(prices$type2=="C"),]$strike, ylab="Implied Probability", xlab="Strike Price")
	}
    if(input$type == "Puts"){
	   plot(prices[which(prices$type2=="P"),]$impliedProb~prices[which(prices$type2=="P"),]$strike, ylab="Implied Probability", xlab="Strike Price")
	}
  })
	
	

output$changedPrices <- renderPlot({
    
	current <- input$decimal
   # Strike <- 400
    vola <- 27.9 /100
    tBill <- 2.16 /100
    FractionalYear <- 71 / 365.25
	
	prices$fprice <- NA
	
	for (i in 1:nrow(prices)){  
			if(prices[i,]$type2=="C"){
			   prices[i,]$fprice <- BlackScholesCallValue(current, prices[i,]$strike, tBill, vola, FractionalYear)
			}
			else{
				 prices[i,]$fprice <- BlackScholesPutValue(current, prices[i,]$strike, tBill, vola, FractionalYear)
			}
			#prices[i,]$impliedVola <-implied.vol(prices[i,]$stockprice, prices[i,]$strike , T, r, prices[i, ]$mid , prices[i, ]$type2)
	}
	
	
	 if(input$type == "All"){
		plot(prices$fprice~prices$strike)
		
	}
	if(input$type == "Calls"){
	   plot( (prices[which(prices$type2=="C"),]$fprice - prices[which(prices$type2=="C"),]$mid) / (prices[which(prices$type2=="C"),]$mid) ~prices[which(prices$type2=="C" ),]$strike)
	}
    if(input$type == "Puts"){
	   plot(prices[which(prices$type2=="P"),]$fprice~prices[which(prices$type2=="P"),]$strike)
	}
  })

 
output$differences <- renderPlot({
    
   current <- input$decimal2
   # Strike <- 400
    vola <- 27.9 /100
    tBill <- 2.16 /100
    FractionalYear <- 71 / 365.25
	
	prices$fprice <- NA
	
	for (i in 1:nrow(prices)){  
			if(prices[i,]$type2=="C"){
			   prices[i,]$fprice <- BlackScholesCallValue(current, prices[i,]$strike, tBill, vola, FractionalYear)
			}
			else{
				 prices[i,]$fprice <- BlackScholesPutValue(current, prices[i,]$strike, tBill, vola, FractionalYear)
			}
			#prices[i,]$impliedVola <-implied.vol(prices[i,]$stockprice, prices[i,]$strike , T, r, prices[i, ]$mid , prices[i, ]$type2)
	}
	
	
	if(input$dtype == "New Option Prices") {
			
			if(input$type == "All"){
				plot(prices$fprice~prices$strike,  ylab="Option Price", xlab="Strike")
				
			}
			if(input$type == "Calls"){
			   plot( (prices[which(prices$type2=="C"),]$fprice - prices[which(prices$type2=="C"),]$mid) / (prices[which(prices$type2=="C"),]$mid) ~prices[which(prices$type2=="C" ),]$strike,  ylab="Option Price", xlab="Strike")
			}
			if(input$type == "Puts"){
			   plot(prices[which(prices$type2=="P"),]$fprice~prices[which(prices$type2=="P"),]$strike,  ylab="Option Price", xlab="Strike")
			}
	}
	
	if(input$dtype == "Absolute Profit"){
	
		if(input$type == "All"){
		plot( prices$fprice - prices$mid ~ prices$strike, ylab="Absolute Profit", xlab="Strike")
		
		}
		if(input$type == "Calls"){
		   plot( (prices[which(prices$type2=="C"),]$fprice - prices[which(prices$type2=="C"),]$mid) ~prices[which(prices$type2=="C" ),]$strike, ylab="Absolute Profit", xlab="Strike")
		}
		if(input$type == "Puts"){
		   plot((prices[which(prices$type2=="P"),]$fprice - prices[which(prices$type2=="P"),]$mid) ~prices[which(prices$type2=="P"),]$strike, ylab="Absolute Profit", xlab="Strike")
		}
		
	
	}
	if(input$dtype == "Relative Profit"){
	
		 if(input$type == "All"){
		 plot( (prices$fprice - prices$mid)/prices$mid ~ prices$strike, ylab="Relative Profit", xlab="Strike")
		 }
	
		   if(input$type == "Calls"){
			   plot( (prices[which(prices$type2=="C"),]$fprice - prices[which(prices$type2=="C"),]$mid) / (prices[which(prices$type2=="C"),]$mid) ~prices[which(prices$type2=="C" ),]$strike, ylab="Relative Profit", xlab="Strike")
			}
			
			if(input$type == "Puts"){
			   plot( (prices[which(prices$type2=="P"),]$fprice - prices[which(prices$type2=="P"),]$mid) / (prices[which(prices$type2=="P"),]$mid) ~prices[which(prices$type2=="P" ),]$strike, ylab="Relative Profit", xlab="Strike")
			}
	
	
	
	}
	
  }) 
  

  
  ## given a certain probability what is the rel / abs. profit?
  
  
  ## give me a option(k) maximizing the abs/rel profit giving an estimate
  
  
  
  ################the lovely pert!
   output$PERT <- renderPlot({
    
	alpha <- giveMeAlpha(input$min, input$ml, input$max)
	beta <- giveMeBeta(input$min, input$ml, input$max)
	
	x <- rbeta(n=2000, alpha, beta)

		for(i in 1: length(x)){
		  
		  x[i] <- x[i] * (input$max - input$min) + input$min
		  
		}

	hist(x, breaks=50)
	
	
}) 

  output$PERTpara <- renderTable({
    
	alpha <- giveMeAlpha(input$min, input$ml, input$max)
	beta <- giveMeBeta(input$min, input$ml, input$max)
	
	
	op <- data.frame(matrix(nrow=1, ncol=2))
	
	colnames(op) <- c("alpha", "beta")
		op$alpha <- alpha
		op$beta <- beta
	
	#print("alpha:")
	#print(alpha)
	op
	
}) 

output$joint <- renderPlot({


			alpha <- giveMeAlpha(input$min, input$ml, input$max)
	        beta <- giveMeBeta(input$min, input$ml, input$max)
			
			 ## scale beta PERT!
			x <- rbeta(n=100, alpha, beta)
 
		for(i in 1: length(x)){
			   
			x[i] <- x[i] * (input$max - input$min) + input$min
			   
		}
			
			
		 for(i in 1:nrow(prices)) {
   
			prices[i,]$avgPrice <- optionPrice(prices[i,]$strike, prices[i,]$type, x, prices[i,]$mid)
   
		}	


		## JUST relative profits!

		if(input$type == "All"){
		 plot( (prices$avgPrice - prices$mid)/prices$mid ~ prices$strike, ylab="Relative Profit", xlab="Strike")
		 }
	
		   if(input$type == "Calls"){
			   plot( (prices[which(prices$type2=="C"),]$avgPrice - prices[which(prices$type2=="C"),]$mid) / (prices[which(prices$type2=="C"),]$mid) ~prices[which(prices$type2=="C" ),]$strike, ylab="Relative Profit", xlab="Strike")
			}
			
			if(input$type == "Puts"){
			   plot( (prices[which(prices$type2=="P"),]$avgPrice - prices[which(prices$type2=="P"),]$mid) / (prices[which(prices$type2=="P"),]$mid) ~prices[which(prices$type2=="P" ),]$strike, ylab="Relative Profit", xlab="Strike")
			}



})

  
  
  
})
