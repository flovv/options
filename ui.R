library(shiny)



# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Apple Option Prices!"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    # Simple integer interval
   # sliderInput("integer", "Integer:", 
    #            min=0, max=1000, value=500),
    
    # Decimal interval with step value
    #sliderInput("decimal", "Decimal:", 
    #            min = 0, max = 1, value = 0.5, step= 0.1),
   
    # Specification of range within an interval
    #sliderInput("range", "Range:",
    #            min = 1, max = 1000, value = c(200,500)),
    
    # Provide a custom currency format for value display, with basic animation
    #sliderInput("format", "Custom Format:", 
    #            min = 0, max = 10000, value = 0, step = 2500,
    #            format="$#,##0", locale="us", animate=TRUE),
    
    # Animation with custom interval (in ms) to control speed, plus looping
    #sliderInput("animation", "Looping Animation:", 1, 2000, 1, step = 10, 
    #            animate=animationOptions(interval=300, loop=TRUE))
    
    
    selectInput(inputId = "type",
                label = "Option type",
                choices = c("All", "Puts", "Calls" ),
                selected = "All" ),
				
				htmlOutput("simple")
    
  ),
  
  # Show a table summarizing the values entered
  mainPanel(
    tabsetPanel(
      tabPanel("Option Prices",
    
              
               h3("Prices."),
               plotOutput("prices")
               
            

               
      ),
      
     tabPanel("Implied Volatility",
    
              
               h3("Vola."),
               plotOutput("vola")
               
            

       ),        
      
	  tabPanel("Probability",
    
              
               h3("Prob."),
               plotOutput("prob")
               
            

               
      ),
	  
	  tabPanel("Changed Prices",
			  # h3("Current Prices."),
               #plotOutput("prices"),
			  
               h3("Given a stock price in +5 days plot the future option prices (using Black Scholes)"),
			   sliderInput("decimal", "Future Stock Price:", 
               min = 350, max = 500, value = 430, step= 1),
               plotOutput("changedPrices")
			   
              # h3("Given a stock prices in +5 days (plot Differences):")
			  
			  # plotOutput("differences")
               
      ),
	  
	  	  tabPanel("Profit Calculation",
			  # h3("Current Prices."),
               #plotOutput("prices"),
			  selectInput(inputId = "dtype",
                label = "Display",
                choices = c("New Option Prices", "Absolute Profit", "Relative Profit" ),
                selected = "New Option Prices" ),
				
               h3("Given a stock price in +5 days we can plot the future option prices (using Black Scholes), calculate the absolute/relative performance for buying any option now and selling it in 5 days."),
			   sliderInput("decimal2", "Future Stock Price:", 
               min = 350, max = 500, value = 430, step= 1),
			   
               plotOutput("differences")
			   
              # h3("Given a stock price in +5 days (plot Differences):")
			  
			  # plotOutput("differences")
               
      ),
	   tabPanel("PERT",
	   
	   h3("PERT method, estimate a beta-distribution from triangle"),
	   	 sliderInput("min", "Min:", 
               min = 350, max = 500, value = 420, step= 1),
			   
		 sliderInput("ml", "Most likely:", 
               min = 350, max = 500, value = 430, step= 1),
			   
		sliderInput("max", "Max:", 
               min = 350, max = 500, value = 460, step= 1),
	   
	     plotOutput("PERT"),
		 
		 h3("Parameter Estimates for beta distribution:"),
	     tableOutput("PERTpara"),
		 
		 h3("Given the PERT distribution... calculate relative profit."),
         plotOutput("joint")
		 
	   )
	  
      
    )
  )
))
