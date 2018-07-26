library(shiny)
library(IRRsim)

# TODO: Add number of raters who score a test
Cicchetti <- c(0.4, 0.6, 0.75)

########## UI ##################################################################
ui <- fluidPage(

   # Application title
   titlePanel("Inter-Rater Reliability Simulation"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
      	sliderInput('nLevels', 'Number of Scoring Levels:', min = 2, max = 10, value = 3, round = TRUE),
		sliderInput('nRaters', 'Number of Raters:', min = 2, max = 26, value = 6, round = TRUE),
		numericInput('nEvents', 'Number of Tests per Sample:', value = 100, min = 10),
		numericInput('nSamples', 'Number of Samples:', value = 100, min = 10, max = 2000),
		actionButton("simulate", "Simulate Data"),
		hr(),
		checkboxInput('includeCicchetti', 'Include Cicchetti Guidelines'),
		uiOutput('irrSelect'),
		selectInput('predictMethod', 'Prediction Method',
					choices = c('Linear', 'Loess', 'Quadratic'), selected = "Loess"),
		hr(),
		p("Response Distribution (Weights)"),
		uiOutput('responseDistribution')
      ),

      # Show a plot of the generated distribution
      mainPanel(
      	tabsetPanel(
      		tabPanel("Plot", plotOutput("plot", height = "600px")),
      		tabPanel("Prediction",
      				 textOutput("predictionSummary"),
      				 tableOutput("predictionTable"),
      				 verbatimTextOutput("predictionOutput")
      				 ),
      		tabPanel("Data", dataTableOutput("data")),
      		tabPanel("About", includeMarkdown('about.md'))
      	)
      )
   )
)

########## Server ##############################################################
predict.agreements <- seq(0.05, 0.95, by = 0.05)
predict.interval <- "confidence"
cache <- list()

server <- function(input, output) {
	thedata <- reactiveValues(test = NULL)

	observeEvent({
		input$nLevels
		input$nRaters
		input$nEvents
		input$nSamples
	}, {
		thedata$test <- NULL
		index <- paste0(input$nRaters, input$nLevels, input$nEvents, input$nSamples)
		test <- cache[[index]]
		if(!is.null(test)) {
			thedata$test <- test
		}
	})

	observeEvent(input$simulate, {
		probs <- numeric()
		for(i in 1:input$nLevels) {
			probs[i] <- input[[paste0('prob', i)]]
		}
		test <- simulateICC(nRaters = input$nRaters,
							nLevels = input$nLevels,
							nEvents = input$nEvents,
							nSamples = ceiling(input$nSamples / 9),
							response.probs = probs,
							showShinyProgress = TRUE
		)
		thedata$test <- test
		index <- paste0(input$nRaters, input$nLevels, input$nEvents, input$nSamples)
		cache[[index]] <<- test
	})

	output$responseDistribution <- renderUI({
		inputs <- list()
		for(i in 1:input$nLevels) {
			inputs[[i]] <- sliderInput(paste0('prob', i),
									   label = LETTERS[i],
									   min = 0, max = 100,
									   value = 100/input$nLevels,
									   step = 1, round = TRUE)
		}
		return(inputs)
	})

	output$data <- renderDataTable({
		test <- as.data.frame(thedata$test)
		if(is.null(test)) { return() }
		round(test, digits = 3)
	}, options = list(pageLength = 50))

	output$irrSelect <- renderUI({
		test <- as.data.frame(thedata$test)
		if(is.null(test)) { return() }
		selectInput('ICC', 'Inter-Rater Reliability Statistic',
					choices = c('All', names(test)[9:ncol(test)]))
	})

	output$predictionTable <- renderTable({
		test <- thedata$test
		if(is.null(test)) { return() }
		if(input$ICC == 'All') {
			return(summary(test,
						   method = tolower(input$predictMethod),
						   agreements = seq(0.1, 0.90, by = 0.05)
						   )$summary)
		} else {
			return(summary(test,
						   stat = input$ICC,
						   method = tolower(input$predictMethod),
						   agreements = seq(0.1, 0.90, by = 0.05)
						   )$summary)
		}
	})

	output$predictionSummary <- renderText({
		test <- thedata$test
		if(is.null(test)) { return() }
		return(paste0("The following table provides 95% confidence intervals ",
					  "within the given percent rater agreements using ",
					  input$nRaters, " raters of ",
					  input$nEvents, " scoring events with ",
					  input$nLevels, " scoring levels."))
	})

	output$predictionOutput <- renderPrint({
		test <- thedata$test
		if(is.null(test)) { return() }
		if(input$ICC != 'All') {
			model.out <- summary(test,
								 stat = input$ICC,
								 method = tolower(input$predictMethod)
								 )$model
			print(summary(model.out))
		} else {
			cat("Select IRR stat to display the model summary.")
		}
	})

	output$plot <- renderPlot({
		req(input$predictMethod)
		test <- thedata$test
		if(is.null(test)) { return() }
		if(input$ICC == 'All') {
			p <- plot(test,
					  method = tolower(input$predictMethod))
			if(input$includeCicchetti) {
				p <- p + geom_hline(yintercept = c(0.4, 0.6, 0.75))
			}
		} else {
			p <- plot(test,
					  stat = input$ICC,
					  method = tolower(input$predictMethod))

			if(input$includeCicchetti) {
				model.out <- summary(test,
									 stat = input$ICC,
									 method = tolower(input$predictMethod)
				)$model
				newdata = data.frame(agreement = seq(0.01, 1, 0.01))
				predictions <- predict(model.out, newdata = newdata)
				tab <- data.frame(ICC = Cicchetti,
								  Agreement = sapply(Cicchetti, FUN = function(x) {
								  	min(which(predictions >= x)) / 100 }))
				for(i in length(Cicchetti)) {
					p <- p +
						geom_segment(data = tab, color = 'black', x = -Inf,
									 aes(y = ICC, yend = ICC, xend = Agreement)) +
						geom_segment(data = tab, color = 'black', y = -Inf,
									 aes(x = Agreement, xend = Agreement, yend = ICC)) +
						geom_text(data = tab, aes(x = 0, y = ICC, label = ICC),
								  color = 'black', vjust = -0.5, size = 3) +
						geom_text(data = tab, aes(x = Agreement, y = 0,
												  label = paste0(round(Agreement*100), '%')),
								  color = 'black', size = 3, hjust = -0.1)
				}
			}
		}
		return(p)
	})
}

########## Run the application #################################################
shinyApp(ui = ui, server = server)

