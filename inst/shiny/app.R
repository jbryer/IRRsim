library(shiny)
library(IRRsim)

# If true, simulations will be saved in the IRRsimShinyCache.rda file to be
# reused across application runs.
SAVE_RDA_CACHE <- TRUE

data(IRRguidelines)

########## UI ##################################################################
ui <- fluidPage(

   # Application title
   titlePanel("Inter-Rater Reliability Simulation"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
      	style = "height: 90vh; overflow-y: auto;",
      	sliderInput('nLevels', 'Number of Scoring Levels:',
      				min = 2, max = 10, step = 1, value = 4, round = TRUE),
		sliderInput('nRaters', 'Number of Raters:',
					min = 2, max = 500, value = 10, step = 1, round = TRUE),
		uiOutput('ratersPerEvent'),
		numericInput('nEvents', 'Number of Scoring Events per Matrix:',
					 value = 100, min = 10),
		numericInput('nSamples', 'Number of Scoring Matrices:',
					 value = 100, min = 10, max = 2000),
		checkboxInput('parallel', 'Simulate data in parallel', value = TRUE),
		actionButton("simulate", "Simulate Data"),
		hr(),
		selectInput('includeGuidelines', 'Include Guideline:',
					choices = c('None', names(IRRguidelines)[order(names(IRRguidelines))])),
		uiOutput('irrSelect'),
		selectInput('predictMethod', 'Prediction Method',
					choices = c('Linear', 'Loess', 'Quadratic'), selected = "Quadratic"),
		hr(),
		p('Response Distribution (Weights)'),
		uiOutput('responseDistribution')
      ),

      # Show a plot of the generated distribution
      mainPanel(
      	titlePanel("Inter-Rater Reliability Simulation"),
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

if(SAVE_RDA_CACHE & file.exists('IRRsimShinyCache.rda')) {
	load('IRRsimShinyCache.rda')
}

server <- function(input, output) {
	thedata <- reactiveValues(test = NULL)

	getCacheIndex <- reactive({
		if(is.null(input[['prob1']])) { # Response distributions haven't been loaded yet
			return('NA')
		}
		probs <- numeric()
		for(i in 1:input$nLevels) {
			probs[i] <- input[[paste0('prob', i)]]
		}
		index <- paste0(
			'k=', input$nRaters, ';',
			'k_per_event=', input$nRatersPerEvent, ';',
			'nLevels=', input$nLevels, ';',
			'nEvents=', input$nEvents, ';',
			'nSamples=', input$nSamples, ';',
			'Responses=', paste0(probs, collapse = ',')
		)
		return(index)
	})

	observeEvent({
		input$nLevels
		input$nRaters
		input$nRatersPerEvent
		input$nEvents
		input$nSamples
	}, {
		thedata$test <- NULL
		test <- cache[[getCacheIndex()]]
		if(!is.null(test)) {
			thedata$test <- test
		}
	})

	observeEvent(input$simulate, {
		probs <- numeric()
		for(i in 1:input$nLevels) {
			probs[i] <- input[[paste0('prob', i)]]
		}
		test <- simulateIRR(nRaters = input$nRaters,
							nRatersPerEvent = input$nRatersPerEvent,
							nLevels = input$nLevels,
							nEvents = input$nEvents,
							nSamples = ceiling(input$nSamples / 9),
							response.probs = probs,
							showShinyProgress = TRUE,
							parallel = input$parallel
		)
		thedata$test <- test
		cache[[getCacheIndex()]] <<- test
		if(SAVE_RDA_CACHE) {
			save(cache, file = 'IRRsimShinyCache.rda')
		}
	})

	output$ratersPerEvent <- renderUI({
		sliderInput('nRatersPerEvent', 'Number of Raters per Event:',
					min = 2, max = input$nRaters, step = 1,
					value = 2, round = TRUE)
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
					choices = c('All', names(test)[10:ncol(test)]),
					selected = input$ICC)
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
					  input$nRatersPerEvent, " raters from ",
					  input$nRaters, " available raters of ",
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
			if(input$includeGuidelines != 'None') {
				p <- p + geom_hline(yintercept = IRRguidelines[[input$includeGuidelines]]$breaks)
			}
		} else {
			p <- plot(test,
					  stat = input$ICC,
					  method = tolower(input$predictMethod))

			if(input$includeGuidelines != 'None') {
				model.out <- summary(test,
									 stat = input$ICC,
									 method = tolower(input$predictMethod)
				)
				newdata = data.frame(agreement = seq(min(model.out$data$agreement),
													 max(model.out$data$agreement),
													 0.01))
				predictions <- predict(model.out$model, newdata = newdata)
				guideline <- IRRguidelines[[input$includeGuidelines]]$breaks[-1]
				tab <- data.frame(ICC = guideline,
								  Agreement = sapply(guideline, FUN = function(x) {
								  	min(newdata$agreement[predictions >= x]) }))
				tab$ICCLabel <- paste0(tab$ICC, ' "', row.names(tab), '"')
				for(i in length(guideline)) {
					p <- p +
						geom_segment(data = tab, color = 'black', x = -Inf,
									 aes(y = ICC, yend = ICC, xend = Agreement)) +
						geom_segment(data = tab, color = 'black', y = -Inf,
									 aes(x = Agreement, xend = Agreement, yend = ICC)) +
						geom_text(data = tab, aes(x = 0, y = ICC, label = ICCLabel),
								  color = 'black', vjust = -0.5, hjust = 'left', size = 3) +
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

