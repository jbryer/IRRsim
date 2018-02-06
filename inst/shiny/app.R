library(shiny)
library(IRRsim)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Inter-Rater Reliability Simulation"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
      	sliderInput('nLevels', 'Number of Levels:', min = 2, max = 10, value = 3),
		sliderInput('nRaters', 'Number of Raters:', min = 2, max = 26, value = 6),
		selectInput('ICC', 'Inter-class Correlation',
					choices = c('All', 'ICC1', 'ICC2', 'ICC3', 'ICC1k', 'ICC2k', 'ICC3k')),
		# sliderInput('agreement', 'Rater Agreement', min = .05, max = 1, value = .6, step = 0.05),
		numericInput('nEvents', 'Number of Ratings:', value = 100, min = 10),
		numericInput('nSamples', 'Number of Samples:', value = 100, min = 10, max = 2000)
      ),

      # Show a plot of the generated distribution
      mainPanel(
      	tabsetPanel(
      		tabPanel("Plot", plotOutput("plot")),
      		tabPanel("Data", dataTableOutput("data")),
      		tabPanel("About", includeMarkdown('about.md'))
      	)
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
	getData <- reactive({
		test <- simulateICC(nRaters = input$nRaters,
							nLevels = input$nLevels,
							nEvents = input$nEvents,
							nSamples = ceiling(input$nSamples / 9),
							showShinyProgress = TRUE
		)
		return(test)
	})

	output$data <- renderDataTable({ round(getData(), digits = 3) },
								   options = list(pageLength = 50))

	output$plot <- renderPlot({
		test <- getData()
		if(input$ICC == 'All') {
			tests.melted <- melt(test, id.vars = c('i', 'k', 'simAgreement', 'agreement'))
			ggplot(tests.melted, aes(x = agreement, y = value)) +
				geom_point(alpha = 0.3) +
				geom_smooth(method = 'loess') +
				scale_color_hue('n Raters') +
				facet_wrap(~ variable) +
				xlim(c(0,1)) + #ylim(c(-0.25,1)) +
				xlab('Percent Agreement') + ylab('ICC') +
				ggtitle(paste0('ICC with ', input$nLevels, ' scoring levels and ',
							   input$nRaters, ' raters'))

		} else {
			ggplot(test, aes_string(x = 'agreement', y = input$ICC)) +
				geom_point(alpha = 0.3) +
				geom_smooth(method = 'loess') +
				xlim(c(0,1)) + ylim(c(-0.25,1)) +
				xlab('Percent Agreement') + ylab(input$ICC) +
				ggtitle(paste0(input$ICC, ' with ', input$nLevels, ' scoring levels and ',
							   input$nRaters, ' raters'))
		}
	})
}

# Run the application
shinyApp(ui = ui, server = server)

