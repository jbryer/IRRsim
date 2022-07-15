library(shiny)
library(tidyverse)
library(cowplot)

range.mean <- 1.2
colors <- c("#1b9e77","#7570b3","#d95f02")


##### UI #######################################################################
ui <- fluidPage(
    # Application title
    titlePanel("Logistic Regression Simulation"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
        	actionButton("resample", "Resample"),
            sliderInput("groupAMean", "Group A Mean:",
            			min = -range.mean, max = range.mean,
            			value = -0.1, step = 0.05),
            sliderInput("groupAn", "Group A n",
            			min = 0, max = 500, value = 100, step = 1),
            hr(),
            sliderInput("groupBMean", "Group B Mean:",
            			min = -range.mean, max = range.mean,
            			value = 0.1, step = 0.05),
            sliderInput("groupBn", "Group B n",
            			min = 0, max = 500, value = 100, step = 1),
            hr(),
        	sliderInput("groupUnknownMean", "Group Unknown Mean:",
        				min = -range.mean, max = range.mean,
        				value = 0, step = 0.1),
        	sliderInput("groupUnknownn", "Group Unknown n",
        				min = 0, max = 500, value = 50, step = 1),
        	sliderInput("groupUnknownRatio", "Ratio of A-to-B",
        				min = 0, max = 1, value = 0.5, step = 0.1),
        	hr(),
        	sliderInput("sd", "Standard Deviation:",
        				min = 0, max = 3, value = 0.25, step = 0.05)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h4("Training Data"),
           plotOutput("distPlot"),
           hr(),
           fluidRow(
	           column(width = 4,
	           	   h5("Training Dataset"),
	           	   verbatimTextOutput("trainingDistribution")),
	           column(width = 4,
	           	   h5("Trained with full dataset"),
	           	   verbatimTextOutput("predictionResultsFull")),
	           column(width = 4,
	           	   h5("Trained with known data"),
	           	   verbatimTextOutput("predictionResultsKnown"))
           )
        )
    )
)

##### Server ###################################################################
server <- function(input, output) {
	getTrainingData <- reactive({
		input$resample
		gr1.n <- input$groupAn
		gr2.n <- input$groupBn
		grUnknown.n <- input$groupUnknownn
		gr1.mean <- input$groupAMean
		gr2.mean <- input$groupBMean
		grUnknown.mean <- input$groupUnknownMean
		gr1.sd <- gr2.sd <- grUnknown.sd <- input$sd
		df <- data.frame(
			Group = c(rep('A', gr1.n),
					  rep('B', gr2.n),
					  rep('Unknown', grUnknown.n)),
			Y = c(rep(0, gr1.n),
				  rep(1, gr2.n),
				  rep(sample(0:1, prob = c(input$groupUnknownRatio, 1 - input$groupUnknownRatio),
				  		     size = grUnknown.n, replace = TRUE))),
			X = c(
				rnorm(gr1.n, mean = gr1.mean, sd = gr1.sd),
				rnorm(gr2.n, mean = gr2.mean, sd = gr2.sd),
				rnorm(grUnknown.n, mean = grUnknown.mean, sd = grUnknown.sd)
			),
			stringsAsFactors = FALSE
		)
		return(df)
	})

	getValidationData <- reactive({
		df <- getTrainingData()
		gr1.n <- input$groupAn
		gr2.n <- input$groupBn
		grUnknown.n <- input$groupUnknownn
		gr1.mean <- input$groupAMean
		gr2.mean <- input$groupBMean
		grUnknown.mean <- input$groupUnknownMean
		gr1.sd <- gr2.sd <- grUnknown.sd <- input$sd
		df.valid <- data.frame(
			Group = c(rep('A', gr1.n),
					  rep('B', gr2.n),
					  rep('Unknown', grUnknown.n)),
			Y = c(rep(0, gr1.n),
				  rep(1, gr2.n),
				  rep(sample(0:1, prob = c(input$groupUnknownRatio, 1 - input$groupUnknownRatio),
				  		     size = grUnknown.n, replace = TRUE))),
			X = c(
				rnorm(gr1.n, mean = gr1.mean, sd = gr1.sd),
				rnorm(gr2.n, mean = gr2.mean, sd = gr2.sd),
				rnorm(grUnknown.n, mean = grUnknown.mean, sd = grUnknown.sd)
			),
			stringsAsFactors = FALSE
		)
		lr.out.full <- glm(Y ~ X, data = df, family = binomial(link = 'logit'))
		lr.out.known <- glm(Y ~ X, data = df[df$Group != 'Unknown',], family = binomial(link = 'logit'))
		df.valid$Fitted.Full <- predict(lr.out.full, newdata = df.valid)
		df.valid$Fitted.Known <- predict(lr.out.known, newdata = df.valid)
		return(df.valid)
	})

	output$distPlot <- renderPlot({
		df <- getTrainingData()

		plot.dist <- ggplot(df, aes(x = X, color = Group)) +
			geom_density() +
			scale_color_manual(values = colors) +
			theme(legend.position = 'none') +
			xlab('') + ylab('Density')

		plot.lr <- ggplot(df, aes(x = X, y = Y)) +
			geom_point(aes(color = Group), alpha = 0.4) +
			geom_smooth(method = "glm",
						method.args = list(family = "binomial"),
						se = FALSE,
						color = colors[3]) +
			geom_smooth(data = df[df$Group != 'Unknown',],
						method = "glm",
						method.args = list(family = "binomial"),
						se = FALSE,
						color = 'black') +
			scale_color_manual(values = colors) +
			theme(legend.position = 'none')

		plot_grid(plot.dist, plot.lr,
				  rel_heights = c(.3, .7), ncol = 1)
	})

	output$trainingDistribution <- renderPrint({
		df.train <- getTrainingData()
		tab.train <- table(df.train$Group, df.train$Y, useNA = 'ifany')
		print(tab.train)
	})

	output$predictionResultsFull <- renderPrint({
		df.valid <- getValidationData()
		tab.valid <- table(df.valid$Y, df.valid$Fitted.Full > 0.5, useNA = 'ifany')
		accuracy <- (tab.valid[1,1] + tab.valid[2,2]) / sum(tab.valid[1:2, 1:2])
		print(tab.valid)
		cat(paste0('\nAccuracy: ', accuracy * 100, '%'))
	})

	output$predictionResultsKnown <- renderPrint({
		df.valid <- getValidationData()
		tab.valid <- table(df.valid$Y, df.valid$Fitted.Known > 0.5, useNA = 'ifany')
		accuracy <- (tab.valid[1,1] + tab.valid[2,2]) / sum(tab.valid[1:2, 1:2])
		print(tab.valid)
		cat(paste0('\nAccuracy: ', accuracy * 100, '%'))
	})
}

##### Run the application ######################################################
shinyApp(ui = ui, server = server)
