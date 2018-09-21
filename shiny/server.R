#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny); library(ggplot2)

# Load data
data1 <- read.csv('data/SV_DND_long_web.csv')
data1$nstudy <- 2
data2 <- read.csv('data/only_DND_long_web.csv')
data2$Study <- 'DND'
data2$nstudy <- 1
data2 <- data2[which(data2$ROI != 'white_matter'),]
data <- rbind(data1, data2)
rm(data1,data2)
pics <- read.csv('data/brainPics.csv')
data$Age2 <- data$Age*data$Age
data$Study_Sex <- interaction(data$Study, data$Sex)

calcPercChng <- function(data, roi) {
  d0 <- data[which(data$name == roi),]
  m1 <- lm(BPnd ~ Age, data = d0)
  est_20 <- m1$coefficients[1] + 20*m1$coefficients[2]
  est_30 <- m1$coefficients[1] + 30*m1$coefficients[2]
  percChange <- round((((est_30 - est_20) / est_20)*100)[[1]], 2)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$roiPlot <- renderPlot({
    
    # determine roi- input$roi from ui.R
    roi <- input$roi 
    data <- data[which(data$name == roi),]
    # draw the graph with the specified fits displayed
    baseplot <- ggplot(data, aes(x=Age, y=BPnd)) + geom_point(aes(shape = Study_Sex)) + theme_bw() + 
      scale_shape_manual(values=c(0,1,15,16))
    if (input$linear & input$quadratic) {
      baseplot + geom_smooth(method=lm, colour = 'red', fill = 'red') +
        geom_smooth(method=lm, formula = y ~ x + I(x^2), colour = 'blue', fill = 'blue')
    } else if (input$linear) {
      baseplot + geom_smooth(method=lm, colour = 'red', fill = 'red')
    } else if (input$quadratic) {
      baseplot + geom_smooth(method=lm, formula = y ~ x + I(x^2), colour = 'blue', fill = 'blue')
    } else {
      baseplot
    }
    
  })
  output$ntext <- renderText ({
    # determine roi- input$roi from ui.R
    roi <- input$roi
    data <- data[which(data$name == roi),]
    # calculate sample size
    n <- nrow(data) -sum(is.na(data$BPnd))
    paste0('N = ', n)
  })
  output$percChangeText <- renderText ({
    # determine roi- input$roi from ui.R
    roi <- input$roi
    data <- data[which(data$name == roi),]
    # calc perc change 
    perc <- calcPercChng(data,roi)
    paste0("The ", roi, " shows ", perc, "% change in binding potential per decade.")
  })
  output$linearText <- renderText({
    # determine roi- input$roi from ui.R
    roi <- input$roi 
    data <- data[which(data$name == roi),]
    #fit linear and quadratic models and return anova table for the models checked
    if(data$nstudy == 2) {
      linmodel <- lm(BPnd ~ Age + Sex + Study, data = data)  
    } else {
      linmodel <- lm(BPnd ~ Age + Sex, data = data)
    }
    
    linci <- confint(linmodel)
    ifelse(input$linear, paste0(" - Linear slope: ", as.character(signif(as.table(linmodel$coefficients)[2], 2)), " [ ", 
                                as.character(signif(linci[2], 2)), " , ", as.character(signif(linci[4], 2)), " ] 95% CI from linear model."
                                ), "" )
  })
  
  output$quadText <- renderText({
    # determine roi- input$roi from ui.R
    roi <- input$roi
    data <- data[which(data$name == roi),]
    #fit linear and quadratic models and return anova table for the models checked
    if (data$nstudy == 2) {
      quadmodel <- lm(BPnd ~ Age2 + Age + Sex + Study, data = data)
    } else {
      quadmodel <- lm(BPnd ~ Age2 + Age + Sex, data = data)
    }
    quadci <- confint(quadmodel)
    ifelse(input$quadratic, paste0(" - Quadratic coefficient: ", as.character(signif(as.table(quadmodel$coefficients)[2], 2)), 
                                   " [ ", as.character(signif(quadci[2], 2)), " , ", as.character(signif(quadci[4], 2)), 
                                   " ] 95% CI from linear + quadratic model."), "")
  })
  output$anovaText <- renderText({
    # determine roi- input$roi from ui.R
    roi <- input$roi
    data <- data[which(data$name == roi),]
    if(data$nstudy == 2) {
      baseline <- lm(BPnd ~ Study + Sex, data = data)
      linmodel <- lm(BPnd ~ Age + Study + Sex, data = data)
      quadmodel <- lm(BPnd ~ Age + Age2 + Study + Sex, data = data)
    } else {
      baseline <- lm(BPnd ~ Sex, data = data)
      linmodel <- lm(BPnd ~ Age + Sex, data = data)
      quadmodel <- lm(BPnd ~ Age + Age2 + Sex, data = data)
    }
    atable <- anova(baseline,linmodel, quadmodel)
    f <- summary(baseline)$fstatistic
    pf<- pf(f[1], f[2],f[3],lower.tail=F)
    #ifelse(atable$`Pr(>F)`[2] <= 0.05, 'The quadratic model is a better fit.', 'The linear model is a better fit.')
    if (atable$`Pr(>F)`[3] <= 0.05) {
      pvalue <- ifelse(atable$`Pr(>F)`[3] < 0.001, '< .001', paste0('= ', as.character(round(atable$`Pr(>F)`[3],3))))
      paste0(" - The linear + quadratic model (", as.character(quadmodel$call)[2], ") is the best fit for this ROI, p ", pvalue, ". ")
    } else if (atable$`Pr(>F)`[2] <= 0.05) {
      pvalue <- ifelse(atable$`Pr(>F)`[2] < 0.001, '< .001', paste0('= ', as.character(round(atable$`Pr(>F)`[2],3))))
      paste0(" - The linear model (", as.character(linmodel$call)[2], ") is the best fit for this ROI, p ", pvalue, ".")
    } else if (pf[[1]] <= .05) {
      pvalue <- ifelse(pf[[1]] < 0.001, '< .001', paste0('= ', as.character(round(pf[[1]],3))))
      paste0(" - The baseline model (", as.character(baseline$call)[2], ") is the best fit for this ROI, p ", pvalue, ".")
    } else {
      " - None of the models fit this ROI."
    }
  })
  output$image1 <- renderImage ({
    if (is.null(input$roi)) {
      return(NULL)
    }
    roi <- input$roi
    pics <- pics[which(pics$rois == roi),]
    return(list(
      src = paste0('www/', as.character(pics$png)),
      contentType = "image/png", 
      alt = 'This should be a picture of the region of interest!'#,
      #height = 409, width = 297
    ))
  }, deleteFile = FALSE)
  
  output$baseline <- renderPrint ({
    # determine roi- input$roi from ui.R
    roi <- input$roi
    data <- data[which(data$name == roi),]
    if (data$nstudy == 2) {
      baseline <- lm(BPnd ~ Study + Sex, data = data)
    } else {
      baseline <- lm(BPnd ~Sex, data = data)
    }
    summary(baseline)
  })
  output$linear <- renderPrint ({
    #determine roi- input$roi from ui.R
    if (input$linear) {
    roi <- input$roi
    data <- data[which(data$name == roi),]
    if (data$nstudy == 2) {
      linear <- lm(BPnd ~ Study + Sex + Age, data = data)
    } else {
      linear <- lm(BPnd ~ Sex + Age, data = data)
    }
    summary(linear)
    }
  })
  output$quadratic <- renderPrint ({
    # determine roi- input$roi from ui.R
    if (input$quadratic) {
      roi <- input$roi
      data <- data[which(data$name == roi),]
      if (data$nstudy == 2) { 
        quadratic <- lm(BPnd ~ Study + Sex + Age + Age2, data = data)
      } else {
        quadratic <- lm(BPnd ~ Sex + Age + Age2, data = data)
      }
      summary(quadratic)
    }
  })
  output$atlas <- renderText({
    "Hammers, A., Allom, R., Koepp, M. J., Free, S. L., Myers, R., Lemieux, L., … Duncan, J. S. (2003). Three-dimensional maximum probability atlas of the human brain, with particular reference to the temporal lobe. Human Brain Mapping, 19(4), 224–247. https://doi.org/10.1002/hbm.10123
  \nGousias, I. S., Rueckert, D., Heckemann, R. A., Dyet, L. E., Boardman, J. P., Edwards, A. D., & Hammers, A. (2008). Automatic segmentation of brain MRIs of 2-year-olds into 83 regions of interest. NeuroImage, 40(2), 672–684. https://doi.org/10.1016/j.neuroimage.2007.11.034"
  })
  output$anatroi <- renderText({
    "3D ROI Visualization: \n\nMadan, C. R. (2015). Creating 3D visualizations of MRI data: A brief guide. F1000Research, 466, 1–13. https://doi.org/10.12688/f1000research.6838.1"
  })
})
