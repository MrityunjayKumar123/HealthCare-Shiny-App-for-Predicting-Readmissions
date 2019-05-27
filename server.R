
library(shiny)

# Define server logic required to draw a histogram

shinyServer(function(input, output) {
   
  demo.encounter = reactive({
    s = input$table_rows_selected
    print(as.character(f.t.p.t.[s,]))
  })
  output$dp_c = renderText({
    demo.encounter()[1]
  })
  output$de_i = renderText({
    demo.encounter()[2]
  })
  output$dp_n = renderText({
    demo.encounter()[3]
  })
  output$d_a = renderText({
    demo.encounter()[4]
  })
  output$d_r = renderText({
    demo.encounter()[5]
  })
  output$d_g = renderText({
    demo.encounter()[6]
  })
  
  ##For patient Tab 2
  h.i.encounter = reactive({
    s = input$table_rows_selected
    print(as.character(s.t.p.t.[s,]))
  })
  output$hi_at = renderText({
    h.i.encounter()[1]
  })
  output$hi_as = renderText({
    h.i.encounter()[2]
  })
  output$hi_th = renderText({
    h.i.encounter()[4]
  })
  output$hi_dd = renderText({
    h.i.encounter()[3]
  })
  output$hi_iv = renderText({
    h.i.encounter()[5]
  })
  output$hi_ov = renderText({
    h.i.encounter()[6]
  })
  output$hi_ev = renderText({
    h.i.encounter()[7]
  })
  d.t.encounter = reactive({
    s = input$table_rows_selected
    print(as.character(t.t.p.t.[s,]))
  })
  output$dt_pd = renderText({
    d.t.encounter()[1]
  })
  output$dt_a1c = renderText({
    d.t.encounter()[2]
  })
  output$dt_mgs = renderText({
    d.t.encounter()[3]
  })
  ###For secondary diagnoses
  s.d.vector = reactive({
    s = input$table_rows_selected
    prime = as.character(t.t.p.t.[s,1])
    secondary = vector(mode = "character",length = 16)
    for(i in 1:15){
      secondary[i] = ifelse(diags[s,i] == 1,catvec[i],NA)
    }
    secondary[16] = ifelse(rowSums(diabdiags[s,]) != 0,catvec[16],NA)
    secondary =  secondary[!is.na(secondary)]
    fsecondary = setdiff(secondary,prime)
    fsecondary = paste(fsecondary,collapse = ", ")
    fsecondary = ifelse(nchar(fsecondary) == 0, "No Additional Diagnoses",fsecondary)
    print(fsecondary)
  })
  output$s_d = renderText({
    s.d.vector()
  })

  #outcome-EDA
  output$outcome<- renderPlotly({
    outcome
  })
    
  #A1C-EDA 
  output$A1C1<- renderPlotly({
    A1C1
  })
  output$A1C2<- renderPlotly({
    A1C2
  })
  
  #diagnoses-EDA
  output$diagnoses1<- renderPlotly({
    diagnoses1
  })
  output$diagnoses2<- renderPlotly({
    diagnoses2
  })
  
  #days in hospital-EDA  
  output$days_hospital_hist1<- renderPlotly({
    days_hospital1
  })
  output$days_hospital_hist2<- renderPlotly({
    days_hospital2
  })
  
  # num of lab-EDA
  output$num_lab_hist1<- renderPlotly({
    num_lab1
  })
  output$num_lab_hist2<- renderPlotly({
    num_lab2
  })
  
  # num of meds
  output$num_meds_hist1<- renderPlotly({
    num_meds1
  })
  output$num_meds_hist2<- renderPlotly({
    num_meds2
  })
  
  # age-EDA
  output$age_hist1<- renderPlotly({
    age1
  })
  output$age_hist2<- renderPlotly({
    age2
  })
  
  #metformin-EDA
  output$metformin_hist<- renderPlotly({
    metformin
  })
  
  #insulin-EDA
  output$insulin_hist<- renderPlotly({
    insulin
  })
  
  #race-EDA
  output$race_hist1<- renderPlotly({
    race1
  })
  
  output$race_hist2<- renderPlotly({
    race2
  })
  
  
#user defined readmission pie chart (coord_polar not supported by Plotly)
  

  output$readmission_pie <- renderPlot({
    readmit_pie <- df_test %>%
      filter(gender %in% input$gender & race %in% input$race  & age %in% input$age &
               (time_in_hospital >= input$time) & (num_procedures >= input$numPro)) %>%
      group_by(predict_cat) %>%
      summarise(count = n()) %>%
      mutate(percent=count/sum(count)*100)
    
    readmit_pie %>%
      ggplot(aes(x = "", y = percent, fill = predict_cat)) +
      geom_bar(stat = "identity", color = "black")  +
      coord_polar(theta = "y", start=0) +
      geom_text(aes(label = paste0(predict_cat,":", round(percent), "%")),
                position = position_stack(vjust = 0.5)) +
      scale_fill_discrete(guide=FALSE) +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            plot.caption = element_text(hjust = 0.5)) +
      ggtitle("Readmission Ratio for Select Variables")
  })
  
  #user defined variable pie chart (coord_polar not supported by Plotly)
  
  output$importance_pie <- renderPlot({
    
    imp_pie <- feat_imp %>%
      filter(Feature %in% input$feature) %>%
      select(Feature, Importance) %>%
      mutate(percent=Importance/sum(Importance)*100)
    
    imp_pie %>%
      ggplot(aes(x = factor(1), y = percent, fill = Feature)) +
      geom_bar(stat = "identity", color = "black")  +
      geom_text(aes(label = paste0(Feature, ":", round(percent), "%")),
                position = position_stack(vjust = 0.5), size = 3) +
      scale_fill_discrete(guide=FALSE) +
      coord_polar(theta = "y", start=0) +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            plot.caption = element_text(hjust = 0.5)) +
      ggtitle("Variable Contribution to Readmission")
  })
})
