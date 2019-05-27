



##Establish that this is a dashboard page



dashboardPage(
  dashboardHeader(title = "HealthCare"), ##Naming the entire page
  ##Creating The Tabs that Will Be Found in the Body
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("About Project",
               tabName = "about", 
               icon = icon("book")), 
      menuItem("Specifications & Research",
               tabName = "background",
               icon = icon("calendar")),
      menuItem("EDA",
               tabName = "eda",
               icon = icon("bar-chart-o")),
      menuItem("Regression",
               tabName = "Regression",
               icon = icon("book")),
      menuItem("Decision Tree",
               tabName = "Decision_Tree",
               icon = icon("book")),
      menuItem("Random Forest",
               tabName = "Random_Forest",
               icon = icon("book")),
      menuItem("Nueral Networks",
               tabName = "Nueral_Networks",
               icon = icon("book")),
      menuItem("SVM",
               tabName = "SVM",
               icon = icon("book")),
      menuItem("Naive Bayes",
               tabName = "Naive_Bayes",
               icon = icon("book")),
      menuItem("PCA",
               tabName = "PCA",
               icon = icon("book"))
    )
  ),
  
  ##Creating The Body of the Page
  
  dashboardBody(tabItems( ##Establishing That I want to work on the tab items
    tabItem("about", ##Calling the tab I wish to work on
            box(h1('Predicting Hospital Readmission For Diabetes Patients',align='center'),background='purple',width=24),
            box(background='light-blue', width=24,
                h3('About the Project'),
                p('This project is develped by Mrityunjay Kumar & Aastha Sharma, CBA student of ISB 2019W as a part of practicum 2'),
                br(),
                h3('Project Rationale'),
                p('The purpose of this project is to develop a predictive model which will help hospitals 
                  reduce their readmission rates among diabetic patients.'),
                p('The objective of this project is to predict whether the patient will be readmitted to the hospital or not. For predicting it 
                  most accurately I have used various prediction models like Logistic Regression, Decision Trees, Neural Networks, Random Forests, 
                  SVM and  Naïve Bayes and seen their performance in order to get the best results.'),
                br(),
                h3('Technical Approach'),
                p('Develop a model which predicts whether a patient will be readmitted in <30 days. 
                  A new diabetic readmission reduction program intervention will use this model in 
                  order to target patients at high risk for readmission. Models will be evaluated on AUC and False Positive Rates.')
                )), 
    
    tabItem("background",
            box(title = "Overview", solidHeader = TRUE, status = "primary",width=24,
                p(" The feature selection was done on the basis of PCA and the correlation analysis. 
                    Data was converted ad pre-processed by removing null values and changing categorical variables.
                  It even involved changing categorical variables using domain knowledge for diagnosis codes from ICD-9 codes."),
                p("The EDA gave us some interesting insights as the readmission is more in female people and in the Caucasian race. 
                  The overall comparison of methods on the basis of their accuracy sensitivity and specificity was generated as the output.")
     
            ),
            box(title = "Title1", solidHeader = TRUE, status = "primary",width=24,
                p('1'),
                p('- the cost of hospital readmission'),
                p('- the cost of a diabetes management program'),
                p('- the success rate of a diabetes management program at preventing a 30-day readmission'),
                p('Other studies that aimed to predict the rate of hospital readmission across patients with diabetes and/or similar
                  chronic conditions were also reviewed. These studies identified additional variables that have been shown to be useful predictors 
                  of readmission, but were unfortunately not available within the dataset.'),
                p("Examples of additional desirable socioeconomic information might include a patient's:"),
                p('- zip code'),
                p('- personal support network'),
                p('- income level'),
                p('- education level'),
                p("Examples of additional desirable health information might include a patient's:"),
                p('- weight'),
                p('- BMI'),
                p('- blood pressure levels'),
                p('- smoking habits'),
                p('- access to regular primary care')
            )
    ),
    
    tabItem("eda",
            tabBox(id = "edatabs",width = 12,
                   tabPanel(title = "Readmission",
                            fluidRow(box(width = 12,
                                         plotlyOutput("outcome"),
                                         strong('Overall Readmission Rate = 11.346%')
                            ))),
                   
                   tabPanel(title = "A1C level",
                            fluidRow(box(width = 6,
                                         plotlyOutput("A1C1")
                            ),
                            box(width = 6,
                                plotlyOutput("A1C2")
                            )
                            )),
                   
                   tabPanel(title = "Diagnoses",
                            fluidRow(box(width = 6,
                                        plotlyOutput("diagnoses1")
                            ),
                            
                            box(width = 6,
                                        plotlyOutput("diagnoses2")
                            )
                            )),
                   
                   tabPanel(title = "Days in hospital",
                            fluidRow(box(width = 6,
                            plotlyOutput("days_hospital_hist1")
                            ),
                            box(width=6,
                                plotlyOutput("days_hospital_hist2"))
                            )),
                   
                   tabPanel(title = "Num of labs",
                            fluidRow(box(width = 6,
                              plotlyOutput("num_lab_hist1")
                            ),
                            box(width = 6,
                                plotlyOutput("num_lab_hist2"))
                            )),
                   
                   tabPanel(title = "Num of meds",
                            fluidRow(box(width = 6,
                              plotlyOutput("num_meds_hist1")
                            ),
                            box(width = 6,
                                plotlyOutput("num_meds_hist2"))
                            )),
                   
                   tabPanel(title = "Age Groups",
                            fluidRow(box(width = 6,
                                         plotlyOutput("age_hist1")
                                         ),
                                     box(width= 6,
                                         plotlyOutput("age_hist2"))
                                     )),
                   
                   tabPanel(title = "Metfomin&Insulin",
                            fluidRow(box(width = 6,
                                         plotlyOutput("metformin_hist")
                                         ),
                                     box(width = 6,
                                         plotlyOutput("insulin_hist")
                                     ))),
                   
                   tabPanel(title = "Race",
                            fluidRow(box(width = 6,
                                         plotlyOutput("race_hist1")
                                         ),
                                     box(width = 6,
                                         plotlyOutput("race_hist2"))
                                     ))
            )),
    
    
    tabItem("Regression",
            fluidRow(
              column(4,
                     h4('Regression  Result',align='center'),
                     textOutput("aucplot_plot")
                     )
            )),
    
    
    tabItem("Decision Tree",
            fluidRow(
              column(4,
                     h4('Decision Tree',align='center'),
                     textOutput("aucplot_plot1")
              )
            )),
    
    
    tabItem("Random_Forest",
            fluidRow(
              column(4,
                     h4('Random_Forest',align='center'),
                     textOutput("aucplot_plot2")
              )
            )),
    
    
    tabItem("Nueral Networks",
            fluidRow(
              column(4,
                     h4('Nueral Networks',align='center'),
                     textOutput("aucplot_plot3")
              )
            )),
    
    
    tabItem("SVM",
            fluidRow(
              column(4,
                     h4('SVM',align='center'),
                     textOutput("aucplot_plot4")
              )
            )),
    
    
  
    
    tabItem("Naive Bayes",
            fluidRow(
              column(4,
                     h4('Naive Bayes',align='center'),
                     textOutput("aucplot_plot5")
              )
            )),
    
    tabItem("PCA",
            fluidRow(
              column(4,
                     h4('PCA  Result',align='center'),
                     textOutput("aucplot_plot6")
                    )
                    ))

    
    )))
  
  
  