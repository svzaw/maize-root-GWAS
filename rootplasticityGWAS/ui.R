#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinyalert)
library(qqman)
library(DBI)
library(stringr)
library(dbplyr)
library(readr)
library(dplyr)
library(tidytidbits)
library(DT)
library(plotly)
library(RColorBrewer)
library(shinyWidgets)
#library(shinythemes)
library(shinycssloaders)
library(slickR)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
    useShinyalert(),  # Set up shinyalert

  
  ## setup for login stuff 
                     tags$head(
  #  tags$link(rel="stylesheet", type="text/css",href="login_style.css"),
  #  tags$script(type="text/javascript", src = "md5.js"),
  #  tags$script(type="text/javascript", src = "passwdInputBinding.js"),
    tags$script(jscode),
    tags$style(HTML(" .navbar-default .navbar-brand {color: white;}"))
                     ),
        useShinyjs(),
    useShinyalert(),  

#      conditionalPanel(
# 
#     condition = "output.myUserLogged != 'TRUE'", # change cond panels != <> ==
#    # Application title
#    titlePanel("Maize Root GWAS",
#      #title=div(
#                   #      img(src="UoNcastle2.png", style="width:64px;height:64px;"),
#                    #     "Maize Root GWAS"),
#               windowTitle = paste0("Maize Root GWAS ", current_version)), 
#    hr(),       
#             uiOutput("basic_login")
# 
# 
# 
# ),

#    conditionalPanel(
#      condition = "output.myUserLogged == 'TRUE'",
      
        ## end setup for login stuff 
      
      navbarPage(   # tags$style(HTML(" .navbar-default .navbar-brand {color: cyan;}")),
        strong("Maize Root Phene GWAS: " ), id = "navbar_main", inverse = TRUE,
        windowTitle = paste0("Maize Root GWAS ", current_version), 
  tabPanel("Select a Root Phene", value = "trait", # home tab 4x action buttons to set trait reactive value
    fluidRow(column(2,
                    actionButton("help_landing_tab", "Start", 
                                 icon = icon('question-circle', "fa-2x"),
                               #  style="padding: 4px; color: #fff; background-color: #337ab7; border-color: #2e6da4"), 
                                 style="padding: 4px; color: #fff; font-family: monospace;
                                       background-color: #3F3F3F; border-color: #6B6B6B"), 
                    br()),
             column(10,
             wellPanel(style = "padding: 9px;",# background-color: white;",
             h3("Select a Maize Root Phene to Query GWAS Results:", 
                style='text-indent: 1em')#,
             )#,
             

             )),
           fluidRow(
        column(2,
            img(src = "maize_in_fieldnarrow.png", class="img-responsive center-block", style='height:70vh'),
            br()
        ),

       column(10,
     fluidRow( column(12,
                      p(strong("Four plant root architectural phenes were phenotyped in the maize",
     a(target='_blank',  href= "https://dl.sciencesocieties.org/publications/cs/abstracts/51/2/704", "Wisconsin Diversity Association Panel")," 
            with and without water deficit stress for three seasons in Arizona and without water deficit stress for 
            four seasons in South Africa. We identified several candidate genes associated with these phenes and their 
            plastic responses to stress and the environment. - Click on a root phene picture to see the GWAS result."))
     ,hr()
     )),
       column(6, 

               br(),
            # br(),
             fluidRow(style='height:30vh',
                   #   splitLayout(cellWidths = c("40%"),#style = "height:40vh;",
              column(6, 
                     h5("Lateral Length (LL)") ,
              p("Lateral Length is the average length of lateral roots on a crown root.
                 16-fold variation was observed across sites and treatments."),
               br()
              ),
              column(6,
                        actionButton("LL", #style = "height: 180px;",
                                     label = img(src = "lateral_avg_length_pic.png",  height="200", #width="180",
                                                 class="img-responsive", alt="lateral_avg_length")),br()
                     )
              ),hr(),
               fluidRow(style='height:30vh',
              column(6,
                     h5("Branching Frequency (BF)"),
              p("Branching Frequency is the average lateral root branching frequency on a crown root.
                           76-fold variation was observed across sites and treatments."),
               br()),
              column(6,
                        actionButton("BF",#style = "height: 180px;",
                                     label = img(src = "lateral_br_freq_pic.png", height="200", #width="180",  
                                                 class="img-responsive", alt="lateral_branching_freq")),br()
                   )),hr()
             ),
        column(6,
               br(),
            fluidRow(style='height:30vh',
               column(6, 
                      h5("Distance to First Lateral Root (DISTLAT)"),
               p("Distance to the first lateral root is the distance to the first lateral root from the root apex on a crown root.  
               25-fold variation was observed across sites and treatments."),
               br()),
              column(6,
                        actionButton("DISTLAT", #style = "height: 180px;",
                                     label = img(src = "lateral_dist_first_pic.png", height="200",# width="180", 
                                                 class="img-responsive", alt="lateral_dist_to_first")),br()
                     )),hr(),
            fluidRow(style='height:30vh',
               column(6, h5("Root Angle (ANGLE)"), 
               p("Angle is the angle of the youngest whorl of crown roots relative to the soil line.
                            6-fold variation was observed across sites and treatments."),
               br()),
              column(6,
                        actionButton("ANGLE", #style = "height: 180px;",
                                     label = img(src = "top_angle_pic.png", height="200", #width="180", 
                                                 class="img-responsive", alt="top_angle")), br()
              )),hr()
          # )
          # )
        
       
      #  )
       # ,
       #   column(1,
       #  #       
       #          br()#
       # ) 
       )
           )
    
    )
  ),
  
    tabPanel("Maize Root GWAS", value = "gwas_plot", 
   
tags$head(tags$style("#manhatPly{height:65vh !important;}")),# fix for dynamic plot height
      tags$style(HTML("
                      .btn-danger.btn, .btn-primary.btn, .btn-success.btn {
                      text-align: left;
                      }
                      ")),  # fix to left justiy text in required bootstrap  button types

    
    fluidRow(
        column(2,
              # h4(textOutput("phene_text_full")),
        #   wellPanel(
            actionButton("help_plot_tab", "SNPs:", icon = icon('question-circle', "fa-2x"),
                         style="padding: 4px; color: #fff; font-family: monospace;
                         background-color: #337ab7; border-color: #2e6da4"), 
            br(),
            br(),     
            selectInput("chroms", ## display all or single chromosome dataset dropdown
                        "Display Chromosome:",
                        choices = chrs,
                        width = '50%'), 
          # br(), 
          
          radioGroupButtons( ## loation choice radio gb
            inputId = "location", label = "Field Site :", 
           # choices = c("Arizona"="AZ","South Africa"= "SA", "Arizona/South Africa"="AZSA"), 
            choices = c("Arizona"="AZ","South Africa"= "SA", setNames("AZSA", full_text["AZSA"])), 
            direction = "vertical",
            justified = FALSE, status = "success",
            checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
          ),
          
           uiOutput("phene_radio"),## ui for trait choice radio gb
           uiOutput("treat_env_radio"), ## ui for treatment/pl choice radio gb

            br()
        #  )
        ),


        column(10,
#          uiOutput("treat_env_radio"), ## ui for treatment/pl choice radio gb
          wellPanel(style = "padding: 9px;", 
                    h3(textOutput("summary_text_nosnp"), style='text-indent: 1em')
                    ),

        #   wellPanel(style = "padding: 9px; background-color: white;",
        #  # h4("Click a SNP Label for further info"),
        #   h5(textOutput('message_txt')),
        #   br(),        
        #   ## Show a dynamic plotly plot 
        #   plotlyOutput("manhatPly") %>% withSpinner(color="black"),
        #   br()#,hr(),
        #   #DT::dataTableOutput("selected_var_table")
        # ),
   div(style = "margin-top:-2em", 
   verticalTabsetPanel(id = 'manhatVTP', contentWidth = 11, menuSide = "right",color = "#808080",
                       #style = "margin-top:-2em", 
          verticalTabPanel(
                    title = "Plot", icon = icon("chart-area", "fa-1x"), box_height = "39vh",
                    h5(textOutput('p_message_txt')),
          br(),        
          ## Show a dynamic plotly plot 
          plotlyOutput("manhatPly") %>% withSpinner(color="black"),
          br()
        ),
          verticalTabPanel(#fluidRow(column(12,
          title = "SNP Table", icon = icon("align-justify", "fa-1x"), box_height = "38vh",
          h5(textOutput('t_message_txt')),
          DT::dataTableOutput("snp_results_curated"), 
          br()
          #))
          # ),
          # verticalTabPanel(
          # title = "Table", icon = icon("align-justify", "fa-1x"), box_height = "25vh",
          # #DT::dataTableOutput("snp_results_cutated"), 
          # br()
          )
   )
   )
# ),
#         column(1, # temp variable tracking info
#         # textOutput("phene_text"), textOutput("loc_text"), textOutput("treat_env_text"),
#        #  shinythemes::themeSelector(),
#          br()
        )
    )
   ),
    tabPanel("Variant Context Info", value = "variant_info",

 tags$style(type = "text/css", ".nav-tabs li a{color: #003300; background-color: #e6ffe6;}"),
                fluidRow(
                  column(2,
                         actionButton("help_genes_tab", "Genes", icon = icon('question-circle', "fa-2x"),
                                      style="padding: 4px; color: #fff; font-family: monospace; 
                                      background-color: #337ab7; border-color: #2e6da4"), 

                         br()),
                  column(10,
                  wellPanel(style = "padding: 9px;", 
                  #h4(textOutput("summary_text_full"))#,
                  h3(textOutput("summary_text_full"), style='text-indent: 1em')
                    
                  #h4(textOutput("V4_Gene_Model_acc"))
                  )
                  )
                  ),
                 # br(),
              #column(1, br()),
              fluidRow(
              column(12,
                     h4(textOutput("V4_Gene_Model_acc")),
                     br(),
                 tabsetPanel(id = "info_tabs", selected = "Maize_Gene_info",#type = "pills",
                             tabPanel("SNP GWAS info", value = "SNP_GWAS_info",
                                fluidRow(
                                 #column(1, br()),
                                 column(5, br(),
                                        h4("GWAS SNP info:"),
                                        DT::dataTableOutput("selected_var_table"),
                                        br()
                                        )
                            )
                                 ),
                            tabPanel("Maize Gene info", value = "Maize_Gene_info", 
                                fluidRow(br()),
                                fluidRow(
                                column(4, style = "background-color:#f0f0f0;", br(),
                                       h4("Maize SNP Target Gene info:"),
                                       DT::dataTableOutput("selected_var_table2"),
                                       br()
                                       ),
                               # column(1, br()),
                                column(4, style = "background-color:#F8F8F8;", br(),
                                       h4("Maize SNP Target Gene links:"),
                                       DT::dataTableOutput("selected_var_table3"),
                                       br()#)
                                       ),
                                column(4, style = "background-color:#f0f0f0;",br(),
                                       h4("Maize SNP Target Gene Expression links:"),
                                       DT::dataTableOutput("selected_var_exprssn_table"),
                                       br()
                                       )
                              # selected_var_exprssn_table
                               )
                                ),
                            tabPanel("Arabidopsis Ortholog info", value = "Arabidopsis_Orth",
                                fluidRow(
                                column(8,br(),
                                       h4("Arabidopsis Orthologs:"),
                                       DT::dataTableOutput("orthologs_At_table"),
                                       br()
                                       ))
                                ),
                            tabPanel("Rice Ortholog info", value = "Rice_Orth",
                                fluidRow(
                                column(8,br(),
                                       h4("Rice Orthologs:"),
                                       DT::dataTableOutput("orthologs_Os_table"),
                                       br()
                                       ))
                                )
                            )



#                             )
              ) #column11
#              br()  )
            )
),
   #  tabPanel("Images", value = "images",
   # 
   #    fluidRow(h3("Image Slideshow: ", style='text-indent: 2em'), 
   #             h4(textOutput("ssimage_title"), style='text-indent: 4em')
   # 
   #    ),
   # 
   #    fluidRow(#style='height:55vh',
   #      column(2,br()),
   #      column(8, align="center",
   #       slickROutput("slickr", height = "60vh", width='100%') %>% #, width = "600px" ) %>% 
   #         withSpinner(color="black"),
   #       br()
   #      ),
   #      column(2, br())
   # )   
   #           
   #  ),
navbarMenu("More Information",
    tabPanel("Study Abstract", value = "background",

      fluidRow(column(2,br(),
               h3("Information: ", style='display: inline;'),
               br(),br(),br(),
               br()
               ),
               column(9,
               div(style='text-indent: 2em', 
              # 
              # h4("Study Abstract", style='text-indent: 4em'), 
              img(src="penn-state-college-logo.png", alt="Penn State Logo", style="display: inline; padding: 0px 60px 0px 10px", align="left" ),
              img(src="UoN_Single_Col_Logo_Blue_RGB.png", height="75px", alt="UoN Logo", style="display: inline;" , align="left" ),
              img(src="plant_crop_sci.png", height="75px", alt="UoN plant crop sci", style="display: inline;", align="left" )
              )),
              br(),#br(),
              br()

      ),

      fluidRow(style='height:75vh',
        column(2,#img(src = "maize_in_fieldnarrow.png", style='height:70vh'),
                 img(src = "half_root_narrow.png",align="right", style='width:50%'),#height:70vh'),
                 img(src = "maize_in_fieldnarrow.png", style='width:50%')#'height:70vh')
               #,br()br(),br()
               ),
        column(8,
               br(),
               # img(src="penn-state-college-logo.png", alt="Penn State Logo"),
               # br(),
               # p("..."),
               # br(),#br(),
               # img(src="UoN_Single_Col_Logo_Blue_RGB.png", height="75px", alt="UoN Logo"),
               # img(src="plant_crop_sci.png", height="75px", alt="UoN plant crop sci"),
               # br(),
               # p("..."),
              # br(),
               h3("Study Abstract"), p(strong(
"Root phenotypes regulate soil resource acquisition, however their genetic control and phenotypic plasticity 
are poorly understood. We hypothesized that the responses of root architectural phenes to water deficit 
(stress plasticity) and different environments (environmental plasticity) are under genetic control and 
that these loci are distinct. Root architectural phenes were phenotyped in the field in a large maize 
association panel with and without water deficit stress for three seasons in Arizona and without water 
deficit stress for four seasons in South Africa. All root phenes were plastic and varied in their response 
to drought and environment. We identified 19 and 15 candidate genes associated with root architectural 
phenes in well-watered conditions and under water deficit, respectively, in Arizona, and 13 candidate genes 
associated with root phenes in well-watered conditions in South Africa. In addition, 17 candidate genes were 
associated with stress plasticity and 5 candidate genes were associated with environmental plasticity between 
the two field sites. Few candidate genes for plasticity overlapped with those for traits expressed under each 
condition. Understanding the genetic control and fitness impacts of phenotypic plasticity will be important for 
the breeding of plants that are adapted to varying environmental conditions.")
),
               br()
        
        ),
      #  column(2, img(src = "half_root_narrow.png",align="right", style='height:70vh'),br())
column(2,#img(src = "maize_in_fieldnarrow.png", style='height:70vh'),
       img(src = "half_root_narrow_hflip.png",align="left", style='width:50%'),#height:70vh'),
       img(src = "maize_in_fieldnarrow_hflip.png", style='width:50%')#'height:70vh')
       #,br()br(),br()
    )
   )   
             
    ), #end tabpanel bg

    tabPanel("Glossary", value = "glossary",

      fluidRow(column(2,h3("Information:"),
                      br(),br(),
                      br()), #style='text-indent: 2em'), 
               column(9,br()),
              # h4("Glossary", style='text-indent: 4em'),Background information: 
               br()#,br()

      ),

      fluidRow(style='height:75vh',
        column(2,img(src = "maize_in_fieldnarrow.png", style='height:70vh'),br()),
        column(8,
               br(),
               # img(src="penn-state-college-logo.png", alt="Penn State Logo"),
               # br(),
               # p("..."),
               # br(),#br(),
               # img(src="UoN_Single_Col_Logo_Blue_RGB.png", height="75px", alt="UoN Logo"),
               # img(src="plant_crop_sci.png", height="75px", alt="UoN plant crop sci"),
               # br(),
              # p("..."),
              # br(),
               h3("Glossary"), 
               p(strong("Arizona field site:"), "At the Apache Root Biology Center (ARBC) in Willcox, Arizona (32° 153 9.252 N, 109° 49 56.928 W) plants were grown in well-watered and water-stressed conditions. Experiments were conducted on a Grabe loam (coarse-loamy, mixed, thermic Typic Torrifluvent) from May to September 2014, 2015, and 2016. Genotypes were grown in two replications per treatment in a randomized complete block design each year."),
               p(strong("South Africa field site:"), "At the Ukulima Root Biology Center (URBC) in Alma, Limpopo, South Africa (24° 33 0012 S, 28° 07 2584 E) plants were grown under non-stress conditions. Experiments were conducted on a Clovelly loamy sand (Typic Ustipsamment) from January to April in 2010, 2011, and 2012 and from November to February in 2013."),
               p(strong("Stress plasticity:"), "The responses of root architectural phenes to water deficit. Plasticity in response to water deficit was calculated as a relative value compared to control growing conditions for each phene under no stress."),
               p(strong("Environmental plasticity:"), "The responses of root architectural phenes to different environmental conditions. Environmental plasticity was calculated as a relative phenotypic value of South Africa growing conditions compared to the Arizona growing conditions for each phene."),
               p(strong("Lateral Branching Length (LL):"), "Average lateral root length on a crown root (mm)."),
               p(strong("Lateral Branching Frequency (BF):"), "Lateral branching frequency on a crown root (branches mm",tags$sup('-1'),")."),
               p(strong("Root Angle (ANGLE):"), "Angle of the youngest whorl of roots relative to the soil line (degrees)."), 
               p(strong("Distance to the First Lateral Branch (DISTLAT):"), "Distance to the first lateral root from the root apex on the excised root (mm)."), 
              #p(strong("")), p(strong("")),
              br()
        
        ),
        column(2, img(src = "half_root_narrow.png",align="right", style='height:70vh'),br())
   )   
             
    )
) #end navbarmenu
    )
# ) 
)
)



  
