#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)




# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

##### set up login stuff #####
  ## comment/uncomment below to remove/add login
  # USER <- reactiveValues(Logged = FALSE)
  USER <- reactiveValues(Logged = TRUE)
  
  output$myUserLogged <- reactive({
    cat("logged???", USER$Logged, "\n")
    as.character(USER$Logged)
  })
  
  observeEvent(input$.login, {
    cat("un:", input$.username, "pw: ", input$.password, "\n")
    if (isTRUE(credentials[[input$.username]]==input$.password)){
      USER$Logged <- TRUE
      cat("IN :-)\n")
      cat(USER$Logged,"\n")
    } else {
      cat(USER$Logged,"\n")
      show("message")
      output$message = renderText("Invalid user name or password")
      delay(2000, hide("message", anim = TRUE, animType = "fade"))
    }
  })
 
    output$basic_login <- renderUI({
    if (!isTRUE(USER$Logged)) {
      fluidRow(column(width=4, offset = 4,
        wellPanel(id = "login",
          textInput(".username", "Username:"),
          passwordInput(".password", "Password:"),
          div(actionButton(".login", "Log in"), style="text-align: center;")
        ),
           HTML(paste0('    <div class="alert alert-info">',
            '<strong>This is a demo</strong> - contact andrew.warry@nottingham.ac.uk for more info', 
            '    </div>'
            )),
        textOutput("message")
      ))
    }
   })  
    
  ##### end set up login stuff #####
    
  ## trait reactive value initial setup  
 phene_rv <- reactiveVal("unselected")
 snp_rv <- reactiveVal()
   # observe event for updating the trait reactiveValue from radiogroupbuttons (manhattan page)
  observeEvent(input$phene_radio_choice,{
   phene_rv(input$phene_radio_choice) 
  })
  
     ## switch tab due to annotation click
   # observe({
   #   req(plotly_clickannot())
   # updateNavbarPage(session = session, "navbar_main", "variant_info")
   # })

  
  
  observeEvent(table_snp_selection(),{
   snp_rv(table_snp_selection()) 
   updateNavbarPage(session = session, "navbar_main", "variant_info") 
  }) 
  
  observeEvent(event_data("plotly_clickannotation", priority = "event")[["text"]],{
   #req(event_data("plotly_clickannotation", priority = "event"))
    # clean off underscore formatting added in plot_ly
    rawsnp <- gsub("<.*?>", "", event_data("plotly_clickannotation")[["text"]])
  # snp_rv(event_data("plotly_clickannotation")[["text"]]) 
   snp_rv(rawsnp) 
   updateNavbarPage(session = session, "navbar_main", "variant_info")
  }) 
  # observe events x4 for updating the trait reactiveValue from homepage activebuttons
  observeEvent(input$LL,
               {
    phene_rv('LL')
    updateNavbarPage(session, "navbar_main", "gwas_plot")
  })

  observeEvent(input$DISTLAT,
               {
    phene_rv('DISTLAT')
    updateNavbarPage(session, "navbar_main", "gwas_plot")
  })

  observeEvent(input$BF,
               {
    phene_rv('BF')
    updateNavbarPage(session, "navbar_main", "gwas_plot")
  })

 observeEvent(input$ANGLE,
               {
    phene_rv('ANGLE')
    updateNavbarPage(session, "navbar_main", "gwas_plot")
  })


 ### text outputs from variables to display in ui
 
 # maize gene name to display
 output$V4_Gene_Model_acc <- renderText({
  # req(V4_Gene_Model_acc())
 #  if(is.na(snp_rv())){
        if(!assertthat::is.string(snp_rv())) {
     return("Select a SNP from the Manhattan plot / SNP table")} else {
return(paste0("Maize Phene Candidate Gene: ", V4_Gene_Model_acc() ) )
   }
 })
 
 output$summary_text_full <- renderText({
  # req(V4_Gene_Model_acc())
   # option selection summary text with snp id
  summary_text <- c("SNP Choice: ", full_text[phene_rv()], full_text[input$location], full_text[input$treat_env], snp_rv()) #, sep = " / ")
   
  summary_text <- str_replace_na(summary_text, replacement = "-")
  
  paste(summary_text, collapse = " / ") 
 })   
 
 message_txt_raw <- reactive({
     if(phene_rv() != "unselected"){
           return('Click a SNP label for further info')
    } else {
        return('Select a Root Phene')
    }
 })
 
  output$p_message_txt <- renderText({
    # message displayed above gwas plot
     message_txt_raw()
    # if(phene_rv() != "unselected"){
    #        return('Click a SNP label for further info')
    # } else {
    #     return('Select a Root Trait')
    #            }
  })  
  
    output$t_message_txt <- renderText({
    # message displayed above curated snp DT
     message_txt_raw()
  })  
 
 
  output$summary_text_nosnp <- renderText({
    # option selection summary text
   tsns <- paste("GWAS: ", full_text[phene_rv()], full_text[input$location], full_text[input$treat_env], sep = " / ")

 }) 
 
  #trait code
 output$phene_text <- renderText({
   phene_rv()
 })
  #trait text
 output$phene_text_full <- renderText({
   full_text[phene_rv()]
 })
  #location code
  output$loc_text <- renderText({
   input$location
 })   
  # treatment...
  output$treat_env_text <- renderText({
   input$treat_env
 })    

  ## Take input variables combine and obtain db table name from csv_info df
gwas_table_selection <- reactive({
 # observe({
  req(phene_rv(), input$location, input$treat_env)
  
  info_filtered <- dplyr::filter(csv_info, loc == input$location, phene ==  phene_rv(), plasticity == input$treat_env)
# View(info_filtered)
  return(info_filtered$table_name[1])
})

  ## Take input variables filter and get appropriate snps from curated snp results df
snp_results_to_annotate <- reactive({
 # observe({
  req(phene_rv(), input$location, input$treat_env)
  snp_rv(NA)
  snp_results_to_annotate <- dplyr::filter(snp_result_info, 
                                           loc == input$location, 
                                           trait ==  phene_rv(), 
                                           plasticity == input$treat_env)
#  View(snp_results_to_annotate)
  return(snp_results_to_annotate)
})

## use location to switch btween treatment options available for the two sites 
# fix to allow plasticity text to be updated via global full_text vector 
treat_env_choices <- reactive({
   req(input$location)
   if(input$location == 'AZ'){
      #     return(c("Well Watered"="WW","Water Stress"= "WS","Environmental Plasticity"= "ENVPL"))
       #    return(c("Well Watered"="WW","Water Stress"= "WS",setNames("ENVPL", full_text["STRPL"]), setNames("ENVPL", full_text["ENVPL"])))
      return(c("Well Watered"="WW","Water Stress"= "WS",setNames("STRPL", full_text["STRPL"])))
     }
     else if(input$location == 'SA'){
          #full_text return(c("Well Watered"="WW","Environmental Plasticity"= "ENVPL"))
      # return(c("Well Watered"="WW", setNames("ENVPL", full_text["ENVPL"]) ))
       return(c("Well Watered"="WW"))
     } else {
       return(c(setNames("ENVPL", full_text["ENVPL"]) ))
     }
   
   })
  

## radioGroupButtons for treatment/pl variable
  output$treat_env_radio <- renderUI({
           radioGroupButtons(
            inputId = "treat_env", label = paste0("Select treatment / plasticity: "), 
           # choices = c("Well Watered"="WW","Water Stress"= "WS","Environmental Plasticity"= "ENVPL"), 
            choices = treat_env_choices(), # choices change depending on loc
            justified = FALSE, # change if horizontal
            direction = "vertical",
            status = "primary", #blue
            checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
          )
})

 ## radioGroupButtons for phene/trait choice (secondary to homepage action buttons)
  output$phene_radio <- renderUI({
           radioGroupButtons(
            inputId = "phene_radio_choice", label = "Select root phene :", 
            choices = c(setNames("ANGLE", full_text["ANGLE"]),
                        setNames("DISTLAT", full_text["DISTLAT"]),
                        setNames("BF", full_text["BF"]),
                        setNames("LL", full_text["LL"])
                        ),
              # "Lateral Branching Freq"="BF",
              # "Avg Lateral Length"="LL"),
            selected = phene_rv(),
            direction = "vertical",
            justified = FALSE, 
            status = "danger", #red
            checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
          )
})
  
    
 gwasResults <- reactive({  
   #req(input$GWAS_data)
   #  test_gwas <- as.data.frame(tbl(mydb, input$GWAS_data))
     req(gwas_table_selection())
     test_gwas <- loadDataFromSQLite(gwas_table_selection()) %>%
       #filter(minus_log10_P.value > 2) %>% 
       rename(`-Log10(P)` = minus_log10_P.value)
    # test_gwas <- as.data.frame(tbl(mydb, gwas_table_selection())) %>% 
                # dplyr::mutate(is_highlight = ifelse(rs %in% snp_results_to_annotate()$SNP, 
                    #                                  "yes", NA)) 
     
if(input$chroms == 'all'){ #return all
                          return(test_gwas)} 
                     else{ test_gwas_chrom <- filter(test_gwas, chrom == as.numeric(input$chroms))
                     #View(test_gwas_chrom)
                          #return data for requested chromosome
                          return(test_gwas_chrom) }
     })     

## Prepare the dataset

 ## extract gwas data for curated snps
 highlighted <- reactive({
  
  req(gwasResults())
  highlighted <- filter(gwasResults(), rs %in% snp_results_to_annotate()$SNP)
  return(highlighted)
  
})
 

## derive information for tick positions and labels for plot x axis
axisdf <- reactive({
    req(gwasResults())
axisdf <- gwasResults() %>% group_by(chrom) %>% summarize(center=( max(BPcum) + min(BPcum) ) / 2 ,
                                                       chrom_max_cum = max(BPcum))    
  # View(axisdf) 
   return(axisdf)
})    
 
## do plptly plot from gwas data   
    output$manhatPly <- renderPlotly({
      
# initiate a line shape object for threshold
# line added in layout(shapes= lines)
line <- list(
  type = "line",
  line = list(color = "silver", width = 1),
  xref = "x",
  yref = "y"
)

#View(head(gwasResults()))
#View(tail(gwasResults()))
local_gwasResults <- gwasResults()
#  make threshold dynamic
lines <- list()
  line[["x0"]] <- local_gwasResults$BPcum[1]
  line[["x1"]] <- local_gwasResults$BPcum[length(local_gwasResults$BPcum)]#2104086397
  line[c("y0", "y1")] <- 7.07
  lines <- c(lines, list(line))
  
 snp_rv <- reactiveVal()
 p <- plot_ly(local_gwasResults, x= ~BPcum, y= ~-log10(P.value), type = "scattergl",  mode = "markers", 
              #
# p <- plot_ly(gwasResults(), x= ~BPcum, y= ~-log10(P.value), type = "scattergl",  mode = "markers", 
# p <- plot_ly(gwasResults(), x= ~BPcum, y= ~minus_log10_P.value, type = "scattergl",  mode = "markers", 
                     text = ~text, hoverinfo = 'text',
          marker = list(color = ~chrom_color), showlegend = FALSE) %>% 
        layout(xaxis = list(title = "Chromosomal Location",
                            zeroline = FALSE,
                            showgrid = FALSE, #showdividers = TRUE,
                            ticktext = as.list(axisdf()$chrom), 
                            tickvals = as.list(axisdf()$center),
                            tickmode = "array"),#) %>% 
               yaxis = list(zeroline = FALSE),
               shapes = lines) %>% 
        execute_if(nrow(highlighted()) != 0, # fix to prevent default annotation if annotation table is empty
        add_annotations(x = highlighted()$BPcum,
                  y = -log10(highlighted()$P.value),
                  captureevents = TRUE, # required to allow click capture on labels
                  text = paste0('<span style="text-decoration: underline;">',
                                highlighted()$rs,
                                "</span>"), # column to use as label text (snp name)
                  font = list(color="#2A5DB0"),  #gmail style
                  #font = list(color="#2200CC"), # google style
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  arrowhead = 0,
                  arrowsize = .5,
                  ax = 20,
                  ay = -40)) %>% 
        config(edits = list(annotationTail = TRUE)) %>% # allow trmporary relocation of annotations
        #add_segments(x = 0, xend = 149773487,y = 7.07, yend = 7.07)   %>%
        #add_lines(x = ~1:149773487, y = ~7.07) %>%
        event_register('plotly_clickannotation') # probably required to allow click capture on labels
 return(p)
    }) 


  
  ##  plotly_clickannotation code 
   plotly_clickannot <- reactive({
     # event_data("plotly_clickannotation")[["text"]] # extracts snp name entry from clickannotation list
     cat(snp_rv(),"\n")
     return(snp_rv())
     })
  
   
   ## provide specific DT table row due to annotation click
  output$selected_var_table = DT::renderDataTable({
    req(plotly_clickannot())
    highlighted
    #var_dataframe_row <- filter(gwasResults(), rs == plotly_clickannot())
    var_dataframe_row <- filter(highlighted(), rs == plotly_clickannot()) %>% select(1:6) %>%
     # mutate(`-Log10(P)` = round(-log10(P.value), 2) )%>% 
     # rename(`-Log10(P)` = minus_log10_P.value) %>% 
      mutate_at(vars(P.value), funs(signif(., 4))) %>% 
      mutate_at(vars( maf, effect), funs(round(., 4))) %>% 
      rename(`P value`=P.value, `SNP ID`=rs) %>% 
      tidyr::gather("label","info") 
  
      DT::datatable(var_dataframe_row, colnames = c("",""), options = list(dom='',bSort=FALSE),
                    rownames = FALSE)
  })
 
   ## provide specific DT table row due to annotation click
  selected_SNP_row <- reactive({
   var_selected_SNP_row <- filter(snp_results_to_annotate(), SNP == plotly_clickannot()) %>% 
       select(-plasticity, -loc )
      #   select(SNP, ends_with("Model"), ends_with("Link")) %>% tidyr::gather("link_code","links", ends_with("link"))
  })

  selected_SNP_row_exprssn_links <- reactive({
         dplyr::filter(snp_result_info_exprssn, SNP == plotly_clickannot(),
                                           loc == input$location, 
                                           trait ==  phene_rv(), 
                                           plasticity == input$treat_env) %>% 
                select(-plasticity, -loc )
      #   select(SNP, ends_with("Model"), ends_with("Link")) %>% tidyr::gather("link_code","links", ends_with("link"))
  })
  
    
  V4_Gene_Model_acc <- reactive({
    req(plotly_clickannot())
    selected_SNP_row()$V4_Gene_Model[1]
    
  })
  
  
  
 # snp_results_to_annotate()
  output$snp_results_curated = DT::renderDataTable({
    req(phene_rv() != "unselected")
    req(snp_results_to_annotate())
    snp_results_to_annotate_neat <- select(snp_results_to_annotate(), 
                                           -loc, -plasticity, -trait, -Chr., -Start, -Stop,
                                           -starts_with("V3"), -ends_with("_link"))  %>% 
      mutate(maf = round(maf, 2), effect = round(effect,2))
 #        select(SNP, ends_with("Model"), ends_with("Link")) %>% tidyr::gather("link_code","links", ends_with("link"))
      #  select(-loc, -plasticity)
    local_colnames <- gsub("_"," ", names(snp_results_to_annotate_neat))
    local_colnames[2] <- 'cross group'
    DT::datatable(snp_results_to_annotate_neat, 
                  colnames = local_colnames, 
                  #extensions = 'FixedColumns',
                  options = list(dom='t', scrollX = TRUE #, 
                                # fixedColumns = list(leftColumns = 1) 
                                 ),#, autoWidth = TRUE
                  escape = FALSE, #allow html
                  rownames = FALSE, 
                  selection = "single") %>%
    DT::formatStyle(columns = names(snp_results_to_annotate_neat),  fontSize = '70%')
  }) 
  
  
 table_snp_selection <- reactive({
#observe({
    # req(snp_results_to_annotate())
     req(input$snp_results_curated_rows_selected)
    # cat(input$snp_results_curated_rows_selected)
        # View(snp_results_to_annotate())
   table_snp_selection <- dplyr::slice(isolate({snp_results_to_annotate()}), 
                      input$snp_results_curated_rows_selected ) %>% 
                      dplyr::pull(SNP)
   return(table_snp_selection[1])
 }) 
  
    
  output$selected_var_table2 = DT::renderDataTable({
    req(plotly_clickannot())
    req(selected_SNP_row())
 #  var_dataframe2_row <- filter(snp_results_to_annotate(), SNP == plotly_clickannot()) %>% 
 #        select(SNP, ends_with("Model"), ends_with("Link")) %>% tidyr::gather("link_code","links", ends_with("link"))
      #  select(-loc, -plasticity)
    
    selected_SNP_info_ltable <- selected_SNP_row() %>% select(-c(2:6), -ends_with("link")) %>% 
      mutate(Gene_loc = paste0("Chr ",Chr.,": ", Start, " - ", Stop)) %>% 
      select(-Chr.,-Start,-Stop) %>% 
      tidyr::gather("label","info")#,1:6)
    
    DT::datatable(selected_SNP_info_ltable, colnames = c("",""), options = list(dom='',bSort=FALSE),
                  escape = FALSE, #allow html
                  rownames = FALSE, selection = "none")
  })  
  output$selected_var_table3 = DT::renderDataTable({
    req(plotly_clickannot())
    
 var_dataframe3_rows <-  select(selected_SNP_row(), SNP, ends_with("Model"), ends_with("Link")) %>% 
                     tidyr::gather("link_code","links", ends_with("link")) %>% 
                     tidyr::separate(link_code, c("link_img"), extra="drop") %>% 
                     mutate(link_img = paste0("<img src='", tolower(link_img),".png' />")) %>% 
                     select(link_img, links)
    
    DT::datatable(var_dataframe3_rows, colnames = c("",""), options = list(dom='',bSort=FALSE),
                  escape = FALSE, #allow html
                  rownames = FALSE, selection = "none")
  })   

    output$selected_var_exprssn_table = DT::renderDataTable({
    req(plotly_clickannot())
    
 var_datatable_exprssn <-  select(selected_SNP_row_exprssn_links(), SNP, ends_with("Model"), ends_with("Link")) %>% 
                     tidyr::gather("link_code","links", ends_with("link")) %>% 
                     tidyr::separate(link_code, c("link_img"), extra="drop") %>% 
                     mutate(link_img = paste0("<img src='", tolower(link_img),".png' />")) %>% 
                     select(link_img, links)
    
    DT::datatable(var_datatable_exprssn, colnames = c("",""), options = list(dom='',bSort=FALSE),
                  escape = FALSE, #allow html
                  rownames = FALSE, selection = "none")
  })   
  selected_SNP_row_exprssn_links
  
  output$orthologs_At_table = DT::renderDataTable({  

    local_col_names <- colnames(biomart_orthologs_At)
    local_col_names[6] <- "<img src='barefp.png' />"
        DT::datatable(filter(biomart_orthologs_At, `Gene stable ID`== V4_Gene_Model_acc()), colnames = local_col_names,
                      options = list(dom='', pageLength = -1),
                  escape = FALSE, #allow html
                  rownames = FALSE, selection = "none")
  })
  
  output$orthologs_Os_table = DT::renderDataTable({  
    local_col_names <- colnames(biomart_orthologs_Os)
    local_col_names[6] <- "<img src='ricexpro.png' />"
        DT::datatable(filter(biomart_orthologs_Os, `Gene stable ID`== V4_Gene_Model_acc()),  colnames = local_col_names,
                      options = list(dom='', pageLength = -1),
                  escape = FALSE, #allow html
                  rownames = FALSE, selection = "none")
  })
  
    output$slickr <- renderSlickR({
    #imgs <- list.files("../slide_show/", pattern=".png", full.names = TRUE)
    slickR(imgs)
  })
    
output$ssimage_title<- renderText({
  ## extract image names for slideshow
  #  req(input$slickin)
   print(sub('\\.jpg$', '', basename(imgs[as.numeric(input$slickin) + 1])))
   #print("photo name")
  })  
  
   observeEvent(input$help_plot_tab, {
    # Show a modal when the button is pressed
    shinyalert("Manhattan Plot / SNP Table", paste0(tags$ul(style="text-align: left;", 
                                                            tags$li(style="padding-bottom: 10px;", "Select the combination of Field Site/Root Phene/Plasticity required using the buttons on the left", 
                                                                                                   " - the approptiate plot will be displayed"),
                                                             tags$li(style="padding-bottom: 10px;", "To see the stats related to a SNP, place the cursor over a plot point.\n"), 
                                                             tags$li(style="padding-bottom: 10px;", "To view data plotted for single chromosomes use the 'Display Chromosome' dropdown menu.\n"),
                                                             tags$li(style="padding-bottom: 10px;", "Click on the label of any of the annotated SNPs to get more information and links related to its gene context.\n"),
                                                             tags$li(style="padding-bottom: 10px;", "If SNP labels are overlapping it is possible to temporarily move them by click-dragging them to new positions\n\n"),
                                                             tags$li(style="padding-bottom: 10px;", "Click on the'SNP Table' tab on the right hand side to view information on all of the annotated SNPs from the current conditions\n"),
                                                             tags$li(style="padding-bottom: 10px;", "Clicking on an entry in the SNP table will also give more information and links related to its gene context.\n\n"),
                                                             tags$li(style="padding-bottom: 2px;", strong("Notes:"), " The horizontal line on the plot represents the stringency threshold used in this study (-log10(P) = 7.07).",
                                                                     " Also data related to SNPs with P-values of very low significance has been ommitted from the plots to improve loading speed")
                                         )), html = TRUE)
  }) 
   
   observeEvent(input$help_genes_tab, {
    # Show a modal when the button is pressed
    shinyalert("Gene context / Orthologs", paste0(tags$ul(style="text-align: left;", tags$li(style="padding-bottom: 10px;", "Externally linked information about the Maize gene associated with the annotated / significant SNPs,",
                                                                                             " along with externally linked information about the expression patterns of orthologs found in Arabidopsis and Rice can be found under the Tabs on this page"),
                                             tags$li(style="padding-bottom: 10px;", "Ortholog information was obtained from Ensembl-Plants / Biomart"),
                                            tags$li(style="padding-bottom: 10px;", "Links to external resources are mainly made based on the Maize V4 Gene Model accession code associated manually with the SNP"),
                                              #V4_Gene_Model
                                             tags$li(style="padding-bottom:  2px;", strong("Note:"),"Links may be missing or blank if entries corresponding to the accessions in question are missing from the linked web resources")
                                         )), html = TRUE)
  }) 

   
   observeEvent(input$help_landing_tab, {
    # Show a modal when the button is pressed
     
   #  Four plant root architectural phenes were phenotyped in the maize Wisconsin Diversity Association Panel with and without water deficit stress for three seasons in Arizona and without water deficit stress for four seasons in South Africa. We identified several candidate genes associated with these phenes and their plastic responses to stress and the environment. - Click on a root phene picture to see the GWAS result.
     
     
    shinyalert("Maize Root Phene GWAS", paste0(tags$ul(style="text-align: left;", tags$li(style="padding-bottom: 10px;", "Four plant root architectural phenes were phenotyped in the maize Wisconsin Diversity Association Panel",
                                                                                          " with and without water deficit stress for three seasons in Arizona and without water deficit stress for four seasons in South Africa"),
                                             tags$li(style="padding-bottom: 10px;", "We identified several candidate genes associated with these phenes and their plastic responses to stress and the environment"),
                                             tags$li(style="padding-bottom: 10px;", "Click on a root phene picture to see the related Manhattan plots and GWAS results"),
                                             tags$li(style="padding-bottom:  2px;", "Alternatively change to the 'Maize Root GWAS' Tab and select parameters using the buttons on the left hand side")
                                            # tags$li(style="padding-bottom: 2px;", "Note: Links may be missing or blank if entries corresponding to the accessions in question are missing from the linked web services")
                                         )), html = TRUE)
  }) 

#dbDisconnect(mydb)

#lapply(c("myUserLogged"),
#       function(x) outputOptions(output, x, suspendWhenHidden = FALSE))
outputOptions(output, "manhatPly", suspendWhenHidden = FALSE)
})



### code graveyard <<<>>>

    
  # click on scatter plot points code
  # plotly_click <- reactive({
  #     d <- event_data("plotly_click") %>% head(1L) %>% as.list()
  #     d[['x']]
  # })
  # 
  # output$selected_var_table = DT::renderDataTable({
  #   req(plotly_click())
  #   
  #   var_dataframe_row <- filter(gwasResults(), BPcum == plotly_click())
  #   DT::datatable(var_dataframe_row, options = list(dom=''))
  #   #if (is.null(d)) "Click details here (double-click to clear)" else d
  # })

  # observe({
  #        str(event_data("plotly_clickannotation")[["text"]])
  # })

# highlighted <- reactive({
#   
#   req(gwasResults())
#   highlighted <- filter(gwasResults(), is_highlight == "yes") %>% 
#                  mutate(rs_href = paste0("<a target='_self' href='./?var=",rs,"'>",rs,'</a>'))
#  View(highlighted)
#   return(highlighted)
#   
# })
 
# don_data <- reactive({
#     
# manhat_data <- gwasResults() #%>% 
# return(manhat_data)
# })
# Prepare X axis


 # output$gwas_tables <- renderUI({
 #   if(input$Centre == 'ARBC'){
 #   selectInput("GWAS_data", "GWAS experiment choice", choices = arbc_gwas)
 #     } else {selectInput("GWAS_data", "GWAS experiment choice", choices = urbc_gwas)}
 # })
 # 

# 
#     output$manhat <- renderPlotly({
# p <- ggplot(don_data(), aes(x=BPcum, y=-log10(P.value), text=text)) +
#     
#     # Show all points
#  #   geom_point( aes(color=as.factor(chrom)), alpha=0.8, size=1.3) +
#       geom_point(color=don_data()$chrom_color, alpha=0.8, size=1.3) +
# 
#  #   scale_color_manual(values = rep(c("grey", "skyblue"), 22 )) +
#     #scale_color_manual(values = brewer.pal(10, "Paired")) +
#     # custom X axis:
#     scale_x_continuous( name = "Chromosomal position", label = axisdf()$chrom, breaks= axisdf()$center ) +
#     scale_y_continuous(expand = c(0, 0) ) +     # remove space between plot area and x axis
#     ylim(0,9) +
# 
#     # Add highlighted points
#     geom_point(data=subset(don_data(), is_highlight=="yes"), color="black", size=1.4) +
# 
#     # Custom the theme:
#     theme_bw() +
#     theme(
#       legend.position="none",
#       panel.border = element_blank(),
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor.x = element_blank()
#     )
# ggplotly(p, 
#          tooltip="text", 
#          type = "scattergl")
#     })
#     
#   # output$click <- renderPrint({
#   #   d <- event_data("plotly_click")
#   #   str(d)
#   #   if (is.null(d)) "Click details here (double-click to clear)" else d
#   # })

  #  observe({ # parsing url code
  #   query <- parseQueryString(session$clientData$url_search)
  #   query1 <- paste(names(query), query, sep = "=", collapse=", ")
  #   query2 <- paste(names(query), "", sep = "=", collapse=", ")
  #   print(query2)
  #   if(query2 == "var="){
  #     #updateTabItems(session, inputId = "container", selected = "data_avail")
  #     updateNavbarPage(session = session, "navbar_main", "variant_info")
  #   }
  # })