
require(plotly)
require(ggrepel)
require(lubridate)
require(utc)
require(ggformula)
require(zoo)
require(purrr)
require(reticulate)
require(data.table)
require(shiny)
require(shinydashboard)
require(DT)
require(shinyWidgets)
require(stringr)
require(shinyFiles)

options(shiny.maxRequestSize = 30*1024^2)





# Define server logic 
shinyServer(function(input, output) {

    # Set up empatica reading functions in the global environment.
    read.empatica.eda <- function(file){
        raw <- read.csv(file,header = F)
        start_s <- raw$V1[1]
        sample_rate <- as.numeric(raw$V1[2])
        data <- data.frame(Timestamp=NA, EDA=raw$V1[3:length(raw$V1)])
        start <- as.POSIXct(start_s, origin = "1970-01-01")
        dt <- as.difftime(as.character(1.0/sample_rate),format = "%OS")
        timestamps <- seq(from = start, by=dt , along.with = data$EDA) 
        data$Timestamp <- timestamps
        data
    }
    read.empatica.acc <- function(file){
        raw <- read.csv(file,header = F)
        start_s <- raw$V1[1]
        sample_rate <- as.numeric(raw$V1[2])
        data <- data.frame(X=raw$V1[3:length(raw$V1)]/64.0,Y=raw$V2[3:length(raw$V2)]/64.0,Z=raw$V3[3:length(raw$V3)]/64.0)
        start <- as.POSIXct(x = start_s, origin = "1970-01-01")
        dt <- as.difftime(as.character(1.0/sample_rate),format = "%OS")
        timestamps <- seq(from = start, by=dt , along.with = data$X) 
        data$Timestamp <- timestamps
        data
    }
    read.empatica.bvp <- function(file){
        raw <- read.csv(file,header = F)
        start_s <- raw$V1[1]
        sample_rate <- as.numeric(raw$V1[2])
        data <- data.frame(BVP=raw$V1[3:length(raw$V1)])
        start <- as.POSIXct(x = start_s, origin = "1970-01-01")
        dt <- as.difftime(as.character(1.0/sample_rate),format = "%OS")
        timestamps <- seq(from = start, by=dt , along.with = data$BVP) 
        data$Timestamp <- timestamps
        data
    }
    read.empatica.hr <- function(file){
        raw <- read.csv(file,header = F)
        start_s <- raw$V1[1]
        start <- as.POSIXct(x = start_s,origin = "1970-01-01")
        dt <- as.difftime(seq_along(raw$V1[2:length(raw$V1)]),units = "secs")
        timestamps <- start+dt
        HR <- as.numeric(raw$V1[2:length(raw$V1)])
        data <- data.frame(Timestamp=timestamps, HR=HR)
        data
    }
    read.empatica.ibi <- function(file){
        raw <- read.csv(file,header = F)
        start_s <- raw$V1[1]
        start <- as.POSIXct(x = start_s,origin = "1970-01-01")
        dt <- as.difftime(raw$V1[2:length(raw$V1)],units = "secs")
        timestamps <- start+dt
        ibi <- as.double(as.character(raw$V2[2:length(raw$V2)]) )
        data <- data.frame(Timestamp=timestamps, IBI=ibi)
        data
    }
    read.empatica.temp <- function(file) {
        raw <- read.csv(file,header = F)
        start_s <- raw$V1[1]
        sample_rate <- as.numeric(raw$V1[2])
        temperatureF <- (9.0/5.0)*(raw$V1[3:length(raw$V1)] + 32.0)
        data <- data.frame(TEMP=temperatureF)
        start <- as.POSIXct(x = start_s, origin = "1970-01-01")
        dt <- as.difftime(as.character(1.0/sample_rate),format = "%OS")
        timestamps <- seq(from = start, by=dt , along.with = data$TEMP) 
        data$Timestamp <- timestamps
        data
    }
    read.empatica <- function(path) {
        FIELDS <- c("Z","Y","X","Battery","Temperature","EDA")
        FILES <- c("ACC.csv","BVP.csv","EDA.csv","TEMP.csv","IBI.csv")
        #First lets read the file header info
        acc <- read.empatica.acc(file.path(path,"ACC.csv"))
        bvp <- read.empatica.bvp(file.path(path,"BVP.csv"))
        ibi <- read.empatica.ibi(file.path(path,"IBI.csv"))
        temp <- read.empatica.temp(file.path(path,"TEMP.csv"))
        eda <- read.empatica.eda(file.path(path,"EDA.csv"))
        
        data <- list(ACC=acc,BVP=bvp,EDA=eda,TEMP=temp,IBI=ibi)
        attr(data,"class") <- "eda"
        return(data)
    }
    
    # Code to set up directory system
    
    shinyDirChoose(
        input,
        'dir',
        roots = c(home = '~'),
        filetypes = c("zip", "csv")
    )
    
    global <- reactiveValues(datapath = getwd())
    
    dir <- reactive(input$dir)
    
    output$direct <- renderText({
        global$datapath
    })
    
    # if there's an input to the directory, list the path of all the files.
    
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                     input$dir
                 },
                 handlerExpr = {
                     if (!"path" %in% names(dir())) return()
                     home <- normalizePath("~")
                     global$datapath <-
                         file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                 })
        # Find the EDA.csv files amongst all the paths in the list of global$datapaths
        # Electrodermal Activity ~ Emotional Response
        gitDataEDA_open <- reactive({
           target_dir = global$datapath
           extract_file <- "EDA.csv"
           list.files(target_dir, full.names=TRUE) %>% 
               keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
               map_df(function(x) {
                   td <- tempdir()
                   read.empatica.eda(unzip(x, extract_file, exdir=td))
               }, .id = "file_name") -> combined_eda
           combined_eda %>% rename(value = EDA)
        })
        
        gitDataEDA <- reactive({
            if ("EDA" %in% req(input$med)){
                gitDataEDA_open()}
            
        })
        
        
        
        
        # Find the TEMP.csv files amongst all the paths in the list of global$datapaths
        # Skin Temperature
        gitDataTEMP <- reactive({
            if ("TEMP" %in% req(input$med)){
            target_dir = global$datapath
            extract_file <- "TEMP.csv"
            list.files(target_dir, full.names=TRUE) %>% 
                keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
                map_df(function(x) {
                    td <- tempdir()
                    read.empatica.temp(unzip(x, extract_file, exdir=td))
                }, .id = "file_name") -> combined_temp
            combined_temp <- combined_temp %>% rename(value = TEMP)
            combined_temp}
        })
        
        # Find the IBI.csv files amongst all the paths in the list of global$datapaths
        
        # Inter-beat Interval
        gitDataNULL <- reactive({
            if ("IBI" %in% req(input$med)){
                target_dir = global$datapath
                extract_file <- "IBI.csv"
                list.files(target_dir, full.names=TRUE) %>% 
                    keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
                    map_df(function(x) {
                        td <- tempdir()
                        read.empatica.ibi(unzip(x, extract_file, exdir=td))
                    }, .id = "file_name") -> ibi
                
            # Then, transform into RMSSD based on the function below
            ibidiffs <- diff(ibi$IBI)
            value <- sqrt(movingaves((ibidiffs^2), 30))
            frame2 <- ibi[-c(1:30),]
            frame2 <- cbind(frame2, value)
            frame2}
            }) 
        
        gitDataIBI <- reactive({
            if ("IBI" %in% req(input$med)){
                target_dir = global$datapath
                extract_file <- "IBI.csv"
                list.files(target_dir, full.names=TRUE) %>% 
                    keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
                    map_df(function(x) {
                        td <- tempdir()
                        read.empatica.ibi(unzip(x, extract_file, exdir=td))
                    }, .id = "file_name") -> interbeatinterval
                }
            
            })
        
        gitDataHRV <- reactive({
            if ("IBI" %in% req(input$med)){
                use_condaenv("r-reticulate")
                numpy <- import('numpy')
                pandas <- import('pandas')
                source_python("/Users/nickcardamone/Downloads/HRV_RMSSD (1).py")
                
        target_dir = global$datapath
        extract_file <- "IBI.csv"
        list.files(target_dir, full.names=TRUE) %>% 
        keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
        map_df(function(x) {
        td <- tempdir()
        pandas$read_csv(unzip(x, extract_file, exdir=td))
        }, .id = "file_name") -> ibi
    
        timemap <- as.numeric(gitDataEDA_open()$Timestamp)
        IBI <- process_IBI(ibi[,-1])
        RM <- rmssd(IBI, timemap, window = 120)  
        RM$Timestamp <- as.POSIXct(RM$UNIX, origin = "1970-01-01")
        
        final_RMSSD <- left_join(RM, gitDataEDA_open(), "Timestamp")
        final_RMSSD$value <- final_RMSSD$HRV
        final_RMSSD
        
            }
            
                
                
            
        })
        
        
        
        # Find the BVP.csv files amongst all the paths in the list of global$datapaths
        # Blood Volume Pulse = Heart Rate
        gitDataBVP <- reactive({
            if ("BVP" %in% req(input$med)){
                target_dir = global$datapath
                extract_file <- "HR.csv"
                list.files(target_dir, full.names=TRUE) %>% 
                    keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
                    map_df(function(x) {
                        td <- tempdir()
                        read.empatica.hr(unzip(x, extract_file, exdir=td))
                    }, .id = "file_name") -> combined_bvp
                combined_bvp <- combined_bvp %>% rename(value = HR)
                combined_bvp
                }
            })
        # Value boxes
        output$emotion <- renderValueBox({
            valueBox(
                value = round((sum(gitDataEDA()$value > 2.5)/60), digits = 2), "Emotionally Active Minutes", icon = icon("laugh-squint"),
                color = "blue"
            )
        })
        output$stress <- renderValueBox({
            valueBox(
                value = round(mean(as.numeric(gitDataHRV()$value)), digits = 2), "Stress Score", icon = icon("fire-alt"),
                color = "red"
            )
        })
        
        output$mode <- renderValueBox({
        getmode <- function(v) {
            uniqv <- unique(v)
            uniqv[which.max(tabulate(match(v, uniqv)))]
        }
        
        valueBox(
            value = getmode(daylio()$activity) , "Most Frequent Activity", icon = icon("laugh"),
            color = "green"
        )
        })
        
        
        # Read the selected daylio .csv file, bind_rows will stitch together multiple files
        daylio <- reactive({
            req(input$file2)
            upload_day = list()
            
            for(nr in 1:length(input$file2[, 1])){
                upload_day[[nr]] <- read.csv(file = input$file2[[nr, 'datapath']], header = FALSE, col.names = c("date","NULL", "day", "time", "rating", "activity", "note"), colClasses = c('character', 'NULL', 'character', 'character', 'character', 'character', 'character'))
            }
            bind_rows(upload_day)
        })
        
        
        rows <- reactive({
            length(input$med)
        })
        
        
        
            

        
        output$interactive <- renderPlotly({
            req(input$layout == "interactive")
            req(input$med)
            
            if (input$layer == "activity") {
                
                day_plot <- daylio() %>% mutate("Timestamp" = as.POSIXct(mdy_hm(paste(date, time))))
                
                min <- min(gitDataEDA()$Timestamp)
                
                max <- max(gitDataEDA()$Timestamp)
                
                day_plot$activity <-str_replace_all(day_plot$activity, pattern ="[|]", replacement = ",")
                day_plot <- day_plot %>% filter(min < Timestamp & max > Timestamp)
                
                setDT(day_plot)
                setDT(gitDataEDA())
                eda_day <- gitDataEDA()[day_plot, roll = 'nearest', on='Timestamp']
                eda_day$rating <- as.factor(eda_day$rating)
                setDT(gitDataTEMP())
                temp_day <- gitDataTEMP()[day_plot, roll = 'nearest', on='Timestamp']
                temp_day$rating <- as.factor(temp_day$rating)
                setDT(gitDataHRV())
                hrv_day <- gitDataHRV()[day_plot, roll = 'nearest', on='Timestamp']
                hrv_day$rating <- as.factor(hrv_day$rating)
                setDT(gitDataBVP())
                bvp_day <- gitDataBVP()[day_plot, roll = 'nearest', on='Timestamp']
                bvp_day$rating <- as.factor(bvp_day$rating)
                
                
            if (!is.null(gitDataEDA())) {
                fig1 <- plot_ly() %>% add_trace(data = gitDataEDA(), x = floor_date(gitDataEDA()$Timestamp, input$tick), y = gitDataEDA()$value, type = 'scatter', mode = 'line', line = list(shape = "spline"), split = gitDataEDA()$file_name, name = "Emotional Activity") %>% 
                    add_trace(eda_day, type = "scatter", mode = "markers+text", marker = list(color = eda_day$rating,  opacity = 0.5, size = 25, hoverinfo = eda_day$rating), text = eda_day$activity, x = eda_day$Timestamp, y = eda_day$value, textposition = "bottom center", legendgroup = eda_day$rating)
            }
            if (!is.null(gitDataHRV())) {
                fig2 <- plot_ly() %>% add_trace(data = gitDataHRV(), x = floor_date(gitDataHRV()$Timestamp, input$tick), y = gitDataHRV()$value, type = 'scatter', mode = 'line', line = list(shape = 'spline'), split = gitDataHRV()$file_name, name = "Heart Rate Variability") %>% 
                    add_trace(hrv_day, type = "scatter", mode = "markers+text", marker = list(color = hrv_day$rating, opacity = 0.5, size = 25, hoverinfo = hrv_day$rating), text = hrv_day$activity, x = hrv_day$Timestamp, y = hrv_day$value, textposition = "bottom center", legendgroup = hrv_day$rating)
                    }
            if (!is.null(gitDataTEMP())) {
                fig3 <- plot_ly() %>% add_trace(data = gitDataTEMP(), x = floor_date(gitDataTEMP()$Timestamp, input$tick), y = gitDataTEMP()$value, type = 'scatter', mode = 'lines', line = list(shape = 'spline'), split = gitDataTEMP()$file_name, name = "Skin Temperature") %>% 
                    add_trace(temp_day, type = "scatter", mode = "markers+text", marker = list(color = temp_day$rating, opacity = 0.5, size = 25, hoverinfo = temp_day$rating), text = temp_day$activity, x = temp_day$Timestamp, y = temp_day$value, textposition = "bottom center", legendgroup = temp_day$rating)
                    }
            
            if (!is.null(gitDataBVP())) {
                fig4 <- plot_ly() %>% add_trace(data = gitDataBVP(), x = floor_date(gitDataBVP()$Timestamp, input$tick), y = gitDataBVP()$value, type = 'scatter', mode = 'lines', line = list(shape = 'spline'), split = gitDataBVP()$file_name, name = "Heart Rate") %>% 
                    add_trace(bvp_day, type = "scatter", mode = "markers+text", marker = list(color = bvp_day$rating, opacity = 0.5, size = 25, hoverinfo = bvp_day$rating), text = bvp_day$activity, x = bvp_day$Timestamp, y = bvp_day$value, textposition = "bottom center", legendgroup = bvp_day$rating)
                    }
                
            if (length(input$med) > 1) {
                # fig <- plot_ly() %>% 
                #   add_trace(data = gear3(), x = gear3()$mpg, y = gear3()$disp, type = "scatter", mode = "lines") %>% 
                #   add_trace(data = gear4(), x = gear4()$mpg, y = gear4()$wt, type = "scatter", mode = "lines") %>% 
                #   add_trace(data = gear5(), x = gear5()$mpg, y = gear5()$qsec, type = "scatter", mode = "lines") 
                
                if (length(input$med)==4){
                    myplots <- list(fig1, fig2, fig3, fig4)
                }else if (length(input$med)==3){
                    if (is.null(gitDataEDA())) myplots <- list(fig2, fig3, fig4) # everything but eda
                    else if (is.null(gitDataHRV())) myplots <- list(fig1, fig3, fig4) # everything but hrv
                    else if (is.null(gitDataTEMP())) myplots <- list(fig1, fig2, fig4) # everything but temp
                    else if (is.null(gitDataBVP())) myplots <- list(fig1, fig2, fig3) # everything but bvp
                }
                else {
                    if ( is.null(gitDataEDA()) && is.null(gitDataHRV()) ) myplots <- list(fig3, fig4) ## temp bvp
                    else if ( is.null(gitDataBVP()) && is.null(gitDataTEMP()) ) myplots <- list(fig1, fig2) # eda hrv
                    
                    else if ( is.null(gitDataHRV()) && is.null(gitDataTEMP()) ) myplots <- list(fig1, fig4) #eda bvp 
                    else if ( is.null(gitDataBVP()) && is.null(gitDataHRV()) ) myplots <- list(fig1, fig3) #temp eda
                
                    else if ( is.null(gitDataTEMP()) && is.null(gitDataEDA()) ) myplots <- list(fig2, fig4) #hrv bvp
                    else if ( is.null(gitDataBVP()) && is.null(gitDataEDA()) ) myplots <- list(fig3, fig2) #temp hrv
                
                }
                
                
                fig <- subplot(myplots, shareX = TRUE, nrows = length(input$med)) %>%
                    layout(
                        xaxis = list(
                            type = 'date',
                            tickformatstops = list(
                                list(
                                    dtickrange = list(NULL, 1000), 
                                    value = "%H:%M:%S.%L ms"
                                ), 
                                list(
                                    dtickrange = list(1000, 60000), 
                                    value = "%b %d %H:%M:%S s"
                                ), 
                                list(
                                    dtickrange = list(60000, 3600000), 
                                    value = "%b %d %H:%M m"
                                ), 
                                list(
                                    dtickrange = list(3600000, 86400000), 
                                    value = "%b %d %H:%M h"
                                ), 
                                list(
                                    dtickrange = list(86400000, 604800000), 
                                    value = "%e. %b d"
                                ), 
                                list(
                                    dtickrange = list(604800000, "M1"), 
                                    value = "%e. %b w"
                                ), 
                                list(
                                    dtickrange = list("M1", "M12"), 
                                    value = "%b '%y M"
                                ), 
                                list(
                                    dtickrange = list("M12", NULL), 
                                    value = "%Y Y"
                                )
                            )
                        )
                    )
                
            }
            else if (length(input$med) == 1) {
                    
                    
                    fig <- plot_ly() %>% add_trace(data = gitDataEDA(), x = floor_date(gitDataEDA()$Timestamp, input$tick), y = gitDataEDA()$value, type = 'scatter', mode = 'lines', line = list(shape = 'spline'), split = gitDataEDA()$file_name, name = "Emotional Activity") %>% 
                    add_trace(data = gitDataHRV(), x = floor_date(gitDataHRV()$Timestamp, input$tick), y = gitDataHRV()$value, type = 'scatter', mode = 'lines', line = list(shape = 'spline'), split = gitDataHRV()$file_name, name = "Heart Rate Variability", connectgaps = TRUE) %>% 
                    add_trace(data = gitDataTEMP(), x = floor_date(gitDataTEMP()$Timestamp, input$tick), y = gitDataTEMP()$value, type = 'scatter', mode = 'lines', line = list(shape = 'spline'), split = gitDataTEMP()$file_name, name = "Skin Temperature") %>% 
                    add_trace(data = gitDataBVP(), x = floor_date(gitDataBVP()$Timestamp, input$tick), y = gitDataBVP()$value,  type = 'scatter', mode = 'lines', line = list(shape = 'spline'), split = gitDataBVP()$file_name, name = "Heart Rate") %>%
                    add_trace(eda_day, mode = "markers+text", marker = list(color = eda_day$rating,  opacity = 0.5, size = 25, hoverinfo = eda_day$rating), text = eda_day$activity, x = eda_day$Timestamp, y = eda_day$value, textposition = "bottom center", name = "Mood & Activity", legendgroup = eda_day$rating) %>% 
                    add_trace(temp_day, mode = "markers+text",marker = list(color = temp_day$rating,  opacity = 0.5, size = 25, hoverinfo = hrv_day$rating), text = temp_day$activity, x = temp_day$Timestamp, y = temp_day$value, textposition = "bottom center", name = "Mood & Activity", legendgroup = temp_day$rating) %>% 
                    add_trace(hrv_day, mode = "markers+text", marker = list(color = hrv_day$rating,  opacity = 0.5, size = 25, hoverinfo = temp_day$rating), text = hrv_day$activity, x = hrv_day$Timestamp, y = hrv_day$value, textposition = "bottom center", name = "Mood & Activity", legendgroup = hrv_day$rating) %>% 
                    add_trace(bvp_day, mode = "markers+text", marker = list(color = bvp_day$rating,  opacity = 0.5, size = 25, hoverinfo = bvp_day$rating), text = bvp_day$activity, x = bvp_day$Timestamp, y = bvp_day$value, textposition = "bottom center", name = "Mood & Activity", legendgroup = bvp_day$rating)
                }
            }
            ## none 
                else{
                
                if (!is.null(gitDataEDA())) {
                    fig1 <- plot_ly() %>% add_trace(data = gitDataEDA(), x = floor_date(gitDataEDA()$Timestamp, input$tick), y = gitDataEDA()$value, type = "scatter", mode = 'lines', line = list(shape = 'spline'), split = gitDataEDA()$file_name, name = "Emotional Activity") 
                }
                if (!is.null(gitDataHRV())) {
                    fig2 <- plot_ly() %>% add_trace(data = gitDataHRV(), x = floor_date(gitDataHRV()$Timestamp, input$tick), y = gitDataHRV()$value, type = 'scatter', mode = 'lines', line = list(shape = 'spline'), split = gitDataHRV()$file_name, name = "Heart Rate Variability")
                }
                if (!is.null(gitDataTEMP())) {
                    fig3 <- plot_ly() %>% add_trace(data = gitDataTEMP(), x = floor_date(gitDataTEMP()$Timestamp, input$tick), y = gitDataTEMP()$value, type = 'scatter', mode = 'lines', line = list(shape = 'spline'), split = gitDataTEMP()$file_name, name = "Skin Temperature")
                }
                
                if (!is.null(gitDataBVP())) {
                    fig4 <- plot_ly() %>% add_trace(data = gitDataBVP(), x = floor_date(gitDataBVP()$Timestamp, input$tick), y = gitDataBVP()$value, type = 'scatter', mode = 'lines', line = list(shape = 'spline'), split = gitDataBVP()$file_name, name = "Heart Rate") 
                }
                
                if (length(input$med) > 1) {
                    # fig <- plot_ly() %>% 
                    #   add_trace(data = gear3(), x = gear3()$mpg, y = gear3()$disp, type = "scatter", mode = "lines") %>% 
                    #   add_trace(data = gear4(), x = gear4()$mpg, y = gear4()$wt, type = "scatter", mode = "lines") %>% 
                    #   add_trace(data = gear5(), x = gear5()$mpg, y = gear5()$qsec, type = "scatter", mode = "lines") 
                    
                    if (length(input$med)==4){
                        myplots <- list(fig1, fig2, fig3, fig4)
                    }else if (length(input$med)==3){
                        if (is.null(gitDataEDA())) myplots <- list(fig2, fig3, fig4) # everything but eda
                        else if (is.null(gitDataHRV())) myplots <- list(fig1, fig3, fig4) # everything but hrv
                        else if (is.null(gitDataTEMP())) myplots <- list(fig1, fig2, fig4) # everything but temp
                        else if (is.null(gitDataBVP())) myplots <- list(fig1, fig2, fig3) # everything but bvp
                    }
                    else {
                        if ( is.null(gitDataEDA()) && is.null(gitDataHRV()) ) myplots <- list(fig3, fig4) ## temp bvp
                        else if ( is.null(gitDataBVP()) && is.null(gitDataTEMP()) ) myplots <- list(fig1, fig2) # eda hrv
                        
                        else if ( is.null(gitDataHRV()) && is.null(gitDataTEMP()) ) myplots <- list(fig1, fig4) #eda bvp 
                        else if ( is.null(gitDataBVP()) && is.null(gitDataHRV()) ) myplots <- list(fig1, fig3) #temp eda
                        
                        else if ( is.null(gitDataTEMP()) && is.null(gitDataEDA()) ) myplots <- list(fig2, fig4) #hrv bvp
                        else if ( is.null(gitDataBVP()) && is.null(gitDataEDA()) ) myplots <- list(fig3, fig2) #temp hrv
                        
                    }
                    
                    
                    fig <- subplot(myplots, shareX = TRUE, nrows = length(input$med)) %>%
                        layout(
                            xaxis = list(
                                type = 'date',
                                tickformatstops = list(
                                    list(
                                        dtickrange = list(NULL, 1000), 
                                        value = "%H:%M:%S.%L ms"
                                    ), 
                                    list(
                                        dtickrange = list(1000, 60000), 
                                        value = "%b %d %H:%M:%S s"
                                    ), 
                                    list(
                                        dtickrange = list(60000, 3600000), 
                                        value = "%b %d %H:%M m"
                                    ), 
                                    list(
                                        dtickrange = list(3600000, 86400000), 
                                        value = "%b %d %H:%M h"
                                    ), 
                                    list(
                                        dtickrange = list(86400000, 604800000), 
                                        value = "%e. %b d"
                                    ), 
                                    list(
                                        dtickrange = list(604800000, "M1"), 
                                        value = "%e. %b w"
                                    ), 
                                    list(
                                        dtickrange = list("M1", "M12"), 
                                        value = "%b '%y M"
                                    ), 
                                    list(
                                        dtickrange = list("M12", NULL), 
                                        value = "%Y Y"
                                    )
                                )
                            )
                        )
                    
                }
                
                else {fig <- plot_ly() %>% add_trace(data = gitDataEDA(), x = floor_date(gitDataEDA()$Timestamp, input$tick), type = "scatter", y = gitDataEDA()$value, mode = "lines", line = list(shape = 'spline'), split = gitDataEDA()$file_name, name = "Emotional Activity") %>% 
                    add_trace(data = gitDataHRV(), x = floor_date(gitDataHRV()$Timestamp, input$tick), y = gitDataHRV()$value, type = 'scatter', mode = 'lines', line = list(shape = 'spline'), split = gitDataHRV()$file_name, name = "Heart Rate Variability", connectgaps = TRUE) %>% 
                    add_trace(data = gitDataTEMP(), x = floor_date(gitDataTEMP()$Timestamp, input$tick), y = gitDataTEMP()$value, type = 'scatter', mode = 'lines', line = list(shape = 'spline'), split = gitDataTEMP()$file_name, name = "Skin Temperature") %>% 
                    add_trace(data = gitDataBVP(), x = floor_date(gitDataBVP()$Timestamp, input$tick), y = gitDataBVP()$value,  type = 'scatter', mode = 'lines', line = list(shape = 'spline'), split = gitDataBVP()$file_name, name = "Heart Rate")
                }
                }
            
            fig %>%
                layout(
                    xaxis = list(
                        type = 'date',
                        tickformatstops = list(
                            list(
                                dtickrange = list(NULL, 1000), 
                                value = "%H:%M:%S.%L ms"
                            ), 
                            list(
                                dtickrange = list(1000, 60000), 
                                value = "%b %d %H:%M:%S s"
                            ), 
                            list(
                                dtickrange = list(60000, 3600000), 
                                value = "%b %d %H:%M m"
                            ), 
                            list(
                                dtickrange = list(3600000, 86400000), 
                                value = "%b %d %H:%M h"
                            ), 
                            list(
                                dtickrange = list(86400000, 604800000), 
                                value = "%e. %b d"
                            ), 
                            list(
                                dtickrange = list(604800000, "M1"), 
                                value = "%e. %b w"
                            ), 
                            list(
                                dtickrange = list("M1", "M12"), 
                                value = "%b '%y M"
                            ), 
                            list(
                                dtickrange = list("M12", NULL), 
                                value = "%Y Y"
                            )
                        )
                    )
                )
            
                 })
        
        
        # Generate a plot of the data ----
        # Caching plots will "remember" processed plots depending on the combination of layers and metrics.
        
        output$staticplotEDA <- renderCachedPlot({
           
        if (input$layer == "activity") {
            
            min <- min(gitDataEDA()$Timestamp)
            
            max <- max(gitDataEDA()$Timestamp)
            
            day_plot <- daylio() %>% mutate("Timestamp" = as.POSIXct(mdy_hm(paste(date, time))))
            
            day_plot$activity <-str_replace_all(day_plot$activity, pattern ="[|]", replacement = ",")
            
            day_plot <- left_join(day_plot, gitDataEDA(), "Timestamp")
            
            day_plot <- filter(day_plot, Timestamp > min & Timestamp < max)
            
            if (input$layout == "circle") {ggplot(data = gitDataEDA(), mapping = aes(x=floor_date(Timestamp, input$tick), group = file_name, y = value)) + geom_spline() + geom_label_repel(day_plot, mapping = aes(label = activity, fill = as.factor(rating)), alpha = 0.6, point.padding = 2.5, size = 6) + labs(fill = "Mood") + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + coord_polar() + scale_x_datetime() 
            } 
            else if (input$layout == "standard")  {ggplot(data = gitDataEDA(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + geom_label_repel(day_plot, mapping = aes(label = activity, fill = as.factor(rating)), alpha = 0.6, point.padding = 2.5, size = 6) + labs(fill = "Mood") + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + scale_x_datetime()
            }
        }
        #else if (input$layer == "mood") {
            
            #min <- min(gitDataEDA()$Timestamp)
            
            #max <- max(gitDataEDA()$Timestamp)
            
            #day_plot <- daylio() %>% mutate("Timestamp" = as.POSIXct(mdy_hm(paste(date, time))))
            
            #day_plot <- left_join(day_plot, gitDataEDA(), "Timestamp")
            
            #day_plot <- filter(day_plot, Timestamp > min & Timestamp < max)
            
           # if (input$layout == "circle") {
                #ggplot(data = gitDataEDA(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + geom_tile(day_plot, mapping = aes(fill = as.factor(rating)), alpha = 0.75, height = 10,  width = 800) + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + coord_polar() + scale_x_datetime() + labs(fill = "Mood")
            #}
            #else if (input$layout == "standard") {
                #ggplot(data = gitDataEDA(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + geom_tile(day_plot, mapping = aes(fill = as.factor(rating)), height = 10, alpha = 0.75, width = 800) + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + scale_x_datetime() + labs(fill = "Mood")
            #}
            
        #}
            else if (input$layout == "circle") {
            ggplot(data = gitDataEDA(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + coord_polar() + scale_x_datetime()
            }
            else if (input$layout == "standard") {
            ggplot(data = gitDataEDA(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + scale_x_datetime()
            }
            
            
        }, cacheKeyExpr = { list(input$layer, input$tick, gitDataEDA(), input$layout)})
            
            
        output$staticplotHRV <- renderCachedPlot({
                
                if (input$layer == "activity") {
                    
                    min <- min(gitDataHRV()$Timestamp)
                    
                    max <- max(gitDataHRV()$Timestamp)
                    
                    day_plot<- daylio() %>% mutate("Timestamp" = as.POSIXct(mdy_hm(paste(date, time))))
                    
                    day_plot$activity <-str_replace_all(day_plot$activity, pattern ="[|]", replacement = ",")
                    
                    day_plot <- filter(day_plot, Timestamp > min & Timestamp < max)
                    
                    if (input$layout == "circle") {ggplot(data = gitDataHRV(), mapping = aes(x=floor_date(Timestamp, input$tick), group = file_name, y = value)) + geom_spline() + geom_label_repel(day_plot, mapping = aes(x = floor_date(Timestamp, "minute"), label = activity, fill = as.factor(rating)), y = 0.1, nudge_y = 0.5, size =6, inherit.aes = FALSE, alpha = 0.75, point.padding = 2.5) + labs(fill = "Mood") + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + coord_polar() + scale_x_datetime()
                    } 
                    else if (input$layout == "standard")  {ggplot(data = gitDataHRV(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + geom_label_repel(day_plot, mapping = aes(x = floor_date(Timestamp, "minute"), label = activity, fill = as.factor(rating)), y = 0.1, nudge_y = 0.5, size =6, inherit.aes = FALSE, alpha = 0.75, point.padding = 2.5) + labs(fill = "Mood") + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + scale_x_datetime() 
                    }
                }
                #else if (input$layer == "mood") {
                    
                  # min <- min(gitDataHRV()$Timestamp)
                    
                   # max <- max(gitDataHRV()$Timestamp)
                    
                   # day_plot <- daylio() %>% mutate("Timestamp" = as.POSIXct(mdy_hm(paste(date, time))))
                    
                   # day_plot <- filter(day_plot, Timestamp > min & Timestamp < max)
                    
                   # if (input$layout == "circle") {
                        #ggplot(data = gitDataHRV(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + geom_rect(day_plot, mapping = aes(xmin = (floor_date(Timestamp, "minute") - 360), xmax = floor_date(Timestamp, "minute"), fill = as.factor(rating), ymin = min(gitDataHRV()$value), ymax = max(gitDataHRV()$value)), alpha = 0.6, inherit.aes = FALSE) + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + coord_polar() + scale_x_datetime() + labs(fill = "Mood")
                   # }
                   # else if (input$layout == "standard") {
                        #ggplot(data = gitDataHRV(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + geom_rect(day_plot, mapping = aes(xmin = (floor_date(Timestamp, "minute") - 360), xmax = floor_date(Timestamp, "minute"), fill = as.factor(rating), ymin = min(gitDataHRV()$value), ymax = max(gitDataHRV()$value)), alpha = 0.6, inherit.aes = FALSE) + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + scale_x_datetime() + labs(fill = "Mood")
                   # }
                    
                #}
                else if (input$layout == "circle") {
                    ggplot(data = gitDataHRV(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + coord_polar() + scale_x_datetime()
                }
                else if (input$layout == "standard") {
                    ggplot(data = gitDataHRV(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + scale_x_datetime()
                }
        

        }, cacheKeyExpr = { list(input$layer, input$tick, gitDataHRV(), input$layout)})
        
        
        output$staticplotTEMP <- renderCachedPlot({
            
            if (input$layer == "activity") {
                
                min <- min(gitDataTEMP()$Timestamp)
                
                max <- max(gitDataTEMP()$Timestamp)
                
                day_plot <- daylio() %>% mutate("Timestamp" = as.POSIXct(mdy_hm(paste(date, time))))
                
                day_plot$activity <-str_replace_all(day_plot$activity, pattern ="[|]", replacement = ",")
                
                day_plot <- left_join(day_plot, gitDataTEMP(), "Timestamp")
                
                day_plot <- filter(day_plot, Timestamp > min & Timestamp < max)
                
                if (input$layout == "circle") {ggplot(data = gitDataTEMP(), mapping = aes(x=floor_date(Timestamp, input$tick), group = file_name, y = value)) + geom_spline() + geom_label_repel(day_plot, mapping = aes(label = activity, fill = as.factor(rating)), alpha = 0.6, point.padding = 2.5, size = 6) + labs(fill = "Mood") + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + coord_polar() + scale_x_datetime() 
                } 
                else if (input$layout == "standard")  {ggplot(data = gitDataTEMP(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + geom_label_repel(day_plot, mapping = aes(label = activity, fill = as.factor(rating)), alpha = 0.6, point.padding = 2.5, size = 6) + labs(fill = "Mood") + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + scale_x_datetime()
                }
            }
            #else if (input$layer == "mood") {
                
                #min <- min(gitDataTEMP()$Timestamp)
                
               # max <- max(gitDataTEMP()$Timestamp)
                
                #day_plot <- daylio() %>% mutate("Timestamp" = as.POSIXct(mdy_hm(paste(date, time))))
                
                #day_plot <- left_join(day_plot, gitDataTEMP(), "Timestamp")
                
                #day_plot <- filter(day_plot, Timestamp > min & Timestamp < max)
                
               # if (input$layout == "circle") {
                    #ggplot(data = gitDataTEMP(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + geom_tile(day_plot, mapping = aes(fill = as.factor(rating)), alpha = 0.75, height = 30,  width = 800) + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + coord_polar() + scale_x_datetime() + labs(fill = "Mood")
#}
               # else if (input$layout == "standard") {
                    #ggplot(data = gitDataTEMP(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + geom_tile(day_plot, mapping = aes(fill = as.factor(rating)), height = 30, alpha = 0.75, width = 800) + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + scale_x_datetime() + labs(fill = "Mood")
                #}
                
            #}
            else if (input$layout == "circle") {
                ggplot(data = gitDataTEMP(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + coord_polar() + scale_x_datetime()
            }
            else if (input$layout == "standard") {
                ggplot(data = gitDataTEMP(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + scale_x_datetime()
            }
            
            
        }, cacheKeyExpr = { list(input$layer, input$tick, gitDataTEMP(), input$layout)})
        
        
        output$staticplotBVP <- renderCachedPlot({
            
            if (input$layer == "activity") {
                
                min <- min(gitDataBVP()$Timestamp)
                
                max <- max(gitDataBVP()$Timestamp)
                
                day_plot <- daylio() %>% mutate("Timestamp" = as.POSIXct(mdy_hm(paste(date, time))))
                
                day_plot$activity <-str_replace_all(day_plot$activity, pattern ="[|]", replacement = ",")
                
                day_plot <- left_join(day_plot, gitDataBVP(), "Timestamp")
                
                day_plot <- filter(day_plot, Timestamp > min & Timestamp < max)
                
                if (input$layout == "circle") {ggplot(data = gitDataBVP(), mapping = aes(x=floor_date(Timestamp, input$tick), group = file_name, y = value)) + geom_spline() + geom_label_repel(day_plot, mapping = aes(label = activity, fill = as.factor(rating)), alpha = 0.6, point.padding = 2.5, size = 6) + labs(fill = "Mood") + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + coord_polar() + scale_x_datetime() 
                } 
                else if (input$layout == "standard")  {ggplot(data = gitDataBVP(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + geom_label_repel(day_plot, mapping = aes(label = activity, fill = as.factor(rating)), alpha = 0.6, point.padding = 2.5, size = 6) + labs(fill = "Mood") + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + scale_x_datetime()
                }
            }
           # else if (input$layer == "mood") {
                
                #min <- min(gitDataBVP()$Timestamp)
                
               # max <- max(gitDataBVP()$Timestamp)
                
                #day_plot <- daylio() %>% mutate("Timestamp" = as.POSIXct(mdy_hm(paste(date, time))))
                
                #day_plot <- left_join(day_plot, gitDataBVP(), "Timestamp")
                
                #day_plot <- filter(day_plot, Timestamp > min & Timestamp < max)
                
                #if (input$layout == "circle") {
                  #  ggplot(data = gitDataBVP(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + geom_tile(day_plot, mapping = aes(fill = as.factor(rating)), alpha = 0.75, height = 30,  width = 800) + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + coord_polar() + scale_x_datetime() + labs(fill = "Mood")
                #}
                #else if (input$layout == "standard") {
                  #  ggplot(data = gitDataBVP(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + geom_tile(day_plot, mapping = aes(fill = as.factor(rating)), height = 30, alpha = 0.75, width = 800) + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + scale_x_datetime() + labs(fill = "Mood")
               # }
                
            #}
            else if (input$layout == "circle") {
                ggplot(data = gitDataBVP(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + coord_polar() + scale_x_datetime()
            }
            else if (input$layout == "standard") {
                ggplot(data = gitDataBVP(), mapping = aes(x=floor_date(Timestamp, input$tick), y = value, group = file_name)) + geom_spline() + theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) + theme(axis.title.x=element_blank()) + scale_x_datetime()
            }
            
            
        }, cacheKeyExpr = { list(input$layer, input$tick, gitDataBVP(), input$layout)})
        

        
        # Generate a summary of the data ----
        
    output$calendar <- renderDT({
        
        calendarview <- daylio() %>% separate(activity, c("0", "0.5", "1"), sep = "[|]", extra = "merge") %>% pivot_longer(c('0', '0.5', '1'), values_to = "activity", names_to = "activity_rank", values_drop_na = TRUE) %>% mutate("log_time" = as.POSIXct(mdy_hm(paste(date, time))))
        
        
        calendarview %>% select(log_time, activity)
        })     
        
    
    output$table <- renderDataTable({
        gitDataHRV()
            })
    
    output[["edaBox"]] <- renderUI({
        req("EDA" %in% req(input$med))
        if(is.null(gitDataEDA()))return()
        if(input$layout == "circle"){
        box(
            title = "Emotional Activity", status = "primary", plotOutput("staticplotEDA", height = 600, width = 600), collapsible = TRUE)
        }
        else if (input$layout == "standard") {
        box(
            title = "Emotional Activity", width = NULL, status = "primary", plotOutput("staticplotEDA"), collapsible = TRUE)
    }
        
    })
    
    
    output[["bvpBox"]] <- renderUI({
        
        if(is.null(gitDataBVP()))return()
        if(input$layout == "circle"){
        box(
            title = "Heart Rate", status = "primary", plotOutput("staticplotBVP", height = 600, width = 600), collapsible = TRUE)
        }else if (input$layout == "standard") {
            box(
                title = "Heart Rate", width = NULL, status = "primary",  plotOutput("staticplotBVP"), collapsible = TRUE)
        }
    })
    
    output[["tempBox"]] <- renderUI({
        
        if(is.null(gitDataTEMP()))return()
        if(input$layout == "circle"){
            box(
                title = "Skin Temperature", status = "primary", plotOutput("staticplotTEMP", height = 600, width = 600), collapsible = TRUE)
        }
        else if (input$layout == "standard"){
        box(
            title = "Skin Temperature", width = NULL, status = "primary", plotOutput("staticplotTEMP"), collapsible = TRUE)
    }
        
    })
    
    
    
    output[["hrvBox"]] <- renderUI({
        
        if(is.null(gitDataHRV()))return()
        if(input$layout == "circle"){
        box(
            title = "Heart Rate Variability", status = "primary",  plotOutput("staticplotHRV", height = 600, width = 600), collapsible = TRUE)
        }
        else if (input$layout == "standard") {
        box(
            title = "Heart Rate Variability", width = NULL, status = "primary",  plotOutput("staticplotHRV"), collapsible = TRUE)
        }
        
    })
    
    output[["interactivebox"]] <- renderUI({
        
        if(input$layout != "interactive")return()
        
            box(
                title = "Interactive", width = NULL, status = "primary", plotlyOutput("interactive", height = 800), collapsible = TRUE)
        
    })
    
})



