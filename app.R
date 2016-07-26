library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(plotly)
library(tidyr)
library(dplyr)
library(forecast)
library(caret)

source("GetTrendInfo.R")
source("GetRandomSd.R")

office <- readRDS("office.rds")
#office$First_day <- as.character(office$First_day)
#office$Last_day <- as.character(office$Last_day)
revenue_office <- readRDS("revenue_office.rds")
office_service <- readRDS("office_service.rds")
baskets_by_office <- readRDS("baskets_by_office.rds")
baskets_code <- readRDS("baskets_code.rds")
net_hourly <- readRDS("net_hourly.rds")
net_weekly <- readRDS("net_weekly.rds")
service_class <- readRDS("service_class.rds")
office_part <- readRDS("office_part.rds")

#Sys.setlocale('LC_ALL', 'russian')

############################################################# S E R V E R
server <- function(input, output){
    
    output$office_stats <- renderDataTable({
        check_data <- readRDS(paste0("check_dynamics/check_dynamics_",input$input_office,".rds"))
        check_data[,2:4] <- apply(check_data[,2:4],2,function(x) as.numeric(as.character(x)))
        check_data$Nweek <- strftime(as.Date(check_data$Date),"%W")
        check_data$Year <- strftime(as.Date(check_data$Date),"%Y")
        check_data <- unite_(check_data,"year_week",c("Year","Nweek"))
        newdf <- data.frame(Week=unique(check_data$year_week),
                            Class1=as.vector(tapply(check_data$Class1,INDEX = check_data$year_week,sum)),
                            Class2=as.vector(tapply(check_data$Class2,INDEX = check_data$year_week,sum)),
                            Class3=as.vector(tapply(check_data$Class3,INDEX = check_data$year_week,sum)))
        data <- data.frame("One"=c("Суммарный оборот", "Период работы, дней",
                                   "Средний оборот в день", "Кол-во дней с 0-м оборотом",
                                   "Первый день работы","Последний день работы",
                                   "Оборот за 2016 г.", "Средний оборот за 2016 г.",
                                   "Соотношение классов чеков (1/2/3)"),
                           "Two" = c(unlist(office[office$office_id==input$input_office,
                                                 c(2:4,12:16)]),
                           paste(round(colSums(newdf[,2:4])/sum(colSums(newdf[,2:4])),2),collapse = "/")))
        data$Two <- as.character(data$Two)
        data[c(3,8),2] <- round(as.numeric(data[c(3,8),2]),3)
        data[c(5,6),2] <- as.character(data[c(5,6),2])
        datatable(data,rownames = F,colnames="",options = list(dom = 't'))
    })
    
    output$office_week_plot <- renderPlotly({
        data <- readRDS(paste0("weekly/weekly_",input$input_office,".rds"))
        pl <- plot_ly(x=data$X,y=data[,2],name="Понедельник")
        pl <- add_trace(pl,x=data$X,y=data[,3],name="Вторник")
        pl <- add_trace(pl,x=data$X,y=data[,4],name="Среда")
        pl <- add_trace(pl,x=data$X,y=data[,5],name="Четверг")
        pl <- add_trace(pl,x=data$X,y=data[,6],name="Friday")
        pl <- add_trace(pl,x=data$X,y=data[,7],name="Суббота")
        pl <- add_trace(pl,x=data$X,y=data[,8],name="Воскресенье")
        pl <- layout(pl, title = "Оборот за недельный период", xaxis = list(title = "День недели"),
                     yaxis = list(title = "% от оборота"))
        pl
    })
    
    output$office_hourly_plot <- renderPlotly({
        data <- readRDS(paste("hourly/hourly_",input$input_office,".rds",sep = ""))
        data <- data[,c("X9_10","X11_12","X13_14","X15_16","X17_18","X19_20","X21_22")]
        data <- data.frame(apply(data,2,function(col) round(col/rowSums(data),2)))
        pl <- plot_ly(x=rownames(data),y=data$X9_10,name="9-10")
        pl <- add_trace(pl,x=rownames(data),y=data$X11_12,name="11-12")
        pl <- add_trace(pl,x=rownames(data),y=data$X13_14,name="13-14")
        pl <- add_trace(pl,x=rownames(data),y=data$X15_16,name="15-16")
        pl <- add_trace(pl,x=rownames(data),y=data$X17_18,name="17-18")
        pl <- add_trace(pl,x=rownames(data),y=data$X19_20,name="19-20")
        pl <- add_trace(pl,x=rownames(data),y=data$X21_22,name="21-22")
        pl <- layout(pl, title = "Оборот по часам", xaxis = list(title = "Час"),
                     yaxis = list(title = "% от оборота за день"))
        pl
    })
    
    output$office_revenue_plot <- renderPlotly({
        data <- revenue_office[,c("X",paste("X",input$input_office,sep = ""))]
        data$week <- strftime(as.Date(data$X),"%W")
        data$year <- strftime(as.Date(data$X),"%Y")
        data <- unite_(data,"year_week",c("year","week"))
        rev <- aggregate(x=data[,2],by=list(data$year_week),FUN=sum)
        pl <- plot_ly(data=rev,x=Group.1,y=x,type = "scatter",name="Оборот")
        pl <- layout(pl, title = "Динамика оборота", xaxis = list(title = "Период"),
                     yaxis = list(title = "Оборот"))
        pl
    })
    
    output$office_top5service <- renderDataTable({
        data <- readRDS(paste0("service_check/service_class_",input$input_office,".rds"))
        data <- data[order(rowSums(data[2:4]),decreasing = T),]
        data <- data[1:10,]
        datatable(data,rownames = F,colnames=c("Топ-10 услуг","Чек 1","Чек 2-3","Чек >4"),options = list(dom = 't'))
    })
    
    output$office_top5basket <- renderDataTable({
        data <- filter(baskets_by_office, rownames(baskets_by_office) == input$input_office)
        data <- data[,order(data[1,],decreasing = T)]
        data <- data.frame(t(data[,1:10]))
        data$basket <- substr(rownames(data),2,5)
        data$description <- sapply(data$basket,function(num){
            return(as.character(filter(baskets_code, code == num)$Baskets))
        })
        data <- data[,c(3,1)]
        datatable(data,rownames = F,colnames=c("Топ-10 корзин","Кол-во"),options = list(dom = 't'))
    })
    
    output$net_revenue2016 <- renderPlotly({
        pl <- plot_ly(data=office,x=as.character(office_id),y=revenue_2016,type = "bar",name="Оборот 2016")
        pl <- layout(pl, title = "Оборот 2016", xaxis = list(title = "Офис"),
                     yaxis = list(title = "Оборот"))
        pl
    })
    
    output$net_revenue_total <- renderPlotly({
        pl <- plot_ly(data=office,x=as.character(office_id),y=total_amount,type = "bar",name="Оборот 2016")
        pl <- layout(pl, title = "Суммарный оборот", xaxis = list(title = "Офис"),
                     yaxis = list(title = "Оборот"))
        pl
    })
    
    output$net_period <- renderPlotly({
        pl <- plot_ly(data=office,x=as.character(office_id),y=total_days,type = "bar",name="Период работы")
        pl <- layout(pl, title = "Период работы", xaxis = list(title = "Офис"),
                     yaxis = list(title = "Дней"))
        pl
    })
    
    output$net_zero <- renderPlotly({
        pl <- plot_ly(data=office,x=as.character(office_id),y=null_days,type = "bar",name="Кол-во дней с 0-м оборотом")
        pl <- layout(pl, title = "Кол-во дней с 0-м оборотом", xaxis = list(title = "Офис"),
                     yaxis = list(title = "Кол-во дней с 0-м оборотом"))
        pl
    })
    
    output$net_week <- renderPlotly({
        pl <- plot_ly(data=net_weekly,x=year_week, y=Md, name="Понедельник")
        pl <- add_trace(pl,data=net_weekly,x=year_week, y=Tu, name="Вторник")
        pl <- add_trace(pl,data=net_weekly,x=year_week, y=We, name="Среда")
        pl <- add_trace(pl,data=net_weekly,x=year_week, y=Th, name="Четверг")
        pl <- add_trace(pl,data=net_weekly,x=year_week, y=Fr, name="Friday")
        pl <- add_trace(pl,data=net_weekly,x=year_week, y=Sa, name="Суббота")
        pl <- add_trace(pl,data=net_weekly,x=year_week, y=Su, name="Воскресенье")
        pl <- layout(pl, title = "Оборот в дни недели", xaxis = list(title = "Week"),
                     yaxis = list(title = "Оборот"))
        pl
    })
    
    
    output$net_alltime <- renderPlotly({
        rev <- aggregate(x=rowSums(revenue_office[,2:31]),by=list(revenue_office$year_week),FUN=sum)
        rev$active <- round(aggregate(rowSums(revenue_office[,2:31]>0),by=list(revenue_office$year_week),FUN=mean)[,2],0)
        ay <- list(tickfont = list(color = "red"),overlaying = "y",side = "right")
        pl <- plot_ly(data=rev,x=Group.1,y=x,type = "scatter",name="Оборот")
        pl <- add_trace(pl,data=rev,x=Group.1,y=active,type = "scatter",name="Активных точек",yaxis = "y2")
        pl <- layout(pl, title = "Динамика оборота сети", xaxis = list(title = "Период"),
                     yaxis = list(title = "Оборот"), yaxis2 = ay)
        pl
    })
    
    #hidden
    output$net_top5service <- renderDataTable({
        data <- data.frame(Service = colnames(office_service)[-1], 
                           Amount=colSums(office_service)[-1])
        data <- data[order(data$Amount,decreasing = T),]
        data <- data[1:10,]
        datatable(data,rownames = F,colnames=c("Топ-10 услуг","Кол-во"),options = list(dom = 't'))
    })
    
    output$net_check_service <- renderDataTable({
        data <- service_class
        data <- data[order(rowSums(data[2:4]),decreasing = T),]
        data <- data[1:10,]
        datatable(data,rownames = F,colnames=c("Топ-10 услуг","Чек 1","Чек 2-3","Чек >4"),options = list(dom = 't'))
    })
    
    output$net_top5basket <- renderDataTable({
        data <- data.frame(Basket = baskets_code$Baskets,Amount= colSums(baskets_by_office))
        data <- data[order(data$Amount,decreasing = T),]
        data <- data[1:10,]
        datatable(data,rownames = F,colnames=c("Топ-10 корзин","Кол-во"),options = list(dom = 't'))
    })
    
    output$net_hourly <- renderPlotly({
        net_hourly[,2:ncol(net_hourly)] <- apply(net_hourly[,2:ncol(net_hourly)],2,
                                                 function(col) round(col/rowSums(net_hourly[,2:ncol(net_hourly)]),2))
        pl <- plot_ly(x=as.character(net_hourly$X),y=net_hourly$X9_10,name="9-10")
        pl <- add_trace(pl,x=as.character(net_hourly$X),y=net_hourly$X11_12,name="11-12")
        pl <- add_trace(pl,x=as.character(net_hourly$X),y=net_hourly$X13_14,name="13-14")
        pl <- add_trace(pl,x=as.character(net_hourly$X),y=net_hourly$X15_16,name="15-16")
        pl <- add_trace(pl,x=as.character(net_hourly$X),y=net_hourly$X17_18,name="17-18")
        pl <- add_trace(pl,x=as.character(net_hourly$X),y=net_hourly$X19_20,name="19-20")
        pl <- add_trace(pl,x=as.character(net_hourly$X),y=net_hourly$X21_22,name="21-22")
        pl <- layout(pl, title = "Оборот по часам", xaxis = list(title = "Month"),
                     yaxis = list(title = "Оборот"))
        pl
    })
    
    output$net_trend_info <- renderDataTable({
        data <- data.frame(Weekdays = c("понедельник","вторник","среда","четверг",
                                        "friday","суббота","воскресенье"),
                           Percent = round(colMeans(net_weekly[(nrow(net_weekly)-13):(nrow(net_weekly)),2:8]),2),
                           Trend = c(GetTrendInfo(net_weekly$Md),GetTrendInfo(net_weekly$Tu),
                                     GetTrendInfo(net_weekly$We),GetTrendInfo(net_weekly$Th),
                                     GetTrendInfo(net_weekly$Fr),GetTrendInfo(net_weekly$Sa),
                                     GetTrendInfo(net_weekly$Su)),
                           RandomSd = c(round(GetRandomSd(net_weekly[,2]),2),round(GetRandomSd(net_weekly[,3]),2),
                                        round(GetRandomSd(net_weekly[,4]),2),round(GetRandomSd(net_weekly[,5]),2),
                                        round(GetRandomSd(net_weekly[,6]),2),round(GetRandomSd(net_weekly[,7]),2),
                                        round(GetRandomSd(net_weekly[,8]),2)))
        datatable(data,rownames = F,colnames=c("День недели","Часть оборота","Тренд","Несовпадение"),options = list(dom = 't'))
    })
    
    output$office_trend_info <- renderDataTable({
        data <- readRDS(paste0("weekly/weekly_",input$input_office,".rds"))
        data <- data[rowSums(data[,2:8])>0,]
        data <- data.frame(Weekdays = c("понедельник","вторник","среда","четверг",
                                        "friday","суббота","воскресенье"),
                           Percent = round(colMeans(data[(nrow(data)-13):(nrow(data)),2:8]),2),
                           Trend = c(GetTrendInfo(data[,2]),GetTrendInfo(data[,3]),
                                     GetTrendInfo(data[,4]),GetTrendInfo(data[,5]),
                                     GetTrendInfo(data[,6]),GetTrendInfo(data[,7]),
                                     GetTrendInfo(data[,8])),
                           RandomSd = c(round(GetRandomSd(data[,2]),2),round(GetRandomSd(data[,3]),2),
                                        round(GetRandomSd(data[,4]),2),round(GetRandomSd(data[,5]),2),
                                        round(GetRandomSd(data[,6]),2),round(GetRandomSd(data[,7]),2),
                                        round(GetRandomSd(data[,8]),2)))
        
        datatable(data,rownames = F,colnames=c("День недели","Часть оборота","Тренд","Несовпадение"),options = list(dom = 't'))
    })
    
    pred_pl <- eventReactive(input$do_prediction,{
        dates <- seq.Date(as.Date(input$date_pred[1]),as.Date(input$date_pred[2]),"day")
        if (!input$num_or_text){
            em1 <- as.numeric(unlist(strsplit(input$sh1_text," ")))
            em2 <- as.numeric(unlist(strsplit(input$sh2_text," ")))
            em3 <- as.numeric(unlist(strsplit(input$sh_all_text," ")))
        }
        else{
            em1 <- rep(input$shift_1,length(dates))
            em2 <- rep(input$shift_2,length(dates))
            em3 <- rep(input$shift_all,length(dates))
        }
        if (input$input_pr_office=="Cеть") data <- readRDS("predict_data/predict_data.rds")
        else data <- readRDS(paste0("predict_data/predict_data_",input$input_pr_office,".rds"))
        #new prediction
        TimeSeries <- msts(data$Trans_sum, seasonal.periods = c(180)) # period could be specified
        Trend <- decompose(TimeSeries)$trend
        Seasonality <- decompose(TimeSeries)$seasonal
        Random <- decompose(TimeSeries)$random
        #trend by arima
        pred_trend <- forecast(auto.arima(Trend), h = length(dates))
        data$random <- Random
        model <- lm(data = data, formula = random ~ shift_all_em + Num_offices)
        if (F) {
            fit <- train(Trans_sum ~ ., data = data[,c(2:7,10,11,12,17)], method = "xgbTree", 
                     preProcess =c("center", "scale"),
                     trControl = trainControl(method = "cv", number = 5, classProbs = T))
        }
        newdf <- data.frame(Date = as.character(as.Date(dates)),
                            Weekday = weekdays(as.Date(dates)),
                            Day_of_month = strftime(as.Date(dates),"%d"),
                            Nweek = strftime(as.Date(dates),"%W"),
                            Month = strftime(as.Date(dates),"%m"),
                            Num_offices = input$num_offices,
                            Num_employees = em1+em2+em3,
                            shift1_em = em1,
                            shift2_em = em2,
                            shift_all_em = em3)
        newdf[,3:6] <- apply(newdf[,3:6],2,function(x) as.numeric(as.character(x)))
        #pred <- predict(fit,newdata = newdf)
        pred_rand <- predict(model, newdf)
        pred <- pred_trend$mean + pred_rand + Seasonality[1:length(dates)]
        pred
    })
    output$plot_pred <- renderPlotly({
        if (input$input_pr_office=="Cеть") data <- readRDS("predict_data/predict_data.rds")
        else data <- readRDS(paste0("predict_data/predict_data_",input$input_pr_office,".rds"))
        dates <- seq.Date(as.Date(input$date_pred[1]),as.Date(input$date_pred[2]),"day")
        pl <- plot_ly(x=as.Date(data$Date),y=data$Trans_sum,name="Реальные данные")
        pl <- add_trace(pl,x=as.Date(dates),y=pred_pl(),name="Прогноз")
        pl <- layout(pl, title = "Прогноз", xaxis = list(title = "Дата"),
                     yaxis = list(title = "Оборот"))
        pl
    })
    
    output$check_dynamics <- renderPlotly({
        data <- readRDS("check_dynamics/check_dynamics.rds")
        data[,2:4] <- apply(data[,2:4],2,function(x) as.numeric(as.character(x)))
        data$Nweek <- strftime(as.Date(data$Date),"%W")
        data$Year <- strftime(as.Date(data$Date),"%Y")
        data <- unite_(data,"year_week",c("Year","Nweek"))
        newdf <- data.frame(Week=unique(data$year_week),
                            Class1=as.vector(tapply(data$Class1,INDEX = data$year_week,sum)),
                            Class2=as.vector(tapply(data$Class2,INDEX = data$year_week,sum)),
                            Class3=as.vector(tapply(data$Class3,INDEX = data$year_week,sum)))
        pl <- plot_ly(data = newdf, x=Week, y=Class1,name="Класс 1")
        pl <- add_trace(pl,data = newdf, x=Week, y=Class2,name="Класс 2")
        pl <- add_trace(pl,data = newdf, x=Week, y=Class3,name="Класс 3")
        pl <- layout(pl, title = "Кол-во чеков разных классов", xaxis = list(title = "Дата"),
                     yaxis = list(title = "Кол-во чеков"))
        pl
    })
    
    output$office_check_dynamics <- renderPlotly({
        data <- readRDS(paste0("check_dynamics/check_dynamics_",input$input_office,".rds"))
        data[,2:4] <- apply(data[,2:4],2,function(x) as.numeric(as.character(x)))
        data$Nweek <- strftime(as.Date(data$Date),"%W")
        data$Year <- strftime(as.Date(data$Date),"%Y")
        data <- unite_(data,"year_week",c("Year","Nweek"))
        newdf <- data.frame(Week=unique(data$year_week),
                            Class1=as.vector(tapply(data$Class1,INDEX = data$year_week,sum)),
                            Class2=as.vector(tapply(data$Class2,INDEX = data$year_week,sum)),
                            Class3=as.vector(tapply(data$Class3,INDEX = data$year_week,sum)))
        pl <- plot_ly(data = newdf, x=Week, y=Class1,name="Класс 1")
        pl <- add_trace(pl,data = newdf, x=Week, y=Class2,name="Класс 2")
        pl <- add_trace(pl,data = newdf, x=Week, y=Class3,name="Класс 3")
        pl <- layout(pl, title = "Кол-во чеков разных классов", xaxis = list(title = "Дата"),
                     yaxis = list(title = "Кол-во чеков"))
        pl
    })
    
    output$offices_parts <- renderPlotly({
        cols <- colnames(office_part)
        pl <- plot_ly(x=office_part$year_week, y=office_part[,2],name="X4")
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,3],name=cols[3])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,4],name=cols[4])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,5],name=cols[5])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,6],name=cols[6])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,7],name=cols[7])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,8],name=cols[8])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,9],name=cols[9])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,10],name=cols[10])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,11],name=cols[11])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,12],name=cols[12])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,13],name=cols[13])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,14],name=cols[14])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,15],name=cols[15])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,16],name=cols[16])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,17],name=cols[17])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,18],name=cols[18])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,19],name=cols[19])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,20],name=cols[20])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,21],name=cols[21])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,22],name=cols[22])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,23],name=cols[23])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,24],name=cols[24])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,25],name=cols[25])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,26],name=cols[26])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,27],name=cols[27])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,28],name=cols[28])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,29],name=cols[29])
        pl <- add_trace(pl, x=office_part$year_week, y=office_part[,30],name=cols[30])
        pl <- layout(pl, title = "Вклад офисов в общий оборот", xaxis = list(title = "Дата"),
                     yaxis = list(title = "Часть оборота за день"))
        pl
    })
}

##############################################################################
############################################################# U I

m_Header <- dashboardHeader( 
    title = "Analytics"
)

m_Sidebar <- dashboardSidebar( 
    sidebarMenu(
        id = "tabs",
        menuItem("Сеть",tabName = "net_tab", icon = icon("scissors")),
        menuItem("Офисы",tabName = "offices_tab", icon = icon("building-o")),
        menuItem("Прогнозирование",tabName = "predict_tab", icon = icon("line-chart"))
        
    )
)

################################################################### T A B 

offices_tab <- tabItem(tabName = "offices_tab",
                      fluidRow(
                          column(6,box(selectInput("input_office","Input office:",choices = office$office_id,
                                               selected = office$office_id[1]),status = "danger",width = 12),
                                 box(dataTableOutput("office_top5service"),status = "warning",width = 12)
                          ),
                          column(6,box(dataTableOutput("office_stats"),status = "warning",width = 12),
                                   box(dataTableOutput("office_trend_info"),status = "warning",width = 12))
                      ),
                      fluidRow(
                          box(plotlyOutput("office_week_plot"),status = "warning",width = 12)
                      ),
                      fluidRow(
                          box(plotlyOutput("office_hourly_plot"),status = "warning",width = 12)
                      ),
                      fluidRow(
                          box(plotlyOutput("office_revenue_plot"),status = "warning",width = 12)
                      ),
                      fluidRow(
                          box(plotlyOutput("office_check_dynamics"),status = "warning",width = 12)
                      ),
                      fluidRow(
                          box(dataTableOutput("office_top5basket"),status = "warning",width = 12)
                      )
                      
)

################################################################### T A B 

net_tab <- tabItem(tabName = "net_tab",
                   fluidRow(
                       column(12,box(plotlyOutput("net_alltime"),status = "warning",width = 12))
                   ),
                   fluidRow(
                       box(plotlyOutput("offices_parts"),status = "warning",width = 12)
                   ),
                   fluidRow(
                       box(plotlyOutput("net_hourly"),status = "warning",width = 12)
                   ),
                   fluidRow(
                       box(plotlyOutput("net_week"),status = "warning",width = 12)
                   ),
                   fluidRow(
                       box(dataTableOutput("net_trend_info"),status = "warning",width = 12)
                   ),
                   fluidRow(
                       box(plotlyOutput("check_dynamics"),status = "warning",width = 12)
                   ),
                   fluidRow(
                       column(6,box(plotlyOutput("net_revenue2016"),status = "warning",width = 12)),
                       column(6,box(plotlyOutput("net_revenue_total"),status = "warning",width = 12))
                   ),
                   fluidRow(
                       column(6,box(plotlyOutput("net_period"),status = "warning",width = 12)),
                       column(6,box(plotlyOutput("net_zero"),status = "warning",width = 12))
                   ),
                   fluidRow(
                       #column(5,box(dataTableOutput("net_top5service"),status = "warning",width = 12)),
                       column(5,box(dataTableOutput("net_check_service"),status = "warning",width = 12)),
                       column(7,box(dataTableOutput("net_top5basket"),status = "warning",width = 12))
                   )
)

################################################################### T A B 

predict_tab <- tabItem(tabName = "predict_tab",
                    fluidRow(
                        box(column(6,selectInput("input_pr_office","Input office or net:",choices = c("Cеть",office$office_id),
                                                 selected = "Cеть"),
                                   box(textInput("sh1_text","Employees at 1st shift (daily)",value = "2 0 3 3 6 4 3 3 0 2 4 2 2 2 3"),
                                       textInput("sh2_text","Employees at 2nd shift (daily)",value = "2 0 5 4 5 11 5 3 1 4 7 6 4 8 2"),
                                       textInput("sh_all_text","Employees at all-day shift (daily)",value = "28 31 31 32 32 32 30 27 29 17 30 31 32 31 32"),
                                       status = "danger",width = 12)),  
                            column(6,
                                   fluidRow(dateRangeInput("date_pred","Period to predict",start=as.Date("2016-03-19"),
                                                    end = as.Date("2016-04-02"),min=as.Date("2012-12-16"))),
                                   fluidRow(
                                       column(6,numericInput("shift_1", "Employees at 1st shift",min=0,step=1,value = 1),
                                          numericInput("shift_all", "Employees at all-day shift",min=0,step=1,value = 1)),
                                       column(6,numericInput("shift_2", "Employees at 2nd shift",min=0,step=1,value = 1),
                                          checkboxInput("num_or_text","Use constant number of employees instead of text input",
                                                 value=F))
                                       ),
                                   fluidRow(column(6,numericInput("num_offices", "Active offices",min=0,step=1,value = 28)),
                                            column(6,actionButton("do_prediction","make prediction",icon = icon("spinner"))))
                            ),
                            status = "danger",width = 12)
                    ),
                    fluidRow(plotlyOutput("plot_pred"))
)

################################################################### E N D  T A B S 

m_Body <- dashboardBody(
    tabItems(offices_tab,net_tab,predict_tab)
)

ui <- dashboardPage(m_Header, m_Sidebar, m_Body, skin = "yellow")
#main function
shinyApp(ui, server)