library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(plotly)
library(quantmod)


data1<-read.csv("./out.csv" ,header=T,sep=",") 
site=readRDS("./site.RDS")

data2=merge(data1, site, by="id")
data<-mutate(data2,ROA=ni/assets)


ui <- dashboardPage(
  dashboardHeader(title="金融科技概念股"), 
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(tags$h4("研究說明"),  tabName = "description"),
      
      
      menuItem(tags$h4("變數說明"), tabName = "var"),
      menuItem(tags$h4("公司資訊查詢"),tabName="per"
              
               
               
              
               
               
               ),
      
      
      menuItem(tags$h4("圖表分析"), 
               
               menuSubItem("公司股票比較", tabName="compare", icon=icon("fas fa-chart-line")) ,
               menuSubItem("公司各項資料比較", tabName="analysis", icon=icon("fas fa-chart-line")) 
               ),
      
      menuItem(tags$h4("公司地圖"), tabName = "maps"),
      menuItem(tags$h4("程式檔說明"), tabName = "intro"),
      
    
     tags$div( style="position: fixed; top: 85%;padding:10px;","組員",br(),"05171165黃姿瑜",br(),"05142420陳書棋")
                          
      
    )),
  dashboardBody(
    
    
    tags$head(tags$style(HTML('
                                /* logo */
                          .skin-blue .main-header .logo {
                          background-color: #3d3d5c;
                          }
                          
                          /* logo when hovered */
                          .skin-blue .main-header .logo:hover {
                          background-color: #3d3d5c;
                          }
                          
                          /* 上面那條 */
                          .skin-blue .main-header .navbar {
                          background-color: #000000;
                          }        
                          
                          /* 右邊 */
                          .skin-blue .main-sidebar {
                          background-color: #000000;
                          }
                          
                          /* 第一個一開始顏色 */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                          background-color: #000000;
                          }
                          
                          /* 字體顏色 */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                          background-color: #000000;
                          color: #FFFFFF;
                          }
                          
                          /* other links in the sidebarmenu when hovered */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                          background-color: #3d3d5c;
                          }
                          /* toggle button when hovered  */                    
                          .skin-blue .main-header .navbar .sidebar-toggle:hover{
                          background-color: #000000 ;
                          }
            
                      .table1{border:2px  black;width:auto;position:relative;left:25%;

}

                          td{padding:10px;}
                  tr:nth-child(1){background-color:	#708090;}
.tr2{background-color:#F8F8FF;}
                    tr:nth-child(even){background-color:#DCDCDC;} 

.pic2{
position:absolute;

transform:translate(600px,100px);


}

.idn{font-size: 20px;text-indent:36px;line-height:48px;padding:50px;margin:0;
position:relative;


}




                          '))),
    
    tabItems(
      tabItem("description",
              
              
              #頁籤
              tabsetPanel(
                #頁籤1
                tabPanel("研究動機",
                         column(8, tags$div(class="idn","   近年來，Fintech (金融科技)的話題和應用越來越火熱，現在已經成為財經和科技業最炙手可熱的趨勢。Fintech是結合Finance 與 Technology ，
                                           是以互聯網科技思惟來改造傳統金融業，運用科技手段使金融服務變得更有效率。",br(),br(),"由於Fintech發展領域相當多元，根據世界經濟論壇（WEF）分類，
                                           將Fintech分類支付、保險、存貸、籌資，投資管理以及數據分析等六大領域。Fintech的功能包括行動支付、P2P借貸、銀行貸款、投資理財、保險證券、股票交易等。
                                           金融科技概念股在過去幾年中，各大金融業都在跟進，例如花旗集團 (Citigroup, C-US)、高盛 (Goldman Sachs, GS-US) 和摩根大通 (JPMorgan Chase, JPM-US)
                                           ，但是各公司的使用策略皆有所不同，我們想知道在2018年眾多金融科技概念股公司中，他們之間的績效差異。" )), 
                         
                         column(4, img(src="fintech-one.jpg",height =400, width =400 ,style="position:absolute;"
                         ))
                         ),#tabpanel
                tabPanel("研究目的",
                         column(12, tags$div(style="font-size: 20px;text-indent:16px　 ;line-height: 48px;position:absolute;top:30px;left:5%;
                                                        ","
                                           針對31家上市(櫃)之金融科技概念股公司",br(),
                                          tags$ul( tags$li("製作31家金融科技概念股公司資訊查詢系統"),
                                                   tags$li("根據31家金融科技概念股公司做股票分析"),
                                           tags$li("針對31家金融科技概念股公司財報科目分析",
                                          tags$ul(
                                            tags$li("總資產"), tags$li("股東權益總額"), tags$li("營業費用"), tags$li("本期稅後淨利"), tags$li("營業收入"),tags$li("	資產報酬率")
                                          )
                                                   ),
                                           
                                           tags$li("查詢31家金融科技概念股公司所在位置")
                                           )
                                           
                                           
                         )), 
                         
                         column(12, tags$div(class="pic2",img(src="fin.jpg",height = 350, width =500 
                         )))
                )#tabpanel
                ,
                
                tabPanel("資料來源",
                         
                         tags$h3(style="font-size: 20px;line-height: 48px;","從",tags$a("MoneyDJ裡的fintech概念股",href=
                                                                                    "http://newjust.masterlink.com.tw/HotProduct/HTML/Detail.xdjhtm?A=PA332-2.html")
                                 ,"依照獲利能力排序，挑選出獲利較好的台灣公司作為研究對象。",br()
                                 ),
                         
                         tags$h3(style="font-size: 20px;line-height: 48px;","
                                           再從台灣經濟新報TEJ資料庫查詢各家公司的財報資料"),
                         
                         
                         url <- a(tags$h4(tags$u("前往TEJ資料庫網站")), href="https://www.tej.com.tw/twsite/TEJ%E8%B3%87%E6%96%99%E5%BA%AB/tabid/164/language/zh-TW/Default.aspx"),
                         
                        tags$a( img(src="RT.gif"),href="https://www.tej.com.tw/twsite/TEJ%E8%B3%87%E6%96%99%E5%BA%AB/tabid/164/language/zh-TW/Default.aspx")
                         
                         
                         
                         
                         )
                
                
                
              )#tabsetpanel
              ,
              
              
              
              
              
              
              
              
              br()
      ),
      tabItem("var",
              
              tabsetPanel(
                tabPanel("變數介紹",
                         tags$ul( style=" line-height: 40px;padding:25px;font-size:18px;margin-top:25px;",
                                   tags$li("總資產：指企業擁有或可控制的能以貨幣計量的經濟資源，包括各種財產、債權和其他權利。"),
                                   tags$li("股東權益總額：指股東對企業凈資產的權利，為全部資產減全部負債後的凈資產。包括：實收資本（股本）、資本公積、盈餘公積和未公配利潤。反映股東權益的收益水平。"),
                                     tags$li("營業費用：指的是公司營運過程中所衍生出來的費用，例如員工薪資、租金等。"),
                               tags$li("本期稅後淨利：稅前淨利扣除所得稅後的淨利，為公司最後的盈餘成果。"),
                                   tags$li("營業收入：指公司因正常商業活動，所獲得之收入，通常是經由提供產品及服務所得。"),
                                     tags$li("資產報酬率：是用來衡量每單位資產創造多少淨利潤的指標，也可以解釋為企業利潤額與企業平均資產的比率。")
                                   
                                   
                         ) 
                ),
                tabPanel("變數資料",
              tags$table(border=1,class="table1",
                         
                         
                         tags$tr(
                           tags$td("代碼"),tags$td("company"), tags$td("assets"),tags$td("equity"),tags$td("fee"),tags$td("ni"),tags$td("ic"),tags$td("ROA")),
                         
                         tags$tr(class="tr2",
                           tags$td("名稱"),tags$td("公司名稱"), tags$td(" 總資產"), tags$td("股東權益總額"), tags$td("營業費用"), tags$td("本期稅後淨利"), tags$td("營業收入"),tags$td("資產報酬率")
                         )),
              br(),
              
             
              
            dataTableOutput("mytable")
      ))),
      #公司資料查詢
      
      tabItem("compare",
              
              
              selectInput(inputId="company1", label="公司名稱",choices=unique(data$company)
              ),
              selectInput(inputId="company2", label="公司名稱",choices=unique(data$company),
                          selected=data$company[2]),
              
              
              radioButtons(inputId="stockvar", label="選擇要分析的項目",
                           
                           choices=c("開盤價"="open", "最高價"="high","最低價"="low",
                                     "收盤價"="close","成交量"="volume","調整後"="adjusted"),
                           selected="open",inline =TRUE),
              
              
              tags$div(style="color:blue;","以下資料為日資料(周休二日除外)"),
              plotlyOutput(outputId="stock")
                                  
                          
                         
                         
                         
                         
                         
                         
                ),
      
     
      tabItem("analysis",
              tabsetPanel( 
                tabPanel("說明",
                       tags$h4( style=" line-height: 40px;padding:10px;font-size:20px;",
                                "分析公司之各變數之間的關係",br(),
                                "並計算其相關係數",br(),
                        "1.請選擇X軸、Y軸之變數",br(),
                        "2.請選擇想要觀看的圖表類型(長條圖、散佈圖)",br(),
                        "3.求兩變數之相關係數",
                        "(P.S  兩個數值變數才可以求相關係數)"
                         
                         
                         ) 
                         
                         
                         
                         
                         
                         
                         ),
                tabPanel("圖表"
                         , column(4,selectInput(inputId="x", label="x軸變數",
                                       choices=c("公司名稱"="company", "總資產"="assets", "股東權益總額"="equity",
                                                 "營業費用"="fee","本期稅後淨利"="ni","收入"="ic","資產報酬率"="ROA") ,
                                       selected="company", multiple=FALSE)), 
                         
                         
                        
                         
                         column(4,selectInput(inputId="y", label="y軸變數",
                                     choices=c("公司名稱"="company", "總資產"="assets", "股東權益總額"="equity",
                                               "營業費用"="fee","本期稅後淨利"="ni","收入"="ic","資產報酬率"="ROA"),
                                     selected="assets")),
                         
                         column(4,selectInput(inputId="z", label="圖表呈現",
                                     choices=c("長條圖"="bar", "散佈圖"="scatter"),
                                     selected="bar")),
                         
                        br(),
                         br(),
                        
                         box(tags$h4("相關係數為"),tags$h4(textOutput("rl"),style="color:white;"),background = "black",
                                width = 6, height =NULL) ,
                         
                        br(),br(),br(),br(),
                       column(12,plotlyOutput(outputId="mainPlot"))  
                ))
                
                
         
              ),#tabitm
      
      tabItem("per",
              
          
              
              
              selectInput(inputId="c", label="公司名稱",choices=unique(data$company)
              ),
              
              column(4,selectInput(inputId="b", label="請選擇要查詢的項目",
                          choices=c( "總資產"="assets", "股東權益總額"="equity",
                                     "營業費用"="fee","本期稅後淨利"="ni","營業收入"="ic","資產報酬率"="ROA" )))
              
              ,column(8,box(tags$h4(textOutput("a"),textOutput("text")),background = "light-blue"
                   )),
              
              
           
              radioButtons(inputId="chk1", label="是否顯示公司股票資料",
                      
                           choices=c("是"="1", "否"="2"),
                           selected="2"),
              
              
              
              conditionalPanel(condition="input.chk1.indexOf('1')>-1",
              dateRangeInput(inputId="date", label="請選擇要查詢的時間",
                             start="2010-01-01", end=Sys.Date() , min="2010-01-01", max=Sys.Date() , 
                              startview="year",
                             separator = " 到 "),
             
              
              radioButtons(inputId="period", label="請選擇時間區隔",
                                 choices=c("周"="weekly","月"="monthly","季"="quarterly","年"="yearly"),
                                 selected="weekly", inline=TRUE),
            
                     checkboxGroupInput(inputId="checkbox", label="請點選要查詢的項目",
                                        choices=c("成交量Volume"="addVo()", 
                                                  "	真實波動幅度均值ATR"="addATR()", 
                                                  "	布林帶BBands"="addBBands()",
                                                  "	價格變動率Rate of Change"="addROC()",
                                                  "	相對強弱指標Relative Strength Indicator"="addRSI()",
                                                  "	順勢指標Commodity Channel Index"="addCCI()"),
                                        selected="1", inline=TRUE),
              
             
              
              br(),
              
              
            
            
              tags$div(tags$h4(style="color:red","資料讀取需要一點時間請稍後")),br(),
             
          
                plotOutput(outputId = "cs"))
              
              
              
              
              
              
              
              ),#tabitm
      
      tabItem("maps",
              
              selectInput(inputId ="aa",label="請選擇公司",choices = unique(data1$company)),
              
              leafletOutput("map")
              
      
      
      
      ),#tabitem 
      
      tabItem("intro",
              
              "out.csv為資料檔案",br(),"site.RDS為公司地圖檔案",br(),"www為圖片檔"
              
              
              
      )
      
               )#tabitms 
    
    )
  
)

server <- function(input, output, session) { 
  
  output$mytable =renderDataTable({
    data
  })
  
  
  output$a=renderText({
    
    a=input$c
    b=input$b
   
  
    Q3 = filter(data,company==a)%>%
      select(b)
   
    #轉換中文字//////////////////////////
      if(b=="assets"){
      b= "總資產"
    }else if(b=="equity")
    {b="股東權益總額"}else if(b=="fee"){
      b="營業費用"}else if(b=="ni"){
        b="本期稅後淨利"
      }else if(b=="ic"){b="營業收入"}
    else if(b=="ROA"){
      b="資產報酬率"
    }
    
    paste(a,"的",b,"為",Q3,"元")
      
  
  })
  
  output$mainPlot=renderPlotly({
    
    
    x<- input$x
    y<- input$y
    
    
    #plot_ly(data, x=~assets, y=~fee, type="bar")
    
    #plot_ly(data, x=~x, y=~y, type="bar")
    #plot_ly(data, x=~"assetes", y=~"fee", type="bar")
    #scatter散佈圖   折線圖+mode='lines'
    
    
    z<-input$z
    
    test = paste('plot_ly(data, x =~',x ,', y=~',y,', type = z,height=600)',sep="")
    
    eval(parse(text=test))
    
    
    
  })
  output$rl=renderText({
    
    var1<- select(data,input$x)
    var2<- select(data,input$y)
    #if(var1==company){Q2=("請選擇數值變數")
    #}else{Q2= cor(var1,var2)}
    if(input$x=="company"){
     ("兩個數值變數才可求相關係數")
      
    }else if(input$y=="company"){
      ("兩個數值變數才可求相關係數")
    }
    
    else{
    Q2= cor(var1,var2)
    
    }
    
    
    
    
  })
  
  
  #observeEvent(input$cs,{
  output$cs=renderPlot({
    #公司設定////////////////////////
    cmp=input$c
    nc<-nchar(input$c)
    
    a=nc-4
    b=nc-1
    
   stock= substring(cmp,a,b)
  st= as.character(stock)
  
   tws=paste(st,".TW",sep="")
    if(tws=="5490.TW"){
    tws= "5490.TWO"
    }
   
   
   if(tws=="5820.TW"){
     tws= "5820.TWO"
   }
   if(tws=="3687.TW"){
     tws= "3687.TWO"
   }
   
   if(tws=="8044.TW"){
     tws= "8044.TWO"
   }
   
   if(tws=="8076.TW"){
     tws= "8076.TWO"
   }
   if(tws=="6180.TW"){
     tws= "6180.TWO"
   }
   
   if(tws=="5478.TW"){
     tws= "5478.TWO"
   }
   if(tws=="3479.TW"){
     tws= "3479.TWO"
   }
  
   #////////////////////////////////////////////////////////
   
   
   #時間設定/////////////////////////////////////////////////
   date=input$date
   
   
   dt=as.character(date)
   
   dtbegin=dt[1]
   dtend=dt[2]
  # /////////////////////////////////
   #顯示其他項目ta
   ta=paste(unlist(input$checkbox),collapse = ";")
   
   if(ta==""){
     ta="NULL"
   }
  
   yy = getSymbols(Symbols=tws, from=dtbegin, to=dtend,
                   auto.assign=FALSE)          
   p=na.omit(yy) 
   
 whatime=paste("to",input$period,sep = ".")
   
  outplot=paste('chartSeries(',whatime,'(p), name=input$c,TA=ta)' ,sep="")
   
  
  
  eval(parse(text=outplot))
   
  }
)
  #地圖//////////////////////
  output$map <- renderLeaflet({  
    ad=filter(data,company==input$aa)
    adwd=
      substr(ad$site, start=1,stop=9)
   
    titlename=substring(input$aa,1,nchar(input$aa)-6)
    
    adgd=
      substr(ad$site, start=11,stop=20)
    
    gd=as.numeric(adgd)
    wd=as.numeric(adwd)
    
   
    m <- leaflet() %>%
      setView(lng=gd, lat=wd, zoom=16) %>%
      addTiles() %>%  # 加入內定的 OpenStreetMap 圖層
      addMarkers(lng=gd, lat=wd, popup=titlename)
    m  # 秀出地圖     
  })
  
  output$stock=renderPlotly({
    
    cmp1=input$company1
    nc<-nchar(cmp1)
    
    a=nc-4
    b=nc-1
    stock1= substring(cmp1,a,b)
   company1= paste(stock1,"TW",sep= ".")
   if(company1=="5490.TW"){
     company1= "5490.TWO"
   }
   
   
   if(company1=="5820.TW"){
     company1= "5820.TWO"
   }
   if(company1=="3687.TW"){
     company1= "3687.TWO"
   }
   
   if(company1=="8044.TW"){
     company1= "8044.TWO"
   }
   
   if(company1=="8076.TW"){
     company1= "8076.TWO"
   }
   if(company1=="6180.TW"){
     company1= "6180.TWO"
   }
 
   
   if(company1=="5478.TW"){
     company1= "5478.TWO"
   }
   if(company1=="3479.TW"){
     company1= "3479.TWO"
   }
    
    cmp2=input$company2
    nc<-nchar(cmp2)
    
    a=nc-4
    b=nc-1
    stock2= substring(cmp2,a,b)
    company2=paste(stock2,"TW",sep= ".")
    
    if(company2=="5490.TW"){
      company2= "5490.TWO"
    }
    
    
    if(company2=="5820.TW"){
      company2= "5820.TWO"
    }
    if(company2=="3687.TW"){
      company2= "3687.TWO"
    }
    
    if(company2=="8044.TW"){
      company2= "8044.TWO"
    }
   
    if(company2=="8076.TW"){
      company2= "8076.TWO"
    }
    if(company2=="6180.TW"){
      company2= "6180.TWO"
    }
    
    if(company2=="5478.TW"){
      company2= "5478.TWO"
    }
    if(company2=="3479.TW"){
      company2= "3479.TWO"
    }
    
    c1= getSymbols(Symbols=company1, from="2018-01-01", to=Sys.Date(),src="yahoo",auto.assign=FALSE)
    c2=getSymbols(Symbols=company2, from="2018-01-01", to=Sys.Date(),src="yahoo",auto.assign=FALSE)
    
    comp1 = c1
    company1.df = as.data.frame(comp1)
    company1.df = mutate(company1.df, row=as.Date(rownames(company1.df)))
    
    
    colnames(company1.df)=c("open","high","low","close","volume","adjusted","date")
    
    comp2 = c2
    company2.df = as.data.frame(comp2)
    company2.df = mutate(company2.df, row=as.Date(rownames(company2.df)))
    
    
    colnames(company2.df)=c("open","high","low","close","volume","adjusted","date")
    y=input$stockvar
    
    
    
    
    test = paste('plot_ly(company1.df, x =~date, y=~',y,', type ="scatter",
                 mode="lines",name=cmp1)%>% 
                 add_trace(data=company2.df,y=~',y,',name=cmp2, mode="lines") ',sep="")
    
    eval(parse(text=test))
    
    #plot_ly(company1.df,x=~row,y=~input$stockvar,
          # type='scatter', mode='lines',name=cmp1
    #)%>% add_trace(data=company2.df,y=~input$stockvar, name=cmp2, mode='lines') 
    
  })
  
}

shinyApp(ui, server)