# ============================================================
# 10 R Shiny — 최종 통합 앱 (배포용, app.R 한 개)
# ------------------------------------------------------------
# 폴더 구조
#   murders_Rshiny_app/          ← 이 폴더 이름이 배포 URL이 됨
#   ├── app.R             ← 이 파일
#   └── murders.csv       ← 앱이 읽는 데이터 (app.R과 같은 폴더)
# ============================================================


# ============================================================
# [준비 — 최초 1회만] CSV 데이터 파일 만들기
# ------------------------------------------------------------
# 아래 블록은 "한 번만" 실행해서 app.R과 같은 폴더에 murders.csv를
# 만들어 두는 용도입니다. 파일이 생기면 다시 실행할 필요가 없고,
# 배포할 때 app.R 안에서 실행되면 안 되므로 주석(#) 처리해 둡니다.
#
# # install.packages("dslabs")   # murders 원본 데이터가 든 패키지
# library(dslabs)
# data(murders)
# write.csv(murders, "murders.csv", row.names = FALSE)   # 같은 폴더에 저장
#
# ============================================================


# ------------------------------------------------------------
# 패키지 불러오기
# (app.R 안에는 install.packages()를 넣지 않음 → 배포 오류의 흔한 원인)
# ------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)


# ------------------------------------------------------------
# 데이터 불러오기: 반드시 '상대 경로'로
#   O  read.csv("murders.csv")                      → app.R이 있는 폴더 기준, 배포 후에도 동작
#   X  read.csv("C:/Users/me/Desktop/murders.csv")  → 내 컴퓨터에만 있는 경로, 배포하면 에러
# Run App / runApp() / 배포 서버 모두 app.R이 있는 폴더를 기준으로 실행되므로,
# 같은 폴더의 파일은 이름만 적으면 됩니다.
# ------------------------------------------------------------
murders <- read.csv("murders.csv")

# 살인율(인구 10만 명당) 변수를 앱 밖(맨 위)에서 한 번만 계산
# → 앱 실행 시 딱 1번만 수행되어 효율적
murders <- murders %>%
  mutate(
    region = as.character(region),
    rate   = total / population * 100000
  )

# y축 선택지: 화면에 보이는 이름 = 실제 변수명
y_choices <- c("살인 건수" = "total", "10만 명당 살인율" = "rate")


# ------------------------------------------------------------
# UI : 무엇을, 어디에 보여줄까
# ------------------------------------------------------------
ui <- page_sidebar(
  title = "미국 살인율 탐색",
  
  # ---- 왼쪽 사이드바: 입력 위젯 --------------------------------
  sidebar = sidebar(
    width = 300,
    
    selectInput(
      inputId  = "selected_region",
      label    = "지역",
      choices  = c("전체", sort(unique(murders$region))),
      selected = "Northeast"
    ),
    
    sliderInput(
      inputId = "pop_range",
      label   = "인구 범위 (명)",
      min     = 0,
      max     = max(murders$population),
      value   = range(murders$population),
      step    = 100000,
      sep     = ","
    ),
    
    radioButtons(
      inputId  = "y_var",
      label    = "y축 변수",
      choices  = y_choices,
      selected = "total"
    ),
    
    checkboxInput("show_labels", "주 약어 표시", value = TRUE),
    checkboxInput("show_lm",     "회귀선 표시",  value = FALSE),
    
    hr(),
    downloadButton("download_csv", "필터 결과 CSV 받기")
  ),
  
  # ---- 메인 영역 1행: 요약 숫자 3개 ------------------------------
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(title = "선택된 주",             value = textOutput("n_states")),
    value_box(title = "총 살인 건수",          value = textOutput("sum_total")),
    value_box(title = "평균 살인율(10만 명당)", value = textOutput("mean_rate"))
  ),
  
  # ---- 메인 영역 2행: 그래프(6칸) + 표(6칸) ----------------------
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("인구 대비 살인"),
      plotOutput("murder_plot", height = "380px")
    ),
    card(
      card_header("선택된 주 목록"),
      tableOutput("result")
    )
  ),
  
  # ---- 메인 영역 3행: 회귀모형 요약 -------------------------------
  card(
    card_header("회귀모형: y ~ 인구(백만 명)"),
    verbatimTextOutput("lm_summary")
  )
)


# ------------------------------------------------------------
# Server : 입력을 받아 계산하고 출력을 만든다
# ------------------------------------------------------------
server <- function(input, output) {
  
  # [핵심] reactive(): 두 입력 → 하나의 계산 → 여러 출력에서 재사용
  filtered_data <- reactive({
    out <- murders %>%
      filter(
        population >= input$pop_range[1],
        population <= input$pop_range[2]
      )
    
    if (input$selected_region != "전체") {
      out <- out %>% filter(region == input$selected_region)
    }
    
    out
  })
  
  # 현재 선택된 y축의 한글 이름 (그래프 축 제목에 사용)
  y_label <- reactive({
    names(y_choices)[y_choices == input$y_var]
  })
  
  # ---- 요약 숫자 ------------------------------------------------
  output$n_states <- renderText({
    paste0(nrow(filtered_data()), "개")
  })
  
  output$sum_total <- renderText({
    format(sum(filtered_data()$total), big.mark = ",")
  })
  
  output$mean_rate <- renderText({
    d <- filtered_data()
    if (nrow(d) == 0) return("-")
    round(mean(d$rate), 2)
  })
  
  # ---- 산점도 ---------------------------------------------------
  output$murder_plot <- renderPlot({
    d <- filtered_data()
    
    # 조건에 맞는 주가 없으면 에러 대신 안내 문구
    validate(need(nrow(d) > 0, "조건에 맞는 주가 없습니다. 인구 범위를 넓혀 보세요."))
    
    p <- ggplot(d, aes(x = population / 1e6, y = .data[[input$y_var]])) +
      geom_point(aes(color = region), size = 3) +
      labs(
        x     = "인구 (백만 명)",
        y     = y_label(),
        color = "지역"
      ) +
      theme_minimal(base_size = 14)
    
    if (input$show_labels) {
      p <- p + geom_text(aes(label = abb), vjust = -0.9, size = 3.5)
    }
    
    if (input$show_lm && nrow(d) >= 3) {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "#B57A23", formula = y ~ x)
    }
    
    p
  })
  
  # ---- 표 --------------------------------------------------------
  output$result <- renderTable(striped = TRUE, spacing = "xs", {
    filtered_data() %>%
      arrange(desc(.data[[input$y_var]])) %>%
      transmute(
        `주`         = state,
        `지역`       = region,
        `인구(백만)` = round(population / 1e6, 2),
        `살인 건수`  = as.integer(total),
        `살인율`     = round(rate, 2)
      )
  })
  
  # ---- 회귀모형 결과 (통계 결과를 실시간으로) ---------------------
  output$lm_summary <- renderPrint({
    d <- filtered_data()
    validate(need(nrow(d) >= 3, "회귀모형을 적합하려면 주가 3개 이상 필요합니다."))
    
    d$pop_million <- d$population / 1e6
    fit <- lm(reformulate("pop_million", response = input$y_var), data = d)
    
    cat("y =", y_label(), " / n =", nrow(d), "\n\n")
    print(round(summary(fit)$coefficients, 4))
    cat("\nR-squared:", round(summary(fit)$r.squared, 3), "\n")
  })
  
  # ---- CSV 다운로드 ----------------------------------------------
  # observeEvent + write.csv()는 '서버 컴퓨터'에 저장됩니다.
  # 배포한 앱에서 사용자가 파일을 받으려면 downloadHandler()를 씁니다.
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("murders_", input$selected_region, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}


# ------------------------------------------------------------
# 앱 실행
# ------------------------------------------------------------
shinyApp(ui, server)


# ============================================================
# [배포 — Console에서 실행, app.R 안에서 실행하지 않음]
# ------------------------------------------------------------
# 1) 폴더 안에 app.R 과 murders.csv 가 함께 있는지 확인
#    list.files("murders_Rshiny_app")
#
# 2) 로컬 테스트: 폴더째로 실행
#    shiny::runApp("murders_Rshiny_app")
#
# 3) 최초 1회: shinyapps.io 계정 토큰 연결
#    # install.packages("rsconnect")
#    # shinyapps.io → Account → Tokens → Show 에서 아래 명령 복사
#    # rsconnect::setAccountInfo(name = "...", token = "...", secret = "...")
#    #   ↑ Token/Secret 은 공개 금지
# 4) 배포: 폴더째로 업로드 (app.R 과 murders.csv 가 함께 올라감)
#    # rsconnect::deployApp("murders_Rshiny_app")   # 또는 RStudio 우상단 Publish 버튼
#    #   완료되면 https://계정명.shinyapps.io/murders_Rshiny_app 로 열림
# rsconnect::deployApp("G:/My Drive/2026_Jenny/2026_2_Teaching/2026_특수데이터사이언스대학원_시각화/강의자료/murders_Rshiny_app")
#
# 배포 전 체크리스트
#   [ ] 폴더 안에 app.R 과 murders.csv 가 함께 있는가?
#   [ ] 데이터를 상대 경로("murders.csv")로 읽는가? (C:/Users/... 절대 경로 X)
#   [ ] app.R 안에 install.packages() 가 없는가?
#   [ ] 쓰는 패키지를 모두 library() 로 불러오는가?
#   [ ] 폴더 안에 올리면 안 되는 파일(개인 데이터 등)은 없는가? → 폴더 전체가 서버로 올라감
# ============================================================


# Posit Cloud Connect
# rsconnect::writeManifest()
