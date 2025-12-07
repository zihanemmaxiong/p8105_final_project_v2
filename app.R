

library(tidyverse)
library(sf)
library(tigris)
library(leaflet)
library(shiny)
library(viridisLite)
library(stringr)
library(htmltools)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

zip_year_month <- readr::read_csv("data/zip_year_month_merged.csv") |>
  dplyr::mutate(
    zipcode = as.character(zipcode),
    zipcode = stringr::str_pad(zipcode, width = 5, pad = "0")
  )

# 0.2 按 zipcode × year 聚合到“年”级别
zip_year <- zip_year_month |>
  dplyr::group_by(zipcode, year) |>
  dplyr::summarise(
    rat_count_year        = sum(rat_count_zip_month, na.rm = TRUE),
    violation_count_year  = sum(violation_count_zip_month, na.rm = TRUE),
    inspection_count_year = sum(inspection_count_zip_month, na.rm = TRUE),
    total_pop             = dplyr::first(total_pop),
    poverty_rate          = dplyr::first(poverty_rate),
    population_density    = dplyr::first(population_density),
    median_household_income = dplyr::first(median_household_income),
    .groups = "drop"
  ) |>
  # 计算 per 10,000 / per inspection（后面依然叫 per_rest，和前文保持名称一致）
  dplyr::mutate(
    rats_per_10000_year = dplyr::if_else(
      !is.na(total_pop) & total_pop > 0,
      rat_count_year / (total_pop / 10000),
      NA_real_
    ),
    viol_per_rest_year = dplyr::if_else(
      inspection_count_year > 0,
      violation_count_year / inspection_count_year,
      NA_real_
    )
  ) |>
  # 改成后面 Shiny 里统一用的列名
  dplyr::rename(
    rats_per_10000 = rats_per_10000_year,
    viol_per_rest  = viol_per_rest_year
  )

# 0.3 获取 NY ZCTA polygon（ZIP 边界）
ny_zcta <- tigris::zctas(state = "NY", year = 2010) |>
  dplyr::rename(zipcode = ZCTA5CE10) |>
  dplyr::mutate(
    zipcode = as.character(zipcode),
    zipcode = stringr::str_pad(zipcode, width = 5, pad = "0")
  )

# 0.4 合并成 ZIP × 年 的 sf 对象，并做 3×3 分类
ny_zip_year_sf <- ny_zcta |>
  dplyr::left_join(zip_year, by = "zipcode") |>
  dplyr::filter(!is.na(year), year >= 2019, year <= 2024) |>
  dplyr::mutate(
    viol_tertile = dplyr::ntile(viol_per_rest, 3),
    rats_tertile = dplyr::ntile(rats_per_10000, 3),
    bi_code = dplyr::if_else(
      is.na(viol_tertile) | is.na(rats_tertile),
      NA_character_,
      paste0("V", viol_tertile, "R", rats_tertile)
    )
  )

# 0.5 转成 WGS84 方便 leaflet（经纬度）
ny_zip_year_leaf <- sf::st_transform(ny_zip_year_sf, 4326)

# 0.6 3×3 cividis palette（和 5.7 保持一致）
biv_cols <- viridisLite::viridis(9, option = "cividis")
names(biv_cols) <- c(
  "V1R1","V2R1","V3R1",
  "V1R2","V2R2","V3R2",
  "V1R3","V2R3","V3R3"
)

# 构造 3×3 legend：横轴 Violations (Low→High)，纵轴 Rats (Low→High，向上)
biv_mat_codes <- matrix(
  c("V1R3","V2R3","V3R3",
    "V1R2","V2R2","V3R2",
    "V1R1","V2R1","V3R1"),
  nrow = 3, byrow = TRUE
)
biv_colors_mat <- matrix(biv_cols[biv_mat_codes], nrow = 3)

legend_html <- htmltools::tags$div(
  style = "background: white; padding: 6px 8px; border-radius: 4px;
           box-shadow: 0 0 5px rgba(0,0,0,0.3); font-family: sans-serif;",
  htmltools::tags$div(
    style = "font-weight: bold; margin-bottom: 4px; font-size: 11px;",
    "Rat sightings per 10,000 \u00d7 Violations per restaurant"
  ),
  htmltools::tags$table(
    style = "border-collapse: collapse; font-size: 11px;",
    # 第一行：横轴标题（Violations）
    htmltools::tags$tr(
      htmltools::tags$td(style = "padding-right: 4px;"),  # 左上角空白
      lapply(c("Low","Med","High"), function(x)
        htmltools::tags$td(
          style = "text-align: center; padding: 0 4px;",
          x
        ))
    ),
    # 三行颜色格 + 纵轴标签（Rats: High, Med, Low）
    lapply(1:3, function(i) {
      htmltools::tags$tr(
        htmltools::tags$td(
          style = "padding-right: 4px;",
          c("High","Med","Low")[i]
        ),
        lapply(1:3, function(j) {
          htmltools::tags$td(
            style = paste0(
              "width: 18px; height: 18px;",
              "background:", biv_colors_mat[i, j], ";"
            )
          )
        })
      )
    })
  )
)
legend_html <- as.character(legend_html)




# app.R (UI 部分) -------------------------------------------------------------

ui <- shiny::fluidPage(
  titlePanel("Interactive Map: Rat Sightings × Violations (ZIP × Year)"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "year", "Year",
        min   = min(ny_zip_year_leaf$year, na.rm = TRUE),
        max   = max(ny_zip_year_leaf$year, na.rm = TRUE),
        value = min(ny_zip_year_leaf$year, na.rm = TRUE),
        step  = 1,
        sep   = ""
      ),
      selectInput(
        "fill_var", "Color variable",
        choices = c(
          "Bivariate category (3×3)"  = "bi",
          "Rat sightings per 10,000"  = "rats",
          "Violations per restaurant" = "viol"
        ),
        selected = "bi"
      ),
      helpText(
        "Use the slider to change year. ",
        "Click a ZIP polygon on the map to see detailed values on the right."
      )
    ),
    mainPanel(
      leafletOutput("map", height = "600px"),
      tags$hr(),
      h4("Clicked ZIP information"),
      verbatimTextOutput("info")
    )
  )
)




# app.R (Server 部分) ---------------------------------------------------------
server <- function(input, output, session) {
  
  ## palettes for continuous & bivariate
  pal_rats <- colorNumeric(
    "viridis",
    domain  = ny_zip_year_leaf$rats_per_10000,
    na.color = "transparent"
  )
  pal_viol <- colorNumeric(
    "viridis",
    domain  = ny_zip_year_leaf$viol_per_rest,
    na.color = "transparent"
  )
  pal_bi   <- colorFactor(
    biv_cols,
    domain  = ny_zip_year_leaf$bi_code,
    na.color = "transparent"
  )
  
  ## 按年份过滤数据
  filtered_data <- reactive({
    ny_zip_year_leaf |>
      dplyr::filter(year == input$year)
  })
  
  ## 初始底图
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -73.94, lat = 40.72, zoom = 10)
  })
  
  ## 根据选择的填色变量更新 polygon + legend
  observe({
    dat <- filtered_data()
    if (nrow(dat) == 0) return()
    
    if (input$fill_var == "rats") {
      pal <- pal_rats
      col_values   <- dat$rats_per_10000
      legend_title <- "Rat sightings per 10,000"
    } else if (input$fill_var == "viol") {
      pal <- pal_viol
      col_values   <- dat$viol_per_rest
      legend_title <- "Violations per restaurant"
    } else {  # bivariate 3×3
      pal <- pal_bi
      col_values   <- dat$bi_code
      legend_title <- "Bivariate (V × R tertiles)"
    }
    
    map_proxy <- leafletProxy("map", data = dat) |>
      clearShapes() |>
      clearControls() |>
      addPolygons(
        fillColor   = pal(col_values),
        color       = "#444444",
        weight      = 0.5,
        opacity     = 1,
        fillOpacity = 0.7,
        layerId     = ~zipcode,
        label       = ~paste0("ZIP: ", zipcode),
        highlightOptions = highlightOptions(
          weight = 2,
          color  = "black",
          bringToFront = TRUE
        )
      )
    
    # 连续变量用默认 legend；双变量用 3×3 方块 legend
    if (input$fill_var == "bi") {
      map_proxy |>
        addControl(
          html     = legend_html,   # 前面定义好的 3×3 legend_html
          position = "bottomright"
        )
    } else {
      map_proxy |>
        addLegend(
          position = "bottomright",
          pal      = pal,
          values   = col_values,
          title    = legend_title,
          opacity  = 0.7
        )
    }
  })
  
  ## 点击 polygon 后显示信息
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    dat   <- filtered_data()
    row_dat <- dat[dat$zipcode == click$id, ]
    if (nrow(row_dat) == 0) return()
    
    txt <- sprintf(
      paste(
        "Year: %d",
        "ZIP: %s",
        "Rat sightings per 10,000: %.1f",
        "Violations per restaurant: %.3f",
        sep = "\n"
      ),
      row_dat$year[1],
      row_dat$zipcode[1],
      row_dat$rats_per_10000[1],
      row_dat$viol_per_rest[1]
    )
    
    output$info <- renderText(txt)
  })
}




# app.R (启动) ----------------------------------------------------------------

shinyApp(ui, server)