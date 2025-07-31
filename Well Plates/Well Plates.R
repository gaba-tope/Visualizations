library(svglite)
library(ggplate)

# 384-well Assay Plate ----------------------------------

create_384_well_data <- function() {
  wells <- c()
  values <- c()
  labels <- c()
  
  # 384-well plate의 행과 열 생성
  rows <- LETTERS[1:16]  # A-P (A가 맨 위, P가 맨 아래)
  cols <- 1:24           # 1-24
  
  concentration_series <- c(
    0,       # A행 (맨 위)
    3e-06,   # B행
    1e-05,   # C행
    3e-05,   # D행
    1e-04,   # E행
    3e-04,   # F행
    0.001,   # G행
    0.003,   # H행
    0.01,    # I행
    0.03,    # J행
    0.1,     # K행
    0.3,     # L행
    1,       # M행
    3,       # N행
    10,      # O행
    30       # P행 (맨 아래)
  )
  
  label_series <- c(
    "0",      # A행
    "3E-06",  # B행
    "E-05",   # C행
    "3E-05",  # D행
    "E-04",   # E행
    "3E-04",  # F행
    "0.001",  # G행
    "0.003",  # H행
    "0.01",   # I행
    "0.03",   # J행
    "0.1",    # K행
    "0.3",    # L행
    "1",      # M행
    "3",      # N행
    "10",     # O행
    "30"      # P행
  )
  
  for(col in 1:24) {
    for(i in 1:16) {
      row <- rows[i]
      wells <- c(wells, paste0(row, col))
      
      # 각 행의 농도값 할당 (A행=1번째, P행=16번째)
      conc_value <- concentration_series[i]
      conc_label <- label_series[i]
      
      # 0 값은 로그 변환을 위해 매우 작은 값으로 변경
      if(conc_value == 0) {
        values <- c(values, 1e-08)
      } else {
        values <- c(values, conc_value)
      }
      
      labels <- c(labels, conc_label)
    }
  }
  
  # Log 변환 추가
  log_values <- log10(values)
  
  return(data.frame(
    well = wells, 
    Value = values, 
    LogValue = log_values,
    Label = labels, 
    stringsAsFactors = FALSE
  ))
}

# 384-well plate 데이터 생성
data_384 <- create_384_well_data()

# 384-well plate 플롯 생성 및 SVG로 저장
svglite("384_well_plate.svg", width = 12, height = 8)

p_384 <- plate_plot(
  data = data_384,
  position = well,
  value = LogValue,  # Log 변환된 값 사용
  label = Label,     # 원래 값으로 라벨 표시
  plate_size = 384,
  plate_type = "round",
  colour = c("#FFFFFF", "#FFE4E1", "#FFB6C1", "#FF69B4", "#FF1493", "#DC143C", "#8B008B"),
  label_size = 1.2,
  title = "384-Well Assay Plate",
  title_size = 16
) +
  ggplot2::labs(fill = "Conc\n[μM]") +
  ggplot2::scale_fill_gradientn(
    colours = c("#FFFFFF", "#FFE4E1", "#FFB6C1", "#FF69B4", "#FF1493", "#DC143C", "#8B008B"),
    breaks = c(0, -2, -4, -6, -8),
    labels = c("10", "E-02", "E-04", "E-06", "0")
  )

print(p_384)
dev.off()


## 96well Drug Plate (Linear Label) ----------------------------------
create_96well_data <- function() {
  # DataFrame
  wells <- c()
  values <- c()
  labels <- c()
  
  rows <- LETTERS[1:8]
  cols <- 1:12
  
  for(row in rows) {
    for(col in cols) {
      wells <- c(wells, paste0(row, col))
      
      if(row == "A") {
        # A행: 0과 3E-06 alternating
        if(col %% 2 == 1) {
          values <- c(values, 1e-08)  # 0 대신 아주 작은 값 사용 (log 변환 위해)
          labels <- c(labels, "0")
        } else {
          values <- c(values, 3e-06)
          labels <- c(labels, "3E-06")
        }
      } else if(row == "B") {
        # B행: 1E-05와 3E-05 alternating  
        if(col %% 2 == 1) {
          values <- c(values, 1e-05)
          labels <- c(labels, "E-05")  # 1E-05를 E-05로 표시
        } else {
          values <- c(values, 3e-05)
          labels <- c(labels, "3E-05")
        }
      } else if(row == "C") {
        # C행: 1E-04와 3E-04 alternating
        if(col %% 2 == 1) {
          values <- c(values, 1e-04)
          labels <- c(labels, "E-04")  # 1E-04를 E-04로 표시
        } else {
          values <- c(values, 3e-04)
          labels <- c(labels, "3E-04")
        }
      } else if(row == "D") {
        # D행: 0.001과 0.003 alternating
        if(col %% 2 == 1) {
          values <- c(values, 0.001)
          labels <- c(labels, "0.001")
        } else {
          values <- c(values, 0.003)
          labels <- c(labels, "0.003")
        }
      } else if(row == "E") {
        # E행: 0.01과 0.03 alternating
        if(col %% 2 == 1) {
          values <- c(values, 0.01)
          labels <- c(labels, "0.01")
        } else {
          values <- c(values, 0.03)
          labels <- c(labels, "0.03")
        }
      } else if(row == "F") {
        # F행: 0.1과 0.3 alternating
        if(col %% 2 == 1) {
          values <- c(values, 0.1)
          labels <- c(labels, "0.1")
        } else {
          values <- c(values, 0.3)
          labels <- c(labels, "0.3")
        }
      } else if(row == "G") {
        # G행: 1과 3 alternating
        if(col %% 2 == 1) {
          values <- c(values, 1)
          labels <- c(labels, "1")
        } else {
          values <- c(values, 3)
          labels <- c(labels, "3")
        }
      } else if(row == "H") {
        # H행: 10과 30 alternating
        if(col %% 2 == 1) {
          values <- c(values, 10)
          labels <- c(labels, "10")
        } else {
          values <- c(values, 30)
          labels <- c(labels, "30")
        }
      }
    }
  }
  
  # Log 변환 추가 (uM을 M으로 변환 후 log10 사용)
  values_in_M <- values * 1e-06
  log_values <- log10(values_in_M)
  
  # Log 라벨 생성 (소수점 1자리까지)
  log_labels <- sprintf("%.1f", log_values)
  
  # A행 홀수 열의 LogLabel을 "0"으로 수정
  for(i in 1:length(wells)) {
    if(substr(wells[i], 1, 1) == "A" && as.numeric(substr(wells[i], 2, nchar(wells[i]))) %% 2 == 1) {
      log_labels[i] <- "0"
    }
  }
  
  return(data.frame(
    well = wells, 
    Value = values, 
    LogValue = log_values,
    Label = labels,
    LogLabel = log_labels,
    stringsAsFactors = FALSE
  ))
}

# 96well Drug Plate Data
data_96 <- create_96well_data()

# Save Plots - Linear Label Version
svglite("96_well_drug_plate_linear.svg", width = 12, height = 8)
p96_linear <- plate_plot(
  data = data_96,
  position = well,
  value = LogValue,  # Log 변환된 값 사용
  label = Label,     # 원래 값으로 라벨 표시
  plate_size = 96,
  plate_type = "round",
  colour = c("#FFFFFF", "#FFE4E1", "#FFB6C1", "#FF69B4", "#FF1493", "#DC143C", "#8B008B"),
  label_size = 1.8,
  title = "96-Well Drug Plate (µM)",
  title_size = 16
) + 
  ggplot2::labs(fill = "Conc [μM]") +
  ggplot2::scale_fill_gradientn(
    colours = c("#FFFFFF", "#FFE4E1", "#FFB6C1", "#FF69B4", "#FF1493", "#DC143C", "#8B008B"),
    breaks = c(-5, -7, -9, -11, -13),
    labels = c("10", "0.1", "0.001", "0.00001", "0")
  )

print(p96_linear)
dev.off()

#Save Plots - Log Label Version
svglite("96_well_drug_plate_log.svg", width = 12, height = 8)

p96_log <- plate_plot(
  data = data_96,
  position = well,
  value = LogValue,  # Log 변환된 값 사용
  label = LogLabel,  # Log 변환된 값으로 라벨 표시
  plate_size = 96,
  plate_type = "round",
  colour = c("#FFFFFF", "#FFE4E1", "#FFB6C1", "#FF69B4", "#FF1493", "#DC143C", "#8B008B"),
  label_size = 1.8,
  title = "96-Well Drug Plate (log[M])",
  title_size = 16
) + 
  ggplot2::labs(fill = "Log[Conc] [M]")

print(p96_log)
dev.off()
