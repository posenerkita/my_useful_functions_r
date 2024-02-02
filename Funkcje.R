# Funkcje

s_error <- function(x, na.rm = FALSE) { # blad standartowy
  if (na.rm) x <- na.omit(x)
  sqrt(var(x) / length(x))
}


as_trans <- function(x) {  # transformacja Blissa
  asin(sqrt((x/100)))*180/pi
}

reverse <- function(x) { # Funkcja cofająca z arc sin (Transformacja Blissa) do procentów
  w <- x*pi/180
  z <- sin(w)^2
  y <- z*100
  return(y)
}



smooth_impute <- function(c_germ, days) { # Funkcja do modelowania kielkowania w dniach bez kontroli - wymagana w 
                                          # pakiecie germinationmetrics
  data <- data.frame(x = days, y = c_germ) %>% na.omit()
  fm <- gam(y ~ s(x, bs = "cs", k = nrow(data)), data = data)
  preds <- round(predict(fm, newdata = data.frame(x = days), type = "response"))
  preds[preds < 0] <- 0
  preds[preds > max(c_germ)] <- max(c_germ)
  preds
}



barplot_theme <- function() { # wyglad moich wykresow
  theme_bw(base_size = 18) %+replace% 
  theme(
  # Tekst na wykresie
  axis.text = element_text(color = "black"),
  #  wyglad paneli
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank(),
  # modyfikacja linii w wykresie
  axis.ticks = element_line(color = "black"),
  panel.border = element_rect(color = "black",
                              fill=NA),
  # legenda 
  legend.position = "top"
  )
}

point_theme <- function() { # wyglad moich wykresow
  theme_bw(base_size = 18) %+replace% 
  theme(
    # Tekst na wykresie
    axis.text = element_text(color = "black"),
    # modyfikacja linii w wykresie
    axis.ticks = element_line(color = "black"),
    panel.border = element_rect(color = "black",
                                fill=NA),
    # legenda 
    legend.position = "top"
  )
}


g_bar.moj <- function(kol) {
geom_bar(stat = 'identity',
         position = position_dodge(0.9),
         fill = kolor_plot[kol],
         color = 'black')
}


# super kolorki do plotow
kolor_plot <- dutchmasters::dutchmasters$view_of_Delft

kolor_fut <- ggsci::pal_futurama()


# funkcja do wyliczenia punktow przeciecia krzywych liniowych
# Topt = (b.supr - b.sub)/(a.sub - a.supr) 
# Y = l1[1] + l1[2] * X
# Y = l2[1] + l2[2] * X
intersect <- function(l1, l2){
  x <- (l2[1] - l1[1]) / (l1[2] - l2[2])
  y <- l1[1] + l1[2] * x
  return(xy=c(x, y))
}

mod_chi_info <- function(model){
  modelChi <- model$null.deviance - model$deviance
  
  modelDF <- model$df.null - model$df.residual
  
  chi.prop <- 1 - pchisq(modelChi, modelDF)
  cat("If bigger than 0.05 model is guessing not predicting\n", chi.prop)
}

logisticPseudoR2 <- function(LogModel) {
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev/nullDev
  R.cs <- 1 - exp(-(nullDev - dev)/modelN)
  R.n <- R.cs/(1 - (exp(-(nullDev/modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2 ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2 ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2 ", round(R.n, 3), "\n")
}

germ_data_template <- function(trt1, trt2, rep, days) {
  col_1 <- rep(trt1, each = length(trt2) * length(rep))
  
  col_2 <- rep(trt2, each = length(rep), times = length(trt1))
  
  col_3 <- rep(rep, times = length(trt1) * length(trt2))
  
  days.col <- as.character(days)
  
  df <- data.frame(col_1, col_2, col_3)
  
  df[,days.col] <- NA
  
  colnames(df)[c(4:(4+(length(days)-1)))] <- paste('D', colnames(df)[c(4:(4+(length(days)-1)))], sep = '.')
  
  print(df)
  
  require(xlsx)
  
  write.xlsx(df, "Germination_Table.xlsx")
  
  
}
