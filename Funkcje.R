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

