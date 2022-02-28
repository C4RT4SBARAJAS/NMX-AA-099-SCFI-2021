# Nombre del programa: Curva de Calibración de la Disolución Patrón de Nitritos.

number.of.patterns <- function(number.of.rows) {
  vector <- c()
  
  for (counter in 1:number.of.rows) {
    vector[counter] <- counter
  }
  data.frame.1 <- as.data.frame(vector)
  rename.column <- "No.Patrones"
  colnames(data.frame.1) <- rename.column
  print(data.frame.1)
  return(data.frame.1)
}

sample.absorbance <- function(number.of.rows) {
  vector <- c()
  
  for (counter in 1:number.of.rows) {
    cat("
No. Patrón: ")
    cat(counter)
    cat("
")
    value <- as.numeric(readline("Ingrese el valor a continuación: " ))
    vector[counter] <- value
  }
  data.frame.2 <- as.data.frame(vector)
  rename.column <- "Absorbancia.de.la.muestra"
  colnames(data.frame.2) <- rename.column
  print(data.frame.2)
  return(data.frame.2)
}

regression.summary <- function(data.frame.1, data.frame.2) suppressWarnings({
  data.frame <- cbind(data.frame.1, data.frame.2)
  regresion <- lm(Absorbancia.de.la.muestra ~ No.Patrones, data = data.frame)
  cat("

Resumen de la regresion:

y = b ∗ x + a

(1) R cuadrada
(2) b (pendiente)
(3) a (ordenada al origen)

")
  
  menu <- "Elige una opción escribiendo el número: "
  
  option <- as.integer(readline(menu))
  
  if (is.na(option)) {
    
    message("❌ Tu respuesta no es valida")
    
  } else if (option == 1) {
    
    cat("
R cuadradra: ")
    cat(summary(regresion)$r.squared)
    
  } else if (option == 2) {
    
    cat("
Pendiente: ")
    cat(summary(regresion)$coefficients[2, 1])
    
  } else if (option == 3) {
    
    cat("
Ordenada al origen: ")
    cat(summary(regresion)$coefficients[1, 1])
    
  } else {
    
    message("❌ Ingrese una opción correcta")
    
  }
})

answer.1 <- function(yes.no.1) {
  answer <- stringr::str_replace_all(yes.no.1,' ', '')
  answer <- stringr::str_to_lower(answer)
  YES <- "si"
  NO <- "no"
  if (answer == YES) {
    
    return(TRUE)
    
  } else if (answer == NO) {
    
    return(FALSE)
    
  }
}

regression.summary.loop <- function(data.frame.1, data.frame.2) suppressWarnings({
  cat("
  
¿Desea consultar los dos Resumenes de la regresion faltantes?

")
  
  yes.no.1 <- as.character(readline("Responde Si o No a continuación: "))
  
  answer <- answer.1(yes.no.1)
  
  if (answer == TRUE) {
    
    for (counter in 1:2) {
      regression.summary(data.frame.1, data.frame.2)
    }
    
  } else if (answer == FALSE) {
    
    for (counter in 1:2) {
      break
    }
    
  } else {
    
    message("❌ Ingrese una opción correcta")
    
  }
})

sample.concentration <- function(number.of.rows, data.frame.1, data.frame.2) suppressWarnings({
  data.frame <- cbind(data.frame.1, data.frame.2)
  regresion <- lm(Absorbancia.de.la.muestra ~ No.Patrones, data = data.frame)
  r.squared <- summary(regresion)$r.squared
  pendiente <- summary(regresion)$coefficients[2, 1]
  intercepto <- summary(regresion)$coefficients[1, 1]
  
  vector <- c()
  
  for (counter in 1:number.of.rows) {
    vector[counter] <- (data.frame.2[counter, 1] - intercepto)/pendiente
  }
  data.frame.3 <- as.data.frame(vector)
  rename.column <- "Concentración.de.la.muestra"
  colnames(data.frame.3) <- rename.column
  cat("
  
")
  print(data.frame.3)
  return(data.frame.3)
})

answer.2 <- function(yes.no.2) {
  answer <- stringr::str_replace_all(yes.no.2,' ', '')
  answer <- stringr::str_to_lower(answer)
  YES <- "si"
  NO <- "no"
  if (answer == YES) {
    
    return(TRUE)
    
  } else if (answer == NO) {
    
    return(FALSE)
    
  }
}

regression.plot <- function(data.frame.1, data.frame.2) {
  cat("
  
¿Desea graficar la Curva de calibración?

")
  
  yes.no.2 <- as.character(readline("Responde Si o No a continuación: "))
  
  answer <- answer.2(yes.no.2)
  
  if (answer == TRUE) {
    
    data.frame <- cbind(data.frame.1, data.frame.2)
    regresion <- lm(Absorbancia.de.la.muestra ~ No.Patrones, data = data.frame)
    plot(data.frame$No.Patrones, data.frame$Absorbancia.de.la.muestra, xlab='Número de patrones', ylab='Absorbancia de la muestra', main = "Curva de calibración")
    abline(regresion)
    
  } else if (answer == FALSE) {
    
    cat("
¡Gracias por usar el programa! 💚 💚 💚
")
    
  } else {
    
    message("❌ Ingrese una opción correcta")
    
  }
}

run <- function() {
  number.of.rows <- as.integer(readline("Indique el Número de Patrones a continuación (mínimo cinco): " ))
  data.frame.1 <- number.of.patterns(number.of.rows)
  data.frame.2 <- sample.absorbance(number.of.rows)
  regression.summary(data.frame.1, data.frame.2)
  regression.summary.loop(data.frame.1, data.frame.2)
  data.frame.3 <- sample.concentration(number.of.rows, data.frame.1, data.frame.2)
  regression.plot(data.frame.1, data.frame.2)
}




cat("
¡Hola! ... ¡Bienvenido! 😎 👋

NOMBRE DEL PROGRAMA: Curva de Calibración de la Disolución Patrón de Nitritos. 🧪

NORMA MEXICANA: NMX-AA-099-SCFI-2021. 📂

CLÁUSULA: Leer la Absorbancia a 543 nm y reportar los resultados para cada Patrón. 👀

")

run()