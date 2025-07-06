# Paquetes
library(ggplot2)

# Secuencia de niveles de depreciación fiscal (0% a 100%)
depr_levels <- seq(0, 1, by = 0.01)

# Supongamos que la reducción de emisiones tiene una forma de curva Laffer
# Función logística invertida para simular saturación
max_reduction <- 100  # Máximo potencial de reducción (por ejemplo, en toneladas)
k <- 10              # Pendiente de la curva
x0 <- 0.50             # Punto de inflexión

emission_reduction <- function(depr) {
  max_reduction * (depr * exp(-k * (depr - x0)^2))
}

reductions <- sapply(depr_levels, emission_reduction)

# Crear data frame
df <- data.frame(
  Depreciacion = depr_levels * 100,     # en porcentaje
  Reduccion = reductions
)

# Graficar
ggplot(df, aes(x = Depreciacion, y = Reduccion)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = df$Depreciacion[which.max(df$Reduccion)],
             linetype = "dashed", color = "red") +
  labs(
    title = "Curva de Laffer Climática",
    subtitle = "Depreciación fiscal vs. reducción de emisiones",
    x = "Nivel de depreciación fiscal (%)",
    y = "Reducción de emisiones (ton CO₂)"
  ) +
  theme_minimal()
