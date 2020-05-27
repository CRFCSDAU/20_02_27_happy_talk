

  p <- c(0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.002, 0.003, 0.003,
         0.003, 0.004, 0.006, 0.011, 0.027, 0.031, 0.078, 0.086, 0.103, 0.131,
         0.139, 0.169, 0.239, 0.262, 0.278, 0.409, 0.432, 0.443, 0.46, 0.46,
         0.556, 0.624, 0.667, 0.71, 0.764, 0.779, 0.787, 0.89, 0.956, 0.993)

  var <- c("Vitamin D", "Ferritin", "Vitamin B12", "Testosterone",
           "Low-density lipoprotein cholesterol", "Albumin",
           "Mean corpuscular hemoglobin", "Total iron binding capacity",
           "Aspartate aminotransferase", "White blood cell count", "Sodium",
           "Magnesium", "Mean platelet volume", "Red cell distribution width",
           "Cortisol", "Basophils", "Mean corpuscular volume",
           "Alanine aminotransferase", "Serum iron",
           "Gamma-glutamyl transferase", "Free testosterone", "Triglycerides",
           "Folate", "Neutrophils", "Sex hormone binding globulin",
           "High sensitivity C-reactive protein", "Lymphocytes",
           "High-density lipoprotein cholesterol", "Hematocrit",
           "Mean corpuscular hemoglobin concentration", "Monocytes",
           "Creatine kinase", "Calcium", "Eosinophils", "Hemoglobin",
           "Cholesterol", "Potassium", "Red blood cells", "Platelets",
           "Glucose")

  data <- data.frame(p = p, var = var)

# data <- data[oder(data$p), ]

  data$m    <- nrow(data)
  data$i    <- c(1:nrow(data))
  data$q    <- 0.05
  data$fdrp <- with(data, i / m * q)
  data$sig  <- data$p <= data$fdrp

