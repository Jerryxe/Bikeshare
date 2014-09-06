# Evaluation Formula (Root Mean Squared Logarithmic Error):
RMSLE <- function(Predicted, Reference) {
    n <- length(Predicted)
    sqrt(sum((log(Predicted + 1) - log(Reference + 1))^2)/n)
}

## User-Defined Prediction Function
PREDICT <- function(Model, FullTraining, Method) {
    # Method = "Log" or "PT", stands for Log Transformation or Power Transformation
    for (i in 6:nrow(FullTraining)) {
        if (FullTraining[i, "Set"] == "Test") {
            ## fill lag 1
            if (Method == "Log") {
                if((FullTraining[i, "Ds"] - FullTraining[i - 1, "Ds"]) < 1) {
                    FullTraining[i, "HLag1CountLog"] <- FullTraining[i - 1, "CountLog"]
                    FullTraining[i, "HLag1Delay"] <- (FullTraining[i, "Ds"] - FullTraining[i - 1, "Ds"]) * 24
                    FullTraining[i, "HLag1LogInt"] <- FullTraining[i, "HLag1CountLog"] / FullTraining[i, "HLag1Delay"]
                } else {
                    FullTraining[i, "HLag1CountLog"] <- 0
                    FullTraining[i, "HLag1Delay"] <- 0
                    FullTraining[i, "HLag1LogInt"] <- 0
                }
            } else {
                if((FullTraining[i, "Ds"] - FullTraining[i - 1, "Ds"]) < 1) {
                    FullTraining[i, "HLag1CountPT"] <- FullTraining[i - 1, "CountPT"]
                    FullTraining[i, "HLag1Delay"] <- (FullTraining[i, "Ds"] - FullTraining[i - 1, "Ds"]) * 24
                    FullTraining[i, "HLag1PTInt"] <- FullTraining[i, "HLag1CountPT"] / FullTraining[i, "HLag1Delay"]
                } else {
                    FullTraining[i, "HLag1CountPT"] <- 0
                    FullTraining[i, "HLag1Delay"] <- 0
                    FullTraining[i, "HLag1PTInt"] <- 0
                }
            }
            
            ## fill lag 2
            if (Method == "Log") {
                if((FullTraining[i, "Ds"] - FullTraining[i - 2, "Ds"]) < 1) {
                    FullTraining[i, "HLag2CountLog"] <- FullTraining[i - 2, "CountLog"]
                    FullTraining[i, "HLag2Delay"] <- (FullTraining[i, "Ds"] - FullTraining[i - 2, "Ds"]) * 24
                    FullTraining[i, "HLag2LogInt"] <- FullTraining[i, "HLag2CountLog"] / FullTraining[i, "HLag2Delay"]
                } else {
                    FullTraining[i, "HLag2CountLog"] <- 0
                    FullTraining[i, "HLag2Delay"] <- 0
                    FullTraining[i, "HLag2LogInt"] <- 0
                }
            } else {
                if((FullTraining[i, "Ds"] - FullTraining[i - 2, "Ds"]) < 1) {
                    FullTraining[i, "HLag2CountPT"] <- FullTraining[i - 2, "CountPT"]
                    FullTraining[i, "HLag2Delay"] <- (FullTraining[i, "Ds"] - FullTraining[i - 2, "Ds"]) * 24
                    FullTraining[i, "HLag2PTInt"] <- FullTraining[i, "HLag2CountPT"] / FullTraining[i, "HLag2Delay"]
                } else {
                    FullTraining[i, "HLag2CountPT"] <- 0
                    FullTraining[i, "HLag2Delay"] <- 0
                    FullTraining[i, "HLag2PTInt"] <- 0
                }
            }
            ## fill lag 3
            if (Method == "Log") {
                if((FullTraining[i, "Ds"] - FullTraining[i - 3, "Ds"]) < 1) {
                    FullTraining[i, "HLag3CountLog"] <- FullTraining[i - 3, "CountLog"]
                    FullTraining[i, "HLag3Delay"] <- (FullTraining[i, "Ds"] - FullTraining[i - 3, "Ds"]) * 24
                    FullTraining[i, "HLag3LogInt"] <- FullTraining[i, "HLag3CountLog"] / FullTraining[i, "HLag3Delay"]
                } else {
                    FullTraining[i, "HLag3CountLog"] <- 0
                    FullTraining[i, "HLag3Delay"] <- 0
                    FullTraining[i, "HLag3LogInt"] <- 0
                }
            } else {
                if((FullTraining[i, "Ds"] - FullTraining[i - 3, "Ds"]) < 1) {
                    FullTraining[i, "HLag3CountPT"] <- FullTraining[i - 3, "CountPT"]
                    FullTraining[i, "HLag3Delay"] <- (FullTraining[i, "Ds"] - FullTraining[i - 3, "Ds"]) * 24
                    FullTraining[i, "HLag3PTInt"] <- FullTraining[i, "HLag3CountPT"] / FullTraining[i, "HLag3Delay"]
                } else {
                    FullTraining[i, "HLag3CountPT"] <- 0
                    FullTraining[i, "HLag3Delay"] <- 0
                    FullTraining[i, "HLag3PTInt"] <- 0
                }
            }
            ## fill lag 4
            if (Method == "Log") {
                if((FullTraining[i, "Ds"] - FullTraining[i - 4, "Ds"]) < 1) {
                    FullTraining[i, "HLag4CountLog"] <- FullTraining[i - 3, "CountLog"]
                    FullTraining[i, "HLag4Delay"] <- (FullTraining[i, "Ds"] - FullTraining[i - 4, "Ds"]) * 24
                    FullTraining[i, "HLag4LogInt"] <- FullTraining[i, "HLag4CountLog"] / FullTraining[i, "HLag4Delay"]
                } else {
                    FullTraining[i, "HLag4CountLog"] <- 0
                    FullTraining[i, "HLag4Delay"] <- 0
                    FullTraining[i, "HLag4LogInt"] <- 0
                }
            } else {
                if((FullTraining[i, "Ds"] - FullTraining[i - 4, "Ds"]) < 1) {
                    FullTraining[i, "HLag4CountPT"] <- FullTraining[i - 3, "CountPT"]
                    FullTraining[i, "HLag4Delay"] <- (FullTraining[i, "Ds"] - FullTraining[i - 4, "Ds"]) * 24
                    FullTraining[i, "HLag4PTInt"] <- FullTraining[i, "HLag4CountPT"] / FullTraining[i, "HLag4Delay"]
                } else {
                    FullTraining[i, "HLag4CountPT"] <- 0
                    FullTraining[i, "HLag4Delay"] <- 0
                    FullTraining[i, "HLag4PTInt"] <- 0
                }
            }
            ## fill lag 5
            if (Method == "Log") {
                if((FullTraining[i, "Ds"] - FullTraining[i - 5, "Ds"]) < 1) {
                    FullTraining[i, "HLag5CountLog"] <- FullTraining[i - 3, "CountLog"]
                    FullTraining[i, "HLag5Delay"] <- (FullTraining[i, "Ds"] - FullTraining[i - 5, "Ds"]) * 24
                    FullTraining[i, "HLag5LogInt"] <- FullTraining[i, "HLag5CountLog"] / FullTraining[i, "HLag5Delay"]
                } else {
                    FullTraining[i, "HLag5CountLog"] <- 0
                    FullTraining[i, "HLag5Delay"] <- 0
                    FullTraining[i, "HLag5LogInt"] <- 0
                }
            } else {
                if((FullTraining[i, "Ds"] - FullTraining[i - 5, "Ds"]) < 1) {
                    FullTraining[i, "HLag5CountPT"] <- FullTraining[i - 3, "CountPT"]
                    FullTraining[i, "HLag5Delay"] <- (FullTraining[i, "Ds"] - FullTraining[i - 5, "Ds"]) * 24
                    FullTraining[i, "HLag5PTInt"] <- FullTraining[i, "HLag5CountPT"] / FullTraining[i, "HLag5Delay"]
                } else {
                    FullTraining[i, "HLag5CountPT"] <- 0
                    FullTraining[i, "HLag5Delay"] <- 0
                    FullTraining[i, "HLag5PTInt"] <- 0
                }
            }
            ## Predict
            if (Method == "Log") {
                FullTraining[i, "CountLog"] <- predict(Model, FullTraining[i, ])
                FullTraining[i, "Count"] <- exp(FullTraining[i, "CountLog"])
                FullTraining[i, "CountPT"] <- (FullTraining[i, "Count"] + 1)^PT$roundlam[1]
            } else {
                FullTraining[i, "CountPT"] <- predict(Model, FullTraining[i, ])
                FullTraining[i, "Count"] <- FullTraining[i, "CountPT"]^(1/PT$roundlam[1]) - 1
                FullTraining[i, "Count"] <- ifelse(FullTraining[i, "Count"] < 0, 0, FullTraining[i, "Count"])
                FullTraining[i, "CountLog"] <- log(FullTraining[i, "Count"] + 1)
            }
        }
    }
    return (FullTraining[FullTraining$Set == "Test", "Count"])
}

