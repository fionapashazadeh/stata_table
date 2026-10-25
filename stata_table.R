stata_table <- function(x) {
	library(dplyr)
	labels <- as.data.frame(attributes(x)$labels)
	colnames(labels) <- "Code"
	labels$Code <- as.factor(labels$Code)
	table <- as.data.frame(table(x, useNA = "ifany"))
	colnames(table) <- c("Code", "Freq")
	stata_table <- left_join(labels, table, by = "Code")
	row.names(stata_table) <- row.names(labels)
	NAs <- table[nrow(table), ]
	names(NAs) <- NULL
	row.names(NAs) <- "Not matched"
	print(attributes(x)$label)
	ifelse(grepl("NA", NAs[1]), print(NAs), "")
	print(stata_table)
}
