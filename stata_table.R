stata_table <- function(x) {
	labels <- as.data.frame(attributes(x)$labels)
	colnames(labels) <- "Code"
	labels$Code <- as.factor(labels$Code)
	table <- as.data.frame(table(x, useNA = "ifany"))
	colnames(table) <- c("Code", "Freq")
	labels_table <- merge(labels, table, by = "Code", all.x = TRUE)
	row.names(labels_table) <- row.names(labels)
	NAs <- table[nrow(table), ]
	names(NAs) <- NULL
	row.names(NAs) <- "Not matched"
	print(attributes(x)$label)
	ifelse(grepl("NA", NAs[1]), print(NAs), "")
	print(labels_table)
}
