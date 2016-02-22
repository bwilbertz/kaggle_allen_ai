# TODO: Add comment
# 
# Author: benedikt
###############################################################################



deriveAnswers <- function(p, ids, ida) {
	require(dplyr)
	
	p$ida <- ida
	p$id <- ids
	
	df <- p %>% group_by(id) %>% arrange(desc(T)) %>% summarise(answer = first(ida))
	
	df %>% select(id,answer) %>% mutate(correctAnswer = gsub("[0-9]*\\.", "", as.character(answer))) %>% select(-answer)
}

