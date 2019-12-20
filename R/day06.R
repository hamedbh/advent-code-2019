library(igraph)
library(magrittr)
input <- read.delim("data/input06.txt", sep = ")", 
                    header = FALSE, 
                    stringsAsFactors = FALSE) %>% 
    set_names(letters[1:2])
orbits <- graph_from_data_frame(input, directed = FALSE)

# Part 1
distances(orbits, to = "COM") %>% 
    sum()

# Part 2
# what objects are YOU and SAN orbitting?
YOU_orbitting <- input$a[input$b == "YOU"]
SAN_orbitting <- input$a[input$b == "SAN"]
# Then use one string as the target and subset the result using the other
distances(orbits, to = SAN_orbitting)[YOU_orbitting, ]
# Check that it works both ways
distances(orbits, to = YOU_orbitting)[SAN_orbitting, ]
# It does!
