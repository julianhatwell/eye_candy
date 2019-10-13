library(ggpubr)

# Basic plot
p <- ggboxplot(ToothGrowth, x = "dose", y = "len",
               color = "dose")
p
# Add grids
p + grids(linetype = "dashed")

# Add panel border line
p + border("black")

# Change background color
p + bgcolor("#BFD5E3") +
  border("#0FD5E3") 

# Change titles and axis labels
p2 <- ggpar(p, 
            title = "Box Plot created with ggpubr",
            subtitle = "Length by dose",
            caption = "Source: ggpubr",
            xlab ="Dose (mg)", 
            ylab = "Teeth length",
            legend.title = "Dose (mg)")
p2

ggpar(p2, 
      font.title = c(14, "bold.italic", "red"),
      font.subtitle = c(10,  "orange"),
      font.caption = c(10,  "orange"),
      font.x = c(14,  "blue"),
      font.y = c(14,  "#993333")
)
p2 +
  font("title", size = 14, color = "red", face = "bold.italic")+
  font("subtitle", size = 10, color = "orange")+
  font("caption", size = 10, color = "orange")+
  font("xlab", size = 12, color = "blue")+
  font("ylab", size = 12, color = "#993333")

# Change title texts and fonts
# line break: \n
ggpar(p, title = "Plot of length \n by dose",
      xlab ="Dose (mg)", ylab = "Teeth length",
      legend.title = "Dose (mg)",
      font.title = c(14,"bold.italic", "red"),
      font.x = c(14, "bold", "#2E9FDF"),
      font.y = c(14, "bold", "#E7B800"))

ggpar(p,
      legend = "right", legend.title = "Dose (mg)") + 
  font("legend.title", color = "blue", face = "bold")+ 
  font("legend.text", color = "red")

# Use custom color palette
ggpar(p, palette = c("#00AFBB", "#E7B800", "#FC4E07"))
# Use brewer palette. 
# Type RColorBrewer::display.brewer.all(), to see possible palettes
ggpar(p, palette = "Dark2" )
# Use grey palette
ggpar(p, palette = "grey")

# Use scientific journal palette from ggsci package
# Allowed values: "npg", "aaas", "lancet", "jco", 
#   "ucscgb", "uchicago", "simpsons" and "rickandmorty".
ggpar(p, palette = "npg") # natu
# jco color palette
p + color_palette("jco")
# Custom color
p + color_palette(c("#00AFBB", "#E7B800", "#FC4E07"))re

p3 <- ggscatter(mtcars, x = "wt", y = "mpg", color = "mpg",
                size = 2)
# Use one custom color
p3 + gradient_color("red")
# Two colors
p3 + gradient_color(c("blue",  "red"))
# Three colors
p3 + gradient_color(c("blue", "white", "red"))
# Use RColorBrewer palette
p3 + gradient_color("RdYlBu")

p4 <- ggscatter(mtcars, x = "wt", y = "mpg", fill = "mpg",
                size = 4, shape = 21)
p4 + gradient_fill(c("blue", "white", "red"))


# Change y axis limits
ggpar(p, ylim = c(0, 50))
# Change y axis scale to log2
ggpar(p, yscale = "log2")
# Format axis scale
ggpar(p, yscale = "log2", format.scale = TRUE)

# Change the font of x and y axis texts.
# Rotate x and y texts, angle = 45
p + 
  font("xy.text", size = 12, color = "blue", face = "bold") +
  rotate_x_text(45)+       
  rotate_y_text(45)
# remove ticks and axis texts
p + rremove("ticks")+
  rremove("axis.text")

# Horizontal box plot
p + rotate()

# Gray theme
p + theme_gray()
# Black and white theme
p + theme_bw()

# Theme light
p + theme_light()
# Minimal theme
p + theme_minimal()
# Empty theme
p + theme_void()

# Basic plot
p <- ggboxplot(ToothGrowth, x = "dose", y = "len",
               ggtheme = theme_gray())
p
# Remove all grids
p + rremove("grid")
# Remove only x grids
p + rremove("x.grid")
