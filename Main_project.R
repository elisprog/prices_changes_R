library(openxlsx) 
library(tidyverse) 
library(ggplot2) 
library(ggiraph) 

#ссылка на общедоступную таблицу excel от Bizmall: https://docs.google.com/spreadsheets/d/13trT-Z0R8GJNfOlnA4CKWLIfEUnjj6xY3zRyjlkt5Js/edit?gid=0#gid=0

#открываем excel для редактирования
wb <- loadWorkbook("./инфляция.xlsx")
df <- read.xlsx(wb, sheet = "Продукты питания") 

#убираем лишнее и меняем названия колонок
df <- df[-c(1,2,28:60),-c(2,22)] 
names(df)[2:28] <- c('Week1', 'Week2','Week3', 'Week4','Week5', 'Week6','Week7', 'Week8','Week9', 'Week10','Week11', 'Week12','Week13', 'Week14','Week15', 'Week16','Week17', 'Week18','Week19', 'Week20','Week21', 'Week22','Week23', 'Week24','Week25', 'Week26','Week27') 

#приводим данные к типу numeric (все колонки, кроме первой)
df[, -1] <- lapply(df[ , -1], function(x) as.numeric(x)) 

#добавляем колонку с подсчетом разницы
df <- df %>%
  mutate(Разница = Week27 - Week1) %>% 
  #для нашей задумки создаем длинный формат
  pivot_longer(cols = starts_with("W"), names_to = "Неделя", values_to = "Цена") %>%
  #для нашей задумки сортируем колонку "разницы" по убыванию
  arrange(Разница)

#готовим график
result <- ggplot(df %>% 
           filter(Неделя %in% c("Week1", "Week27")),
           aes(y = Цена, x = reorder(Продукт, Разница), fill = Неделя,
           #при ::hoover на столбцы всплывает разница цены
           tooltip = ifelse(round(Разница)>0, paste0("Выше на ", round(Разница), " руб"), ifelse(Разница==0.0, paste0("Без изменений "), paste0("Ниже на ", round(Разница), " руб"))), 
           data_id = Продукт)) + 
           #библио ggiraph для интреактивных столбцов
           geom_bar_interactive(stat = 'identity', position = position_dodge(width = 0.5), width = 0.6) + 
           labs(y="Цена, руб", x="Продукт") + 
           #вручную выбираем цвета для столбцов 
           scale_fill_manual(name = "", values = c("Week1" = "steelblue", "Week27" = "brown"), labels = c("Week1" = "Май 2024", "Week27" = "Ноябрь 2024")) +
           ggtitle("Изменение цен на продукты (май-ноябрь 2024)")  +
           #вручную форматируем надписи и расположение текста
           theme_minimal()+
           theme(
                plot.title = element_text(size = 10, face = "bold", hjust = 0.01, vjust = 3.5),
                axis.title.y = element_text(vjust = 4, size = 9, face = "bold"),
                axis.title.x = element_text(size = 9, face = "bold"),
                axis.text.x = element_text(angle = 60, hjust = 1, size = 7,),
                axis.text.y = element_text(size = 7),
                legend.title = element_text(size = 8, face = "bold"),
                legend.text = element_text(size = 7)
                )
#выводим объект с графиком
girafe(ggobj = result, width_svg = 8.5, height_svg = 4)

#описательная статистика комбо
summary(df$Разница)
#размах
range(df$Разница)[2]-range(df$Разница)[1]

