library(tidyverse)
library(r2rtf)
setwd("C:/Training/Academy/Recordings/R/RTF Reporting")
df <- read.csv("DM.csv")

cnts <- df %>% group_by(trtcd) %>% summarise(n=n())
cnt_pbo <- cnts$n[1]
cnt_5mg <- cnts$n[2]
cnt_10mg <- cnts$n[3]
cnt_50mg <- cnts$n[4]

sex_summ <- df %>% 
              group_by(trtcd,sex) %>% 
              summarise(n=n(),.groups = 'drop') %>% 
              pivot_wider(id_cols = sex, names_from = trtcd, values_from = n) %>% 
              mutate(total = `1`+`2`+`3`+`4`, 
                     pct_1 = (`1`/cnt_pbo)*100,
                     pct_2 = (`2`/cnt_5mg)*100,
                     pct_3 = (`3`/cnt_10mg)*100,
                     pct_4 = (`4`/cnt_50mg)*100,
                     pct_tot = ((`1`+`2`+`3`+`4`)/sum(cnts$n))*100) %>% 
              mutate(c_pct_1 = formatC(pct_1,format = "f",digits = 1),
                     c_pct_2 = formatC(pct_2,format = "f",digits = 1),
                     c_pct_3 = formatC(pct_3,format = "f",digits = 1),
                     c_pct_4 = formatC(pct_4,format = "f",digits = 1),
                     c_pct_tot = formatC(pct_tot,format = "f",digits = 1)) %>% 
              mutate(fmt_1 = paste0(`1`," ( ", c_pct_1, "% )"),
                     fmt_2 = paste0(`2`," ( ", c_pct_2, "% )"),
                     fmt_3 = paste0(`3`," ( ", c_pct_3, "% )"),
                     fmt_4 = paste0(`4`," ( ", c_pct_4, "% )"),
                     fmt_tot = paste0(total," ( ", c_pct_tot, "% )"),
                     stats = c("n ( % )")) %>% 
              select(sex,stats,fmt_1,fmt_2,fmt_3,fmt_4,fmt_tot)

firstrow <- data.frame(sex="Sex",stats="",fmt_1="",
              fmt_2="",fmt_3="",fmt_4="",fmt_tot="")

sex_summ$sex <- paste(" ", sex_summ$sex)
cnt_all <- rbind.data.frame(firstrow,sex_summ)

file_path <- rstudioapi::getSourceEditorContext()$path
dt_time <- as.character(Sys.time())
foot_string <- paste0(file_path," / ",dt_time)



cnt_all %>%
  rtf_page(orientation = "landscape",
           border_first = "single",
           border_last = "single") %>%
  rtf_title(title = "Table 13.1.2",
            subtitle = c("Demographic Characteristics",
                         "Full Analysis Set"),
            text_justification = "c",
            text_font_size=8) %>%
  rtf_colheader(colheader = "Characteristic | Statistics | Placebo | XYZ 5mg | XYZ 10mg | XYZ 50mg | Total",
                col_rel_width=c(7,4,3,3,3,3,3),
                text_justification = c("l","l","c","c","c","c","c"),
                border_top = rep("",7),
                border_right = rep("",7),
                border_left =rep("",7)) %>%
  rtf_colheader(colheader = paste0("| | N=",cnt_pbo," | N=",cnt_5mg ,"| N=",cnt_10mg,
                                   "| N=",cnt_50mg,"| N=",cnt_pbo+cnt_5mg+cnt_10mg+cnt_50mg),
                col_rel_width=c(7,4,3,3,3,3,3),
                text_justification = c("l","l","c","c","c","c","c"),
                border_top = rep("",7),
                border_left = rep("",7),
                border_right = rep("",7)) %>%
  rtf_body(as_colheader = F,
           col_rel_width=c(7,4,3,3,3,3,3),
           border_first = rep("single",7),
           border_last = rep("single",7),
           border_left = rep("",7),
           border_right = rep("",7),
           text_justification = c("l","l","c","c","c","c","c"),
           text_font_size=7,
           last_row = F) %>%
  rtf_footnote(footnote = c("SOURCE: Listings 16.2.4.1 and 16.2.4.3",
                            "NOTE: The percentages are based on the XXX Analysis Set.",
                            foot_string),
               border_left = "",
               border_right = "",
               border_bottom = "",
               text_font_size=7) %>%
  rtf_encode() %>%
  write_rtf("tbl_13_1_2.rtf")


               