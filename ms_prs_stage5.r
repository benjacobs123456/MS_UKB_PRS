
library(dplyr)
library(readr)


# collate PRS scores
pvals = c("1","0.8","0.6","0.4","0.2","0.1","0.05","0.005","0.0005","0.00005","0.000005","0.0000005","0.00000005")
r2_vector = c(0.1,0.2,0.4,0.6,0.8)

for(r2 in r2_vector){
  for(pval in pvals){
    scores = list()
    for(chr in 1:22){
      message("P value: ",pval)
      message("Clumping R2: ",r2)
      message("Chr: ",chr)
      file = paste0("/data/scratch/hmy117/chr",chr,"_pval",pval,"_r2_",r2,".sscore")
      if(file.exists(file)){
        df = read_table2(file)
        df = df %>% select(2,6)
        scores[[chr]] = df
      }
    }
    overall_score = do.call("rbind",scores)

    overall_score = overall_score %>% group_by(IID) %>% summarise(PRS = sum(SCORE1_SUM))
    colnames(overall_score)=c("EID","PRS")
    write_tsv(overall_score,paste0("/data/Wolfson-UKBB-Dobson/ms_prs/summarised_PRS_results_pval",pval,"r2",r2))
    message("Done")
  }
}
