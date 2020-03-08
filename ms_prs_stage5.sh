#! /bin/bash
#$ -pe smp 1
#$ -l h_vmem=16G
#$ -l h_rt=1:0:0
#$ -cwd
#$ -j y
#$ -o /data/scratch/hmy117/


cd /data/scratch/hmy117/

for j in 0.01 0.1 0.2 0.4 0.6 0.8 1
  do 
#this script should only be run once 'stage 1' has been run

#generate scores

    for i in {2..22}
      do
        join sorted_chr1_scores_pval$j sorted_chr$i\_scores_pval$j > combined_score
        mv combined_score sorted_chr1_scores_pval$j
      done
    awk 'BEGIN{print "EID","PRS"}; NR>=1{for (i=2;i<=NF;i++) x+=$i; print $1,x; x=0}' sorted_chr1_scores_pval$j > summarised_PRS_results_pval$j
  done

mv summarised_PRS_results* /data/Wolfson-UKBB-Dobson/ms_prs
rm overall_prs*
rm sorted_chr*
rm  prs_clumped_pval*
rm *.sscore
rm *scores*
