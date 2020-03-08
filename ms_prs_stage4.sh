#! /bin/bash
#$ -pe smp 20
#$ -l h_vmem=1G
#$ -l h_rt=1:0:0
#$ -cwd
#$ -j y
#$ -t 1:22
#$ -o /data/scratch/hmy117/
cd /data/Wolfson-UKBB-Dobson/
module load plink/2.0-20170920


#this script should only be run once 'stage 3' has been run

#generate scores

for j in 0.01 0.1 0.2 0.4 0.6 0.8 1

  do
    plink2 --pfile /data/Wolfson-UKBB-Dobson/imputed_ukb_genotypes/plink2_files/chr_${SGE_TASK_ID} \
--score ms_gwas variance-standardize cols=+scoresums \
--threads $NSLOTS \
--extract /data/scratch/hmy117/overall_prs_pval$j \
--exclude non_unique_snps_chr${SGE_TASK_ID} \
--out /data/scratch/hmy117/chr${SGE_TASK_ID}\_pval$j
    awk 'NR>1{print $2,$6}' /data/scratch/hmy117/chr${SGE_TASK_ID}\_pval$j\.sscore > /data/scratch/hmy117/chr${SGE_TASK_ID}\_scores_pval$j
    sort -n /data/scratch/hmy117/chr${SGE_TASK_ID}\_scores_pval$j > /data/scratch/hmy117/sorted_chr${SGE_TASK_ID}\_scores_pval$j
  done
