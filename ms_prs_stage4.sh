#! /bin/bash
#$ -pe smp 1
#$ -l h_vmem=32G
#$ -l h_rt=240:0:0
#$ -cwd
#$ -j y
#$ -t 1:22
#$ -o /data/scratch/hmy117/
cd /data/Wolfson-UKBB-Dobson/
module load plink/2.0-20170920


#this script should only be run once 'stage 3' has been run

#generate scores
for c in 0.1 0.2 0.4 0.6 0.8
  do
    for j in 0.00000005 0.0000005 0.000005 0.00005 0.0005 0.005 0.05 0.1 0.2 0.4 0.6 0.8 1

      do
        plink2 --pfile /data/Wolfson-UKBB-Dobson/imputed_ukb_genotypes/plink2_files/chr_${SGE_TASK_ID} \
    --score ms_gwas variance-standardize cols=+scoresums list-variants \
    --threads $NSLOTS \
    --extract /data/scratch/hmy117/overall_prs_pval$j\_r2_$c \
    --exclude non_unique_snps_chr${SGE_TASK_ID} \
    --out /data/scratch/hmy117/chr${SGE_TASK_ID}\_pval$j\_r2_$c
  

        echo "Completed p value $j and r2 $c at"
        date
      done
  done

echo "Finished at "
date
