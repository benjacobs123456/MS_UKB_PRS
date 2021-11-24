#! /bin/bash
#$ -pe smp 4
#$ -l h_vmem=4G
#$ -l h_rt=1:0:0
#$ -cwd
#$ -j y
#$ -o /data/scratch/hmy117/
module load plink/2.0-20170920

cd /data/Wolfson-UKBB-Dobson/


# print rsid, effect allele, and logOR
awk '{print $3,$4,log($8)}' /data/Wolfson-UKBB-Dobson/mschip_discovery_gwas/discovery_metav3.0.meta > /data/Wolfson-UKBB-Dobson/ms_gwas

for i in {1..22}
  do
    awk /rs/'{print $3}' ./imputed_ukb_genotypes/plink2_files/chr_$i\.pvar | sort | uniq -d > non_unique_snps_chr$i
  done

rm /data/scratch/hmy117/overall_prs_pval*
