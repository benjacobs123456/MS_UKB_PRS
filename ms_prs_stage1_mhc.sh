#! /bin/bash
#$ -pe smp 25
#$ -l h_vmem=4G
#$ -l h_rt=1:0:0
#$ -cwd
#$ -j y


#load plink
module load plink/1.9-170906

  # define the function which will generate the prs for a given p threshold

  generate_prs () {
  # navigate to the wd
          cd /data/Wolfson-UKBB-Dobson/1kg_reference

  # this script takes gwas discovery data and makes a rudimentary prs

  # filter ms discovery gwas by p value threshold
          pval_threshold=$1
          awk -v p="$pval_threshold" '{if($7<=p) {print}}' ../mschip_discovery_gwas/discovery_metav3.0.meta > prs2.txt

  # remove palindromic variants
          cat prs2.txt | awk '{print $3,$4$5,$4,$5,$8,$7}' | grep -v -E "AT|TA|CG|GC" | awk '{print $1,$3,$4,$5,$6}' > prs3.txt

  # include only variants with an rsid
          cat prs3.txt | grep rs > prs4.txt

  #add in col names
          echo "SNP A1 A2 OR P" > col_names.txt
          cat col_names.txt prs4.txt > pval_threshold_$1_prs.txt

  # clean up
          rm prs1.txt
          rm prs2.txt
          rm prs3.txt
          rm prs4.txt
          rm col_names.txt;
  }


  # now loop the function over selected p thresholds

  for i in 0.00000005 0.0000005 0.000005 0.00005 0.0005 0.005 0.05 0.1 0.2 0.4 0.6 0.8 1
          do
                  generate_prs $i
	  done

rm /data/Wolfson-UKBB-Dobson/ms_prs/overall_prs_pval*
