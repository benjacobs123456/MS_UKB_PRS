#! /bin/bash
#$ -pe smp 20
#$ -l h_vmem=1G
#$ -l h_rt=1:0:0
#$ -cwd
#$ -j y
#$ -t 1:22
#$ -o /data/scratch/hmy117/

i=${SGE_TASK_ID}

#load plink
module load plink/1.9-170906

# now clump by 2r and distance
#navigate to the destination folder

cd /data/Wolfson-UKBB-Dobson/1kg_reference

for x in 0.01 0.1 0.2 0.4 0.6 0.8 1
        do
        	plink --bfile ./filtered_chr$i \
                                --clump pval_threshold_$x\_prs\.txt \
                                --clump-r2 0.8 \
                                --clump-kb 250 \
                                --clump-p1 $x \
                                --out /data/scratch/hmy117/prs_clumped_pval$x\_chr$i \
				--threads $NSLOTS
                cat /data/scratch/hmy117/prs_clumped_pval$x\_chr$i\.clumped | awk '{print $3}' | grep rs | cat >> /data/scratch/hmy117/overall_prs_pval$x

        done
        




