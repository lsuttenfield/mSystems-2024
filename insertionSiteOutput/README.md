```
date: 07/03/24
author: Laura Suttenfield
```
# Notes on host splits files. 

These are outputs from the transposable-insertion-site-finder pipeline by J Cristobal Vera. 
"Overlaps" is a reference to the way that the insertion sites are found. At the insertion site, 
after alignment (bwa-mem) to a two-contig reference, the host and the virus,  the read is "split" 
between the two contigs.  

All insertion sites are precise to the nucleotide. As Mu-like viruses lytically replicate by 
insertion into the host chromosome, we found a persistent background of reads supporting this 
lifestyle. 

## Structure of insertion sites in alignment

Structure of Mu-like insertion: 
```
5' ----NNNNN*VIRUS_GENOME*NNNNN---- 3'

    Host    |    Virus   |   Host
5'-----Read1|			 |Read2-----3'
            |Read1  Read2|

Structure of insertion on the host alignment:

5' ----NNNNN---- 3'
5' ----Read1|    3' 
5'    |Read2---- 3'
```
Therefore, one insertion needs at a minimum 2 reads to support it. In our lysogens we found a 
persistent background of reads which suggested lysis. 

The raw reads of our isolates are available on NCBI BioProject # PRJNA1021667. 



