#$ -S /bin/bash
#$ -N args_evtree
#$ -wd /home/agilson/jobs
#$ -o R-prueba.salida
#$ -e R-prueba.err
#$ -q diaria_multicore
#$ -pe smp 2-8
#$ -l virtual_free=24G
# $ -l slots=4
#
# Copio el fichero de entrada a un subdirectorio mio en /scratch
mkdir -p /scratch/agilson/jobs
cp matrix_evtree.R /scratch/agilson/jobs
cp dataset.csv /scratch/agilson/jobs
# Ahora que he preparado el entorno de trabajo
# en el nodo en el que se va a ejecutar mi programa, lo lanzo
#
export OMP_NUM_THREADS=$NSLOTS
#module load R-3.3.2-Bioconductor
module load R-3.4.2-Bioconductor
R --slave --no-save --max-ppsize 500000 --args 1 < /scratch/agilson/jobs/matrix_evtree.R
#
# Limpio el scratch
# Si el proceso hubiese dejado ficheros de salida que me interesan
# los copio antes a mi /home:
cp /scratch/agilson/jobs/result_p1.txt /home/agilson/jobs
cp /scratch/agilson/jobs/model_evtree_p1.rda /home/agilson/jobs
cp /scratch/agilson/jobs/evtree_plot_p1.pdf /home/agilson/jobs

#cp /scratch/fdivina/R-top2/modelRF.rds /home/fdivina/
#cp /scratch/fdivina/R-top2/modelR.jpg /home/fdivina/

rm -rf /scratch/agilson/jobs
