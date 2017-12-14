#$ -S /bin/bash
#$ -N P19args_evtree
#$ -wd /home/agilson/jobs
#$ -o R-prueba19.salida
#$ -e R-prueba19.err
#$ -q diaria_multicore
#$ -pe smp 2-8
#$ -l virtual_free=24G
# $ -l slots=4
#
# Copio el fichero de entrada a un subdirectorio mio en /scratch
mkdir -p /scratch/agilson/jobs/scr19
cp matrix_evtree.R /scratch/agilson/jobs/scr19
cp dataset.csv /scratch/agilson/jobs/scr19
# Ahora que he preparado el entorno de trabajo
# en el nodo en el que se va a ejecutar mi programa, lo lanzo
#
export OMP_NUM_THREADS=$NSLOTS
#module load R-4.3.2-Bioconductor
module load R-3.4.2-Bioconductor
R --slave --no-save --max-ppsize 500000 --args 19 < /scratch/agilson/jobs/scr19/matrix_evtree.R
#
# Limpio el scratch
# Si el proceso hubiese dejado ficheros de salida que me interesan
# los copio antes a mi /home:
cp /scratch/agilson/jobs/scr19/result_p19.txt /home/agilson/jobs
cp /scratch/agilson/jobs/scr19/model_evtree_p19.rda /home/agilson/jobs
cp /scratch/agilson/jobs/scr19/evtree_plot_p19.pdf /home/agilson/jobs
rm -rf /scratch/agilson/jobs/scr19
