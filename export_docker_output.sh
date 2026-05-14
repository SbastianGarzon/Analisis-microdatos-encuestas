#!/bin/bash
set -e

mkdir -p output_docker

docker cp encuestas-run:/app/encuestas_concatenadas.xlsx output_docker/
docker cp encuestas-run:/app/analisis_desde_cero.xlsx output_docker/
Docker cp encuestas-run:/app/analisis_final.xlsx output_docker/
docker cp encuestas-run:/app/procesador_executed.ipynb output_docker/

docker rm encuestas-run

echo "Done. Results are in output_docker/"
