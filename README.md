# Análisis de Microdatos de Encuestas Presidenciales Colombia 2026

Reproducción del análisis de intención de voto a partir de los microdatos de 12 encuestas presidenciales registradas ante el CNE.

## Autoría

El análisis original — incluyendo los scripts `harmonize_encuestas.py`, `analisis_desde_cero.py` y `procesador.ipynb` — es trabajo de Pablo. Este repositorio añade únicamente el entorno de ejecución reproducible (`Dockerfile`, `requirements.txt`, `run_docker.sh`, `export_docker_output.sh`) para facilitar la reproducción del análisis.

## Pipeline

El análisis corre en tres pasos en secuencia:

1. **`harmonize_encuestas.py`** — Lee los microdatos de cada encuestadora, armoniza nombres de candidatos y variables demográficas, y produce `encuestas_concatenadas.xlsx`.
2. **`analisis_desde_cero.py`** — Lee `encuestas_concatenadas.xlsx`, aplica ponderación y produce `analisis_desde_cero.xlsx` y `analisis_desde_cero.json`.
3. **`procesador.ipynb`** — Lee los dos archivos anteriores y produce análisis adicionales.

## Cómo reproducir

### Opción 1: Docker (recomendado)

Requiere tener [Docker](https://www.docker.com/products/docker-desktop/) instalado y corriendo.

```bash
# Construir la imagen (solo necesario una vez)
docker build -t encuestas .

# Correr el análisis (muestra el output en pantalla)
./run_docker.sh

# Copiar los resultados a output_docker/
./export_docker_output.sh
```

### Opción 2: Entorno local (conda)

```bash
conda create -n encuestas python=3.11
conda activate encuestas
pip install -r requirements.txt
python harmonize_encuestas.py
python analisis_desde_cero.py
jupyter nbconvert --to notebook --execute procesador.ipynb --output procesador_executed.ipynb
```

## Outputs

| Archivo | Generado por |
|---|---|
| `output_docker/encuestas_concatenadas.xlsx` | `harmonize_encuestas.py` |
| `output_docker/analisis_desde_cero.xlsx` | `analisis_desde_cero.py` |
| `output_docker/analisis_final.xlsx` | `procesador.ipynb` |
| `output_docker/procesador_executed.ipynb` | `procesador.ipynb` |
