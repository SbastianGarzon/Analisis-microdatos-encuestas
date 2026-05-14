FROM python:3.11-slim

WORKDIR /app

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

CMD ["sh", "-c", "python harmonize_encuestas.py && python analisis_desde_cero.py && jupyter nbconvert --to notebook --execute procesador.ipynb --output procesador_executed.ipynb"]
