import pandas as pd
import json

try:
    df = pd.read_excel("analisis_desde_cero.xlsx", sheet_name="genero_por_candidato_top4")
    print(f"Hoja genero_por_candidato_top4: SI")
    candidatos = df.iloc[:, 0].dropna().tolist()
    print(f"Candidatos en tabla: {candidatos}")
except Exception as e:
    print(f"Error al leer Excel: {e}")

try:
    with open("analisis_desde_cero.json", "r", encoding="utf-8") as f:
        data = json.load(f)
        if "tables" in data and "genero_por_candidato_top4" in data["tables"]:
            print("Clave tables.genero_por_candidato_top4: SI")
        else:
            print("Clave tables.genero_por_candidato_top4: NO")
except Exception as e:
    print(f"Error al leer JSON: {e}")
