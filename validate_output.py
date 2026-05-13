import pandas as pd
import json
import os

path_xlsx = "analisis_desde_cero.xlsx"
path_json = "analisis_desde_cero.json"
sheet = "genero_por_candidato_top4"

# Validación Excel
print("--- Validación Excel ---")
try:
    df = pd.read_excel(path_xlsx, sheet_name=sheet)
    col_candidato = df.columns[0]
    candidatos = df[col_candidato].dropna().astype(str).tolist()
    print(f"Candidatos encontrados ({len(candidatos)}): {candidatos}")
    
    found_xlsx = any(str(c).strip() == 'Abelardo de la Espriella' for c in candidatos)
    print(f"¿'Abelardo de la Espriella' presente en Excel?: {'SÍ' if found_xlsx else 'NO'}")
except Exception as e:
    print(f"Error Excel: {e}")

# Validación JSON
print("\n--- Validación JSON ---")
try:
    with open(path_json, "r", encoding="utf-8") as f:
        data = json.load(f)
        table_data = data.get("tables", {}).get(sheet, [])
        if table_data:
            # Asumiendo que es una lista de diccionarios o una estructura con datos
            print(f"Clave {sheet} encontrada.")
            # Intentar encontrar en la estructura del JSON (depende de cómo se guardó, pero buscamos el valor)
            json_str = json.dumps(table_data)
            found_json = 'Abelardo de la Espriella' in json_str
            print(f"¿'Abelardo de la Espriella' presente en JSON?: {'SÍ' if found_json else 'NO'}")
        else:
            print(f"Clave {sheet} NO encontrada o vacía.")
except Exception as e:
    print(f"Error JSON: {e}")
