import json
import pandas as pd

try:
    with open("analisis_desde_cero.json", "r", encoding="utf-8") as f:
        data = json.load(f)
    table = data.get("tables", {}).get("indecisos_total", [])
    print(f"JSON_ROWS: {len(table)}")
    if len(table) == 1:
        row = table[0]
        print(f"JSON_DATA: {row}")
        print(f"PCT_TOTAL: {row.get('pct_total')}")
    else:
        print("JSON_DATA: Error en numero de filas")
except Exception as e:
    print(f"JSON_ERROR: {e}")

try:
    xlsx = pd.ExcelFile("analisis_desde_cero.xlsx")
    exists = "indecisos_total" in xlsx.sheet_names
    print(f"EXCEL_SHEET_EXISTS: {exists}")
except Exception as e:
    print(f"EXCEL_ERROR: {e}")
