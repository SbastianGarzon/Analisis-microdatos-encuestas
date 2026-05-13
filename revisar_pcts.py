import pandas as pd
import sys

sys.stdout.reconfigure(encoding="utf-8")

df = pd.read_excel("encuestas_concatenadas.xlsx")

PREGUNTAS = [
    "primera_vuelta",
    "primera_vuelta_espontanea",
    "sv_cepeda_vs_espriella",
    "sv_cepeda_vs_fajardo",
    "sv_cepeda_vs_valencia",
    "sv_espriella_vs_fajardo",
    "sv_fajardo_vs_valencia",
    "sv_espriella_vs_valencia",
    "sv_cepeda_vs_pinzon",
    "sv_fajardo_vs_pinzon",
    "sv_cepeda_vs_claudia",
    "sv_cepeda_vs_davila",
    "sv_cepeda_vs_galan",
    "sv_cepeda_vs_gaviria",
    "sv_cepeda_vs_cardenas",
    "sv_cardenas_vs_cepeda",
    "sv_barreras_vs_cepeda",
    "sv_cepeda_vs_lizcano",
    "sv_cepeda_vs_murillo",
    "sv_cepeda_vs_uribe_londono",
    "aprobacion_petro",
]

# Solo incluir preguntas que existen en el df
PREGUNTAS = [p for p in PREGUNTAS if p in df.columns]

for (enc, fecha), grupo in df.groupby(["encuestadora", "fecha"]):
    fecha_str = str(fecha)[:10]
    print(f"\n{'='*60}")
    print(f"  {enc}  |  {fecha_str}  |  n={len(grupo):,}")
    print(f"{'='*60}")

    for pregunta in PREGUNTAS:
        col = grupo[pregunta]
        # Solo mostrar si la pregunta tiene datos en esta encuesta/fecha
        validos = grupo[col.notna()]
        if validos.empty:
            continue

        res = validos.groupby(pregunta)["factor"].sum()
        res_norm = (res / res.sum() * 100).round(1).sort_values(ascending=False)

        print(f"\n  [{pregunta}]  (n ponderado={res.sum():,.0f})")
        for val, pct in res_norm.items():
            print(f"    {val:<35} {pct:>5.1f}%")
