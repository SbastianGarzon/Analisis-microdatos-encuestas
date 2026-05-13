import unicodedata
import json
from pathlib import Path

import pandas as pd


# -----------------------------------------------------------------------------
# Configuracion
# -----------------------------------------------------------------------------
DATA_PATH = Path("encuestas_concatenadas.xlsx")
OUTPUT_PATH = Path("analisis_desde_cero.xlsx")
JSON_PATH = Path("analisis_desde_cero.json")

PESOS = {
    ("Invamer", "2026-04-25"): 1766,
    ("Atlas Intel", "2026-04-10"): 707,
    ("GAD3", "2026-03-16"): 89,
    ("CNC", "2026-03-16"): 220,
    ("Atlas Intel", "2026-03-12"): 175,
    ("Atlas Intel", "2026-02-28"): 29,
    ("Invamer", "2026-02-25"): 22,
    ("CNC", "2026-02-20"): 26,
    ("GAD3", "2026-02-16"): 15,
    ("Atlas Intel", "2026-02-05"): 29,
    ("GAD3", "2026-01-13"): 1,
    ("Atlas Intel", "2026-01-09"): 175,
}

EDAD_MAP = {
    "18-24": "18-34",
    "25-34": "18-34",
    "18-34": "18-34",
    "35-44": "35-54",
    "45-54": "35-54",
    "35-54": "35-54",
    "45-59": "55+",
    "55+": "55+",
    "60+": "55+",
}

CANDIDATOS_VIGENTES = {
    "Iván Cepeda",
    "Abelardo de la Espriella",
    "Paloma Valencia",
    "Sergio Fajardo",
    "Claudia López",
    "Santiago Botero",
    "Luis Gilberto Murillo",
    "Roy Barreras",
    "Miguel Uribe Londoño",
    "Mauricio Lizcano",
    "Carlos Caicedo",
    "Sondra Macollins",
    "Clara López",
    "Voto en blanco",
}

OPCIONES_INDECISOS = {
    "NS/NR",
    "Ninguno",
    "No sé",
    "No votaría",
    "Otro candidato",
}

CANDIDATOS_REALES = CANDIDATOS_VIGENTES.difference({"Voto en blanco"})

PRIMERA_VUELTA_MAP = {
    "ivan cepeda": "Iván Cepeda",
    "abelardo de la espriella": "Abelardo de la Espriella",
    "paloma valencia": "Paloma Valencia",
    "sergio fajardo": "Sergio Fajardo",
    "claudia lopez": "Claudia López",
    "santiago botero": "Santiago Botero",
    "luis gilberto murillo": "Luis Gilberto Murillo",
    "roy barreras": "Roy Barreras",
    "miguel uribe londono": "Miguel Uribe Londoño",
    "mauricio lizcano": "Mauricio Lizcano",
    "carlos caicedo": "Carlos Caicedo",
    "sondra macollins": "Sondra Macollins",
    "clara lopez": "Clara López",
    "voto en blanco": "Voto en blanco",
    "blanco": "Voto en blanco",
    "ns/nr": "NS/NR",
    "nsnr": "NS/NR",
    "ninguno": "Ninguno",
    "no se": "No sé",
    "no sabe": "No sé",
    "no votaria": "No votaría",
    "otro candidato": "Otro candidato",
    "otro": "Otro candidato",
}

CANDIDATOS_TOP_SEXO_INVERSO = [
    "Abelardo de la Espriella",
    "Sergio Fajardo",
    "Paloma Valencia",
    "Iván Cepeda",
    "Claudia López",
]


# -----------------------------------------------------------------------------
# Utilidades
# -----------------------------------------------------------------------------
def _norm_text(x: object) -> str:
    if pd.isna(x):
        return ""
    s = str(x).strip().lower()
    s = unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode("ascii")
    s = " ".join(s.split())
    return s


def _fecha_str(x: object) -> str:
    return str(x)[:10]


def _renormalizar_a_100(df: pd.DataFrame, value_col: str, group_cols: list[str]) -> pd.DataFrame:
    out = df.copy()
    if not group_cols:
        total = out[value_col].sum()
        if total > 0:
            out[value_col] = out[value_col] / total * 100
        return out

    total = out.groupby(group_cols)[value_col].transform("sum")
    mask = total > 0
    out.loc[mask, value_col] = out.loc[mask, value_col] / total[mask] * 100
    return out


def _es_indeciso(x: object) -> bool:
    s = _norm_text(x)
    if not s:
        return False

    # Definicion solicitada: indeciso es todo lo que no sea candidato real.
    candidatos_norm = {_norm_text(v) for v in CANDIDATOS_REALES}
    return s not in candidatos_norm


# -----------------------------------------------------------------------------
# Normalizacion
# -----------------------------------------------------------------------------
def normalizar_encuestadora(s: pd.Series) -> pd.Series:
    return s.replace(
        {
            "Invamer (Colombia Opina 21)": "Invamer",
            "Invamer (Colombia Opina 20)": "Invamer",
        }
    )


def normalizar_edad(s: pd.Series) -> pd.Series:
    txt = s.astype(str).str.strip()
    out = txt.map(EDAD_MAP).fillna(txt)
    out = out.replace({"nan": pd.NA, "": pd.NA, "None": pd.NA})
    return out


def normalizar_region(s: pd.Series) -> pd.Series:
    mapa = {
        "centro - oriente": "Centro - Oriente",
        "centro - sur - amazonia": "Centro - Sur - Amazonia",
        "amazonia - orinquia": "Amazonia - Orinoquia",
        "amazonia - orinoquia": "Amazonia - Orinoquia",
        "eje cafetero": "Eje Cafetero",
        "llano": "Llano",
        "caribe": "Caribe",
        "bogota": "Bogota",
        "pacifico": "Pacifico",
        "central": "Central",
    }

    def f(x: object) -> object:
        if pd.isna(x):
            return pd.NA
        key = _norm_text(x)
        return mapa.get(key, str(x).strip())

    return s.apply(f)


def normalizar_aprobacion_petro(s: pd.Series) -> pd.Series:
    # Estandar comun para encuestas heterogeneas
    # Aprueba, Desaprueba, Regular, NS/NR
    mapa = {
        "aprueba": "Aprueba",
        "desaprueba": "Desaprueba",
        "excelente / bueno": "Aprueba",
        "excelente/bueno": "Aprueba",
        "malo / muy malo": "Desaprueba",
        "malo/muy malo": "Desaprueba",
        "positiva": "Aprueba",
        "negativa": "Desaprueba",
        "regular": "Regular",
        "ns/nr": "NS/NR",
        "nsnr": "NS/NR",
        "p2": "NS/NR",
        "no sabe": "NS/NR",
        "no responde": "NS/NR",
    }

    def f(x: object) -> object:
        if pd.isna(x):
            return pd.NA
        key = _norm_text(x)
        return mapa.get(key, str(x).strip())

    return s.apply(f)


def normalizar_sexo(s: pd.Series) -> pd.Series:
    mapa = {
        "hombre": "Hombre",
        "masculino": "Hombre",
        "varon": "Hombre",
        "mujer": "Mujer",
        "femenino": "Mujer",
        "otro": "Otro",
        "no sabe": "NS/NR",
        "no responde": "NS/NR",
        "ns/nr": "NS/NR",
        "nsnr": "NS/NR",
    }

    def f(x: object) -> object:
        if pd.isna(x):
            return pd.NA
        key = _norm_text(x)
        return mapa.get(key, str(x).strip())

    return s.apply(f)


def normalizar_genero(s: pd.Series) -> pd.Series:
    mapa = {
        "hombre": "Hombre",
        "masculino": "Hombre",
        "varon": "Hombre",
        "mujer": "Mujer",
        "femenino": "Mujer",
        "otro": "Otro",
        "no sabe": "NS/NR",
        "no responde": "NS/NR",
        "ns/nr": "NS/NR",
        "nsnr": "NS/NR",
        "b_sexo": pd.NA,
    }

    def f(x: object) -> object:
        if pd.isna(x):
            return pd.NA
        key = _norm_text(x)
        return mapa.get(key, str(x).strip())

    return s.apply(f)


def normalizar_df(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    if "encuestadora" in out.columns:
        out["encuestadora"] = normalizar_encuestadora(out["encuestadora"])
    if "genero" in out.columns and "sexo" not in out.columns:
        out["sexo"] = normalizar_genero(out["genero"])
    if "sexo" in out.columns:
        out["sexo"] = normalizar_sexo(out["sexo"])
    if "edad_grupo" in out.columns:
        out["edad_grupo"] = normalizar_edad(out["edad_grupo"])
    if "region" in out.columns:
        out["region"] = normalizar_region(out["region"])
    if "aprobacion_petro" in out.columns:
        out["aprobacion_petro"] = normalizar_aprobacion_petro(out["aprobacion_petro"])
    if "primera_vuelta" in out.columns:
        out["primera_vuelta"] = normalizar_primera_vuelta(out["primera_vuelta"])
    return out


def normalizar_primera_vuelta(s: pd.Series) -> pd.Series:
    def f(x: object) -> object:
        if pd.isna(x):
            return pd.NA
        key = _norm_text(x)
        return PRIMERA_VUELTA_MAP.get(key, str(x).strip())

    return s.apply(f)


def filtrar_primera_vuelta_vigente(df: pd.DataFrame) -> pd.DataFrame:
    if "primera_vuelta" not in df.columns:
        return df.copy()
    out = df.copy()
    permitidos = CANDIDATOS_VIGENTES.union(OPCIONES_INDECISOS)
    out = out[out["primera_vuelta"].isin(permitidos)].copy()
    return out


# -----------------------------------------------------------------------------
# Funcion 1: calcular metricas por encuesta (encuestadora + fecha)
# -----------------------------------------------------------------------------
def calcular_por_encuesta(
    df: pd.DataFrame,
    group_cols: str | list[str],
    normalize_within: list[str] | None,
    value_col: str = "factor",
) -> pd.DataFrame:
    """
    Devuelve una tabla por encuesta (encuestadora+fecha) con una metrica por combinacion.

    - group_cols: columna(s) de salida (categoria/variables).
    - normalize_within:
      []       -> porcentaje dentro de toda la encuesta.
      ["x"]    -> porcentaje dentro de x, por encuesta.
      None     -> no convierte a porcentaje; deja suma ponderada.
    """
    cols = [group_cols] if isinstance(group_cols, str) else list(group_cols)
    base_cols = ["encuestadora", "fecha", value_col] + cols

    sub = df[base_cols].copy()
    sub = sub[sub[value_col].notna()]
    for c in cols:
        sub = sub[sub[c].notna()]
    if sub.empty:
        return pd.DataFrame()

    agg = (
        sub.groupby(["encuestadora", "fecha"] + cols, dropna=False)[value_col]
        .sum()
        .reset_index(name="valor")
    )

    if normalize_within is None:
        return agg

    denom_keys = ["encuestadora", "fecha"] + list(normalize_within)
    denom = agg.groupby(denom_keys, dropna=False)["valor"].transform("sum")
    agg = agg[denom > 0].copy()
    agg["valor"] = agg["valor"] / denom[denom > 0] * 100
    agg = _renormalizar_a_100(agg, "valor", denom_keys)
    return agg


# -----------------------------------------------------------------------------
# Funcion 2: combinar entre encuestas usando PESOS
# -----------------------------------------------------------------------------
def combinar_entre_encuestas(
    tabla_encuesta: pd.DataFrame,
    final_group_cols: str | list[str],
    normalize_within_final: list[str] | None,
    value_col: str = "valor",
) -> pd.DataFrame:
    cols = [final_group_cols] if isinstance(final_group_cols, str) else list(final_group_cols)
    d = tabla_encuesta.copy()

    d["fecha_str"] = d["fecha"].apply(_fecha_str)
    d["peso_encuesta"] = d.apply(
        lambda r: PESOS.get((r["encuestadora"], r["fecha_str"]), 0), axis=1
    )
    d = d[d["peso_encuesta"] > 0].copy()
    if d.empty:
        return pd.DataFrame()

    d["_num"] = d[value_col] * d["peso_encuesta"]
    out = d.groupby(cols, dropna=False).agg(num=("_num", "sum"), den=("peso_encuesta", "sum")).reset_index()
    out = out[out["den"] > 0].copy()
    out[value_col] = out["num"] / out["den"]
    out = out.drop(columns=["num", "den"])

    if normalize_within_final is not None:
        out = _renormalizar_a_100(out, value_col, normalize_within_final)

    out[value_col] = out[value_col].round(2)
    return out


# -----------------------------------------------------------------------------
# Analitica solicitada
# -----------------------------------------------------------------------------
def sesgo_por_encuestadora(df: pd.DataFrame, variable: str) -> pd.DataFrame:
    base = calcular_por_encuesta(df, group_cols=[variable], normalize_within=[])
    if base.empty:
        return pd.DataFrame()

    encuestadoras = sorted(base["encuestadora"].dropna().unique())
    filas = []

    for enc in encuestadoras:
        own = base[base["encuestadora"] == enc]
        oth = base[base["encuestadora"] != enc]
        if own.empty or oth.empty:
            continue

        own_c = combinar_entre_encuestas(own, final_group_cols=[variable], normalize_within_final=[])
        oth_c = combinar_entre_encuestas(oth, final_group_cols=[variable], normalize_within_final=[])
        if own_c.empty or oth_c.empty:
            continue

        merged = own_c.merge(oth_c, on=variable, suffixes=("_enc", "_otras"))
        for _, r in merged.iterrows():
            filas.append(
                {
                    "encuestadora": enc,
                    "variable": variable,
                    "categoria": r[variable],
                    "peso_encuestadora": round(float(r["valor_enc"]), 2),
                    "peso_promedio_otras": round(float(r["valor_otras"]), 2),
                    "sesgo_rel_pp": round(float(r["valor_enc"] - r["valor_otras"]), 2),
                }
            )

    if not filas:
        return pd.DataFrame()
    return pd.DataFrame(filas).sort_values(["encuestadora", "sesgo_rel_pp"], ascending=[True, False])


def tabla_aprobacion_vs_voto(df: pd.DataFrame) -> pd.DataFrame:
    base = calcular_por_encuesta(
        df,
        group_cols=["aprobacion_petro", "primera_vuelta"],
        normalize_within=["aprobacion_petro"],
    )
    out = combinar_entre_encuestas(
        base,
        final_group_cols=["aprobacion_petro", "primera_vuelta"],
        normalize_within_final=["aprobacion_petro"],
    )
    if out.empty:
        return out
    piv = out.pivot(index="aprobacion_petro", columns="primera_vuelta", values="valor").fillna(0)
    piv = _renormalizar_a_100(piv.reset_index().melt(id_vars="aprobacion_petro", var_name="primera_vuelta", value_name="valor"), "valor", ["aprobacion_petro"])\
        .pivot(index="aprobacion_petro", columns="primera_vuelta", values="valor").fillna(0)
    return piv.round(2)


def tabla_voto_vs_aprobacion(df: pd.DataFrame) -> pd.DataFrame:
    base = calcular_por_encuesta(
        df,
        group_cols=["primera_vuelta", "aprobacion_petro"],
        normalize_within=["primera_vuelta"],
    )
    out = combinar_entre_encuestas(
        base,
        final_group_cols=["primera_vuelta", "aprobacion_petro"],
        normalize_within_final=["primera_vuelta"],
    )
    if out.empty:
        return out
    piv = out.pivot(index="primera_vuelta", columns="aprobacion_petro", values="valor").fillna(0)
    piv = _renormalizar_a_100(
        piv.reset_index().melt(id_vars="primera_vuelta", var_name="aprobacion_petro", value_name="valor"),
        "valor",
        ["primera_vuelta"],
    ).pivot(index="primera_vuelta", columns="aprobacion_petro", values="valor").fillna(0)
    return piv.round(2)


def tabla_aprobacion_votantes_cepeda(voto_vs_aprobacion: pd.DataFrame) -> pd.DataFrame:
    if voto_vs_aprobacion is None or voto_vs_aprobacion.empty:
        return pd.DataFrame()
    idx_cepeda = [idx for idx in voto_vs_aprobacion.index if "cepeda" in _norm_text(idx)]
    if not idx_cepeda:
        return pd.DataFrame()
    return voto_vs_aprobacion.loc[idx_cepeda].copy()


def tabla_voto_por_region(df: pd.DataFrame) -> pd.DataFrame:
    base = calcular_por_encuesta(df, group_cols=["region", "primera_vuelta"], normalize_within=["region"])
    out = combinar_entre_encuestas(
        base,
        final_group_cols=["region", "primera_vuelta"],
        normalize_within_final=["region"],
    )
    if out.empty:
        return out
    return out.pivot(index="region", columns="primera_vuelta", values="valor").fillna(0).round(2)


def tabla_voto_por_edad(df: pd.DataFrame) -> pd.DataFrame:
    base = calcular_por_encuesta(df, group_cols=["edad_grupo", "primera_vuelta"], normalize_within=["edad_grupo"])
    out = combinar_entre_encuestas(
        base,
        final_group_cols=["edad_grupo", "primera_vuelta"],
        normalize_within_final=["edad_grupo"],
    )
    if out.empty:
        return out
    return out.pivot(index="edad_grupo", columns="primera_vuelta", values="valor").fillna(0).round(2)


def tabla_voto_por_genero(df: pd.DataFrame) -> pd.DataFrame:
    if "sexo" not in df.columns:
        return pd.DataFrame()
    base = calcular_por_encuesta(df, group_cols=["sexo", "primera_vuelta"], normalize_within=["sexo"])
    out = combinar_entre_encuestas(
        base,
        final_group_cols=["sexo", "primera_vuelta"],
        normalize_within_final=["sexo"],
    )
    if out.empty:
        return out
    return out.pivot(index="sexo", columns="primera_vuelta", values="valor").fillna(0).round(2)


def tabla_genero_por_candidato_top4(df: pd.DataFrame) -> pd.DataFrame:
    if "sexo" not in df.columns:
        return pd.DataFrame()
    sub = df[df["primera_vuelta"].isin(CANDIDATOS_TOP_SEXO_INVERSO)].copy()
    if sub.empty:
        return pd.DataFrame()

    base = calcular_por_encuesta(
        sub,
        group_cols=["primera_vuelta", "sexo"],
        normalize_within=["primera_vuelta"],
    )
    out = combinar_entre_encuestas(
        base,
        final_group_cols=["primera_vuelta", "sexo"],
        normalize_within_final=["primera_vuelta"],
    )
    if out.empty:
        return out

    piv = out.pivot(index="primera_vuelta", columns="sexo", values="valor").fillna(0)
    orden = [c for c in CANDIDATOS_TOP_SEXO_INVERSO if c in piv.index]
    if orden:
        piv = piv.loc[orden]
    return piv.round(2)


def tabla_primera_vuelta_total(df: pd.DataFrame) -> pd.DataFrame:
    base = calcular_por_encuesta(df, group_cols=["primera_vuelta"], normalize_within=[])
    out = combinar_entre_encuestas(
        base,
        final_group_cols=["primera_vuelta"],
        normalize_within_final=[],
    )
    if out.empty:
        return out
    out = out.rename(columns={"valor": "pct_total"}).sort_values("pct_total", ascending=False)
    return out


def tablas_indecisos_demograficas(df: pd.DataFrame) -> dict[str, pd.DataFrame]:
    indecisos = df[df["primera_vuelta"].apply(_es_indeciso)].copy()
    if indecisos.empty:
        return {}

    demo_cols = [c for c in ["edad_grupo", "region", "sexo", "estrato"] if c in indecisos.columns]
    out = {}

    for col in demo_cols:
        base = calcular_por_encuesta(indecisos, group_cols=[col], normalize_within=[])
        comb = combinar_entre_encuestas(base, final_group_cols=[col], normalize_within_final=[])
        if comb.empty:
            continue
        tabla = comb.rename(columns={"valor": "pct_final"}).set_index(col).sort_values("pct_final", ascending=False)
        tabla = _renormalizar_a_100(tabla.reset_index(), "pct_final", []).set_index(col)
        out[col] = tabla.round(2)

    return out


def tabla_indecisos_total(df: pd.DataFrame) -> pd.DataFrame:
    if "primera_vuelta" not in df.columns:
        return pd.DataFrame()

    base = calcular_por_encuesta(df, group_cols=["primera_vuelta"], normalize_within=[])
    out = combinar_entre_encuestas(
        base,
        final_group_cols=["primera_vuelta"],
        normalize_within_final=[],
    )
    if out.empty:
        return out

    pct_indecisos = out.loc[out["primera_vuelta"].apply(_es_indeciso), "valor"].sum()
    return pd.DataFrame([{"grupo": "Indecisos", "pct_total": round(float(pct_indecisos), 2)}])


def _revisar_sumas_tabla(
    nombre: str,
    tabla: pd.DataFrame,
    axis: int = 1,
    objetivo: float = 100.0,
    tolerancia: float = 0.25,
) -> list[dict]:
    if tabla is None or tabla.empty:
        return []

    if axis == 1:
        sumas = tabla.sum(axis=1)
    else:
        sumas = tabla.sum(axis=0)

    hallazgos = []
    for idx, valor in sumas.items():
        if abs(float(valor) - objetivo) > tolerancia:
            hallazgos.append(
                {
                    "tabla": nombre,
                    "grupo": str(idx),
                    "suma": round(float(valor), 3),
                    "esperado": objetivo,
                }
            )
    return hallazgos


def verificar_cierres_100(
    aprobacion_vs_voto: pd.DataFrame,
    voto_vs_aprobacion: pd.DataFrame,
    voto_region: pd.DataFrame,
    voto_edad: pd.DataFrame,
    voto_genero: pd.DataFrame,
    genero_por_candidato_top4: pd.DataFrame,
    indecisos_tabs: dict[str, pd.DataFrame],
    base_aprobacion: pd.DataFrame,
    base_voto_vs_aprobacion: pd.DataFrame,
    base_region: pd.DataFrame,
    base_edad: pd.DataFrame,
    base_indecisos: dict[str, pd.DataFrame],
) -> pd.DataFrame:
    hallazgos: list[dict] = []

    # Cierres en tablas finales
    hallazgos += _revisar_sumas_tabla("aprobacion_vs_voto", aprobacion_vs_voto, axis=1)
    hallazgos += _revisar_sumas_tabla("voto_vs_aprobacion", voto_vs_aprobacion, axis=1)
    hallazgos += _revisar_sumas_tabla("voto_por_region", voto_region, axis=1)
    hallazgos += _revisar_sumas_tabla("voto_por_edad", voto_edad, axis=1)
    hallazgos += _revisar_sumas_tabla("voto_por_genero", voto_genero, axis=1)
    hallazgos += _revisar_sumas_tabla("genero_por_candidato_top4", genero_por_candidato_top4, axis=1)

    for nombre, t in indecisos_tabs.items():
        hallazgos += _revisar_sumas_tabla(f"indecisos_{nombre}", t, axis=0)

    # Cierres en etapa por encuesta (antes de combinar entre encuestas)
    for nombre, base, grupos in [
        ("base_aprobacion_vs_voto", base_aprobacion, ["encuestadora", "fecha", "aprobacion_petro"]),
        ("base_voto_vs_aprobacion", base_voto_vs_aprobacion, ["encuestadora", "fecha", "primera_vuelta"]),
        ("base_voto_region", base_region, ["encuestadora", "fecha", "region"]),
        ("base_voto_edad", base_edad, ["encuestadora", "fecha", "edad_grupo"]),
    ]:
        if base is not None and not base.empty:
            chk = base.groupby(grupos, dropna=False)["valor"].sum().reset_index(name="suma")
            for _, r in chk.iterrows():
                if abs(float(r["suma"]) - 100.0) > 0.25:
                    hallazgos.append(
                        {
                            "tabla": nombre,
                            "grupo": " | ".join(str(r[g]) for g in grupos),
                            "suma": round(float(r["suma"]), 3),
                            "esperado": 100.0,
                        }
                    )

    for nombre, base in base_indecisos.items():
        if base is None or base.empty:
            continue
        chk = base.groupby(["encuestadora", "fecha"], dropna=False)["valor"].sum().reset_index(name="suma")
        for _, r in chk.iterrows():
            if abs(float(r["suma"]) - 100.0) > 0.25:
                hallazgos.append(
                    {
                        "tabla": f"base_indecisos_{nombre}",
                        "grupo": f"{r['encuestadora']} | {r['fecha']}",
                        "suma": round(float(r["suma"]), 3),
                        "esperado": 100.0,
                    }
                )

    if not hallazgos:
        return pd.DataFrame(columns=["tabla", "grupo", "suma", "esperado"])

    return pd.DataFrame(hallazgos).sort_values(["tabla", "grupo"]).reset_index(drop=True)


def _tabla_a_json(tabla: pd.DataFrame) -> list[dict]:
    if tabla is None or tabla.empty:
        return []

    tabla = tabla.reset_index()
    if "index" in tabla.columns:
        tabla = tabla.drop(columns=["index"])

    tabla = tabla.where(pd.notna(tabla), None)
    return tabla.to_dict(orient="records")


# -----------------------------------------------------------------------------
# Pipeline principal
# -----------------------------------------------------------------------------
def main() -> None:
    if not DATA_PATH.exists():
        raise FileNotFoundError(f"No se encontro el archivo: {DATA_PATH}")

    df = pd.read_excel(DATA_PATH)
    df = normalizar_df(df)
    df_voto = filtrar_primera_vuelta_vigente(df)

    # 1) Sesgo por encuestadora (seccion separada)
    sesgo_tablas = {}
    for var in ["sexo", "edad_grupo", "region", "estrato"]:
        if var in df.columns:
            sesgo_tablas[var] = sesgo_por_encuestadora(df, var)

    # 2) Cruce aprobacion Petro vs voto
    base_aprobacion = calcular_por_encuesta(
        df_voto,
        group_cols=["aprobacion_petro", "primera_vuelta"],
        normalize_within=["aprobacion_petro"],
    )
    base_voto_vs_aprobacion = calcular_por_encuesta(
        df_voto,
        group_cols=["primera_vuelta", "aprobacion_petro"],
        normalize_within=["primera_vuelta"],
    )
    aprobacion_vs_voto = tabla_aprobacion_vs_voto(df_voto)
    voto_vs_aprobacion = tabla_voto_vs_aprobacion(df_voto)
    aprobacion_votantes_cepeda = tabla_aprobacion_votantes_cepeda(voto_vs_aprobacion)

    # 2b) Primera vuelta total (control)
    primera_vuelta_total = tabla_primera_vuelta_total(df_voto)

    # 3) Voto por region
    base_region = calcular_por_encuesta(
        df_voto,
        group_cols=["region", "primera_vuelta"],
        normalize_within=["region"],
    )
    voto_region = tabla_voto_por_region(df_voto)

    # 4) Voto por edad
    base_edad = calcular_por_encuesta(
        df_voto,
        group_cols=["edad_grupo", "primera_vuelta"],
        normalize_within=["edad_grupo"],
    )
    voto_edad = tabla_voto_por_edad(df_voto)

    # 4b) Voto por genero
    base_genero = None
    if "sexo" in df.columns:
        base_genero = calcular_por_encuesta(
            df_voto,
            group_cols=["sexo", "primera_vuelta"],
            normalize_within=["sexo"],
        )
    voto_genero = tabla_voto_por_genero(df_voto)
    genero_por_candidato_top4 = tabla_genero_por_candidato_top4(df_voto)

    # 5) Demograficas de indecisos
    indecisos_tabs = tablas_indecisos_demograficas(df)
    indecisos_total = tabla_indecisos_total(df)
    indecisos = df[df["primera_vuelta"].apply(_es_indeciso)].copy()
    base_indecisos = {}
    for col in [c for c in ["edad_grupo", "region", "sexo", "estrato"] if c in indecisos.columns]:
        base_indecisos[col] = calcular_por_encuesta(indecisos, group_cols=[col], normalize_within=[])

    chequeo = verificar_cierres_100(
        aprobacion_vs_voto=aprobacion_vs_voto,
        voto_vs_aprobacion=voto_vs_aprobacion,
        voto_region=voto_region,
        voto_edad=voto_edad,
        voto_genero=voto_genero,
        genero_por_candidato_top4=genero_por_candidato_top4,
        indecisos_tabs=indecisos_tabs,
        base_aprobacion=base_aprobacion,
        base_voto_vs_aprobacion=base_voto_vs_aprobacion,
        base_region=base_region,
        base_edad=base_edad,
        base_indecisos=base_indecisos,
    )

    # Exportacion
    with pd.ExcelWriter(OUTPUT_PATH, engine="openpyxl") as writer:
        for var, t in sesgo_tablas.items():
            if not t.empty:
                t.to_excel(writer, sheet_name=f"sesgo_{var}"[:31], index=False)

        if not aprobacion_vs_voto.empty:
            aprobacion_vs_voto.to_excel(writer, sheet_name="aprobacion_vs_voto")

        if not voto_vs_aprobacion.empty:
            voto_vs_aprobacion.to_excel(writer, sheet_name="voto_vs_aprobacion")

        if not aprobacion_votantes_cepeda.empty:
            aprobacion_votantes_cepeda.to_excel(writer, sheet_name="cepeda_aprobacion")

        if not primera_vuelta_total.empty:
            primera_vuelta_total.to_excel(writer, sheet_name="primera_vuelta_total", index=False)

        if not voto_region.empty:
            voto_region.to_excel(writer, sheet_name="voto_por_region")

        if not voto_edad.empty:
            voto_edad.to_excel(writer, sheet_name="voto_por_edad")

        if not voto_genero.empty:
            voto_genero.to_excel(writer, sheet_name="voto_por_genero")

        if not genero_por_candidato_top4.empty:
            genero_por_candidato_top4.to_excel(writer, sheet_name="genero_por_candidato_top4")

        for var, t in indecisos_tabs.items():
            if not t.empty:
                t.to_excel(writer, sheet_name=f"indecisos_{var}"[:31])

        if not indecisos_total.empty:
            indecisos_total.to_excel(writer, sheet_name="indecisos_total", index=False)

        if not chequeo.empty:
            chequeo.to_excel(writer, sheet_name="verificacion_100", index=False)

    payload = {
        "meta": {
            "source_file": str(DATA_PATH.resolve()),
            "excel_file": str(OUTPUT_PATH.resolve()),
            "json_file": str(JSON_PATH.resolve()),
            "pesos": [{"encuestadora": k[0], "fecha": k[1], "peso": v} for k, v in PESOS.items()],
        },
        "tables": {
            "sesgo_edad_grupo": _tabla_a_json(sesgo_tablas.get("edad_grupo")),
            "sesgo_sexo": _tabla_a_json(sesgo_tablas.get("sexo")),
            "sesgo_region": _tabla_a_json(sesgo_tablas.get("region")),
            "sesgo_estrato": _tabla_a_json(sesgo_tablas.get("estrato")),
            "aprobacion_vs_voto": _tabla_a_json(aprobacion_vs_voto),
            "voto_vs_aprobacion": _tabla_a_json(voto_vs_aprobacion),
            "cepeda_aprobacion": _tabla_a_json(aprobacion_votantes_cepeda),
            "primera_vuelta_total": _tabla_a_json(primera_vuelta_total),
            "voto_por_region": _tabla_a_json(voto_region),
            "voto_por_edad": _tabla_a_json(voto_edad),
            "voto_por_genero": _tabla_a_json(voto_genero),
            "genero_por_candidato_top4": _tabla_a_json(genero_por_candidato_top4),
            "indecisos_edad_grupo": _tabla_a_json(indecisos_tabs.get("edad_grupo")),
            "indecisos_sexo": _tabla_a_json(indecisos_tabs.get("sexo")),
            "indecisos_region": _tabla_a_json(indecisos_tabs.get("region")),
            "indecisos_estrato": _tabla_a_json(indecisos_tabs.get("estrato")),
            "indecisos_total": _tabla_a_json(indecisos_total),
            "verificacion_100": _tabla_a_json(chequeo),
        },
    }

    with JSON_PATH.open("w", encoding="utf-8") as f:
        json.dump(payload, f, ensure_ascii=False, indent=2)

    print(f"OK. Archivo generado: {OUTPUT_PATH.resolve()}")
    print(f"OK. JSON generado: {JSON_PATH.resolve()}")
    if chequeo.empty:
        print("OK. Verificacion: todos los cierres estan en 100 +/- 0.25")
    else:
        print("ADVERTENCIA. Hay cierres fuera de tolerancia; revisar hoja verificacion_100")


if __name__ == "__main__":
    main()
