"""
harmonize_encuestas.py
Reads 12 Colombian presidential polling surveys, harmonizes questions and
candidate names, and saves a single concatenated Excel file.

Output segunda-vuelta columns are named by matchup, e.g. sv_cepeda_vs_espriella.
"""

import json
import re
import unicodedata
import warnings
from pathlib import Path

import pandas as pd
import pyreadstat

warnings.filterwarnings("ignore")

BASE = Path(r"C:\Users\pablo\Downloads\declaraciones_senado\OneDrive_4_12-5-2026")
OUT  = BASE / "encuestas_concatenadas.xlsx"

# ─── CANDIDATE NAME HARMONIZATION ────────────────────────────────────────────

def _norm(text: str) -> str:
    s = unicodedata.normalize("NFD", str(text).lower().strip())
    return "".join(c for c in s if unicodedata.category(c) != "Mn")

# Order matters: specific before general
RULES = [
    (r"efrain cepeda",               "Efraín Cepeda"),
    (r"ivan cepeda|i\.\s*cepeda|cepeda castro|cepeda", "Iván Cepeda"),
    (r"paloma|p\.\s*valencia",       "Paloma Valencia"),
    (r"espriella",                    "Abelardo de la Espriella"),
    (r"barreras",                     "Roy Barreras"),
    (r"fajardo",                      "Sergio Fajardo"),
    (r"claudia|c\.\s*lopez",         "Claudia López"),
    (r"pinzon",                       "Juan Carlos Pinzón"),
    (r"carlos caicedo|caicedo",       "Carlos Caicedo"),
    (r"luis gilberto|gilberto murillo","Luis Gilberto Murillo"),
    (r"santiago botero|botero",       "Santiago Botero"),
    (r"galan",                        "Juan Manuel Galán"),
    (r"oviedo",                       "Juan Daniel Oviedo"),
    (r"lizcano",                      "Mauricio Lizcano"),
    (r"uribe londo",                  "Miguel Uribe Londoño"),
    (r"matamoros",                    "Gustavo Matamoros"),
    (r"luis carlos reyes|l\.?\s?c\.?\s?reyes", "Luis Carlos Reyes"),
    (r"david luna",                   "David Luna"),
    (r"murillo",                      "Luis Gilberto Murillo"),
    (r"macollins|sondra",             "Sondra Macollins"),
    (r"anibal gaviria|gaviria",       "Aníbal Gaviria"),
    (r"mauricio cardenas|m\.\s*cardenas", "Mauricio Cárdenas"),
    (r"penalosa",                     "Enrique Peñalosa"),
    (r"daniel palacios",              "Daniel Palacios"),
    (r"vargas lleras",                "Germán Vargas Lleras"),
    (r"camilo romero",                "Camilo Romero"),
    (r"fernando cristo|j\.?\s?f\.?\s?cristo|juan\s*fernando\s*cristo", "Juan Fernando Cristo"),
    (r"clara lopez|clara eugenia|clara l\.",  "Clara López"),
    (r"jose manuel restrepo|restrepo",        "José Manuel Restrepo"),
    (r"quilcue|quilcu",               "Aída Marina Quilcué"),
    (r"leonardo huerta|huerta",       "Leonardo Huerta"),
    (r"edna bonilla|bonilla",         "Edna Bonilla"),
    (r"luz maria zapata|zapata",      "Luz María Zapata"),
    (r"vicky davila|davila",          "Vicky Dávila"),
    (r"quintero",                     "Daniel Quintero"),
    (r"cabal",                        "María Fernanda Cabal"),
    (r"armitage",                     "Maurice Armitage"),
    (r"paola holguin|holguin",        "Paola Holguín"),
    (r"felipe cordoba|f\.\s*cordoba", "Felipe Córdoba"),
    (r"luna",                         "David Luna"),
    (r"voto.*blanco|en blanco|blanco.*nulo|blank|nulo", "Voto en blanco"),
    (r"no votar|no ira|no piensa|no votaria", "No votaría"),
    (r"ninguno",                      "Ninguno"),
    (r"ns/nc|ns/nr|no sabe|no responde|^ns$|^nr$", "NS/NR"),
    (r"hombre|masculino|^m$",         "Hombre"),
    (r"mujer|femenino|^f$",           "Mujer"),
]

def harmonize(val) -> str | None:
    if val is None or (isinstance(val, float) and pd.isna(val)):
        return None
    s = _norm(str(val))
    if s in ("", "nan", "none"):
        return None
    for pattern, canonical in RULES:
        if re.search(pattern, s):
            return canonical
    # Return original (title-cased) if no rule matched
    raw = str(val).strip()
    return raw if raw else None


# ─── SEGUNDA VUELTA COLUMN NAMING ────────────────────────────────────────────

# Short keys for candidate names used in column names
CAND_KEY = {
    "Iván Cepeda":            "cepeda",
    "Abelardo de la Espriella": "espriella",
    "Paloma Valencia":        "valencia",
    "Sergio Fajardo":         "fajardo",
    "Juan Carlos Pinzón":     "pinzon",
    "Claudia López":          "claudia",
    "Juan Manuel Galán":      "galan",
    "Aníbal Gaviria":         "gaviria",
    "Vicky Dávila":           "davila",
    "Mauricio Cárdenas":      "cardenas",
    "Roy Barreras":           "barreras",
    "Miguel Uribe Londoño":   "uribe_londono",
    "Mauricio Lizcano":       "lizcano",
    "Luis Gilberto Murillo":  "murillo",
}

def sv_col_name(cand_a: str, cand_b: str) -> str | None:
    """Return normalized sv column name for two candidates, sorted alphabetically."""
    if not cand_a or not cand_b:
        return None
    k1 = CAND_KEY.get(cand_a)
    k2 = CAND_KEY.get(cand_b)
    if not k1 or not k2:
        return None
    a, b = sorted([k1, k2])
    return f"sv_{a}_vs_{b}"


# ─── ATLAS INTEL JSON PARSER ──────────────────────────────────────────────────

def parse_atlas_label(label: str) -> str | None:
    """Convert Atlas JSON label (e.g. 'Espriella vs. Cepeda') to sv column name."""
    parts = re.split(r"\s+vs\.?\s+", str(label), flags=re.IGNORECASE)
    if len(parts) == 2:
        h1 = harmonize(parts[0].strip())
        h2 = harmonize(parts[1].strip())
        return sv_col_name(h1, h2)
    return None

def parse_atlas_json(cell) -> dict:
    """Return {sv_col_name: harmonized_value} from Atlas second-round JSON cell."""
    if pd.isna(cell) or str(cell).strip() == "":
        return {}
    try:
        items = json.loads(str(cell))
        result = {}
        for item in items:
            col = parse_atlas_label(item["label"])
            if col:
                result[col] = harmonize(item["value"])
        return result
    except Exception:
        return {}


# ─── INVAMER CANDIDATE ID MAP (official dictionary) ──────────────────────────

INVAMER_CAND_MAP = {
    2:    "Germán Vargas Lleras",
    3:    "Sergio Fajardo",
    7:    "Felipe Córdoba",
    11:   "Claudia López",
    25:   "Juan Manuel Galán",
    27:   "Roy Barreras",
    29:   "Paola Holguín",
    30:   "Paloma Valencia",
    38:   "Enrique Peñalosa",
    44:   "Juan Carlos Pinzón",
    45:   "Mauricio Cárdenas",
    50:   "Iván Cepeda",
    52:   "María Fernanda Cabal",
    55:   "Camilo Romero",
    60:   "Juan Fernando Cristo",
    65:   "Luis Gilberto Murillo",
    87:   "David Luna",
    88:   "Juan Daniel Oviedo",
    90:   "Vicky Dávila",
    95:   "Carlos Caicedo",
    97:   "Abelardo de la Espriella",
    98:   "Mauricio Lizcano",
    100:  "Aníbal Gaviria",
    101:  "Juan Guillermo Zuluaga",
    102:  "Maurice Armitage",
    103:  "Mauricio Gómez Amín",
    104:  "Miguel Uribe Londoño",
    105:  "Juan Carlos Cárdenas",
    106:  "Daniel Palacios",
    107:  "Héctor Olimpo",
    108:  "Efraín Cepeda",
    109:  "Luis Carlos Reyes",
    110:  "Santiago Botero",
    112:  "Clara López",
    113:  "Sondra Macollins",
    150:  "Gustavo Petro",
    255:  "Gustavo Matamoros",
    990:  "Ninguno",
    995:  "Voto en blanco",
    999:  "NS/NR",
    9996: "Otro",
}


# ─── UTILITY ─────────────────────────────────────────────────────────────────

def find_col(df: pd.DataFrame, *substrings) -> str | None:
    for s in substrings:
        matches = [c for c in df.columns if s.lower() in str(c).lower()]
        if matches:
            return matches[0]
    return None

def ensure_cols(df: pd.DataFrame, cols: list) -> pd.DataFrame:
    for c in cols:
        if c not in df.columns:
            df[c] = None
    return df


# All possible named sv columns (alphabetically within each pair)
SV_COLS = [
    "sv_barreras_vs_cepeda",
    "sv_cardenas_vs_cepeda",
    "sv_cepeda_vs_claudia",
    "sv_cepeda_vs_davila",
    "sv_cepeda_vs_espriella",
    "sv_cepeda_vs_fajardo",
    "sv_cepeda_vs_galan",
    "sv_cepeda_vs_gaviria",
    "sv_cepeda_vs_lizcano",
    "sv_cepeda_vs_murillo",
    "sv_cepeda_vs_pinzon",
    "sv_cepeda_vs_uribe_londono",
    "sv_cepeda_vs_valencia",
    "sv_espriella_vs_fajardo",
    "sv_espriella_vs_valencia",
    "sv_fajardo_vs_pinzon",
    "sv_fajardo_vs_valencia",
]

FINAL_COLS = [
    "encuestadora", "fecha", "factor",
    "departamento", "municipio", "region", "zona",
    "genero", "edad_grupo", "estrato", "educacion",
    "primera_vuelta", "primera_vuelta_espontanea",
    *SV_COLS,
    "aprobacion_petro",
]


# ─── READERS ──────────────────────────────────────────────────────────────────

def read_atlas(path: Path, fecha) -> pd.DataFrame:
    df = pd.read_excel(path) if str(path).endswith(".xlsx") else pd.read_csv(path)
    r = pd.DataFrame(index=df.index)
    r["encuestadora"] = "Atlas Intel"
    r["fecha"]        = pd.Timestamp(fecha)
    r["factor"]       = pd.to_numeric(df["weight"], errors="coerce")
    r["departamento"] = df.get("state")
    r["municipio"]    = df.get("municipality")
    r["region"]       = df.get("region")
    r["zona"]         = None
    r["genero"]       = df.get("gender")
    r["edad_grupo"]   = df.get("age")
    r["educacion"]    = df.get("educational_level")
    r["estrato"]      = None
    r["aprobacion_petro"] = df.get("approve_disapprove_president")

    r["primera_vuelta"] = df["presidential_election_2026"].apply(harmonize)
    spont = "vote_president_2026_spontaneous"
    r["primera_vuelta_espontanea"] = df[spont].apply(harmonize) if spont in df.columns else None

    # Parse JSON multi-matchup column: each item has label (matchup) and value (vote)
    if "second_round_president_2026_co" in df.columns:
        json_series = df["second_round_president_2026_co"].apply(parse_atlas_json)
        sv_cols_in_survey = []
        for d in json_series:
            for col in d:
                if col not in sv_cols_in_survey:
                    sv_cols_in_survey.append(col)
        for col in sv_cols_in_survey:
            r[col] = json_series.apply(lambda d, c=col: d.get(c))

    # s2/s3/s4 are multi-candidate scenarios (3-4 candidates), not binary matchups — skip
    return r


def read_gad3_05() -> pd.DataFrame:
    path = BASE / "05. CNE-E-DG-2026-001504 - GAD3" / "505-221 RCN Enero_4.xlsx"
    df = pd.read_excel(path)

    r = pd.DataFrame(index=df.index)
    r["encuestadora"] = "GAD3"
    fecha_col = find_col(df, "Fecha")
    r["fecha"] = pd.to_datetime(df[fecha_col], errors="coerce").dt.normalize().min() if fecha_col else None
    r["factor"]       = pd.to_numeric(df.get("Ponderación total"), errors="coerce")
    r["departamento"] = df.get("Departamento")
    r["municipio"]    = df.get("Municipio")
    r["region"]       = None
    r["zona"]         = None
    r["genero"]       = df[find_col(df, "Q02")].apply(harmonize)
    r["edad_grupo"]   = df.get("Grupo de edad")
    r["educacion"]    = df[find_col(df, "Q14")].apply(str) if find_col(df, "Q14") else None
    r["estrato"]      = df[find_col(df, "Q16")].apply(str) if find_col(df, "Q16") else None
    r["aprobacion_petro"] = None

    q10 = find_col(df, "Q10")
    r["primera_vuelta"]            = df[q10].apply(harmonize)
    r["primera_vuelta_espontanea"] = df[q10].apply(harmonize)

    # Q11A-G: explicit matchup per column name
    GAD3_05_SV = {
        "Q11A": ("Abelardo de la Espriella", "Iván Cepeda"),
        "Q11B": ("Iván Cepeda", "Sergio Fajardo"),
        "Q11C": ("Paloma Valencia", "Iván Cepeda"),
        "Q11D": ("Iván Cepeda", "Juan Carlos Pinzón"),
        "Q11E": ("Abelardo de la Espriella", "Sergio Fajardo"),
        "Q11F": ("Juan Carlos Pinzón", "Sergio Fajardo"),
        "Q11G": ("Paloma Valencia", "Sergio Fajardo"),
    }
    for prefix, (c1, c2) in GAD3_05_SV.items():
        col = find_col(df, prefix)
        sv = sv_col_name(c1, c2)
        if col and sv:
            r[sv] = df[col].apply(harmonize)
    return r


def read_gad3_23() -> pd.DataFrame:
    path = BASE / "23. CNE-E-DG-2026-008418 - GAD3" / "Anexo IV. B. Microdatos estudio (SPSS).sav"
    df, _ = pyreadstat.read_sav(str(path), apply_value_formats=True)

    r = pd.DataFrame(index=df.index)
    r["encuestadora"] = "GAD3"
    r["fecha"]        = pd.to_datetime(df.get("fecha"), errors="coerce").dt.normalize().min() if "fecha" in df.columns else None
    r["factor"]       = pd.to_numeric(df.get("Ponderación"), errors="coerce")
    r["departamento"] = df.get("Departamento")
    r["municipio"]    = df.get("municipio")
    r["region"]       = df.get("Macroregion")
    r["zona"]         = df.get("zona")
    r["genero"]       = df["Q02"].apply(harmonize)
    r["edad_grupo"]   = df.get("Gedad")
    r["educacion"]    = df["Q11"].apply(str) if "Q11" in df.columns else None
    r["estrato"]      = df["Q13"].apply(str) if "Q13" in df.columns else None
    r["aprobacion_petro"] = None

    r["primera_vuelta"]            = df["Q08"].apply(harmonize)
    r["primera_vuelta_espontanea"] = df["Q08"].apply(harmonize)

    GAD3_23_SV = {
        "Q09":  ("Abelardo de la Espriella", "Iván Cepeda"),
        "Q09A": ("Iván Cepeda", "Sergio Fajardo"),
        "Q09B": ("Paloma Valencia", "Iván Cepeda"),
        "Q09C": ("Abelardo de la Espriella", "Sergio Fajardo"),
        "Q09D": ("Paloma Valencia", "Abelardo de la Espriella"),
    }
    for prefix, (c1, c2) in GAD3_23_SV.items():
        sv = sv_col_name(c1, c2)
        if prefix in df.columns and sv:
            r[sv] = df[prefix].apply(harmonize)
    return r


def read_gad3_31() -> pd.DataFrame:
    path = BASE / "31. CNE-E-DG-2026-011911 - GAD3" / "Anexo IV.A.Microdatos estudio (Excel).xlsx"
    df = pd.read_excel(path)

    r = pd.DataFrame(index=df.index)
    r["encuestadora"] = "GAD3"
    fecha_col = find_col(df, "fecha")
    r["fecha"] = pd.to_datetime(df[fecha_col], errors="coerce").dt.normalize().min() if fecha_col else None
    r["factor"]       = pd.to_numeric(df.get("Ponderación"), errors="coerce")
    r["departamento"] = None
    r["municipio"]    = df[find_col(df, "municipio")] if find_col(df, "municipio") else None
    r["region"]       = None
    r["zona"]         = df[find_col(df, "zona")] if find_col(df, "zona") else None
    r["genero"]       = df[find_col(df, "Q02")].apply(harmonize)
    r["edad_grupo"]   = df[find_col(df, "Gedad")] if find_col(df, "Gedad") else None
    r["educacion"]    = df[find_col(df, "Q10")].apply(str) if find_col(df, "Q10") else None
    r["estrato"]      = df[find_col(df, "Q12")].apply(str) if find_col(df, "Q12") else None
    r["aprobacion_petro"] = None

    q6 = find_col(df, "Q06")
    r["primera_vuelta"]            = df[q6].apply(harmonize)
    r["primera_vuelta_espontanea"] = df[q6].apply(harmonize)

    # Q08 columns identified by prefix match on column name
    GAD3_31_SV = {
        "Q08 -":  ("Abelardo de la Espriella", "Iván Cepeda"),
        "Q08A -": ("Iván Cepeda", "Sergio Fajardo"),
        "Q08B -": ("Paloma Valencia", "Iván Cepeda"),
        "Q08C -": ("Abelardo de la Espriella", "Sergio Fajardo"),
        "Q08D -": ("Paloma Valencia", "Abelardo de la Espriella"),
    }
    for prefix, (c1, c2) in GAD3_31_SV.items():
        col = next((c for c in df.columns if str(c).startswith(prefix)), None)
        sv = sv_col_name(c1, c2)
        if col and sv:
            r[sv] = df[col].apply(harmonize)
    return r


def read_invamer(path: Path, fecha, survey_name: str) -> pd.DataFrame:
    df = pd.read_excel(path)

    fac_col = find_col(df, "Factor de ponderaci")
    df = df[df[fac_col].astype(str) != fac_col].copy()
    df[fac_col] = pd.to_numeric(df[fac_col], errors="coerce")


    def decode_inv(val):
        if pd.isna(val) or str(val).strip() in ("", "nan"):
            return None
        try:
            code = int(float(str(val)))
            name = INVAMER_CAND_MAP.get(code)
            if name:
                return harmonize(name)
        except (ValueError, TypeError):
            pass
        return harmonize(val)

    r = pd.DataFrame(index=df.index)
    r["encuestadora"] = f"Invamer ({survey_name})"
    r["fecha"]        = pd.Timestamp(fecha)
    r["factor"]       = df[fac_col]
    r["departamento"] = None
    r["municipio"]    = df["Municipio"].astype(str) if "Municipio" in df.columns else None
    _region_map = {1: "Caribe", 2: "Centro - Oriente", 3: "Eje Cafetero",
                   4: "Pacífico", 5: "Centro – Sur - Amazonía", 6: "Llano",
                   "1": "Caribe", "2": "Centro - Oriente", "3": "Eje Cafetero",
                   "4": "Pacífico", "5": "Centro – Sur - Amazonía", "6": "Llano"}
    r["region"]       = df["REGIÓN"].map(_region_map) if "REGIÓN" in df.columns else None
    r["zona"]         = df["ZONA"].astype(str).replace({"B_ZONA": None}) if "ZONA" in df.columns else None
    r["genero"]       = df["SEXO"].map({"1": "Hombre", "2": "Mujer", 1: "Hombre", 2: "Mujer"}).fillna(df["SEXO"].astype(str)) if "SEXO" in df.columns else None
    _edad_map = {1: "Entre 18 y 24", 2: "Entre 25 y 34", 3: "Entre 35 y 44",
                 4: "Entre 45 y 54", 5: "55 ó más",
                 "1": "Entre 18 y 24", "2": "Entre 25 y 34", "3": "Entre 35 y 44",
                 "4": "Entre 45 y 54", "5": "55 ó más"}
    _edad_col = find_col(df, "P24_GRUPOEDAD")
    r["edad_grupo"]   = df[_edad_col].map(_edad_map) if _edad_col else None
    r["estrato"]      = df["ESTRATO SOCIAL"].astype(str).replace({"B_ESTRATO": None}) if "ESTRATO SOCIAL" in df.columns else None
    r["educacion"]    = df[find_col(df, "P22.")].astype(str) if find_col(df, "P22.") else None

    p2_col = find_col(df, "P2.", "aprueba")
    if p2_col:
        r["aprobacion_petro"] = df[p2_col].map(
            {"1": "Aprueba", "2": "Desaprueba", "999": "NS/NR",
             1: "Aprueba", 2: "Desaprueba", 999: "NS/NR"}
        ).fillna(df[p2_col].astype(str).replace({"nan": None}))
    else:
        r["aprobacion_petro"] = None

    p245 = find_col(df, "P245")
    p247 = find_col(df, "P247")
    r["primera_vuelta"]            = df[p245].apply(decode_inv) if p245 else None
    r["primera_vuelta_espontanea"] = df[p247].apply(decode_inv) if p247 else None

    # P231/P232/P233/P256/P258/P267/P268 are binary matchups; P271/P272 are multi-candidate — skip
    INVAMER_SV_MAP = {
        "P231": ("Abelardo de la Espriella", "Sergio Fajardo"),
        "P232": ("Iván Cepeda", "Sergio Fajardo"),
        "P233": ("Iván Cepeda", "Abelardo de la Espriella"),
        "P256": ("Iván Cepeda", "Paloma Valencia"),
        "P258": ("Iván Cepeda", "Claudia López"),
        "P267": ("Sergio Fajardo", "Abelardo de la Espriella"),
        "P268": ("Paloma Valencia", "Sergio Fajardo"),
    }
    for pat, (c1, c2) in INVAMER_SV_MAP.items():
        col = find_col(df, pat)
        sv = sv_col_name(c1, c2)
        if col and sv:
            r[sv] = df[col].apply(decode_inv)
    return r


def read_cnc_25() -> pd.DataFrame:
    path = (BASE / "25. CNE-E-DG-2026-009065 - CENTRO NACIONAL DE CONSULTORÍA"
            / "RevCambioFebrero" / "data" / "CC892901_BASE_REVISTA_CAMBIO.sav")
    df, _ = pyreadstat.read_sav(str(path), apply_value_formats=True)

    r = pd.DataFrame(index=df.index)
    r["encuestadora"] = "CNC"
    r["fecha"]        = pd.Timestamp("2026-02-20")
    r["factor"]       = pd.to_numeric(df["FACTOR"], errors="coerce")
    r["departamento"] = df.get("DPTO")
    r["municipio"]    = df.get("MUNICIPIO")
    r["region"]       = df.get("REGION")
    r["zona"]         = df.get("ZONA")
    r["genero"]       = df["GENERO"].apply(harmonize)
    r["edad_grupo"]   = df.get("REDAD")
    r["estrato"]      = df.get("ESTRATO")
    r["educacion"]    = df.get("P20")
    r["aprobacion_petro"] = df["P14"].apply(harmonize)

    r["primera_vuelta"]            = df["P2"].apply(harmonize)
    r["primera_vuelta_espontanea"] = None

    CNC25_SV_MAP = {
        "P5":  ("Iván Cepeda", "Sergio Fajardo"),
        "P6":  ("Iván Cepeda", "Abelardo de la Espriella"),
        "P7":  ("Iván Cepeda", "Claudia López"),
        "P8":  ("Iván Cepeda", "Paloma Valencia"),
        "P9":  ("Iván Cepeda", "Juan Manuel Galán"),
        "P10": ("Iván Cepeda", "Aníbal Gaviria"),
        "P11": ("Iván Cepeda", "Vicky Dávila"),
        "P12": ("Iván Cepeda", "Mauricio Cárdenas"),
    }
    for col, (c1, c2) in CNC25_SV_MAP.items():
        sv = sv_col_name(c1, c2)
        if col in df.columns and sv:
            r[sv] = df[col].apply(harmonize)
    return r


def read_cnc_34() -> pd.DataFrame:
    path = (BASE / "34. CNE-E-DG-2026-012431 - CENTRO NACIONAL DE CONSULTORIA"
            / "Base de datos" / "CC893201_POLITICA_REVISTA_CAMBIO_TEXTOS.xlsx")
    df = pd.read_excel(path)

    r = pd.DataFrame(index=df.index)
    r["encuestadora"] = "CNC"
    r["fecha"]        = pd.Timestamp("2026-03-16")
    r["factor"]       = pd.to_numeric(df["FACTOR"], errors="coerce")
    r["departamento"] = df.get("DPTO")
    r["municipio"]    = df.get("MUNICIPIO")
    r["region"]       = df.get("REGION")
    r["zona"]         = df.get("ZONA")
    r["genero"]       = df["GENERO"].apply(harmonize)
    r["edad_grupo"]   = df.get("REDAD")
    r["estrato"]      = df.get("ESTRATO")
    r["educacion"]    = df.get("P19")
    r["aprobacion_petro"] = df["P15"].apply(harmonize)

    r["primera_vuelta"]            = df["P1"].apply(harmonize)
    r["primera_vuelta_espontanea"] = None

    CNC34_SV_MAP = {
        "P7":  ("Iván Cepeda", "Abelardo de la Espriella"),
        "P8":  ("Iván Cepeda", "Paloma Valencia"),
        "P9":  ("Iván Cepeda", "Sergio Fajardo"),
        "P10": ("Iván Cepeda", "Claudia López"),
        "P11": ("Iván Cepeda", "Roy Barreras"),
        "P12": ("Iván Cepeda", "Miguel Uribe Londoño"),
        "P13": ("Iván Cepeda", "Mauricio Lizcano"),
        "P14": ("Iván Cepeda", "Luis Gilberto Murillo"),
    }
    for col, (c1, c2) in CNC34_SV_MAP.items():
        sv = sv_col_name(c1, c2)
        if col in df.columns and sv:
            r[sv] = df[col].apply(harmonize)
    return r


# ─── BUILD & CONCATENATE ──────────────────────────────────────────────────────

print("Reading surveys...")

pieces = [
    # Atlas Intel (5 surveys)
    read_atlas(BASE / "04. CNE-E-DG-2026-000755 - ATLAS INTEL"
               / "Atlas Semana E126 Raw Data 010926.xlsx",       "2026-01-09"),
    read_atlas(BASE / "13. CNE-E-DG-2026-004955 - ATLAS INTEL"
               / "Atlas Semana E226 Raw Data 020526_1 (1).csv",  "2026-02-05"),
    read_atlas(BASE / "27. CNE-E-DG-2026-008698 - ATLAS INTEL"
               / "Base de Dados Atlas Semana 022826_2.xlsx",      "2026-02-28"),
    read_atlas(BASE / "29. CNE-E-DG-2026-011140 - ATLAS INTEL"
               / "Base de Dados Atlas Semana 031226_1.xlsx",      "2026-03-12"),
    read_atlas(BASE / "36. CNE-E-DG-2026-014018 - ATLAS INTEL"
               / "Base de Dados Atlas Semana 041026_1.xlsx",      "2026-04-10"),
    # GAD3 (3 surveys)
    read_gad3_05(),
    read_gad3_23(),
    read_gad3_31(),
    # Invamer (2 surveys)
    read_invamer(BASE / "19. CNE-E-DG-2026-008198 - INVAMER"
                 / "3. Data (Regsitros primarios).xlsx",
                 "2026-02-25", "Colombia Opina 20"),
    read_invamer(BASE / "38. CNE-E-DG-2026-015879 - INVAMER"
                 / "3. Data (Regsitros primarios).xlsx",
                 "2026-04-25", "Colombia Opina 21"),
    # CNC (2 surveys)
    read_cnc_25(),
    read_cnc_34(),
]

print(f"  Loaded {len(pieces)} surveys")

for p in pieces:
    ensure_cols(p, FINAL_COLS)

combined = pd.concat([p[FINAL_COLS] for p in pieces], ignore_index=True)

# ── Normalización de edad_grupo y region ──────────────────────────────────
EDAD_NORM = {
    # Atlas Intel
    "18 - 24": "18-24", "25 - 34": "25-34", "35 - 44": "35-44",
    "45 - 59": "45-59", "60 - 100": "60+",
    # CNC
    "18 a 24": "18-24", "25 a 34": "25-34", "35 a 44": "35-44",
    "45 a 54": "45-54", "55 o más": "55+",
    # GAD3
    "18-24": "18-24", "25-34": "25-34", "35-44": "35-44",
    "45-54": "45-54", "55+": "55+", "35-54": "35-54", "18-34": "18-34",
    # Invamer (etiquetas del mapa)
    "Entre 18 y 24": "18-24", "Entre 25 y 34": "25-34", "Entre 35 y 44": "35-44",
    "Entre 45 y 54": "45-54", "55 ó más": "55+",
    # Invamer (si quedaron códigos numéricos)
    "1": "18-24", "2": "25-34", "3": "35-44", "4": "45-54", "5": "55+",
}

REGION_NORM = {
    # Invamer códigos numéricos
    "1": "Caribe", "2": "Centro - Oriente", "3": "Eje Cafetero",
    "4": "Pacífico", "5": "Centro - Sur - Amazonía", "6": "Llano",
    # GAD3 (mayúsculas)
    "CENTRAL": "Central", "CARIBE": "Caribe", "ORIENTAL": "Centro - Oriente",
    "PACIFICA": "Pacífico", "BOGOTA": "Bogotá", "AMAZ-ORIN": "Amazonía - Orinoquía",
    # Atlas Intel
    "Pacífica": "Pacífico", "Bogotá D.C.": "Bogotá",
    "Amazonía y Orinoquía": "Amazonía - Orinoquía",
    # Variantes con guion largo (–) → guion normal (-)
    "Centro \u2013 Sur - Amazonía": "Centro - Sur - Amazonía",
    "Centro \u2013 Sur - Amazon\u00eda": "Centro - Sur - Amazonía",
}

def _norm(val, mapping):
    if pd.isna(val):
        return None
    s = str(val).strip()
    # "1.0" → "1"
    try:
        s = str(int(float(s)))
    except (ValueError, TypeError):
        pass
    return mapping.get(s, s)

combined["edad_grupo"] = combined["edad_grupo"].apply(lambda x: _norm(x, EDAD_NORM))
combined["region"]     = combined["region"].apply(lambda x: _norm(x, REGION_NORM))
# ──────────────────────────────────────────────────────────────────────────

combined.to_excel(OUT, index=False)

print(f"\nSaved -> {OUT}")
print(f"Total rows: {len(combined):,}  |  Columns: {len(combined.columns)}")
print("\nRows per encuestadora:")
print(combined.groupby("encuestadora").size().to_string())
print("\nDate range per encuestadora:")
print(combined.groupby("encuestadora")["fecha"].agg(["min", "max"]).to_string())
print("\nCoverage por sv column (encuestadoras con datos):")
sv_coverage = {}
for col in SV_COLS:
    encs = combined[combined[col].notna()]["encuestadora"].unique()
    sv_coverage[col] = list(encs)
for col, encs in sv_coverage.items():
    print(f"  {col}: {encs}")
