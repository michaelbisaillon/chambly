#!/usr/bin/env python3
"""Génère le portrait économique et géographique de Chambly.

Sources (dans ce dépôt) :
  - data/recensement_chambly_{2011,2016,2021}.csv (recensements)
  - data/SDR_chambly.gpkg, data/occupant_chambly_final.gpkg (géométries)
  - ../OpenStreetMap n'est pas requis ; la carte utilise SDR_chambly.gpkg
    et, si présent, le dépôt OpenStreetMap cloné à côté pour commerces_chambly.gpkg.

Sorties (dans analyses/) : portrait_chambly.png, carte_chambly.png, portrait_chambly.md
"""
import csv
import json
import os
import sqlite3
import struct
import collections

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Polygon as MplPolygon
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
DATA = os.path.join(ROOT, "data")


def load_year(y):
    path = os.path.join(DATA, f"recensement_chambly_{y}.csv")
    with open(path, encoding="utf-8-sig") as f:
        rows = list(csv.DictReader(f))
    out = []
    for r in rows:
        out.append({
            "annee": r["annee"],
            "theme": (r["theme"] or "").strip(),
            "id": (r["id_caracteristiques"] or "").strip(),
            "nom": (r.get("nom_caracteristique") or r.get("nom_caracteristiques") or "").strip(),
            "total": (r.get("chiffre_total") or "").strip(),
            "H": (r.get("chiffre_hommes") or "").strip(),
            "F": (r.get("chiffre_femmes") or r.get("chiffres_femmes") or "").strip(),
            "taux": (r.get("taux_total") or "").strip(),
        })
    return out


def num(s):
    s = (s or "").strip().replace(",", ".")
    try:
        return float(s)
    except ValueError:
        return None


def fr(x):
    return f"{x:,.0f}".replace(",", " ")


def clean_scian(n):
    return n.split(" ", 1)[1] if n[:2].strip().isdigit() else n


r11, r16, r21 = load_year(2011), load_year(2016), load_year(2021)


def find(rows, theme_pred, nom_pred=lambda r: True):
    return [r for r in rows if theme_pred(r["theme"]) and nom_pred(r)]


def one(rows, theme_pred, nom_pred=lambda r: True):
    for r in rows:
        if theme_pred(r["theme"]) and nom_pred(r):
            return r
    return None


pop_theme = lambda t: t.lower().startswith(("population et logements", "chiffres de population"))
age_theme = lambda t: t == "Caractéristiques selon l'âge"

pop_2006 = num(one(r11, pop_theme, lambda r: "2006" in r["nom"])["total"])
pop = {
    2006: pop_2006,
    2011: num(one(r11, pop_theme, lambda r: r["nom"].startswith("Population en 2011"))["total"]),
    2016: num(one(r16, pop_theme, lambda r: "2016" in r["nom"] and ";" in r["nom"])["total"]),
    2021: num(one(r21, pop_theme, lambda r: "2021" in r["nom"] and "," in r["nom"])["total"]),
}
logements = {
    2011: num(one(r11, pop_theme, lambda r: "Total des logements" in r["nom"])["total"]),
    2016: num(one(r16, pop_theme, lambda r: "Total des logements" in r["nom"])["total"]),
    2021: num(one(r21, pop_theme, lambda r: "Total des logements" in r["nom"])["total"]),
}
densite = num(one(r21, pop_theme, lambda r: "Densité" in r["nom"])["total"])
superficie = num(one(r21, pop_theme, lambda r: "Superficie" in r["nom"])["total"])

age_groups = [
    (r["nom"], num(r["total"]), num(r["taux"]))
    for r in r21
    if age_theme(r["theme"]) and r["id"] in ("9", "13", "24")
]
age_moyen = num(one(r21, age_theme, lambda r: "moyen" in r["nom"])["total"])
age_median = num(one(r21, age_theme, lambda r: "médian" in r["nom"])["total"])

sit_act = lambda t: t == "Situation d'activité"
emp21 = {r["nom"]: num(r["total"]) for r in r21 if sit_act(r["theme"])}
emp16 = {r["nom"]: num(r["total"]) for r in r16 if sit_act(r["theme"])}
taux_emp = {
    "Taux d'activité": (num(one(r16, sit_act, lambda r: "Taux d'activité" in r["nom"])["total"]),
                        num(one(r21, sit_act, lambda r: "Taux d'activité" in r["nom"])["total"])),
    "Taux d'emploi": (num(one(r16, sit_act, lambda r: "Taux d'emploi" in r["nom"])["total"]),
                      num(one(r21, sit_act, lambda r: "Taux d'emploi" in r["nom"])["total"])),
    "Taux de chômage": (num(one(r16, sit_act, lambda r: "Taux de chômage" in r["nom"])["total"]),
                        num(one(r21, sit_act, lambda r: "Taux de chômage" in r["nom"])["total"])),
}

transport = [(r["nom"], num(r["taux"])) for r in r21 if "mode de transport" in r["theme"].lower() and r["id"] != "2603"]
duree = [(r["nom"], num(r["taux"])) for r in r21 if "durée du navettage" in r["theme"].lower() and r["id"] != "2611"]

menage_med = {
    2015: num(one(r16, lambda t: "revenu du ménage en 2015" in t.lower(),
                  lambda r: "médian des ménages en 2015" in r["nom"])["total"]),
    2020: num(one(r21, lambda t: "revenu du ménage en 2020" in t.lower(),
                  lambda r: "médian des ménages en 2020" in r["nom"])["total"]),
}
part_med_2020 = num(one(r21, lambda t: "revenu des particuliers en 2020" in t.lower(),
                        lambda r: "Revenu total médian" in r["nom"])["total"])
plein_temps = num(one(r21, lambda t: "revenu des particuliers en 2020" in t.lower(),
                      lambda r: "toute l'année à plein temps" in r["nom"] and "médian" in r["nom"])["total"])
def _mfr(r):
    return "fréquence" in r["nom"].lower() and "mfr-api" in r["nom"].lower()


faible_rev = {
    2015: num(one(r16, lambda t: "faible revenu en 2015" in t.lower(), _mfr)["total"]),
    2020: num(one(r21, lambda t: "faible revenu" in t.lower(), _mfr)["total"]),
}
gini_apres = num(one(r21, lambda t: "faible revenu" in t.lower(),
                     lambda r: "Gini sur le revenu après impôt" in r["nom"])["total"])
gini_marche = num(one(r21, lambda t: "faible revenu" in t.lower(),
                      lambda r: "Gini sur le revenu du marché" in r["nom"])["total"])

industrie = sorted(
    [(r["nom"], num(r["taux"])) for r in r21 if r["theme"].startswith("Industrie") and r["id"] not in ("2259", "2260", "2261")],
    key=lambda x: x[1], reverse=True,
)

logement_types = [(r["nom"], num(r["taux"])) for r in r21
                  if "ménages et des logements" in r["theme"].lower() and r["id"] in ("42", "43", "44", "45", "46")]
taille_menage = num(one(r21, lambda t: "ménages et des logements" in t.lower(),
                        lambda r: "Taille moyenne des ménages" in r["nom"])["total"])

langues = [(r["nom"], num(r["taux"])) for r in r21 if "connaissance des langues officielles" in r["theme"].lower() and r["id"] != "383"]
immigrants = num(one(r21, lambda t: "statut d'immigrant" in t.lower(),
                     lambda r: r["nom"] == "Immigrants")["total"])
immigrants_pct = num(one(r21, lambda t: "statut d'immigrant" in t.lower(),
                         lambda r: r["nom"] == "Immigrants")["taux"])
pas_demenage = num(one(r21, lambda t: t.startswith("Mobilité 1 an"),
                       lambda r: "n'ayant pas déménagé" in r["nom"])["taux"])

# ---------------- Figure 1 : tableau de bord ----------------
fig, axes = plt.subplots(3, 3, figsize=(17, 13))
fig.suptitle("Portrait économique de Chambly (Québec) — recensements 2011/2016/2021 (dépôt chambly, data/recensement_chambly_*.csv)",
             fontsize=13, fontweight="bold")

ax = axes[0, 0]
years = list(pop.keys())
bars = ax.bar([str(y) for y in years], [pop[y] for y in years], color="#4269d0")
for b, y in zip(bars, years):
    ax.text(b.get_x() + b.get_width() / 2, b.get_height() + 400, f"{pop[y]:,.0f}", ha="center", fontsize=9)
ax.set_title("Démographie — population")
ax.set_ylabel("Habitants"); ax.set_ylim(0, 36000)
ax.text(0.02, 0.95, f"+{round((pop[2021]/pop[2006]-1)*100,1)} % depuis 2006\nLogements privés 2021 : {logements[2021]:,.0f} (densité {densite:,.0f} hab./km²)",
        transform=ax.transAxes, va="top", fontsize=9, bbox=dict(fc="#eef", ec="#4269d0"))

ax = axes[0, 1]
labels = [a[0] for a in age_groups][::-1]; vals = [a[1] for a in age_groups][::-1]; pcts = [a[2] for a in age_groups][::-1]
ax.barh(labels, vals, color="#2ca02c")
for i, (v, p) in enumerate(zip(vals, pcts)):
    ax.text(v + 250, i, f"{v:,.0f} ({p} %)", va="center", fontsize=9)
ax.set_title("Structure par âge — 2021"); ax.set_xlim(0, 26000)
ax.text(0.98, 0.05, f"Âge moyen {age_moyen} ans — médian {age_median} ans", transform=ax.transAxes, ha="right", fontsize=9,
        bbox=dict(fc="#efe", ec="#2ca02c"))

ax = axes[0, 2]; ax.axis("off")
ax.set_title("Société — faits saillants 2021")
lang_txt = "\n".join(f"   {n} : {p} %" for n, p in langues)
txt = (f"• Langues officielles 2021 :\n{lang_txt}\n"
       f"• Immigrants : {immigrants_pct} % ({immigrants:,.0f} pers.)\n"
       f"• {pas_demenage} % n'ont pas déménagé dans l'année\n"
       f"• Taille moyenne des ménages : {taille_menage} pers.")
ax.text(0.02, 0.98, txt, transform=ax.transAxes, va="top", fontsize=9.5, family="monospace")

ax = axes[1, 0]
x = np.arange(3); w = 0.35
v16 = [taux_emp[k][0] for k in taux_emp]; v21 = [taux_emp[k][1] for k in taux_emp]
ax.bar(x - w / 2, v16, w, label="2016", color="#999999")
ax.bar(x + w / 2, v21, w, label="2021", color="#d62728")
for xi, v in zip(x, v21):
    ax.text(xi + w / 2, v + 1, f"{v}%", ha="center", fontsize=9)
ax.set_xticks(x); ax.set_xticklabels(list(taux_emp.keys()), fontsize=9)
ax.set_title("Emploi — population 15 ans et plus (%)"); ax.legend(); ax.set_ylim(0, 90)

ax = axes[1, 1]
labels = [t[0] for t in transport][::-1]; vals = [t[1] for t in transport][::-1]
ax.barh(labels, vals, color="#ff7f0e")
for i, v in enumerate(vals):
    ax.text(v + 1, i, f"{v} %", va="center", fontsize=9)
ax.set_title("Navettage — mode de transport 2021 (%)"); ax.set_xlim(0, 100)

ax = axes[1, 2]
labels = [d[0] for d in duree]; vals = [d[1] for d in duree]
bars = ax.bar(labels, vals, color="#9467bd")
for b, v in zip(bars, vals):
    ax.text(b.get_x() + b.get_width() / 2, v + 0.6, f"{v}%", ha="center", fontsize=9)
ax.set_title("Durée du trajet domicile-travail 2021 (%)"); ax.set_ylim(0, 40)
plt.setp(ax.get_xticklabels(), fontsize=8)

ax = axes[2, 0]
rev = [("Ménage médian\n2015", menage_med[2015] / 1000), ("Ménage médian\n2020", menage_med[2020] / 1000),
       ("Particulier médian\n2020", part_med_2020 / 1000), ("Plein temps médian\n2020", plein_temps / 1000)]
labels = [r[0] for r in rev]; vals = [r[1] for r in rev]
bars = ax.bar(labels, vals, color="#17becf")
for b, v in zip(bars, vals):
    ax.text(b.get_x() + b.get_width() / 2, v + 1.5, f"{v:.0f} k$", ha="center", fontsize=9)
ax.set_title("Revenus médians ($ canadiens, en k$)"); ax.set_ylim(0, 120)
plt.setp(ax.get_xticklabels(), fontsize=8)
ax.text(0.02, 0.97, f"Faible revenu (MFR-ApI) : {faible_rev[2015]} % (2015) → {faible_rev[2020]} % (2020)\nGini après impôt 2021 : {gini_apres} (marché : {gini_marche})",
        transform=ax.transAxes, va="top", fontsize=9, bbox=dict(fc="#eff", ec="#17becf"))

ax = axes[2, 1]
top_ind = industrie[:8]
labels = [i[0][:42] for i in top_ind][::-1]; vals = [i[1] for i in top_ind][::-1]
ax.barh(labels, vals, color="#8c564b")
for i, v in enumerate(vals):
    ax.text(v + 0.2, i, f"{v} %", va="center", fontsize=9)
ax.set_title("Industrie (SCIAN 2017) — % pop. active 2021"); ax.set_xlim(0, 16)
plt.setp(ax.get_yticklabels(), fontsize=8)

ax = axes[2, 2]
labels = [l[0] for l in logement_types][::-1]; vals = [l[1] for l in logement_types][::-1]
ax.barh(labels, vals, color="#e377c2")
for i, v in enumerate(vals):
    ax.text(v + 0.8, i, f"{v} %", va="center", fontsize=9)
ax.set_title("Logements privés occupés 2021 — type (%)"); ax.set_xlim(0, 65)
plt.setp(ax.get_yticklabels(), fontsize=8)

fig.tight_layout(rect=[0, 0, 1, 0.96])
fig.savefig(os.path.join(HERE, "portrait_chambly.png"), dpi=100)
plt.close(fig)
print("portrait_chambly.png OK")

# ---------------- Figure 2 : carte ----------------
def parse_gpkg(blob):
    flags = blob[3]
    env_ind = (flags >> 1) & 0x07
    env_len = {0: 0, 1: 32, 2: 48, 3: 48, 4: 64}[env_ind]
    return parse_wkb(blob[8 + env_len:])


def parse_wkb(b, off=0):
    bo = "<" if b[off] == 1 else ">"
    gtype = struct.unpack_from(bo + "I", b, off + 1)[0]
    off += 5
    base = gtype % 1000
    if base == 1:
        x, y = struct.unpack_from(bo + "2d", b, off)
        return ("Point", (x, y)), off + 16
    if base == 3:
        nr = struct.unpack_from(bo + "I", b, off)[0]; off += 4
        rings = []
        for _ in range(nr):
            n = struct.unpack_from(bo + "I", b, off)[0]; off += 4
            rings.append([struct.unpack_from(bo + "2d", b, off + 16 * i) for i in range(n)])
            off += 16 * n
        return ("Polygon", rings), off
    if base == 6:
        n = struct.unpack_from(bo + "I", b, off)[0]; off += 4
        polys = []
        for _ in range(n):
            g, off = parse_wkb(b, off)
            polys.append(g[1])
        return ("MultiPolygon", polys), off
    raise ValueError(gtype)


con = sqlite3.connect(os.path.join(DATA, "SDR_chambly.gpkg"))
geom, sdr_nom = con.execute("SELECT geom, sdrnom FROM SDR_chambly").fetchone()
con.close()
g, _ = parse_gpkg(geom)
rings = [r for p in g[1] for r in p]
ring = [[round(x, 4), round(y, 4)] for x, y in rings[0]]

com_path = None
for cand in [os.path.join(ROOT, "..", "github__michaelbisaillon__OpenStreetMap", "data", "commerces_chambly.gpkg"),
             os.path.join(DATA, "commerces_chambly.gpkg")]:
    if os.path.exists(cand):
        com_path = cand
        break
com = []
if com_path:
    con = sqlite3.connect(com_path)
    for geom, name, key in con.execute("SELECT geom, name, key FROM commerces_chambly"):
        gg, _ = parse_gpkg(geom)
        if gg[0] == "Point":
            com.append((gg[1][0], gg[1][1], key, name))
    con.close()

con = sqlite3.connect(os.path.join(DATA, "occupant_chambly_final.gpkg"))
usage = collections.Counter(r[0] for r in con.execute("SELECT usage_1 FROM occupant_final")).most_common(10)
total_occup = con.execute("SELECT COUNT(*) FROM occupant_final").fetchone()[0]
con.close()

colors = {"shop": "#d62728", "amenity": "#1f77b4", "office": "#2ca02c",
          "leisure": "#9467bd", "tourism": "#ff7f0e", "healthcare": "#8c564b"}
labels_fr = {"shop": "Commerces (shop)", "amenity": "Services/équipements (amenity)",
             "office": "Bureaux (office)", "leisure": "Loisirs (leisure)",
             "tourism": "Tourisme (tourism)", "healthcare": "Santé (healthcare)"}

fig = plt.figure(figsize=(15.5, 8.5))
gs = fig.add_gridspec(1, 2, width_ratios=[2.3, 1], wspace=0.18)
ax = fig.add_subplot(gs[0])
xs = [p[0] for p in ring]; ys = [p[1] for p in ring]
ax.add_patch(MplPolygon(list(zip(xs, ys)), closed=True, facecolor="#d5e8d4", edgecolor="#2e7d32", lw=1.8, zorder=1))
ax.set_aspect(1 / np.cos(np.radians(45.45)))
cnt = collections.Counter(c[2] for c in com)
for key in ["shop", "amenity", "office", "healthcare", "leisure", "tourism"]:
    pts = [(c[0], c[1]) for c in com if c[2] == key]
    if pts:
        ax.scatter([p[0] for p in pts], [p[1] for p in pts], s=26, c=colors[key],
                   label=f"{labels_fr[key]} ({cnt[key]})", edgecolors="white", linewidths=0.4, zorder=3, alpha=0.9)
if com:
    ax.annotate("Fort-Chambly", xy=(-73.2744, 45.4488), xytext=(-73.268, 45.4435), fontsize=9, fontweight="bold",
                arrowprops=dict(arrowstyle="->", color="black", lw=0.8))
    ax.annotate("Marina de Chambly", xy=(-73.2822, 45.4473), xytext=(-73.276, 45.4415), fontsize=8,
                arrowprops=dict(arrowstyle="->", color="black", lw=0.6))
    ax.text(-73.3008, 45.4528, "Centre-ville\n(boul. de Périgny)", fontsize=8, ha="center", style="italic")
x0, x1 = min(xs), max(xs); y0, y1 = min(ys), max(ys)
ax.set_xlim(x0 - 0.004, x1 + 0.004); ax.set_ylim(y0 - 0.003, y1 + 0.003)
ax.plot([x0 + 0.003, x0 + 0.003 + 0.0254], [y0 + 0.0012, y0 + 0.0012], color="black", lw=2.5)
ax.text(x0 + 0.003 + 0.0127, y0 + 0.0018, "2 km", ha="center", fontsize=8)
ax.set_title(f"Carte de {sdr_nom} — limite municipale (SDR) et {len(com)} points d'intérêt OSM", fontsize=10)
ax.set_xlabel("Longitude"); ax.set_ylabel("Latitude")
ax.legend(loc="upper right", fontsize=8, framealpha=0.9)
ax.grid(alpha=0.25, lw=0.4)

ax2 = fig.add_subplot(gs[1])
labels = [u[0] for u in usage][::-1]; vals = [u[1] for u in usage][::-1]
ax2.barh(labels, vals, color="#4269d0")
for i, v in enumerate(vals):
    ax2.text(v + 1.5, i, str(v), va="center", fontsize=9)
ax2.set_title(f"{fr(total_occup)} établissements répertoriés — par usage", fontsize=10)
ax2.set_xlim(0, max(vals) * 1.15)
plt.setp(ax2.get_yticklabels(), fontsize=8)

fig.suptitle("Portrait géographique et commercial de Chambly", fontsize=13, fontweight="bold")
fig.tight_layout(rect=[0, 0, 1, 0.94])
fig.savefig(os.path.join(HERE, "carte_chambly.png"), dpi=100)
plt.close(fig)
print("carte_chambly.png OK", "| commerces:", len(com))

# ---------------- Rapport markdown ----------------
md = f"""# Portrait économique et géographique de Chambly (Québec)

Analyse Python (matplotlib) réalisée à partir des données de ce dépôt :
`data/recensement_chambly_2011.csv`, `_2016.csv`, `_2021.csv`,
`data/SDR_chambly.gpkg`, `data/occupant_chambly_final.gpkg` (+ `commerces_chambly.gpkg` du dépôt OpenStreetMap).

![Portrait économique](portrait_chambly.png)
![Carte de Chambly](carte_chambly.png)

## Faits saillants

- **Démographie** : population {fr(pop[2006])} (2006) → {fr(pop[2021])} (2021), soit +{round((pop[2021]/pop[2006]-1)*100,1)} % en 15 ans.
  0-14 ans : {age_groups[0][2]} % | 15-64 ans : {age_groups[1][2]} % | 65+ : {age_groups[2][2]} % ; âge moyen {age_moyen} ans.
  {fr(logements[2021])} logements privés en 2021 (densité {densite:,.0f} hab./km² sur {superficie} km²).
- **Emploi (2021)** : taux d'activité {taux_emp['Taux d\'activité'][1]} %, taux d'emploi {taux_emp['Taux d\'emploi'][1]} %, chômage {taux_emp['Taux de chômage'][1]} %
  (2016 : {taux_emp['Taux d\'activité'][0]} % / {taux_emp['Taux d\'emploi'][0]} % / {taux_emp['Taux de chômage'][0]} %).
  Navettage : {transport[0][0].lower()} {transport[0][1]} %.
- **Revenus** : ménage médian {fr(menage_med[2015])} $ (2015) → {fr(menage_med[2020])} $ (2020) ; particulier médian 2020 {fr(part_med_2020)} $ ;
  plein temps médian {fr(plein_temps)} $. Faible revenu (MFR-ApI) : {faible_rev[2015]} % → {faible_rev[2020]} %.
  Gini après impôt 2021 : {gini_apres} (marché {gini_marche}).
- **Industrie 2021 (SCIAN, % pop. active)** : {", ".join(f"{clean_scian(n).lower()} {v} %" for n, v in industrie[:3])}.
- **Société 2021** : {pas_demenage} % sans déménagement dans l'année ; immigrants {immigrants_pct} % ; bilingues fr+en {dict(langues).get('Français et anglais', 0)} %.
- **Carte** : {len(com)} points OSM à Chambly (dont {cnt.get('shop', 0)} commerces), {fr(total_occup)} établissements répertoriés.

## Reproduction

```bash
pip install matplotlib pandas
python analyses/generer_portrait.py   # depuis la racine du dépôt
```
"""
with open(os.path.join(HERE, "portrait_chambly.md"), "w", encoding="utf-8") as f:
    f.write(md)
print("portrait_chambly.md OK")
