"""
Gera as 4 figuras do Relatório Metodológico sob o The Broadsheet Design System (design-rules.md)
com estética editorial de jornal clássico (Financial Times / NYT), alta densidade analítica,
paleta de papel e tinta tipográfica, réguas nítidas (hairlines) e dados do DeltaFolha 2026.
"""

import os
import zipfile
from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.patches as patches

# Diretório de saída
OUT_DIR = Path("docs/figures")
OUT_DIR.mkdir(parents=True, exist_ok=True)

# Tipografia e estética global editorial
plt.rcParams['font.family'] = 'serif'
plt.rcParams['font.serif'] = ['Georgia', 'Times New Roman', 'DejaVu Serif', 'serif']
plt.rcParams['axes.edgecolor'] = '#D8CCC0'
plt.rcParams['axes.linewidth'] = 1.0

# Paleta The Broadsheet (design-rules.md)
COLOR_PAPER_BG = '#FBF9F5'        # Papel Marfim Nobre (NYT / WSJ)
COLOR_CARD_BG = '#FFFFFF'         # Branco papel puro
COLOR_INK_PRIMARY = '#121212'     # Tinta preta de imprensa
COLOR_INK_SECONDARY = '#4A4643'   # Grafite de apoio
COLOR_INK_MUTED = '#7A746E'       # Metadados
COLOR_HAIRLINE = '#D8CCC0'        # Borda 1px hairline
COLOR_STRONG_RULE = '#121212'     # Linha de corte / régua de topo

COLOR_CLARET = '#990F3D'          # Vinho Borgonha FT
COLOR_COBALT = '#004F9F'          # Azul Político WaPo
COLOR_TEAL = '#0D7680'            # Verde Petróleo Editorial
COLOR_GOLD = '#9E7B30'            # Areia / Ouro Politon
COLOR_AMBER = '#B45309'           # Âmbar Queimado
COLOR_CRIMSON = '#B91C1C'         # Carmim


# ======================================================================
# FIGURA 1: FLUXO DA TRIANGULAÇÃO METODOLÓGICA (SIMPLIFICADA E FONTES GRANDES)
# ======================================================================
def generate_fig1_fluxo():
    fig, ax = plt.subplots(figsize=(15.5, 8.4), dpi=300)
    fig.patch.set_facecolor('#FFFFFF')
    ax.set_facecolor('#FFFFFF')
    ax.axis('off')

    # Régua superior dupla editorial
    ax.plot([0.04, 0.96], [0.965, 0.965], color='#06333D', linewidth=2.8)
    ax.plot([0.04, 0.96], [0.956, 0.956], color='#06333D', linewidth=0.9)

    # Kicker e Título Principal em fontes amplas
    ax.text(0.04, 0.918, "ARQUITETURA METODOLÓGICA • ELEIÇÕES GERAIS 2026",
            fontsize=12.5, fontweight='bold', fontfamily='sans-serif', color='#005B68')
    ax.text(0.04, 0.865, "Triangulação de Três Pilares Independentes",
            fontsize=24, fontweight='bold', fontfamily='serif', color='#111827')
    ax.text(0.04, 0.825, "Mapeamento empírico do espectro político-partidário a partir de fontes consagradas",
            fontsize=14.0, style='italic', fontfamily='serif', color='#4B5563')

    # Dimensões dos 3 blocos superiores
    w_box = 0.285
    h_box = 0.235
    y_top = 0.505

    pilares = [
        {
            "x": 0.050,
            "header": "PILAR 1 • COMPORTAMENTAL",
            "titulo": "GPS Partidário 2026",
            "fonte": "DeltaFolha / Folha de S.Paulo",
            "desc": "Votações nominais e alianças de bancada",
            "cor": "#005B68"
        },
        {
            "x": 0.358,
            "header": "PILAR 2 • ESPECIALISTAS",
            "titulo": "Levantamento ABCP",
            "fonte": "Bolognesi et al. (Revista DADOS)",
            "desc": "Avaliação de cientistas políticos",
            "cor": "#990F3D"
        },
        {
            "x": 0.665,
            "header": "PILAR 3 • PARLAMENTAR",
            "titulo": "Pesquisa BLS",
            "fonte": "Power & Zucco Jr. (Oxford / FGV)",
            "desc": "Microdados de deputados e senadores",
            "cor": "#1E3A8A"
        }
    ]

    for p in pilares:
        # Fundo do Card
        rect = patches.FancyBboxPatch((p["x"], y_top), w_box, h_box,
                                      boxstyle="round,pad=0.012,rounding_size=0.015",
                                      facecolor='#FAF8F5', edgecolor='#D8CCC0', linewidth=1.4)
        ax.add_patch(rect)
        # Borda superior colorida
        ax.plot([p["x"], p["x"] + w_box], [y_top + h_box, y_top + h_box], color=p["cor"], linewidth=4.5)

        # Textos com fontes ampliadas e excelente espaçamento
        ax.text(p["x"] + w_box/2, y_top + h_box - 0.045, p["header"],
                fontsize=12.0, fontweight='bold', fontfamily='sans-serif', color=p["cor"], ha='center')
        ax.text(p["x"] + w_box/2, y_top + h_box - 0.102, p["titulo"],
                fontsize=18.0, fontweight='bold', fontfamily='serif', color='#111827', ha='center')
        ax.text(p["x"] + w_box/2, y_top + h_box - 0.150, p["fonte"],
                fontsize=13.0, fontweight='bold', fontfamily='sans-serif', color='#374151', ha='center')
        ax.text(p["x"] + w_box/2, y_top + 0.038, p["desc"],
                fontsize=12.5, style='italic', fontfamily='sans-serif', color='#4B5563', ha='center')

    # Setas conectoras para o bloco central
    ax.annotate('', xy=(0.50, 0.435), xytext=(0.050 + w_box/2, y_top),
                arrowprops=dict(arrowstyle='->,head_width=0.38,head_length=0.50', color='#06333D', lw=1.8, connectionstyle='arc3,rad=-0.07'))
    ax.annotate('', xy=(0.50, 0.435), xytext=(0.50, y_top),
                arrowprops=dict(arrowstyle='->,head_width=0.38,head_length=0.50', color='#06333D', lw=1.8))
    ax.annotate('', xy=(0.50, 0.435), xytext=(0.665 + w_box/2, y_top),
                arrowprops=dict(arrowstyle='->,head_width=0.38,head_length=0.50', color='#06333D', lw=1.8, connectionstyle='arc3,rad=0.07'))

    # Bloco Central: Índice Sintético Triangulado
    y_mid = 0.270
    w_mid = 0.680
    rect_mid = patches.FancyBboxPatch((0.160, y_mid), w_mid, 0.140,
                                      boxstyle="round,pad=0.012,rounding_size=0.015",
                                      facecolor='#FFFFFF', edgecolor='#06333D', linewidth=1.8)
    ax.add_patch(rect_mid)
    ax.plot([0.160, 0.160 + w_mid], [y_mid + 0.140, y_mid + 0.140], color='#06333D', linewidth=4.0)

    ax.text(0.50, y_mid + 0.088, "ÍNDICE SINTÉTICO TRIANGULADO • ESCALA 0 A 100",
            fontsize=16.0, fontweight='bold', fontfamily='sans-serif', color='#06333D', ha='center')
    ax.text(0.50, y_mid + 0.038, "Média das fontes disponíveis padronizadas de 0 (Extrema-Esquerda) a 100 (Extrema-Direita)",
            fontsize=13.5, fontfamily='sans-serif', color='#374151', ha='center')

    # Seta para a régua inferior
    ax.annotate('', xy=(0.50, 0.190), xytext=(0.50, y_mid),
                arrowprops=dict(arrowstyle='->,head_width=0.38,head_length=0.50', color='#06333D', lw=1.8))

    # Régua Inferior: 7 Faixas Orientativas
    y_bot = 0.030
    w_bot = 0.920
    rect_bot = patches.FancyBboxPatch((0.040, y_bot), w_bot, 0.145,
                                      boxstyle="round,pad=0.012,rounding_size=0.015",
                                      facecolor='#06333D', edgecolor='#06333D', linewidth=1.2)
    ax.add_patch(rect_bot)
    ax.text(0.50, y_bot + 0.098, "7 FAIXAS CONCEITUAIS ORIENTATIVAS",
            fontsize=15.5, fontweight='bold', fontfamily='sans-serif', color='#FFFFFF', ha='center')
    
    bot_l1 = "1. Extrema-Esquerda [0-14]   •   2. Esquerda [14-28]   •   3. Centro-Esquerda [28-42]   •   4. Centro [42-58]"
    bot_l2 = "5. Centro-Direita [58-72]   •   6. Direita [72-86]   •   7. Extrema-Direita [86-100]"
    
    ax.text(0.50, y_bot + 0.054, bot_l1,
            fontsize=13.5, fontweight='bold', fontfamily='sans-serif', color='#E2F4F7', ha='center')
    ax.text(0.50, y_bot + 0.018, bot_l2,
            fontsize=13.5, fontweight='bold', fontfamily='sans-serif', color='#E2F4F7', ha='center')

    plt.tight_layout()
    out_path = OUT_DIR / "fluxo_metodologico_triangulacao.png"
    plt.savefig(out_path, dpi=300, bbox_inches='tight', facecolor='#FFFFFF', edgecolor='none')
    plt.close()
    print(f"[OK] Gerado com Broadsheet Design System: {out_path}")


# ======================================================================
# FIGURA 2: ESPECTRO PARTIDÁRIO 2026 (FONTES GRANDES E NÍTIDAS)
# ======================================================================
def generate_fig2_espectro():
    df = pd.read_csv('data/partidos_ideologia.csv')
    # Remover duplicatas técnicas: PCDOB (já existe 'PC do B') e PMB (já existe 'DEMOCRATA')
    df = df[~df['sigla'].isin(['PCDOB', 'PMB'])].sort_values('indice_sintetico', ascending=True).reset_index(drop=True)

    fig, ax = plt.subplots(figsize=(16.0, 15.5), dpi=300)
    fig.patch.set_facecolor('#FFFFFF')
    ax.set_facecolor('#FFFFFF')

    # Faixas conceituais com tons pastéis neutros e elegantes
    faixas = [
        (0, 14, '1. Extrema-Esquerda', '#FDF6F6'),
        (14, 28, '2. Esquerda', '#FDF8F6'),
        (28, 42, '3. Centro-Esquerda', '#FFFDF7'),
        (42, 58, '4. Centro', '#F8FAF9'),
        (58, 72, '5. Centro-Direita', '#F4F9F9'),
        (72, 86, '6. Direita', '#F2F7FA'),
        (86, 100, '7. Extrema-Direita', '#F8F5FA')
    ]
    
    for x0, x1, name, bg in faixas:
        ax.axvspan(x0, x1, color=bg, alpha=0.85, zorder=1)
        ax.axvline(x1, color='#CBD5E1', linestyle='-', linewidth=0.9, zorder=2)
        ax.text((x0 + x1)/2, len(df) - 0.12, name.upper(), fontsize=11.5, fontweight='bold',
                fontfamily='sans-serif', color='#334155', ha='center', va='bottom', zorder=2)

    y_pos = np.arange(len(df))

    # Réguas horizontais finas
    for y in y_pos:
        ax.axhline(y, color='#F1EFEA', linestyle='-', linewidth=0.8, zorder=2)

    # Coluna de cabeçalho do índice sintético no canto direito
    ax.text(105.0, len(df) - 0.12, "ÍNDICE", fontsize=13.5, fontweight='bold',
            fontfamily='sans-serif', color='#06333D', ha='center', va='bottom', zorder=2)

    # Plotar dispersão das 3 fontes e o índice sintético
    for idx, row in df.iterrows():
        y = idx
        val_folha = row['folha_score']
        val_bolo = row['bolognesi_adj']
        val_bls = row['bls_adj']
        val_idx = row['indice_sintetico']
        cor = row['cor_hex'] if pd.notna(row['cor_hex']) else '#005B68'

        pts = [v for v in [val_folha, val_bolo, val_bls] if pd.notna(v)]
        if len(pts) > 1:
            ax.plot([min(pts), max(pts)], [y, y], color='#94A3B8', linewidth=1.8, zorder=3)

        # Marcadores das fontes individuais
        if pd.notna(val_folha):
            ax.scatter(val_folha, y, color='#990F3D', s=70, marker='s', edgecolors='#5C0924', lw=0.9, zorder=4)
        if pd.notna(val_bolo):
            ax.scatter(val_bolo, y, color='#1E3A8A', s=80, marker='^', edgecolors='#0F172A', lw=0.9, zorder=4)
        if pd.notna(val_bls):
            ax.scatter(val_bls, y, color='#005B68', s=80, marker='o', edgecolors='#06333D', lw=0.9, zorder=4)

        # Ponto do Índice Sintético Consolidado
        ax.scatter(val_idx, y, color=cor, s=150, edgecolors='#06333D', linewidth=1.8, zorder=5)

        # Badge do Índice na coluna direita dedicada (alinhamento limpo sem colisões)
        ax.text(105.0, y, f"{val_idx:.1f}", fontsize=13.0, fontweight='bold', fontfamily='sans-serif',
                color='#06333D', va='center', ha='center', zorder=6,
                bbox=dict(boxstyle='square,pad=0.28', facecolor='#F8FAF9', edgecolor='#CBD5E1', lw=0.9))

    ax.set_yticks(y_pos)
    labels = []
    for s in df['sigla']:
        if s in ['PC do B', 'PCDOB']:
            labels.append('PCdoB')
        elif s == 'DEMOCRATA':
            labels.append('DEMOCRATA (PMB)')
        else:
            labels.append(s)
            
    ax.set_yticklabels(labels, fontsize=14.5, fontweight='bold', fontfamily='serif', color='#111827')
    ax.set_xlim(-2, 109)
    ax.set_ylim(-0.8, len(df) + 0.6)

    # Réguas de eixos
    ax.set_xlabel('Posicionamento Político Padronizado (Escala Contínua de 0 a 100)',
                  fontsize=15.0, fontweight='bold', fontfamily='sans-serif', color='#111827', labelpad=14)
    ax.set_title('Espectro Político-Partidário Brasileiro (2026) • Triangulação dos Três Pilares',
                  fontsize=20.0, fontweight='bold', fontfamily='serif', color='#06333D', pad=24)
    ax.tick_params(axis='x', labelsize=13)

    # Legenda formal completa e explícita com todas as 4 dimensões
    from matplotlib.lines import Line2D
    legend_elements = [
        Line2D([0], [0], marker='s', color='w', markerfacecolor='#990F3D', markeredgecolor='#5C0924', markersize=10, label='DeltaFolha (GPS Partidário 2026)'),
        Line2D([0], [0], marker='^', color='w', markerfacecolor='#1E3A8A', markeredgecolor='#0F172A', markersize=11, label='ABCP (Bolognesi et al. 2023)'),
        Line2D([0], [0], marker='o', color='w', markerfacecolor='#005B68', markeredgecolor='#06333D', markersize=10, label='BLS (Power & Zucco, Oxford/FGV)'),
        Line2D([0], [0], marker='o', color='w', markerfacecolor='#06333D', markeredgecolor='#06333D', markersize=13, markeredgewidth=1.8, label='Índice Sintético Consolidado'),
    ]
    legend = ax.legend(handles=legend_elements, loc='lower right', bbox_to_anchor=(0.90, 0.025),
                       ncol=1, frameon=True, facecolor='#FFFFFF', edgecolor='#CBD5E1', fontsize=13.0, borderpad=0.9)
    legend.get_frame().set_boxstyle('square', pad=0.4)
    legend.get_frame().set_linewidth(1.1)

    plt.tight_layout()
    out_path = OUT_DIR / "espectro_partidario_2026.png"
    plt.savefig(out_path, dpi=300, bbox_inches='tight', facecolor='#FFFFFF', edgecolor='none')
    plt.close()
    print(f"[OK] Gerado com Broadsheet Design System: {out_path}")


# ======================================================================
# FIGURA 3: RANKING DE CORRELAÇÕES DO BLS 9 (FONTES GRANDES)
# ======================================================================
def generate_fig3_correlacoes():
    data = [
        ('Porte e posse de armas de fogo', 0.730, 'armas', '#005B68'),
        ('Cotas com critérios raciais', -0.650, 'cotaafro', '#C84B31'),
        ('Empresas estatais na economia', -0.616, 'econlmr', '#C84B31'),
        ('Proteção social pelo Estado (govresp)', -0.608, 'govresp', '#C84B31'),
        ('Proteção ambiental vs. crescimento', -0.600, 'enviro', '#C84B31'),
        ('Valores religiosos em políticas', 0.479, 'valcrist', '#005B68'),
        ('Corte de gastos no equilíbrio fiscal', 0.468, 'fiscal', '#005B68'),
        ('Reforma da Previdência', 0.450, 'previd', '#005B68'),
        ('Privatização ampla de estatais', 0.435, 'privat', '#005B68'),
        ('Maioridade penal aos 16 anos', 0.360, 'maiorid', '#005B68'),
    ]
    
    df = pd.DataFrame(data, columns=['Tema', 'Correlacao', 'Variavel', 'Cor']).sort_values('Correlacao', ascending=True).reset_index(drop=True)

    fig, ax = plt.subplots(figsize=(15.0, 9.2), dpi=300)
    fig.patch.set_facecolor('#FFFFFF')
    ax.set_facecolor('#FFFFFF')

    y_pos = np.arange(len(df))
    bars = ax.barh(y_pos, df['Correlacao'], color=df['Cor'], height=0.62, alpha=0.92, zorder=3, edgecolor='#06333D', lw=1.0)

    ax.axvline(0, color='#06333D', linewidth=1.6, zorder=4)
    ax.grid(axis='x', color='#EFE8DF', linestyle='-', linewidth=0.9, zorder=2)

    for bar, r in zip(bars, df['Correlacao']):
        x_val = bar.get_width()
        y_val = bar.get_y() + bar.get_height()/2.0
        r_str = f"{r:+.2f}"

        if r > 0:
            ax.text(x_val + 0.025, y_val, r_str, va='center', ha='left', fontsize=15.0, fontweight='bold', fontfamily='sans-serif',
                    color='#005B68', bbox=dict(boxstyle='square,pad=0.25', facecolor='#F0F9FA', edgecolor='#005B68', lw=0.8))
        else:
            ax.text(x_val - 0.025, y_val, r_str, va='center', ha='right', fontsize=15.0, fontweight='bold', fontfamily='sans-serif',
                    color='#C84B31', bbox=dict(boxstyle='square,pad=0.25', facecolor='#FDF2E9', edgecolor='#C84B31', lw=0.8))

    ax.set_yticks(y_pos)
    labels = [f"{row['Tema']} [{row['Variavel']}]" for _, row in df.iterrows()]
    ax.set_yticklabels(labels, fontsize=15.5, fontweight='bold', fontfamily='sans-serif', color='#111827')

    ax.set_xlim(-0.85, 0.95)
    ax.set_ylim(-0.8, len(df) + 0.6)
    ax.tick_params(axis='x', labelsize=13.0)

    ax.set_xlabel('Coeficiente de Correlação de Pearson (r com Autoposicionamento Ideológico no BLS 9)',
                  fontsize=14.5, fontweight='bold', fontfamily='sans-serif', color='#111827', labelpad=14)
    ax.set_title('Poder Preditivo das Pautas Legislativas no Congresso Nacional (BLS 9)',
                 fontsize=20.0, fontweight='bold', fontfamily='serif', color='#06333D', pad=24)

    # Banners de orientação estilo jornal
    ax.text(-0.45, len(df) + 0.12, "← Alinhamento Progressista / Esquerda", fontsize=13.5, fontweight='bold', fontfamily='sans-serif',
            color='#C84B31', ha='center', va='bottom', bbox=dict(boxstyle='square,pad=0.35', facecolor='#FDF2E9', edgecolor='#C84B31', lw=0.9))
    ax.text(0.45, len(df) + 0.12, "Alinhamento Conservador / Direita →", fontsize=13.5, fontweight='bold', fontfamily='sans-serif',
            color='#005B68', ha='center', va='bottom', bbox=dict(boxstyle='square,pad=0.35', facecolor='#F0F9FA', edgecolor='#005B68', lw=0.9))

    plt.tight_layout()
    out_path = OUT_DIR / "bls_correlacoes_ranking.png"
    plt.savefig(out_path, dpi=300, bbox_inches='tight', facecolor='#FFFFFF', edgecolor='none')
    plt.close()
    print(f"[OK] Gerado com Broadsheet Design System: {out_path}")


# ======================================================================
# FIGURA 4: PAINEL DE DISPERSÃO DOS 7 TEMAS DO QUIZ (FONTES GRANDES)
# ======================================================================
def generate_fig4_dispersao():
    zip_path = 'archive/ideologia/bls_dados.zip'
    csv_name = 'BLS9_full.csv'
    
    with zipfile.ZipFile(zip_path, 'r') as z:
        with z.open(csv_name) as f:
            df = pd.read_csv(f)

    df9 = df[df['wave'] == 2021] if 'wave' in df.columns else df

    temas_info = [
        {
            "col": "armas",
            "titulo": "1. Armas de Fogo",
            "r_str": "+0.730",
            "y_label": "1=Mais Controle ... 5=Mais Acesso",
            "y_min": 0.5, "y_max": 5.5,
        },
        {
            "col": "cotaafro",
            "titulo": "2. Cotas Raciais",
            "r_str": "-0.650",
            "y_label": "1=Desfavorável ... 5=Favorável",
            "y_min": 0.5, "y_max": 5.5,
        },
        {
            "col": "econlmr",
            "titulo": "3. Empresas Estatais",
            "r_str": "-0.616",
            "y_label": "1=Estatizada ... 4=Mercado",
            "y_min": 0.4, "y_max": 4.6,
        },
        {
            "col": "govresp",
            "titulo": "4. Proteção Social / Estado",
            "r_str": "+0.608",
            "y_label": "1=Estado ... 10=Indivíduo",
            "y_min": 0.5, "y_max": 10.5,
        },
        {
            "col": "enviro",
            "titulo": "5. Meio Ambiente vs. Crescimento",
            "r_str": "+0.600",
            "y_label": "1=Ambiente ... 2=Economia",
            "y_min": 0.5, "y_max": 2.5,
        },
        {
            "col": "fiscal",
            "titulo": "6. Equilíbrio Fiscal",
            "r_str": "-0.468",
            "y_label": "1=Cortar Gastos ... 2=Tributos",
            "y_min": 0.5, "y_max": 2.5,
        },
        {
            "col": "valcrist",
            "titulo": "7. Valores Religiosos",
            "r_str": "+0.479",
            "y_label": "1=Discorda ... 5=Concorda",
            "y_min": 0.5, "y_max": 5.5,
        },
    ]

    fig, axes = plt.subplots(2, 4, figsize=(23.5, 12.5), dpi=300)
    fig.patch.set_facecolor('#FFFFFF')

    fig.suptitle('Dispersão e Linhas de Tendência: Posição Ideológica vs. Respostas dos Parlamentares (BLS 9)\nValidação Empírica Econométrica dos 7 Eixos Temáticos do Quiz Eleitoral (VAA)',
                 fontsize=19.5, fontweight='bold', fontfamily='serif', color='#06333D', y=0.98)

    np.random.seed(42)
    flat_axes = axes.flatten()

    for idx, info in enumerate(temas_info):
        ax = flat_axes[idx]
        ax.set_facecolor('#FFFFFF')
        col = info['col']
        sub = df9[['czideo', col]].dropna()
        sub = sub[(sub[col] > 0) & (sub['czideo'] > -900)]
        
        n_obs = len(sub)
        x = sub['czideo'].values
        y = sub[col].values

        y_jit = y + np.random.normal(0, 0.05, size=len(y))
        x_jit = x + np.random.normal(0, 0.015, size=len(x))

        cores = []
        for xi in x:
            if xi < -0.25:
                cores.append('#C84B31')
            elif xi <= 0.25:
                cores.append('#005B68')
            else:
                cores.append('#1E3A8A')

        ax.scatter(x_jit, y_jit, c=cores, s=80, alpha=0.82, edgecolors='#06333D', lw=0.8, zorder=3)

        # Regressão linear OLS
        slope, intercept = np.polyfit(x, y, 1)
        x_vals = np.linspace(x.min() - 0.1, x.max() + 0.1, 100)
        y_vals = slope * x_vals + intercept
        ax.plot(x_vals, y_vals, color='#06333D', linewidth=2.4, linestyle='-', zorder=4)

        ax.set_title(f"{info['titulo']}\nr = {info['r_str']} (N={n_obs})",
                     fontsize=15.0, fontweight='bold', fontfamily='serif', color='#06333D', pad=10)
        ax.set_xlabel('Posição Aldrich-McKelvey (czideo)', fontsize=11.5, fontweight='bold', fontfamily='sans-serif', color='#374151')
        ax.set_ylabel(info['y_label'], fontsize=11.0, fontweight='bold', fontfamily='sans-serif', color='#374151')
        ax.set_ylim(info['y_min'], info['y_max'])
        ax.set_xlim(-1.1, 1.1)
        ax.grid(True, linestyle='-', linewidth=0.7, color='#EFE8DF', alpha=0.9, zorder=1)
        ax.tick_params(labelsize=11)

    # 8º Subplot: Card Metodológico Síntese
    ax_card = flat_axes[7]
    ax_card.set_facecolor('#FDFBF7')
    ax_card.axis('off')
    card_text = (
        "SÍNTESE METODOLÓGICA (VAA)\n"
        "───────────────────────────────\n\n"
        "• Base Empírica: BLS 9 (2021/2023)\n"
        "  109 parlamentares federais (FGV/Oxford)\n\n"
        "• 7 Eixos Temáticos Alinhados:\n"
        "  1. Segurança & Armas\n"
        "  2. Cotas Raciais\n"
        "  3. Empresas Estatais\n"
        "  4. Proteção Social / Estado\n"
        "  5. Meio Ambiente vs. Crescimento\n"
        "  6. Equilíbrio Fiscal\n"
        "  7. Valores e Laicidade\n\n"
        "• Escala Contínua: 0 (Esquerda) a 100 (Direita)\n"
        "• Calibração: Correlações de Pearson com\n"
        "  autoposicionamento Aldrich-McKelvey"
    )
    ax_card.text(0.5, 0.5, card_text, ha='center', va='center', fontsize=12.0, fontfamily='monospace',
                 color='#06333D', bbox=dict(boxstyle='square,pad=1.0', facecolor='#F4EFE6', edgecolor='#06333D', lw=1.2))

    plt.tight_layout(rect=[0, 0.02, 1, 0.94])
    out_path = OUT_DIR / "bls_dispersao_temas.png"
    plt.savefig(out_path, dpi=300, bbox_inches='tight', facecolor='#FFFFFF', edgecolor='none')
    plt.close()
    print(f"[OK] Gerado com Broadsheet Design System: {out_path}")


if __name__ == '__main__':
    print("\n--- Gerando Figuras do Relatório (The Broadsheet Design System) ---")
    generate_fig1_fluxo()
    generate_fig2_espectro()
    generate_fig3_correlacoes()
    generate_fig4_dispersao()
    print("--- Todas as figuras foram geradas com sucesso! ---\n")
