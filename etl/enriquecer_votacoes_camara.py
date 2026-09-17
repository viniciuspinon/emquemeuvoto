#!/usr/bin/env python3
"""
etl/enriquecer_votacoes_camara.py
=================================
DESATIVADO:
Conforme diretriz do projeto, a fonte única e oficial de votações legislativas nominais
é o Radar do Congresso (Congresso em Foco), integrada via `etl/13_radar_congresso.py`.

Matérias que não foram votadas com quórum deliberativo em plenário (como propostas
em fase de coleta de assinaturas, ex: PEC 8/2025) e proposições não catalogadas
na API oficial do Radar do Congresso não são puxadas para a base de dados.
"""

def main():
    print("[Aviso] etl/enriquecer_votacoes_camara.py foi desativado.")
    print("A fonte única e oficial de votações legislativas é o Radar do Congresso (etl/13_radar_congresso.py).")

if __name__ == "__main__":
    main()
