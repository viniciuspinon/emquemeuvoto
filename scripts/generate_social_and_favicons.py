"""
Gera a arte oficial de compartilhamento (Open Graph 1200x630) do Em Quem Eu Voto.
Delega para o gerador canônico em scripts/generate_social_banner.py.
"""

import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from scripts.generate_social_banner import generate_banner

if __name__ == "__main__":
    generate_banner()

