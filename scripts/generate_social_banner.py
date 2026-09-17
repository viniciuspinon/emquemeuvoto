"""
Gera o novo banner Open Graph (1200x630) do Em Quem Eu Voto 2026.
Alinhado à nova identidade visual editorial:
- Cores: Azul Petróleo profundo (#06333D / #08404C), Terracota/Coral (#C84B31), Amarelo (#E5B800), Teal (#00AFA5) e Papel (#FAF6F0)
- Minimalista: Apenas o básico (marca, ano eleitoral, tagline oficial e domínio)
- Sem textos excessivos, sem listas de features ou botões poluídos
"""

import sys
from pathlib import Path
from PIL import Image, ImageDraw, ImageFont

if hasattr(sys.stdout, "reconfigure"):
    try:
        sys.stdout.reconfigure(encoding="utf-8")
    except Exception:
        pass

BASE_DIR = Path(__file__).resolve().parent.parent
FRONTEND_DIR = BASE_DIR / "frontend"
IMG_DIR = FRONTEND_DIR / "img"
SCRATCH_DIR = BASE_DIR / "scratch"
SCRATCH_DIR.mkdir(parents=True, exist_ok=True)

LOGO_NOBG_PATH = IMG_DIR / "logo_emquemeuvoto_nobg.png"
LOGO_FLAT_PATH = IMG_DIR / "logo_emquemeuvoto_flat.png"

def get_font(name: str, size: int):
    fonts = {
        "black": "C:/Windows/Fonts/ariblk.ttf",
        "bold": "C:/Windows/Fonts/segoeuib.ttf",
        "semibold": "C:/Windows/Fonts/seguisb.ttf",
        "regular": "C:/Windows/Fonts/segoeui.ttf",
        "serif_bold": "C:/Windows/Fonts/georgiab.ttf",
        "serif_regular": "C:/Windows/Fonts/georgia.ttf",
        "mono": "C:/Windows/Fonts/consola.ttf",
        "impact": "C:/Windows/Fonts/impact.ttf",
    }
    path = fonts.get(name, "C:/Windows/Fonts/segoeuib.ttf")
    try:
        return ImageFont.truetype(path, size)
    except Exception:
        return ImageFont.load_default()

def create_gradient_background(w, h, color1, color2):
    """Cria degradê suave e nobre entre duas cores."""
    base = Image.new("RGBA", (w, h), color1)
    top = Image.new("RGBA", (w, h), color2)
    mask = Image.new("L", (w, h))
    mask_data = []
    for y in range(h):
        for x in range(w):
            # Gradiente diagonal suave
            factor = (x / w * 0.7) + (y / h * 0.3)
            mask_data.append(int(min(255, max(0, factor * 255))))
    mask.putdata(mask_data)
    base.paste(top, (0, 0), mask)
    return base

def generate_banner():
    W, H = 1200, 630

    # 1. BASE: Petróleo nobre profundo (#042229 a #073842)
    # Reflete o background da barra lateral institucional do app (#06333D)
    color_dark = (4, 34, 41, 255)    # #042229
    color_light = (8, 56, 66, 255)   # #083842
    img = create_gradient_background(W, H, color_dark, color_light)
    draw = ImageDraw.Draw(img)

    # 2. DETALHES GEOMÉTRICOS & DE PROFUNDIDADE (Sutis, inspirados no design editorial)
    # Moldura editorial fina nas bordas (hairline elegante)
    border_color = (255, 255, 255, 36)
    draw.rectangle([24, 24, W - 25, H - 25], outline=border_color, width=1)
    
    # Detalhe de cantos editoriais (pequenos marcadores sofisticados)
    corner_len = 18
    corner_col = (0, 175, 165, 200) # Teal
    for cx, cy in [(24, 24), (W - 25, 24), (24, H - 25), (W - 25, H - 25)]:
        dx = 1 if cx == 24 else -1
        dy = 1 if cy == 24 else -1
        draw.line([cx, cy, cx + (corner_len * dx), cy], fill=corner_col, width=2)
        draw.line([cx, cy, cx, cy + (corner_len * dy)], fill=corner_col, width=2)

    # 3. KICKER / CHIP SUPERIOR: "ELEIÇÕES 2026 • DADOS OFICIAIS DO TSE"
    kicker_x, kicker_y = 80, 80
    font_kicker_bold = get_font("bold", 15)
    font_kicker_norm = get_font("semibold", 14)

    # Pill para "ELEIÇÕES 2026"
    pill_w, pill_h = 148, 36
    draw.rounded_rectangle([kicker_x, kicker_y, kicker_x + pill_w, kicker_y + pill_h], radius=6, fill=(200, 75, 49, 255))
    draw.text((kicker_x + pill_w // 2, kicker_y + pill_h // 2), "ELEIÇÕES 2026", fill=(255, 255, 255), font=font_kicker_bold, anchor="mm")

    # Separador sutil e selo do TSE
    tse_dot_x = kicker_x + pill_w + 20
    draw.text((tse_dot_x, kicker_y + pill_h // 2), "•", fill=(0, 175, 165, 240), font=font_kicker_bold, anchor="mm")
    draw.text((tse_dot_x + 18, kicker_y + pill_h // 2), "DADOS OFICIAIS DO TSE", fill=(163, 220, 228, 230), font=font_kicker_norm, anchor="lm")

    # 4. MARCA INSTITUCIONAL PRINCIPAL (Tipografia Bold Flat Canônica)
    # "EM QUEM"
    # "EU VOTO" (com realce terracota/coral em VOTO, idêntico ao app)
    font_title = get_font("black", 86)
    title_y1 = 160
    title_y2 = 258

    draw.text((80, title_y1), "EM QUEM", fill=(255, 255, 255), font=font_title)

    # Medir "EU " para posicionar o realce em "VOTO"
    eu_text = "EU "
    eu_bbox = draw.textbbox((80, title_y2), eu_text, font=font_title)
    voto_x = eu_bbox[2]
    voto_text = "VOTO"
    voto_bbox = draw.textbbox((voto_x, title_y2), voto_text, font=font_title)

    # Caixa/tarja de destaque sob "VOTO" em Terracota (#C84B31)
    pad_h = 10
    pad_w = 8
    bx0 = voto_x - pad_w
    by0 = voto_bbox[1] + 12
    bx1 = voto_bbox[2] + pad_w
    by1 = voto_bbox[3] + pad_h

    # Retângulo de realce estilo marca-texto editorial
    draw.rounded_rectangle([bx0, by0, bx1, by1], radius=6, fill=(200, 75, 49, 255))

    # Desenhar texto "EU " e "VOTO"
    draw.text((80, title_y2), eu_text, fill=(255, 255, 255), font=font_title)
    draw.text((voto_x, title_y2), voto_text, fill=(255, 255, 255), font=font_title)

    # 5. LINHA DIVISÓRIA EDITORIAL FINA
    line_y = 390
    draw.line([80, line_y, 450, line_y], fill=(0, 175, 165, 140), width=2)
    draw.ellipse([450, line_y - 3, 456, line_y + 3], fill=(229, 184, 0, 255)) # Ponto dourado/amarelo

    # 6. TAGLINE INSTITUCIONAL OFICIAL: "Por um voto bem informado."
    font_tagline = get_font("serif_bold", 30)
    tagline_y = 422
    draw.text((80, tagline_y), "Por um voto bem informado.", fill=(250, 246, 240), font=font_tagline)

    # 7. RODAPÉ MINIMALISTA: DOMÍNIO DA PLATAFORMA
    font_domain = get_font("bold", 20)
    domain_y = 520
    
    # Ícone de link / ponto indicador verde suave
    draw.ellipse([80, domain_y + 4, 92, domain_y + 16], fill=(0, 175, 165))
    draw.text((102, domain_y + 10), "emquemeuvoto.onrender.com", fill=(163, 220, 228), font=font_domain, anchor="lm")

    # 8. ÍCONE DA URNA ELETRÔNICA COM PONTO DE INTERROGAÇÃO (À Direita)
    # Usa logo oficial transparente
    logo_path = LOGO_NOBG_PATH if LOGO_NOBG_PATH.exists() else LOGO_FLAT_PATH
    if logo_path.exists():
        logo_im = Image.open(logo_path).convert("RGBA")
        
        # Redimensionar com alta qualidade
        logo_target_h = 440
        aspect = logo_im.width / logo_im.height
        logo_target_w = int(logo_target_h * aspect)
        logo_resized = logo_im.resize((logo_target_w, logo_target_h), Image.Resampling.LANCZOS)

        # Posicionamento à direita
        logo_x = 750 + (420 - logo_target_w) // 2
        logo_y = (H - logo_target_h) // 2 + 10

        # Sombra suave sob a urna
        shadow_w = int(logo_target_w * 0.75)
        shadow_h = 24
        shadow_x = logo_x + (logo_target_w - shadow_w) // 2
        shadow_y = logo_y + logo_target_h - 22
        draw.ellipse([shadow_x, shadow_y, shadow_x + shadow_w, shadow_y + shadow_h], fill=(0, 15, 20, 120))

        # Colagem da urna
        img.paste(logo_resized, (logo_x, logo_y), logo_resized)

    # Salvar nos arquivos de destino
    rgb_img = img.convert("RGB")
    
    # Caminho final em frontend/img
    out_png = IMG_DIR / "og_share_emquemeuvoto.png"
    out_jpg = IMG_DIR / "og_share_emquemeuvoto.jpg"
    rgb_img.save(out_png, "PNG", optimize=True)
    rgb_img.save(out_jpg, "JPEG", quality=96, optimize=True)

    # Copiar também para o scratch para inspeção com view_file
    scratch_preview = SCRATCH_DIR / "preview_banner.png"
    rgb_img.save(scratch_preview, "PNG")

    print(f"✓ Banner PNG gerado em: {out_png} ({out_png.stat().st_size / 1024:.1f} KB)")
    print(f"✓ Banner JPG gerado em: {out_jpg} ({out_jpg.stat().st_size / 1024:.1f} KB)")
    print(f"✓ Preview salvo em: {scratch_preview}")

if __name__ == "__main__":
    generate_banner()
