/**
 * Gerador de Santinhos Digitais & Estúdio de Minha Colinha — Em Quem Eu Voto 2026
 * Design Editorial, Tipografia Premium & Espaçamentos Profissionais
 */

/**
 * Utilitário de Compartilhamento Social Direto com Referência ao App
 */
const ShareHelper = {
  getAppUrl() {
    return (window.location.origin && window.location.origin !== "null" && !window.location.origin.includes("localhost") && !window.location.origin.includes("127.0.0.1"))
      ? window.location.origin
      : "https://emquemeuvoto.onrender.com";
  },

  downloadCanvas(canvasEl, filename = "emquemeuvoto_2026.png") {
    if (!canvasEl) return;
    try {
      const a = document.createElement("a");
      a.download = filename;
      a.href = canvasEl.toDataURL("image/png");
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);

      // Google Analytics 4 - Evento Customizado de Download de Santinho / Colinha
      if (typeof window.gtag === 'function') {
        window.gtag('event', 'download_image', {
          file_name: filename
        });
      }
    } catch (e) {
      console.warn("Download error:", e);
    }
  },

  async copyImageToClipboard(canvasEl) {
    if (!canvasEl || !navigator.clipboard || !window.ClipboardItem) return false;
    try {
      const blob = await new Promise(r => canvasEl.toBlob(r, 'image/png'));
      if (blob) {
        await navigator.clipboard.write([new ClipboardItem({ "image/png": blob })]);
        return true;
      }
    } catch (err) {
      console.warn("Clipboard write failed:", err);
    }
    return false;
  },

  async shareWhatsApp(text, url, canvasEl, filename = "santinho_2026.png") {
    const appUrl = url || this.getAppUrl();
    const fullText = `${text}\n\n👉 Acesse e confira no app: ${appUrl}`;
    const shareUrl = `https://wa.me/?text=${encodeURIComponent(fullText)}`;

    // 1. Mobile: tentar compartilhar nativamente com imagem
    const isMobile = /Android|iPhone|iPad|iPod/i.test(navigator.userAgent);
    if (isMobile && canvasEl && navigator.canShare) {
      try {
        const blob = await new Promise(r => canvasEl.toBlob(r, 'image/png'));
        if (blob) {
          const file = new File([blob], filename, { type: "image/png" });
          if (navigator.canShare({ files: [file] })) {
            await navigator.share({
              title: "Em Quem Eu Voto 2026",
              text: fullText,
              files: [file]
            });
            return true;
          }
        }
      } catch (e) {
        if (e.name === "AbortError") return false;
      }
    }

    // 2. Desktop: abrir WhatsApp imediatamente sem atrasos assíncronos que acionem o popup blocker
    window.open(shareUrl, "_blank", "noopener,noreferrer");

    // Baixar a imagem e copiar para a área de transferência
    if (canvasEl) {
      this.downloadCanvas(canvasEl, filename);
      this.copyImageToClipboard(canvasEl).then(copied => {
        if (window.App && typeof App.showToast === "function") {
          App.showToast(copied
            ? "🖼️ Imagem baixada e copiada! No WhatsApp Web, pressione Ctrl + V para colar na conversa."
            : "🖼️ Imagem salva! Você pode anexá-la na conversa do WhatsApp.");
        }
      });
    }
    return true;
  },

  async shareTwitter(text, url, canvasEl, filename = "santinho_2026.png") {
    const appUrl = url || this.getAppUrl();
    const shareUrl = `https://twitter.com/intent/tweet?text=${encodeURIComponent(text)}&url=${encodeURIComponent(appUrl)}`;

    const isMobile = /Android|iPhone|iPad|iPod/i.test(navigator.userAgent);
    if (isMobile && canvasEl && navigator.canShare) {
      try {
        const blob = await new Promise(r => canvasEl.toBlob(r, 'image/png'));
        if (blob) {
          const file = new File([blob], filename, { type: "image/png" });
          if (navigator.canShare({ files: [file] })) {
            await navigator.share({
              title: "Em Quem Eu Voto 2026",
              text: `${text}\n👉 ${appUrl}`,
              files: [file]
            });
            return true;
          }
        }
      } catch (e) {
        if (e.name === "AbortError") return false;
      }
    }

    window.open(shareUrl, "_blank", "noopener,noreferrer");

    if (canvasEl) {
      this.downloadCanvas(canvasEl, filename);
      this.copyImageToClipboard(canvasEl).then(copied => {
        if (window.App && typeof App.showToast === "function") {
          App.showToast(copied
            ? "🖼️ Imagem baixada e copiada! Na aba do X, pressione Ctrl + V para colar a foto no tweet."
            : "🖼️ Imagem baixada! Você pode anexá-la ao seu post no X.");
        }
      });
    }
  },

  async shareFacebook(url, text, canvasEl, filename = "santinho_2026.png") {
    const appUrl = url || this.getAppUrl();
    const shareUrl = `https://www.facebook.com/sharer/sharer.php?u=${encodeURIComponent(appUrl)}&quote=${encodeURIComponent(text || '')}`;

    const isMobile = /Android|iPhone|iPad|iPod/i.test(navigator.userAgent);
    if (isMobile && canvasEl && navigator.canShare) {
      try {
        const blob = await new Promise(r => canvasEl.toBlob(r, 'image/png'));
        if (blob) {
          const file = new File([blob], filename, { type: "image/png" });
          if (navigator.canShare({ files: [file] })) {
            await navigator.share({
              title: "Em Quem Eu Voto 2026",
              text: `${text}\n👉 ${appUrl}`,
              files: [file]
            });
            return true;
          }
        }
      } catch (e) {
        if (e.name === "AbortError") return false;
      }
    }

    window.open(shareUrl, "_blank", "noopener,noreferrer");

    if (canvasEl) {
      this.downloadCanvas(canvasEl, filename);
      if (window.App && typeof App.showToast === "function") {
        App.showToast("🖼️ Imagem baixada! Você pode anexá-la na sua publicação do Facebook.");
      }
    }
  },

  async shareInstagram(title, text, canvasEl, filename = "santinho_2026.png") {
    const appUrl = this.getAppUrl();
    const isMobile = /Android|iPhone|iPad|iPod/i.test(navigator.userAgent);
    if (isMobile && canvasEl && navigator.canShare) {
      try {
        const blob = await new Promise(r => canvasEl.toBlob(r, 'image/png'));
        if (blob) {
          const file = new File([blob], filename, { type: "image/png" });
          if (navigator.canShare({ files: [file] })) {
            await navigator.share({
              title: title || "Em Quem Eu Voto 2026",
              text: `${text}\n👉 Acesse: ${appUrl}`,
              files: [file]
            });
            return true;
          }
        }
      } catch (e) {
        if (e.name === "AbortError") return false;
      }
    }
    if (canvasEl) {
      this.downloadCanvas(canvasEl, filename);
    }
    await this.copyToClipboard(`${text}\n\n👉 Acesse: ${appUrl}`);
    if (window.App && typeof App.showToast === "function") {
      App.showToast("📸 Imagem baixada e texto copiado! Abra o Instagram para publicar nos Stories ou Feed.");
    }
  },

  async shareNative(title, text, canvasEl) {
    const appUrl = this.getAppUrl();
    const shareData = {
      title: title || "Em Quem Eu Voto 2026",
      text: `${text}\n\n👉 Veja no app: ${appUrl}`,
      url: appUrl
    };

    if (canvasEl && navigator.canShare) {
      try {
        const blob = await new Promise(r => canvasEl.toBlob(r, 'image/png'));
        if (blob) {
          const file = new File([blob], "emquemeuvoto_2026.png", { type: "image/png" });
          if (navigator.canShare({ files: [file] })) {
            shareData.files = [file];
          }
        }
      } catch (e) {
        console.warn("Não foi possível anexar arquivo ao share nativo:", e);
      }
    }

    if (navigator.share) {
      try {
        await navigator.share(shareData);
        return true;
      } catch (err) {
        if (err.name !== "AbortError") {
          console.warn("Erro no Web Share API:", err);
        }
      }
    }
    // Fallback: cópia para a área de transferência
    this.copyToClipboard(shareData.text);
    return false;
  },

  async copyToClipboard(text) {
    try {
      await navigator.clipboard.writeText(text);
      if (window.App && typeof App.showToast === "function") {
        App.showToast("📋 Link e mensagem copiados com sucesso!");
      } else {
        alert("Link e mensagem copiados para a área de transferência!");
      }
    } catch (e) {
      prompt("Copie o texto para compartilhar:", text);
    }
  }
};

window.ShareHelper = ShareHelper;

/**
 * Função utilitária de quebra de linha automatizada no Canvas
 */
function drawWrappedText(ctx, text, x, y, maxWidth, lineHeight, maxLines = 2, align = "left") {
  ctx.save();
  ctx.textAlign = align;
  ctx.textBaseline = "top";
  const words = (text || "").trim().split(/\s+/);
  if (words.length === 0 || !words[0]) {
    ctx.restore();
    return 0;
  }

  const lines = [];
  let curLine = words[0];

  for (let i = 1; i < words.length; i++) {
    const testLine = curLine + " " + words[i];
    const metrics = ctx.measureText(testLine);
    if (metrics.width > maxWidth) {
      lines.push(curLine);
      curLine = words[i];
      if (lines.length === maxLines - 1) {
        const remaining = words.slice(i + 1).join(" ");
        if (remaining) {
          curLine += " " + remaining;
        }
        break;
      }
    } else {
      curLine = testLine;
    }
  }
  lines.push(curLine);

  // Trunca com reticências se a última linha ainda exceder
  if (lines.length > 0) {
    let last = lines[lines.length - 1];
    if (ctx.measureText(last).width > maxWidth) {
      while (ctx.measureText(last + "...").width > maxWidth && last.length > 0) {
        last = last.slice(0, -1);
      }
      lines[lines.length - 1] = last.trim() + "...";
    }
  }

  const actualLines = Math.min(lines.length, maxLines);
  for (let i = 0; i < actualLines; i++) {
    ctx.fillText(lines[i], x, y + (i * lineHeight));
  }
  ctx.restore();

  return actualLines * lineHeight;
}

/**
 * ══════════════════════════════════════════════════════════════════════════════
 * EM QUEM EU VOTO — MOTOR EDITORIAL DE SANTINHO & COLINHA (DS v2.0)
 * ══════════════════════════════════════════════════════════════════════════════
 * Modelos inspirados em publicações brasileiras de dados eleitorais e civic-tech:
 * - Ilustração vetorial da Urna Eletrônica brasileira
 * - Identidade visual com Azul Petróleo (#005B68), Terracota/Coral (#C84B31), Amarelo (#F4C400)
 * - Protagonismo ao NÚMERO NA URNA em cartão dedicado
 * - Tipografia: Editorial Serif (DM Serif Display) + Sans (Plus Jakarta Sans) + Mono (JetBrains Mono)
 * - QR Code dinâmico funcional em canvas
 * - Exportação de alta resolução para PNG e PDF
 */

/**
 * 1. Ilustração Vetorial da Urna Eletrônica Brasileira
 * Renderizada matematicamente com paths em canvas para nitidez absoluta em qualquer DPI.
 */
function drawUrnaIllustration(ctx, cx, cy, size) {
  ctx.save();

  // Sol / Círculo Amarelo Cívico de Destaque (#F4C400)
  const radius = size * 0.44;
  ctx.fillStyle = "#F4C400";
  ctx.beginPath();
  ctx.arc(cx, cy, radius, 0, Math.PI * 2);
  ctx.fill();

  // Dimensões do gabinete da urna
  const uW = size * 0.76;
  const uH = size * 0.54;
  const uX = cx - uW * 0.48;
  const uY = cy - uH * 0.38;

  // Sombra suave sob a urna
  ctx.fillStyle = "rgba(0, 0, 0, 0.16)";
  ctx.beginPath();
  ctx.ellipse(cx, uY + uH + 4, uW * 0.50, 5, 0, 0, Math.PI * 2);
  ctx.fill();

  // Gabinete da Urna (off-white / cinza claro elegante)
  ctx.fillStyle = "#EDF1F5";
  ctx.strokeStyle = "#1B2A32";
  ctx.lineWidth = 1.8;
  ctx.beginPath();
  ctx.roundRect(uX, uY, uW, uH, 5);
  ctx.fill();
  ctx.stroke();

  // Divisão vertical interna da urna (entre tela à esquerda e teclado à direita)
  const screenW = uW * 0.54;
  const kbW = uW - screenW;

  // Painel da Tela (LCD azul com moldura escura)
  const scMargin = 4.5;
  const scX = uX + scMargin;
  const scY = uY + scMargin;
  const scH = uH - (scMargin * 2);

  // Moldura da tela
  ctx.fillStyle = "#18232C";
  ctx.beginPath();
  ctx.roundRect(scX, scY, screenW - (scMargin * 2), scH, 3);
  ctx.fill();

  // Display LCD (Azul urna clássico)
  const lcdMargin = 3;
  const lcdX = scX + lcdMargin;
  const lcdY = scY + lcdMargin;
  const lcdW = screenW - (scMargin * 2) - (lcdMargin * 2);
  const lcdH = scH - (lcdMargin * 2);
  ctx.fillStyle = "#0A4E82";
  ctx.fillRect(lcdX, lcdY, lcdW, lcdH);

  // Linhas do display digital (foto e dados simulados)
  ctx.fillStyle = "rgba(255, 255, 255, 0.32)";
  ctx.fillRect(lcdX + 3, lcdY + 4, lcdW * 0.34, lcdH - 8); // silhueta foto
  ctx.fillStyle = "rgba(255, 255, 255, 0.85)";
  ctx.fillRect(lcdX + lcdW * 0.44, lcdY + 5, lcdW * 0.5, 2.5); // linha 1
  ctx.fillRect(lcdX + lcdW * 0.44, lcdY + 11, lcdW * 0.4, 2);   // linha 2
  ctx.fillRect(lcdX + lcdW * 0.44, lcdY + 16, lcdW * 0.32, 2);  // linha 3

  // Painel do Teclado (à direita)
  const kbX = uX + screenW;
  const kbY = uY + scMargin;
  const kbH = scH;
  const kbContentW = kbW - scMargin;

  ctx.fillStyle = "#25323C";
  ctx.beginPath();
  ctx.roundRect(kbX, kbY, kbContentW, kbH, 3);
  ctx.fill();

  // Teclado Numérico (grade 3x3)
  const keyCols = 3;
  const keyRows = 3;
  const keyPadMargin = 4;
  const keyAreaW = kbContentW - (keyPadMargin * 2);
  const keyW = (keyAreaW - 4) / 3;
  const keyH = (kbH * 0.48 - 4) / 3;

  ctx.fillStyle = "#ECEFF1";
  for (let r = 0; r < keyRows; r++) {
    for (let c = 0; c < keyCols; c++) {
      ctx.fillRect(kbX + keyPadMargin + c * (keyW + 2), kbY + keyPadMargin + r * (keyH + 2), keyW, keyH);
    }
  }

  // Teclas de Ação (Branco, Corrige, Confirma)
  const actionY = kbY + kbH * 0.65;
  const actionH = kbH * 0.28;
  const actionKeyW = (keyAreaW - 4) / 3;

  // 1. Branco
  ctx.fillStyle = "#FFFFFF";
  ctx.fillRect(kbX + keyPadMargin, actionY, actionKeyW, actionH);

  // 2. Corrige (Laranja)
  ctx.fillStyle = "#E65100";
  ctx.fillRect(kbX + keyPadMargin + actionKeyW + 2, actionY, actionKeyW, actionH);

  // 3. Confirma (Verde, ligeiramente mais alto)
  ctx.fillStyle = "#2E7D32";
  ctx.fillRect(kbX + keyPadMargin + (actionKeyW + 2) * 2, actionY - 2, actionKeyW, actionH + 2);

  ctx.restore();
}

/**
 * 2. Paleta Cromática Rigorosa do Design System v2.0
 */
function getThemeColors(themeKey, partyColor = "#005B68") {
  if (themeKey === "slate") {
    // Dark Mode / Noturno Chumbo
    return {
      name: "slate",
      isDark: true,
      bgPage: "#121417",
      bgCard: "#1A1E24",
      bgCardAlt: "#222831",
      borderCard: "#30363F",
      borderRule: "#454E5B",
      textPrimary: "#F5E5D5",
      textSecondary: "#A7A19B",
      textMuted: "#736C65",
      kickerColor: "#EF5B4D",
      accentColor: "#00AFA5",
      yellowAccent: "#F4C400"
    };
  }
  if (themeKey === "party" || themeKey === "petroleum") {
    // Azul Petróleo Institucional
    return {
      name: "party",
      isDark: true,
      bgPage: "#06333D",
      bgCard: "#0A4552",
      bgCardAlt: "#0E5463",
      borderCard: "#156373",
      borderRule: "#1F7A8C",
      textPrimary: "#FFFFFF",
      textSecondary: "#D2EEF2",
      textMuted: "#89C8D4",
      kickerColor: "#EF5B4D",
      accentColor: "#00AFA5",
      yellowAccent: "#F4C400"
    };
  }
  if (themeKey === "claret" || themeKey === "terracota") {
    // Terracota / Borgonha Nobre
    return {
      name: "claret",
      isDark: true,
      bgPage: "#2D0A08",
      bgCard: "#3E120F",
      bgCardAlt: "#521A16",
      borderCard: "#6E241E",
      borderRule: "#8C3229",
      textPrimary: "#FFF1E5",
      textSecondary: "#E8C2BD",
      textMuted: "#C08E88",
      kickerColor: "#FF8A7A",
      accentColor: "#FF6F59",
      yellowAccent: "#F4C400"
    };
  }
  if (themeKey === "light") {
    // Minimalista Branco
    return {
      name: "light",
      isDark: false,
      bgPage: "#FFFFFF",
      bgCard: "#F8F9FA",
      bgCardAlt: "#EEF0F2",
      borderCard: "#DDE1E5",
      borderRule: "#C5CCD3",
      textPrimary: "#111418",
      textSecondary: "#38312B",
      textMuted: "#6B625B",
      kickerColor: "#C84B31",
      accentColor: "#005B68",
      yellowAccent: "#F4C400"
    };
  }
  // "paper" — Marfim Editorial Cívico (Padrão Oficial do Design Kit)
  return {
    name: "paper",
    isDark: false,
    bgPage: "#FAF6F0",
    bgCard: "#FFFFFF",
    bgCardAlt: "#F4EEE5",
    borderCard: "#D8CCC0",
    borderRule: "#C5B8AA",
    textPrimary: "#111418",
    textSecondary: "#38312B",
    textMuted: "#6B625B",
    kickerColor: "#C84B31", // Terracota / Coral
    accentColor: "#005B68", // Petróleo
    yellowAccent: "#F4C400" // Amarelo cívico
  };
}

/**
 * Desenha os dígitos de um número de urna dentro de quadradinhos estilizados
 */
function drawUrnaDigitBoxes(ctx, numStr, startX, startY, totalW, boxH, colors, maxBoxW = 120, minBoxW = 28, boxGap = null) {
  const digits = String(numStr || "---").trim().split("");
  const numDigits = digits.length;
  if (numDigits === 0) return 0;

  const gap = boxGap !== null ? boxGap : Math.max(4, Math.min(14, Math.floor(totalW * 0.03)));
  const availWForBoxes = totalW - ((numDigits - 1) * gap);
  const boxW = Math.max(minBoxW, Math.min(maxBoxW, Math.floor(availWForBoxes / numDigits)));
  const actualTotalW = (numDigits * boxW) + ((numDigits - 1) * gap);
  const actualStartX = startX + (totalW - actualTotalW) / 2;

  for (let i = 0; i < numDigits; i++) {
    const bx = actualStartX + i * (boxW + gap);

    // Superfície do quadradinho (com contraste)
    ctx.fillStyle = colors.bgCard || "#FFFFFF";
    ctx.beginPath();
    ctx.roundRect(bx, startY, boxW, boxH, Math.min(8, Math.round(boxW * 0.15)));
    ctx.fill();

    ctx.strokeStyle = colors.borderCard || "#D8CCC0";
    ctx.lineWidth = Math.max(1.4, Math.min(2.4, boxH * 0.028));
    ctx.stroke();

    // Dígito numérico centralizado
    ctx.fillStyle = colors.textPrimary || "#111418";
    const fontSize = Math.round(boxH * 0.68);
    ctx.font = `bold ${fontSize}px 'JetBrains Mono', monospace`;
    ctx.textAlign = "center";
    ctx.textBaseline = "middle";
    ctx.fillText(digits[i], bx + boxW / 2, startY + boxH / 2);
  }
  return actualTotalW;
}
window.drawUrnaDigitBoxes = drawUrnaDigitBoxes;

let _cachedSiteLogo = null;
function getSiteLogoImage() {
  if (!_cachedSiteLogo) {
    _cachedSiteLogo = new Image();
    _cachedSiteLogo.crossOrigin = "anonymous";
    _cachedSiteLogo.onload = () => {
      // Se algum estúdio estiver aberto, atualiza para exibir a logo oficial
      const stModal = document.getElementById("santinhoModal");
      if (stModal && stModal.classList.contains("open") && window.SantinhoStudio) {
        window.SantinhoStudio.update();
      }
      const svModal = document.getElementById("savedModal");
      if (svModal && svModal.classList.contains("open") && window.ColinhaStudio) {
        window.ColinhaStudio.update();
      }
      if (window.CompareController && typeof CompareController.generateCanvas === "function") {
        CompareController.generateCanvas();
      }
    };
    _cachedSiteLogo.onerror = () => {
      if (_cachedSiteLogo.src.indexOf("/img/") === -1) {
        _cachedSiteLogo.src = "/img/logo_emquemeuvoto_nobg.png";
      }
    };
    _cachedSiteLogo.src = "/img/logo_emquemeuvoto_nobg.png";
  }
  return _cachedSiteLogo;
}
if (typeof window !== "undefined") {
  setTimeout(() => { getSiteLogoImage(); }, 20);
}

/**
 * 4. Cabeçalho Editorial com Lockup da Marca e Logo Oficial do Site
 */
function drawEditorialHeader(ctx, width, title, subtitle, themeColors) {
  const isDark = themeColors.isDark;
  const padX = width > 1000 ? 70 : 45;
  const topY = width > 1000 ? 50 : 38;

  // 1. Logo "EM QUEM" + badge "EU VOTO"
  ctx.save();
  ctx.textAlign = "left";
  ctx.textBaseline = "top";

  // Linha 1: "EM QUEM"
  ctx.fillStyle = isDark ? "#FFFFFF" : "#0A2530";
  ctx.font = "900 30px 'Plus Jakarta Sans', sans-serif";
  ctx.letterSpacing = "0.02em";
  ctx.fillText("EM QUEM", padX, topY);

  // Linha 2: Badge "EU VOTO"
  const badgeY = topY + 36;
  const badgeW = 144;
  const badgeH = 34;
  ctx.fillStyle = themeColors.kickerColor || "#C84B31";
  ctx.beginPath();
  ctx.roundRect ? ctx.roundRect(padX, badgeY, badgeW, badgeH, 4) : ctx.rect(padX, badgeY, badgeW, badgeH);
  ctx.fill();

  ctx.fillStyle = "#FFFFFF";
  ctx.font = "900 22px 'Plus Jakarta Sans', sans-serif";
  ctx.textAlign = "center";
  ctx.fillText("EU VOTO", padX + badgeW / 2, badgeY + 5);
  ctx.restore();

  // 2. Linha divisória vertical entre o logo e o título
  const dividerX = padX + badgeW + 28;
  ctx.strokeStyle = themeColors.borderCard || "#D8CCC0";
  ctx.lineWidth = 1.4;
  ctx.beginPath();
  ctx.moveTo(dividerX, topY + 2);
  ctx.lineTo(dividerX, topY + 68);
  ctx.stroke();

  // 3. Título e Subtítulo descritivo com fontes ampliadas
  ctx.save();
  ctx.textAlign = "left";
  ctx.textBaseline = "top";

  ctx.fillStyle = isDark ? "#F5E5D5" : "#111418";
  ctx.font = "bold 24px 'Plus Jakarta Sans', sans-serif";
  ctx.fillText((title || "ELEIÇÕES GERAIS 2026").toUpperCase(), dividerX + 24, topY + 6);

  ctx.fillStyle = themeColors.textMuted || "#6B625B";
  ctx.font = "bold 15px 'Plus Jakarta Sans', sans-serif";
  ctx.fillText((subtitle || "POR UM VOTO BEM INFORMADO.").toUpperCase(), dividerX + 24, topY + 40);
  ctx.restore();

  // 4. Logo Oficial do Site (sem fundo ou disco solar)
  const logoImg = getSiteLogoImage();
  const logoH = width > 1000 ? 76 : 64;
  const logoW = Math.round(logoH * (1107 / 1221));
  const logoX = width - padX - logoW;
  const logoY = topY - 2;

  if (logoImg && logoImg.complete && logoImg.naturalWidth > 0) {
    ctx.drawImage(logoImg, logoX, logoY, logoW, logoH);
  } else if (typeof drawUrnaIllustration === "function") {
    drawUrnaIllustration(ctx, width - padX - 32, topY + 34, 70);
  }

  const headerRuleY = topY + (width > 1000 ? 84 : 76);
  ctx.strokeStyle = themeColors.borderCard || "#D8CCC0";
  ctx.lineWidth = 1.2;
  ctx.beginPath();
  ctx.moveTo(padX, headerRuleY);
  ctx.lineTo(width - padX, headerRuleY);
  ctx.stroke();

  return headerRuleY;
}

/**
 * 5. Rodapé Editorial com Domínio Oficial (Sem poluição visual de QR Code)
 */
function drawEditorialFooter(ctx, width, height, themeColors, isColinha = false, customUrl = null) {
  const isWide = width > 1000;
  const padX = isWide ? 64 : 45;
  const footY = height - (isWide ? 115 : 90);

  // Linha de régua superior do rodapé
  ctx.strokeStyle = themeColors.borderCard || "#D8CCC0";
  ctx.lineWidth = 1.5;
  ctx.beginPath();
  ctx.moveTo(padX, footY);
  ctx.lineTo(width - padX, footY);
  ctx.stroke();

  const appDomain = "emquemeuvoto.onrender.com";

  if (isColinha) {
    // Rodapé da Colinha: "Por um voto bem informado."
    ctx.save();
    ctx.textAlign = "left";
    ctx.textBaseline = "middle";
    ctx.fillStyle = themeColors.textPrimary || "#111418";
    ctx.font = isWide ? "italic bold 25px 'DM Serif Display', Georgia, serif" : "italic bold 19px 'DM Serif Display', Georgia, serif";
    ctx.fillText("Por um voto bem informado.", padX, footY + (isWide ? 54 : 44));
    ctx.restore();

    // Marca e Domínio
    ctx.save();
    ctx.textAlign = "right";
    ctx.textBaseline = "middle";
    ctx.fillStyle = themeColors.textPrimary || "#111418";
    ctx.font = "900 19px 'Plus Jakarta Sans', sans-serif";
    ctx.fillText("EM QUEM EU VOTO", width - padX, footY + (isWide ? 42 : 34));
    ctx.fillStyle = themeColors.textMuted || "#6B625B";
    ctx.font = "bold 13px 'Plus Jakarta Sans', sans-serif";
    ctx.fillText(appDomain, width - padX, footY + (isWide ? 66 : 56));
    ctx.restore();

  } else {
    // Rodapé do Santinho:
    // Lado Esquerdo: DADOS • PESSOAS • DEMOCRACIA
    ctx.save();
    ctx.textAlign = "left";
    ctx.textBaseline = "middle";
    ctx.fillStyle = themeColors.textMuted || "#6B625B";
    const leftFontSize = isWide ? 12 : 11;
    ctx.font = `bold ${leftFontSize}px 'Plus Jakarta Sans', sans-serif`;
    ctx.fillText("D A D O S   •   P E S S O A S   •   D E M O C R A C I A", padX, footY + (isWide ? 54 : 44));
    ctx.restore();

    // Lado Direito: EM QUEM EU VOTO / Domínio
    ctx.save();
    ctx.textAlign = "right";
    ctx.textBaseline = "middle";
    ctx.fillStyle = themeColors.textPrimary || "#111418";
    ctx.font = "900 19px 'Plus Jakarta Sans', sans-serif";
    ctx.fillText("EM QUEM EU VOTO", width - padX, footY + (isWide ? 42 : 34));
    ctx.fillStyle = themeColors.textMuted || "#6B625B";
    ctx.font = "bold 13px 'Plus Jakarta Sans', sans-serif";
    ctx.fillText(appDomain, width - padX, footY + (isWide ? 66 : 56));
    ctx.restore();
  }
}

/**
 * ══════════════════════════════════════════════════════════════════════════════
 * MODELO 1: SANTINHO DIGITAL INDIVIDUAL
 * ══════════════════════════════════════════════════════════════════════════════
 */
const SantinhoStudio = {
  currentCand: null,
  candidate: null,
  theme: "paper",
  font: "serif",
  format: "9:16",
  _returnToModal: null,

  init() {
    const themeBtns = document.querySelectorAll("[data-st-theme]");
    themeBtns.forEach(btn => {
      btn.addEventListener("click", () => {
        this.setTheme(btn.getAttribute("data-st-theme"));
      });
    });

    const fontBtns = document.querySelectorAll("[data-st-font]");
    fontBtns.forEach(btn => {
      btn.addEventListener("click", () => {
        this.setFont(btn.getAttribute("data-st-font"));
      });
    });

    const fmtBtns = document.querySelectorAll("[data-st-fmt]");
    fmtBtns.forEach(btn => {
      btn.addEventListener("click", () => {
        this.setFormat(btn.getAttribute("data-st-fmt"));
      });
    });

    const phraseInput = document.getElementById("stCustomPhrase");
    if (phraseInput) {
      phraseInput.addEventListener("input", () => this.update());
    }

    const checkboxes = [
      "stOpt_ideologia",
      "stOpt_mandato",
      "stOpt_profissao",
      "stOpt_bens",
      "stOpt_financas",
      "stOpt_frase"
    ];
    checkboxes.forEach(id => {
      const el = document.getElementById(id);
      if (el) el.addEventListener("change", () => this.update());
    });
  },

  open(c) {
    if (window.App && typeof App.closeMobileSidebar === 'function') {
      App.closeMobileSidebar();
    }
    if (c) {
      this.currentCand = c;
      this.candidate = c;
    }
    const modal = document.getElementById("santinhoModal");
    if (modal) {
      modal.classList.add("open");
      modal.style.display = "flex";
    }
    const btnBack = document.getElementById("santinhoBtnBackToSaved");
    if (btnBack) {
      btnBack.style.display = (this._returnToModal === 'savedListModal') ? 'inline-flex' : 'none';
    }
    this.update();
  },

  close() {
    const modal = document.getElementById("santinhoModal");
    if (modal) {
      modal.classList.remove("open");
      modal.style.display = "none";
    }
    if (this._returnToModal === 'savedListModal' && window.ListManager) {
      this._returnToModal = null;
      ListManager.openSavedModal();
    }
  },

  setCandidate(c) {
    this.currentCand = c;
    this.candidate = c;
    this.update();
  },

  setFont(fontKey) {
    this.font = fontKey;
    ["serif", "moderno", "mono"].forEach(f => {
      const el = document.getElementById(`stFont_${f}`);
      if (el) el.classList.toggle("active", f === fontKey);
    });
    this.update();
  },

  getFontFamily(type = "sans") {
    if (this.font === "serif") {
      return type === "display" ? "'DM Serif Display', 'Merriweather', Georgia, serif" : "'Plus Jakarta Sans', sans-serif";
    }
    if (this.font === "mono") {
      return type === "display" ? "'JetBrains Mono', monospace" : "'Plus Jakarta Sans', sans-serif";
    }
    return "'Plus Jakarta Sans', sans-serif";
  },

  setFormat(fmt) {
    this.format = fmt;
    ["sq", "vt", "hz"].forEach(id => {
      const el = document.getElementById(`stFmt_${id}`);
      if (el) el.classList.remove("active");
    });
    if (fmt === "1:1") document.getElementById("stFmt_sq")?.classList.add("active");
    if (fmt === "9:16") document.getElementById("stFmt_vt")?.classList.add("active");
    if (fmt === "16:9") document.getElementById("stFmt_hz")?.classList.add("active");
    this.update();
  },

  setTheme(theme) {
    this.theme = theme;
    ["paper", "slate", "party", "claret", "light"].forEach(t => {
      const el = document.getElementById(`stTheme_${t}`);
      if (el) el.classList.toggle("active", t === theme);
    });
    this.update();
  },

  async update() {
    const canvas = document.getElementById("santinhoCanvas");
    if (!canvas) return;
    const ctx = canvas.getContext("2d");
    if (!ctx) return;

    let c = this.currentCand || this.candidate;
    if (!c && window.ListManager) {
      if (typeof ListManager.getColinhaCandidates === "function") {
        const list = ListManager.getColinhaCandidates();
        if (list && list.length > 0) c = list[0];
      }
      if (!c && typeof ListManager.getActiveCandidates === "function") {
        const list = ListManager.getActiveCandidates();
        if (list && list.length > 0) c = list[0];
      }
    }
    if (!c) {
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      return;
    }
    this.currentCand = c;
    this.candidate = c;

    // Leitura dos Checkboxes de Campos Opcionais
    const showIdeologia = document.getElementById("stOpt_ideologia") ? document.getElementById("stOpt_ideologia").checked : true;
    const showMandato = document.getElementById("stOpt_mandato") ? document.getElementById("stOpt_mandato").checked : true;
    const showOcupacao = document.getElementById("stOpt_profissao") ? document.getElementById("stOpt_profissao").checked : true;
    const showBens = document.getElementById("stOpt_bens") ? document.getElementById("stOpt_bens").checked : true;
    const showFinancas = document.getElementById("stOpt_financas") ? document.getElementById("stOpt_financas").checked : true;
    const showSlogan = document.getElementById("stOpt_frase") ? document.getElementById("stOpt_frase").checked : true;

    const fmt = this.format || "9:16";
    let width = 760;
    let height = 1180;

    if (fmt === "1:1") {
      width = 1080;
      height = 1080;
    } else if (fmt === "16:9") {
      width = 1280;
      height = 720;
    }

    canvas.width = width;
    canvas.height = height;
    canvas.style.maxWidth = "100%";
    canvas.style.height = "auto";
    canvas.style.display = "block";

    const partyColor = c.partido_cor_hex || (typeof getPartyColor === "function" ? getPartyColor(c.partido || c.sigla_partido) : "#005B68");
    const colors = typeof getThemeColors === "function" ? getThemeColors(this.theme || "paper", partyColor) : {
      bgPage: "#FAF6F0",
      bgCard: "#FFFFFF",
      bgCardAlt: "#F4EEE5",
      borderCard: "#D8CCC0",
      textPrimary: "#111418",
      textSecondary: "#38312B",
      textMuted: "#6B625B",
      kickerColor: "#C84B31",
      isDark: false
    };

    const fontDisplay = this.getFontFamily("display");
    const sansFont = this.getFontFamily("sans");

    // 1. Fundo Editorial e Borda
    ctx.fillStyle = colors.bgPage || "#FAF6F0";
    ctx.fillRect(0, 0, width, height);

    ctx.strokeStyle = colors.borderCard || "#D8CCC0";
    ctx.lineWidth = 1.5;
    ctx.strokeRect(18, 18, width - 36, height - 36);

    const padX = fmt === "16:9" ? 52 : 44;

    // 2. Cabeçalho Editorial
    const headerTopY = 36;
    ctx.save();
    ctx.textAlign = "left";
    ctx.textBaseline = "top";
    ctx.fillStyle = colors.isDark ? "#FFFFFF" : "#0A2530";
    ctx.font = "900 20px 'Plus Jakarta Sans', sans-serif";
    ctx.fillText("EM QUEM", padX, headerTopY);

    const badgeY = headerTopY + 25;
    const badgeW = 96;
    const badgeH = 24;
    ctx.fillStyle = colors.kickerColor || "#C84B31";
    ctx.beginPath();
    ctx.roundRect ? ctx.roundRect(padX, badgeY, badgeW, badgeH, 3) : ctx.rect(padX, badgeY, badgeW, badgeH);
    ctx.fill();

    ctx.fillStyle = "#FFFFFF";
    ctx.font = "900 14px 'Plus Jakarta Sans', sans-serif";
    ctx.textAlign = "center";
    ctx.fillText("EU VOTO", padX + badgeW / 2, badgeY + 4);
    ctx.restore();

    const dividerX = padX + badgeW + 18;
    ctx.strokeStyle = colors.borderCard || "#D8CCC0";
    ctx.lineWidth = 1.2;
    ctx.beginPath();
    ctx.moveTo(dividerX, headerTopY + 2);
    ctx.lineTo(dividerX, headerTopY + 50);
    ctx.stroke();

    ctx.save();
    ctx.textAlign = "left";
    ctx.textBaseline = "top";
    ctx.fillStyle = colors.textPrimary || "#111418";
    ctx.font = "800 16px 'Plus Jakarta Sans', sans-serif";
    ctx.letterSpacing = "0.04em";
    ctx.fillText("ELEIÇÕES GERAIS 2026", dividerX + 18, headerTopY + 3);

    ctx.fillStyle = colors.textMuted || "#6B625B";
    ctx.font = "bold 12px 'Plus Jakarta Sans', sans-serif";
    ctx.letterSpacing = "0.06em";
    ctx.fillText("POR UM VOTO BEM INFORMADO.", dividerX + 18, headerTopY + 27);
    ctx.restore();

    const logoImg = typeof getSiteLogoImage === "function" ? getSiteLogoImage() : null;
    const logoH = 54;
    const logoW = Math.round(logoH * (1107 / 1221));
    const logoX = width - padX - logoW;
    const logoY = headerTopY - 2;

    if (logoImg && logoImg.complete && logoImg.naturalWidth > 0) {
      ctx.drawImage(logoImg, logoX, logoY, logoW, logoH);
    } else if (typeof drawUrnaIllustration === "function") {
      drawUrnaIllustration(ctx, width - padX - 32, headerTopY + 24, 60);
    }

    const headerRuleY = headerTopY + 64;
    ctx.strokeStyle = colors.borderCard || "#D8CCC0";
    ctx.lineWidth = 1.2;
    ctx.beginPath();
    ctx.moveTo(padX, headerRuleY);
    ctx.lineTo(width - padX, headerRuleY);
    ctx.stroke();

    // Title Case Helper
    const toTitleCase = (str) => {
      if (!str) return "";
      const lowers = ["de", "da", "do", "das", "dos", "e", "em"];
      return str.toLowerCase().split(" ").map((w, idx) => {
        if (idx > 0 && lowers.includes(w)) return w;
        return w.charAt(0).toUpperCase() + w.slice(1);
      }).join(" ");
    };

    const candName = toTitleCase(c.nome_urna || c.nome || "Candidato");
    const numeroStr = String(c.numero || "0000").trim();

    // 3. Foto do Candidato (sem distorção, tamanho reduzido e bordas arredondadas)
    let photoImg = null;
    const photoSrc = c.foto_url || c.foto || c.imagem;
    if (photoSrc || c.sq_candidato) {
      try {
        photoImg = await new Promise((resolve) => {
          const i = new Image();
          i.crossOrigin = "anonymous";
          i.src = photoSrc || `/api/candidatos/${c.sq_candidato}/foto`;
          i.onload = () => resolve(i);
          i.onerror = () => resolve(null);
        });
      } catch (e) {
        photoImg = null;
      }
    }

    const drawCoverPhoto = (img, x, y, w, h, radius = 12) => {
      ctx.save();
      ctx.beginPath();
      if (typeof ctx.roundRect === "function") {
        ctx.roundRect(x, y, w, h, radius);
      } else {
        ctx.rect(x, y, w, h);
      }
      ctx.clip();

      if (img && img.complete && img.naturalWidth > 0) {
        const imgW = img.naturalWidth || img.width;
        const imgH = img.naturalHeight || img.height;
        const srcAspect = imgW / imgH;
        const dstAspect = w / h;
        let sx = 0, sy = 0, sw = imgW, sh = imgH;

        if (srcAspect > dstAspect) {
          sw = Math.round(imgH * dstAspect);
          sh = imgH;
          sx = Math.round((imgW - sw) / 2);
          sy = 0;
        } else {
          sw = imgW;
          sh = Math.round(imgW / dstAspect);
          sx = 0;
          sy = Math.round(Math.max(0, Math.min(imgH - sh, (imgH - sh) * 0.16)));
        }
        ctx.drawImage(img, sx, sy, sw, sh, x, y, w, h);
      } else {
        ctx.fillStyle = colors.bgCardAlt || "#F4EEE5";
        ctx.fillRect(x, y, w, h);
        ctx.fillStyle = colors.textPrimary || "#111418";
        ctx.font = `bold 48px ${fontDisplay}`;
        ctx.textAlign = "center";
        ctx.textBaseline = "middle";
        ctx.fillText(candName.charAt(0) || "?", x + w / 2, y + h / 2);
      }
      ctx.restore();

      ctx.strokeStyle = colors.borderCard || "#D8CCC0";
      ctx.lineWidth = 1.4;
      ctx.beginPath();
      ctx.roundRect ? ctx.roundRect(x, y, w, h, radius) : ctx.rect(x, y, w, h);
      ctx.stroke();
    };

    // 4. Hero Section (Foto e Textos Ampliados)
    const heroTopY = headerRuleY + 16;

    // Foto ampliada (de ~160x200 para 210x260)
    const photoW = fmt === "16:9" ? 230 : (fmt === "1:1" ? 240 : 210);
    const photoH = fmt === "16:9" ? 280 : (fmt === "1:1" ? 290 : 260);
    const heroH = photoH;

    drawCoverPhoto(photoImg, padX, heroTopY, photoW, photoH, 14);

    const textStartX = padX + photoW + 26;
    const maxTextW = width - textStartX - padX;
    let ty = heroTopY + 4;

    // 1. Cargo • UF (Aumentado de 12px para 15px)
    ctx.fillStyle = colors.kickerColor || "#C84B31";
    ctx.font = `bold ${fmt === "1:1" ? 17 : 15}px ${sansFont}`;
    ctx.textAlign = "left";
    ctx.textBaseline = "top";
    ctx.fillText(`${(c.cargo || "CANDIDATO").toUpperCase()} • ${c.uf || "BR"}`, textStartX, ty);
    ty += fmt === "1:1" ? 26 : 24;

    // 2. Nome do Candidato (Aumentado de 28px para 38px)
    ctx.fillStyle = colors.textPrimary || "#111418";
    ctx.font = `800 ${fmt === "1:1" ? 42 : 38}px ${fontDisplay}`;
    const nameLineH = fmt === "1:1" ? 44 : 40;
    const nameLinesConsumed = drawWrappedText(ctx, candName, textStartX, ty, maxTextW, nameLineH, 2);
    ty += nameLinesConsumed + (fmt === "1:1" ? 10 : 8);

    // 3. Partido (Aumentado de 14px para 19px)
    ctx.fillStyle = colors.textSecondary || "#38312B";
    ctx.font = `bold ${fmt === "1:1" ? 21 : 19}px ${sansFont}`;
    ctx.fillText(`${c.partido || c.sigla_partido || "—"}${c.coligacao ? ` • ${c.coligacao}` : ""}`, textStartX, ty);
    ty += fmt === "1:1" ? 28 : 24;

    // 4. Label "NÚMERO NA URNA" (Aumentado de 11px para 13px)
    ctx.fillStyle = colors.textMuted || "#6B625B";
    ctx.font = `bold 13px ${sansFont}`;
    ctx.fillText("NÚMERO NA URNA", textStartX, ty);
    ty += 18;

    // 5. Caixinhas do Número na Urna (Proporcionalmente maiores)
    const digits = numeroStr.split("");
    const boxW = fmt === "1:1" ? 50 : 44;
    const boxH = fmt === "1:1" ? 60 : 54;
    const totalNumW = (digits.length * boxW) + ((digits.length - 1) * 6);
    drawUrnaDigitBoxes(ctx, numeroStr, textStartX, ty, totalNumW, boxH, colors, boxW, 30, 6);

    // 5. Módulos de Dados Reais Úteis (Ordenados por relevância cívica)
    const dataModules = [];

    // 1. Ideologia e Posicionamento
    if (showIdeologia && (c.ideologia_nome || c.ideologia || c.espectro || c.posicionamento)) {
      dataModules.push({
        type: "user",
        icon: "⚖",
        title: "Ideologia do Partido",
        main: toTitleCase(c.ideologia_nome || c.ideologia || c.espectro || "Centro"),
        sub: toTitleCase(c.posicionamento || "Orientação política oficial declarada")
      });
    }

    // 2. Mandato e Exercício Público
    if (showMandato) {
      const mandInfo = (window.App && App.getCandidateMandatoInfo) ? App.getCandidateMandatoInfo(c) : null;
      let mandTitle = "Mandato Atual";
      let mandMain = "Sem Mandato Atual";
      let mandSub = "Não consta exercício de cargo eletivo vigente";

      if (mandInfo && mandInfo.isReeleicao) {
        mandMain = toTitleCase(mandInfo.label.replace(/^⚡\s*/, ''));
        mandSub = "Tentando reeleição para o mesmo cargo";
      } else if (c.em_exercicio && c.cargo_exercicio) {
        mandMain = toTitleCase(c.cargo_exercicio);
        mandSub = `Em exercício na legislatura atual (${c.uf || 'BR'})`;
      }

      dataModules.push({
        type: "briefcase",
        icon: "🏛",
        title: mandTitle,
        main: mandMain,
        sub: mandSub
      });
    }

    // 3. Profissão e Escolaridade
    if (showOcupacao && (c.ocupacao || c.profissao || c.grau_instrucao)) {
      dataModules.push({
        type: "briefcase",
        icon: "💼",
        title: "Profissão / Ocupação",
        main: toTitleCase(c.ocupacao || c.profissao || "Profissão Declarada"),
        sub: c.grau_instrucao ? `Instrução: ${toTitleCase(c.grau_instrucao)}` : "Declarado junto ao TSE"
      });
    }

    // 4. Financiamento de Campanha (com ranking no partido quando disponível)
    if (showFinancas && (c.total_receita !== undefined || c.receita_total !== undefined || c.financiamento_receita !== undefined || c.arrecadacao !== undefined)) {
      const valorRec = Number(c.total_receita || c.receita_total || c.financiamento_receita || c.arrecadacao || 0);
      let financSub = "Arrecadação declarada na prestação de contas";
      if (c.ranking_partido_receita && c.total_partido_cargo_uf) {
        financSub = `${c.ranking_partido_receita}º mais financiado do ${c.partido} (de ${c.total_partido_cargo_uf})`;
      } else if (c.ranking_receita_cargo) {
        financSub = `${c.ranking_receita_cargo}º mais financiado para ${c.cargo || 'o cargo'}`;
      }

      dataModules.push({
        type: "money",
        icon: "💰",
        title: "Financiamento de Campanha",
        main: valorRec.toLocaleString("pt-BR", { style: "currency", currency: "BRL" }),
        sub: financSub
      });
    }

    // 5. Bens Declarados
    if (showBens && (c.total_bens !== undefined || c.bens_total !== undefined)) {
      const valorBens = Number(c.total_bens || c.bens_total || 0);
      dataModules.push({
        type: "chart",
        icon: "📊",
        title: "Patrimônio Declarado",
        main: valorBens.toLocaleString("pt-BR", { style: "currency", currency: "BRL" }),
        sub: "Total de bens registrados na Justiça Eleitoral"
      });
    }

    // Grid Adaptativo por Formato:
    // 9:16 (Vertical/Stories) -> 1 coluna
    // 16:9 (Horizontal/X)     -> 3 colunas (apenas 2 linhas, sem sobreposição)
    // 1:1 (Quadrado/Feed)     -> 2 colunas
    let numCols = 2;
    if (fmt === "9:16") numCols = 1;
    else if (fmt === "16:9") numCols = 3;

    // Altura compacta para a foto no 16:9
    const isHorizontal = (fmt === "16:9");
    const gridTopY = heroTopY + heroH + (isHorizontal ? 12 : 18);
    const footRuleY = height - 60;
    const customPhrase = document.getElementById("stCustomPhrase")?.value?.trim() || "Meu voto para transformar o Brasil em 2026!";
    const hasPhrase = showSlogan && customPhrase.length > 0;

    // Reserva de altura para a frase
    const bannerH = isHorizontal ? 38 : 46;
    const phraseReserveH = hasPhrase ? (bannerH + (isHorizontal ? 12 : 18)) : 10;
    const availGridHeight = footRuleY - gridTopY - phraseReserveH;

    const totalCards = dataModules.length;
    if (totalCards > 0) {
      const numRows = Math.ceil(totalCards / numCols);
      const gridGapX = isHorizontal ? 12 : 16;
      const colW = Math.floor((width - (padX * 2) - ((numCols - 1) * gridGapX)) / numCols);

      const gridGapY = isHorizontal ? 8 : Math.max(8, Math.min(14, Math.floor((availGridHeight - (numRows * 70)) / (numRows - 1 || 1))));
      const cardH = Math.floor((availGridHeight - ((numRows - 1) * gridGapY)) / numRows);

      dataModules.forEach((card, idx) => {
        const col = idx % numCols;
        const row = Math.floor(idx / numCols);

        // Se for o último card ímpar em 2 colunas, expande; no 16:9 (3 cols), mantém alinhado à grade
        const isLastSingle = (numCols === 2 && idx === totalCards - 1 && totalCards % 2 === 1);

        const cx = isLastSingle
          ? padX
          : (padX + (col * (colW + gridGapX)));
        const cy = gridTopY + (row * (cardH + gridGapY));
        const currentCardW = isLastSingle ? (width - (padX * 2)) : colW;
        const centerX = cx + (currentCardW / 2);

        // Cartão
        ctx.fillStyle = colors.bgCard || "#FFFFFF";
        ctx.beginPath();
        ctx.roundRect ? ctx.roundRect(cx, cy, currentCardW, cardH, 6) : ctx.rect(cx, cy, currentCardW, cardH);
        ctx.fill();

        ctx.strokeStyle = colors.borderCard || "#D8CCC0";
        ctx.lineWidth = 1;
        ctx.stroke();

        // Barra Superior Colorida
        ctx.fillStyle = colors.kickerColor || "#C84B31";
        ctx.fillRect(cx + 6, cy, currentCardW - 12, 2.5);

        // Posições verticais calculadas proporcionalmente à altura da caixa
        const titleY = cy + Math.round(cardH * 0.22);
        const mainY = cy + Math.round(cardH * 0.54);
        const subY = cy + Math.round(cardH * 0.78);

        // Tipografia adaptada à altura
        const titleFontSize = cardH > 100 ? 14 : 12;
        const mainFontSize = cardH > 100 ? 32 : 26;
        const subFontSize = cardH > 100 ? 14 : 12;

        // 1. Título Superior
        ctx.fillStyle = colors.kickerColor || "#C84B31";
        ctx.font = `bold ${titleFontSize}px ${sansFont}`;
        ctx.textAlign = "center";
        ctx.textBaseline = "middle";
        ctx.fillText(`${card.icon}  ${card.title.toUpperCase()}`, centerX, titleY);

        // 2. Dado Principal no Centro Absoluto
        ctx.fillStyle = colors.textPrimary || "#111418";
        ctx.font = `bold ${mainFontSize}px ${card.type === 'money' || card.type === 'chart' ? "'JetBrains Mono', monospace" : fontDisplay}`;
        ctx.textAlign = "center";
        ctx.textBaseline = "middle";
        ctx.fillText(card.main, centerX, mainY);

        // 3. Subtítulo Inferior
        if (card.sub) {
          ctx.fillStyle = colors.textMuted || "#6B625B";
          ctx.font = `600 ${subFontSize}px ${sansFont}`;
          drawWrappedText(ctx, card.sub, centerX, subY - Math.round(subFontSize / 2), currentCardW - 32, subFontSize + 4, 1, "center");
        }
      });
    }

    // 6. Frase de Apoio Ancorada e Proporcional
    if (hasPhrase) {
      const bannerW = width - (padX * 2);
      const bannerY = footRuleY - bannerH - (isHorizontal ? 8 : 14);

      ctx.fillStyle = "rgba(200, 75, 49, 0.07)";
      ctx.beginPath();
      ctx.roundRect ? ctx.roundRect(padX, bannerY, bannerW, bannerH, 6) : ctx.rect(padX, bannerY, bannerW, bannerH);
      ctx.fill();

      ctx.strokeStyle = "rgba(200, 75, 49, 0.22)";
      ctx.lineWidth = 1;
      ctx.stroke();

      ctx.save();
      ctx.fillStyle = colors.kickerColor || "#C84B31";
      ctx.font = `italic bold ${isHorizontal ? 15 : 18}px ${fontDisplay}`;
      ctx.textAlign = "center";
      ctx.textBaseline = "middle";
      ctx.fillText(`“${customPhrase}”`, width / 2, bannerY + (bannerH / 2));
      ctx.restore();
    }

    // 7. Rodapé Editorial: fixo e centralizado
    const footerRuleY = height - 64;

    // Linha divisória horizontal (alinhada às margens de padX)
    ctx.strokeStyle = colors.borderCard || "#D8CCC0";
    ctx.lineWidth = 1.2;
    ctx.beginPath();
    ctx.moveTo(padX, footerRuleY);
    ctx.lineTo(width - padX, footerRuleY);
    ctx.stroke();

    // Centro vertical entre a linha divisória e o fundo
    const footerTextY = footerRuleY + 30;

    ctx.save();
    ctx.textBaseline = "middle";

    const labelText = "Monte o seu santinho em: ";
    const domainText = "emquemeuvoto.onrender.com";

    ctx.font = `bold 15px ${sansFont}`;
    const labelW = ctx.measureText(labelText).width || 190;

    ctx.font = "bold 15px 'JetBrains Mono', monospace";
    const domainW = ctx.measureText(domainText).width || 210;

    const totalW = labelW + domainW;
    const startFooterX = Math.round((width - totalW) / 2);

    // Texto de chamada
    ctx.fillStyle = colors.textMuted || "#6B625B";
    ctx.font = `bold 15px ${sansFont}`;
    ctx.textAlign = "left";
    ctx.fillText(labelText, startFooterX, footerTextY);

    // Link
    ctx.fillStyle = colors.textPrimary || "#111418";
    ctx.font = "bold 15px 'JetBrains Mono', monospace";
    ctx.fillText(domainText, startFooterX + labelW, footerTextY);
    ctx.restore();
  },

  async copyImage() {
    const canvas = document.getElementById("santinhoCanvas");
    if (!canvas) return;
    const ok = await ShareHelper.copyImageToClipboard(canvas);
    if (window.App && typeof App.showToast === "function") {
      App.showToast(ok ? "📋 Santinho copiado para a área de transferência!" : "⚠️ Não foi possível copiar a imagem automaticamente. Use o botão Salvar.");
    }
  },

  downloadPNG() {
    const canvas = document.getElementById("santinhoCanvas");
    if (!canvas || !this.currentCand) return;
    const c = this.currentCand;
    const safeName = (c.nome_urna || "candidato").toLowerCase().replace(/[^a-z0-9]/g, "_");
    const filename = `santinho_${safeName}_2026.png`;
    ShareHelper.downloadCanvas(canvas, filename);
  },

  downloadPDF() {
    const canvas = document.getElementById("santinhoCanvas");
    if (!canvas || !this.currentCand) return;
    const c = this.currentCand;
    const safeName = (c.nome_urna || "candidato").toLowerCase().replace(/[^a-z0-9]/g, "_");
    const dataUrl = canvas.toDataURL("image/png");

    const printFrame = document.createElement("iframe");
    printFrame.style.position = "fixed";
    printFrame.style.right = "0";
    printFrame.style.bottom = "0";
    printFrame.style.width = "0";
    printFrame.style.height = "0";
    printFrame.style.border = "0";
    document.body.appendChild(printFrame);

    const doc = printFrame.contentWindow.document;
    doc.open();
    doc.write(`
      <!DOCTYPE html>
      <html>
      <head>
        <title>Santinho 2026 — ${c.nome_urna}</title>
        <style>
          @page { size: ${this.format === "16:9" ? "landscape" : "portrait"}; margin: 8mm; }
          body { margin: 0; padding: 0; display: flex; align-items: center; justify-content: center; min-height: 100vh; background: #FFFFFF; }
          img { max-width: 100%; max-height: 96vh; object-fit: contain; display: block; margin: auto; }
        </style>
      </head>
      <body>
        <img src="${dataUrl}" onload="window.focus(); window.print(); setTimeout(() => { window.frameElement.remove(); }, 2000);">
      </body>
      </html>
    `);
    doc.close();
  },

  share(network) {
    if (!this.currentCand) return;
    const c = this.currentCand;
    const canvas = document.getElementById("santinhoCanvas");
    const filename = `santinho_${(c.nome_urna || 'candidato').toLowerCase().replace(/[^a-z0-9]/g, '_')}.png`;
    const text = `Meu candidato para as Eleições 2026: ${c.nome_urna} (${c.partido}), Nº ${c.numero}. Conheça seu histórico:`;

    if (network === "whatsapp") {
      ShareHelper.shareWhatsApp(text, null, canvas, filename);
    } else if (network === "twitter" || network === "x") {
      ShareHelper.shareTwitter(text, null, canvas, filename);
    } else if (network === "facebook") {
      ShareHelper.shareFacebook(null, text, canvas, filename);
    } else if (network === "instagram") {
      ShareHelper.shareInstagram(c.nome_urna, text, canvas, filename);
    } else {
      ShareHelper.shareNative(c.nome_urna, text, canvas);
    }
  }
};

/**
 * ══════════════════════════════════════════════════════════════════════════════
 * MODELO 2: COLINHA ELEITORAL (MÚLTIPLOS CANDIDATOS)
 * ══════════════════════════════════════════════════════════════════════════════
 */
const ColinhaStudio = {
  format: "9:16",
  theme: "paper",
  font: "serif",

  setFont(fontKey) {
    this.font = fontKey;
    ["serif", "moderno", "mono"].forEach(f => {
      const el = document.getElementById(`colFont_${f}`);
      if (el) el.classList.toggle("active", f === fontKey);
    });
    this.update();
  },

  getFontFamily(type = "sans") {
    if (this.font === "serif") {
      return type === "display" ? "'DM Serif Display', 'Merriweather', Georgia, serif" : "'Plus Jakarta Sans', sans-serif";
    }
    if (this.font === "mono") {
      return type === "display" ? "'JetBrains Mono', monospace" : "'Plus Jakarta Sans', sans-serif";
    }
    return "'Plus Jakarta Sans', sans-serif";
  },

  setFormat(fmt) {
    this.format = fmt;
    ['colFmt_vt', 'colFmt_sq', 'colFmt_hz'].forEach(id => {
      const btn = document.getElementById(id);
      if (btn) btn.classList.remove('active');
    });
    const idMap = { "9:16": "colFmt_vt", "1:1": "colFmt_sq", "16:9": "colFmt_hz" };
    const activeBtn = document.getElementById(idMap[fmt]);
    if (activeBtn) activeBtn.classList.add('active');
    this.update();
  },

  setTheme(theme) {
    this.theme = theme;
    ["paper", "slate", "party", "claret", "light"].forEach(t => {
      const el = document.getElementById(`colTheme_${t}`);
      if (el) el.classList.toggle("active", t === theme);
    });
    this.update();
  },

  async update() {
    const canvas = document.getElementById("colinhaCanvas");
    if (!canvas) return;
    const ctx = canvas.getContext("2d");
    if (!ctx) return;

    const candidates = (window.ListManager && typeof ListManager.getColinhaCandidates === "function")
      ? ListManager.getColinhaCandidates()
      : ((window.ListManager && typeof ListManager.getSavedCandidates === "function")
        ? ListManager.getSavedCandidates()
        : ((window.ListManager && typeof ListManager.getActiveCandidates === "function") ? ListManager.getActiveCandidates() : []));

    const activeUf = (window.App && App.state && App.state.uf && App.state.uf !== "BR")
      ? (window.UF_NOMES ? window.UF_NOMES[App.state.uf] || App.state.uf : App.state.uf)
      : (candidates[0]?.uf || "PERNAMBUCO");

    const customPhrase = document.getElementById("colPhraseInput")?.value?.trim() || "Meu voto consciente para transformar o Brasil!";
    const optIdeologia = document.getElementById("colOpt_ideologia") ? document.getElementById("colOpt_ideologia").checked : true;

    const fmt = this.format || "9:16";
    let width = 760;
    let height = 1180;

    if (fmt === "1:1") {
      width = 1080;
      height = 1080;
    } else if (fmt === "16:9") {
      width = 1280;
      height = 720;
    }

    canvas.width = width;
    canvas.height = height;

    const colors = typeof getThemeColors === "function" ? getThemeColors(this.theme || "paper", "#005B68") : {
      bgPage: "#FAF6F0",
      bgCard: "#FFFFFF",
      bgCardAlt: "#F4EEE5",
      borderCard: "#D8CCC0",
      textPrimary: "#111418",
      textSecondary: "#38312B",
      textMuted: "#6B625B",
      kickerColor: "#C84B31",
      isDark: false
    };

    const fontDisplay = this.getFontFamily("display");
    const sansFont = this.getFontFamily("sans");

    // 1. Fundo e Borda Editorial
    ctx.fillStyle = colors.bgPage || "#FAF6F0";
    ctx.fillRect(0, 0, width, height);

    ctx.strokeStyle = colors.borderCard || "#D8CCC0";
    ctx.lineWidth = 1.5;
    ctx.strokeRect(18, 18, width - 36, height - 36);

    const padX = fmt === "16:9" ? 52 : 40;

    // 2. Cabeçalho Editorial Mais Encorpado
    const headerTopY = fmt === "16:9" ? 30 : 42;

    ctx.save();
    ctx.textAlign = "left";
    ctx.textBaseline = "top";
    ctx.fillStyle = colors.isDark ? "#FFFFFF" : "#0A2530";
    ctx.font = "900 24px 'Plus Jakarta Sans', sans-serif";
    ctx.fillText("EM QUEM", padX, headerTopY);

    const badgeY = headerTopY + 28;
    const badgeW = 108;
    const badgeH = 26;
    ctx.fillStyle = colors.kickerColor || "#C84B31";
    ctx.beginPath();
    ctx.roundRect ? ctx.roundRect(padX, badgeY, badgeW, badgeH, 4) : ctx.rect(padX, badgeY, badgeW, badgeH);
    ctx.fill();

    ctx.fillStyle = "#FFFFFF";
    ctx.font = "900 15px 'Plus Jakarta Sans', sans-serif";
    ctx.textAlign = "center";
    ctx.fillText("EU VOTO", padX + badgeW / 2, badgeY + 5);
    ctx.restore();

    const dividerX = padX + badgeW + 20;
    ctx.strokeStyle = colors.borderCard || "#D8CCC0";
    ctx.lineWidth = 1.5;
    ctx.beginPath();
    ctx.moveTo(dividerX, headerTopY + 2);
    ctx.lineTo(dividerX, headerTopY + 56);
    ctx.stroke();

    ctx.save();
    ctx.textAlign = "left";
    ctx.textBaseline = "top";
    ctx.fillStyle = colors.textPrimary || "#111418";
    ctx.font = `800 ${fmt === "16:9" ? 28 : 34}px ${fontDisplay}`;
    const colTitle = document.getElementById("colTitleInput")?.value?.trim() || "COLINHA 2026";
    ctx.fillText(colTitle.toUpperCase(), dividerX + 20, headerTopY - 2);

    ctx.fillStyle = colors.textMuted || "#6B625B";
    ctx.font = "bold 13px 'Plus Jakarta Sans', sans-serif";
    ctx.letterSpacing = "0.10em";
    ctx.fillText(`${candidates.length} CANDIDATURAS SALVAS • ${String(activeUf).toUpperCase()}`, dividerX + 20, headerTopY + (fmt === "16:9" ? 34 : 38));
    ctx.restore();

    const logoImg = typeof getSiteLogoImage === "function" ? getSiteLogoImage() : null;
    const logoH = fmt === "16:9" ? 56 : 64;
    const logoW = Math.round(logoH * (1107 / 1221));
    const logoX = width - padX - logoW;
    const logoY = headerTopY - 2;

    if (logoImg && logoImg.complete && logoImg.naturalWidth > 0) {
      ctx.drawImage(logoImg, logoX, logoY, logoW, logoH);
    } else if (typeof drawUrnaIllustration === "function") {
      drawUrnaIllustration(ctx, width - padX - 36, headerTopY + 28, 68);
    }

    const headerRuleY = headerTopY + (fmt === "16:9" ? 70 : 78);
    ctx.strokeStyle = colors.borderCard || "#D8CCC0";
    ctx.lineWidth = 1.3;
    ctx.beginPath();
    ctx.moveTo(padX, headerRuleY);
    ctx.lineTo(width - padX, headerRuleY);
    ctx.stroke();

    // 3. Pré-carregamento das Fotos
    const loadPhoto = (c) => new Promise((resolve) => {
      const img = new Image();
      img.crossOrigin = "anonymous";
      img.src = c.foto_url || `/api/candidatos/${c.sq_candidato}/foto`;
      img.onload = () => resolve(img);
      img.onerror = () => resolve(null);
    });
    const candidatePhotos = await Promise.all(candidates.map(loadPhoto));

    // Title Case Helper
    const toTitleCase = (str) => {
      if (!str) return "";
      const lowers = ["de", "da", "do", "das", "dos", "e", "em"];
      return str.toLowerCase().split(" ").map((w, idx) => {
        if (idx > 0 && lowers.includes(w)) return w;
        return w.charAt(0).toUpperCase() + w.slice(1);
      }).join(" ");
    };

    // 4. Cálculo de Frase e Rodapé Fiel à Imagem
    const footRuleY = height - 54;
    const showFrase = document.getElementById("colOpt_frase") ? document.getElementById("colOpt_frase").checked : true;
    const hasPhrase = showFrase && customPhrase.length > 0;

    let quoteLines = [];
    const maxQuoteW = width - (padX * 2) - 40;
    const quoteFontSize = 25;
    const quoteLineH = 26;

    if (hasPhrase) {
      ctx.font = `italic bold ${quoteFontSize}px ${fontDisplay}`;
      const words = customPhrase.split(/\s+/);
      let testLine = "";
      for (let w of words) {
        let test = testLine ? (testLine + " " + w) : w;
        if (ctx.measureText("“" + test + "”").width > maxQuoteW && testLine) {
          quoteLines.push(testLine);
          testLine = w;
        } else {
          testLine = test;
        }
      }
      if (testLine) quoteLines.push(testLine);
      quoteLines = quoteLines.slice(0, 3);
    }

    const phraseReserveH = hasPhrase ? (quoteLines.length * quoteLineH + 24) : 10;
    const footerRuleY = height - 72;
    const availCardsH = footerRuleY - (headerRuleY + 16) - phraseReserveH;

    const totalCands = candidates.length;
    let cardsStartY = headerRuleY + 16;
    let totalBlockH = 0;

    if (totalCands === 0) {
      ctx.fillStyle = colors.textMuted || "#6B625B";
      ctx.font = `600 16px ${sansFont}`;
      ctx.textAlign = "center";
      ctx.textBaseline = "middle";
      ctx.fillText("Nenhum candidato salvo na sua colinha ainda.", width / 2, (headerRuleY + footerRuleY) / 2);
    } else {
      // Ordenação canônica da urna
      const CARGO_SLOT_ORDER = {
        "deputado estadual": 1,
        "deputado distrital": 1,
        "deputado federal": 2,
        "senador": 3,
        "governador": 4,
        "presidente": 5
      };

      let senadorCount = 0;
      const sortedCands = [...candidates].sort((a, b) => {
        const ca = (a.cargo || "").toLowerCase();
        const cb = (b.cargo || "").toLowerCase();
        const oa = Object.keys(CARGO_SLOT_ORDER).find(k => ca.includes(k)) ? CARGO_SLOT_ORDER[Object.keys(CARGO_SLOT_ORDER).find(k => ca.includes(k))] : 99;
        const ob = Object.keys(CARGO_SLOT_ORDER).find(k => cb.includes(k)) ? CARGO_SLOT_ORDER[Object.keys(CARGO_SLOT_ORDER).find(k => cb.includes(k))] : 99;
        return oa - ob;
      }).slice(0, 6);

      const numCards = sortedCands.length;
      const isTwoCol = fmt === "16:9" && numCards > 3;
      const cols = isTwoCol ? 2 : 1;
      const rows = Math.ceil(numCards / cols);
      const gapY = Math.max(8, Math.min(16, Math.floor((availCardsH - (rows * 88)) / Math.max(1, rows - 1))));
      const cardH = Math.max(84, Math.min(120, Math.floor((availCardsH - ((rows - 1) * gapY)) / rows)));
      const gapX = 20;
      const cardW = (width - (padX * 2) - ((cols - 1) * gapX)) / cols;

      // Centralização do bloco inteiro de candidatos na área útil
      totalBlockH = (rows * cardH) + ((rows - 1) * gapY);
      cardsStartY = headerRuleY + 16 + Math.max(0, Math.floor((availCardsH - totalBlockH) / 2));

      for (let i = 0; i < sortedCands.length; i++) {
        const c = sortedCands[i];
        const colIdx = i % cols;
        const rowIdx = Math.floor(i / cols);
        const cx = padX + (colIdx * (cardW + gapX));
        const cy = cardsStartY + (rowIdx * (cardH + gapY));

        const origIdx = candidates.indexOf(c);
        const img = origIdx >= 0 ? candidatePhotos[origIdx] : null;

        const pColor = c.partido_cor_hex || (typeof getPartyColor === "function" ? getPartyColor(c.partido || c.sigla_partido) : "#005B68");
        const nameTitle = toTitleCase(c.nome_urna || c.nome || "Candidato");

        let cargoTitle = (c.cargo || "CANDIDATO").toUpperCase();
        if (cargoTitle.includes("SENADOR")) {
          senadorCount++;
          if (senadorCount === 2) cargoTitle = "SENADOR (2º VOTO)";
        }
        const ufStr = c.uf || "BR";
        const numDigits = String(c.numero || "0000").trim().split("");

        // Cartão (Fundo Branco com Cantos Arredondados)
        ctx.fillStyle = colors.bgCard || "#FFFFFF";
        if (typeof ctx.roundRect === "function") {
          ctx.beginPath();
          ctx.roundRect(cx, cy, cardW, cardH, 8);
          ctx.fill();
        } else {
          ctx.fillRect(cx, cy, cardW, cardH);
        }

        ctx.strokeStyle = colors.borderCard || "#D8CCC0";
        ctx.lineWidth = 1.2;
        if (typeof ctx.roundRect === "function") {
          ctx.beginPath();
          ctx.roundRect(cx, cy, cardW, cardH, 8);
          ctx.stroke();
        } else {
          ctx.strokeRect(cx, cy, cardW, cardH);
        }

        // Barra Lateral Esquerda com a Cor do Partido
        ctx.fillStyle = pColor || colors.kickerColor || "#C84B31";
        const barMargin = 6;
        if (typeof ctx.roundRect === "function") {
          ctx.beginPath();
          ctx.roundRect(cx, cy + barMargin, 4.5, cardH - (barMargin * 2), [3, 0, 0, 3]);
          ctx.fill();
        } else {
          ctx.fillRect(cx, cy + barMargin, 4.5, cardH - (barMargin * 2));
        }

        // Foto com tamanho reduzido e bordas arredondadas
        const photoPad = 12;
        const photoDim = cardH - (photoPad * 2);
        const photoRadius = 10;
        const photoX = cx + 8 + photoPad;
        const photoY = cy + photoPad;

        ctx.save();
        ctx.beginPath();
        if (typeof ctx.roundRect === "function") {
          ctx.roundRect(photoX, photoY, photoDim, photoDim, photoRadius);
        } else {
          ctx.rect(photoX, photoY, photoDim, photoDim);
        }
        ctx.clip();

        if (img && img.complete && img.naturalWidth > 0) {
          const imgW = img.naturalWidth || img.width;
          const imgH = img.naturalHeight || img.height;
          const srcAspect = imgW / imgH;
          let sx = 0, sy = 0, sw = imgW, sh = imgH;
          if (srcAspect > 1) {
            sw = imgH;
            sh = imgH;
            sx = Math.round((imgW - sw) / 2);
          } else {
            sw = imgW;
            sh = imgW;
            sy = Math.round(Math.max(0, Math.min(imgH - sh, (imgH - sh) * 0.16)));
          }
          ctx.drawImage(img, sx, sy, sw, sh, photoX, photoY, photoDim, photoDim);
        } else {
          ctx.fillStyle = colors.bgCardAlt || "#F4EEE5";
          ctx.fillRect(photoX, photoY, photoDim, photoDim);
          ctx.fillStyle = colors.textPrimary || "#111418";
          ctx.font = `bold 24px ${fontDisplay}`;
          ctx.textAlign = "center";
          ctx.textBaseline = "middle";
          ctx.fillText(nameTitle.charAt(0) || "?", photoX + photoDim / 2, photoY + photoDim / 2);
        }
        ctx.restore();

        ctx.strokeStyle = colors.borderCard || "#D8CCC0";
        ctx.lineWidth = 1.2;
        ctx.beginPath();
        ctx.roundRect ? ctx.roundRect(photoX, photoY, photoDim, photoDim, photoRadius) : ctx.rect(photoX, photoY, photoDim, photoDim);
        ctx.stroke();

        // Bloco de Identificação com Distribuição Vertical Equilibrada
        const infoX = photoX + photoDim + 18;
        const boxH = Math.max(40, Math.min(56, Math.round(cardH * 0.52)));
        const boxW = Math.max(30, Math.min(44, Math.round(boxH * 0.74)));
        const boxGap = Math.max(4, Math.min(7, Math.round(boxW * 0.16)));
        const totalNumW = (numDigits.length * boxW) + ((numDigits.length - 1) * boxGap);
        const maxInfoW = cardW - (infoX - cx) - totalNumW - 20;

        // Distribuição vertical proporcional à altura real do card
        const nameFontSize = cardH > 95 ? 25 : 21;
        const line1Y = cy + Math.round(cardH * 0.25);
        const line2Y = cy + Math.round(cardH * 0.50);
        const line3Y = cy + Math.round(cardH * 0.75);

        // 1. Cargo • UF
        ctx.fillStyle = colors.kickerColor || "#C84B31";
        ctx.font = `bold 12px ${sansFont}`;
        ctx.textAlign = "left";
        ctx.textBaseline = "middle";
        ctx.fillText(`${cargoTitle} • ${ufStr}`, infoX, line1Y);

        // 2. Nome de Urna
        ctx.fillStyle = colors.textPrimary || "#111418";
        ctx.font = `800 ${nameFontSize}px ${fontDisplay}`;
        ctx.textAlign = "left";
        ctx.textBaseline = "middle";
        ctx.fillText(nameTitle, infoX, line2Y);

        // 3. Linha do Partido + Tag de Ideologia
        ctx.save();
        ctx.textAlign = "left";
        ctx.textBaseline = "middle";

        // Marcador quadrado do partido
        ctx.fillStyle = pColor || colors.kickerColor || "#C84B31";
        ctx.fillRect(infoX, line3Y - 5, 10, 10);

        // Sigla do Partido
        ctx.fillStyle = colors.textPrimary || "#111418";
        ctx.font = `bold 13px ${sansFont}`;
        const pLabel = c.partido || c.sigla_partido || "—";
        ctx.fillText(pLabel, infoX + 15, line3Y);
        const pWidth = ctx.measureText(pLabel).width;

        // Tag de Ideologia alinhada
        if (optIdeologia) {
          const ideologiaNome = c.ideologia_nome || c.ideologia || c.espectro;
          if (ideologiaNome) {
            const tagX = infoX + 15 + pWidth + 10;
            ctx.font = `600 11px ${sansFont}`;
            const tagTextW = ctx.measureText(ideologiaNome).width;
            const tagW = tagTextW + 12;
            const tagH = 18;
            const tagY = line3Y - (tagH / 2);

            ctx.fillStyle = colors.isDark ? "rgba(255,255,255,0.12)" : "#FEE2E2";
            if (typeof ctx.roundRect === "function") {
              ctx.beginPath();
              ctx.roundRect(tagX, tagY, tagW, tagH, 3);
              ctx.fill();
            } else {
              ctx.fillRect(tagX, tagY, tagW, tagH);
            }

            ctx.fillStyle = colors.isDark ? "#FFFFFF" : "#991B1B";
            ctx.textAlign = "center";
            ctx.textBaseline = "middle";
            ctx.fillText(ideologiaNome, tagX + (tagW / 2), line3Y);
          }
        }
        ctx.restore();

        // Número na Urna à Direita em Caixinhas Contornadas
        const numStartX = cx + cardW - totalNumW - 16;
        const numStartY = cy + (cardH - boxH) / 2;
        drawUrnaDigitBoxes(ctx, c.numero, numStartX, numStartY, totalNumW, boxH, colors, boxW, 28, boxGap);
      }
    }

    // 5. Frase de Apoio (Centralizada perfeitamente no vão entre os cards e o rodapé)
    if (hasPhrase && quoteLines.length > 0) {
      const quoteTotalH = quoteLines.length * quoteLineH;
      // Ponto final dos cards desenhados
      const actualCardsBottomY = (totalCands > 0 && typeof cardsStartY !== "undefined" && typeof totalBlockH !== "undefined")
        ? (cardsStartY + totalBlockH)
        : (headerRuleY + 40);

      // Centro vertical exato entre o fim dos cards e a barra do rodapé
      const quoteSpaceAvailable = footerRuleY - actualCardsBottomY;
      const startQuoteY = actualCardsBottomY + Math.max(12, Math.floor((quoteSpaceAvailable - quoteTotalH) / 2));

      ctx.save();
      ctx.fillStyle = colors.kickerColor || "#C84B31";
      ctx.font = `italic bold ${quoteFontSize}px ${fontDisplay}`;
      ctx.textAlign = "center";
      ctx.textBaseline = "top";

      quoteLines.forEach((lineText, idx) => {
        const formatted = idx === 0 && quoteLines.length === 1
          ? `“${lineText}”`
          : (idx === 0 ? `“${lineText}` : (idx === quoteLines.length - 1 ? `${lineText}”` : lineText));
        ctx.fillText(formatted, width / 2, startQuoteY + (idx * quoteLineH));
      });
      ctx.restore();
    }

    // 6. Rodapé Editorial da Colinha: Mais Encorpado e Centralizado
    // Linha divisória horizontal
    ctx.strokeStyle = colors.borderCard || "#D8CCC0";
    ctx.lineWidth = 1.3;
    ctx.beginPath();
    ctx.moveTo(18, footerRuleY);
    ctx.lineTo(width - 18, footerRuleY);
    ctx.stroke();

    // Centro vertical exato entre a linha divisória e a moldura externa inferior
    const footerTextY = footerRuleY + ((height - 18) - footerRuleY) / 2;

    ctx.save();
    ctx.textBaseline = "middle";

    const labelText = "Monte a sua colinha em: ";
    const domainText = "emquemeuvoto.onrender.com";

    // Mede as larguras com tipografia ampliada para 16px
    ctx.font = `bold 16px ${sansFont}`;
    const labelW = ctx.measureText(labelText).width;

    ctx.font = "bold 16px 'JetBrains Mono', monospace";
    const domainW = ctx.measureText(domainText).width;

    const totalFooterW = labelW + domainW;
    const startFooterX = Math.round((width - totalFooterW) / 2);

    // Texto de chamada
    ctx.fillStyle = colors.textMuted || "#6B625B";
    ctx.font = `bold 16px ${sansFont}`;
    ctx.textAlign = "left";
    ctx.fillText(labelText, startFooterX, footerTextY);

    // Link mono
    ctx.fillStyle = colors.textPrimary || "#111418";
    ctx.font = "bold 16px 'JetBrains Mono', monospace";
    ctx.fillText(domainText, startFooterX + labelW, footerTextY);
    ctx.restore();
  },

  async copyImage() {
    const canvas = document.getElementById("colinhaCanvas");
    if (!canvas) return;
    const ok = await ShareHelper.copyImageToClipboard(canvas);
    if (window.App && typeof App.showToast === "function") {
      App.showToast(ok ? "📋 Imagem da colinha copiada para a área de transferência!" : "⚠️ Não foi possível copiar a imagem automaticamente. Use o botão Salvar.");
    }
  },

  downloadPNG() {
    const canvas = document.getElementById("colinhaCanvas");
    if (!canvas) return;
    const listName = (document.getElementById("colTitleInput")?.value?.trim() || "colinha")
      .toLowerCase()
      .replace(/[^a-z0-9]/g, "_");
    const filename = `${listName}_2026.png`;
    ShareHelper.downloadCanvas(canvas, filename);
  },

  downloadPDF() {
    const canvas = document.getElementById("colinhaCanvas");
    if (!canvas) return;
    const listName = (document.getElementById("colTitleInput")?.value?.trim() || "colinha")
      .toLowerCase()
      .replace(/[^a-z0-9]/g, "_");
    const filename = `${listName}_2026.pdf`;
    const dataUrl = canvas.toDataURL("image/png");

    const printFrame = document.createElement("iframe");
    printFrame.style.position = "fixed";
    printFrame.style.right = "0";
    printFrame.style.bottom = "0";
    printFrame.style.width = "0";
    printFrame.style.height = "0";
    printFrame.style.border = "0";
    document.body.appendChild(printFrame);

    const doc = printFrame.contentWindow.document;
    doc.open();
    doc.write(`
      <!DOCTYPE html>
      <html>
      <head>
        <title>Colinha 2026 — ${listName}</title>
        <style>
          @page { size: ${this.format === "16:9" ? "landscape" : "portrait"}; margin: 8mm; }
          body { margin: 0; padding: 0; display: flex; align-items: center; justify-content: center; min-height: 100vh; background: #FFFFFF; }
          img { max-width: 100%; max-height: 96vh; object-fit: contain; display: block; margin: auto; }
        </style>
      </head>
      <body>
        <img src="${dataUrl}" onload="window.focus(); window.print(); setTimeout(() => { window.frameElement.remove(); }, 2000);">
      </body>
      </html>
    `);
    doc.close();
  },

  share(network) {
    const listName = document.getElementById("colTitleInput")?.value?.trim() || "Minha Colinha Eleitoral 2026";
    const canvas = document.getElementById("colinhaCanvas");
    const filename = "minha_colinha_2026.png";
    const text = `Confira a minha ${listName} para as Eleições 2026! Conheça meus candidatos e monte a sua:`;

    if (network === "whatsapp") {
      ShareHelper.shareWhatsApp(text, null, canvas, filename);
    } else if (network === "twitter" || network === "x") {
      ShareHelper.shareTwitter(text, null, canvas, filename);
    } else if (network === "facebook") {
      ShareHelper.shareFacebook(null, text, canvas, filename);
    } else if (network === "instagram") {
      ShareHelper.shareInstagram(listName, text, canvas, filename);
    } else {
      ShareHelper.shareNative(listName, text, canvas);
    }
  }
};

/**
 * Compatibilidade legada com ExportManager
 */
const ExportManager = {
  async generateSantinhoImage(candidatos, listName = "Colinha") {
    ColinhaStudio.update();
    ColinhaStudio.downloadPNG();
  }
};

// Exportações Globais
window.drawWrappedText = drawWrappedText;
window.drawUrnaIllustration = drawUrnaIllustration;
window.drawEditorialHeader = drawEditorialHeader;
window.drawEditorialFooter = drawEditorialFooter;
window.getThemeColors = getThemeColors;
window.getSiteLogoImage = getSiteLogoImage;
window.SantinhoStudio = SantinhoStudio;
window.ColinhaStudio = ColinhaStudio;
window.ExportManager = ExportManager;
