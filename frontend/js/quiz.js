/**
 * Controlador do Modal do Quiz de Afinidade (Bússola Ideológica)
 */

const QuizController = {
  perguntas: [],
  currentIndex: 0,
  respostas: {}, // id_pergunta -> valor (1..7)

  async init() {
    try {
      const resp = await fetch("/api/quiz/perguntas");
      if (resp.ok) {
        this.perguntas = await resp.json();
      }
    } catch (e) {
      console.error("Erro ao carregar perguntas do quiz:", e);
    }
  },

  open() {
    if (window.App && typeof App.closeMobileSidebar === "function") {
      App.closeMobileSidebar();
    }
    const modal = document.getElementById("quizModal");
    if (!modal) return;
    this.currentIndex = 0;
    this.respostas = {};

    const qView = document.getElementById("quizQuestionView");
    const rView = document.getElementById("quizResultView");
    const pBar = document.getElementById("quizProgressBarWrap");
    if (qView) qView.style.display = "block";
    if (rView) rView.style.display = "none";
    if (pBar) pBar.style.display = "block";

    this.renderQuestion();
    modal.classList.add("open");
  },

  close() {
    const modal = document.getElementById("quizModal");
    if (modal) modal.classList.remove("open");
  },

  retake() {
    this.currentIndex = 0;
    this.respostas = {};

    const qView = document.getElementById("quizQuestionView");
    const rView = document.getElementById("quizResultView");
    const pBar = document.getElementById("quizProgressBarWrap");
    if (qView) qView.style.display = "block";
    if (rView) rView.style.display = "none";
    if (pBar) pBar.style.display = "block";

    this.renderQuestion();
  },

  renderQuestion() {
    if (!this.perguntas || this.perguntas.length === 0) return;
    const q = this.perguntas[this.currentIndex];
    if (!q) return;

    // Atualizar barra de progresso
    const progressPct = ((this.currentIndex + 1) / this.perguntas.length) * 100;
    const progressFill = document.getElementById("quizProgressFill");
    if (progressFill) progressFill.style.width = `${progressPct}%`;

    const dimTag = document.getElementById("quizDimTag");
    const statement = document.getElementById("quizStatement");
    const poleLeft = document.getElementById("quizPoleLeft");
    const poleRight = document.getElementById("quizPoleRight");
    const likertContainer = document.getElementById("quizLikertButtons");
    const stepIndicator = document.getElementById("quizStepIndicator");

    if (dimTag) dimTag.innerText = q.dimensao;
    if (statement) statement.innerText = `"${q.enunciado}"`;
    if (poleLeft) poleLeft.innerText = "1 - " + q.polo_esquerda_label.replace("Discordo Totalmente", "Discordo");
    if (poleRight) poleRight.innerText = "7 - " + q.polo_direita_label.replace("Concordo Totalmente", "Concordo");
    if (stepIndicator) stepIndicator.innerText = `Pergunta ${this.currentIndex + 1} de ${this.perguntas.length}`;

    // Renderizar botões da escala Likert (1 a 7)
    if (likertContainer) {
      likertContainer.innerHTML = "";
      for (let i = 1; i <= 7; i++) {
        const btn = document.createElement("button");
        btn.type = "button";
        btn.className = "btn-likert";
        btn.innerText = i;
        if (this.respostas[q.id] === i) {
          btn.classList.add("selected");
        }
        btn.onclick = () => this.selectValue(q.id, i);
        likertContainer.appendChild(btn);
      }
    }

    // Botões de navegação
    const btnPrev = document.getElementById("quizBtnPrev");
    const btnNext = document.getElementById("quizBtnNext");
    if (btnPrev) btnPrev.style.display = this.currentIndex > 0 ? "inline-flex" : "none";
    if (btnNext) {
      btnNext.innerText = this.currentIndex === this.perguntas.length - 1 ? "Ver Meu Posicionamento →" : "Próxima →";
      btnNext.disabled = !this.respostas[q.id];
    }
  },

  selectValue(perguntaId, valor) {
    this.respostas[perguntaId] = valor;
    this.renderQuestion();
  },

  prevQuestion() {
    if (this.currentIndex > 0) {
      this.currentIndex--;
      this.renderQuestion();
    }
  },

  async nextQuestion() {
    const q = this.perguntas[this.currentIndex];
    if (!this.respostas[q.id]) {
      alert("Por favor, selecione uma nota de 1 a 7 para continuar.");
      return;
    }

    if (this.currentIndex < this.perguntas.length - 1) {
      this.currentIndex++;
      this.renderQuestion();
    } else {
      await this.submitQuiz();
    }
  },

  async submitQuiz() {
    const payload = {
      respostas: Object.keys(this.respostas).map(id => ({
        id_pergunta: parseInt(id),
        valor: this.respostas[id]
      }))
    };

    try {
      const resp = await fetch("/api/quiz/calcular", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload)
      });

      if (resp.ok) {
        const resultado = await resp.json();
        this.renderResult(resultado);

        // Google Analytics 4 - Evento Customizado de Conclusão do Quiz
        if (typeof window.gtag === 'function') {
          window.gtag('event', 'quiz_completed', {
            posicao_economica: resultado.posicao_economica,
            posicao_social: resultado.posicao_social,
            quadrante: resultado.quadrante || 'Indefinido'
          });
        }
      }
    } catch (e) {
      console.error("Erro ao calcular quiz:", e);
      alert("Erro ao processar o resultado do quiz.");
    }
  },

  renderResult(res) {
    const qView = document.getElementById("quizQuestionView");
    const rView = document.getElementById("quizResultView");
    const pBar = document.getElementById("quizProgressBarWrap");

    if (qView) qView.style.display = "none";
    if (pBar) pBar.style.display = "none";
    if (!rView) return;

    rView.style.display = "block";

    // Cor semântica do posicionamento
    let ideoColor = "var(--accent-petroleum, #005B68)";
    if (res.faixa_ideologica <= 2) ideoColor = "#C84B31"; // Esquerda
    else if (res.faixa_ideologica === 3) ideoColor = "#E87A5D"; // Centro-Esquerda
    else if (res.faixa_ideologica === 4) ideoColor = "#00AFA5"; // Centro
    else if (res.faixa_ideologica === 5) ideoColor = "#005B68"; // Centro-Direita
    else ideoColor = "#0A2240"; // Direita / Extrema-Direita

    // Posição percentual na régua (0 a 100)
    const clampedPct = Math.min(96, Math.max(4, res.escala_continua_0_100));

    // Montar cards dos partidos com afinidade
    const partyCardsHtml = (res.partidos_mais_proximos || []).map(p => {
      const nota = p.indice_sintetico !== undefined && p.indice_sintetico !== null ? p.indice_sintetico : 50;
      const dist = Math.abs(nota - res.escala_continua_0_100);
      const afinidade = Math.max(15, Math.round(100 - (dist * 1.25)));
      const dotColor = p.cor_hex || (window.App ? App.getPartidoColor(p.sigla) : "#005B68");

      return `
        <div style="display: flex; align-items: center; justify-content: space-between; padding: 0.6rem 0.8rem; background: var(--bg-page); border: 1px solid var(--border-color); border-radius: var(--radius-xs); gap: 0.75rem;">
          <div style="display: flex; align-items: center; gap: 0.6rem; min-width: 0;">
            <span style="width: 10px; height: 10px; border-radius: 50%; background: ${dotColor}; flex-shrink: 0; box-shadow: 0 0 0 1px rgba(0,0,0,0.1);"></span>
            <div style="min-width: 0;">
              <div style="display: flex; align-items: center; gap: 0.4rem;">
                <strong style="font-size: 0.9rem; font-weight: 800; color: var(--text-primary); font-family: var(--font-mono);">${p.sigla}</strong>
                <span style="font-size: 0.68rem; font-weight: 700; color: var(--text-muted); background: var(--bg-surface); padding: 0.1rem 0.4rem; border-radius: 4px; border: 1px solid var(--border-color);">${p.classificacao || 'Legenda'}</span>
              </div>
              <div style="font-size: 0.72rem; color: var(--text-muted); margin-top: 0.15rem;">
                Posição no espectro: <strong style="color: var(--text-primary); font-family: var(--font-mono);">${nota.toFixed(1)}</strong>
              </div>
            </div>
          </div>
          <div style="display: flex; align-items: center; gap: 0.5rem; flex-shrink: 0;">
            <span style="font-size: 0.75rem; font-weight: 800; color: var(--accent-petroleum); background: rgba(0, 91, 104, 0.08); padding: 0.25rem 0.5rem; border-radius: var(--radius-xs);">
              ${afinidade}% afinidade
            </span>
            <button type="button" class="btn-header" style="padding: 0.25rem 0.5rem; font-size: 0.7rem; font-weight: 700;" onclick="QuizController.filterByParty('${p.sigla}')" title="Filtrar candidatos do ${p.sigla}">
              Ver candidatos
            </button>
          </div>
        </div>
      `;
    }).join("");

    rView.innerHTML = `
      <div style="text-align: center; padding: 0.25rem 0 0.5rem 0;">
        <!-- Kicker de Cabeçalho -->
        <div style="font-size: 0.72rem; font-weight: 800; text-transform: uppercase; letter-spacing: 0.08em; color: ${ideoColor}; margin-bottom: 0.2rem;">
          Seu Posicionamento no Espectro Político
        </div>

        <!-- Título do Posicionamento com Alto Contraste Editorial -->
        <h2 style="font-family: var(--font-serif); font-size: 2rem; font-weight: 800; color: var(--text-primary); margin: 0.2rem 0 0.5rem 0; line-height: 1.15;">
          ${res.classificacao_nome}
        </h2>

        <!-- Badge da Posição Contínua -->
        <div style="display: inline-flex; align-items: center; gap: 0.5rem; padding: 0.35rem 0.95rem; background: var(--bg-page); border: 1px solid var(--border-color); border-radius: 9999px; font-size: 0.8rem; font-weight: 700; color: var(--text-primary); margin-bottom: 1.25rem;">
          <span>Posição no Espectro: <strong style="color: ${ideoColor}; font-family: var(--font-mono); font-size: 0.95rem;">${res.escala_continua_0_100.toFixed(1)}</strong> / 100</span>
          <span style="color: var(--border-color);">•</span>
          <span style="color: var(--text-muted); font-size: 0.75rem;">Escala 1 a 7: ${res.pontuacao_escala_1_7.toFixed(2)}</span>
        </div>

        <!-- Régua Visual do Espectro (0 a 100) com Marcador do Eleitor -->
        <div style="background: var(--bg-page); border: 1px solid var(--border-color); border-radius: var(--radius-sm); padding: 1rem 1.1rem; margin-bottom: 1.25rem; text-align: left;">
          <div style="display: flex; justify-content: space-between; font-size: 0.72rem; font-weight: 800; margin-bottom: 0.6rem;">
            <span style="color: #C84B31;">0 • Extrema-Esquerda</span>
            <span style="color: var(--text-muted);">50 • Centro</span>
            <span style="color: #005B68;">100 • Extrema-Direita</span>
          </div>

          <!-- Barra Gradiente Contínua com Pinpoint do Usuário -->
          <div style="position: relative; height: 16px; border-radius: 8px; background: linear-gradient(90deg, #C84B31 0%, #E87A5D 25%, #00AFA5 50%, #005B68 75%, #0A2240 100%); box-shadow: inset 0 1px 3px rgba(0,0,0,0.18);">
            <div style="position: absolute; left: ${clampedPct}%; top: -7px; transform: translateX(-50%); width: 30px; height: 30px; background: var(--bg-surface); border: 3px solid var(--text-primary); border-radius: 50%; box-shadow: 0 2px 8px rgba(0,0,0,0.35); display: flex; align-items: center; justify-content: center; font-size: 0.65rem; font-weight: 900; color: var(--text-primary); cursor: default;" title="Sua posição: ${res.escala_continua_0_100.toFixed(1)}">
              ●
            </div>
          </div>

          <!-- Rótulos da Régua -->
          <div style="display: flex; justify-content: space-between; font-size: 0.68rem; color: var(--text-muted); margin-top: 0.55rem; font-weight: 600;">
            <span>Esq.</span>
            <span>Centro-Esq.</span>
            <span>Centro</span>
            <span>Centro-Dir.</span>
            <span>Dir.</span>
          </div>
        </div>

        <!-- Bloco de Partidos com Maior Afinidade -->
        <div style="background: var(--bg-surface); border: 1px solid var(--border-color); border-radius: var(--radius-sm); padding: 1.1rem; text-align: left; margin-bottom: 1.25rem;">
          <div style="display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 0.35rem;">
            <div style="font-size: 0.76rem; font-weight: 800; text-transform: uppercase; letter-spacing: 0.05em; color: var(--text-primary);">
              Partidos com Maior Afinidade no Espectro
            </div>
            <span style="font-size: 0.68rem; font-weight: 700; color: var(--accent-petroleum);">${(res.partidos_mais_proximos || []).length} Legendas no seu Espectro</span>
          </div>

          <!-- Explicação Concisa da Escala 0 a 100 -->
          <div style="font-size: 0.74rem; color: var(--text-muted); line-height: 1.45; margin-bottom: 0.85rem; padding: 0.5rem 0.75rem; background: var(--bg-page); border-left: 3px solid var(--accent-petroleum); border-radius: 0 var(--radius-xs) var(--radius-xs) 0;">
            A escala vai de <strong>0 (Esquerda)</strong> a <strong>100 (Direita)</strong>. Quanto mais próximo da sua posição, maior a afinidade com a legenda. <a href="docs/relatorio_ideologia_e_quiz.md" target="_blank" style="color: var(--accent-petroleum); text-decoration: underline; font-weight: 700;">Ver metodologia</a>
          </div>

          <!-- Lista de Partidos -->
          <div style="display: flex; flex-direction: column; gap: 0.45rem;">
            ${partyCardsHtml}
          </div>
        </div>

        <!-- Botões de Ação -->
        <div style="display: flex; gap: 0.6rem; flex-wrap: wrap; margin-top: 1rem;">
          <button type="button" class="btn-header primary" style="flex: 2; min-width: 210px; justify-content: center; padding: 0.85rem; font-size: 0.85rem;" onclick="QuizController.applyResult(${res.faixa_ideologica})">
            ✓ Aplicar ao Filtro e Ver Candidatos
          </button>
          <button type="button" class="btn-header" style="flex: 1; min-width: 140px; justify-content: center; padding: 0.85rem; font-size: 0.85rem;" onclick="QuizController.retake()">
            🔄 Refazer Quiz
          </button>
        </div>
      </div>
    `;
  },

  applyResult(faixa) {
    if (window.App && typeof App.setIdeologyRange === "function") {
      App.setIdeologyRange(Math.max(1, faixa - 1), Math.min(7, faixa + 1));
    }
    this.close();
  },

  filterByParty(sigla) {
    if (window.App) {
      if (typeof App.setIdeologyRange === "function") {
        App.setIdeologyRange(1, 7); // Reseta faixa para não ocultar o partido selecionado
      }
      App.state.partidos_gosta = [sigla];
      App.state.partidos_desgosta = [];
      if (typeof App.renderPartidosChips === "function") {
        App.renderPartidosChips();
      }
      App.state.pagina = 1;
      if (typeof App.carregarCandidatos === "function") {
        App.carregarCandidatos();
      }
    }
    this.close();
  }
};

window.QuizController = QuizController;
