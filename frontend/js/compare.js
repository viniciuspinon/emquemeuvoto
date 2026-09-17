/**
 * Comparador de Candidatos Lado a Lado — Em Quem Eu Voto 2026
 * Dados Oficiais do TSE com Fotos Locais e Suporte a:
 * - Reordenação fluida via Arrastar e Soltar (Drag and Drop com handle "=")
 * - Exportação de Imagem em Alta Resolução (PNG)
 * - Compartilhamento Social Direto (WhatsApp, X, Telegram, Nativo)
 * - Personalização Dinâmica dos Campos Exibidos
 */

const CompareController = {
  selectedCandidates: [],
  draggedIdx: null,
  dragOverIdx: null,
  lastExportCanvas: null,
  visibleFields: {
    ideologia: true,
    profissao: true,
    instrucao: true,
    receitas: false,
    gastos: false,
    ranking_partido: false,
    ranking_geral: false,
    governismo: false,
    assiduidade: false,
    bancadas: false,
    votacao: false,
    mandato: false,
    bens: false,
    demografia: false
  },

  availableFields: [
    { key: "ideologia", label: "Ideologia do Partido" },
    { key: "profissao", label: "Profissão / Ocupação" },
    { key: "instrucao", label: "Escolaridade" },
    { key: "mandato", label: "Mandato Atual" },
    { key: "bens", label: "Bens Declarados" },
    { key: "receitas", label: "Receitas Arrecadadas" },
    { key: "gastos", label: "Gastos Declarados" },
    { key: "votacao", label: "Votação em Lei" },
    { key: "assiduidade", label: "Presença / Assiduidade" },
    { key: "governismo", label: "Governismo (Radar)" },
    { key: "bancadas", label: "Frentes & Bancadas" },
    { key: "ranking_partido", label: "Rank no Partido" },
    { key: "ranking_geral", label: "Rank Geral (Cargo)" },
    { key: "demografia", label: "Gênero / Cor" }
  ],

  isFederalLegislator(c) {
    if (!c) return false;
    const cEx = String(c.cargo_exercicio || '').toLowerCase();
    if (cEx.includes('estadual') || cEx.includes('distrital') || cEx.includes('vereador') || cEx.includes('prefeito')) {
      return cEx.includes('federal') || cEx.includes('senad');
    }
    if (cEx.includes('federal') || cEx.includes('senad')) return true;
    if (c.radar_congresso_url) return true;
    return false;
  },

  getAvailableVotes() {
    const set = new Set();
    const list = [];
    this.selectedCandidates.forEach(c => {
      if (!this.isFederalLegislator(c)) return;
      const votos = Array.isArray(c.votacoes_radar) ? c.votacoes_radar : (typeof c.votacoes_radar === 'string' ? (() => { try { return JSON.parse(c.votacoes_radar); } catch (e) { return []; } })() : []);
      votos.forEach(v => {
        const name = (v.apelido || v.proposicao || "").trim();
        if (name && !set.has(name)) {
          set.add(name);
          list.push({ apelido: v.apelido || v.proposicao, proposicao: v.proposicao || "" });
        }
      });
    });
    return list;
  },

  selectVote(apelido) {
    this.selectedVoteApelido = apelido;
    this.renderToolbar();
    this.renderGrid();
    setTimeout(() => this.generateCanvas(), 60);
  },

  toggleCandidate(candidato) {
    const sq = String(candidato.sq_candidato);
    const idx = this.selectedCandidates.findIndex(c => String(c.sq_candidato) === sq);

    if (idx >= 0) {
      this.selectedCandidates.splice(idx, 1);
    } else {
      if (this.selectedCandidates.length >= 4) {
        alert("Você pode comparar no máximo 4 candidatos simultaneamente.");
        return;
      }
      this.selectedCandidates.push(candidato);
    }

    this.renderBar();
  },

  toggleFromSaved(sq_candidato) {
    const cand = (window.App && typeof App.getCandidateBySq === "function")
      ? App.getCandidateBySq(sq_candidato)
      : null;

    if (cand) {
      this.toggleCandidate(cand);

      // Atualiza os cards de trás no feed
      if (window.App && typeof App.renderView === "function") {
        App.renderView();
      }

      // Atualiza a listagem de Salvos para acender a classe 'active'
      if (window.ListManager && typeof ListManager.renderSavedModal === "function") {
        ListManager.renderSavedModal();
      }
    }
  },

  openFromSaved() {
    this._returnToModal = 'savedListModal';
    this.openModal();
  },

  isSelected(sq_candidato) {
    return this.selectedCandidates.some(c => String(c.sq_candidato) === String(sq_candidato));
  },

  removeCandidate(index) {
    this.selectedCandidates.splice(index, 1);
    this.renderBar();
    if (this.selectedCandidates.length === 0) {
      this.closeModal();
    } else {
      this.renderModalContent();
    }
    if (window.App) window.App.renderView();
  },

  toggleField(fieldKey) {
    this.visibleFields[fieldKey] = !this.visibleFields[fieldKey];
    this.renderToolbar();
    this.renderGrid();
    setTimeout(() => this.generateCanvas(), 60);
  },

  /* ─── Drag and Drop (Arrastar para Reordenar) ────────────────────────── */

  onDragStart(e, idx) {
    this.draggedIdx = idx;
    e.dataTransfer.effectAllowed = "move";
    e.dataTransfer.setData("text/plain", String(idx));
    const cardEl = e.currentTarget;
    setTimeout(() => {
      if (cardEl) cardEl.classList.add("dragging");
    }, 10);
  },

  onDragOver(e, idx) {
    e.preventDefault();
    e.dataTransfer.dropEffect = "move";
    if (this.dragOverIdx !== idx) {
      this.dragOverIdx = idx;
      const cards = document.querySelectorAll("#compareGridContainer .compare-card");
      cards.forEach((c, i) => c.classList.toggle("drag-over", i === idx));
    }
  },

  onDragEnd(e) {
    this.draggedIdx = null;
    this.dragOverIdx = null;
    const cards = document.querySelectorAll("#compareGridContainer .compare-card");
    cards.forEach(c => {
      c.classList.remove("dragging");
      c.classList.remove("drag-over");
    });
  },

  onDrop(e, targetIdx) {
    e.preventDefault();
    if (this.draggedIdx !== null && this.draggedIdx !== targetIdx) {
      const moved = this.selectedCandidates.splice(this.draggedIdx, 1)[0];
      this.selectedCandidates.splice(targetIdx, 0, moved);
    }
    this.draggedIdx = null;
    this.dragOverIdx = null;
    this.renderGrid();
    setTimeout(() => this.generateCanvas(), 60);
  },

  /* ─── Renderização da Interface ─────────────────────────────────────── */

  renderBar() {
    let bar = document.getElementById("compareFloatingBar");
    if (this.selectedCandidates.length === 0) {
      if (bar) bar.style.display = "none";
      return;
    }

    if (!bar) {
      bar = document.createElement("div");
      bar.id = "compareFloatingBar";
      bar.style.cssText = `
        position: fixed; bottom: 20px; left: 50%; transform: translateX(-50%);
        background: var(--bg-surface-elevated, #FFFFFF); border: 1.5px solid var(--border-strong, #121212);
        border-radius: var(--radius-sm, 3px); padding: 0.6rem 1.1rem; z-index: 45;
        box-shadow: 0 4px 14px rgba(0,0,0,0.18); display: flex; align-items: center; gap: 0.85rem;
      `;
      document.body.appendChild(bar);
    }

    bar.style.display = "flex";
    bar.innerHTML = `
      <span style="font-size: 0.82rem; font-weight: 700; color: var(--text-primary); font-family: var(--font-sans);">
        ${this.selectedCandidates.length} selecionado(s) para comparação
      </span>
      <button type="button" class="btn-header primary" style="font-size: 0.75rem; padding: 0.35rem 0.65rem;" onclick="CompareController.openModal()">
        Comparar Lado a Lado ⚖️
      </button>
      <button type="button" class="btn-danger-ghost" onclick="CompareController.clear()">
        Limpar
      </button>
    `;
  },

  clear() {
    this.selectedCandidates = [];
    this.renderBar();
    if (window.App) window.App.renderView();
  },

  renderToolbar() {
    const toolbar = document.getElementById("compareFieldsPills");
    if (!toolbar) return;

    const pillsHtml = this.availableFields.map(f => {
      const active = !!this.visibleFields[f.key];
      return `
        <label class="col-check-item ${active ? 'active' : ''}" style="font-size: 0.74rem; cursor: pointer; display: inline-flex; align-items: center; gap: 0.4rem; padding: 0.35rem 0.65rem; background: var(--bg-page); border: 1px solid ${active ? 'var(--accent-petroleo, #005B68)' : 'var(--border-color)'}; border-radius: var(--radius-xs); user-select: none; transition: var(--transition);">
          <input type="checkbox" ${active ? 'checked' : ''} onchange="CompareController.toggleField('${f.key}')" style="cursor: pointer;">
          <span style="font-weight: ${active ? '700' : '500'}; color: ${active ? 'var(--text-primary)' : 'var(--text-secondary)'};">${f.label}</span>
        </label>
      `;
    }).join("");

    // Seletor interativo para a matéria específica quando 'Votação em Lei' está ativa
    let voteSelectorHtml = "";
    if (this.visibleFields.votacao) {
      const votes = this.getAvailableVotes();
      if (votes.length > 0) {
        if (!this.selectedVoteApelido || !votes.some(v => v.apelido === this.selectedVoteApelido)) {
          this.selectedVoteApelido = votes[0].apelido;
        }
        voteSelectorHtml = `
          <div class="compare-vote-selector-box" style="display: flex; align-items: center; gap: 0.6rem; margin-top: 0.55rem; width: 100%; padding: 0.45rem 0.75rem; background: var(--bg-surface-elevated, #FFFFFF); border: 1.5px solid var(--accent-petroleo, #005B68); border-radius: var(--radius-sm, 4px);">
            <span style="font-size: 0.72rem; font-weight: 800; text-transform: uppercase; color: var(--text-primary); white-space: nowrap; display: flex; align-items: center; gap: 0.35rem;">
              📜 Comparar Votação:
            </span>
            <select class="select-base" style="font-size: 0.78rem; font-weight: 600; padding: 0.25rem 0.5rem; flex: 1; min-width: 180px; max-width: 420px; border: 1px solid var(--border-color); border-radius: 4px;" onchange="CompareController.selectVote(this.value)">
              ${votes.map(v => `
                <option value="${v.apelido.replace(/"/g, '&quot;')}" ${v.apelido === this.selectedVoteApelido ? 'selected' : ''}>
                  ${v.apelido} ${v.proposicao ? `(${v.proposicao})` : ''}
                </option>
              `).join('')}
            </select>
          </div>
        `;
      } else {
        voteSelectorHtml = `
          <div style="font-size: 0.7rem; color: var(--text-muted); font-style: italic; margin-top: 0.4rem; width: 100%;">
            ℹ️ Nenhum dos candidatos selecionados possui votações catalogadas no Radar do Congresso.
          </div>
        `;
      }
    }

    toolbar.innerHTML = `
      <div style="display: flex; gap: 0.4rem; flex-wrap: wrap; width: 100%;">
        ${pillsHtml}
      </div>
      ${voteSelectorHtml}
    `;
  },

  renderGrid() {
    const container = document.getElementById("compareGridContainer");
    if (!container) return;

    const total = this.selectedCandidates.length;

    container.innerHTML = this.selectedCandidates.map((c, idx) => {
      const initials = (c.nome_urna || "C").substring(0, 2).toUpperCase();
      const fotoLocal = c.foto_url || `/fotos/${c.sq_candidato}.jpg`;
      const rankGasto = c.gasto_ranking_texto || (c.ranking_gasto_cargo ? `${c.ranking_gasto_cargo}º de ${c.total_cands_cargo || ''} no cargo (${c.uf || 'BR'})` : '');
      const rankRec = c.receita_ranking_texto || (c.ranking_receita_cargo ? `${c.ranking_receita_cargo}º de ${c.total_cands_cargo || ''} no cargo (${c.uf || 'BR'})` : '');
      const vf = this.visibleFields;

      const vsHtml = idx > 0 ? `
        <div class="compare-vs-divider">
          <span class="compare-vs-badge">VS</span>
        </div>
      ` : '';

      return `
        ${vsHtml}
        <div class="compare-card" draggable="true"
             ondragstart="CompareController.onDragStart(event, ${idx})"
             ondragover="CompareController.onDragOver(event, ${idx})"
             ondragend="CompareController.onDragEnd(event)"
             ondrop="CompareController.onDrop(event, ${idx})"
             style="flex: 1; min-width: 280px; max-width: 340px; background: var(--bg-surface); border: 1px solid var(--border-color); border-top: 3px solid var(--accent-petroleo); border-radius: var(--radius-sm); padding: 0.85rem; display: flex; flex-direction: column; gap: 0.65rem;">
          
          <!-- Topo do Card com Ícone de Arrastar "=" e Fechar -->
          <div style="display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid var(--border-color-subtle); padding-bottom: 0.35rem;">
            <div style="display: flex; align-items: center; gap: 0.4rem;">
              <div class="compare-drag-handle" title="Clique e arraste este cartão para reordenar">=</div >
              <span style="font-size: 0.68rem; font-weight: 700; color: var(--text-muted); text-transform: uppercase;">
                Candidato ${idx + 1} de ${total}
              </span>
            </div>
            <button type="button" class="btn-danger-ghost" style="padding: 0.15rem 0.35rem; font-size: 0.85rem; line-height: 1;" onclick="CompareController.removeCandidate(${idx})" title="Remover da comparação">
              ✕
            </button>
          </div>

          <!-- Identificação do Candidato com Foto Sem Distorção -->
          <div style="display: flex; align-items: center; gap: 0.65rem;">
            <div class="card-photo-box" style="width: 52px; height: 52px; flex-shrink: 0; overflow: hidden; border-radius: 8px; border: none;">
              <img src="${fotoLocal}" class="card-photo-img" style="object-fit: cover; width: 100%; height: 100%;" onerror="if(!this.dataset.triedFallback){this.dataset.triedFallback='1';this.src='/api/candidatos/${c.sq_candidato}/foto';}else{this.style.display='none';this.nextElementSibling.style.display='flex';}">
              <div class="card-photo-initials" style="display:none;">${initials}</div>
            </div>
            <div style="min-width: 0; flex: 1;">
              <div style="font-size: 0.65rem; font-weight: 800; text-transform: uppercase; letter-spacing: 0.05em; color: var(--accent-coral, #C84B31); line-height: 1.2;">${c.cargo} • ${c.uf}</div>
              <div style="font-family: var(--font-serif); font-size: 1.08rem; font-weight: 700; color: var(--text-primary); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; line-height: 1.25; margin-top: 0.1rem;" title="${c.nome_urna}">${c.nome_urna}</div>
            </div>
          </div>

          <div style="display: flex; gap: 0.45rem; align-items: center; flex-wrap: wrap;">
            <span class="party-tag-pill" style="border-left-color: ${c.partido_cor_hex || '#3B82F6'}; font-weight: 800;">${c.partido}</span>
            <div style="display: inline-flex; gap: 3px; align-items: center;" title="Número na urna: ${c.numero || '---'}">
              ${String(c.numero || '---').split('').map(d => `<span style="display: inline-flex; align-items: center; justify-content: center; min-width: 19px; height: 23px; padding: 0 3px; background: var(--bg-surface-elevated, #FFFFFF); border: 1.5px solid var(--border-color, #D8CCC0); border-radius: 3px; font-family: var(--font-mono); font-weight: 800; font-size: 0.82rem; color: var(--text-primary);">${d}</span>`).join('')}
            </div>
          </div>

          <!-- Bloco Condicional: Mandato -->
          ${vf.mandato ? `
            <div style="font-size: 0.75rem; padding: 0.45rem 0.6rem; background: var(--bg-page); border: 1px solid var(--border-color); border-radius: var(--radius-xs); display: flex; justify-content: space-between; align-items: center;">
              <span style="color: var(--text-muted); font-size: 0.68rem; font-weight: 700; text-transform: uppercase;">Mandato</span>
              <span>${c.em_exercicio ? `<span style="color: var(--accent-blue); font-weight: 700;">⚡ ${c.cargo_exercicio || 'Em Exercício'}</span>` : '<span style="color: var(--text-muted);">Não consta</span>'}</span>
            </div>
          ` : ''}

          <!-- Bloco Condicional: Governismo -->
          ${vf.governismo ? (() => {
          const isFed = this.isFederalLegislator(c);
          const hasGov = isFed && c.governismo_pct !== null && c.governismo_pct !== undefined && !isNaN(c.governismo_pct);
          const govVal = hasGov ? Math.round(c.governismo_pct) : null;
          return `
              <div style="font-size: 0.75rem; padding: 0.45rem 0.6rem; background: var(--bg-page); border: 1px solid var(--border-color); border-radius: var(--radius-xs); display: flex; flex-direction: column; gap: 0.25rem;">
                <div style="display: flex; justify-content: space-between; align-items: center;">
                  <span style="color: var(--text-muted); font-size: 0.68rem; font-weight: 700; text-transform: uppercase;">🏛️ Governismo (Radar)</span>
                  <span style="font-family: var(--font-mono); font-weight: 800; font-size: 0.85rem; color: var(--text-primary);">${hasGov ? `${govVal}%` : '—'}</span>
                </div>
                ${hasGov ? `
                  <div style="height: 5px; width: 100%; background: var(--border-color-subtle); border-radius: 3px; overflow: hidden;">
                    <div style="height: 100%; width: ${govVal}%; background: var(--accent-petroleo, #005B68);"></div>
                  </div>
                ` : `<div style="font-size: 0.65rem; color: var(--text-muted); font-style: italic;">${isFed ? 'Sem registro na 57ª Leg.' : 'Sem mandato federal'}</div>`}
              </div>
            `;
        })() : ''}

          <!-- Bloco Condicional: Assiduidade / Presença -->
          ${vf.assiduidade ? (() => {
          const hasAssid = c.assiduidade_pct !== null && c.assiduidade_pct !== undefined && !isNaN(c.assiduidade_pct);
          const assidVal = hasAssid ? Math.round(c.assiduidade_pct) : null;
          return `
              <div style="font-size: 0.75rem; padding: 0.45rem 0.6rem; background: var(--bg-page); border: 1px solid var(--border-color); border-radius: var(--radius-xs); display: flex; flex-direction: column; gap: 0.25rem;">
                <div style="display: flex; justify-content: space-between; align-items: center;">
                  <span style="color: var(--text-muted); font-size: 0.68rem; font-weight: 700; text-transform: uppercase;">⏱️ Presença / Assiduidade</span>
                  <span style="font-family: var(--font-mono); font-weight: 800; font-size: 0.85rem; color: var(--accent-emerald, #0D7680);">${hasAssid ? `${assidVal}%` : '—'}</span>
                </div>
                ${hasAssid ? `
                  <div style="height: 5px; width: 100%; background: var(--border-color-subtle); border-radius: 3px; overflow: hidden;">
                    <div style="height: 100%; width: ${assidVal}%; background: var(--accent-emerald, #0D7680);"></div>
                  </div>
                ` : '<div style="font-size: 0.65rem; color: var(--text-muted); font-style: italic;">Sem registro na 57ª Leg.</div>'}
              </div>
            `;
        })() : ''}

          <!-- Bloco Condicional: Frentes & Bancadas -->
          ${vf.bancadas ? (() => {
          const bancadas = Array.isArray(c.bancadas) ? c.bancadas : [];
          return `
              <div style="font-size: 0.75rem; padding: 0.45rem 0.6rem; background: var(--bg-page); border: 1px solid var(--border-color); border-radius: var(--radius-xs); display: flex; flex-direction: column; gap: 0.3rem;">
                <span style="color: var(--text-muted); font-size: 0.68rem; font-weight: 700; text-transform: uppercase;">🏷️ Frentes & Bancadas</span>
                ${bancadas.length > 0 ? `
                  <div style="display: flex; gap: 0.25rem; flex-wrap: wrap;">
                    ${bancadas.includes('RURALISTA') ? '<span style="font-size: 0.62rem; font-weight: 700; padding: 0.12rem 0.35rem; border-radius: 3px; background: rgba(34, 197, 94, 0.14); color: #15803d;">🌾 Ruralista</span>' : ''}
                    ${(bancadas.includes('BALA') || bancadas.includes('SEGURANCA')) ? '<span style="font-size: 0.62rem; font-weight: 700; padding: 0.12rem 0.35rem; border-radius: 3px; background: rgba(239, 68, 68, 0.14); color: #b91c1c;">🛡️ Bala</span>' : ''}
                    ${bancadas.includes('EVANGELICA') ? '<span style="font-size: 0.62rem; font-weight: 700; padding: 0.12rem 0.35rem; border-radius: 3px; background: rgba(168, 85, 247, 0.14); color: #7e22ce;">⛪ Evangélica</span>' : ''}
                  </div>
                ` : '<div style="font-size: 0.68rem; color: var(--text-muted); font-style: italic;">Nenhuma bancada temática registrada</div>'}
              </div>
            `;
        })() : ''}

          <!-- Bloco Condicional: Votação em Matéria Específica -->
          ${vf.votacao ? (() => {
          const isFed = this.isFederalLegislator(c);
          if (!isFed) {
            return `
              <div style="font-size: 0.75rem; padding: 0.45rem 0.6rem; background: var(--bg-page); border: 1px solid var(--border-color); border-radius: var(--radius-xs); display: flex; flex-direction: column; gap: 0.25rem;">
                <div style="display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid var(--border-color-subtle); padding-bottom: 0.2rem;">
                  <span style="color: var(--text-muted); font-size: 0.65rem; font-weight: 700; text-transform: uppercase;">📜 Voto: ${this.selectedVoteApelido || 'Matéria Federal'}</span>
                </div>
                <div style="margin-top: 0.15rem; display: flex; align-items: center; justify-content: space-between;">
                  <span style="font-size: 0.68rem; color: var(--text-secondary); font-weight: 600;">Posição:</span>
                  <span style="font-size: 0.68rem; color: var(--text-muted); font-style: italic;">Sem mandato federal</span>
                </div>
              </div>
            `;
          }

          const votos = Array.isArray(c.votacoes_radar) ? c.votacoes_radar : (typeof c.votacoes_radar === 'string' ? (() => { try { return JSON.parse(c.votacoes_radar); } catch (e) { return []; } })() : []);
          const targetApelido = this.selectedVoteApelido || (votos[0]?.apelido || "Matéria");
          const match = votos.find(v => (v.apelido || v.proposicao) === targetApelido);

          let voteBadge = '<span style="font-size: 0.68rem; color: var(--text-muted); font-style: italic;">Não constava em exercício</span>';
          if (match) {
            const rawV = String(match.voto || '').trim().toUpperCase();
            const isSim = rawV === 'SIM';
            const isNao = rawV === 'NAO' || rawV === 'NÃO';
            const isObs = rawV === 'OBSTRUCAO' || rawV === 'OBSTRUÇÃO';
            const isAbs = rawV === 'ABSTENCAO' || rawV === 'ABSTENÇÃO';
            const isArt17 = rawV === 'ARTIGO 17';
            const isAusente = rawV === 'AUSENTE';

            const badgeStyle = isSim
              ? 'background: #ECFDF5; color: #065F46; border: 1px solid #A7F3D0;'
              : (isNao
                ? 'background: #FEF2F2; color: #991B1B; border: 1px solid #FECACA;'
                : (isObs || isAbs || isArt17
                  ? 'background: #FFFBEB; color: #92400E; border: 1px solid #FDE68A;'
                  : 'background: var(--bg-surface); color: var(--text-secondary); border: 1px solid var(--border-color);'));

            const icon = isSim ? '✓' : (isNao ? '✗' : (isObs || isAbs ? '⚠' : '—'));
            const textLabel = isSim ? 'Votou SIM' : (isNao ? 'Votou NÃO' : (isObs ? 'Obstrução' : (isAbs ? 'Abstenção' : (isArt17 ? 'Artigo 17' : (isAusente ? 'Ausente' : match.voto)))));
            voteBadge = `<span style="font-size: 0.72rem; font-weight: 800; padding: 0.15rem 0.55rem; border-radius: 4px; font-family: var(--font-mono); ${badgeStyle}">${icon} ${textLabel}</span>`;
          }

          return `
              <div style="font-size: 0.75rem; padding: 0.45rem 0.6rem; background: var(--bg-page); border: 1px solid var(--border-color); border-radius: var(--radius-xs); display: flex; flex-direction: column; gap: 0.25rem;">
                <div style="display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid var(--border-color-subtle); padding-bottom: 0.2rem;">
                  <span style="color: var(--text-muted); font-size: 0.65rem; font-weight: 700; text-transform: uppercase;">📜 Voto: ${targetApelido}</span>
                </div>
                <div style="margin-top: 0.15rem; display: flex; align-items: center; justify-content: space-between;">
                  <span style="font-size: 0.68rem; color: var(--text-secondary); font-weight: 600;">Posição:</span>
                  ${voteBadge}
                </div>
              </div>
            `;
        })() : ''}

          <!-- Bloco Condicional: Ideologia -->
          ${vf.ideologia ? `
            <div style="font-size: 0.75rem; padding: 0.45rem 0.6rem; background: var(--bg-page); border: 1px solid var(--border-color); border-radius: var(--radius-xs); display: flex; justify-content: space-between; align-items: center;">
              <span style="color: var(--text-muted); font-size: 0.68rem; font-weight: 700; text-transform: uppercase;">Ideologia do Partido</span>
              <span style="font-weight: 800; color: var(--text-primary);">${c.ideologia_nome || 'Sem Classificação'}</span>
            </div>
          ` : ''}

          <!-- Lista de Atributos Customizáveis (Finanças, Ranking e Dados Pessoais) -->
          <div style="font-size: 0.75rem; color: var(--text-secondary); display: flex; flex-direction: column; gap: 0.45rem; background: var(--bg-page); padding: 0.65rem; border: 1px solid var(--border-color); border-radius: var(--radius-xs);">
            
            ${vf.receitas ? (() => {
          const recVal = (c.financiamento_receita || c.receita_total || 0);
          return `
                <div style="display: flex; justify-content: space-between; gap: 0.5rem; border-bottom: 1px solid var(--border-color-subtle); padding-bottom: 0.35rem;">
                  <span style="color: var(--text-muted);">Receita Arrecadada:</span>
                  <span style="font-family: var(--font-mono); font-weight: 600; color: var(--text-primary); text-align: right;">
                    ${recVal > 0 ? `R$ ${(recVal).toLocaleString('pt-BR', { minimumFractionDigits: 2 })} ${rankRec ? `<small style="display:block; font-size:0.62rem; color:var(--text-muted);">(${rankRec})</small>` : ''}` : 'Não declarada'}
                  </span>
                </div>
              `;
        })() : ''}

            ${vf.gastos ? `
              <div style="display: flex; justify-content: space-between; gap: 0.5rem; border-bottom: 1px solid var(--border-color-subtle); padding-bottom: 0.35rem;">
                <span style="color: var(--text-muted);">Gastos Declarados:</span>
                <span style="font-family: var(--font-mono); font-weight: 600; color: var(--text-primary); text-align: right;">
                  ${c.financiamento_despesa > 0 ? `R$ ${(c.financiamento_despesa).toLocaleString('pt-BR', { minimumFractionDigits: 2 })} ${rankGasto ? `<small style="display:block; font-size:0.62rem; color:var(--text-muted);">(${rankGasto})</small>` : ''}` : 'Não declarado'}
                </span>
              </div>
            ` : ''}

            ${vf.ranking_partido ? (() => {
          const rankNum = c.ranking_partido_receita || c.ranking_partido_despesa;
          const totPart = c.total_partido_cargo_uf;
          return `
                <div style="display: flex; justify-content: space-between; gap: 0.5rem; border-bottom: 1px solid var(--border-color-subtle); padding-bottom: 0.35rem;">
                  <span style="color: var(--text-muted);">Rank no Partido:</span>
                  <span style="font-weight: 700; color: var(--accent-petroleo, #005B68); text-align: right; font-size: 0.72rem;">
                    ${rankNum ? `🎯 ${rankNum}º ${totPart ? `de ${totPart}` : ''} mais financiado (${c.partido})` : 'Candidatura única / Não aplicável'}
                  </span>
                </div>
              `;
        })() : ''}

            ${vf.ranking_geral ? (() => {
          const rankRecNum = c.ranking_receita_cargo;
          const rankGasNum = c.ranking_gasto_cargo;
          const totCargo = c.total_cands_cargo;
          return `
                <div style="display: flex; justify-content: space-between; gap: 0.5rem; border-bottom: 1px solid var(--border-color-subtle); padding-bottom: 0.35rem;">
                  <span style="color: var(--text-muted);">Rank Geral (${c.uf || 'BR'}):</span>
                  <span style="font-weight: 600; color: var(--text-primary); text-align: right; font-size: 0.7rem;">
                    ${(rankRecNum || rankGasNum) ? `Receitas: ${rankRecNum ? `${rankRecNum}º` : '—'} • Gastos: ${rankGasNum ? `${rankGasNum}º` : '—'}${totCargo ? ` (de ${totCargo})` : ''}` : 'Não classificado'}
                  </span>
                </div>
              `;
        })() : ''}

            ${vf.bens ? `
              <div style="display: flex; justify-content: space-between; gap: 0.5rem; border-bottom: 1px solid var(--border-color-subtle); padding-bottom: 0.35rem;">
                <span style="color: var(--text-muted);">Bens Declarados:</span>
                <span style="font-family: var(--font-mono); font-weight: 600; color: var(--accent-emerald, #0D7680);">
                  ${c.total_bens > 0 ? `R$ ${(c.total_bens).toLocaleString('pt-BR', { minimumFractionDigits: 2 })}` : 'R$ 0,00'}
                </span>
              </div>
            ` : ''}

            ${vf.instrucao ? `
              <div style="display: flex; justify-content: space-between; gap: 0.5rem; border-bottom: 1px solid var(--border-color-subtle); padding-bottom: 0.35rem;">
                <span style="color: var(--text-muted);">Escolaridade:</span>
                <span style="color: var(--text-primary); font-weight: 500; text-align: right;">${c.grau_instrucao || 'Não informada'}</span>
              </div>
            ` : ''}

            ${vf.profissao ? `
              <div style="display: flex; justify-content: space-between; gap: 0.5rem; border-bottom: 1px solid var(--border-color-subtle); padding-bottom: 0.35rem;">
                <span style="color: var(--text-muted);">Ocupação:</span>
                <span style="color: var(--text-primary); font-weight: 500; text-align: right;">${c.ocupacao || 'Não informada'}</span>
              </div>
            ` : ''}

            ${vf.demografia ? `
              <div style="display: flex; justify-content: space-between; gap: 0.5rem;">
                <span style="color: var(--text-muted);">Gênero / Cor:</span>
                <span style="color: var(--text-primary); font-weight: 500;">${c.genero === 'FEMININO' ? 'Mulher' : (c.genero === 'MASCULINO' ? 'Homem' : (c.genero || '---'))} • ${c.cor_raca || '---'}</span>
              </div>
            ` : ''}

          </div>

          <button type="button" class="btn-card-action" style="margin-top: auto; padding: 0.45rem;" onclick="CompareController.openCandidateDetails('${c.sq_candidato}')">
            Abrir Ficha Completa ↗
          </button>
        </div>
      `;
    }).join("");
  },

  renderModalContent() {
    this.renderToolbar();
    this.renderGrid();
  },

  openModal() {
    if (window.App && typeof App.closeMobileSidebar === 'function') {
      App.closeMobileSidebar();
    }
    const modal = document.getElementById("compareModal");
    if (!modal) return;
    this.renderModalContent();

    const btnBack = document.getElementById("compareBtnBackToSaved");
    if (btnBack) {
      btnBack.style.display = (this._returnToModal === 'savedListModal') ? 'inline-flex' : 'none';
    }

    modal.classList.add("open");
    modal.style.display = "flex"; // Força a exibição sobrepondo qualquer conflito CSS
    setTimeout(() => {
      this.generateCanvas();
    }, 100);
  },

  openCandidateDetails(sq) {
    if (window.App && typeof App.openDetails === 'function') {
      App._returnToModal = 'compareModal';
      const modal = document.getElementById("compareModal");
      if (modal) {
        modal.classList.remove("open");
        modal.style.display = "none";
      }
      App.openDetails(sq);
    }
  },

  closeModal() {
    const modal = document.getElementById("compareModal");
    if (modal) {
      modal.classList.remove("open");
      modal.style.display = "none";
    }
    if (this._returnToModal === 'savedListModal') {
      const returnTarget = this._returnToModal;
      this._returnToModal = null;
      if (window.ListManager && typeof ListManager.openSavedModal === 'function') {
        ListManager.openSavedModal();
      }
    }
  },

  /* ─── Compartilhamento Social Direto ────────────────────────────────── */

  async share(network) {
    if (this.selectedCandidates.length === 0) return;
    const canvas = this.lastExportCanvas || await this.generateCanvas();
    const names = this.selectedCandidates.map(c => c.nome_urna).join(", ");
    const text = `Comparei lado a lado ${names} para as Eleições 2026 no Em Quem Eu Voto!`;
    const filename = "comparativo_candidatos_2026.png";

    if (network === "whatsapp") {
      ShareHelper.shareWhatsApp(text, null, canvas, filename);
    } else if (network === "twitter" || network === "x") {
      ShareHelper.shareTwitter(text, null, canvas, filename);
    } else if (network === "facebook") {
      ShareHelper.shareFacebook(null, text, canvas, filename);
    } else if (network === "instagram") {
      ShareHelper.shareInstagram("Quadro Comparativo 2026", text, canvas, filename);
    } else {
      ShareHelper.shareNative("Quadro Comparativo 2026", text, canvas);
    }
  },

  async copyImage() {
    const canvas = this.lastExportCanvas || await this.generateCanvas();
    if (!canvas) return;
    const ok = await ShareHelper.copyImageToClipboard(canvas);
    if (window.App && typeof App.showToast === "function") {
      App.showToast(ok ? "📋 Quadro comparativo copiado para a área de transferência!" : "⚠️ Não foi possível copiar a imagem automaticamente. Use o botão Salvar.");
    }
  },

  async exportImage() {
    const canvas = this.lastExportCanvas || await this.generateCanvas();
    if (!canvas) return;

    const filename = `comparativo_candidatos_${Date.now()}.png`;
    const link = document.createElement("a");
    link.download = filename;
    link.href = canvas.toDataURL("image/png");
    link.click();
    if (window.App && typeof App.showToast === "function") {
      App.showToast("Quadro comparativo exportado em alta resolução!");
    }
  },

  async generateCanvas() {
    const cands = this.selectedCandidates;
    if (cands.length === 0) return null;

    const isLight = document.documentElement.getAttribute("data-theme") !== "dark" &&
      !(document.body && document.body.classList.contains("dark-mode"));

    // Cores do Design System
    const themeColors = (window.getThemeColors ? window.getThemeColors(isLight ? "paper" : "slate") : null) || {
      isDark: !isLight,
      bgPage: isLight ? "#FAF7F2" : "#121417",
      bgCard: isLight ? "#FFFFFF" : "#1A1E24",
      bgCardAlt: isLight ? "#F6EFEA" : "#222831",
      borderCard: isLight ? "#E4DDD3" : "#30363F",
      borderRule: isLight ? "#C5B8AA" : "#454E5B",
      textPrimary: isLight ? "#111418" : "#F5E5D5",
      textSecondary: isLight ? "#38312B" : "#A7A19B",
      textMuted: isLight ? "#6B625B" : "#736C65",
      kickerColor: isLight ? "#C84B31" : "#EF5B4D",
      accentColor: isLight ? "#005B68" : "#00AFA5"
    };

    const count = cands.length;

    // Escalonamento Dinâmico de Dimensões e Fontes (quanto menos candidatos, maiores os elementos)
    let cardW = 340;
    let cardGap = 18;
    let padX = 44;
    let photoSize = 82;
    let scale = 1.0;

    if (count === 1) {
      cardW = 680;
      cardGap = 0;
      padX = 60;
      photoSize = 112;
      scale = 1.35;
    } else if (count === 2) {
      cardW = 520;
      cardGap = 28;
      padX = 50;
      photoSize = 100;
      scale = 1.22;
    } else if (count === 3) {
      cardW = 400;
      cardGap = 20;
      padX = 44;
      photoSize = 90;
      scale = 1.1;
    } else {
      cardW = 340;
      cardGap = 16;
      padX = 40;
      photoSize = 80;
      scale = 0.98;
    }

    const totalW = Math.max(1280, (padX * 2) + (count * cardW) + ((count - 1) * cardGap));

    // Calcular altura necessária com base nos campos visíveis
    const vf = this.visibleFields;
    let attrItemsCount = 0;
    if (vf.mandato) attrItemsCount++;
    if (vf.governismo) attrItemsCount++;
    if (vf.assiduidade) attrItemsCount++;
    if (vf.bancadas) attrItemsCount++;
    if (vf.votacao) attrItemsCount++;
    if (vf.ideologia) attrItemsCount++;
    if (vf.receitas) attrItemsCount++;
    if (vf.gastos) attrItemsCount++;
    if (vf.ranking_partido) attrItemsCount++;
    if (vf.ranking_geral) attrItemsCount++;
    if (vf.bens) attrItemsCount++;
    if (vf.instrucao) attrItemsCount++;
    if (vf.profissao) attrItemsCount++;
    if (vf.demografia) attrItemsCount++;

    const rowItemH = Math.round(72 * scale);
    const cardHeaderH = Math.round(180 * scale);
    const cardAttrsH = Math.max(160, Math.round(attrItemsCount * (rowItemH + 8) + 24));
    const cardH = cardHeaderH + cardAttrsH;
    const headerTopH = 135;
    const footerH = 90;
    const totalH = headerTopH + cardH + footerH + 30;

    const canvas = document.createElement("canvas");
    canvas.width = totalW;
    canvas.height = totalH;
    const ctx = canvas.getContext("2d");

    // Fundo Editorial Sólido e Borda
    ctx.fillStyle = themeColors.bgPage;
    ctx.fillRect(0, 0, totalW, totalH);

    ctx.strokeStyle = themeColors.borderCard;
    ctx.lineWidth = 1.5;
    ctx.strokeRect(18, 18, totalW - 36, totalH - 36);

    // Pré-carregamento do Logo Oficial do Site (transparente, sem fundo)
    const logoPromise = new Promise((resolve) => {
      const img = (window.getSiteLogoImage ? window.getSiteLogoImage() : null) || new Image();
      if (img.complete && img.naturalWidth > 0) return resolve(img);
      img.onload = () => resolve(img);
      img.onerror = () => resolve(null);
      if (!img.src) img.src = "/img/logo_emquemeuvoto_nobg.png";
    });

    // Pré-carregamento das fotos dos candidatos
    const loadPhoto = (cand) => {
      return new Promise((resolve) => {
        const img = new Image();
        img.crossOrigin = "anonymous";
        img.src = cand.foto_url || `/api/candidatos/${cand.sq_candidato}/foto`;
        img.onload = () => resolve(img);
        img.onerror = () => resolve(null);
      });
    };

    const [siteLogo, ...photos] = await Promise.all([logoPromise, ...cands.map(loadPhoto)]);

    // 1. Cabeçalho Editorial com Lockup da Marca e Logo Oficial
    const headerTopY = 36;
    ctx.save();
    ctx.textAlign = "left";
    ctx.textBaseline = "top";
    ctx.fillStyle = themeColors.isDark ? "#FFFFFF" : "#0A2530";
    ctx.font = "900 22px 'Plus Jakarta Sans', sans-serif";
    ctx.fillText("EM QUEM", padX, headerTopY);

    const badgeY = headerTopY + 28;
    const badgeW = 104;
    const badgeH = 26;
    ctx.fillStyle = themeColors.kickerColor || "#C84B31";
    ctx.beginPath();
    ctx.roundRect ? ctx.roundRect(padX, badgeY, badgeW, badgeH, 4) : ctx.rect(padX, badgeY, badgeW, badgeH);
    ctx.fill();

    ctx.fillStyle = "#FFFFFF";
    ctx.font = "900 15px 'Plus Jakarta Sans', sans-serif";
    ctx.textAlign = "center";
    ctx.fillText("EU VOTO", padX + badgeW / 2, badgeY + 5);
    ctx.restore();

    const dividerX = padX + badgeW + 22;
    ctx.strokeStyle = themeColors.borderCard;
    ctx.lineWidth = 1.2;
    ctx.beginPath();
    ctx.moveTo(dividerX, headerTopY + 2);
    ctx.lineTo(dividerX, headerTopY + 54);
    ctx.stroke();

    // Título Principal "QUADRO COMPARATIVO"
    ctx.save();
    ctx.textAlign = "left";
    ctx.textBaseline = "top";
    ctx.fillStyle = themeColors.textPrimary;
    ctx.font = "bold 34px 'DM Serif Display', Georgia, serif";
    ctx.letterSpacing = "0.02em";
    ctx.fillText("QUADRO COMPARATIVO", dividerX + 22, headerTopY - 2);

    const ufStr = cands[0]?.uf ? ` • ${cands[0].uf}` : "";
    ctx.fillStyle = themeColors.textMuted;
    ctx.font = "bold 13px 'Plus Jakarta Sans', sans-serif";
    ctx.letterSpacing = "0.08em";
    ctx.fillText(`ELEIÇÕES 2026${ufStr}  —  DADOS OFICIAIS DO TSE`, dividerX + 22, headerTopY + 38);
    ctx.restore();

    // Logo Oficial do Projeto no topo direito (sem fundo ou disco)
    const logoH = 60;
    const logoW = Math.round(logoH * (1107 / 1221));
    const logoX = totalW - padX - logoW;
    const logoY = headerTopY - 2;

    if (siteLogo && siteLogo.complete && siteLogo.naturalWidth > 0) {
      ctx.drawImage(siteLogo, logoX, logoY, logoW, logoH);
    }

    // Linha de régua do cabeçalho
    const headerRuleY = headerTopY + 70;
    ctx.strokeStyle = themeColors.borderCard;
    ctx.lineWidth = 1.2;
    ctx.beginPath();
    ctx.moveTo(padX, headerRuleY);
    ctx.lineTo(totalW - padX, headerRuleY);
    ctx.stroke();

    // 2. Renderização das Colunas dos Candidatos
    const cardY = headerRuleY + 20;
    const startX = (totalW - (count * cardW + (count - 1) * cardGap)) / 2;

    const fontDisplay = "'DM Serif Display', Georgia, serif";

    cands.forEach((c, i) => {
      const x = startX + i * (cardW + cardGap);
      const partyColor = c.partido_cor_hex || themeColors.accentColor;

      // Fundo do Card
      ctx.fillStyle = themeColors.bgCard;
      ctx.beginPath();
      ctx.roundRect ? ctx.roundRect(x, cardY, cardW, cardH, 8) : ctx.rect(x, cardY, cardW, cardH);
      ctx.fill();

      ctx.strokeStyle = themeColors.borderCard;
      ctx.lineWidth = 1.2;
      ctx.stroke();

      // Linha de Topo com a Cor do Partido (4px)
      ctx.fillStyle = partyColor;
      ctx.fillRect(x, cardY, cardW, 4);

      // Foto do Candidato
      const photoX = x + 16;
      const photoY = cardY + 16;
      const img = photos[i];

      if (img) {
        ctx.save();
        ctx.beginPath();
        ctx.roundRect ? ctx.roundRect(photoX, photoY, photoSize, photoSize, 6) : ctx.rect(photoX, photoY, photoSize, photoSize);
        ctx.clip();

        const imgW = img.naturalWidth || img.width;
        const imgH = img.naturalHeight || img.height;
        let sx = 0, sy = 0, sw = imgW, sh = imgH;
        if (imgW > imgH) {
          sw = imgH;
          sx = (imgW - imgH) / 2;
        } else if (imgH > imgW) {
          sh = imgW;
          sy = Math.max(0, Math.min(imgH - sh, (imgH - imgW) * 0.22));
        }

        ctx.drawImage(img, sx, sy, sw, sh, photoX, photoY, photoSize, photoSize);
        ctx.restore();

        ctx.strokeStyle = themeColors.borderCard;
        ctx.lineWidth = 1.2;
        ctx.beginPath();
        ctx.roundRect ? ctx.roundRect(photoX, photoY, photoSize, photoSize, 6) : ctx.rect(photoX, photoY, photoSize, photoSize);
        ctx.stroke();
      } else {
        ctx.fillStyle = themeColors.bgCardAlt;
        ctx.beginPath();
        ctx.roundRect ? ctx.roundRect(photoX, photoY, photoSize, photoSize, 6) : ctx.rect(photoX, photoY, photoSize, photoSize);
        ctx.fill();
        ctx.fillStyle = themeColors.textPrimary;
        ctx.font = `bold ${Math.round(28 * scale)}px ${fontDisplay}`;
        ctx.textAlign = "center";
        ctx.textBaseline = "middle";
        ctx.fillText((c.nome_urna || "C").substring(0, 2).toUpperCase(), photoX + photoSize / 2, photoY + photoSize / 2);
      }

      // Detalhes ao lado da foto
      const infoX = photoX + photoSize + 14;
      const availInfoW = cardW - (photoSize + 44);

      // Nome do candidato
      ctx.textAlign = "left";
      ctx.textBaseline = "top";
      ctx.fillStyle = themeColors.textPrimary;
      const nameFontSize = Math.round(22 * scale);
      ctx.font = `bold ${nameFontSize}px ${fontDisplay}`;

      const rawName = String(c.nome_urna || c.nome_completo || "Candidato");
      let displayName = rawName;
      if (ctx.measureText(displayName).width > availInfoW) {
        const words = rawName.split(" ");
        displayName = words[0] + (words[1] ? ` ${words[1]}` : "");
        if (ctx.measureText(displayName).width > availInfoW) {
          displayName = words[0];
        }
      }
      ctx.fillText(displayName, infoX, photoY + 2);

      // Partido & Ideologia Pill
      const partyStr = c.partido || "---";
      const ideoStr = c.ideologia_nome || "Centro";

      ctx.save();
      ctx.font = `bold ${Math.round(13 * scale)}px 'Plus Jakarta Sans', sans-serif`;
      ctx.fillStyle = themeColors.textPrimary;
      ctx.fillText(partyStr, infoX, photoY + Math.round(30 * scale));

      // Ideologia Pill
      const ideoPillX = infoX + ctx.measureText(partyStr).width + 10;
      ctx.font = `bold ${Math.round(10.5 * scale)}px 'Plus Jakarta Sans', sans-serif`;
      const ideoW = Math.round(ctx.measureText(ideoStr).width + 14);
      const ideoH = Math.round(18 * scale);
      const ideoY = photoY + Math.round(30 * scale) - 1;

      ctx.fillStyle = themeColors.bgCardAlt;
      ctx.beginPath();
      ctx.roundRect ? ctx.roundRect(ideoPillX, ideoY, ideoW, ideoH, 4) : ctx.rect(ideoPillX, ideoY, ideoW, ideoH);
      ctx.fill();

      ctx.strokeStyle = themeColors.borderCard;
      ctx.lineWidth = 1;
      ctx.stroke();

      ctx.fillStyle = themeColors.kickerColor;
      ctx.textAlign = "center";
      ctx.textBaseline = "middle";
      ctx.fillText(ideoStr, ideoPillX + ideoW / 2, ideoY + ideoH / 2 + 1);
      ctx.restore();

      // Número na Urna (Quadradinhos)
      const numBoxY = photoY + Math.round(54 * scale);
      const numDigits = String(c.numero || "---").split("");
      const numBoxW = Math.round(24 * scale);
      const numBoxH = Math.round(32 * scale);
      const numBoxGap = 4;

      numDigits.forEach((d, dIdx) => {
        const bx = infoX + (dIdx * (numBoxW + numBoxGap));
        ctx.fillStyle = themeColors.bgCard;
        ctx.beginPath();
        ctx.roundRect ? ctx.roundRect(bx, numBoxY, numBoxW, numBoxH, 3) : ctx.rect(bx, numBoxY, numBoxW, numBoxH);
        ctx.fill();

        ctx.strokeStyle = themeColors.borderCard;
        ctx.lineWidth = 1.2;
        ctx.stroke();

        ctx.fillStyle = themeColors.textPrimary;
        ctx.font = `bold ${Math.round(18 * scale)}px 'JetBrains Mono', monospace`;
        ctx.textAlign = "center";
        ctx.textBaseline = "middle";
        ctx.fillText(d, bx + numBoxW / 2, numBoxY + numBoxH / 2 + 1);
      });

      // Divisória do Header para os Atributos
      let rowY = cardY + cardHeaderH;
      ctx.strokeStyle = themeColors.borderCard;
      ctx.lineWidth = 1;
      ctx.beginPath();
      ctx.moveTo(x + 14, rowY);
      ctx.lineTo(x + cardW - 14, rowY);
      ctx.stroke();
      rowY += 12;

      // Renderizador de Atributo Individual
      const renderAttr = (label, val, valColor = themeColors.textPrimary, isMono = false, subtext = null) => {
        const itemH = rowItemH;
        ctx.fillStyle = themeColors.bgCardAlt;
        ctx.beginPath();
        ctx.roundRect ? ctx.roundRect(x + 12, rowY, cardW - 24, itemH, 6) : ctx.rect(x + 12, rowY, cardW - 24, itemH);
        ctx.fill();

        ctx.strokeStyle = themeColors.borderCard;
        ctx.lineWidth = 1;
        ctx.stroke();

        ctx.save();
        ctx.fillStyle = themeColors.textMuted;
        ctx.font = `bold ${Math.round(11 * scale)}px 'Plus Jakarta Sans', sans-serif`;
        ctx.letterSpacing = "0.04em";
        ctx.textAlign = "left";
        ctx.textBaseline = "top";
        ctx.fillText(label.toUpperCase(), x + 20, rowY + Math.round(10 * scale));

        ctx.fillStyle = valColor;
        ctx.font = isMono ? `bold ${Math.round(16 * scale)}px 'JetBrains Mono', monospace` : `bold ${Math.round(15 * scale)}px 'Plus Jakarta Sans', sans-serif`;
        let valStr = String(val);
        if (ctx.measureText(valStr).width > cardW - 44) {
          valStr = valStr.substring(0, 26) + "...";
        }
        ctx.fillText(valStr, x + 20, rowY + Math.round(30 * scale));

        if (subtext) {
          ctx.fillStyle = themeColors.textMuted;
          ctx.font = `bold ${Math.round(11 * scale)}px 'Plus Jakarta Sans', sans-serif`;
          ctx.fillText(subtext, x + 20, rowY + Math.round(48 * scale));
        }
        ctx.restore();

        rowY += itemH + 8;
      };

      if (vf.mandato) {
        const mandInfo = (window.App && App.getCandidateMandatoInfo) ? App.getCandidateMandatoInfo(c) : null;
        let mandVal = "Não consta";
        let subVal = null;
        if (mandInfo) {
          mandVal = mandInfo.label;
          if (mandInfo.sublabel) subVal = mandInfo.sublabel;
        } else {
          const shortCargo = (c.cargo_exercicio || "")
            .replace(/Presidente da República/i, "Presidente")
            .replace(/Governador.*Estado/i, "Governador")
            .replace(/Senador.*Federal/i, "Senador")
            .trim();
          mandVal = c.is_reeleicao ? `⚡ ${shortCargo || c.cargo}` : (c.em_exercicio ? (shortCargo || 'Em exercício') : "Não consta");
          subVal = c.em_exercicio ? "Em exercício (2023–2026)" : null;
        }
        renderAttr("Mandato Atual", mandVal, c.em_exercicio ? themeColors.textPrimary : themeColors.textMuted, false, subVal);
      }

      if (vf.ideologia) {
        renderAttr("Ideologia do Partido", c.ideologia_nome || 'Centro', themeColors.textPrimary);
      }

      if (vf.votacao) {
        const isFed = this.isFederalLegislator(c);
        const targetApelido = this.selectedVoteApelido || "Votação";
        let val = "Sem mandato federal";
        let col = themeColors.textMuted;
        if (isFed) {
          const votos = Array.isArray(c.votacoes_radar) ? c.votacoes_radar : (typeof c.votacoes_radar === 'string' ? (() => { try { return JSON.parse(c.votacoes_radar); } catch (e) { return []; } })() : []);
          const match = votos.find(v => (v.apelido || v.proposicao) === (this.selectedVoteApelido || (votos[0]?.apelido || "Votação")));
          if (match) {
            const rawV = String(match.voto || '').trim().toUpperCase();
            if (rawV === 'SIM') {
              val = 'Votou SIM';
              col = '#059669';
            } else if (rawV === 'NAO' || rawV === 'NÃO') {
              val = 'Votou NÃO';
              col = '#DC2626';
            } else if (rawV === 'OBSTRUCAO' || rawV === 'OBSTRUÇÃO') {
              val = 'Obstrução';
              col = '#D97706';
            } else if (rawV === 'ABSTENCAO' || rawV === 'ABSTENÇÃO') {
              val = 'Abstenção';
              col = '#D97706';
            } else if (rawV === 'ARTIGO 17') {
              val = 'Artigo 17';
              col = '#6B7280';
            } else if (rawV === 'AUSENTE') {
              val = 'Ausente na sessão';
              col = themeColors.textMuted;
            } else {
              val = `Votou ${match.voto}`;
              col = themeColors.textPrimary;
            }
          } else {
            val = "Não constava em exercício";
          }
        }
        renderAttr(`Votações: ${targetApelido}`.substring(0, 26), val, col);
      }

      if (vf.receitas) {
        const rankRec = c.ranking_receita_cargo ? ` (${c.ranking_receita_cargo}º)` : '';
        const recVal = (c.financiamento_receita || c.receita_total || 0);
        const strVal = recVal > 0 ? `R$ ${(recVal).toLocaleString('pt-BR', { minimumFractionDigits: 2 })}${rankRec}` : "Não declarada";
        renderAttr("Receita Arrecadada", strVal, themeColors.textPrimary, true, "Total declarado TSE");
      }

      if (vf.gastos) {
        const rankGasto = c.ranking_gasto_cargo ? ` (${c.ranking_gasto_cargo}º)` : '';
        const gastVal = c.financiamento_despesa > 0 ? `R$ ${(c.financiamento_despesa).toLocaleString('pt-BR', { minimumFractionDigits: 2 })}${rankGasto}` : "Não declarado";
        renderAttr("Gastos de Campanha", gastVal, themeColors.textPrimary, true, "Despesas pagas");
      }

      if (vf.bens) {
        const bensVal = c.total_bens > 0 ? `R$ ${(c.total_bens).toLocaleString('pt-BR', { minimumFractionDigits: 2 })}` : "R$ 0,00";
        renderAttr("Bens Declarados", bensVal, themeColors.textPrimary, true, "Total em bens (TSE)");
      }

      if (vf.assiduidade) {
        const isFed = this.isFederalLegislator(c);
        const hasAssid = isFed && c.assiduidade_pct !== null && c.assiduidade_pct !== undefined && !isNaN(c.assiduidade_pct);
        const val = hasAssid ? `${Math.round(c.assiduidade_pct)}% de presença` : (isFed ? "Sem registro recente" : "Não se aplica");
        renderAttr("Presença / Assiduidade", val, hasAssid ? (themeColors.isDark ? "#10B981" : "#0D7680") : themeColors.textMuted);
      }

      if (vf.governismo) {
        const isFed = this.isFederalLegislator(c);
        const hasGov = isFed && c.governismo_pct !== null && c.governismo_pct !== undefined && !isNaN(c.governismo_pct);
        const val = hasGov ? `${Math.round(c.governismo_pct)}% alinhamento` : (isFed ? "Sem registro" : "Sem mandato federal");
        renderAttr("Governismo", val, hasGov ? themeColors.accentColor : themeColors.textMuted, hasGov);
      }

      if (vf.bancadas) {
        const isFed = this.isFederalLegislator(c);
        const bancArr = isFed && Array.isArray(c.bancadas) ? c.bancadas : [];
        let val = isFed ? "Nenhuma bancada" : "Sem mandato federal";
        if (bancArr.length > 0) {
          val = bancArr.map(b => b === 'RURALISTA' ? 'Rural' : ((b === 'BALA' || b === 'SEGURANCA') ? 'Bala' : 'Evangélica')).join(' • ');
        }
        renderAttr("Frentes & Bancadas", val, bancArr.length > 0 ? themeColors.textPrimary : themeColors.textMuted);
      }

      if (vf.ranking_partido) {
        const rankNum = c.ranking_partido_receita || c.ranking_partido_despesa;
        const totPart = c.total_partido_cargo_uf;
        const partVal = rankNum ? `🎯 ${rankNum}º ${totPart ? `de ${totPart}` : ''} no ${c.partido}` : "Não aplicável";
        renderAttr("Rank no Partido", partVal, rankNum ? themeColors.accentColor : themeColors.textMuted);
      }

      if (vf.ranking_geral) {
        const rankRecNum = c.ranking_receita_cargo;
        const rankGasNum = c.ranking_gasto_cargo;
        const genVal = (rankRecNum || rankGasNum) ? `Rec: ${rankRecNum || '—'}º • Desp: ${rankGasNum || '—'}º` : "Não classificado";
        renderAttr(`Rank Geral (${c.uf || 'BR'})`, genVal, themeColors.textPrimary);
      }

      if (vf.instrucao) {
        renderAttr("Escolaridade", c.grau_instrucao || "Não informada", themeColors.textPrimary);
      }

      if (vf.profissao) {
        renderAttr("Profissão / Ocupação", c.ocupacao || "Não informada", themeColors.textPrimary);
      }

      if (vf.demografia) {
        const demoVal = `${c.genero === 'FEMININO' ? 'Mulher' : (c.genero === 'MASCULINO' ? 'Homem' : (c.genero || '---'))} • ${c.cor_raca || '---'}`;
        renderAttr("Gênero / Cor", demoVal, themeColors.textPrimary);
      }
    });

    // 3. Rodapé Editorial
    const footRuleY = totalH - 58;
    ctx.strokeStyle = themeColors.borderCard;
    ctx.lineWidth = 1.2;
    ctx.beginPath();
    ctx.moveTo(padX, footRuleY);
    ctx.lineTo(totalW - padX, footRuleY);
    ctx.stroke();

    const footY = totalH - 28;
    ctx.save();
    // Esquerda: "Por um voto bem informado."
    ctx.textAlign = "left";
    ctx.textBaseline = "middle";
    ctx.fillStyle = themeColors.textPrimary;
    ctx.font = "italic bold 15px 'DM Serif Display', Georgia, serif";
    ctx.fillText("Por um voto bem informado.", padX, footY);

    // Direita: EM QUEM EU VOTO / emquemeuvoto.onrender.com
    ctx.textAlign = "right";
    ctx.fillStyle = themeColors.textPrimary;
    ctx.font = "800 13px 'Plus Jakarta Sans', sans-serif";
    ctx.letterSpacing = "0.04em";
    ctx.fillText("EM QUEM EU VOTO", totalW - padX, footY - 8);

    ctx.fillStyle = themeColors.textMuted;
    ctx.font = "bold 11px 'JetBrains Mono', monospace";
    ctx.fillText("emquemeuvoto.onrender.com", totalW - padX, footY + 9);
    ctx.restore();

    this.lastExportCanvas = canvas;
    return canvas;
  }
};

window.CompareController = CompareController;
