/**
 * Gerenciador de Listas e Candidatos Salvos (Privacidade total no LocalStorage) — 2026
 * Suporta coleções de salvos ilimitadas e Colinha Oficial com até 6 vagas (regras TSE 2026).
 */

const STORAGE_KEY_LISTS = "emquemeuvoto_listas_v2";
const STORAGE_KEY_ACTIVE = "emquemeuvoto_lista_ativa";
const MAX_LISTS_LIMIT = 10;

// Definição canônica das vagas eleitorais para a eleição geral de 2026
const CARGO_SLOTS_2026 = [
  { id: "dep_est", label: "Dep. Estadual / Distrital", cargos: ["DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL"], max: 1, order: 1 },
  { id: "dep_fed", label: "Deputado Federal", cargos: ["DEPUTADO FEDERAL"], max: 1, order: 2 },
  { id: "senador", label: "Senador (2 vagas)", cargos: ["SENADOR"], max: 2, order: 3 },
  { id: "governador", label: "Governador", cargos: ["GOVERNADOR"], max: 1, order: 4 },
  { id: "presidente", label: "Presidente", cargos: ["PRESIDENTE", "PRESIDENTE DA REPÚBLICA"], max: 1, order: 5 }
];

function getCargoSlotId(cargo) {
  const c = String(cargo || "").toUpperCase().trim();
  if (c.includes("ESTADUAL") || c.includes("DISTRITAL")) return "dep_est";
  if (c.includes("FEDERAL")) return "dep_fed";
  if (c.includes("SENADOR")) return "senador";
  if (c.includes("GOVERNADOR")) return "governador";
  if (c.includes("PRESIDENTE")) return "presidente";
  return "outro";
}

const ListManager = {
  getLists() {
    try {
      const data = localStorage.getItem(STORAGE_KEY_LISTS);
      if (!data) {
        const initial = {
          "Meus Favoritos": {
            candidates: [],
            colinha_ids: []
          }
        };
        localStorage.setItem(STORAGE_KEY_LISTS, JSON.stringify(initial));
        return initial;
      }
      const parsed = JSON.parse(data);
      // Normalização transparente de listas legadas (que eram arrays simples)
      let needsSave = false;
      for (const key of Object.keys(parsed)) {
        if (Array.isArray(parsed[key])) {
          parsed[key] = {
            candidates: parsed[key],
            colinha_ids: []
          };
          needsSave = true;
        } else if (!parsed[key] || typeof parsed[key] !== "object") {
          parsed[key] = { candidates: [], colinha_ids: [] };
          needsSave = true;
        } else {
          if (!Array.isArray(parsed[key].candidates)) parsed[key].candidates = [];
          if (!Array.isArray(parsed[key].colinha_ids)) parsed[key].colinha_ids = [];
        }
      }
      if (needsSave) {
        localStorage.setItem(STORAGE_KEY_LISTS, JSON.stringify(parsed));
      }
      return parsed;
    } catch (e) {
      console.error("Erro ao ler listas:", e);
      return { "Meus Favoritos": { candidates: [], colinha_ids: [] } };
    }
  },

  getActiveListName() {
    const lists = this.getLists();
    const active = localStorage.getItem(STORAGE_KEY_ACTIVE);
    if (active && lists[active]) return active;
    const first = Object.keys(lists)[0] || "Meus Favoritos";
    localStorage.setItem(STORAGE_KEY_ACTIVE, first);
    return first;
  },

  setActiveListName(name) {
    localStorage.setItem(STORAGE_KEY_ACTIVE, name);
    this.notifyUpdate();
  },

  createList(name) {
    if (!name || !name.trim()) return false;
    const lists = this.getLists();
    const listNames = Object.keys(lists);
    if (listNames.length >= MAX_LISTS_LIMIT) {
      alert(`Você atingiu o limite máximo de ${MAX_LISTS_LIMIT} listas. Exclua uma lista antes de criar outra.`);
      return false;
    }
    const cleanName = name.trim();
    if (lists[cleanName]) {
      alert(`Já existe uma lista com o nome "${cleanName}".`);
      return false;
    }
    lists[cleanName] = {
      candidates: [],
      colinha_ids: []
    };
    localStorage.setItem(STORAGE_KEY_LISTS, JSON.stringify(lists));
    this.setActiveListName(cleanName);
    this.notifyUpdate();
    return true;
  },

  deleteList(name) {
    const lists = this.getLists();
    const listNames = Object.keys(lists);
    if (listNames.length <= 1) {
      alert("Você precisa manter ao menos uma lista salva.");
      return false;
    }
    if (!confirm(`Tem certeza que deseja excluir a lista "${name}" e todos os candidatos nela?`)) {
      return false;
    }
    delete lists[name];
    localStorage.setItem(STORAGE_KEY_LISTS, JSON.stringify(lists));
    const firstRemaining = Object.keys(lists)[0];
    this.setActiveListName(firstRemaining);
    this.notifyUpdate();
    return true;
  },

  getListData(name = null) {
    const lists = this.getLists();
    const listName = name || this.getActiveListName();
    if (!lists[listName]) {
      lists[listName] = { candidates: [], colinha_ids: [] };
    }
    return lists[listName];
  },

  saveListData(name, data) {
    const lists = this.getLists();
    lists[name] = data;
    localStorage.setItem(STORAGE_KEY_LISTS, JSON.stringify(lists));
    this.notifyUpdate();
  },

  isFavorited(sq_candidato) {
    const data = this.getListData();
    return data.candidates.some(c => String(c.sq_candidato) === String(sq_candidato));
  },

  toggleFavorite(candidato) {
    const activeName = this.getActiveListName();
    const listData = this.getListData(activeName);
    const sq = String(candidato.sq_candidato);
    const idx = listData.candidates.findIndex(c => String(c.sq_candidato) === sq);

    if (idx >= 0) {
      // Remover dos salvos
      listData.candidates.splice(idx, 1);
      listData.colinha_ids = listData.colinha_ids.filter(id => String(id) !== sq);
    } else {
      // Adicionar aos salvos
      listData.candidates.push(candidato);
      // Auto-seleção inteligente para a Colinha se o slot ainda estiver vago
      this._smartAutoSelectForColinha(listData, candidato);
    }

    this.saveListData(activeName, listData);
  },

  _smartAutoSelectForColinha(listData, cand) {
    const slotId = getCargoSlotId(cand.cargo);
    const slotDef = CARGO_SLOTS_2026.find(s => s.id === slotId);
    if (!slotDef) return;

    // Contar quantos desse cargo já estão na colinha
    const selectedOfCargo = listData.candidates.filter(c =>
      listData.colinha_ids.includes(String(c.sq_candidato)) &&
      getCargoSlotId(c.cargo) === slotId
    );

    if (selectedOfCargo.length < slotDef.max) {
      listData.colinha_ids.push(String(cand.sq_candidato));
    }
  },

  toggleColinhaSelection(sq_candidato) {
    const activeName = this.getActiveListName();
    const listData = this.getListData(activeName);
    const sq = String(sq_candidato);
    const cand = listData.candidates.find(c => String(c.sq_candidato) === sq);
    if (!cand) return;

    const isSelected = listData.colinha_ids.includes(sq);

    if (isSelected) {
      // Desmarcar da colinha
      listData.colinha_ids = listData.colinha_ids.filter(id => id !== sq);
    } else {
      // Marcar para a colinha (obedecendo os limites do cargo)
      const slotId = getCargoSlotId(cand.cargo);
      const slotDef = CARGO_SLOTS_2026.find(s => s.id === slotId);
      const maxSlots = slotDef ? slotDef.max : 1;

      // Buscar quem desse cargo já está na colinha
      const existingOfCargo = listData.candidates.filter(c =>
        listData.colinha_ids.includes(String(c.sq_candidato)) &&
        getCargoSlotId(c.cargo) === slotId
      );

      if (existingOfCargo.length >= maxSlots) {
        // Substituir o anterior para garantir agilidade e conveniência ao usuário
        const toRemove = existingOfCargo[0];
        listData.colinha_ids = listData.colinha_ids.filter(id => id !== String(toRemove.sq_candidato));
        listData.colinha_ids.push(sq);
      } else {
        listData.colinha_ids.push(sq);
      }
    }

    this.saveListData(activeName, listData);
  },

  isCandidateInColinha(sq_candidato) {
    const listData = this.getListData();
    return listData.colinha_ids.includes(String(sq_candidato));
  },

  draggedIdx: null,
  dragOverIdx: null,

  onDragStart(e, idx) {
    this.draggedIdx = idx;
    if (e.dataTransfer) {
      e.dataTransfer.effectAllowed = "move";
      e.dataTransfer.setData("text/plain", String(idx));
    }
    const cardEl = e.currentTarget;
    setTimeout(() => {
      if (cardEl) cardEl.classList.add("dragging");
    }, 0);
  },

  onDragOver(e, idx) {
    e.preventDefault();
    if (e.dataTransfer) e.dataTransfer.dropEffect = "move";
    if (this.dragOverIdx !== idx) {
      this.dragOverIdx = idx;
      const cards = document.querySelectorAll(".saved-cand-card");
      cards.forEach((c, i) => c.classList.toggle("drag-over", i === idx));
    }
  },

  onDragEnd(e) {
    this.draggedIdx = null;
    this.dragOverIdx = null;
    document.querySelectorAll(".saved-cand-card").forEach(c => {
      c.classList.remove("dragging");
      c.classList.remove("drag-over");
    });
  },

  onDrop(e, targetIdx) {
    e.preventDefault();
    if (this.draggedIdx !== null && this.draggedIdx !== targetIdx) {
      const activeName = this.getActiveListName();
      const listData = this.getListData(activeName);
      if (listData && listData.candidates) {
        const moved = listData.candidates.splice(this.draggedIdx, 1)[0];
        listData.candidates.splice(targetIdx, 0, moved);
        this.saveListData(activeName, listData);
        this.renderSavedModal();
        if (window.ColinhaStudio) ColinhaStudio.update();
      }
    }
    this.draggedIdx = null;
    this.dragOverIdx = null;
  },

  reorderCandidate(sq_candidato, direction) {
    const activeName = this.getActiveListName();
    const listData = this.getListData(activeName);
    const sq = String(sq_candidato);
    const idx = listData.candidates.findIndex(c => String(c.sq_candidato) === sq);
    if (idx < 0) return;

    const targetIdx = idx + direction;
    if (targetIdx < 0 || targetIdx >= listData.candidates.length) return;

    const [moved] = listData.candidates.splice(idx, 1);
    listData.candidates.splice(targetIdx, 0, moved);
    this.saveListData(activeName, listData);
  },

  moveCandidate(sq_candidato, targetListName) {
    if (!targetListName) return;
    const activeName = this.getActiveListName();
    if (activeName === targetListName) return;

    const lists = this.getLists();
    const srcList = lists[activeName];
    const dstList = lists[targetListName];
    if (!srcList || !dstList) return;

    const sq = String(sq_candidato);
    const idx = srcList.candidates.findIndex(c => String(c.sq_candidato) === sq);
    if (idx < 0) return;

    const [cand] = srcList.candidates.splice(idx, 1);
    srcList.colinha_ids = srcList.colinha_ids.filter(id => id !== sq);

    if (!dstList.candidates.some(c => String(c.sq_candidato) === sq)) {
      dstList.candidates.push(cand);
      this._smartAutoSelectForColinha(dstList, cand);
    }

    localStorage.setItem(STORAGE_KEY_LISTS, JSON.stringify(lists));
    this.notifyUpdate();
  },

  getActiveCandidates() {
    const data = this.getListData();
    return data.candidates || [];
  },

  /**
   * Retorna os candidatos selecionados para a Colinha Oficial (até 6),
   * ordenados rigorosamente pela ordem de votação canônica da Urna Eletrônica:
   * 1. Deputado Estadual / Distrital
   * 2. Deputado Federal
   * 3. Senador (1ª e 2ª vaga)
   * 4. Governador
   * 5. Presidente
   */
  getColinhaCandidates(name = null) {
    const listData = this.getListData(name);
    const candidates = listData.candidates || [];
    let colinhaIds = listData.colinha_ids || [];

    // Se o usuário ainda não escolheu nada manualmente, auto-seleciona padrão
    if (colinhaIds.length === 0 && candidates.length > 0) {
      const autoList = [];
      const counts = {};
      for (const c of candidates) {
        const slot = getCargoSlotId(c.cargo);
        const slotDef = CARGO_SLOTS_2026.find(s => s.id === slot);
        const max = slotDef ? slotDef.max : 1;
        counts[slot] = (counts[slot] || 0);
        if (counts[slot] < max) {
          autoList.push(String(c.sq_candidato));
          counts[slot]++;
        }
      }
      colinhaIds = autoList;
      listData.colinha_ids = autoList;
      this.saveListData(name || this.getActiveListName(), listData);
    }

    const filtered = candidates.filter(c => colinhaIds.includes(String(c.sq_candidato)));

    // Ordenação canônica da Urna Eletrônica
    filtered.sort((a, b) => {
      const slotA = CARGO_SLOTS_2026.find(s => s.id === getCargoSlotId(a.cargo))?.order || 99;
      const slotB = CARGO_SLOTS_2026.find(s => s.id === getCargoSlotId(b.cargo))?.order || 99;
      return slotA - slotB;
    });

    return filtered.slice(0, 6);
  },

  getSavedCandidates(name = null) {
    return this.getColinhaCandidates(name);
  },

  openSavedModal(defaultView = "list") {
    if (window.App && typeof App.closeMobileSidebar === 'function') {
      App.closeMobileSidebar();
    }
    const modal = document.getElementById("savedListModal");
    if (!modal) return;
    this.switchView(defaultView);
    this.renderSavedModal();
    modal.classList.add("open");
  },

  switchView(mode) {
    const listView = document.getElementById("colinhaListView");
    const studioView = document.getElementById("colinhaStudioView");
    const btnList = document.getElementById("btnColMode_list");
    const btnStudio = document.getElementById("btnColMode_studio");

    if (mode === "studio") {
      if (listView) listView.style.display = "none";
      if (studioView) studioView.style.display = "grid";
      if (btnList) btnList.classList.remove("active");
      if (btnStudio) btnStudio.classList.add("active");
      setTimeout(() => {
        if (window.ColinhaStudio) ColinhaStudio.update();
      }, 40);
    } else {
      if (listView) listView.style.display = "block";
      if (studioView) studioView.style.display = "none";
      if (btnList) btnList.classList.add("active");
      if (btnStudio) btnStudio.classList.remove("active");
    }
  },

  filterMainViewByActiveList() {
    const activeName = this.getActiveListName();
    const candidates = this.getActiveCandidates();
    if (candidates.length === 0) {
      alert(`A lista "${activeName}" está vazia. Salve candidatos primeiro para filtrar.`);
      return;
    }
    const sqList = candidates.map(c => String(c.sq_candidato));
    if (window.App && typeof App.setFilterBySavedList === "function") {
      App.setFilterBySavedList(activeName, sqList);
      const modal = document.getElementById("savedListModal");
      if (modal) modal.classList.remove("open");
    }
  },

  renderSavedModal() {
    const container = document.getElementById("savedItemsList");
    if (!container) return;

    const lists = this.getLists();
    const activeName = this.getActiveListName();
    const listData = this.getListData(activeName);
    const candidates = listData.candidates || [];
    const colinhaCandidates = this.getColinhaCandidates(activeName);
    const colinhaCount = colinhaCandidates.length;

    const listNames = Object.keys(lists);

    // Mapeamento dos 6 slots oficiais da colinha
    const slotsStatus = CARGO_SLOTS_2026.map(slot => {
      const matched = colinhaCandidates.filter(c => getCargoSlotId(c.cargo) === slot.id);
      return {
        ...slot,
        filled: matched.length,
        candidates: matched
      };
    });

    container.innerHTML = `
      <!-- Toolbar de Listas (Seletor + Ações) -->
      <div class="saved-list-toolbar" style="display: flex; flex-wrap: wrap; gap: 0.5rem; align-items: center; margin-bottom: 0.85rem; background: var(--bg-card); padding: 0.65rem 0.85rem; border: 1px solid var(--border-color); border-radius: var(--radius-sm);">
        <div style="display: flex; align-items: center; gap: 0.4rem; flex: 1; min-width: 240px;">
          <span style="font-size: 0.72rem; font-weight: 700; color: var(--text-muted); text-transform: uppercase;">Lista:</span>
          <select class="form-select" style="flex: 1; font-weight: 600;" onchange="ListManager.setActiveListName(this.value); ListManager.renderSavedModal();">
            ${listNames.map(name => `
              <option value="${name}" ${name === activeName ? 'selected' : ''}>
                📁 ${name} (${(lists[name].candidates || []).length} salvos)
              </option>
            `).join("")}
          </select>
        </div>

        <div style="display: flex; gap: 0.35rem; align-items: center;">
          <button type="button" class="btn-header" style="font-size: 0.74rem; padding: 0.35rem 0.6rem;" onclick="const n = prompt('Nome da nova lista (máx. 10 listas):'); if(n) { if(ListManager.createList(n)) ListManager.renderSavedModal(); }">
            + Nova Lista
          </button>
          ${listNames.length > 1 ? `
            <button type="button" class="btn-danger-ghost" style="font-size: 0.74rem; padding: 0.35rem 0.55rem;" title="Excluir esta lista" onclick="if(ListManager.deleteList('${activeName}')) ListManager.renderSavedModal();">
              🗑️ Excluir
            </button>
          ` : ''}
          <button type="button" class="btn-header" style="font-size: 0.74rem; padding: 0.35rem 0.65rem;" title="Filtrar os cards e a tabela por esta lista salva" onclick="ListManager.filterMainViewByActiveList()">
            🔍 Filtrar no Feed
          </button>
        </div>
      </div>

      <!-- Régua de Vagas da Colinha 2026 (Máx 6 Votos) -->
      <div class="colinha-slots-banner" style="background: var(--bg-page); border: 1px solid var(--border-color); border-radius: var(--radius-sm); padding: 0.75rem 0.9rem; margin-bottom: 1rem;">
        <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.5rem;">
          <div>
            <strong style="font-size: 0.82rem; color: var(--text-primary); text-transform: uppercase; letter-spacing: 0.04em;">Minha Colinha</strong>
            <span style="font-size: 0.72rem; color: var(--text-muted); margin-left: 0.35rem;">(Máx. 6 candidatos)</span>
          </div>
          <span class="badge-count" style="background: ${colinhaCount === 6 ? 'var(--accent-emerald, #10B981)' : 'var(--accent-terracota, #C84B31)'}; color: #FFFFFF; font-size: 0.74rem; padding: 0.15rem 0.55rem; font-weight: 700; border-radius: 12px;">
            ${colinhaCount} de 6 preenchidos
          </span>
        </div>

        <div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(140px, 1fr)); gap: 0.4rem;">
          ${slotsStatus.map(s => {
      const isFull = s.filled >= s.max;
      return `
              <div style="padding: 0.4rem 0.55rem; background: var(--bg-card); border: 1px solid ${isFull ? 'var(--border-color)' : 'var(--border-color)'}; border-left: 3px solid ${isFull ? 'var(--accent-emerald, #10B981)' : 'var(--text-muted)'}; border-radius: var(--radius-xs); font-size: 0.7rem;">
                <div style="display: flex; justify-content: space-between; font-weight: 700; color: ${isFull ? 'var(--text-primary)' : 'var(--text-muted)'}; margin-bottom: 0.15rem;">
                  <span>${s.label.split("(")[0]}</span>
                  <span>${s.filled}/${s.max}</span>
                </div>
                <div style="white-space: nowrap; overflow: hidden; text-overflow: ellipsis; font-size: 0.68rem; color: ${isFull ? 'var(--accent-emerald, #10B981)' : 'var(--text-muted)'};">
                  ${s.filled > 0 ? s.candidates.map(c => c.nome_urna).join(", ") : '— Vazio —'}
                </div>
              </div>
            `;
    }).join("")}
        </div>
      </div>

      <!-- Lista de Candidatos Salvos com Ações Completas -->
      <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.5rem;">
        <span style="font-size: 0.78rem; font-weight: 700; color: var(--text-primary); text-transform: uppercase;">
          Candidatos Salvos na Lista (${candidates.length})
        </span>
        <span style="font-size: 0.7rem; color: var(--text-muted);">
          Marque quem você deseja levar para a colinha.
        </span>
      </div>

      ${candidates.length === 0 ? `
        <div style="text-align: center; padding: 2.8rem 1.2rem; color: var(--text-muted); font-size: 0.82rem; background: var(--bg-page); border: 1px solid var(--border-color); border-radius: var(--radius-sm);">
          <div style="margin-bottom: 0.5rem; color: var(--accent-terracota, #C84B31);">
            <svg width="32" height="32" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round">
              <path d="M19 21l-7-5-7 5V5a2 2 0 0 1 2-2h10a2 2 0 0 1 2 2z"></path>
            </svg>
          </div>
          <strong style="color: var(--text-primary); font-size: 0.95rem; display: block; margin-bottom: 0.35rem;">Esta lista está vazia</strong>
          Explore os cards ou a tabela e clique no ícone de salvar (marcador) para colecionar candidatos aqui.
        </div>
      ` : `
        <div style="display: flex; flex-direction: column; gap: 0.5rem; max-height: 380px; overflow-y: auto; padding-right: 0.25rem;">
          ${candidates.map((c, idx) => {
      const inColinha = listData.colinha_ids.includes(String(c.sq_candidato));
      const slotId = getCargoSlotId(c.cargo);
      const slotDef = CARGO_SLOTS_2026.find(s => s.id === slotId);
      const slotLabel = slotDef ? slotDef.label.split("(")[0].trim() : c.cargo;

      return `
              <div class="saved-cand-card" draggable="true"
                   ondragstart="ListManager.onDragStart(event, ${idx})"
                   ondragover="ListManager.onDragOver(event, ${idx})"
                   ondragend="ListManager.onDragEnd(event)"
                   ondrop="ListManager.onDrop(event, ${idx})"
                   style="display: flex; align-items: center; justify-content: space-between; gap: 0.6rem; padding: 0.65rem 0.85rem; background: var(--bg-card); border: 1px solid ${inColinha ? 'var(--accent-emerald, #10B981)' : 'var(--border-color)'}; border-left: 4px solid ${inColinha ? 'var(--accent-emerald, #10B981)' : (c.partido_cor_hex || 'var(--border-color)')}; border-radius: var(--radius-sm); transition: all 0.15s ease;">
                
                <!-- Handle de Arrastar + Toggle para Colinha Oficial -->
                <div style="display: flex; align-items: center; gap: 0.4rem;">
                  <div class="saved-drag-handle" title="Arraste para reordenar" draggable="false">⠿</div>

                  <button type="button" class="btn-colinha-toggle ${inColinha ? 'active' : ''}" 
                          style="font-size: 0.68rem; font-weight: 700; padding: 0.35rem 0.6rem; border-radius: var(--radius-xs); border: 1px solid ${inColinha ? 'var(--accent-emerald, #10B981)' : 'var(--border-color)'}; background: ${inColinha ? 'var(--accent-emerald, #10B981)' : 'transparent'}; color: ${inColinha ? '#FFFFFF' : 'var(--text-secondary)'}; cursor: pointer; display: flex; align-items: center; gap: 0.3rem;"
                          onclick="ListManager.toggleColinhaSelection('${c.sq_candidato}'); ListManager.renderSavedModal(); if(window.ColinhaStudio) ColinhaStudio.update();"
                          title="${inColinha ? 'Remover da Colinha' : 'Incluir na Colinha (máx. 6)'}">
                    ${inColinha ? '✓ Na Colinha' : '+ Levar p/ Colinha'}
                  </button>
                </div>

                <!-- Identificação do Candidato -->
                <div style="display: flex; align-items: center; gap: 0.6rem; flex: 1; min-width: 0;">
                  <div style="width: 38px; height: 38px; border-radius: var(--radius-xs); overflow: hidden; background: var(--bg-page); flex-shrink: 0; border: 1px solid var(--border-color); display: flex; align-items: center; justify-content: center;">
                    <img src="${c.foto_url || `/api/candidatos/${c.sq_candidato}/foto`}" alt="${c.nome_urna}" style="width: 100%; height: 100%; object-fit: cover;" onerror="this.onerror=null; this.style.display='none'; this.parentNode.innerHTML='<span style=\\'font-weight:700;font-size:0.75rem;color:var(--text-muted);\\'>${(c.nome_urna || 'C').substring(0, 2)}</span>';">
                  </div>
                  
                  <div style="min-width: 0; flex: 1;">
                    <div style="display: flex; align-items: center; gap: 0.35rem; flex-wrap: wrap;">
                      <span class="party-tag-pill" style="border-left-color: ${c.partido_cor_hex || '#3B82F6'}; font-size: 0.65rem; padding: 0.1rem 0.35rem;">${c.partido}</span>
                      <span class="urna-number-tag" style="font-size: 0.68rem; font-family: var(--font-mono); font-weight: 800; background: var(--accent-petroleo, #005B68); color: #FFFFFF; border: none; padding: 0.12rem 0.4rem; border-radius: 3px;">
                        Nº ${c.numero || '---'}
                      </span>
                      <span style="font-size: 0.66rem; font-weight: 600; color: var(--accent-terracota, #C84B31);">
                        ${slotLabel} • ${c.uf}
                      </span>
                    </div>
                    <div style="font-family: var(--font-serif); font-weight: 700; color: var(--text-primary); font-size: 0.92rem; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; margin-top: 0.15rem;">
                      ${c.nome_urna}
                    </div>
                  </div>
                </div>

                <!-- Ações do Candidato Salvo -->
                <div style="display: flex; align-items: center; gap: 0.35rem; flex-shrink: 0;">
                  <button type="button" class="btn-card-action primary" style="font-size: 0.7rem; padding: 0.3rem 0.55rem;" title="Ver ficha completa" onclick="ListManager.openCandidateDetails('${c.sq_candidato}')">
                    Ficha ↗
                  </button>
                  <button type="button" class="btn-icon-square" style="width: 28px; height: 28px; font-size: 0.78rem;" title="Personalizar Santinho Digital" onclick="ListManager.openSantinho('${c.sq_candidato}')">
                    <svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="3" width="18" height="18" rx="2"/><circle cx="8.5" cy="8.5" r="1.5"/><path d="M21 15l-5-5L5 21"/></svg>
                  </button>
                  <button type="button" class="btn-icon-square ${window.CompareController && CompareController.isSelected(c.sq_candidato) ? 'active' : ''}" style="width: 28px; height: 28px; font-size: 0.78rem;" title="Comparar candidato" onclick="ListManager.toggleCandidateCompare(${JSON.stringify(c).replace(/"/g, '&quot;')})">
                    <svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                      <path d="m16 16 3-8 3 8c-.87.65-1.92 1-3 1s-2.13-.35-3-1Z"/>
                      <path d="m2 16 3-8 3 8c-.87.65-1.92 1-3 1s-2.13-.35-3-1Z"/>
                      <path d="M7 21h10"/>
                      <path d="M12 3v18"/>
                      <path d="M3 7h2c2 0 5-1 7-2 2 1 5 2 7 2h2"/>
                    </svg>
                  </button>

                  <!-- Mover para outra lista -->
                  ${listNames.length > 1 ? `
                    <select class="form-select tiny-move-select" style="font-size: 0.68rem; padding: 0.15rem 0.35rem; height: 26px; width: 68px; border-color: var(--border-color); color: var(--text-muted);" title="Mover para outra lista" onchange="if(this.value) { ListManager.moveCandidate('${c.sq_candidato}', this.value); ListManager.renderSavedModal(); }">
                      <option value="">Mover...</option>
                      ${listNames.filter(n => n !== activeName).map(n => `<option value="${n}">${n}</option>`).join("")}
                    </select>
                  ` : ''}

                  <button type="button" class="btn-danger-ghost" style="font-size: 0.78rem; padding: 0.3rem 0.45rem; line-height: 1;" title="Remover desta lista" onclick="ListManager.toggleFavorite(${JSON.stringify(c).replace(/"/g, '&quot;')}); ListManager.renderSavedModal(); if(window.ColinhaStudio) ColinhaStudio.update();">
                    ✕
                  </button>
                </div>

              </div>
            `;
    }).join("")}
        </div>
      `}

      <!-- Rodapé de Ações do Modal -->
      <div style="display: flex; flex-wrap: wrap; gap: 0.6rem; justify-content: space-between; align-items: center; margin-top: 1.25rem; border-top: 1px solid var(--border-color); padding-top: 0.85rem;">
        <div style="font-size: 0.75rem; color: var(--text-muted);">
          ${colinhaCount > 0 ? `<strong>${colinhaCount}</strong> candidato(s) prontos para a colinha.` : 'Nenhum candidato selecionado para a colinha.'}
        </div>
        <div style="display: flex; gap: 0.5rem; align-items: center;">
          <button type="button" class="btn-header" style="font-size: 0.78rem; padding: 0.45rem 0.85rem; display: inline-flex; align-items: center;" onclick="ListManager.compareSaved()">
            <svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" style="margin-right: 6px;">
              <path d="m16 16 3-8 3 8c-.87.65-1.92 1-3 1s-2.13-.35-3-1Z"/>
              <path d="m2 16 3-8 3 8c-.87.65-1.92 1-3 1s-2.13-.35-3-1Z"/>
              <path d="M7 21h10"/>
              <path d="M12 3v18"/>
              <path d="M3 7h2c2 0 5-1 7-2 2 1 5 2 7 2h2"/>
            </svg>
            <span>Comparar Salvos</span>
          </button>
        </div>
      </div>
    `;
  },

  openSantinho(candidateOrSq) {
    let cand = null;
    if (typeof candidateOrSq === 'object' && candidateOrSq !== null) {
      cand = candidateOrSq;
    } else {
      const listData = this.getListData();
      cand = (listData.candidates || []).find(c => String(c.sq_candidato) === String(candidateOrSq));
      if (!cand && window.App && typeof App.getCandidateBySq === 'function') {
        cand = App.getCandidateBySq(candidateOrSq);
      }
    }
    if (!cand) return;
    if (window.SantinhoStudio) {
      SantinhoStudio._returnToModal = 'savedListModal';
      const savedModal = document.getElementById("savedListModal");
      if (savedModal) savedModal.classList.remove("open");
      SantinhoStudio.open(cand);
    }
  },

  openCandidateDetails(sq) {
    if (window.App && typeof App.openDetails === 'function') {
      App._returnToModal = 'savedListModal';
      const savedModal = document.getElementById("savedListModal");
      if (savedModal) savedModal.classList.remove("open");
      App.openDetails(sq);
    }
  },

  toggleCandidateCompare(cand) {
    if (!cand || !window.CompareController) return;
    CompareController.toggleCandidate(cand);
    this.renderSavedModal();
    if (window.App && typeof App.renderView === "function") {
      App.renderView();
    }
  },

  compareSaved() {
    const listData = this.getListData();
    const candidates = listData.candidates || [];
    if (candidates.length < 2) {
      alert("Adicione ao menos 2 candidatos aos salvos para poder compará-los lado a lado.");
      return;
    }
    if (window.CompareController) {
      CompareController.selectedCandidates = candidates.slice(0, 4);
      CompareController._returnToModal = 'savedListModal';
      const savedModal = document.getElementById("savedListModal");
      if (savedModal) savedModal.classList.remove("open"); // Fecha salvos
      CompareController.openModal(); // Abre o comparador
    }
  },

  notifyUpdate() {
    window.dispatchEvent(new CustomEvent("lists:updated"));
  }
};

window.ListManager = ListManager;
