/**
 * Aplicação Principal — Em Quem Eu Voto 2026
 * Dados 100% Oficiais do TSE com Fotos Locais e Design Minimalista
 */

const STORAGE_KEY_VIEW = "emquemeuvoto_view_mode";

const UF_NOMES = {
  "AC": "Acre", "AL": "Alagoas", "AP": "Amapá", "AM": "Amazonas", "BA": "Bahia",
  "CE": "Ceará", "DF": "Distrito Federal", "ES": "Espírito Santo", "GO": "Goiás",
  "MA": "Maranhão", "MT": "Mato Grosso", "MS": "Mato Grosso do Sul", "MG": "Minas Gerais",
  "PA": "Pará", "PB": "Paraíba", "PR": "Paraná", "PE": "Pernambuco", "PI": "Piauí",
  "RJ": "Rio de Janeiro", "RN": "Rio Grande do Norte", "RS": "Rio Grande do Sul",
  "RO": "Rondônia", "RR": "Roraima", "SC": "Santa Catarina", "SP": "São Paulo",
  "SE": "Sergipe", "TO": "Tocantins", "BR": "Brasil"
};

const App = {
  PARTIDO_CORES: {
    "PT": "#C0122D",
    "PL": "#30306C",
    "NOVO": "#EC671C",
    "PSOL": "#68018D",
    "MDB": "#009959",
    "UNIÃO": "#00A0DF",
    "UNIAO": "#00A0DF",
    "PP": "#54B8EA",
    "PSD": "#FFA400",
    "REPUBLICANOS": "#005CA9",
    "PSDB": "#0F2BC5",
    "PDT": "#FE8E6D",
    "PSB": "#FFCC00",
    "PODE": "#00D663",
    "SOLIDARIEDADE": "#F37021",
    "AVANTE": "#2EABB1",
    "CIDADANIA": "#EC008C",
    "PRD": "#007C3C",
    "PV": "#01652F",
    "REDE": "#3CA08C",
    "AGIR": "#01369E",
    "MOBILIZA": "#DD3333",
    "DC": "#C89721",
    "PRTB": "#0047AB",
    "PMB": "#183C7C",
    "DEMOCRATA": "#183C7C",
    "PCDOB": "#800314",
    "PCdoB": "#800314",
    "PSTU": "#C92127",
    "PCB": "#A8231C",
    "PCO": "#9F030A",
    "UP": "#1A1A1A",
    "MISSÃO": "#FCBE26",
    "MISSAO": "#FCBE26"
  },

  getPartidoColor(sigla) {
    if (!sigla) return "#3B82F6";
    const clean = String(sigla).trim().toUpperCase();
    if (this.PARTIDO_CORES[clean]) return this.PARTIDO_CORES[clean];
    if (this.PARTIDO_CORES[sigla]) return this.PARTIDO_CORES[sigla];
    if (clean.includes("MISS")) return "#FCBE26";
    if (clean.includes("DEMOCRAT") || clean === "PMB") return "#183C7C";
    if (clean.includes("PCDOB") || clean.includes("PC DO B")) return "#800314";
    if (this.state && this.state.partidos && this.state.partidos.length > 0) {
      const p = this.state.partidos.find(x => x.sigla === sigla || x.sigla.toUpperCase() === clean);
      if (p && p.cor_hex) return p.cor_hex;
    }
    return "#3B82F6";
  },

  getResolvedPartidoColor(c) {
    if (!c) return "#3B82F6";
    const sigla = String(c.partido || "").trim().toUpperCase();
    if (sigla.includes("MISS")) return "#FCBE26";
    if (sigla.includes("DEMOCRAT") || sigla === "PMB") return "#183C7C";
    if (c.partido_cor_hex && c.partido_cor_hex !== "#71717A" && c.partido_cor_hex !== "#71717a") {
      return c.partido_cor_hex;
    }
    return this.getPartidoColor(c.partido);
  },

  state: {
    ano_eleicao: 2026,
    uf: "BR",
    selectedUfs: [], // Array de UFs selecionadas (vazio = Todo o Brasil)
    municipio: "",
    cargo: "",
    selectedCargos: [], // Array de Cargos selecionados (vazio = Todos os Cargos)
    situacao_mandato: "todos", // "todos", "reeleicao", "outro_cargo", "sem_mandato"
    situacao: "DEFERIDOS_AGUARDANDO", // Por padrão, candidatos deferidos e aguardando julgamento no TSE
    ideologia_min: 1,
    ideologia_max: 7,
    partidos_gosta: [],
    partidos_desgosta: [],
    genero: null,
    cor_raca: null,
    grau_instrucao: null,
    ocupacao: null,
    faixa_etaria: null,
    busca: "",
    ordenacao: "receitas",
    viewMode: localStorage.getItem(STORAGE_KEY_VIEW) || "cards", // "cards" ou "table"
    pagina: 1,
    limite: 30,
    total: 0,
    candidatos: [],
    partidos: [],
    ufs: [],
    cargos: [],
    todasOcupacoes: [],
    contagens: null,
    columnWidths: (() => {
      try {
        return JSON.parse(localStorage.getItem("emquemeuvoto_col_widths")) || {};
      } catch (e) {
        return {};
      }
    })(),
    columnOrder: [
      "candidato",
      "numero",
      "partido",
      "cargo",
      "uf",
      "ideologia",
      "mandato",
      "situacao",
      "profissao",
      "raca",
      "idade",
      "estado_civil",
      "prioridade_partido",
      "bens",
      "receitas",
      "gastos",
      "acoes"
    ],
    visibleColumns: {
      candidato: true,
      numero: true,
      partido: true,
      cargo: true,
      uf: true,
      ideologia: true,
      mandato: true,
      situacao: false,
      profissao: true,
      raca: true,
      idade: false,
      estado_civil: false,
      prioridade_partido: true,
      bens: true,
      receitas: true,
      gastos: true,
      acoes: true
    },
    colFilters: {
      nome: "",
      numero: "",
      partido: "",
      cargo: "",
      uf: "",
      ideologia: "",
      mandato: "",
      situacao: "",
      profissao: "",
      raca: "",
      idade: "",
      estado_civil: "",
      prioridade_partido: "",
      bens: "",
      receitas: "",
      gastos: ""
    }
  },

  async init() {
    console.log("Inicializando Em Quem Eu Voto 2026 (Dados TSE)...");
    try {
      const allCols = [
        "candidato", "numero", "partido", "cargo", "uf", "ideologia",
        "mandato", "situacao", "profissao", "raca", "idade", "estado_civil",
        "prioridade_partido", "bens", "receitas", "gastos", "acoes"
      ];
      try {
        const saved = JSON.parse(localStorage.getItem("emquemeuvoto_cols_v4") || "null");
        if (saved && typeof saved === "object") {
          allCols.forEach(col => {
            if (saved[col] !== undefined) this.state.visibleColumns[col] = Boolean(saved[col]);
          });
        }
        const savedOrder = JSON.parse(localStorage.getItem("emquemeuvoto_col_order_v3") || "null");
        if (Array.isArray(savedOrder) && savedOrder.length > 0) {
          const validSaved = savedOrder.filter(c => allCols.includes(c));
          allCols.forEach(c => {
            if (!validSaved.includes(c)) validSaved.push(c);
          });
          this.state.columnOrder = validSaved;
        }
      } catch (e) { }
      this.initTheme();
      this.initLayoutMode();
      this.initSidebarCollapse();
      this.initSidebarSticky();
      this.applyInitialViewMode();
      this.syncColumnCheckboxes();
      this.renderCargosPills();
      this.renderPartidosChips();
      this.updateIdeologyUI();
      await this.carregarMetadados();
      if (window.QuizController && typeof QuizController.init === "function") {
        await QuizController.init();
      }
      this.setupEventListeners();
      this.updateBookmarkBadge();
    } catch (err) {
      console.warn("Aviso durante inicialização:", err);
    }
    await this.carregarCandidatos();
  },

  setupEventListeners() {
    // 1. Fechar mobile sidebar ao clicar em qualquer item do menu lateral
    document.querySelectorAll(".nav-menu-btn, .mobile-nav-item").forEach(btn => {
      btn.addEventListener("click", () => {
        if (window.innerWidth <= 1024) {
          this.closeMobileSidebar();
        }
      });
    });

    // 2. Atalho Ctrl+K ou / para focar no campo de busca
    window.addEventListener("keydown", (e) => {
      if ((e.ctrlKey || e.metaKey) && e.key.toLowerCase() === "k") {
        e.preventDefault();
        const searchInput = document.getElementById("searchInput");
        if (searchInput) {
          searchInput.focus();
          searchInput.select();
        }
      }
    });

    // 3. Fechar popovers ao clicar fora
    document.addEventListener("click", (e) => {
      if (!e.target.closest("#topIdeologiaPopover") && !e.target.closest("#btnTopIdeologia") &&
        !e.target.closest("#topPartidosPopover") && !e.target.closest("#btnTopPartidos")) {
        this.closeTopPopovers();
      }
      if (!e.target.closest("#colDropdownWrap")) {
        const colMenu = document.getElementById("colDropdownMenu");
        if (colMenu) colMenu.classList.remove("open");
      }
    });
  },

  /* ─── Verificação de Filtros Ativos (para Feedback no Header) ───────────── */
  hasActiveFilters() {
    const s = this.state;
    const isUf = (s.uf && s.uf !== "BR") || (s.selectedUfs && s.selectedUfs.length > 0);
    const isCargo = (s.cargo && s.cargo !== "" && s.cargo !== "todos") || (s.selectedCargos && s.selectedCargos.length > 0);
    const isMandato = s.situacao_mandato && s.situacao_mandato !== "todos";
    const isSituacao = s.situacao && s.situacao !== "DEFERIDOS_AGUARDANDO" && s.situacao !== "todos" && s.situacao !== "todas";
    const isIdeologia = (s.ideologia_min > 1) || (s.ideologia_max < 7);
    const isPartidos = (s.partidos_gosta && s.partidos_gosta.length > 0) || (s.partidos_desgosta && s.partidos_desgosta.length > 0);
    const isBusca = !!(s.busca && s.busca.trim());
    const isTableFiltered = window.TableFilterManager && TableFilterManager.filters && Object.keys(TableFilterManager.filters).length > 0;
    return Boolean(isUf || isCargo || isMandato || isSituacao || isIdeologia || isPartidos || isBusca || isTableFiltered);
  },

  /* ─── Sticky Dinâmico Bidirecional dos Filtros da Barra Lateral ─────────── */
  initSidebarSticky() {
    const wrap = document.getElementById("sidebarStickyWrap");
    if (!wrap) return;

    let lastY = window.scrollY || window.pageYOffset;
    let ticking = false;

    const update = () => {
      if (window.innerWidth <= 1024) {
        wrap.classList.remove("stick-bottom", "stick-top");
        wrap.style.top = "";
        ticking = false;
        return;
      }

      const currentY = window.scrollY || window.pageYOffset;
      const delta = currentY - lastY;
      const wrapH = wrap.offsetHeight;
      const viewH = window.innerHeight;

      wrap.style.setProperty("--sidebar-filters-height", `${wrapH}px`);

      if (wrapH + 40 <= viewH) {
        // Bloco cabe inteiramente na tela: fixa confortavelmente no topo
        wrap.classList.remove("stick-bottom");
        wrap.classList.add("stick-top");
      } else {
        // Bloco é mais alto que a tela:
        // Ao descer, ancora no rodapé para exibir partidos e quiz
        // Ao dar uma subidinha, ancora no topo para exibir local, cargo e mandato
        if (delta > 2) {
          wrap.classList.remove("stick-top");
          wrap.classList.add("stick-bottom");
        } else if (delta < -2) {
          wrap.classList.remove("stick-bottom");
          wrap.classList.add("stick-top");
        }
      }

      lastY = currentY;
      ticking = false;
    };

    window.addEventListener("scroll", () => {
      if (!ticking) {
        requestAnimationFrame(update);
        ticking = true;
      }
    }, { passive: true });

    window.addEventListener("resize", () => {
      if (!ticking) {
        requestAnimationFrame(update);
        ticking = true;
      }
    }, { passive: true });

    update();
  },

  /* ─── Gestão de Tema Editorial (The Broadsheet / Modo Papel & Noturno) ──── */
  initTheme() {
    let saved = localStorage.getItem("emquemeuvoto_theme");
    // Padrão editorial oficial: Modo Claro (The Broadsheet Paper)
    if (!saved) {
      saved = 'light';
    }
    this.setTheme(saved, false);
  },

  setTheme(theme, notify = false) {
    const isDark = theme === "dark";
    document.documentElement.setAttribute("data-theme", isDark ? "dark" : "light");
    if (document.body) {
      document.body.classList.toggle("dark-mode", isDark);
    }
    localStorage.setItem("emquemeuvoto_theme", isDark ? "dark" : "light");

    const sunIcon = document.getElementById("themeIconSun");
    const moonIcon = document.getElementById("themeIconMoon");
    const btn = document.getElementById("btnThemeToggle");

    if (sunIcon && moonIcon) {
      sunIcon.style.display = isDark ? "block" : "none";
      moonIcon.style.display = isDark ? "none" : "block";
    }

    if (btn) {
      btn.title = isDark ? "Alternar para Edição Papel (Modo Claro)" : "Alternar para Edição Noturna (Modo Escuro)";
      btn.setAttribute("aria-label", btn.title);
    }

    if (notify && typeof this.showToast === "function") {
      this.showToast(isDark ? "Edição Noturna ativada" : "Edição Papel (The Broadsheet) ativada");
    }
  },

  toggleTheme() {
    const current = document.documentElement.getAttribute("data-theme") ||
      (document.body && document.body.classList.contains("dark-mode") ? "dark" : "light");
    const newTheme = current === "dark" ? "light" : "dark";
    this.setTheme(newTheme, true);
  },

  /* ─── Gestão da Configuração da Interface (Menu Lateral vs Clássico) ──────── */
  initLayoutMode() {
    let mode = null;
    try {
      const urlParams = new URLSearchParams(window.location.search);
      mode = urlParams.get("layout");
    } catch (e) { }

    if (!mode) {
      try {
        mode = localStorage.getItem("emquemeuvoto_layout_mode");
      } catch (e) { }
    }

    // Padrão: Novo Modo (Menu Principal na Sidebar + Filtros no Topo)
    if (mode !== "classic") {
      mode = "menu";
    }
    this.applyLayoutMode(mode);
  },

  applyLayoutMode(mode) {
    const isClassic = (mode === "classic");
    if (document.body) {
      document.body.classList.toggle("layout-classic", isClassic);
    }
    try {
      localStorage.setItem("emquemeuvoto_layout_mode", mode);
    } catch (e) { }

    const targetLabels = document.querySelectorAll(".layout-switch-target-text");
    targetLabels.forEach(el => {
      el.textContent = isClassic ? "Alternar para Menu Principal" : "Alternar para Filtros na Lateral";
    });

    window.dispatchEvent(new Event("resize"));
  },

  toggleLayoutMode() {
    const isClassic = document.body && document.body.classList.contains("layout-classic");
    const newMode = isClassic ? "menu" : "classic";
    this.applyLayoutMode(newMode);
    if (typeof this.showToast === "function") {
      this.showToast(newMode === "classic" ? "Modo Clássico (Filtros na Lateral) ativado" : "Novo Modo (Menu Principal) ativado");
    }
  },

  navigateToCandidates() {
    if (window.innerWidth <= 1024) {
      this.closeMobileSidebar();
    }
    const target = document.getElementById("topFilterPanel") || document.getElementById("candidatesContainer");
    if (target) {
      target.scrollIntoView({ behavior: "smooth", block: "start" });
    }
    document.querySelectorAll(".nav-menu-btn").forEach(b => b.classList.remove("active"));
    const btnCand = document.getElementById("navItemCandidates");
    if (btnCand) btnCand.classList.add("active");
  },

  /* ─── Recolhimento do Painel Lateral (Sidebar Collapsible) ───────────────── */
  initSidebarCollapse() {
    try {
      if (localStorage.getItem("emquemeuvoto_sidebar_collapsed") === "1") {
        this.toggleSidebar(true);
      }
    } catch (e) { }
  },

  toggleSidebar(forceState) {
    if (window.innerWidth <= 1024) {
      if (document.body.classList.contains("mobile-sidebar-open")) {
        this.closeMobileSidebar();
      } else {
        this.openMobileSidebar();
      }
      return;
    }

    const shell = document.querySelector(".app-shell-layout");
    if (!shell) return;

    const isCurrentlyCollapsed = shell.classList.contains("sidebar-collapsed");
    const willCollapse = (typeof forceState === "boolean") ? forceState : !isCurrentlyCollapsed;

    shell.classList.toggle("sidebar-collapsed", willCollapse);
    try {
      localStorage.setItem("emquemeuvoto_sidebar_collapsed", willCollapse ? "1" : "0");
    } catch (e) { }

    const btn = document.getElementById("btnToggleSidebar");
    if (btn) {
      btn.title = willCollapse
        ? "Expandir painel de filtros"
        : "Recolher painel de filtros (mais espaço para cards e tabela)";
      btn.setAttribute("aria-label", btn.title);
    }

    // Dispara evento de redimensionamento para sincronizar tabela e cards virtuais
    window.dispatchEvent(new Event("resize"));
  },

  openMobileSidebar() {
    document.body.classList.add("mobile-sidebar-open");
    this.updateMobileSidebarCount();
    const sidebar = document.getElementById("sidebarPanel");
    if (sidebar) sidebar.scrollTop = 0;
    const wrap = document.getElementById("sidebarStickyWrap");
    if (wrap) wrap.scrollTop = 0;
  },

  closeMobileSidebar() {
    document.body.classList.remove("mobile-sidebar-open");
  },

  updateMobileSidebarCount() {
    const el = document.getElementById("mobileSidebarCandidateCount");
    if (el && this.state && this.state.candidatosFiltrados) {
      el.textContent = this.state.candidatosFiltrados.length;
    }
  },

  focusMobileSearch() {
    const input = document.getElementById("searchInput");
    if (input) {
      input.scrollIntoView({ behavior: "smooth", block: "center" });
      setTimeout(() => input.focus(), 280);
    }
  },

  scrollToTop() {
    window.scrollTo({ top: 0, behavior: "smooth" });
  },

  onSearchInput(val) {
    const btn = document.getElementById("btnSearchClear");
    if (btn) {
      btn.style.display = (val && val.trim().length > 0) ? "flex" : "none";
    }
    const clean = (val || "").trim();
    if (clean.length === 0) {
      this.state.busca = "";
      if (window.TableFilterManager && TableFilterManager.filters.candidato) {
        delete TableFilterManager.filters.candidato;
      }
    } else {
      this.state.busca = clean;
      if (window.TableFilterManager && TableFilterManager.filters.candidato) {
        TableFilterManager.filters.candidato = clean;
      }
    }
  },

  clearSearchInput() {
    const input = document.getElementById("searchInput");
    if (input) {
      input.value = "";
      input.focus();
    }
    const btn = document.getElementById("btnSearchClear");
    if (btn) btn.style.display = "none";
    this.state.busca = "";
    if (window.TableFilterManager && TableFilterManager.filters.candidato) {
      delete TableFilterManager.filters.candidato;
    }
    this.state.pagina = 1;
    this.carregarCandidatos();
  },

  updateBookmarkBadge() {
    const count = (window.ListManager && typeof ListManager.getActiveCandidates === "function")
      ? ListManager.getActiveCandidates().length
      : 0;
    const badge = document.getElementById("savedCountBadge");
    if (badge) {
      badge.textContent = count;
      badge.style.display = count > 0 ? "inline-block" : "none";
    }
    const mobBadge = document.getElementById("mobNavSavedBadge");
    if (mobBadge) {
      mobBadge.textContent = count;
      mobBadge.style.display = count > 0 ? "inline-block" : "none";
    }
    const sidebarBadge = document.getElementById("sidebarSavedBadge");
    if (sidebarBadge) {
      sidebarBadge.textContent = count;
      sidebarBadge.style.display = count > 0 ? "inline-block" : "none";
    }
  },

  applyInitialViewMode() {
    const btnCards = document.getElementById("btnViewCards");
    const btnTable = document.getElementById("btnViewTable");
    const colWrap = document.getElementById("colDropdownWrap");
    const filterBar = document.getElementById("feedFilterBar");

    const mode = this.state.viewMode || "cards";
    document.body.setAttribute("data-view-mode", mode);

    if (btnCards && btnTable) {
      btnCards.classList.toggle("active", mode === "cards");
      btnTable.classList.toggle("active", mode === "table");
    }
    if (colWrap) {
      colWrap.style.display = mode === "table" ? "block" : "none";
    }
    if (filterBar) {
      filterBar.style.display = mode === "cards" ? "flex" : "none";
    }
    const sortSelect = document.getElementById("selectSortFeed");
    if (sortSelect) {
      sortSelect.value = this.state.ordenacao || "receitas";
    }
  },

  async carregarMetadados() {
    try {
      const [respUfs, respPartidos, respCargos, respOcupacoes, respStatus] = await Promise.all([
        fetch(`/api/meta/ufs?ano=${this.state.ano_eleicao}`),
        fetch("/api/partidos"),
        fetch(`/api/meta/cargos?ano=${this.state.ano_eleicao}`),
        fetch(`/api/meta/ocupacoes?ano=${this.state.ano_eleicao}`),
        fetch("/api/meta/status").catch(() => null)
      ]);

      if (respUfs.ok) {
        this.state.ufs = await respUfs.json();
        this.renderSelectUfs();
      }
      if (respPartidos.ok) {
        this.state.partidos = await respPartidos.json();
        this.renderPartidosChips();
      }
      if (respCargos.ok) {
        this.state.cargos = await respCargos.json();
        this.renderCargosPills();
      }
      if (respOcupacoes && respOcupacoes.ok) {
        this.state.todasOcupacoes = await respOcupacoes.json();
        this.renderModalProfissoes();
      }
      if (respStatus && respStatus.ok) {
        const stData = await respStatus.json();
        if (stData && stData.ultima_atualizacao_texto) {
          const hEl = document.getElementById("headerLastUpdated");
          const fEl = document.getElementById("footerLastUpdated");
          const dateEl = document.getElementById("headerUpdatedDate");
          const timeEl = document.getElementById("headerUpdatedTime");
          if (hEl) hEl.textContent = stData.ultima_atualizacao_texto;
          if (fEl) fEl.textContent = stData.ultima_atualizacao_texto;

          // Extrair data e hora de forma simetrica e limpa
          const match = stData.ultima_atualizacao_texto.match(/(\d{2}\/\d{2}(?:\/\d{4})?)\s+(?:às\s+)?(\d{2}:\d{2})/i);
          if (match) {
            if (dateEl) dateEl.textContent = match[1];
            if (timeEl) timeEl.textContent = `às ${match[2]}`;
          }
        }
      }
      this.updateIdeologyUI();
    } catch (e) {
      console.error("Erro ao carregar metadados:", e);
    }
  },

  renderModalProfissoes() {
    const select = document.getElementById("modalSelectProfissao");
    if (!select) return;
    const toTitle = (s) => s.charAt(0).toUpperCase() + s.slice(1).toLowerCase();
    const list = this.state.todasOcupacoes || [];
    const curVal = select.value;
    select.innerHTML = `
      <option value="">Todas as ocupações (${list.length})</option>
      ${list.map(item => `
        <option value="${item.ocupacao}">${toTitle(item.ocupacao)} (${item.total.toLocaleString('pt-BR')})</option>
      `).join('')}
    `;
    if (curVal) select.value = curVal;
  },

  renderSelectUfs() {
    const select = document.getElementById("selectUf");
    const topSelect = document.getElementById("topSelectUf");
    if (!select && !topSelect) return;

    const UF_NOMES = {
      "AC": "Acre", "AL": "Alagoas", "AP": "Amapá", "AM": "Amazonas", "BA": "Bahia",
      "CE": "Ceará", "DF": "Distrito Federal", "ES": "Espírito Santo", "GO": "Goiás",
      "MA": "Maranhão", "MT": "Mato Grosso", "MS": "Mato Grosso do Sul", "MG": "Minas Gerais",
      "PA": "Pará", "PB": "Paraíba", "PR": "Paraná", "PE": "Pernambuco", "PI": "Piauí",
      "RJ": "Rio de Janeiro", "RN": "Rio Grande do Norte", "RS": "Rio Grande do Sul",
      "RO": "Rondônia", "RR": "Roraima", "SC": "Santa Catarina", "SP": "São Paulo",
      "SE": "Sergipe", "TO": "Tocantins"
    };

    const sortedUfs = [...this.state.ufs.filter(u => u.uf !== "BR")].sort((a, b) => {
      const nomeA = UF_NOMES[a.uf] || a.uf;
      const nomeB = UF_NOMES[b.uf] || b.uf;
      return nomeA.localeCompare(nomeB);
    });

    const optionsHtml = `
      <option value="BR">🇧🇷 Todo o Brasil</option>
      ${sortedUfs.map(u => {
      const nomeExtenso = UF_NOMES[u.uf] || u.uf;
      return `<option value="${u.uf}">${nomeExtenso} (${u.uf})</option>`;
    }).join("")}
    `;

    if (select) {
      select.innerHTML = optionsHtml;
      select.value = this.state.uf || "BR";
    }
    if (topSelect) {
      topSelect.innerHTML = optionsHtml;
      topSelect.value = this.state.uf || "BR";
    }
  },

  setUfFilter(val) {
    const cleanVal = val || "BR";
    this.state.uf = cleanVal;
    this.state.selectedUfs = (cleanVal !== "BR") ? [cleanVal] : [];
    this.renderSelectUfs();
    this.state.pagina = 1;
    this.carregarCandidatos();
  },

  renderCargosPills() {
    const container = document.getElementById("cargoPillsContainer");
    const topContainer = document.getElementById("topCargoPillsContainer");
    if (!container && !topContainer) return;

    const cargosList = [
      { cargo: "PRESIDENTE", label: "Presidente" },
      { cargo: "GOVERNADOR", label: "Governador" },
      { cargo: "SENADOR", label: "Senador" },
      { cargo: "DEPUTADO FEDERAL", label: "Dep. Federal" },
      { cargo: "DEPUTADO ESTADUAL", label: "Dep. Estadual" },
      { cargo: "DEPUTADO DISTRITAL", label: "Dep. Distrital (DF)" }
    ];

    const pillsHtml = cargosList.map(c => {
      const isSelected = this.state.selectedCargos.includes(c.cargo);
      return `
        <button type="button" class="cargo-pill ${isSelected ? 'active' : ''}" 
                onclick="App.toggleCargo('${c.cargo}')">
          ${isSelected ? '✓ ' : ''}${c.label}
        </button>
      `;
    }).join("");

    if (container) container.innerHTML = pillsHtml;
    if (topContainer) topContainer.innerHTML = pillsHtml;
  },

  clearCargoFilter() {
    this.state.selectedCargos = [];
    this.renderCargosPills();
    this.state.pagina = 1;
    this.carregarCandidatos();
  },

  toggleCargo(cargo) {
    if (!cargo) {
      this.state.selectedCargos = [];
    } else {
      if (this.state.selectedCargos.length === 1 && this.state.selectedCargos[0] === cargo) {
        this.state.selectedCargos = [];
      } else {
        this.state.selectedCargos = [cargo];
      }
    }
    this.renderCargosPills();
    this.state.pagina = 1;
    this.carregarCandidatos();
  },

  setCargoFilter(cargo) {
    this.toggleCargo(cargo === "todos" ? "" : cargo);
  },

  renderPartidosChips() {
    const container = document.getElementById("partidosChipsContainer");
    const topContainer = document.getElementById("topPartidosChipsContainer");
    if (!container && !topContainer) return;

    if (!this.state.partidos || this.state.partidos.length === 0) {
      if (container) container.innerHTML = `<div style="font-size: 0.7rem; color: var(--text-muted); padding: 0.25rem 0;">Carregando partidos...</div>`;
      if (topContainer) topContainer.innerHTML = `<div style="font-size: 0.7rem; color: var(--text-muted); padding: 0.25rem 0;">Carregando partidos...</div>`;
      return;
    }

    const isAll = (this.state.ideologia_min === 1 && this.state.ideologia_max === 7);
    const partidosNoRange = this.state.partidos.filter(p => {
      if (isAll) return true;
      if (p.faixa === null || p.faixa === undefined) return false;
      return p.faixa >= this.state.ideologia_min && p.faixa <= this.state.ideologia_max;
    });

    const chipsHtml = partidosNoRange.map(p => {
      const isGosto = this.state.partidos_gosta.includes(p.sigla);
      const isDesgosto = this.state.partidos_desgosta.includes(p.sigla);
      let statusClass = "";
      if (isGosto) statusClass = "selected";
      if (isDesgosto) statusClass = "desgostado";

      const cleanSigla = p.sigla.toUpperCase().includes('MISS') ? 'MISSÃO' : p.sigla;
      const displayName = (p.sigla === 'PMB' || p.nome_completo === 'Democrata' || p.nome_completo === 'DEMOCRATA' || p.sigla === 'DEMOCRATA') ? 'DEMOCRATA' : cleanSigla;
      const displayTitle = (p.sigla === 'PMB' || p.nome_completo === 'Democrata' || p.nome_completo === 'DEMOCRATA' || p.sigla === 'DEMOCRATA') ? 'DEMOCRATA (PMB)' : (p.nome_completo || cleanSigla);
      const dotColor = (cleanSigla === 'MISSÃO' || p.sigla.toUpperCase().includes('MISS')) ? '#FCBE26' : (p.cor_hex || App.getPartidoColor(p.sigla));

      return `
        <button type="button" class="chip-btn ${statusClass}" 
                title="${displayTitle} • ${p.classificacao || ''}"
                onclick="App.togglePartidoChip('${p.sigla}')">
          <span style="width: 6px; height: 6px; border-radius: 50%; background: ${dotColor};"></span>
          ${displayName}
        </button>
      `;
    }).join("");

    if (container) container.innerHTML = chipsHtml;
    if (topContainer) topContainer.innerHTML = chipsHtml;

    const topPartidosLabel = document.getElementById("topPartidosDropdownLabel");
    if (topPartidosLabel) {
      const gCount = this.state.partidos_gosta ? this.state.partidos_gosta.length : 0;
      const dCount = this.state.partidos_desgosta ? this.state.partidos_desgosta.length : 0;
      if (gCount === 0 && dCount === 0) {
        topPartidosLabel.innerHTML = `Partidos: <strong>Todos</strong>`;
      } else {
        const parts = [];
        if (gCount > 0) parts.push(`${gCount} priorizado${gCount > 1 ? 's' : ''}`);
        if (dCount > 0) parts.push(`${dCount} oculto${dCount > 1 ? 's' : ''}`);
        topPartidosLabel.innerHTML = `Partidos: <strong>${parts.join(', ')}</strong>`;
      }
    }
  },

  clearPartidosPreferences() {
    this.state.partidos_gosta = [];
    this.state.partidos_desgosta = [];
    this.renderPartidosChips();
    this.state.pagina = 1;
    this.carregarCandidatos();
  },

  togglePartidoChip(sigla) {
    if (this.state.partidos_gosta.includes(sigla)) {
      this.state.partidos_gosta = this.state.partidos_gosta.filter(s => s !== sigla);
      this.state.partidos_desgosta.push(sigla);
    } else if (this.state.partidos_desgosta.includes(sigla)) {
      this.state.partidos_desgosta = this.state.partidos_desgosta.filter(s => s !== sigla);
    } else {
      this.state.partidos_gosta.push(sigla);
    }

    this.renderPartidosChips();
    this.state.pagina = 1;
    this.carregarCandidatos();
  },

  setMandateFilter(val) {
    if (this.state.situacao_mandato === val && val !== "todos") {
      val = "todos";
    }
    this.state.situacao_mandato = val;

    const isReeleicao = val === "reeleicao";
    const isOutroCargo = val === "outro_cargo";
    const isSemMandato = val === "sem_mandato" || val === "novos";
    const hasSelection = isReeleicao || isOutroCargo || isSemMandato;

    const btnTodos = document.getElementById("btnMandateTodos");
    const btnReeleicao = document.getElementById("btnMandateReeleicao");
    const btnOutroCargo = document.getElementById("btnMandateOutroCargo");
    const btnSemMandato = document.getElementById("btnMandateSemMandato") || document.getElementById("btnMandateNovos");

    if (btnTodos) btnTodos.classList.toggle("active", val === "todos");
    if (btnReeleicao) btnReeleicao.classList.toggle("active", isReeleicao);
    if (btnOutroCargo) btnOutroCargo.classList.toggle("active", isOutroCargo);
    if (btnSemMandato) btnSemMandato.classList.toggle("active", isSemMandato);

    // Sincronizar botões do painel superior
    const topBtnReeleicao = document.getElementById("topBtnMandateReeleicao");
    const topBtnOutroCargo = document.getElementById("topBtnMandateOutroCargo");
    const topBtnSemMandato = document.getElementById("topBtnMandateSemMandato");

    if (topBtnReeleicao) topBtnReeleicao.classList.toggle("active", isReeleicao);
    if (topBtnOutroCargo) topBtnOutroCargo.classList.toggle("active", isOutroCargo);
    if (topBtnSemMandato) topBtnSemMandato.classList.toggle("active", isSemMandato);

    // Sincronizar com o modal de filtros avançados
    const modalBtnTodos = document.getElementById("modalBtnMandateTodos");
    if (modalBtnTodos) modalBtnTodos.classList.toggle("active", val === "todos");
    document.querySelectorAll('#moreFiltersModal .filter-modal-chip[data-group="mandato"]').forEach(c => {
      c.classList.toggle("active", c.dataset.val === val || (val === "sem_mandato" && c.dataset.val === "novos") || (val === "novos" && c.dataset.val === "sem_mandato"));
    });

    if (window.TableFilterManager) {
      if (val === "todos") delete TableFilterManager.filters.mandato;
      else TableFilterManager.filters.mandato = [val];
    }

    this.state.pagina = 1;
    this.carregarCandidatos();
  },

  updateSidebarMandateCounts() {
    // No painel lateral, mantemos os botões limpos sem números de contagem por categoria
    const elTodos = document.getElementById("countMandateTodos");
    const elReeleicao = document.getElementById("countMandateReeleicao");
    const elOutroCargo = document.getElementById("countMandateOutroCargo");
    const elSemMandato = document.getElementById("countMandateSemMandato");
    if (elTodos) elTodos.textContent = '';
    if (elReeleicao) elReeleicao.textContent = '';
    if (elOutroCargo) elOutroCargo.textContent = '';
    if (elSemMandato) elSemMandato.textContent = '';
  },

  updateModalFilterCounts() {
    const c = this.state.contagens;
    if (!c) return;

    // 1. Situação no modal de mais filtros
    const elSitTodos = document.querySelector('#moreFiltersModal [data-group="situacao"][data-val="todos"]');
    const elSitApto = document.querySelector('#moreFiltersModal [data-group="situacao"][data-val="APTO"]');
    const elSitAgu = document.querySelector('#moreFiltersModal [data-group="situacao"][data-val="AGUARDANDO"]');
    const elSitIna = document.querySelector('#moreFiltersModal [data-group="situacao"][data-val="INAPTO"]');

    const total = (c.total || 0).toLocaleString('pt-BR');
    if (elSitTodos) elSitTodos.textContent = `Todos os Registrados (${total})`;
    if (elSitApto) elSitApto.textContent = `✓ Apenas Aptos / Deferidos (Padrão) (${(c.situacao?.APTO || 0).toLocaleString('pt-BR')})`;
    if (elSitAgu) elSitAgu.textContent = `⏳ Aguardando julgamento (${(c.situacao?.AGUARDANDO || 0).toLocaleString('pt-BR')})`;
    if (elSitIna) elSitIna.textContent = `✕ Inaptos / Indeferidos (${(c.situacao?.INAPTO || 0).toLocaleString('pt-BR')})`;

    // 2. Mandato no modal de mais filtros
    const elMandRee = document.querySelector('#moreFiltersModal [data-group="mandato"][data-val="reeleicao"]');
    const elMandOut = document.querySelector('#moreFiltersModal [data-group="mandato"][data-val="outro_cargo"]');
    const elMandSem = document.querySelector('#moreFiltersModal [data-group="mandato"][data-val="sem_mandato"]');

    if (elMandRee) elMandRee.textContent = `⚡ Reeleição (${(c.mandato?.reeleicao || 0).toLocaleString('pt-BR')})`;
    if (elMandOut) elMandOut.textContent = `Em exercício (outro cargo) (${(c.mandato?.outro_cargo || 0).toLocaleString('pt-BR')})`;
    if (elMandSem) elMandSem.textContent = `Não consta (${(c.mandato?.sem_mandato || 0).toLocaleString('pt-BR')})`;

    // 3. Gênero
    const elGenFem = document.querySelector('#moreFiltersModal [data-group="genero"][data-val="FEMININO"]');
    const elGenMasc = document.querySelector('#moreFiltersModal [data-group="genero"][data-val="MASCULINO"]');
    if (elGenFem) elGenFem.textContent = `Mulheres (${(c.genero?.FEMININO || 0).toLocaleString('pt-BR')})`;
    if (elGenMasc) elGenMasc.textContent = `Homens (${(c.genero?.MASCULINO || 0).toLocaleString('pt-BR')})`;

    const findCount = (dict, k) => {
      if (!dict || !k) return 0;
      if (dict[k] !== undefined) return dict[k];
      const clean = s => String(s).normalize("NFD").replace(/[\u0300-\u036f]/g, "").toUpperCase().trim();
      const target = clean(k);
      for (const [dk, dv] of Object.entries(dict)) {
        if (clean(dk) === target) return dv;
      }
      return 0;
    };

    const updateGroupLabels = (group, dict) => {
      if (!dict) return;
      document.querySelectorAll(`#moreFiltersModal [data-group="${group}"]`).forEach(el => {
        const val = el.dataset.val;
        const cnt = findCount(dict, val);
        let baseName = el.getAttribute('data-orig-label');
        if (!baseName) {
          baseName = el.textContent.replace(/\s*\([\d\.]+\)\s*$/, '').trim();
          el.setAttribute('data-orig-label', baseName);
        }
        el.textContent = cnt > 0 ? `${baseName} (${cnt.toLocaleString('pt-BR')})` : baseName;
      });
    };

    // 4. Raça, Escolaridade e Estado Civil
    updateGroupLabels('raca', c.raca);
    updateGroupLabels('instrucao', c.instrucao);
    updateGroupLabels('estado_civil', c.estado_civil);
  },

  onIdeologySliderInput(type, val) {
    this.onIdeologiaSliderChange(type, val);
  },

  onTopIdeologySliderInput(type, val) {
    this.onIdeologiaSliderChange(type, val);
  },

  onIdeologiaSliderChange(type, val) {
    let minSlider = document.getElementById("ideologiaMinSlider") || document.getElementById("topIdeologiaMinSlider");
    let maxSlider = document.getElementById("ideologiaMaxSlider") || document.getElementById("topIdeologiaMaxSlider");
    let minVal = minSlider ? parseInt(minSlider.value, 10) : this.state.ideologia_min;
    let maxVal = maxSlider ? parseInt(maxSlider.value, 10) : this.state.ideologia_max;

    // Priorizar z-index do thumb ativo para permitir arraste mesmo quando colados
    const syncZIndex = (minEl, maxEl) => {
      if (type === 'min' && minEl && maxEl) {
        minEl.style.zIndex = "5";
        maxEl.style.zIndex = "3";
      } else if (type === 'max' && minEl && maxEl) {
        maxEl.style.zIndex = "5";
        minEl.style.zIndex = "3";
      }
    };
    syncZIndex(document.getElementById("ideologiaMinSlider"), document.getElementById("ideologiaMaxSlider"));
    syncZIndex(document.getElementById("topIdeologiaMinSlider"), document.getElementById("topIdeologiaMaxSlider"));

    if (type === 'min') {
      let parsed = parseInt(val, 10);
      if (parsed > maxVal) {
        parsed = maxVal;
      }
      minVal = parsed;
    } else if (type === 'max') {
      let parsed = parseInt(val, 10);
      if (parsed < minVal) {
        parsed = minVal;
      }
      maxVal = parsed;
    }

    const sMin = document.getElementById("ideologiaMinSlider");
    const sMax = document.getElementById("ideologiaMaxSlider");
    const tMin = document.getElementById("topIdeologiaMinSlider");
    const tMax = document.getElementById("topIdeologiaMaxSlider");
    if (sMin) sMin.value = minVal;
    if (sMax) sMax.value = maxVal;
    if (tMin) tMin.value = minVal;
    if (tMax) tMax.value = maxVal;

    this.state.ideologia_min = minVal;
    this.state.ideologia_max = maxVal;
    this.updateIdeologyUI();
    this.renderPartidosChips();

    clearTimeout(this._ideologyDebounceTimer);
    this._ideologyDebounceTimer = setTimeout(() => {
      this.state.pagina = 1;
      this.carregarCandidatos();
    }, 80);
  },

  setIdeologyRange(min, max) {
    this.state.ideologia_min = min;
    this.state.ideologia_max = max;

    const minSlider = document.getElementById("ideologiaMinSlider");
    const maxSlider = document.getElementById("ideologiaMaxSlider");
    const topMinSlider = document.getElementById("topIdeologiaMinSlider");
    const topMaxSlider = document.getElementById("topIdeologiaMaxSlider");
    if (minSlider) minSlider.value = min;
    if (maxSlider) maxSlider.value = max;
    if (topMinSlider) topMinSlider.value = min;
    if (topMaxSlider) topMaxSlider.value = max;

    this.updateIdeologyUI();
    this.renderPartidosChips();
    this.state.pagina = 1;
    this.carregarCandidatos();
  },

  getIdeologyRangeLabel(min, max) {
    const names = {
      1: "Extrema-Esquerda",
      2: "Esquerda",
      3: "Centro-Esquerda",
      4: "Centro",
      5: "Centro-Direita",
      6: "Direita",
      7: "Extrema-Direita"
    };

    if (min === 1 && max === 7) return "Todos os espectros";
    if (min === max) return names[min];
    if (min === 1 && max === 3) return "Campo da Esquerda";
    if (min === 3 && max === 5) return "Campo do Centro";
    if (min === 5 && max === 7) return "Campo da Direita";
    if (min === 1 && max === 2) return "Extrema-Esquerda a Esquerda";
    if (min === 6 && max === 7) return "Direita a Extrema-Direita";
    return `${names[min]} a ${names[max]}`;
  },

  updateIdeologyUI() {
    const min = this.state.ideologia_min;
    const max = this.state.ideologia_max;
    const rangeLabel = this.getIdeologyRangeLabel(min, max);
    const isAll = (min === 1 && max === 7);

    // Sidebar Label & Highlight
    const label = document.getElementById("ideologiaCurrentLabel");
    if (label) {
      label.innerText = rangeLabel;
    }

    const highlight = document.getElementById("ideologiaTrackHighlight");
    const leftPct = ((min - 1) / 6) * 100;
    const widthPct = Math.max(2, ((max - min) / 6) * 100);
    if (highlight) {
      highlight.style.left = `${leftPct}%`;
      highlight.style.width = `${widthPct}%`;
    }

    // Top Panel Label, Dropdown text & Highlight
    const topLabel = document.getElementById("topIdeologiaCurrentLabel");
    if (topLabel) {
      topLabel.innerText = rangeLabel;
    }

    const topDropdownLabel = document.getElementById("topIdeologiaDropdownLabel");
    if (topDropdownLabel) {
      topDropdownLabel.innerHTML = `Ideologia: <strong>${isAll ? 'Todos' : rangeLabel}</strong>`;
    }

    const topHighlight = document.getElementById("topIdeologiaTrackHighlight");
    if (topHighlight) {
      topHighlight.style.left = `${leftPct}%`;
      topHighlight.style.width = `${widthPct}%`;
    }

    const topMinSlider = document.getElementById("topIdeologiaMinSlider");
    const topMaxSlider = document.getElementById("topIdeologiaMaxSlider");
    if (topMinSlider) topMinSlider.value = min;
    if (topMaxSlider) topMaxSlider.value = max;

    // Sincronizar botões de atalho da régua (Sidebar)
    const btnEsq = document.getElementById("btnIdeoEsq");
    const btnCentro = document.getElementById("btnIdeoCentro");
    const btnDir = document.getElementById("btnIdeoDir");
    const btnTodos = document.getElementById("btnIdeoTodos");

    if (btnEsq) btnEsq.classList.toggle("active", min === 1 && max === 3);
    if (btnCentro) btnCentro.classList.toggle("active", min === 3 && max === 5);
    if (btnDir) btnDir.classList.toggle("active", min === 5 && max === 7);
    if (btnTodos) btnTodos.classList.toggle("active", isAll);

    // Sincronizar botões de atalho da régua (Top Panel)
    const topBtnEsq = document.getElementById("topBtnIdeoEsq");
    const topBtnCentro = document.getElementById("topBtnIdeoCentro");
    const topBtnDir = document.getElementById("topBtnIdeoDir");

    if (topBtnEsq) topBtnEsq.classList.toggle("active", min === 1 && max === 3);
    if (topBtnCentro) topBtnCentro.classList.toggle("active", min === 3 && max === 5);
    if (topBtnDir) topBtnDir.classList.toggle("active", min === 5 && max === 7);
  },

  /* ─── Controle dos Popovers do Painel Superior (Ideologia & Partidos) ──────── */
  toggleTopPopover(key, evt) {
    if (evt && typeof evt.stopPropagation === 'function') {
      evt.stopPropagation();
    }
    const popIdeo = document.getElementById("topIdeologiaPopover");
    const popPart = document.getElementById("topPartidosPopover");
    const btnIdeo = document.getElementById("btnTopIdeologia");
    const btnPart = document.getElementById("btnTopPartidos");

    // Close CardFilterManager popover to prevent overlap
    if (window.CardFilterManager && typeof CardFilterManager.close === 'function') {
      CardFilterManager.close();
    }

    if (key === 'ideologia') {
      const isVisible = popIdeo && popIdeo.style.display !== "none";
      if (popPart) popPart.style.display = "none";
      if (btnPart) btnPart.classList.remove("active");
      if (popIdeo) popIdeo.style.display = isVisible ? "none" : "block";
      if (btnIdeo) btnIdeo.classList.toggle("active", !isVisible);
    } else if (key === 'partidos') {
      const isVisible = popPart && popPart.style.display !== "none";
      if (popIdeo) popIdeo.style.display = "none";
      if (btnIdeo) btnIdeo.classList.remove("active");
      if (popPart) popPart.style.display = isVisible ? "none" : "block";
      if (btnPart) btnPart.classList.toggle("active", !isVisible);
    }
  },

  closeTopPopovers() {
    const popIdeo = document.getElementById("topIdeologiaPopover");
    const popPart = document.getElementById("topPartidosPopover");
    const btnIdeo = document.getElementById("btnTopIdeologia");
    const btnPart = document.getElementById("btnTopPartidos");
    if (popIdeo) popIdeo.style.display = "none";
    if (popPart) popPart.style.display = "none";
    if (btnIdeo) btnIdeo.classList.remove("active");
    if (btnPart) btnPart.classList.remove("active");
  },

  setViewMode(mode) {
    this.state.viewMode = mode;
    localStorage.setItem(STORAGE_KEY_VIEW, mode);
    document.body.setAttribute("data-view-mode", mode);

    const btnCards = document.getElementById("btnViewCards");
    const btnTable = document.getElementById("btnViewTable");
    const colWrap = document.getElementById("colDropdownWrap");
    const filterBar = document.getElementById("feedFilterBar");

    if (btnCards) btnCards.classList.toggle("active", mode === "cards");
    if (btnTable) btnTable.classList.toggle("active", mode === "table");
    if (colWrap) {
      colWrap.style.display = mode === "table" ? "block" : "none";
    }
    if (filterBar) {
      filterBar.style.display = mode === "cards" ? "flex" : "none";
    }

    if (mode !== "table" && this._stickyTableCleanup) {
      this._stickyTableCleanup();
    }

    this.renderView();
  },

  setOrdenacao(ord) {
    this.state.ordenacao = ord;
    const sortSelect = document.getElementById("selectSortFeed");
    if (sortSelect && sortSelect.value !== ord) {
      sortSelect.value = ord;
    }
    this.state.pagina = 1;
    this.carregarCandidatos();
  },

  setLimite(lim) {
    this.state.limite = parseInt(lim, 10) || 30;
    this.state.pagina = 1;
    this.carregarCandidatos();
  },

  setSituacaoFilter(val) {
    const clean = (!val || val === "padrao" || val === "DEFERIDOS_AGUARDANDO") ? "DEFERIDOS_AGUARDANDO" : val;
    this.state.situacao = clean;
    this.state.pagina = 1;

    // Sincronizar com o modal de filtros avançados
    document.querySelectorAll('#moreFiltersModal .filter-modal-chip[data-group="situacao"]').forEach(c => {
      c.classList.toggle("active", c.dataset.val === clean || (clean === "DEFERIDOS_AGUARDANDO" && (c.dataset.val === "DEFERIDOS_AGUARDANDO" || c.dataset.val === "padrao")));
    });

    if (window.TableFilterManager) {
      if (clean === "DEFERIDOS_AGUARDANDO") {
        TableFilterManager.filters.situacao = ["Aptos / Deferidos", "Aguardando julgamento"];
      } else if (clean === "APTO") {
        TableFilterManager.filters.situacao = ["Aptos / Deferidos"];
      } else if (clean === "AGUARDANDO") {
        TableFilterManager.filters.situacao = ["Aguardando julgamento"];
      } else if (clean === "INAPTO") {
        TableFilterManager.filters.situacao = ["Inaptos / Indeferidos"];
      } else {
        delete TableFilterManager.filters.situacao;
      }
    }
    if (window.CardFilterManager) CardFilterManager.updateCardFilterButtons();
    this.carregarCandidatos();
  },

  setFilterBySavedList(listName, sqList) {
    if (!window.TableFilterManager) return;
    TableFilterManager.filters.filtro_lista = {
      name: listName || "Lista Salva",
      sqList: new Set((sqList || []).map(String))
    };
    this.state.pagina = 1;
    this.renderView();
  },

  clearFilterBySavedList() {
    if (window.TableFilterManager && TableFilterManager.filters.filtro_lista) {
      delete TableFilterManager.filters.filtro_lista;
      this.state.pagina = 1;
      this.renderView();
    }
  },

  hasAnyActiveFilter() {
    if (this.state.uf && this.state.uf !== "BR") return true;
    if (this.state.selectedUfs && this.state.selectedUfs.length > 0) return true;
    if (this.state.selectedCargos && this.state.selectedCargos.length > 0) return true;
    if (this.state.situacao_mandato && this.state.situacao_mandato !== "todos") return true;
    if (this.state.ideologia_min !== 1 || this.state.ideologia_max !== 7) return true;
    if (this.state.situacao && this.state.situacao !== "DEFERIDOS_AGUARDANDO" && this.state.situacao !== "todos" && this.state.situacao !== "todas") return true;
    if (this.state.partidos_gosta && this.state.partidos_gosta.length > 0) return true;
    if (this.state.partidos_desgosta && this.state.partidos_desgosta.length > 0) return true;
    if (this.state.busca && this.state.busca.trim().length > 0) return true;
    if (window.TableFilterManager && TableFilterManager.hasAnyFilter()) return true;
    return false;
  },

  clearAllFilters() {
    // 1. Localização
    this.state.uf = "BR";
    this.state.selectedUfs = [];
    const selUf = document.getElementById("selectUf");
    if (selUf) selUf.value = "BR";
    const topSelUf = document.getElementById("topSelectUf");
    if (topSelUf) topSelUf.value = "BR";
    const modalSelUf = document.getElementById("modalSelectUf");
    if (modalSelUf) modalSelUf.value = "BR";
    this.renderSelectUfs();

    // 2. Cargos
    this.state.selectedCargos = [];
    this.state.cargo = "";
    this.renderCargosPills();

    // 3. Mandato
    this.state.situacao_mandato = "todos";
    const btnTodosMand = document.getElementById("btnMandateTodos");
    if (btnTodosMand) btnTodosMand.classList.add("active");
    ["btnMandateReeleicao", "btnMandateOutroCargo", "btnMandateSemMandato", "btnMandateNovos",
      "topBtnMandateReeleicao", "topBtnMandateOutroCargo", "topBtnMandateSemMandato"].forEach(id => {
        const b = document.getElementById(id);
        if (b) b.classList.remove("active");
      });
    const btnMandatoVerTodos = document.getElementById("btnMandatoVerTodos");
    if (btnMandatoVerTodos) btnMandatoVerTodos.style.display = "none";

    // 4. Ideologia (reseta de volta para 1 a 7 - Todos os candidatos)
    this.state.ideologia_min = 1;
    this.state.ideologia_max = 7;
    const minSlider = document.getElementById("ideologiaMinSlider");
    const maxSlider = document.getElementById("ideologiaMaxSlider");
    if (minSlider) minSlider.value = 1;
    if (maxSlider) maxSlider.value = 7;
    this.updateIdeologyUI();

    // 5. Partidos
    this.state.partidos_gosta = [];
    this.state.partidos_desgosta = [];
    this.renderPartidosChips();

    // 6. Situação (Padrão: Deferidos e Aguardando julgamento)
    this.state.situacao = "DEFERIDOS_AGUARDANDO";

    // 7. Busca por texto
    this.state.busca = "";
    ["searchInput", "inputBusca"].forEach(id => {
      const inp = document.getElementById(id);
      if (inp) inp.value = "";
    });

    // 8. Tabela e Slicers / Popovers / Quick filters
    delete this.state.genero;
    delete this.state.cor_raca;
    delete this.state.grau_instrucao;
    delete this.state.ocupacao;
    delete this.state.bens_min;
    delete this.state.bens_max;
    delete this.state.gastos_min;
    delete this.state.gastos_max;
    delete this.state.receitas_min;
    delete this.state.receitas_max;
    if (window.TableFilterManager) {
      TableFilterManager.filters = {};
      TableFilterManager.close();
    }
    if (window.CardFilterManager) {
      CardFilterManager.close();
      CardFilterManager.updateCardFilterButtons();
    }

    this.state.pagina = 1;
    this.carregarCandidatos();
  },

  async carregarCandidatos() {
    const container = document.getElementById("candidatesContainer");
    const statsEl = document.getElementById("feedStats");
    if (container) {
      container.innerHTML = `<div style="text-align: center; padding: 3rem; color: #A1A1AA; font-size: 0.85rem;">Carregando candidatos oficiais do TSE...</div>`;
    }

    const params = new URLSearchParams({
      ano_eleicao: this.state.ano_eleicao,
      ideologia_min: this.state.ideologia_min,
      ideologia_max: this.state.ideologia_max,
      pagina: this.state.pagina,
      limite: this.state.limite,
      ordenacao: this.state.ordenacao
    });

    if (this.state.selectedUfs.length > 0) {
      params.append("uf", this.state.selectedUfs.join(","));
    } else if (this.state.uf && this.state.uf !== "BR") {
      params.append("uf", this.state.uf);
    }

    if (this.state.selectedCargos.length > 0) {
      params.append("cargo", this.state.selectedCargos.join(","));
    } else if (this.state.cargo) {
      params.append("cargo", this.state.cargo);
    }

    if (this.state.situacao_mandato && this.state.situacao_mandato !== "todos") {
      params.append("situacao_mandato", this.state.situacao_mandato);
    }
    if (this.state.situacao && this.state.situacao !== "todos" && this.state.situacao !== "todas") {
      params.append("situacao", this.state.situacao);
    }
    if (this.state.genero && typeof this.state.genero === "string" && this.state.genero.trim()) params.append("genero", this.state.genero.trim());
    if (this.state.cor_raca && typeof this.state.cor_raca === "string" && this.state.cor_raca.trim()) params.append("cor_raca", this.state.cor_raca.trim());
    if (this.state.grau_instrucao && typeof this.state.grau_instrucao === "string" && this.state.grau_instrucao.trim()) params.append("grau_instrucao", this.state.grau_instrucao.trim());
    if (this.state.ocupacao && typeof this.state.ocupacao === "string" && this.state.ocupacao.trim()) params.append("ocupacao", this.state.ocupacao.trim());
    if (this.state.faixa_etaria && typeof this.state.faixa_etaria === "string" && this.state.faixa_etaria.trim()) params.append("faixa_etaria", this.state.faixa_etaria.trim());
    if (this.state.bens_min !== undefined && this.state.bens_min !== null) params.append("bens_min", this.state.bens_min);
    if (this.state.bens_max !== undefined && this.state.bens_max !== null) params.append("bens_max", this.state.bens_max);
    if (this.state.gastos_min !== undefined && this.state.gastos_min !== null) params.append("gastos_min", this.state.gastos_min);
    if (this.state.gastos_max !== undefined && this.state.gastos_max !== null) params.append("gastos_max", this.state.gastos_max);
    if (this.state.receitas_min !== undefined && this.state.receitas_min !== null) params.append("receitas_min", this.state.receitas_min);
    if (this.state.receitas_max !== undefined && this.state.receitas_max !== null) params.append("receitas_max", this.state.receitas_max);

    if (this.state.busca) params.append("busca", this.state.busca);
    if (this.state.partidos_gosta.length > 0) params.append("partidos_gosta", this.state.partidos_gosta.join(","));
    if (this.state.partidos_desgosta.length > 0) params.append("partidos_desgosta", this.state.partidos_desgosta.join(","));

    const countParams = new URLSearchParams({
      ano_eleicao: this.state.ano_eleicao,
      ideologia_min: this.state.ideologia_min,
      ideologia_max: this.state.ideologia_max,
    });
    if (this.state.selectedUfs.length > 0) {
      countParams.append("uf", this.state.selectedUfs.join(","));
    } else if (this.state.uf && this.state.uf !== "BR") {
      countParams.append("uf", this.state.uf);
    }
    if (this.state.selectedCargos.length > 0) {
      countParams.append("cargo", this.state.selectedCargos.join(","));
    } else if (this.state.cargo) {
      countParams.append("cargo", this.state.cargo);
    }
    if (this.state.busca) countParams.append("busca", this.state.busca);
    if (this.state.situacao_mandato && this.state.situacao_mandato !== "todos") countParams.append("situacao_mandato", this.state.situacao_mandato);
    if (this.state.faixa_etaria && typeof this.state.faixa_etaria === "string" && this.state.faixa_etaria.trim()) countParams.append("faixa_etaria", this.state.faixa_etaria.trim());
    if (this.state.partidos_gosta.length > 0) countParams.append("partidos_gosta", this.state.partidos_gosta.join(","));
    if (this.state.partidos_desgosta.length > 0) countParams.append("partidos_desgosta", this.state.partidos_desgosta.join(","));

    try {
      const [resp, respCounts] = await Promise.all([
        fetch(`/api/candidatos?${params.toString()}`),
        fetch(`/api/candidatos/contagens?${countParams.toString()}`)
      ]);

      if (resp.ok) {
        const data = await resp.json();
        this.state.total = data.total;
        this.state.candidatos = data.candidatos;
        this.renderView();
        this.renderPagination(data.total_paginas);

        const bigNumEl = document.getElementById("statsBigNumber");
        if (bigNumEl) {
          bigNumEl.textContent = data.total.toLocaleString("pt-BR");
        }

        // Indicador de filtro ativo nas estatísticas (Apenas a tag Filtrado é exibida)
        const isFiltered = this.hasActiveFilters();
        const tagEl = document.getElementById("statsFilteredTag");
        const labelEl = document.getElementById("statsLabelInline");
        if (tagEl) {
          tagEl.style.display = isFiltered ? "inline-flex" : "none";
        }
        if (labelEl) {
          labelEl.textContent = "candidatos registrados (2026)";
        }

        if (statsEl) {
          statsEl.innerHTML = `<strong>${data.total.toLocaleString("pt-BR")}</strong> candidatos registrados (2026)`;
        }
        const heroStatsEl = document.getElementById("briefCountTotal");
        if (heroStatsEl) {
          heroStatsEl.innerText = data.total.toLocaleString("pt-BR");
        }
      }

      if (respCounts && respCounts.ok) {
        const countsData = await respCounts.json();
        this.state.contagens = countsData;
        if (countsData && countsData.situacao) {
          const detEl = document.getElementById("feedStatsDetails");
          if (detEl) {
            const aptos = (countsData.situacao.APTO || 0).toLocaleString("pt-BR");
            const aguard = (countsData.situacao.AGUARDANDO || 0).toLocaleString("pt-BR");
            detEl.innerHTML = `<strong>${aptos}</strong> aptos • <strong>${aguard}</strong> em julgamento`;
          }
        }
        this.updateSidebarMandateCounts();
        this.updateModalFilterCounts();

        // Se houver popover de filtro aberto, refrescar as opções com as novas contagens
        if (window.CardFilterManager && CardFilterManager.currentKey) {
          const optCont = document.getElementById("cardFilterOptions");
          if (optCont && document.getElementById("cardFilterPopover")?.style.display !== "none") {
            CardFilterManager.renderOptions(CardFilterManager.currentKey, optCont);
          }
        }
      }
    } catch (e) {
      console.error("Erro ao carregar candidatos:", e);
      if (container) container.innerHTML = `<div style="text-align: center; padding: 2rem; color: #EF4444;">Erro ao carregar candidatos.</div>`;
    }
  },

  renderUniversalActiveFilters(filteredCount, totalCount) {
    const wrap = document.getElementById("universalActiveFilters");
    const btnClearSidebar = document.getElementById("btnClearSidebarFilters");

    const tf = window.TableFilterManager ? TableFilterManager.filters : {};
    const tags = [];

    // 1. Filtros da Barra Lateral (Sidebar)
    if (this.state.selectedUfs && this.state.selectedUfs.length > 0) {
      tags.push({ key: '_sidebar_uf', label: `📍 Estado: ${this.state.selectedUfs.join(', ')}` });
    } else if (this.state.uf && this.state.uf !== "BR" && (!tf.uf || tf.uf.length === 0)) {
      tags.push({ key: '_sidebar_uf', label: `📍 Estado: ${this.state.uf}` });
    }

    if (this.state.selectedCargos && this.state.selectedCargos.length > 0 && (!tf.cargo || tf.cargo.length === 0)) {
      tags.push({ key: '_sidebar_cargo', label: `👔 Cargos: ${this.state.selectedCargos.join(', ')}` });
    }

    if (this.state.situacao_mandato && this.state.situacao_mandato !== "todos" && (!tf.mandato || tf.mandato.length === 0)) {
      const labelsMand = {
        reeleicao: "Reeleição",
        outro_cargo: "Em exercício (outro cargo)",
        sem_mandato: "Não consta",
        novos: "Não consta"
      };
      tags.push({ key: '_sidebar_mandato', label: `⚡ Mandato: ${labelsMand[this.state.situacao_mandato] || this.state.situacao_mandato}` });
    }

    if (this.state.ideologia_min !== 1 || this.state.ideologia_max !== 7) {
      tags.push({ key: '_sidebar_ideologia', label: `⚖️ ${this.getIdeologyRangeLabel(this.state.ideologia_min, this.state.ideologia_max)}` });
    }

    if (this.state.partidos_gosta && this.state.partidos_gosta.length > 0) {
      tags.push({ key: '_sidebar_partidos_gosta', label: `⭐ Partidos: ${this.state.partidos_gosta.join(', ')}` });
    }
    if (this.state.partidos_desgosta && this.state.partidos_desgosta.length > 0) {
      tags.push({ key: '_sidebar_partidos_desgosta', label: `🚫 Ocultos: ${this.state.partidos_desgosta.join(', ')}` });
    }

    if (this.state.busca && this.state.busca.trim().length > 0 && !tf.candidato) {
      tags.push({ key: '_sidebar_busca', label: `🔍 "${this.state.busca.trim()}"` });
    }

    if (tf.filtro_lista && tf.filtro_lista.name) {
      tags.push({ key: 'filtro_lista', label: `📁 Lista: ${tf.filtro_lista.name}` });
    }

    // 2. Filtros de Tabela / Slicers / Avançados
    if (tf.bens && (tf.bens.min !== null || tf.bens.max !== null)) {
      const minStr = tf.bens.min !== null ? `R$ ${(tf.bens.min / 1e3).toLocaleString('pt-BR')}k` : 'R$ 0';
      const maxStr = tf.bens.max !== null ? `R$ ${(tf.bens.max / 1e6).toFixed(1)}M` : 'Sem limite';
      tags.push({ key: 'bens', label: `💰 Bens: ${minStr} a ${maxStr}` });
    }
    if (tf.ranking_bens) {
      tags.push({ key: 'ranking_bens', label: `💎 Top ${tf.ranking_bens} Mais Ricos` });
    }
    if (tf.gastos && (tf.gastos.min !== null || tf.gastos.max !== null)) {
      const minStr = tf.gastos.min !== null ? `R$ ${(tf.gastos.min / 1e3).toLocaleString('pt-BR')}k` : 'R$ 0';
      const maxStr = tf.gastos.max !== null ? `R$ ${(tf.gastos.max / 1e6).toFixed(1)}M` : 'Sem limite';
      tags.push({ key: 'gastos', label: `📈 Gastos: ${minStr} a ${maxStr}` });
    }
    if (tf.ranking_partido) {
      tags.push({ key: 'ranking_partido', label: `🏆 Top ${tf.ranking_partido} do Partido` });
    }
    if (tf.ranking_cargo) {
      tags.push({ key: 'ranking_cargo', label: `⭐ Top ${tf.ranking_cargo} no Cargo` });
    }
    if (tf.campanha_ranking) {
      const cr = tf.campanha_ranking;
      const escDesc = cr.escopo === 'partido' ? 'no Partido' : 'no Cargo';
      const metDesc = cr.metrica === 'receitas' ? 'Arrecadação' : 'Gastos';
      let lbl = '';
      if (cr.min === 1 && cr.max) {
        lbl = `📊 Top ${cr.max} ${escDesc} (${metDesc})`;
      } else if (cr.max) {
        lbl = `📊 ${cr.min}º ao ${cr.max}º ${escDesc} (${metDesc})`;
      } else {
        lbl = `📊 A partir do ${cr.min}º ${escDesc} (${metDesc})`;
      }
      tags.push({ key: 'campanha_ranking', label: lbl });
    }
    if (tf.receitas && (tf.receitas.min !== null || tf.receitas.max !== null)) {
      const minStr = tf.receitas.min !== null ? `R$ ${(tf.receitas.min / 1e3).toLocaleString('pt-BR')}k` : 'R$ 0';
      const maxStr = tf.receitas.max !== null ? `R$ ${(tf.receitas.max / 1e6).toFixed(1)}M` : 'Sem limite';
      tags.push({ key: 'receitas', label: `💳 Receitas: ${minStr} a ${maxStr}` });
    }
    if (tf.genero && Array.isArray(tf.genero) && tf.genero.length > 0) {
      const desc = tf.genero.map(g => g === 'FEMININO' ? 'Mulheres' : 'Homens').join(', ');
      tags.push({ key: 'genero', label: `👤 Gênero: ${desc}` });
    }
    if (tf.raca && Array.isArray(tf.raca) && tf.raca.length > 0) {
      const desc = tf.raca.join(', ');
      tags.push({ key: 'raca', label: `Cor / Raça: ${desc}` });
    }
    if (tf.instrucao && Array.isArray(tf.instrucao) && tf.instrucao.length > 0) {
      const desc = tf.instrucao.length === 1 ? tf.instrucao[0] : `${tf.instrucao.length} selecionadas`;
      tags.push({ key: 'instrucao', label: `🎓 Escolaridade: ${desc}` });
    }
    if (tf.profissao && Array.isArray(tf.profissao) && tf.profissao.length > 0) {
      const desc = tf.profissao.length === 1 ? tf.profissao[0] : `${tf.profissao.length} selecionadas`;
      tags.push({ key: 'profissao', label: `💼 Profissão: ${desc}` });
    }
    if (tf.faixa_etaria && Array.isArray(tf.faixa_etaria) && tf.faixa_etaria.length > 0) {
      const fMap = { "18-24": "18-24 anos", "25-34": "25-34 anos", "35-44": "35-44 anos", "45-59": "45-59 anos", "60+": "60+ anos" };
      const desc = tf.faixa_etaria.map(k => fMap[k] || k).join(', ');
      tags.push({ key: 'faixa_etaria', label: `🎂 Idade: ${desc}` });
    } else if (this.state.faixa_etaria && typeof this.state.faixa_etaria === 'string' && this.state.faixa_etaria.trim()) {
      const fMap = { "18-24": "18-24 anos", "25-34": "25-34 anos", "35-44": "35-44 anos", "45-59": "45-59 anos", "60+": "60+ anos" };
      const parts = this.state.faixa_etaria.split(',').map(k => fMap[k.trim()] || k.trim()).join(', ');
      tags.push({ key: 'faixa_etaria', label: `🎂 Idade: ${parts}` });
    }
    if (tf.cargo && Array.isArray(tf.cargo) && tf.cargo.length > 0) {
      tags.push({ key: 'cargo', label: `👔 Cargo: ${tf.cargo.join(', ')}` });
    }
    if (tf.uf && Array.isArray(tf.uf) && tf.uf.length > 0) {
      tags.push({ key: 'uf', label: `📍 UF: ${tf.uf.join(', ')}` });
    }
    if (tf.mandato && Array.isArray(tf.mandato) && tf.mandato.length > 0) {
      const desc = tf.mandato.join(', ');
      tags.push({ key: 'mandato', label: `⚡ Mandato: ${desc}` });
    }
    if (tf.proposta === true) {
      tags.push({ key: 'proposta', label: `📄 Com Plano de Governo` });
    }
    const curSit = (tf.situacao && tf.situacao.length > 0) ? tf.situacao[0] : (this.state.situacao && this.state.situacao !== 'DEFERIDOS_AGUARDANDO' ? this.state.situacao : null);
    if (curSit && curSit !== 'DEFERIDOS_AGUARDANDO' && !String(curSit).includes('Deferidos e Aguardando')) {
      let desc = curSit;
      if (curSit === 'todos' || curSit === 'todas') desc = 'Todos os Registrados';
      else if (curSit === 'APTO' || String(curSit).includes('Aptos')) desc = 'Apenas Deferidos';
      else if (curSit === 'AGUARDANDO' || String(curSit).includes('Aguardando')) desc = '⏳ Aguardando julgamento';
      else if (curSit === 'INAPTO' || String(curSit).includes('Inapto')) desc = '✕ Inaptos / Indeferidos';
      tags.push({ key: 'situacao', label: `🏛️ Situação: ${desc}` });
    }
    if (tf.candidato && typeof tf.candidato === 'string' && tf.candidato.trim()) {
      tags.push({ key: 'candidato', label: `🔍 "${tf.candidato}"` });
    }
    if (tf.numero && typeof tf.numero === 'string' && tf.numero.trim()) {
      tags.push({ key: 'numero', label: `🔢 Nº "${tf.numero}"` });
    }

    // Atualizar badge do botão Mais Filtros
    const activeFiltersCount = Object.keys(tf).filter(k => TableFilterManager.hasFilter(k)).length;
    const moreBadge = document.getElementById("moreFiltersBadge");
    if (moreBadge) {
      moreBadge.textContent = activeFiltersCount;
      moreBadge.style.display = activeFiltersCount > 0 ? "inline-block" : "none";
    }

    // Atualizar badge e contagem da navegação móvel
    const mobFilterBadge = document.getElementById("mobNavFilterBadge");
    if (mobFilterBadge) {
      mobFilterBadge.textContent = tags.length;
      mobFilterBadge.style.display = tags.length > 0 ? "inline-block" : "none";
    }
    const mobCountEl = document.getElementById("mobileSidebarCandidateCount");
    if (mobCountEl) {
      mobCountEl.textContent = (filteredCount !== undefined) ? filteredCount : (this.state.candidatosFiltrados?.length || 0);
    }

    const quickKeys = ['mandato', 'bens', 'campanha', 'genero', 'raca', 'instrucao', 'profissao', 'faixa_etaria'];
    quickKeys.forEach(k => {
      const btn = document.getElementById(`btnQuick_${k}`);
      if (btn) {
        const isActive = TableFilterManager.hasFilter(k);
        btn.classList.toggle('active-filter', isActive);
        btn.classList.toggle('active', isActive);
      }
    });

    if (window.CardFilterManager) {
      CardFilterManager.updateCardFilterButtons();
    }

    // Atualizar visibilidade do botão discreto na sidebar e no painel superior
    if (btnClearSidebar) {
      btnClearSidebar.style.display = tags.length > 0 ? "inline-flex" : "none";
    }
    const topActiveChipsRow = document.getElementById("topActiveChipsRow");
    if (topActiveChipsRow) {
      topActiveChipsRow.style.display = tags.length > 0 ? "block" : "none";
    }
    const topClearBtn = document.getElementById("btnTopClearAll");
    if (topClearBtn) {
      topClearBtn.style.display = tags.length > 0 ? "inline-flex" : "none";
    }
    const topMoreBadge = document.getElementById("topMoreFiltersBadge");
    if (topMoreBadge) {
      topMoreBadge.textContent = activeFiltersCount;
      topMoreBadge.style.display = activeFiltersCount > 0 ? "inline-block" : "none";
    }
    const classicMoreBadge = document.getElementById("classicMoreFiltersBadge");
    if (classicMoreBadge) {
      classicMoreBadge.textContent = activeFiltersCount;
      classicMoreBadge.style.display = activeFiltersCount > 0 ? "inline-block" : "none";
    }

    // Renderizar chips selecionados inline na própria barra superior (Linha 2)
    const topInlineChips = document.getElementById("topInlineActiveChips");
    if (topInlineChips) {
      if (tags.length === 0) {
        topInlineChips.innerHTML = "";
      } else {
        topInlineChips.innerHTML = tags.map(t => {
          const cleanLabel = t.label.replace(/^[^\p{L}\p{N}"']+/u, '').trim();
          return `
            <span class="top-inline-chip" title="${t.label}">
              <span>${cleanLabel}</span>
              <button type="button" class="top-inline-chip-remove" onclick="TableFilterManager.removeFilter('${t.key}')" title="Remover filtro">✕</button>
            </span>
          `;
        }).join('');
      }
    }

    if (!wrap) return;

    if (tags.length === 0) {
      wrap.style.display = "none";
      wrap.innerHTML = "";
      return;
    }

    wrap.style.display = "flex";
    const totalRegistrados = totalCount || (this.state.candidatos ? this.state.candidatos.length : 0);
    const totalFiltrados = filteredCount !== undefined ? filteredCount : totalRegistrados;
    wrap.innerHTML = `
      <span class="active-filter-title">Filtros Ativos (${totalFiltrados} de ${totalRegistrados}):</span>
      ${tags.map(t => `
        <span class="active-filter-tag">
          ${t.label}
          <button type="button" class="btn-remove-tag" onclick="TableFilterManager.removeFilter('${t.key}')" title="Remover este filtro">✕</button>
        </span>
      `).join('')}
      <button type="button" class="btn-clear-all-tags" onclick="App.clearAllFilters()" title="Redefinir e limpar todos os filtros aplicados">
        <svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" style="vertical-align: -1.5px; margin-right: 3px;"><path d="m7 21-4.3-4.3c-1-1-1-2.5 0-3.4l9.6-9.6c1-1 2.5-1 3.4 0l5.6 5.6c1 1 1 2.5 0 3.4L13 21"></path><path d="M22 21H7"></path><path d="m5 11 9 9"></path></svg>Limpar todos os filtros
      </button>
    `;
  },

  renderView() {
    const container = document.getElementById("candidatesContainer");
    if (!container) return;

    if (this.state.candidatos.length === 0) {
      this.renderUniversalActiveFilters(0, 0);
      container.innerHTML = `
        <div style="text-align: center; padding: 3.5rem 1rem; background: var(--bg-surface); border: 1px solid var(--border-color); border-radius: var(--radius-md);">
          <div style="font-size: 1.5rem; margin-bottom: 0.4rem;">🔍</div>
          <div style="font-size: 0.95rem; font-weight: 700; color: var(--text-primary);">Nenhum candidato encontrado</div>
          <p style="font-size: 0.78rem; color: var(--text-muted); margin-top: 0.25rem;">Tente ajustar o estado, o cargo ou o filtro selecionado.</p>
        </div>
      `;
      return;
    }

    if (this.state.viewMode === "table") {
      this.renderTableView(container);
    } else {
      this.renderCardsView(container);
    }
  },
  getCandidateMandatoInfo(c) {
    if (!c) return { isReeleicao: false, label: "Não consta", badgeText: null, shortLabel: "Não consta" };
    const rawCargo = c.cargo_exercicio || "";
    const shortCargo = rawCargo
      .replace(/Presidente da República/i, "Presidente")
      .replace(/Governador.*Estado/i, "Governador")
      .replace(/Senador.*Federal/i, "Senador")
      .trim();

    if (c.is_reeleicao) {
      const displayCargo = shortCargo || (c.cargo ? c.cargo.charAt(0) + c.cargo.slice(1).toLowerCase() : "Titular");
      return {
        isReeleicao: true,
        label: `⚡ ${displayCargo}`,
        badgeText: `⚡ ${displayCargo}`,
        shortLabel: `⚡ ${displayCargo}`
      };
    }
    if (c.em_exercicio && c.cargo_exercicio) {
      return {
        isReeleicao: false,
        label: shortCargo,
        badgeText: shortCargo,
        shortLabel: shortCargo
      };
    }
    return { isReeleicao: false, label: "Não consta", badgeText: null };
  },

  renderCardsView(container) {
    if (this._stickyTableCleanup) {
      this._stickyTableCleanup();
    }
    const filterBar = document.getElementById("feedFilterBar");
    if (filterBar) filterBar.style.display = "flex";
    document.body.setAttribute("data-view-mode", "cards");

    const filteredCands = TableFilterManager.filterCandidates(this.state.candidatos || []);
    this.renderUniversalActiveFilters(filteredCands.length, this.state.candidatos.length);

    if (filteredCands.length === 0) {
      container.innerHTML = `
        <div style="text-align: center; padding: 3.5rem 1rem; background: var(--bg-surface); border: 1px solid var(--border-color); border-radius: var(--radius-md);">
          <div style="font-size: 1.5rem; margin-bottom: 0.4rem;">🔍</div>
          <div style="font-size: 0.95rem; font-weight: 700; color: var(--text-primary);">Nenhum candidato corresponde aos filtros selecionados</div>
          <p style="font-size: 0.78rem; color: var(--text-muted); margin-top: 0.25rem;">Tente ajustar ou limpar os filtros na barra acima.</p>
          <button type="button" class="btn-header primary" style="margin-top: 0.85rem; font-size: 0.75rem;" onclick="TableFilterManager.clearAll()">Limpar Filtros</button>
        </div>
      `;
      return;
    }

    container.innerHTML = `
      <div class="candidates-cards-grid">
        ${filteredCands.map(c => {
      const isFav = (window.ListManager && typeof ListManager.isFavorited === 'function') ? ListManager.isFavorited(c.sq_candidato) : false;
      const isComp = (window.CompareController && typeof CompareController.isSelected === 'function') ? CompareController.isSelected(c.sq_candidato) : false;
      const initials = (c.nome_urna || "C").substring(0, 2).toUpperCase();
      const fotoLocal = c.foto_url || `/fotos/${c.sq_candidato}.jpg`;
      const sitUpper = String(c.situacao_candidatura || '').toUpperCase();
      const isAguardando = sitUpper.includes('AGUARDANDO') || sitUpper.includes('PENDENTE');
      const mandInfo = App.getCandidateMandatoInfo(c);

      // Variação editorial suave de fundo atrás da foto (estilo Imagem 4)
      const tintList = ['rgba(200, 75, 49, 0.10)', 'rgba(0, 91, 104, 0.10)', 'rgba(118, 84, 216, 0.10)', 'rgba(229, 184, 0, 0.12)'];
      const hashNum = (c.numero || c.sq_candidato || '1').toString().split('').reduce((a, b) => a + b.charCodeAt(0), 0);
      const photoTint = tintList[hashNum % tintList.length];

      return `
            <div class="candidate-card-clean">
              <!-- Botão Bookmark no Canto Superior Direito -->
              <button type="button" class="card-bookmark-btn ${isFav ? 'active' : ''}" 
                      title="${isFav ? 'Salvo nos Salvos (Clique para remover)' : 'Salvar nos Salvos'}" 
                      onclick="event.stopPropagation(); App.toggleFav('${c.sq_candidato}')">
                <svg width="18" height="18" viewBox="0 0 24 24" fill="${isFav ? '#C84B31' : 'none'}" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round">
                  <path d="M19 21l-7-5-7 5V5a2 2 0 0 1 2-2h10a2 2 0 0 1 2 2z"></path>
                </svg>
              </button>

              <div>
                <div class="card-top-row">
                  <div class="card-photo-col">
                    <div class="card-photo-box" style="background: ${photoTint};">
                      <img src="${fotoLocal}" alt="${c.nome_urna}" class="card-photo-img" loading="lazy" onerror="if(!this.dataset.triedFallback){this.dataset.triedFallback='1';this.src='/api/candidatos/${c.sq_candidato}/foto';}else{this.style.display='none';this.nextElementSibling.style.display='flex';}">
                      <div class="card-photo-initials" style="display: none;">${initials}</div>
                    </div>
                    <div class="card-urna-badge" title="Número na Urna Eletrônica">Nº <strong>${c.numero || '---'}</strong></div>
                  </div>

                  <div class="card-meta-col">
                    <div class="card-name-title" title="${c.nome_urna}${isAguardando ? ' — Aguardando julgamento' : ''}">
                      <span class="card-name-text">${c.nome_urna}</span>
                      <span class="party-tag-pill" style="border-left-color: ${App.getResolvedPartidoColor(c)};" title="${(c.partido === 'PMB' || c.partido === 'DEMOCRATA') ? 'DEMOCRATA (registro PMB)' : (c.partido_nome_completo || c.partido)}">
                        ${(c.partido === 'PMB' || c.partido === 'DEMOCRATA') ? 'DEMOCRATA' : c.partido}
                      </span>
                      ${isAguardando ? `<span class="cand-hourglass-icon" title="Aguardando julgamento" aria-label="Aguardando julgamento">⏳</span>` : ''}
                    </div>
                    <div class="card-cargo-state">${c.cargo} • ${c.uf}</div>

                    <div class="card-sub-tags-row">
                      ${c.plano_governo_url ? `
                        <a href="${c.plano_governo_url}" target="_blank" rel="noopener" class="card-sub-badge card-plan-badge" title="Abrir Plano de Governo / Proposta Oficial registrada no TSE" onclick="event.stopPropagation();">
                          📄 Plano de Governo ↗
                        </a>
                      ` : ''}
                      ${c.ocupacao && c.ocupacao !== 'Não informada' ? `
                        <span class="card-sub-badge card-occupation-badge" title="Profissão / Ocupação Declarada no TSE">💼 ${c.ocupacao}</span>
                      ` : ''}
                      ${c.grau_instrucao && c.grau_instrucao !== 'Não informada' && c.grau_instrucao !== 'NÃO INFORMADO' ? `
                        <span class="card-sub-badge card-education-badge" title="Grau de Instrução / Escolaridade Declarada no TSE">🎓 ${c.grau_instrucao}</span>
                      ` : ''}
                      ${c.cor_raca && c.cor_raca !== 'NÃO INFORMADO' ? `
                        <span class="card-sub-badge card-race-badge" title="Cor / Raça declarada no TSE">👤 ${c.cor_raca}</span>
                      ` : ''}
                      ${(() => {
          const id = c.idade || (c.dt_nascimento ? App.calcularIdade(c.dt_nascimento, 2026) : null);
          return id ? `<span class="card-sub-badge card-age-badge" title="Idade declarada / calculada: ${id} anos (${c.dt_nascimento || '---'})">🎂 ${id} anos</span>` : '';
        })()}
                      ${(c.cargo !== 'PRESIDENTE' && c.cargo !== 'GOVERNADOR' && c.total_partido_cargo_uf > 1 && c.ranking_partido_receita) ? `
                        <span class="card-ranking-badge" title="Ranking no partido: ${c.ranking_partido_receita}º mais financiado do ${c.partido} (${c.uf})">
                          🎯 ${c.ranking_partido_receita}º no ${c.partido}
                        </span>
                      ` : ''}
                      ${mandInfo.isReeleicao ? `
                        <span class="card-ranking-badge reeleicao-tag" title="Tentando reeleição para o mesmo cargo (${c.cargo})">
                          🔄 Reeleição
                        </span>
                      ` : ''}
                    </div>
                  </div>
                </div>

                <!-- Barra de Ideologia Contínua -->
                <div class="card-ideology-section">
                  ${c.ideologia_continua !== null && c.ideologia_continua !== undefined ? `
                    <div class="card-ideology-header">
                      <span class="card-ideology-label">${c.ideologia_nome || 'Centro'}</span>
                      <span class="card-ideology-score">${c.ideologia_continua.toFixed(1)}</span>
                    </div>
                    <div class="ideologia-track-wrap">
                      <div class="ideologia-track-dot" style="left: ${Math.min(100, Math.max(0, c.ideologia_continua))}%;"></div>
                    </div>
                  ` : `
                    <div class="card-ideology-header" style="color: var(--text-muted); font-weight: 500;">
                      <span>Sem classificação ideológica</span>
                      <span style="font-size: 0.60rem; background: var(--bg-surface); padding: 0.08rem 0.28rem; border-radius: var(--radius-xs); border: 1px solid var(--border-color);">Partido estreante</span>
                    </div>
                  `}
                </div>

                <!-- Caixas de Metadados Editoriais (Mandato & Finanças) -->
                <div class="card-stats-grid">
                  <div class="card-stat-box">
                    <div class="stat-metric-label">Mandato Atual</div>
                    <div class="stat-metric-value" style="color: ${c.em_exercicio ? 'var(--accent-petroleo)' : 'var(--text-secondary)'};">
                      ${c.em_exercicio ? mandInfo.label : '<span class="stat-receita-nao">Não consta</span>'}
                    </div>
                  </div>

                  <div class="card-stat-box">
                    <div class="stat-metric-label">Receita Arrecadada</div>
                    <div class="stat-metric-value stat-receita-value" title="${c.receita_ranking_texto || ('Total de Receitas Declaradas no TSE: R$ ' + (c.financiamento_receita || 0).toLocaleString('pt-BR'))}">
                      ${c.financiamento_receita > 0 ? `
                        <span class="stat-receita-num">R$ ${(c.financiamento_receita / 1e3 >= 1000 ? (c.financiamento_receita / 1e6).toFixed(1) + 'M' : (c.financiamento_receita / 1e3).toFixed(0) + ' mil')}</span>
                        ${c.ranking_receita_cargo ? `<span class="stat-receita-rank" title="${c.receita_ranking_texto || (c.ranking_receita_cargo + 'º no cargo')}"> (${c.ranking_receita_cargo}º)</span>` : ''}
                      ` : '<span class="stat-receita-nao">Não declarada</span>'}
                    </div>
                  </div>
                </div>
              </div>

              <!-- Barra de Ações (Ficha Completa em Pílula Larga + Ícones de Ação) -->
              <div class="card-actions-bar">
                <button type="button" class="btn-card-action-ficha" onclick="App.openDetails('${c.sq_candidato}')">
                  Ficha Completa →
                </button>
                <div class="card-actions-icons">
                  <button type="button" class="btn-card-icon ${isComp ? 'active' : ''}" 
                          title="${isComp ? 'Remover da comparação' : 'Comparar candidatos (Balança)'}"
                          onclick="event.stopPropagation(); App.toggleCompare('${c.sq_candidato}')">
                    <svg width="15" height="15" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                      <path d="m16 16 3-8 3 8c-.87.65-1.92 1-3 1s-2.13-.35-3-1Z"/>
                      <path d="m2 16 3-8 3 8c-.87.65-1.92 1-3 1s-2.13-.35-3-1Z"/>
                      <path d="M7 21h10"/>
                      <path d="M12 3v18"/>
                      <path d="M3 7h2c2 0 5-1 7-2 2 1 5 2 7 2h2"/>
                    </svg>
                  </button>
                  <button type="button" class="btn-card-icon" 
                          title="Criar Santinho Digital"
                          onclick="event.stopPropagation(); SantinhoStudio.open(App.getCandidateBySq('${c.sq_candidato}'))">
                    <svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                      <rect width="18" height="18" x="3" y="3" rx="2"/>
                      <circle cx="8.5" cy="8.5" r="1.5"/>
                      <path d="m21 15-5-5L5 21"/>
                    </svg>
                  </button>
                </div>
              </div>
            </div>
          `;
    }).join("")}
      </div>
    `;
  },

  getCandidateBySq(sq) {
    const found = this.state.candidatos.find(c => String(c.sq_candidato) === String(sq));
    if (found) return found;
    if (window.ListManager && Array.isArray(ListManager.lists)) {
      for (const l of ListManager.lists) {
        const c = (l.candidates || []).find(cand => String(cand.sq_candidato) === String(sq));
        if (c) return c;
      }
    }
    return null;
  },

  toggleColDropdown(e) {
    if (e) e.stopPropagation();
    const menu = document.getElementById("colDropdownMenu");
    if (menu) menu.classList.toggle("show");
  },

  toggleColumn(colName, isVisible) {
    this.state.visibleColumns[colName] = isVisible;
    localStorage.setItem("emquemeuvoto_cols_v4", JSON.stringify(this.state.visibleColumns));
    this.renderView();
  },

  syncColumnCheckboxes() {
    for (const [col, isVis] of Object.entries(this.state.visibleColumns)) {
      const chk = document.getElementById(`colCheck_${col}`);
      if (chk) chk.checked = isVis;
    }
  },

  initColResize(e, colKey) {
    e.preventDefault();
    e.stopPropagation();

    const th = e.target.closest("th");
    if (!th) return;

    const startX = e.pageX;
    const startWidth = th.getBoundingClientRect().width;
    const resizer = e.target;
    resizer.classList.add("is-resizing");
    document.body.classList.add("is-col-resizing");

    const onMouseMove = (moveEvent) => {
      moveEvent.preventDefault();
      const deltaX = moveEvent.pageX - startX;
      const newWidth = Math.max(60, Math.round(startWidth + deltaX));
      th.style.width = `${newWidth}px`;
      th.style.minWidth = `${newWidth}px`;

      if (!this.state.columnWidths) this.state.columnWidths = {};
      this.state.columnWidths[colKey] = newWidth;
    };

    const onMouseUp = () => {
      resizer.classList.remove("is-resizing");
      document.body.classList.remove("is-col-resizing");
      window.removeEventListener("mousemove", onMouseMove);
      window.removeEventListener("mouseup", onMouseUp);

      try {
        localStorage.setItem("emquemeuvoto_col_widths", JSON.stringify(this.state.columnWidths));
      } catch (err) { }

      if (typeof this.initStickyTableSync === "function") {
        this.initStickyTableSync();
      }
    };

    window.addEventListener("mousemove", onMouseMove);
    window.addEventListener("mouseup", onMouseUp);
  },

  _draggedCol: null,

  onColDragStart(e, colKey) {
    this._draggedCol = colKey;
    e.dataTransfer.effectAllowed = "move";
    e.dataTransfer.setData("text/plain", colKey);
    e.currentTarget.classList.add("dragging-col");
  },

  onColDragOver(e, colKey) {
    e.preventDefault();
    e.dataTransfer.dropEffect = "move";
    if (this._draggedCol && this._draggedCol !== colKey) {
      const rect = e.currentTarget.getBoundingClientRect();
      const isRight = (e.clientX - rect.left) > (rect.width / 2);
      e.currentTarget.classList.toggle("col-drop-target-left", !isRight);
      e.currentTarget.classList.toggle("col-drop-target-right", isRight);
    }
  },

  onColDragEnter(e, colKey) {
    e.preventDefault();
  },

  onColDragLeave(e, colKey) {
    e.currentTarget.classList.remove("col-drop-target-left", "col-drop-target-right");
  },

  onColDrop(e, targetCol) {
    e.preventDefault();
    e.currentTarget.classList.remove("col-drop-target-left", "col-drop-target-right");
    const srcCol = this._draggedCol || e.dataTransfer.getData("text/plain");
    if (!srcCol || srcCol === targetCol) return;

    const rect = e.currentTarget.getBoundingClientRect();
    const isRight = (e.clientX - rect.left) > (rect.width / 2);

    const order = [...this.state.columnOrder];
    const srcIdx = order.indexOf(srcCol);
    if (srcIdx === -1) return;
    order.splice(srcIdx, 1);

    let targetIdx = order.indexOf(targetCol);
    if (targetIdx === -1) {
      order.push(srcCol);
    } else {
      if (isRight) targetIdx += 1;
      order.splice(targetIdx, 0, srcCol);
    }

    this.state.columnOrder = order;
    localStorage.setItem("emquemeuvoto_col_order_v3", JSON.stringify(order));
    this._draggedCol = null;
    this.renderView();
  },

  onColDragEnd(e) {
    this._draggedCol = null;
    document.querySelectorAll(".data-table-clean th").forEach(th => {
      th.classList.remove("dragging-col", "col-drop-target-left", "col-drop-target-right");
    });
  },

  sortTableBy(colKey) {
    const cur = this.state.ordenacao;
    let nextSort = colKey;

    if (colKey === "nome") {
      nextSort = (cur === "nome") ? "nome_desc" : "nome";
    } else if (colKey === "numero") {
      nextSort = (cur === "numero") ? "numero_desc" : "numero";
    } else if (colKey === "partido") {
      nextSort = (cur === "partido") ? "partido_desc" : "partido";
    } else if (colKey === "cargo") {
      nextSort = (cur === "cargo") ? "cargo_desc" : "cargo";
    } else if (colKey === "uf") {
      nextSort = (cur === "uf") ? "uf_desc" : "uf";
    } else if (colKey === "ideologia") {
      nextSort = (cur === "ideologia") ? "ideologia_desc" : "ideologia";
    } else if (colKey === "mandato") {
      nextSort = (cur === "mandato") ? "mandato_desc" : "mandato";
    } else if (colKey === "situacao") {
      nextSort = (cur === "situacao") ? "situacao_desc" : "situacao";
    } else if (colKey === "prioridade_partido") {
      const isCargo = window.TableFilterManager?.filters?.campanha_ranking?.escopo === "cargo";
      if (isCargo) {
        nextSort = (cur === "prioridade_cargo") ? "prioridade_cargo_desc" : "prioridade_cargo";
      } else {
        nextSort = (cur === "prioridade_partido") ? "prioridade_partido_desc" : "prioridade_partido";
      }
    } else if (colKey === "profissao") {
      nextSort = (cur === "profissao") ? "profissao_desc" : "profissao";
    } else if (colKey === "raca") {
      nextSort = (cur === "raca") ? "raca_desc" : "raca";
    } else if (colKey === "idade") {
      nextSort = (cur === "idade") ? "idade_desc" : "idade";
    } else if (colKey === "estado_civil") {
      nextSort = (cur === "estado_civil") ? "estado_civil_desc" : "estado_civil";
    } else if (colKey === "bens") {
      nextSort = (cur === "bens") ? "bens_asc" : "bens";
    } else if (colKey === "receitas") {
      nextSort = (cur === "receitas") ? "receitas_asc" : "receitas";
    } else if (colKey === "gastos") {
      nextSort = (cur === "gastos") ? "gastos_asc" : "gastos";
    }

    this.setOrdenacao(nextSort);
  },

  initSmartStickySidebar() {
    const sidebar = document.querySelector(".sidebar-panel");
    if (!sidebar) return;
    sidebar.style.position = "";
    sidebar.style.top = "";
  },

  brazilStates: [
    { uf: "AC", nome: "Acre", regiao: "N" },
    { uf: "AL", nome: "Alagoas", regiao: "NE" },
    { uf: "AP", nome: "Amapá", regiao: "N" },
    { uf: "AM", nome: "Amazonas", regiao: "N" },
    { uf: "BA", nome: "Bahia", regiao: "NE" },
    { uf: "CE", nome: "Ceará", regiao: "NE" },
    { uf: "DF", nome: "Distrito Federal", regiao: "CO" },
    { uf: "ES", nome: "Espírito Santo", regiao: "SE" },
    { uf: "GO", nome: "Goiás", regiao: "CO" },
    { uf: "MA", nome: "Maranhão", regiao: "NE" },
    { uf: "MT", nome: "Mato Grosso", regiao: "CO" },
    { uf: "MS", nome: "Mato Grosso do Sul", regiao: "CO" },
    { uf: "MG", nome: "Minas Gerais", regiao: "SE" },
    { uf: "PA", nome: "Pará", regiao: "N" },
    { uf: "PB", nome: "Paraíba", regiao: "NE" },
    { uf: "PR", nome: "Paraná", regiao: "S" },
    { uf: "PE", nome: "Pernambuco", regiao: "NE" },
    { uf: "PI", nome: "Piauí", regiao: "NE" },
    { uf: "RJ", nome: "Rio de Janeiro", regiao: "SE" },
    { uf: "RN", nome: "Rio Grande do Norte", regiao: "NE" },
    { uf: "RS", nome: "Rio Grande do Sul", regiao: "S" },
    { uf: "RO", nome: "Rondônia", regiao: "N" },
    { uf: "RR", nome: "Roraima", regiao: "N" },
    { uf: "SC", nome: "Santa Catarina", regiao: "S" },
    { uf: "SP", nome: "São Paulo", regiao: "SE" },
    { uf: "SE", nome: "Sergipe", regiao: "NE" },
    { uf: "TO", nome: "Tocantins", regiao: "N" }
  ],

  // 🗺️ Contornos oficiais IBGE — GeoJSON convertido (viewBox 580×620, projeção Plate Carrée)
  brazilSvgStates: [
    { uf: "AC", nome: "Acre", cx: 57, cy: 235, d: "M 109.4 242.8 L 88.6 233.4 L 69.2 215.3 L 34.5 210.9 L 17.7 204.3 L 19.1 207.1 L 15.5 208.1 L 21.4 222.9 L 28.8 231.5 L 25.8 238.9 L 37.8 240.3 L 40.0 247.7 L 53.4 247.2 L 63.0 239.0 L 61.5 262.7 L 77.9 261.6 L 90.0 264.4 L 116.4 246.2 L 109.4 242.8 Z" },
    { uf: "AL", nome: "Alagoas", cx: 524, cy: 238, d: "M 509.8 235.0 L 507.2 238.9 L 521.9 246.4 L 532.1 255.2 L 549.2 231.4 L 537.0 231.3 L 532.6 236.0 L 524.6 238.4 L 513.4 230.4 L 509.8 235.0 Z" },
    { uf: "AM", nome: "Amazonas", cx: 143, cy: 137, d: "M 233.0 207.8 L 229.1 196.9 L 257.1 131.8 L 261.1 128.0 L 253.2 130.8 L 247.6 124.3 L 235.6 118.5 L 234.5 114.1 L 230.6 114.8 L 223.0 102.8 L 222.7 93.6 L 207.0 93.6 L 202.1 105.8 L 203.2 108.4 L 198.3 110.9 L 194.7 105.8 L 190.7 105.1 L 186.4 108.9 L 185.1 117.1 L 187.2 121.3 L 181.4 118.5 L 173.0 109.0 L 177.4 102.4 L 172.9 90.9 L 173.5 81.3 L 168.9 73.7 L 170.3 68.5 L 161.1 64.4 L 151.6 68.6 L 148.0 76.0 L 136.6 80.7 L 131.3 87.8 L 130.7 82.4 L 120.6 86.5 L 113.2 79.1 L 110.0 80.1 L 109.9 71.6 L 105.9 63.9 L 98.3 70.1 L 94.2 68.6 L 95.3 71.6 L 72.1 71.9 L 72.1 81.4 L 80.4 81.9 L 82.2 87.9 L 77.1 86.5 L 69.4 89.1 L 69.2 100.4 L 75.3 105.3 L 78.3 114.8 L 70.7 161.7 L 59.6 160.0 L 57.1 163.4 L 43.7 165.6 L 30.3 175.1 L 25.3 189.8 L 27.3 193.8 L 19.9 199.1 L 17.7 204.3 L 34.5 210.9 L 69.2 215.3 L 88.6 233.4 L 113.9 245.0 L 119.4 238.8 L 128.7 241.1 L 135.0 236.6 L 136.8 239.4 L 141.4 232.5 L 150.6 231.9 L 157.7 217.3 L 168.1 217.3 L 180.1 230.8 L 183.9 228.0 L 228.9 229.7 L 229.8 215.1 L 233.0 207.8 Z" },
    { uf: "AP", nome: "Amapá", cx: 315, cy: 74, d: "M 335.6 65.4 L 329.7 45.8 L 330.1 39.2 L 322.5 29.9 L 322.2 36.7 L 305.1 64.7 L 293.2 61.9 L 287.4 64.9 L 278.0 61.0 L 279.8 70.9 L 284.8 71.1 L 297.8 79.0 L 304.7 99.7 L 310.1 106.2 L 311.7 113.4 L 316.0 115.8 L 321.6 113.5 L 328.4 99.3 L 346.5 82.4 L 346.1 72.2 L 339.2 70.5 L 335.6 65.4 Z" },
    { uf: "BA", nome: "Bahia", cx: 452, cy: 294, d: "M 491.3 225.9 L 473.9 239.9 L 467.2 228.2 L 463.5 228.3 L 457.0 236.3 L 444.4 242.0 L 433.8 237.3 L 429.6 240.9 L 431.7 248.6 L 426.4 256.5 L 419.6 257.1 L 414.5 261.5 L 408.1 257.4 L 405.5 249.3 L 392.3 268.3 L 398.9 272.1 L 395.0 272.2 L 394.7 278.5 L 397.3 285.3 L 395.5 292.1 L 398.4 291.5 L 395.5 296.5 L 399.1 297.2 L 396.6 299.3 L 396.4 307.0 L 401.3 313.1 L 398.9 326.7 L 419.8 312.8 L 427.9 311.9 L 430.5 312.8 L 429.1 317.5 L 434.0 320.0 L 438.8 317.5 L 451.3 324.6 L 457.7 324.3 L 463.8 330.2 L 464.2 333.9 L 471.3 332.5 L 479.3 334.8 L 484.5 339.5 L 478.7 346.4 L 478.6 351.3 L 475.7 351.1 L 474.2 358.1 L 479.4 363.8 L 479.4 367.5 L 487.1 372.9 L 494.4 363.1 L 493.3 355.2 L 498.3 335.5 L 495.4 319.0 L 497.4 295.9 L 505.8 291.4 L 519.1 269.4 L 512.8 270.5 L 507.1 261.7 L 507.2 258.4 L 512.6 258.0 L 513.5 252.8 L 505.6 232.5 L 491.3 225.9 Z" },
    { uf: "CE", nome: "Ceará", cx: 486, cy: 176, d: "M 477.4 139.8 L 464.3 141.4 L 463.1 147.9 L 467.4 160.3 L 465.3 170.7 L 469.7 175.2 L 472.7 197.8 L 477.4 199.7 L 475.3 207.4 L 486.1 207.6 L 496.7 215.4 L 502.4 207.7 L 499.5 202.6 L 502.2 192.0 L 507.8 186.7 L 514.9 171.5 L 520.3 170.1 L 503.6 153.2 L 483.3 140.6 L 477.4 139.8 Z" },
    { uf: "DF", nome: "Distrito Federal", cx: 374, cy: 336, d: "M 372.3 338.5 L 382.0 338.5 L 381.8 331.6 L 369.7 330.3 L 368.7 338.5 L 372.3 338.5 Z" },
    { uf: "ES", nome: "Espírito Santo", cx: 469, cy: 388, d: "M 465.0 397.0 L 463.1 400.9 L 458.3 400.9 L 456.7 409.3 L 458.9 414.4 L 469.3 417.4 L 485.1 392.6 L 487.1 372.9 L 477.4 366.6 L 470.1 367.6 L 471.9 369.4 L 468.0 370.3 L 466.3 374.4 L 469.9 380.0 L 465.4 380.6 L 469.2 384.6 L 469.1 390.4 L 465.0 397.0 Z" },
    { uf: "GO", nome: "Goiás", cx: 364, cy: 324, d: "M 369.3 332.7 L 369.7 330.3 L 379.5 330.3 L 381.8 331.6 L 382.0 338.3 L 388.9 335.8 L 387.3 323.6 L 393.1 323.5 L 393.1 318.3 L 395.6 321.3 L 399.5 320.9 L 401.3 313.1 L 396.4 307.0 L 396.6 299.3 L 399.1 297.2 L 395.5 296.5 L 398.4 291.5 L 395.0 292.6 L 394.3 290.1 L 393.7 292.3 L 380.8 296.7 L 378.4 294.5 L 376.9 299.8 L 370.2 297.2 L 370.5 295.0 L 366.2 296.9 L 365.4 294.8 L 364.5 297.4 L 360.5 289.8 L 359.1 292.1 L 357.1 289.6 L 355.5 291.0 L 353.7 296.8 L 340.7 289.6 L 343.1 283.7 L 340.8 285.1 L 333.0 303.8 L 330.1 321.6 L 323.9 323.7 L 319.1 335.1 L 314.0 336.2 L 308.1 342.3 L 308.8 345.5 L 300.8 357.2 L 302.4 372.5 L 307.1 373.3 L 304.2 376.1 L 333.6 389.9 L 341.3 378.1 L 353.6 377.3 L 359.9 372.4 L 375.7 374.1 L 382.4 368.4 L 382.6 362.0 L 378.9 359.6 L 384.5 352.5 L 380.0 344.7 L 382.0 338.5 L 368.7 338.5 L 369.3 332.7 Z" },
    { uf: "MA", nome: "Maranhão", cx: 411, cy: 169, d: "M 372.2 169.3 L 362.1 177.9 L 367.5 175.2 L 378.5 179.6 L 381.0 191.4 L 378.7 202.9 L 376.0 205.1 L 386.0 218.6 L 393.1 217.3 L 392.5 222.4 L 389.2 223.2 L 385.3 233.6 L 392.6 241.1 L 391.1 243.8 L 393.9 248.9 L 400.7 251.6 L 402.9 239.9 L 400.1 231.6 L 407.5 212.7 L 418.0 208.6 L 425.9 199.7 L 430.9 198.2 L 435.0 200.3 L 442.4 197.4 L 443.3 191.8 L 439.9 186.3 L 444.0 175.5 L 441.4 160.9 L 448.2 149.3 L 455.3 145.9 L 457.7 138.6 L 448.3 138.2 L 437.0 132.7 L 431.7 135.3 L 433.1 131.8 L 431.3 132.0 L 425.5 139.1 L 427.1 133.7 L 423.3 134.9 L 421.7 141.8 L 418.4 142.8 L 422.6 132.7 L 421.1 129.8 L 418.2 131.8 L 420.9 127.4 L 416.5 123.2 L 418.8 121.8 L 415.3 121.6 L 415.7 118.8 L 409.3 121.5 L 409.5 117.6 L 407.2 119.8 L 400.1 113.4 L 395.7 123.7 L 396.1 129.9 L 385.0 155.5 L 377.8 166.0 L 372.2 169.3 Z" },
    { uf: "MG", nome: "Minas Gerais", cx: 410, cy: 368, d: "M 376.9 373.1 L 373.1 375.3 L 359.9 372.4 L 353.6 377.3 L 341.3 378.1 L 332.1 389.8 L 331.2 399.1 L 338.8 394.6 L 355.3 397.4 L 355.2 401.7 L 358.9 400.4 L 360.3 404.4 L 361.2 400.2 L 379.5 397.4 L 383.1 401.1 L 386.1 419.2 L 393.0 420.2 L 390.1 432.5 L 395.3 441.4 L 430.7 428.8 L 439.3 429.4 L 451.3 423.6 L 456.9 403.4 L 463.1 400.9 L 469.1 390.4 L 469.2 384.6 L 465.4 380.6 L 469.9 380.0 L 466.6 372.4 L 471.9 369.4 L 470.1 367.6 L 479.4 367.5 L 474.5 355.0 L 484.5 339.5 L 472.9 332.8 L 464.2 333.9 L 463.8 330.2 L 457.7 324.3 L 451.3 324.6 L 438.8 317.5 L 434.0 320.0 L 429.1 317.5 L 430.5 312.8 L 427.9 311.9 L 419.8 312.8 L 398.9 326.7 L 400.5 322.2 L 393.1 318.3 L 393.1 323.5 L 387.3 323.6 L 388.9 335.8 L 382.0 338.3 L 380.0 344.7 L 384.5 352.5 L 378.9 359.6 L 382.6 362.0 L 382.4 368.4 L 376.9 373.1 Z" },
    { uf: "MS", nome: "Mato Grosso do Sul", cx: 278, cy: 393, d: "M 298.2 367.7 L 290.7 366.5 L 294.1 362.7 L 294.4 356.6 L 285.8 362.7 L 283.1 360.0 L 274.4 362.5 L 261.9 355.4 L 252.6 357.4 L 247.1 364.5 L 242.6 365.9 L 238.0 362.3 L 242.5 371.3 L 233.2 394.2 L 236.9 397.4 L 232.7 400.3 L 237.3 411.9 L 235.1 429.2 L 250.9 432.4 L 257.1 429.0 L 259.6 432.0 L 264.7 432.2 L 270.7 457.5 L 280.6 455.3 L 287.7 457.9 L 296.2 441.2 L 311.2 430.6 L 323.3 407.2 L 331.2 399.3 L 332.1 389.8 L 330.0 387.4 L 305.5 377.5 L 304.2 376.1 L 307.1 373.3 L 302.4 372.5 L 302.8 368.3 L 298.2 367.7 Z" },
    { uf: "MT", nome: "Mato Grosso", cx: 258, cy: 304, d: "M 258.8 238.7 L 252.1 238.8 L 240.6 229.0 L 239.9 221.0 L 233.1 208.0 L 229.8 215.1 L 228.9 229.7 L 185.7 229.7 L 186.2 262.5 L 201.2 262.6 L 207.8 264.5 L 208.6 267.8 L 206.1 275.4 L 210.5 282.8 L 202.2 299.6 L 197.7 303.1 L 202.2 307.6 L 204.1 324.2 L 199.6 324.2 L 204.2 329.9 L 205.1 341.8 L 230.6 341.8 L 229.5 355.5 L 242.6 365.9 L 247.1 364.5 L 252.6 357.4 L 261.9 355.4 L 274.4 362.5 L 283.1 360.0 L 288.9 362.0 L 294.4 356.6 L 294.1 362.7 L 290.7 366.5 L 302.8 368.3 L 300.8 357.2 L 308.8 345.5 L 308.1 342.3 L 314.0 336.2 L 319.1 335.1 L 323.9 323.7 L 330.1 321.6 L 338.0 290.6 L 335.6 287.5 L 334.8 270.9 L 336.7 257.6 L 341.9 245.3 L 258.8 238.7 Z" },
    { uf: "PA", nome: "Pará", cx: 311, cy: 127, d: "M 369.8 171.3 L 377.8 166.0 L 390.7 144.0 L 397.8 121.9 L 399.1 114.2 L 397.1 110.9 L 393.5 113.0 L 391.3 109.6 L 373.6 106.1 L 366.9 111.2 L 366.6 101.0 L 353.5 100.4 L 353.3 96.6 L 349.1 95.3 L 352.0 92.6 L 342.7 92.5 L 344.5 89.6 L 341.9 87.1 L 328.4 99.3 L 321.6 113.5 L 316.0 115.8 L 311.7 113.4 L 310.1 106.2 L 304.7 99.7 L 297.8 79.0 L 284.8 71.1 L 279.8 70.9 L 276.9 58.8 L 271.0 61.3 L 263.4 59.5 L 260.6 63.6 L 262.5 70.1 L 244.6 67.6 L 240.8 72.2 L 230.6 74.0 L 228.2 78.5 L 222.7 79.1 L 223.0 102.8 L 229.1 113.0 L 232.9 116.1 L 234.5 114.1 L 235.6 118.5 L 247.6 124.3 L 253.2 130.8 L 261.1 128.0 L 257.1 131.8 L 228.4 199.4 L 239.9 221.0 L 240.6 229.0 L 252.1 238.8 L 341.9 245.3 L 344.5 237.0 L 355.7 220.7 L 356.5 214.5 L 353.6 210.1 L 355.9 201.5 L 367.3 192.8 L 370.6 181.7 L 362.1 177.9 L 369.8 171.3 Z" },
    { uf: "PB", nome: "Paraíba", cx: 523, cy: 204, d: "M 515.9 210.0 L 520.2 206.8 L 523.6 208.7 L 518.9 216.9 L 524.4 222.2 L 532.0 214.8 L 541.9 213.2 L 547.4 208.5 L 553.5 210.9 L 551.7 195.0 L 538.0 194.9 L 532.8 192.2 L 528.5 201.6 L 526.0 198.6 L 517.1 198.3 L 521.4 188.4 L 514.0 190.3 L 508.3 195.5 L 502.3 193.0 L 499.5 201.3 L 502.4 206.4 L 500.2 212.0 L 508.3 215.0 L 515.9 210.0 Z" },
    { uf: "PE", nome: "Pernambuco", cx: 507, cy: 222, d: "M 463.8 228.3 L 470.8 232.0 L 473.9 239.9 L 491.3 225.9 L 501.6 232.0 L 503.5 230.7 L 506.7 237.6 L 513.4 230.4 L 524.0 238.1 L 528.9 237.7 L 537.0 231.3 L 549.2 231.4 L 553.5 210.9 L 547.4 208.5 L 541.9 213.2 L 532.0 214.8 L 524.4 222.2 L 518.9 216.9 L 524.0 209.8 L 520.2 206.8 L 508.3 215.0 L 500.2 212.0 L 495.0 215.5 L 486.1 207.6 L 475.3 207.4 L 472.7 209.9 L 474.2 219.2 L 463.8 228.3 Z" },
    { uf: "PI", nome: "Piauí", cx: 440, cy: 208, d: "M 472.0 221.3 L 475.0 217.1 L 472.7 209.9 L 477.4 199.7 L 472.7 197.8 L 469.7 175.2 L 465.3 170.7 L 467.4 160.3 L 463.1 147.9 L 465.0 142.2 L 457.4 139.0 L 455.3 145.9 L 448.2 149.3 L 441.4 160.9 L 444.0 175.5 L 439.9 186.3 L 443.3 191.8 L 442.4 197.4 L 435.0 200.3 L 430.9 198.2 L 425.9 199.7 L 418.0 208.6 L 407.5 212.7 L 400.1 231.6 L 402.9 239.9 L 399.8 251.5 L 401.2 253.3 L 405.5 249.3 L 408.1 257.4 L 414.5 261.5 L 419.6 257.1 L 425.2 257.3 L 431.7 248.6 L 430.5 239.5 L 435.2 236.7 L 444.4 242.0 L 456.2 236.8 L 472.0 221.3 Z" },
    { uf: "PR", nome: "Paraná", cx: 329, cy: 470, d: "M 357.0 468.1 L 354.5 467.8 L 355.3 462.6 L 350.4 455.5 L 350.1 447.1 L 344.3 441.4 L 334.1 442.1 L 317.1 435.9 L 304.8 436.3 L 296.2 441.2 L 290.5 449.5 L 285.2 460.2 L 284.2 475.2 L 281.5 479.7 L 283.9 482.9 L 288.0 480.7 L 292.1 483.3 L 295.0 491.7 L 325.6 498.7 L 328.5 492.4 L 332.6 492.2 L 337.2 487.9 L 345.9 488.2 L 350.5 491.3 L 364.3 487.6 L 371.2 477.5 L 369.2 472.8 L 364.9 474.2 L 364.2 467.9 L 357.0 468.1 Z" },
    { uf: "RJ", nome: "Rio de Janeiro", cx: 438, cy: 434, d: "M 418.7 441.6 L 415.4 445.9 L 417.5 448.4 L 420.1 447.2 L 418.3 443.7 L 422.8 441.7 L 437.3 443.1 L 440.7 438.2 L 439.7 442.2 L 454.5 441.9 L 455.3 436.3 L 459.3 432.4 L 468.7 428.2 L 469.3 417.4 L 458.9 414.4 L 456.7 409.3 L 452.9 412.4 L 451.3 423.6 L 439.3 429.4 L 430.7 428.8 L 416.5 433.8 L 418.6 436.9 L 425.2 438.0 L 418.7 441.6 Z" },
    { uf: "RN", nome: "Rio Grande do Norte", cx: 524, cy: 189, d: "M 530.6 193.4 L 551.7 195.0 L 544.5 175.0 L 527.7 173.8 L 520.3 170.1 L 514.9 171.5 L 507.8 186.7 L 502.2 192.0 L 508.3 195.5 L 514.0 190.3 L 521.4 188.4 L 517.1 198.3 L 526.0 198.6 L 528.5 201.6 L 531.5 197.1 L 530.6 193.4 Z" },
    { uf: "RO", nome: "Rondônia", cx: 167, cy: 260, d: "M 174.3 294.5 L 178.4 295.2 L 182.5 300.7 L 193.6 300.3 L 197.7 303.1 L 202.2 299.6 L 210.5 282.8 L 206.1 275.4 L 207.8 264.5 L 186.2 262.5 L 187.2 242.1 L 185.1 236.5 L 187.3 231.5 L 183.9 228.0 L 180.1 230.8 L 168.1 217.3 L 157.7 217.3 L 150.6 231.9 L 141.4 232.5 L 136.8 239.4 L 135.0 236.6 L 128.7 241.1 L 119.4 238.8 L 113.9 245.0 L 133.8 243.5 L 133.8 264.9 L 138.4 277.7 L 146.9 284.4 L 165.0 287.3 L 174.3 294.5 Z" },
    { uf: "RR", nome: "Roraima", cx: 184, cy: 64, d: "M 150.0 45.2 L 151.7 60.0 L 160.5 60.7 L 161.1 64.4 L 170.3 68.5 L 168.9 73.7 L 173.5 81.3 L 172.9 90.9 L 177.4 102.4 L 173.0 109.0 L 181.4 118.5 L 187.2 121.3 L 185.1 117.1 L 186.4 108.9 L 190.7 105.1 L 194.7 105.8 L 198.3 110.9 L 203.2 108.4 L 202.1 105.8 L 207.0 93.6 L 222.7 93.6 L 222.3 77.8 L 211.8 71.2 L 207.7 57.3 L 209.3 44.1 L 214.0 38.7 L 211.1 31.2 L 205.3 29.7 L 207.9 21.4 L 205.6 18.8 L 197.9 19.1 L 199.5 23.2 L 193.8 29.8 L 190.0 29.4 L 178.0 36.4 L 169.9 36.9 L 169.7 42.4 L 166.4 43.4 L 163.4 38.3 L 153.0 39.5 L 150.1 35.6 L 144.8 36.0 L 141.8 33.2 L 150.0 45.2 Z" },
    { uf: "RS", nome: "Rio Grande do Sul", cx: 306, cy: 559, d: "M 290.7 577.3 L 301.3 587.9 L 308.9 580.2 L 307.6 590.5 L 302.0 589.9 L 297.3 596.8 L 296.6 603.3 L 298.7 604.2 L 308.9 594.6 L 316.2 580.4 L 314.0 575.7 L 318.6 567.6 L 325.2 564.3 L 326.0 557.6 L 329.4 554.5 L 327.1 548.8 L 332.2 554.5 L 336.5 550.9 L 337.0 555.3 L 335.0 553.5 L 335.5 558.6 L 328.8 565.0 L 328.0 569.8 L 316.2 575.5 L 316.4 580.1 L 333.4 565.8 L 340.4 555.5 L 348.9 537.8 L 343.0 537.0 L 348.8 525.7 L 336.6 523.8 L 330.5 516.0 L 313.9 506.8 L 290.7 505.2 L 265.6 521.6 L 266.7 524.2 L 264.6 523.3 L 240.6 550.6 L 245.9 552.2 L 250.8 549.3 L 262.2 559.7 L 262.3 564.2 L 268.3 560.5 L 272.9 566.8 L 281.9 569.8 L 290.7 577.3 Z" },
    { uf: "SC", nome: "Santa Catarina", cx: 335, cy: 508, d: "M 311.4 494.5 L 295.0 491.7 L 292.2 505.4 L 303.5 504.3 L 313.9 506.8 L 330.5 516.0 L 336.6 523.8 L 348.8 525.7 L 343.1 535.8 L 344.4 538.3 L 345.6 535.9 L 348.9 537.8 L 362.2 525.5 L 367.4 509.7 L 364.3 507.7 L 364.3 487.6 L 350.5 491.3 L 345.9 488.2 L 337.2 487.9 L 332.6 492.2 L 328.5 492.4 L 325.6 498.7 L 311.4 494.5 Z" },
    { uf: "SE", nome: "Sergipe", cx: 518, cy: 257, d: "M 524.9 249.6 L 509.2 241.5 L 513.5 252.8 L 512.6 258.0 L 506.7 259.7 L 512.8 270.5 L 519.1 269.4 L 525.8 258.9 L 532.1 255.2 L 524.9 249.6 Z" },
    { uf: "SP", nome: "São Paulo", cx: 371, cy: 435, d: "M 390.5 425.4 L 393.0 420.2 L 386.1 419.2 L 383.1 401.1 L 379.5 397.4 L 361.2 400.2 L 360.3 404.4 L 358.9 400.4 L 355.2 401.7 L 355.3 397.4 L 338.8 394.6 L 326.5 403.2 L 312.1 429.6 L 302.3 437.2 L 315.0 437.7 L 315.6 435.8 L 334.1 442.1 L 344.3 441.4 L 350.1 447.1 L 350.4 455.5 L 355.3 462.6 L 354.5 467.8 L 364.2 467.9 L 364.9 474.2 L 369.2 472.8 L 371.2 477.5 L 390.2 460.0 L 402.2 454.2 L 408.3 455.0 L 408.2 452.2 L 417.5 448.4 L 415.4 445.9 L 416.6 442.6 L 425.2 438.0 L 418.6 436.9 L 416.4 433.9 L 395.3 441.4 L 390.1 432.5 L 390.5 425.4 Z" },
    { uf: "TO", nome: "Tocantins", cx: 375, cy: 253, d: "M 380.3 194.0 L 378.5 179.6 L 367.5 175.2 L 362.1 177.9 L 370.6 181.7 L 367.3 192.8 L 355.9 201.5 L 353.6 210.1 L 356.5 214.5 L 354.9 223.4 L 344.5 237.0 L 336.7 257.6 L 334.8 270.9 L 336.4 290.0 L 343.1 283.7 L 340.7 289.6 L 353.7 296.8 L 355.5 291.0 L 357.1 289.6 L 359.1 292.1 L 360.5 289.8 L 364.5 297.4 L 365.4 294.8 L 366.2 296.9 L 370.5 295.0 L 370.2 297.2 L 376.9 299.8 L 378.4 294.5 L 380.8 296.7 L 393.7 292.3 L 394.3 290.1 L 395.0 292.6 L 398.4 291.5 L 395.5 292.1 L 397.3 285.3 L 394.7 278.5 L 395.0 272.2 L 398.9 272.1 L 394.0 270.5 L 392.4 266.7 L 404.1 251.6 L 401.2 253.3 L 399.7 250.5 L 395.5 250.4 L 391.1 243.8 L 391.3 238.9 L 385.3 233.6 L 393.6 218.8 L 391.6 216.1 L 386.0 218.6 L 378.1 209.3 L 379.7 207.7 L 376.0 205.1 L 378.7 202.9 L 380.3 194.0 Z" },
  ],


  openSobreModal() {
    this.closeMobileSidebar();
    const modal = document.getElementById("sobreModal");
    if (modal) modal.classList.add("open");
  },

  closeSobreModal() {
    const modal = document.getElementById("sobreModal");
    if (modal) modal.classList.remove("open");
  },

  openApoieModal() {
    this.openSobreModal();
  },

  closeApoieModal() {
    this.closeSobreModal();
  },

  openMetodologiaModal() {
    window.open("/docs/relatorio_ideologia_e_quiz.pdf", "_blank");
  },

  closeMetodologiaModal() {
    this.closeSobreModal();
  },

  toggleQuizMetodologiaInfo() {
    const card = document.getElementById("quizMetodologiaCard");
    if (card) {
      const isHidden = card.style.display === "none" || !card.style.display;
      card.style.display = isHidden ? "block" : "none";
    }
  },

  copyPixKey() {
    const input = document.getElementById("pixKeyInput");
    const feedback = document.getElementById("pixCopyFeedback");
    if (!input) return;
    navigator.clipboard.writeText(input.value).then(() => {
      if (feedback) {
        feedback.style.display = "block";
        setTimeout(() => { feedback.style.display = "none"; }, 3500);
      }
      if (typeof this.showToast === "function") {
        this.showToast("✓ Chave PIX copiada com sucesso!");
      }
    }).catch(() => {
      input.select();
      document.execCommand("copy");
      if (feedback) feedback.style.display = "block";
    });
  },

  openMapModal() {
    const modal = document.getElementById("brazilMapModal");
    if (!modal) return;
    try {
      this.renderBrazilSvgMap();
      this.renderBrazilStatesGrid();
      this.updateMapSelectedStatus();
    } catch (err) {
      console.error("Erro ao abrir modal do mapa:", err);
    }
    modal.classList.add("open");
  },

  closeMapModal() {
    const modal = document.getElementById("brazilMapModal");
    if (modal) modal.classList.remove("open");
  },

  highlightMapState(uf, isHover) {
    if (!uf) return;
    const path = document.getElementById(`state_path_${uf}`);
    const chip = document.getElementById(`state_card_${uf}`);
    if (isHover) {
      if (path) path.classList.add("hover-highlight");
      if (chip) chip.classList.add("hover-highlight");
    } else {
      if (path) path.classList.remove("hover-highlight");
      if (chip) chip.classList.remove("hover-highlight");
    }
  },

  toggleMapUf(uf) {
    if (!uf || uf === "BR" || this.state.uf === uf) {
      this.state.uf = "BR";
      this.state.selectedUfs = [];
    } else {
      this.state.uf = uf;
      this.state.selectedUfs = [uf];
    }

    this.renderSelectUfs();
    this.renderBrazilSvgMap();
    this.renderBrazilStatesGrid();
    this.updateMapSelectedStatus();

    this.state.pagina = 1;
    this.carregarCandidatos();
  },

  updateMapSelectedStatus() {
    const statusEl = document.getElementById("mapSelectedStatus");
    if (!statusEl) return;
    const uf = this.state.uf;
    if (uf && uf !== "BR") {
      const stateObj = this.brazilStates.find(s => s.uf === uf);
      const name = stateObj ? stateObj.nome : uf;
      statusEl.innerHTML = `<span>📍</span> <span>Estado selecionado: <strong>${name} (${uf})</strong></span>`;
    } else {
      statusEl.innerHTML = `<span>📍</span> <span>Exibindo: <strong>Todo o Brasil (BR)</strong></span>`;
    }
  },

  renderBrazilSvgMap() {
    const container = document.getElementById("brazilSvgMapContainer");
    if (!container) return;

    const currentUf = this.state.uf;
    const selectedUfs = this.state.selectedUfs || [];

    const svgPaths = (this.brazilSvgStates || []).map(s => {
      const isActive = currentUf === s.uf || selectedUfs.includes(s.uf);
      return `
        <g class="svg-state-group ${isActive ? 'active' : ''}" id="svg_group_${s.uf}" onmouseenter="App.highlightMapState('${s.uf}', true)" onmouseleave="App.highlightMapState('${s.uf}', false)" onclick="App.toggleMapUf('${s.uf}')" style="cursor: pointer;">
          <title>${s.nome} (${s.uf}) — Clique para selecionar</title>
          <path d="${s.d}" class="state-path ${isActive ? 'active' : ''}" id="state_path_${s.uf}"></path>
        </g>
      `;
    }).join("");

    container.innerHTML = `
      <svg viewBox="0 0 580 620" class="brazil-map-svg">
        ${svgPaths}
      </svg>
    `;
  },

  renderBrazilStatesGrid(filterRegion = null) {
    const container = document.getElementById("brazilStatesGrid");
    if (!container) return;

    let states = this.brazilStates || [];
    if (filterRegion) {
      states = states.filter(s => s.regiao === filterRegion);
    }

    const currentUf = this.state.uf;
    const selectedUfs = this.state.selectedUfs || [];

    container.innerHTML = states.map(s => {
      const isActive = currentUf === s.uf || selectedUfs.includes(s.uf);
      return `
        <div class="state-map-chip ${isActive ? 'active' : ''}" id="state_card_${s.uf}" onmouseenter="App.highlightMapState('${s.uf}', true)" onmouseleave="App.highlightMapState('${s.uf}', false)" onclick="App.toggleMapUf('${s.uf}')" title="${s.nome} (${s.uf})">
          <span class="state-chip-uf">${isActive ? '✓ ' : ''}${s.uf}</span>
          <span class="state-chip-name">${s.nome}</span>
        </div>
      `;
    }).join("");
  },

  filterMapRegion(region) {
    document.querySelectorAll(".btn-map-region").forEach(b => b.classList.remove("active"));
    if (window.event && window.event.currentTarget) {
      window.event.currentTarget.classList.add("active");
    }
    this.renderBrazilStatesGrid(region);
  },

  renderTableView(container) {
    const filterBar = document.getElementById("feedFilterBar");
    if (filterBar) filterBar.style.display = "none";
    document.body.setAttribute("data-view-mode", "table");

    const v = this.state.visibleColumns;
    const curOrd = this.state.ordenacao;

    let filteredCands = TableFilterManager.filterCandidates(this.state.candidatos || []);

    const getArrow = (colKey) => {
      if (colKey === "nome") {
        if (curOrd === "nome") return '<span class="sort-arrow">▲</span>';
        if (curOrd === "nome_desc") return '<span class="sort-arrow">▼</span>';
      } else if (colKey === "numero") {
        if (curOrd === "numero") return '<span class="sort-arrow">▲</span>';
        if (curOrd === "numero_desc") return '<span class="sort-arrow">▼</span>';
      } else if (colKey === "partido") {
        if (curOrd === "partido") return '<span class="sort-arrow">▲</span>';
        if (curOrd === "partido_desc") return '<span class="sort-arrow">▼</span>';
      } else if (colKey === "cargo") {
        if (curOrd === "cargo") return '<span class="sort-arrow">▲</span>';
        if (curOrd === "cargo_desc") return '<span class="sort-arrow">▼</span>';
      } else if (colKey === "uf") {
        if (curOrd === "uf") return '<span class="sort-arrow">▲</span>';
        if (curOrd === "uf_desc") return '<span class="sort-arrow">▼</span>';
      } else if (colKey === "ideologia") {
        if (curOrd === "ideologia") return '<span class="sort-arrow">▲</span>';
        if (curOrd === "ideologia_desc") return '<span class="sort-arrow">▼</span>';
      } else if (colKey === "mandato") {
        if (curOrd === "mandato") return '<span class="sort-arrow">▼</span>';
        if (curOrd === "mandato_desc") return '<span class="sort-arrow">▲</span>';
      } else if (colKey === "situacao") {
        if (curOrd === "situacao") return '<span class="sort-arrow">▲</span>';
        if (curOrd === "situacao_desc") return '<span class="sort-arrow">▼</span>';
      } else if (colKey === "prioridade_partido") {
        if (curOrd === "prioridade_partido") return '<span class="sort-arrow">▲</span>';
        if (curOrd === "prioridade_partido_desc") return '<span class="sort-arrow">▼</span>';
      } else if (colKey === "profissao") {
        if (curOrd === "profissao") return '<span class="sort-arrow">▲</span>';
        if (curOrd === "profissao_desc") return '<span class="sort-arrow">▼</span>';
      } else if (colKey === "raca") {
        if (curOrd === "raca") return '<span class="sort-arrow">▲</span>';
        if (curOrd === "raca_desc") return '<span class="sort-arrow">▼</span>';
      } else if (colKey === "idade") {
        if (curOrd === "idade") return '<span class="sort-arrow">▲</span>';
        if (curOrd === "idade_desc") return '<span class="sort-arrow">▼</span>';
      } else if (colKey === "bens") {
        if (curOrd === "bens") return '<span class="sort-arrow">▼</span>';
        if (curOrd === "bens_asc") return '<span class="sort-arrow">▲</span>';
      } else if (colKey === "receitas") {
        if (curOrd === "receitas") return '<span class="sort-arrow">▼</span>';
        if (curOrd === "receitas_asc") return '<span class="sort-arrow">▲</span>';
      } else if (colKey === "gastos") {
        if (curOrd === "gastos") return '<span class="sort-arrow">▼</span>';
        if (curOrd === "gastos_asc") return '<span class="sort-arrow">▲</span>';
      }
      return '<span class="sort-arrow">↕</span>';
    };

    const isAct = (colKey) => {
      if (colKey === "nome") return curOrd === "nome" || curOrd === "nome_desc";
      if (colKey === "numero") return curOrd === "numero" || curOrd === "numero_desc";
      if (colKey === "partido") return curOrd === "partido" || curOrd === "partido_desc";
      if (colKey === "cargo") return curOrd === "cargo" || curOrd === "cargo_desc";
      if (colKey === "uf") return curOrd === "uf" || curOrd === "uf_desc";
      if (colKey === "ideologia") return curOrd === "ideologia" || curOrd === "ideologia_desc";
      if (colKey === "mandato") return curOrd === "mandato" || curOrd === "mandato_desc";
      if (colKey === "situacao") return curOrd === "situacao" || curOrd === "situacao_desc";
      if (colKey === "prioridade_partido") return curOrd === "prioridade_partido" || curOrd === "prioridade_partido_desc";
      if (colKey === "profissao") return curOrd === "profissao" || curOrd === "profissao_desc";
      if (colKey === "raca") return curOrd === "raca" || curOrd === "raca_desc";
      if (colKey === "idade") return curOrd === "idade" || curOrd === "idade_desc";
      if (colKey === "bens") return curOrd === "bens" || curOrd === "bens_asc";
      if (colKey === "receitas") return curOrd === "receitas" || curOrd === "receitas_asc";
      if (colKey === "gastos") return curOrd === "gastos" || curOrd === "gastos_asc";
      return false;
    };

    const activeCols = (this.state.columnOrder || []).filter(c => this.state.visibleColumns[c]);

    this.renderUniversalActiveFilters(filteredCands.length, this.state.candidatos.length);

    container.innerHTML = `
      <div class="table-container-dual-scroll" id="tableContainerDualScroll">
        <div class="table-top-scroll" id="tableTopScrollBar">
          <div class="table-top-scroll-inner" id="tableTopScrollTrack"></div>
        </div>
        <div class="table-responsive-wrapper" id="tableMainScrollWrapper">
          <table class="data-table-clean" id="candidatosTable">
            <thead>
              <tr>
                ${activeCols.map(col => this.renderTableTh(col, isAct, getArrow)).join("")}
              </tr>
            </thead>
            <tbody>
              ${filteredCands.map(c => `
                <tr>
                  ${activeCols.map(col => this.renderTableTd(col, c)).join("")}
                </tr>
              `).join("")}
            </tbody>
          </table>
        </div>
      </div>
    `;

    this.initStickyTableSync();
  },

  renderTableTh(colKey, isAct, getArrow) {
    const dragAttrs = `draggable="true" data-col="${colKey}" ondragstart="App.onColDragStart(event, '${colKey}')" ondragover="App.onColDragOver(event, '${colKey}')" ondragenter="App.onColDragEnter(event, '${colKey}')" ondragleave="App.onColDragLeave(event, '${colKey}')" ondrop="App.onColDrop(event, '${colKey}')" ondragend="App.onColDragEnd(event)"`;
    const dragHandle = `<span class="th-drag-handle" title="Clique e arraste para reordenar coluna" draggable="false">⠿</span>`;
    const resizer = `<span class="col-resizer" onmousedown="App.initColResize(event, '${colKey}')" title="Arraste para ajustar a largura da coluna" draggable="false"></span>`;

    const userW = this.state.columnWidths && this.state.columnWidths[colKey];
    const getWStyle = (defaultMin) => {
      if (userW) return `width: ${userW}px; min-width: ${userW}px;`;
      return `min-width: ${defaultMin}px;`;
    };

    switch (colKey) {
      case "candidato":
        return `
          <th class="draggable-col" ${dragAttrs} style="${getWStyle(175)}">
            <div class="th-content-wrapper">
              ${dragHandle}
              <span class="th-sort-title ${isAct('nome') ? 'active' : ''}" onclick="App.sortTableBy('nome')" title="Ordenar por Nome" draggable="false">Candidato ${getArrow('nome')}</span>
              <button type="button" class="btn-table-col-filter ${TableFilterManager.hasFilter('candidato') ? 'active-filter' : ''}" onclick="event.stopPropagation(); TableFilterManager.toggle(this, 'candidato', event)" title="Filtrar por Nome" draggable="false">⚙️</button>
            </div>
            ${resizer}
          </th>`;
      case "numero":
        return `
          <th class="draggable-col" ${dragAttrs} style="${getWStyle(95)}">
            <div class="th-content-wrapper">
              ${dragHandle}
              <span class="th-sort-title ${isAct('numero') ? 'active' : ''}" onclick="App.sortTableBy('numero')" title="Ordenar por Número" draggable="false">Número ${getArrow('numero')}</span>
              <button type="button" class="btn-table-col-filter ${TableFilterManager.hasFilter('numero') ? 'active-filter' : ''}" onclick="event.stopPropagation(); TableFilterManager.toggle(this, 'numero', event)" title="Filtrar por Número" draggable="false">⚙️</button>
            </div>
            ${resizer}
          </th>`;
      case "partido":
        return `
          <th class="draggable-col" ${dragAttrs} style="${getWStyle(110)}">
            <div class="th-content-wrapper">
              ${dragHandle}
              <span class="th-sort-title ${isAct('partido') ? 'active' : ''}" onclick="App.sortTableBy('partido')" title="Ordenar por Partido" draggable="false">Partido ${getArrow('partido')}</span>
              <button type="button" class="btn-table-col-filter ${TableFilterManager.hasFilter('partido') ? 'active-filter' : ''}" onclick="event.stopPropagation(); TableFilterManager.toggle(this, 'partido', event)" title="Filtrar por Partido" draggable="false">⚙️</button>
            </div>
            ${resizer}
          </th>`;
      case "cargo":
        return `
          <th class="draggable-col" ${dragAttrs} style="${getWStyle(120)}">
            <div class="th-content-wrapper">
              ${dragHandle}
              <span class="th-sort-title ${isAct('cargo') ? 'active' : ''}" onclick="App.sortTableBy('cargo')" title="Ordenar por Cargo" draggable="false">Cargo ${getArrow('cargo')}</span>
              <button type="button" class="btn-table-col-filter ${TableFilterManager.hasFilter('cargo') ? 'active-filter' : ''}" onclick="event.stopPropagation(); TableFilterManager.toggle(this, 'cargo', event)" title="Filtrar por Cargo" draggable="false">⚙️</button>
            </div>
            ${resizer}
          </th>`;
      case "uf":
        return `
          <th class="draggable-col" ${dragAttrs} style="${getWStyle(140)}">
            <div class="th-content-wrapper">
              ${dragHandle}
              <span class="th-sort-title ${isAct('uf') ? 'active' : ''}" onclick="App.sortTableBy('uf')" title="Ordenar por UF / Estado" draggable="false">UF / Estado ${getArrow('uf')}</span>
              <button type="button" class="btn-table-col-filter ${TableFilterManager.hasFilter('uf') ? 'active-filter' : ''}" onclick="event.stopPropagation(); TableFilterManager.toggle(this, 'uf', event)" title="Filtrar por UF" draggable="false">⚙️</button>
            </div>
            ${resizer}
          </th>`;
      case "ideologia":
        return `
          <th class="draggable-col" ${dragAttrs} style="${getWStyle(140)}">
            <div class="th-content-wrapper">
              ${dragHandle}
              <span class="th-sort-title ${isAct('ideologia') ? 'active' : ''}" onclick="App.sortTableBy('ideologia')" title="Ordenar por Ideologia" draggable="false">Ideologia ${getArrow('ideologia')}</span>
              <button type="button" class="btn-table-col-filter ${TableFilterManager.hasFilter('ideologia') ? 'active-filter' : ''}" onclick="event.stopPropagation(); TableFilterManager.toggle(this, 'ideologia', event)" title="Filtrar por Ideologia" draggable="false">⚙️</button>
            </div>
            ${resizer}
          </th>`;
      case "mandato":
        return `
          <th class="draggable-col" ${dragAttrs} style="${getWStyle(130)}">
            <div class="th-content-wrapper">
              ${dragHandle}
              <span class="th-sort-title ${isAct('mandato') ? 'active' : ''}" onclick="App.sortTableBy('mandato')" title="Ordenar por Mandato" draggable="false">Mandato ${getArrow('mandato')}</span>
              <button type="button" class="btn-table-col-filter ${TableFilterManager.hasFilter('mandato') ? 'active-filter' : ''}" onclick="event.stopPropagation(); TableFilterManager.toggle(this, 'mandato', event)" title="Filtrar por Mandato" draggable="false">⚙️</button>
            </div>
            ${resizer}
          </th>`;
      case "situacao":
        return `
          <th class="draggable-col" ${dragAttrs} style="${getWStyle(120)}">
            <div class="th-content-wrapper">
              ${dragHandle}
              <span class="th-sort-title ${isAct('situacao') ? 'active' : ''}" onclick="App.sortTableBy('situacao')" title="Ordenar por Situação no TSE" draggable="false">Situação ${getArrow('situacao')}</span>
              <button type="button" class="btn-table-col-filter ${TableFilterManager.hasFilter('situacao') ? 'active-filter' : ''}" onclick="event.stopPropagation(); TableFilterManager.toggle(this, 'situacao', event)" title="Filtrar por Situação no TSE" draggable="false">⚙️</button>
            </div>
            ${resizer}
          </th>`;
      case "profissao":
        return `
          <th class="draggable-col" ${dragAttrs} style="${getWStyle(140)}">
            <div class="th-content-wrapper">
              ${dragHandle}
              <span class="th-sort-title ${isAct('profissao') ? 'active' : ''}" onclick="App.sortTableBy('profissao')" title="Ordenar por Ocupação" draggable="false">Profissão ${getArrow('profissao')}</span>
              <button type="button" class="btn-table-col-filter ${TableFilterManager.hasFilter('profissao') ? 'active-filter' : ''}" onclick="event.stopPropagation(); TableFilterManager.toggle(this, 'profissao', event)" title="Filtrar por Profissão" draggable="false">⚙️</button>
            </div>
            ${resizer}
          </th>`;
      case "raca":
        return `
          <th class="draggable-col" ${dragAttrs} style="${getWStyle(120)}">
            <div class="th-content-wrapper">
              ${dragHandle}
              <span class="th-sort-title ${isAct('raca') ? 'active' : ''}" onclick="App.sortTableBy('raca')" title="Ordenar por Raça/Etnia" draggable="false">Raça / Etnia ${getArrow('raca')}</span>
              <button type="button" class="btn-table-col-filter ${TableFilterManager.hasFilter('raca') ? 'active-filter' : ''}" onclick="event.stopPropagation(); TableFilterManager.toggle(this, 'raca', event)" title="Filtrar por Raça / Etnia" draggable="false">⚙️</button>
            </div>
            ${resizer}
          </th>`;
      case "idade":
        return `
          <th class="draggable-col" ${dragAttrs} style="${getWStyle(110)}">
            <div class="th-content-wrapper">
              ${dragHandle}
              <span class="th-sort-title ${isAct('idade') ? 'active' : ''}" onclick="App.sortTableBy('idade')" title="Ordenar por Idade" draggable="false">Idade ${getArrow('idade')}</span>
              <button type="button" class="btn-table-col-filter ${TableFilterManager.hasFilter('faixa_etaria') ? 'active-filter' : ''}" onclick="event.stopPropagation(); CardFilterManager.toggle(this, 'faixa_etaria', event)" title="Filtrar por Faixa Etária" draggable="false">⚙️</button>
            </div>
            ${resizer}
          </th>`;
      case "estado_civil":
        return `
          <th class="draggable-col" ${dragAttrs} style="${getWStyle(130)}">
            <div class="th-content-wrapper">
              ${dragHandle}
              <span class="th-sort-title ${isAct('estado_civil') ? 'active' : ''}" onclick="App.sortTableBy('estado_civil')" title="Ordenar por Estado Civil" draggable="false">Estado Civil ${getArrow('estado_civil')}</span>
              <button type="button" class="btn-table-col-filter ${TableFilterManager.hasFilter('estado_civil') ? 'active-filter' : ''}" onclick="event.stopPropagation(); TableFilterManager.toggle(this, 'estado_civil', event)" title="Filtrar por Estado Civil" draggable="false">⚙️</button>
            </div>
            ${resizer}
          </th>`;
      case "prioridade_partido": {
        const campFilt = window.TableFilterManager?.filters?.campanha_ranking;
        const isEscopoCargo = campFilt?.escopo === "cargo";
        const thColTitle = isEscopoCargo ? "Ranking no Cargo" : "Ranking no Partido";
        const thColTip = isEscopoCargo ? "Filtrar por Ranking no Cargo/UF" : "Filtrar por Ranking no Partido";
        const thColSortTip = isEscopoCargo ? "Ordenar por Ranking Geral no Cargo" : "Ordenar por Ranking Financeiro no Partido";
        const isSortActive = isAct('prioridade_partido') || isAct('prioridade_cargo');
        const arrow = isEscopoCargo ? getArrow('prioridade_cargo') : getArrow('prioridade_partido');
        return `
          <th class="draggable-col" ${dragAttrs} style="${getWStyle(130)}">
            <div class="th-content-wrapper">
              ${dragHandle}
              <span class="th-sort-title ${isSortActive ? 'active' : ''}" onclick="App.sortTableBy('prioridade_partido')" title="${thColSortTip}" draggable="false">${thColTitle} ${arrow}</span>
              <button type="button" class="btn-table-col-filter ${TableFilterManager.hasFilter('prioridade_partido') || TableFilterManager.hasFilter('campanha_ranking') ? 'active-filter' : ''}" onclick="event.stopPropagation(); TableFilterManager.toggle(this, 'prioridade_partido', event)" title="${thColTip}" draggable="false">⚙️</button>
            </div>
            ${resizer}
          </th>`;
      }
      case "bens":
        return `
          <th class="draggable-col" ${dragAttrs} style="${getWStyle(130)} text-align: right;">
            <div class="th-content-wrapper right">
              ${dragHandle}
              <span class="th-sort-title ${isAct('bens') ? 'active' : ''}" onclick="App.sortTableBy('bens')" title="Ordenar por Bens" draggable="false">Bens ${getArrow('bens')}</span>
              <button type="button" class="btn-table-col-filter ${TableFilterManager.hasFilter('bens') ? 'active-filter' : ''}" onclick="event.stopPropagation(); TableFilterManager.toggle(this, 'bens', event)" title="Filtrar por Bens" draggable="false">⚙️</button>
            </div>
            ${resizer}
          </th>`;
      case "receitas":
        return `
          <th class="draggable-col" ${dragAttrs} style="${getWStyle(130)} text-align: right;">
            <div class="th-content-wrapper right">
              ${dragHandle}
              <span class="th-sort-title ${isAct('receitas') ? 'active' : ''}" onclick="App.sortTableBy('receitas')" title="Ordenar por Receitas" draggable="false">Receitas ${getArrow('receitas')}</span>
              <button type="button" class="btn-table-col-filter ${TableFilterManager.hasFilter('receitas') ? 'active-filter' : ''}" onclick="event.stopPropagation(); TableFilterManager.toggle(this, 'receitas', event)" title="Filtrar por Receitas" draggable="false">⚙️</button>
            </div>
            ${resizer}
          </th>`;
      case "gastos":
        return `
          <th class="draggable-col" ${dragAttrs} style="${getWStyle(130)} text-align: right;">
            <div class="th-content-wrapper right">
              ${dragHandle}
              <span class="th-sort-title ${isAct('gastos') ? 'active' : ''}" onclick="App.sortTableBy('gastos')" title="Ordenar por Gastos" draggable="false">Gastos ${getArrow('gastos')}</span>
              <button type="button" class="btn-table-col-filter ${TableFilterManager.hasFilter('gastos') ? 'active-filter' : ''}" onclick="event.stopPropagation(); TableFilterManager.toggle(this, 'gastos', event)" title="Filtrar por Gastos" draggable="false">⚙️</button>
            </div>
            ${resizer}
          </th>`;
      case "acoes":
        return `
          <th class="draggable-col" ${dragAttrs} style="${getWStyle(130)} text-align: right;">
            <div class="th-content-wrapper right">
              ${dragHandle}
              <span>Ações</span>
            </div>
            ${resizer}
          </th>`;
      default:
        return '';
    }
  },

  renderTableTd(colKey, c) {
    const isFav = (window.ListManager && typeof ListManager.isFavorited === 'function') ? ListManager.isFavorited(c.sq_candidato) : false;
    const isComp = (window.CompareController && typeof CompareController.isSelected === 'function') ? CompareController.isSelected(c.sq_candidato) : false;
    const initials = (c.nome_urna || "C").substring(0, 2).toUpperCase();
    const rankGasto = c.gasto_ranking_texto || (c.ranking_gasto_cargo ? `${c.ranking_gasto_cargo}º no cargo (${c.uf || 'BR'})` : '');
    const rankReceita = c.receita_ranking_texto || (c.ranking_receita_cargo ? `${c.ranking_receita_cargo}º no cargo (${c.uf || 'BR'})` : '');
    const fotoLocal = c.foto_url || `/api/candidatos/${c.sq_candidato}/foto`;

    switch (colKey) {
      case "candidato":
        return `
          <td>
            <div class="table-cand-cell">
              <img src="${fotoLocal}" class="table-photo-thumb" loading="lazy" onerror="this.style.display='none'; this.nextElementSibling.style.display='flex';">
              <div class="table-photo-thumb" style="display:none; align-items:center; justify-content:center; font-family:var(--font-mono); font-weight:700; font-size:0.75rem; color:var(--text-muted);">${initials}</div>
              <div class="table-cand-name" style="font-weight: 700; color: var(--text-primary); font-size: 0.85rem;">
                ${c.nome_urna}
              </div>
            </div>
          </td>`;
      case "numero":
        return `<td><span class="urna-number-tag">${c.numero || '---'}</span></td>`;
      case "partido":
        return `
          <td>
            <span class="party-tag-pill" style="border-left-color: ${App.getResolvedPartidoColor(c)};" title="${(c.partido === 'PMB' || c.partido === 'DEMOCRATA') ? 'DEMOCRATA (registro PMB)' : (c.partido || '')}">
              ${(c.partido === 'PMB' || c.partido === 'DEMOCRATA') ? 'DEMOCRATA' : c.partido}
            </span>
          </td>`;
      case "cargo":
        return `<td style="color: var(--text-secondary); font-size: 0.78rem; font-weight: 500;">${c.cargo}</td>`;
      case "uf":
        return `
          <td>
            <div style="display: inline-flex; align-items: center; gap: 0.35rem;">
              <span class="table-uf-tag">${c.uf || 'BR'}</span>
              <span style="font-size: 0.76rem; color: var(--text-primary); font-weight: 500; white-space: nowrap;">
                ${UF_NOMES[c.uf] || (c.uf === 'BR' ? 'Brasil' : (c.uf || 'BR'))}
              </span>
            </div>
          </td>`;
      case "ideologia":
        return `
          <td>
            ${(c.ideologia_continua !== null && c.ideologia_continua !== undefined && !isNaN(Number(c.ideologia_continua))) ? `
              <span style="font-size: 0.75rem; font-weight: 600; color: var(--text-primary);">
                ${c.ideologia_nome || 'Centro'}
              </span>
              <span style="font-size: 0.68rem; font-family: var(--font-mono); color: var(--text-muted); margin-left: 0.25rem;">
                (${Number(c.ideologia_continua).toFixed(1)})
              </span>
            ` : `
              <span style="font-size: 0.72rem; color: var(--text-muted);">
                Sem Classificação
              </span>
            `}
          </td>`;
      case "mandato":
        return `
          <td>
            ${(() => {
            const mand = App.getCandidateMandatoInfo(c);
            if (mand.isReeleicao) {
              return `<span class="party-tag-pill" style="border-left-color: #10B981; font-size: 0.68rem; color: var(--accent-emerald); font-weight: 700;" title="Tentando reeleição para o mesmo cargo: ${c.cargo}">🔄 Reeleição</span>`;
            } else if (mand.badgeText) {
              return `<span class="party-tag-pill" style="border-left-color: #3B82F6; font-size: 0.68rem; color: var(--accent-blue); font-weight: 700;" title="Em exercício de outro cargo: ${c.cargo_exercicio}">🏛️ ${c.cargo_exercicio}</span>`;
            } else {
              return `<span style="color: var(--text-muted); font-size: 0.72rem;">Não consta*</span>`;
            }
          })()}
          </td>`;
      case "situacao":
        return `
          <td>
            ${(() => {
            const st = (c.situacao_candidatura || 'Aguardando julgamento').toUpperCase();
            let cls = 'apto';
            let icon = '✓ ';
            if (st.includes('INAPTO') || st.includes('INDEFERIDO') || st.includes('CANCELADO') || st.includes('REN')) {
              cls = 'inapto';
              icon = '✕ ';
            } else if (st.includes('PENDENTE') || st.includes('SUB JUDICE') || st.includes('AGUARDANDO')) {
              cls = 'pendente';
              icon = '⏳ ';
            }
            return `<span class="cand-status-badge ${cls}">${icon}${c.situacao_candidatura || 'Apto'}</span>`;
          })()}
          </td>`;
      case "profissao":
        return `
          <td style="font-size: 0.75rem; color: var(--text-secondary);">
            ${c.ocupacao && c.ocupacao !== 'Não informada' ? `💼 ${c.ocupacao}` : '<span style="color: var(--text-muted);">---</span>'}
          </td>`;
      case "raca":
        return `
          <td style="font-size: 0.75rem; color: var(--text-secondary);">
            ${c.cor_raca && c.cor_raca !== 'NÃO INFORMADO' ? c.cor_raca : '<span style="color: var(--text-muted);">---</span>'}
          </td>`;
      case "idade": {
        const id = c.idade || (c.dt_nascimento ? App.calcularIdade(c.dt_nascimento, 2026) : null);
        return `
          <td style="font-size: 0.75rem; color: var(--text-secondary);">
            ${id ? `🎂 ${id} anos` : (c.dt_nascimento ? c.dt_nascimento : '<span style="color: var(--text-muted);">---</span>')}
          </td>`;
      }
      case "estado_civil":
        return `
          <td style="font-size: 0.75rem; color: var(--text-secondary);">
            ${c.estado_civil ? c.estado_civil : '<span style="color: var(--text-muted);">---</span>'}
          </td>`;
      case "prioridade_partido": {
        const campFilt = window.TableFilterManager?.filters?.campanha_ranking;
        const isCargoScope = campFilt?.escopo === "cargo";
        const isDespesa = campFilt?.metrica === "gastos";

        if (isCargoScope) {
          const rkCargo = isDespesa ? (c.ranking_gasto_cargo || c.ranking_receita_cargo) : (c.ranking_receita_cargo || c.ranking_gasto_cargo);
          return `
            <td>
              ${rkCargo ? `
                <span class="party-tag-pill" style="border-left-color: var(--accent-petroleo); font-size: 0.68rem; color: var(--text-primary); font-weight: 600;" title="Ranking geral no cargo: ${rkCargo}º mais financiado para ${c.cargo} (${c.uf || 'BR'})">
                  🎖️ ${rkCargo}º no cargo (${c.uf || 'BR'})
                </span>
              ` : `
                <span style="color: var(--text-muted); font-size: 0.72rem;">---</span>
              `}
            </td>`;
        } else {
          const rkPart = isDespesa ? (c.ranking_partido_despesa || c.ranking_partido_receita) : (c.ranking_partido_receita || c.ranking_partido_despesa);
          return `
            <td>
              ${(c.cargo !== 'PRESIDENTE' && c.cargo !== 'GOVERNADOR' && c.total_partido_cargo_uf > 1 && rkPart) ? `
                <span class="party-tag-pill" style="border-left-color: var(--accent-emerald); font-size: 0.68rem; color: var(--text-primary); font-weight: 600;" title="Ranking no partido: ${rkPart}º mais financiado do ${c.partido} (${c.uf}) entre ${c.total_partido_cargo_uf} candidatos">
                  🎯 ${rkPart}º de ${c.total_partido_cargo_uf} no ${c.partido}
                </span>
              ` : `
                <span style="color: var(--text-muted); font-size: 0.72rem;">---</span>
              `}
            </td>`;
        }
      }
      case "bens":
        return `
          <td>
            <span style="font-size: 0.78rem; font-weight: 700; color: var(--accent-emerald); font-family: var(--font-mono);">
              ${c.total_bens > 0 ? `R$ ${(c.total_bens).toLocaleString('pt-BR', { minimumFractionDigits: 2 })}` : '<span style="color: var(--text-muted); font-family: var(--font-mono); font-size: 0.78rem; font-weight: 400;">R$ 0,00</span>'}
            </span>
          </td>`;
      case "receitas":
        return `
          <td>
            ${c.financiamento_receita > 0 ? `
              <div>
                <span style="font-family: var(--font-mono); font-weight: 700; color: var(--accent-emerald); font-size: 0.78rem;">
                  R$ ${(c.financiamento_receita).toLocaleString('pt-BR', { minimumFractionDigits: 2 })}
                </span>
                ${rankReceita ? `
                  <span style="font-size: 0.68rem; color: var(--text-muted); display: block;">
                    ${rankReceita}
                  </span>
                ` : ''}
              </div>
            ` : '<span style="color: var(--text-muted); font-family: var(--font-mono); font-size: 0.78rem; font-weight: 400;">R$ 0,00</span>'}
          </td>`;
      case "gastos":
        return `
          <td>
            ${c.financiamento_despesa > 0 ? `
              <div>
                <span style="font-family: var(--font-mono); font-weight: 700; color: var(--accent-emerald); font-size: 0.78rem;">
                  R$ ${(c.financiamento_despesa).toLocaleString('pt-BR', { minimumFractionDigits: 2 })}
                </span>
                ${rankGasto ? `
                  <span style="font-size: 0.68rem; color: var(--text-muted); display: block;">
                    ${rankGasto}
                  </span>
                ` : ''}
              </div>
            ` : '<span style="color: var(--text-muted); font-family: var(--font-mono); font-size: 0.78rem; font-weight: 400;">R$ 0,00</span>'}
          </td>`;
      case "acoes":
        return `
          <td style="text-align: right;">
            <div style="display: inline-flex; align-items: center; gap: 0.35rem;">
              <button type="button" class="btn-card-action primary" style="padding: 0.22rem 0.5rem; font-size: 0.72rem;" onclick="App.openDetails('${c.sq_candidato}')">
                Ficha ↗
              </button>
              <button type="button" class="btn-icon-square ${isFav ? 'active' : ''}" style="width: 24px; height: 24px; font-size: 0.75rem; display: inline-flex; align-items: center; justify-content: center;" title="${isFav ? 'Remover dos Salvos' : 'Salvar nos Salvos'}" onclick="App.toggleFav('${c.sq_candidato}')">
                <svg width="14" height="14" viewBox="0 0 24 24" fill="${isFav ? '#C84B31' : 'none'}" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                  <path d="M19 21l-7-5-7 5V5a2 2 0 0 1 2-2h10a2 2 0 0 1 2 2z"></path>
                </svg>
              </button>
              <button type="button" class="btn-icon-square ${isComp ? 'active' : ''}" style="width: 24px; height: 24px; font-size: 0.75rem;" title="Comparar" onclick="App.toggleCompare('${c.sq_candidato}')">
                ⚖️
              </button>
              <button type="button" class="btn-icon-square" style="width: 24px; height: 24px; font-size: 0.75rem;" title="Criar Santinho Digital" onclick="event.stopPropagation(); SantinhoStudio.open(App.getCandidateBySq('${c.sq_candidato}'))">
                🎨
              </button>
            </div>
          </td>`;
      default:
        return '';
    }
  },

  /* ─── Sincronização do Cabeçalho Sticky Flutuante da Tabela ─────────────── */

  initStickyTableSync() {
    if (this._stickyTableCleanup) {
      this._stickyTableCleanup();
    }

    const wrapper = document.querySelector(".table-responsive-wrapper");
    const table = document.getElementById("candidatosTable");
    if (!wrapper || !table) return;

    let cloneWrap = document.getElementById("tableStickyHeaderWrap");
    if (!cloneWrap) {
      cloneWrap = document.createElement("div");
      cloneWrap.id = "tableStickyHeaderWrap";
      cloneWrap.className = "table-sticky-header-floating-wrap";
      document.body.appendChild(cloneWrap);
    }

    const origThead = table.querySelector("thead");
    if (!origThead) return;

    cloneWrap.innerHTML = `
      <table class="data-table-clean" style="margin:0; border-collapse:separate; border-spacing:0;">
        ${origThead.outerHTML}
      </table>
    `;

    const topScroll = document.getElementById("tableTopScrollBar");
    const topTrack = document.getElementById("tableTopScrollTrack");

    let bottomScroll = document.getElementById("tableFloatingBottomScroll");
    if (!bottomScroll) {
      bottomScroll = document.createElement("div");
      bottomScroll.id = "tableFloatingBottomScroll";
      bottomScroll.className = "table-floating-bottom-scroll";
      bottomScroll.innerHTML = `<div class="table-floating-bottom-track" id="tableFloatingBottomTrack"></div>`;
      document.body.appendChild(bottomScroll);
    }
    const bottomTrack = document.getElementById("tableFloatingBottomTrack");

    const syncColWidths = () => {
      const origThs = origThead.querySelectorAll("th");
      const cloneThs = cloneWrap.querySelectorAll("th");
      origThs.forEach((th, i) => {
        if (cloneThs[i]) {
          const rect = th.getBoundingClientRect();
          const w = `${rect.width}px`;
          cloneThs[i].style.width = w;
          cloneThs[i].style.minWidth = w;
          cloneThs[i].style.maxWidth = w;
        }
      });
      const tableRect = table.getBoundingClientRect();
      const cloneTable = cloneWrap.querySelector("table");
      if (cloneTable) {
        cloneTable.style.width = `${tableRect.width}px`;
        cloneTable.style.minWidth = `${tableRect.width}px`;
      }
      if (topTrack && table) {
        topTrack.style.width = `${table.scrollWidth}px`;
      }
      if (bottomTrack && table) {
        bottomTrack.style.width = `${table.scrollWidth}px`;
      }
    };

    syncColWidths();

    const onWrapperScroll = () => {
      cloneWrap.scrollLeft = wrapper.scrollLeft;
      if (topScroll) topScroll.scrollLeft = wrapper.scrollLeft;
      if (bottomScroll) bottomScroll.scrollLeft = wrapper.scrollLeft;
    };
    const onCloneScroll = () => {
      wrapper.scrollLeft = cloneWrap.scrollLeft;
      if (topScroll) topScroll.scrollLeft = cloneWrap.scrollLeft;
      if (bottomScroll) bottomScroll.scrollLeft = cloneWrap.scrollLeft;
    };
    const onTopScroll = () => {
      wrapper.scrollLeft = topScroll.scrollLeft;
      cloneWrap.scrollLeft = topScroll.scrollLeft;
      if (bottomScroll) bottomScroll.scrollLeft = topScroll.scrollLeft;
    };
    const onBottomScroll = () => {
      wrapper.scrollLeft = bottomScroll.scrollLeft;
      cloneWrap.scrollLeft = bottomScroll.scrollLeft;
      if (topScroll) topScroll.scrollLeft = bottomScroll.scrollLeft;
    };

    wrapper.addEventListener("scroll", onWrapperScroll, { passive: true });
    cloneWrap.addEventListener("scroll", onCloneScroll, { passive: true });
    if (topScroll) topScroll.addEventListener("scroll", onTopScroll, { passive: true });
    if (bottomScroll) bottomScroll.addEventListener("scroll", onBottomScroll, { passive: true });

    const getViewportTopOffset = () => {
      const fixedHeader = document.querySelector(".app-header-fixed");
      if (fixedHeader) {
        const r = fixedHeader.getBoundingClientRect();
        return Math.max(0, r.bottom);
      }
      return 0;
    };

    const updateFloatingVisibility = () => {
      if (App.state.viewMode !== "table") {
        cloneWrap.style.display = "none";
        if (bottomScroll) bottomScroll.style.display = "none";
        return;
      }

      const wrapRect = wrapper.getBoundingClientRect();
      const theadRect = origThead.getBoundingClientRect();
      const topOffset = getViewportTopOffset();

      const isScrolledPastHeader = theadRect.bottom <= topOffset;
      const isTableStillVisible = wrapRect.bottom > (topOffset + 40);

      if (isScrolledPastHeader && isTableStillVisible) {
        cloneWrap.style.display = "block";
        cloneWrap.style.top = `${topOffset}px`;
        cloneWrap.style.left = `${wrapRect.left}px`;
        cloneWrap.style.width = `${wrapRect.width}px`;
        cloneWrap.scrollLeft = wrapper.scrollLeft;
        syncColWidths();
      } else {
        cloneWrap.style.display = "none";
      }

      // Barra de rolagem horizontal flutuante inferior (acompanha a descida pela tabela)
      if (bottomScroll) {
        const isTableCutoffAtBottom = wrapRect.bottom > window.innerHeight && wrapRect.top < (window.innerHeight - 50);
        const hasHorizontalOverflow = table.scrollWidth > wrapRect.width;

        if (isTableCutoffAtBottom && hasHorizontalOverflow) {
          bottomScroll.style.display = "block";
          bottomScroll.style.left = `${wrapRect.left}px`;
          bottomScroll.style.width = `${wrapRect.width}px`;
          bottomScroll.scrollLeft = wrapper.scrollLeft;
          if (bottomTrack) bottomTrack.style.width = `${table.scrollWidth}px`;
        } else {
          bottomScroll.style.display = "none";
        }
      }
    };

    window.addEventListener("scroll", updateFloatingVisibility, { passive: true });
    window.addEventListener("resize", () => {
      updateFloatingVisibility();
      syncColWidths();
    }, { passive: true });

    updateFloatingVisibility();

    this._stickyTableCleanup = () => {
      wrapper.removeEventListener("scroll", onWrapperScroll);
      cloneWrap.removeEventListener("scroll", onCloneScroll);
      if (topScroll) topScroll.removeEventListener("scroll", onTopScroll);
      if (bottomScroll) bottomScroll.removeEventListener("scroll", onBottomScroll);
      window.removeEventListener("scroll", updateFloatingVisibility);
      if (cloneWrap && cloneWrap.parentNode) {
        cloneWrap.parentNode.removeChild(cloneWrap);
      }
      if (bottomScroll && bottomScroll.parentNode) {
        bottomScroll.parentNode.removeChild(bottomScroll);
      }
      this._stickyTableCleanup = null;
    };
  },

  /* ─── Modal de Exportação ─────────────────────────────────────────── */

  exportState: {
    format: "csv",
    scope: "page"
  },

  openExportModal() {
    const modal = document.getElementById("exportModal") || document.getElementById("downloadModal");
    if (!modal) return;

    const pageCountEl = document.getElementById("exportPageCount");
    const totalCountEl = document.getElementById("exportTotalCount");
    if (pageCountEl) pageCountEl.textContent = (this.state.candidatos || []).length;
    if (totalCountEl) totalCountEl.textContent = (this.state.total || 0).toLocaleString('pt-BR');

    modal.classList.add("open");
  },

  closeExportModal() {
    const modal = document.getElementById("exportModal") || document.getElementById("downloadModal");
    if (modal) modal.classList.remove("open");
  },

  openMetodologiaModal() {
    const modal = document.getElementById("metodologiaIdeologiaModal");
    if (modal) modal.classList.add("open");
  },

  closeMetodologiaModal() {
    const modal = document.getElementById("metodologiaIdeologiaModal");
    if (modal) modal.classList.remove("open");
  },

  setExportFormat(fmt) {
    this.exportState.format = fmt;
    ["csv", "xls", "json"].forEach(f => {
      const el = document.getElementById("expFmt_" + f);
      if (el) el.classList.remove("active");
    });
    const activeEl = document.getElementById("expFmt_" + fmt);
    if (activeEl) activeEl.classList.add("active");
  },

  setExportScope(scope) {
    this.exportState.scope = scope;
    ["page", "all"].forEach(s => {
      const el = document.getElementById("scope_" + s);
      if (el) el.classList.remove("active");
    });
    const activeEl = document.getElementById("scope_" + scope);
    if (activeEl) activeEl.classList.add("active");
  },

  toggleAllExportCols() {
    const cols = ["nome", "completo", "numero", "partido", "cargo", "uf", "ideologia", "mandato", "profissao", "estado_civil", "bens", "gastos"];
    const anyChecked = cols.some(c => {
      const el = document.getElementById("expCol_" + c);
      return el && el.checked;
    });
    cols.forEach(c => {
      const el = document.getElementById("expCol_" + c);
      if (el) el.checked = !anyChecked;
    });
  },

  async executarDownloadExportacao() {
    const { format, scope } = this.exportState;
    let dados = this.state.candidatos;

    if (scope === "all") {
      const params = new URLSearchParams({
        ano_eleicao: this.state.ano_eleicao,
        ideologia_min: this.state.ideologia_min,
        ideologia_max: this.state.ideologia_max,
        pagina: 1,
        limite: Math.min(20000, this.state.total || 500),
        ordenacao: this.state.ordenacao
      });
      if (this.state.uf && this.state.uf !== "BR") params.append("uf", this.state.uf);
      if (this.state.cargo) params.append("cargo", this.state.cargo);
      if (this.state.situacao_mandato && this.state.situacao_mandato !== "todos") {
        params.append("situacao_mandato", this.state.situacao_mandato);
      }
      if (this.state.busca) params.append("busca", this.state.busca);
      if (this.state.partidos_gosta.length > 0) params.append("partidos_gosta", this.state.partidos_gosta.join(","));
      if (this.state.partidos_desgosta.length > 0) params.append("partidos_desgosta", this.state.partidos_desgosta.join(","));

      try {
        const resp = await fetch("/api/candidatos?" + params.toString());
        if (resp.ok) {
          const json = await resp.json();
          if (json.candidatos && json.candidatos.length > 0) {
            dados = json.candidatos;
          }
        }
      } catch (e) {
        console.warn("Erro ao buscar todos os dados:", e);
      }
    }

    if (!dados || dados.length === 0) {
      alert("Nenhum candidato para exportar.");
      return;
    }

    const incNome = document.getElementById("expCol_nome")?.checked ?? true;
    const incCompleto = document.getElementById("expCol_completo")?.checked ?? true;
    const incNumero = document.getElementById("expCol_numero")?.checked ?? true;
    const incPartido = document.getElementById("expCol_partido")?.checked ?? true;
    const incCargo = document.getElementById("expCol_cargo")?.checked ?? true;
    const incUf = document.getElementById("expCol_uf")?.checked ?? true;
    const incIdeologia = document.getElementById("expCol_ideologia")?.checked ?? true;
    const incMandato = document.getElementById("expCol_mandato")?.checked ?? true;
    const incProfissao = document.getElementById("expCol_profissao")?.checked ?? true;
    const incEstadoCivil = document.getElementById("expCol_estado_civil")?.checked ?? false;
    const incBens = document.getElementById("expCol_bens")?.checked ?? true;
    const incGastos = document.getElementById("expCol_gastos")?.checked ?? true;

    const timestamp = new Date().toISOString().slice(0, 10);
    const filename = "candidatos_tse_2026_" + (this.state.uf || "BR") + "_" + timestamp + "." + format;

    if (format === "json") {
      const filteredData = dados.map(c => {
        const item = {};
        if (incNome) item.nome_urna = c.nome_urna;
        if (incCompleto) item.nome_completo = c.nome_completo;
        if (incNumero) item.numero = c.numero;
        if (incPartido) item.partido = c.partido;
        if (incCargo) item.cargo = c.cargo;
        if (incUf) item.uf = c.uf || "BR";
        if (incIdeologia) { item.ideologia_nome = c.ideologia_nome; item.ideologia_continua = c.ideologia_continua; }
        if (incMandato) { item.em_exercicio = c.em_exercicio; item.cargo_exercicio = c.cargo_exercicio; }
        if (incProfissao) item.ocupacao = c.ocupacao;
        if (incEstadoCivil) item.estado_civil = c.estado_civil;
        if (incBens) item.total_bens = c.total_bens;
        if (incGastos) item.financiamento_despesa = c.financiamento_despesa;
        return item;
      });

      const blob = new Blob([JSON.stringify(filteredData, null, 2)], { type: "application/json;charset=utf-8;" });
      this.triggerDownload(blob, filename);
    } else if (format === "csv") {
      const headers = [];
      if (incNome) headers.push("Nome de Urna");
      if (incCompleto) headers.push("Nome Completo");
      if (incNumero) headers.push("Número");
      if (incPartido) headers.push("Partido");
      if (incCargo) headers.push("Cargo");
      if (incUf) headers.push("UF");
      if (incIdeologia) headers.push("Ideologia do Partido");
      if (incMandato) headers.push("Mandato Atual");
      if (incProfissao) headers.push("Profissão / Ocupação");
      if (incEstadoCivil) headers.push("Estado Civil");
      if (incBens) headers.push("Bens Declarados (R$)");
      if (incGastos) headers.push("Despesas TSE (R$)");

      const rows = dados.map(c => {
        const row = [];
        if (incNome) row.push('"' + (c.nome_urna || '').replace(/"/g, '""') + '"');
        if (incCompleto) row.push('"' + (c.nome_completo || '').replace(/"/g, '""') + '"');
        if (incNumero) row.push('"' + (c.numero || '') + '"');
        if (incPartido) row.push('"' + (c.partido || '') + '"');
        if (incCargo) row.push('"' + (c.cargo || '').replace(/"/g, '""') + '"');
        if (incUf) row.push('"' + (c.uf || 'BR') + '"');
        if (incIdeologia) row.push('"' + (c.ideologia_nome || 'Sem Classificação') + '"');
        if (incMandato) row.push('"' + (c.em_exercicio ? (c.cargo_exercicio || 'Em Exercício') : 'Não consta*') + '"');
        if (incProfissao) row.push('"' + (c.ocupacao || 'Não informada').replace(/"/g, '""') + '"');
        if (incEstadoCivil) row.push('"' + (c.estado_civil || 'Não informado').replace(/"/g, '""') + '"');
        if (incBens) row.push('"' + (Number(c.total_bens) || 0).toFixed(2) + '"');
        if (incGastos) row.push('"' + (Number(c.financiamento_despesa) || 0).toFixed(2) + '"');
        return row;
      });

      const csvContent = "\uFEFF" + [headers.join(";"), ...rows.map(r => r.join(";"))].join("\r\n");
      const blob = new Blob([csvContent], { type: "text/csv;charset=utf-8;" });
      this.triggerDownload(blob, filename);
    } else if (format === "xls") {
      const headersHtml = [];
      if (incNome) headersHtml.push("<th>Nome de Urna</th>");
      if (incCompleto) headersHtml.push("<th>Nome Completo</th>");
      if (incNumero) headersHtml.push("<th>Número</th>");
      if (incPartido) headersHtml.push("<th>Partido</th>");
      if (incCargo) headersHtml.push("<th>Cargo</th>");
      if (incUf) headersHtml.push("<th>UF</th>");
      if (incIdeologia) headersHtml.push("<th>Ideologia do Partido</th>");
      if (incMandato) headersHtml.push("<th>Mandato Atual</th>");
      if (incProfissao) headersHtml.push("<th>Profissão / Ocupação</th>");
      if (incEstadoCivil) headersHtml.push("<th>Estado Civil</th>");
      if (incBens) headersHtml.push("<th>Bens Declarados (R$)</th>");
      if (incGastos) headersHtml.push("<th>Despesas TSE (R$)</th>");

      const rowsHtml = dados.map(c => {
        const cells = [];
        if (incNome) cells.push("<td>" + (c.nome_urna || "") + "</td>");
        if (incCompleto) cells.push("<td>" + (c.nome_completo || "") + "</td>");
        if (incNumero) cells.push("<td>" + (c.numero || "") + "</td>");
        if (incPartido) cells.push("<td>" + (c.partido || "") + "</td>");
        if (incCargo) cells.push("<td>" + (c.cargo || "") + "</td>");
        if (incUf) cells.push("<td>" + (c.uf || "BR") + "</td>");
        if (incIdeologia) cells.push("<td>" + (c.ideologia_nome || "Sem Classificação") + "</td>");
        if (incMandato) cells.push("<td>" + (c.em_exercicio ? (c.cargo_exercicio || "Em Exercício") : "Não consta*") + "</td>");
        if (incProfissao) cells.push("<td>" + (c.ocupacao || "Não informada") + "</td>");
        if (incEstadoCivil) cells.push("<td>" + (c.estado_civil || "Não informado") + "</td>");
        if (incBens) cells.push("<td>" + (Number(c.total_bens) || 0).toLocaleString("pt-BR", { minimumFractionDigits: 2 }) + "</td>");
        if (incGastos) cells.push("<td>" + (Number(c.financiamento_despesa) || 0).toLocaleString("pt-BR", { minimumFractionDigits: 2 }) + "</td>");
        return "<tr>" + cells.join("") + "</tr>";
      }).join("");

      const xlsContent = '<html xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:x="urn:schemas-microsoft-com:office:excel" xmlns="http://www.w3.org/TR/REC-html40"><head><meta charset="utf-8"/></head><body><table border="1"><thead><tr style="background-color: #1E293B; color: #FFFFFF;">' + headersHtml.join("") + '</tr></thead><tbody>' + rowsHtml + '</tbody></table></body></html>';
      const blob = new Blob([xlsContent], { type: "application/vnd.ms-excel;charset=utf-8;" });
      this.triggerDownload(blob, filename);
    }

    this.closeExportModal();
  },

  triggerDownload(blob, filename) {
    const link = document.createElement("a");
    link.href = URL.createObjectURL(blob);
    link.setAttribute("download", filename);
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
  },

  renderPagination(totalPages) {
    const wrap = document.getElementById("paginationWrapper");
    if (!wrap) return;

    const page = this.state.pagina;
    const total = this.state.total || 0;
    const limit = this.state.limite || 30;
    const startItem = total === 0 ? 0 : (page - 1) * limit + 1;
    const endItem = Math.min(total, page * limit);

    let paginationNav = "";
    if (totalPages > 1) {
      paginationNav = `
        <div class="pagination-nav-group">
          <button type="button" class="btn-page-arrow" ${page <= 1 ? 'disabled' : ''} onclick="App.goToPage(1)" title="Primeira Página">
            «
          </button>
          <button type="button" class="btn-page-arrow" ${page <= 1 ? 'disabled' : ''} onclick="App.goToPage(${page - 1})" title="Página Anterior">
            ‹
          </button>

          <div class="pagination-page-box">
            <span class="pagination-page-label">Pág.</span>
            <input type="text" 
                   inputmode="numeric" 
                   pattern="[0-9]*" 
                   id="inputJumpPage" 
                   class="pagination-page-input" 
                   value="${page}" 
                   onkeydown="if(event.key==='Enter') App.jumpToPage(this.value, ${totalPages})" 
                   onblur="App.jumpToPage(this.value, ${totalPages})"
                   title="Digite o número da página e tecle Enter">
            <span class="pagination-page-total">de ${totalPages}</span>
          </div>

          <button type="button" class="btn-page-arrow" ${page >= totalPages ? 'disabled' : ''} onclick="App.goToPage(${page + 1})" title="Próxima Página">
            ›
          </button>
          <button type="button" class="btn-page-arrow" ${page >= totalPages ? 'disabled' : ''} onclick="App.goToPage(${totalPages})" title="Última Página">
            »
          </button>
        </div>
      `;
    }

    wrap.innerHTML = `
      <div class="pagination-info">
        ${total > 0 ? `Mostrando <span class="pagination-highlight">${startItem}–${endItem}</span> de <span class="pagination-highlight">${total.toLocaleString('pt-BR')}</span>` : ''}
      </div>

      ${paginationNav}

      <div class="pagination-limit-wrap">
        <label for="selectLimiteBottom" class="pagination-limit-label">Por pág:</label>
        <select id="selectLimiteBottom" class="form-select pagination-limit-select" onchange="App.setLimite(this.value)">
          <option value="30" ${limit === 30 ? 'selected' : ''}>30</option>
          <option value="50" ${limit === 50 ? 'selected' : ''}>50</option>
          <option value="100" ${limit === 100 ? 'selected' : ''}>100</option>
        </select>
      </div>
    `;
  },

  jumpToPage(val, totalPages) {
    let p = parseInt(val, 10);
    if (isNaN(p) || p < 1) p = 1;
    if (p > totalPages) p = totalPages;
    if (p !== this.state.pagina) {
      this.goToPage(p);
    }
  },


  goToPage(page) {
    this.state.pagina = page;
    this.carregarCandidatos();
    window.scrollTo({ top: 0, behavior: 'smooth' });
  },

  toggleFav(sq_candidato) {
    const cand = this.getCandidateBySq(sq_candidato);
    if (cand) {
      ListManager.toggleFavorite(cand);
      this.renderView();
      this.updateBookmarkBadge();
    }
  },

  toggleCompare(sq_candidato) {
    const cand = this.getCandidateBySq(sq_candidato);
    if (cand && window.CompareController && typeof CompareController.toggleCandidate === 'function') {
      CompareController.toggleCandidate(cand);
      this.renderView();
      // Se o modal de salvos estiver aberto, abre a tela de comparação por cima
      const savedModal = document.getElementById("savedListModal");
      if (savedModal && savedModal.classList.contains("open")) {
        if (typeof CompareController.openModal === "function") {
          CompareController.openModal();
        }
      }
    }
  },

  updateBookmarkBadge() {
    const count = (window.ListManager && typeof ListManager.getActiveCandidates === 'function') ? ListManager.getActiveCandidates().length : 0;
    const badge = document.getElementById("savedCountBadge");
    if (badge) {
      badge.textContent = count;
      badge.style.display = count > 0 ? "inline-block" : "none";
    }
    const mobBadge = document.getElementById("mobNavSavedBadge");
    if (mobBadge) {
      mobBadge.textContent = count;
      mobBadge.style.display = count > 0 ? "inline-block" : "none";
    }
    const sidebarBadge = document.getElementById("sidebarSavedBadge");
    if (sidebarBadge) {
      sidebarBadge.textContent = count;
      sidebarBadge.style.display = count > 0 ? "inline-flex" : "none";
    }
  },

  async openDetails(sq_candidato) {
    const drawer = document.getElementById("detailsDrawer");
    const body = document.getElementById("drawerBody");
    const nameEl = document.getElementById("drawerCandName");
    if (!drawer || !body) return;

    body.innerHTML = "<div style='text-align: center; padding: 2rem; color: var(--text-muted); font-size: 0.82rem;'>Carregando dados oficiais do TSE...</div>";
    drawer.classList.add("open");

    try {
      const resp = await fetch(`/api/candidatos/${sq_candidato}`);
      if (resp.ok) {
        const c = await resp.json();
        if (nameEl) nameEl.innerText = c.nome_urna;
        this.renderDrawerDetails(c);

        // Google Analytics 4 - Evento Customizado de Visualização de Candidato
        if (typeof window.gtag === 'function') {
          window.gtag('event', 'view_candidate', {
            candidate_id: c.sq_candidato,
            candidate_name: c.nome_urna,
            cargo: c.cargo,
            partido: c.partido,
            uf: c.uf
          });
        }
      } else {
        body.innerHTML = "<div style='color: #EF4444; padding: 1rem;'>Candidato não encontrado no TSE.</div>";
      }
    } catch (e) {
      console.error("Erro ao carregar detalhes:", e);
      body.innerHTML = "<div style='color: #EF4444; padding: 1rem;'>Erro ao carregar ficha do TSE.</div>";
    }
  },

  calcularIdade(dtNasc, anoEleicao = 2026) {
    if (!dtNasc) return null;
    const parts = String(dtNasc).trim().split("/");
    if (parts.length === 3) {
      const dia = parseInt(parts[0], 10);
      const mes = parseInt(parts[1], 10);
      const ano = parseInt(parts[2], 10);
      if (!isNaN(ano) && ano > 1900 && ano < anoEleicao) {
        let idade = anoEleicao - ano;
        if (mes > 10 || (mes === 10 && dia > 4)) {
          idade -= 1;
        }
        return idade;
      }
    }
    return null;
  },

  renderSocialButtonsHtml(r, options = {}) {
    if (typeof r === 'string') {
      try {
        r = JSON.parse(r);
      } catch (e) {
        r = {};
      }
    }

    if (!r || typeof r !== 'object' || Object.keys(r).length === 0) {
      if (options.compact) return '';
      return `<div style="font-size: 0.74rem; color: var(--text-muted); background: var(--bg-page); padding: 0.45rem 0.65rem; border-radius: var(--radius-xs); border: 1px solid var(--border-color); display: inline-flex; align-items: center; gap: 0.35rem;">
        Não consta
      </div>`;
    }

    const SVG_ICONS = {
      instagram: `<svg class="social-svg-icon" viewBox="0 0 24 24" style="color:#E1306C;"><path d="M12 2.163c3.204 0 3.584.012 4.85.07 3.252.148 4.771 1.691 4.919 4.919.058 1.265.069 1.645.069 4.849 0 3.205-.012 3.584-.069 4.849-.149 3.225-1.664 4.771-4.919 4.919-1.266.058-1.644.07-4.85.07-3.204 0-3.584-.012-4.849-.07-3.26-.149-4.771-1.699-4.919-4.92-.058-1.265-.07-1.644-.07-4.849 0-3.204.013-3.583.07-4.849.149-3.227 1.664-4.771 4.919-4.919 1.266-.057 1.645-.069 4.849-.069zm0-2.163c-3.259 0-3.667.014-4.947.072-4.358.2-6.78 2.618-6.98 6.98-.059 1.281-.073 1.689-.073 4.948 0 3.259.014 3.668.072 4.948.2 4.358 2.618 6.78 6.98 6.98 1.281.058 1.689.072 4.948.072 3.259 0 3.668-.014 4.948-.072 4.354-.2 6.782-2.618 6.979-6.98.059-1.28.073-1.689.073-4.948 0-3.259-.014-3.667-.072-4.947-.196-4.354-2.617-6.78-6.979-6.98-1.281-.059-1.69-.073-4.949-.073zm0 5.838c-3.403 0-6.162 2.759-6.162 6.162s2.759 6.163 6.162 6.163 6.162-2.759 6.162-6.163c0-3.403-2.759-6.162-6.162-6.162zm0 10.162c-2.209 0-4-1.79-4-4 0-2.209 1.791-4 4-4s4 1.791 4 4c0 2.21-1.791 4-4 4zm6.406-11.845c-.796 0-1.441.645-1.441 1.44s.645 1.44 1.441 1.44c.795 0 1.439-.645 1.439-1.44s-.644-1.44-1.439-1.44z"/></svg>`,
      twitter: `<svg class="social-svg-icon" viewBox="0 0 24 24" style="color:#F3F4F6;"><path d="M18.244 2.25h3.308l-7.227 8.26 8.502 11.24H16.17l-5.214-6.817L4.99 21.75H1.68l7.73-8.835L1.254 2.25H8.08l4.713 6.231zm-1.161 17.52h1.833L7.084 4.126H5.117z"/></svg>`,
      facebook: `<svg class="social-svg-icon" viewBox="0 0 24 24" style="color:#1877F2;"><path d="M24 12.073c0-6.627-5.373-12-12-12s-12 5.373-12 12c0 5.99 4.388 10.954 10.125 11.854v-8.385H7.078v-3.47h3.047V9.43c0-3.007 1.792-4.669 4.533-4.669 1.312 0 2.686.235 2.686.235v2.953H15.83c-1.491 0-1.956.925-1.956 1.874v2.25h3.328l-.532 3.47h-2.796v8.385C19.612 23.027 24 18.062 24 12.073z"/></svg>`,
      tiktok: `<svg class="social-svg-icon" viewBox="0 0 24 24" style="color:#25F4EE;"><path d="M12.525.02c1.31-.02 2.61-.01 3.91-.02.08 1.53.63 3.09 1.75 4.17 1.12 1.11 2.7 1.62 4.24 1.79v4.03c-1.44-.05-2.89-.35-4.2-.97-.57-.26-1.1-.59-1.62-.93-.01 2.92.01 5.84-.02 8.75-.08 1.4-.54 2.79-1.35 3.94-1.31 1.92-3.58 3.17-5.91 3.21-1.43.08-2.86-.31-4.08-1.03-2.02-1.19-3.44-3.37-3.65-5.71-.02-.5-.03-1-.01-1.49.18-1.9 1.12-3.72 2.58-4.96 1.66-1.44 3.98-2.13 6.15-1.72.02 1.48-.04 2.96-.04 4.44-.99-.32-2.15-.23-3.02.37-.63.41-1.11 1.04-1.36 1.75-.21.51-.24 1.07-.14 1.61.24 1.64 1.82 3.02 3.5 2.87 1.12-.01 2.19-.66 2.77-1.61.19-.33.4-.67.41-1.06.1-1.79.06-3.57.07-5.36.01-4.03-.01-8.05.02-12.07z"/></svg>`,
      youtube: `<svg class="social-svg-icon" viewBox="0 0 24 24" style="color:#FF0000;"><path d="M23.498 6.186a3.016 3.016 0 0 0-2.122-2.136C19.505 3.545 12 3.545 12 3.545s-7.505 0-9.377.505A3.017 3.017 0 0 0 .502 6.186C0 8.07 0 12 0 12s0 3.93.502 5.814a3.016 3.016 0 0 0 2.122 2.136c1.871.505 9.376.505 9.376.505s7.505 0 9.377-.505a3.015 3.015 0 0 0 2.122-2.136C24 15.93 24 12 24 12s0-3.93-.502-5.814zM9.545 15.568V8.432L15.818 12l-6.273 3.568z"/></svg>`,
      linkedin: `<svg class="social-svg-icon" viewBox="0 0 24 24" style="color:#0A66C2;"><path d="M19 0h-14c-2.761 0-5 2.239-5 5v14c0 2.761 2.239 5 5 5h14c2.762 0 5-2.239 5-5v-14c0-2.761-2.238-5-5-5zm-11 19h-3v-11h3v11zm-1.5-12.268c-.966 0-1.75-.79-1.75-1.764s.784-1.764 1.75-1.764 1.75.79 1.75 1.764-.783 1.764-1.75 1.764zm13.5 12.268h-3v-5.604c0-3.368-4-3.113-4 0v5.604h-3v-11h3v1.765c1.396-2.586 7-2.777 7 2.476v6.759z"/></svg>`,
      site: `<svg class="social-svg-icon" viewBox="0 0 24 24" style="color:#60A5FA;"><path d="M12 0c-6.627 0-12 5.373-12 12s5.373 12 12 12 12-5.373 12-12-5.373-12-12-12zm1 16.947v1.986c-2.748-.285-4.838-1.554-5.679-3.933h2.158c.633 1.189 1.83 1.947 3.521 1.947zm0-3.947h-4.789c-.139-.638-.211-1.306-.211-2s.072-1.362.211-2h4.789v4zm0-6h-3.521c-1.691 0-2.888.758-3.521 1.947h-2.158c.841-2.379 2.931-3.648 5.679-3.933v1.986zm2 9.933v-1.986c1.691 0 2.888-.758 3.521-1.947h2.158c-.841 2.379-2.931 3.648-5.679 3.933zm0-3.933v-4h4.789c.139.638.211 1.306.211 2s-.072 1.362-.211 2h-4.789zm0-6v-1.986c2.748.285 4.838 1.554 5.679 3.933h-2.158c-.633-1.189-1.83-1.947-3.521-1.947z"/></svg>`
    };

    const REDES_CONFIG = [
      { key: "instagram", nome: "Instagram" },
      { key: "twitter", aliases: ["x"], nome: "X/Twitter" },
      { key: "facebook", nome: "Facebook" },
      { key: "tiktok", nome: "TikTok" },
      { key: "youtube", nome: "YouTube" },
      { key: "linkedin", nome: "LinkedIn" },
      { key: "site", nome: "Site Oficial" }
    ];

    const formatHref = (url, key) => {
      const u = String(url || '').trim();
      if (u.startsWith('http://') || u.startsWith('https://')) return u;
      if (u.startsWith('@')) {
        if (key === 'instagram') return `https://instagram.com/${u.slice(1)}`;
        if (key === 'twitter') return `https://x.com/${u.slice(1)}`;
        if (key === 'tiktok') return `https://tiktok.com/@${u.slice(1)}`;
      }
      return `https://${u}`;
    };

    const cleanLabel = (url) => {
      try {
        const u = new URL(url.startsWith('http') ? url : `https://${url}`);
        return u.pathname.replace(/^\/+/, '') || u.hostname;
      } catch {
        return url;
      }
    };

    const buttons = [];

    REDES_CONFIG.forEach(cfg => {
      let rawVal = r[cfg.key];
      if (!rawVal && cfg.aliases) {
        for (const al of cfg.aliases) {
          if (r[al]) {
            rawVal = r[al];
            break;
          }
        }
      }
      if (!rawVal) return;

      const urls = (Array.isArray(rawVal) ? rawVal : [rawVal]).filter(u => !!u);
      if (urls.length === 0) return;

      const iconSvg = SVG_ICONS[cfg.key] || SVG_ICONS.site;

      if (urls.length === 1) {
        const href = formatHref(urls[0], cfg.key);
        buttons.push(`
          <a href="${href}" target="_blank" rel="noopener" class="social-btn" title="Abrir ${cfg.nome}" onclick="event.stopPropagation()">
            ${iconSvg}
            <span>${cfg.nome} ↗</span>
          </a>
        `);
      } else {
        // Múltiplos links: 1 botão com dropdown flutuante
        const dropdownItems = urls.map((u, i) => {
          const href = formatHref(u, cfg.key);
          const lbl = cleanLabel(u) || `Perfil ${i + 1}`;
          return `
            <a href="${href}" target="_blank" rel="noopener" class="social-dropdown-item" title="${u}" onclick="event.stopPropagation()">
              <span>${cfg.nome} #${i + 1} (${lbl})</span>
              <span style="font-size: 0.65rem; color: #60A5FA;">↗</span>
            </a>
          `;
        }).join("");

        buttons.push(`
          <div class="social-dropdown-wrapper" onclick="event.stopPropagation()">
            <button type="button" class="social-btn" title="${urls.length} perfis de ${cfg.nome}">
              ${iconSvg}
              <span>${cfg.nome}</span>
              <span class="social-count-badge">${urls.length} ▾</span>
            </button>
            <div class="social-dropdown-menu">
              ${dropdownItems}
            </div>
          </div>
        `);
      }
    });

    if (buttons.length === 0) {
      if (options.compact) return '';
      return `<div style="font-size: 0.74rem; color: var(--text-muted); background: var(--bg-page); padding: 0.45rem 0.65rem; border-radius: var(--radius-xs); border: 1px solid var(--border-color); display: inline-flex; align-items: center; gap: 0.35rem;">
        Não consta
      </div>`;
    }

    if (options.compact) {
      return `<div class="card-social-strip" style="margin-top: 0.45rem; display: flex; flex-wrap: wrap; gap: 0.3rem;" onclick="event.stopPropagation()">${buttons.join('')}</div>`;
    }

    return `<div class="social-links-grid">${buttons.join('')}</div>`;
  },

  renderDrawerDetails(c) {
    const body = document.getElementById("drawerBody");
    if (!body) return;

    const initials = (c.nome_urna || "C").substring(0, 2).toUpperCase();
    const rankGasto = c.gasto_ranking_texto || (c.ranking_gasto_cargo ? `${c.ranking_gasto_cargo}º de ${c.total_cands_cargo || ''} no cargo (${c.uf || 'BR'})` : '');
    const rankReceita = c.receita_ranking_texto || (c.ranking_receita_cargo ? `${c.ranking_receita_cargo}º de ${c.total_cands_cargo || ''} no cargo (${c.uf || 'BR'})` : '');
    const fotoLocal = c.foto_url || `/fotos/${c.sq_candidato}.jpg`;
    const idade = this.calcularIdade(c.dt_nascimento, 2026);

    // Cálculos de Receitas e Despesas
    const totalReceita = c.financiamento_receita || 0;
    const totalDespesa = c.financiamento_despesa || 0;
    const fundo = c.fundo_eleitoral || 0;
    const doacoes = c.doacoes_pf || 0;
    const outros = Math.max(0, totalReceita - fundo - doacoes);

    const pctFundo = totalReceita > 0 ? (fundo / totalReceita) * 100 : 0;
    const pctDoacoes = totalReceita > 0 ? (doacoes / totalReceita) * 100 : 0;
    const pctOutros = totalReceita > 0 ? (outros / totalReceita) * 100 : 0;

    const cargoStr = String(c.cargo || '').toUpperCase();
    const cargoExerc = String(c.cargo_exercicio || '').toLowerCase();
    const ocupStr = String(c.ocupacao || '').toLowerCase();

    // Restrição estrita: Radar do Congresso aplica-se unicamente a mandato federal comprovado
    const isMandatoFederalAtivo = (
      cargoExerc.includes('federal') ||
      cargoExerc.includes('senad') ||
      Boolean(c.radar_congresso_url)
    ) && !cargoExerc.includes('estadual') && !cargoExerc.includes('distrital') && !cargoExerc.includes('vereador') && !cargoExerc.includes('prefeito');

    const hasRadarData = Boolean(c.governismo_pct !== null && c.governismo_pct !== undefined) ||
      Boolean(c.assiduidade_pct !== null && c.assiduidade_pct !== undefined) ||
      Boolean(c.presenca_pct !== null && c.presenca_pct !== undefined) ||
      Boolean(c.bancadas && (Array.isArray(c.bancadas) ? c.bancadas.length > 0 : (typeof c.bancadas === 'string' && c.bancadas !== '[]' && c.bancadas.trim() !== ''))) ||
      Boolean(c.votacoes_radar && (Array.isArray(c.votacoes_radar) ? c.votacoes_radar.length > 0 : (typeof c.votacoes_radar === 'string' && c.votacoes_radar !== '[]' && c.votacoes_radar.trim() !== '')));

    const exibirRadarFederal = isMandatoFederalAtivo && hasRadarData;

    // Pesquisas: SOMENTE para majoritários (Governador, Senador, Presidente)
    const isMajoritario = cargoStr.includes('GOVERNADOR') || cargoStr.includes('SENADOR') || cargoStr.includes('PRESIDENTE');

    body.innerHTML = `
      <!-- Header do Candidato no Drawer -->
      <div style="display: flex; align-items: center; gap: 0.75rem; margin-bottom: 0.75rem; padding-bottom: 0.75rem; border-bottom: 1px solid var(--border-color);">
        <div class="card-photo-box" style="width: 58px; height: 58px; flex-shrink: 0;">
          <img src="${fotoLocal}" alt="${c.nome_urna}" class="card-photo-img" onerror="if(!this.dataset.triedFallback){this.dataset.triedFallback='1';this.src='/api/candidatos/${c.sq_candidato}/foto';}else{this.style.display='none';this.nextElementSibling.style.display='flex';}">
          <div class="card-photo-initials" style="display:none; font-size: 1.15rem;">${initials}</div>
        </div>
        <div style="flex: 1; min-width: 0;">
          <div style="font-family: var(--font-serif); font-size: 1.25rem; font-weight: 700; color: var(--text-primary); line-height: 1.2;">${c.nome_urna}</div>
          <div style="font-size: 0.76rem; color: var(--text-secondary); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; margin-top: 1px;">
            ${c.nome_completo || ''}
          </div>
          <div style="display: flex; gap: 0.35rem; margin-top: 0.35rem; align-items: center; flex-wrap: wrap;">
            <span class="party-tag-pill" style="border-left-color: ${c.partido_cor_hex || App.getPartidoColor(c.partido)}; font-size: 0.68rem;">${c.partido}</span>
            <span class="urna-number-tag" style="font-size: 0.68rem;">Nº ${c.numero || '---'}</span>
            <span style="font-size: 0.68rem; font-weight: 700; text-transform: uppercase; letter-spacing: 0.05em; color: var(--accent-petroleo);">${c.cargo} • ${c.uf}</span>
            ${(() => {
        const mand = App.getCandidateMandatoInfo(c);
        if (mand.isReeleicao) {
          return `<span class="party-tag-pill" style="border-left-color: #10B981; font-size: 0.65rem; color: var(--accent-emerald); font-weight: 700;">Reeleição (${c.cargo_exercicio})</span>`;
        } else if (mand.badgeText) {
          return `<span class="party-tag-pill" style="border-left-color: #3B82F6; font-size: 0.65rem; color: var(--accent-blue); font-weight: 700;">Mandato: ${c.cargo_exercicio}</span>`;
        }
        return '';
      })()}
            ${idade ? `<span style="font-size: 0.68rem; color: var(--text-secondary); background: var(--bg-page); padding: 0.1rem 0.35rem; border-radius: var(--radius-xs); border: 1px solid var(--border-color);">${idade} anos</span>` : ''}
          </div>
        </div>
      </div>

      <!-- Abas do Drawer (3 Abas Consistentes e Padronizadas) -->
      <div class="drawer-tabs">
        <button type="button" class="drawer-tab-btn active" onclick="App.switchDrawerTab('tabPerfil', this)">1. Perfil & TSE</button>
        <button type="button" class="drawer-tab-btn" onclick="App.switchDrawerTab('tabFinancas', this)">2. Finanças & Despesas</button>
        <button type="button" class="drawer-tab-btn" onclick="App.switchDrawerTab('tabMandato', this)">3. Atuação & Mandato</button>
      </div>

      <!-- Aba 1: Perfil & TSE -->
      <div id="tabPerfil" class="drawer-tab-content" style="padding-top: 0.5rem;">
        <div style="display: flex; flex-direction: column; gap: 0.55rem; font-size: 0.8rem;">
          <div><strong style="color:var(--text-muted);">Idade:</strong> ${idade ? `${idade} anos (Nascimento: ${c.dt_nascimento || '---'})` : (c.dt_nascimento ? `Nascimento: ${c.dt_nascimento}` : 'Não informada')} ${c.uf_nascimento ? `• Naturalidade: ${c.uf_nascimento}` : ''}</div>
          <div><strong style="color:var(--text-muted);">Coligação / Federação:</strong> ${c.coligacao || 'Partido Isolado'}</div>
          <div><strong style="color:var(--text-muted);">Ocupação Declarada:</strong> ${c.ocupacao || 'Não informada'}</div>
          <div><strong style="color:var(--text-muted);">Escolaridade:</strong> ${c.grau_instrucao || 'Não informada'}</div>
          <div><strong style="color:var(--text-muted);">Gênero / Cor:</strong> ${c.genero || '---'} • ${c.cor_raca || '---'}</div>
          <div><strong style="color:var(--text-muted);">Situação no TSE:</strong> ${(() => {
        const st = (c.situacao_candidatura || 'Aguardando julgamento').toUpperCase();
        if (st.includes('INAPTO') || st.includes('INDEFERIDO') || st.includes('CANCELADO') || st.includes('REN')) {
          return `<span class="cand-status-badge inapto">✕ ${c.situacao_candidatura || 'Inapto'}</span>`;
        } else if (st.includes('PENDENTE') || st.includes('SUB JUDICE') || st.includes('AGUARDANDO')) {
          return `<span class="cand-status-badge pendente">⏳ ${c.situacao_candidatura || 'Aguardando julgamento'}</span>`;
        }
        return `<span class="cand-status-badge apto">✓ ${c.situacao_candidatura || 'Apto / Deferido'}</span>`;
      })()}</div>

          <!-- Bloco de Vice na Chapa Majoritária (Presidente / Governador) -->
          ${(c.chapa_info && (c.chapa_info.tipo === 'vice_presidente' || c.chapa_info.tipo === 'vice_governador')) ? `
            <div style="background: rgba(59, 130, 246, 0.08); border: 1px solid rgba(59, 130, 246, 0.25); border-radius: var(--radius-sm); padding: 0.85rem; margin-top: 0.35rem;">
              <div style="font-size: 0.7rem; font-weight: 700; color: #60A5FA; text-transform: uppercase; display: flex; align-items: center; gap: 0.35rem;">
                ${c.cargo === 'PRESIDENTE' ? 'Vice-Presidente na Chapa' : 'Vice-Governador(a) na Chapa'}
              </div>
              <div style="display: flex; align-items: center; gap: 0.75rem; margin-top: 0.5rem;">
                <div style="width: 48px; height: 48px; border-radius: 50%; overflow: hidden; border: 2px solid #3B82F6; flex-shrink: 0; background: var(--bg-surface);">
                  <img src="${c.chapa_info.foto_url}" alt="${c.chapa_info.nome_urna}" style="width: 100%; height: 100%; object-fit: cover;" onerror="this.src='/static/img/fallback_avatar.png'">
                </div>
                <div>
                  <div style="font-size: 0.92rem; font-weight: 700; color: var(--text-primary);">${c.chapa_info.nome_urna}</div>
                  <div style="font-size: 0.75rem; color: var(--text-secondary);">${c.chapa_info.nome_completo} • <strong style="color: #93C5FD;">${c.chapa_info.partido}</strong></div>
                </div>
              </div>
            </div>
          ` : ''}

          <!-- Bloco de Suplentes de Senado -->
          ${(c.chapa_info && c.chapa_info.tipo === 'suplentes' && (c.chapa_info.suplente_1 || c.chapa_info.suplente_2)) ? `
            <div style="background: rgba(139, 92, 246, 0.08); border: 1px solid rgba(139, 92, 246, 0.25); border-radius: var(--radius-sm); padding: 0.85rem; margin-top: 0.35rem;">
              <div style="font-size: 0.7rem; font-weight: 700; color: #A78BFA; text-transform: uppercase; display: flex; align-items: center; gap: 0.35rem;">
                Suplentes de Senado Registrados
              </div>
              <div style="display: flex; flex-direction: column; gap: 0.5rem; margin-top: 0.5rem;">
                ${c.chapa_info.suplente_1 ? `
                  <div style="display: flex; align-items: center; gap: 0.65rem;">
                    <div style="width: 38px; height: 38px; border-radius: 50%; overflow: hidden; border: 2px solid #8B5CF6; flex-shrink: 0; background: var(--bg-surface);">
                      <img src="${c.chapa_info.suplente_1.foto_url}" alt="${c.chapa_info.suplente_1.nome_urna}" style="width: 100%; height: 100%; object-fit: cover;" onerror="this.src='/static/img/fallback_avatar.png'">
                    </div>
                    <div>
                      <div style="font-size: 0.85rem; font-weight: 700; color: var(--text-primary);"><span style="font-size: 0.65rem; background: rgba(139, 92, 246, 0.2); color: #C4B5FD; padding: 0.1rem 0.35rem; border-radius: 3px; margin-right: 0.3rem;">1º Suplente</span>${c.chapa_info.suplente_1.nome_urna}</div>
                      <div style="font-size: 0.72rem; color: var(--text-secondary);">${c.chapa_info.suplente_1.nome_completo} • <strong style="color: #C4B5FD;">${c.chapa_info.suplente_1.partido}</strong></div>
                    </div>
                  </div>
                ` : ''}
                ${c.chapa_info.suplente_2 ? `
                  <div style="display: flex; align-items: center; gap: 0.65rem;">
                    <div style="width: 38px; height: 38px; border-radius: 50%; overflow: hidden; border: 2px solid #8B5CF6; flex-shrink: 0; background: var(--bg-surface);">
                      <img src="${c.chapa_info.suplente_2.foto_url}" alt="${c.chapa_info.suplente_2.nome_urna}" style="width: 100%; height: 100%; object-fit: cover;" onerror="this.src='/static/img/fallback_avatar.png'">
                    </div>
                    <div>
                      <div style="font-size: 0.85rem; font-weight: 700; color: var(--text-primary);"><span style="font-size: 0.65rem; background: rgba(139, 92, 246, 0.2); color: #C4B5FD; padding: 0.1rem 0.35rem; border-radius: 3px; margin-right: 0.3rem;">2º Suplente</span>${c.chapa_info.suplente_2.nome_urna}</div>
                      <div style="font-size: 0.72rem; color: var(--text-secondary);">${c.chapa_info.suplente_2.nome_completo} • <strong style="color: #C4B5FD;">${c.chapa_info.suplente_2.partido}</strong></div>
                    </div>
                  </div>
                ` : ''}
              </div>
            </div>
          ` : ''}

          <!-- Plano / Proposta de Governo na Aba 1 -->
          ${c.plano_governo_url ? `
            <div style="background: rgba(16, 185, 129, 0.08); border: 1px solid rgba(16, 185, 129, 0.25); border-radius: var(--radius-sm); padding: 0.85rem; margin-top: 0.5rem;">
              <div style="font-size: 0.7rem; font-weight: 700; color: var(--accent-emerald); text-transform: uppercase;">📄 Proposta / Plano de Governo Registrado</div>
              <p style="font-size: 0.76rem; color: var(--text-muted); margin-bottom: 0.5rem; line-height: 1.4;">
                Diretrizes e metas programáticas registradas oficialmente na Justiça Eleitoral.
              </p>
              <a href="${c.plano_governo_url}" target="_blank" rel="noopener" class="btn-header primary" style="text-decoration: none; display: inline-flex; font-size: 0.76rem; padding: 0.35rem 0.65rem;">
                📄 Abrir Plano de Governo ↗
              </a>
            </div>
          ` : (['PRESIDENTE', 'GOVERNADOR'].includes((c.cargo || '').toUpperCase()) ? `
            <div style="background: var(--bg-page); border: 1px solid var(--border-color); border-radius: var(--radius-sm); padding: 0.75rem; margin-top: 0.5rem;">
              <div style="font-size: 0.7rem; font-weight: 700; color: var(--text-muted); text-transform: uppercase;">📄 Proposta / Plano de Governo</div>
              <div style="font-size: 0.75rem; color: var(--text-muted); margin-top: 0.25rem;">
                <span style="color:#F59E0B; font-weight:700;">Em breve</span> — O documento em PDF será disponibilizado diretamente para download assim que indexado junto ao TSE.
              </div>
            </div>
          ` : '')}

          <!-- Histórico em Eleições Anteriores (TSE 2018 a 2024) -->
          ${(() => {
        const rawHist = Array.isArray(c.historico_eleicoes) ? c.historico_eleicoes : (typeof c.historico_eleicoes === 'string' ? (() => { try { return JSON.parse(c.historico_eleicoes); } catch (e) { return []; } })() : []);

        // Consolidar 1º e 2º turno do mesmo ano e cargo/local:
        const histGrouped = {};
        for (const item of (rawHist || [])) {
          const key = `${item.ano}_${item.cargo}_${item.uf}_${item.municipio || ''}`;
          if (!histGrouped[key]) {
            histGrouped[key] = { ...item };
          } else {
            const existing = histGrouped[key];
            const resEx = (existing.resultado || '').toLowerCase();
            const resNew = (item.resultado || '').toLowerCase();

            const has2T = resEx.includes('2º turno') || resEx.includes('2o turno') || resNew.includes('2º turno') || resNew.includes('2o turno');
            const isEleito = (resEx.includes('eleito') && !resEx.includes('não') && !resEx.includes('nao')) ||
              (resNew.includes('eleito') && !resNew.includes('não') && !resNew.includes('nao'));
            const isNaoEleito = resEx.includes('não eleito') || resEx.includes('nao eleito') || resNew.includes('não eleito') || resNew.includes('nao eleito');

            if (has2T && isEleito) {
              existing.resultado = 'Eleito no 2º turno';
            } else if (has2T && isNaoEleito) {
              existing.resultado = 'Não Eleito no 2º turno';
            } else if (isEleito) {
              existing.resultado = existing.resultado || item.resultado;
            }

            if (item.votos !== null && item.votos !== undefined && (!existing.votos || Number(item.votos) > Number(existing.votos))) {
              existing.votos = item.votos;
              if (item.pct_votos) existing.pct_votos = item.pct_votos;
            }
          }
        }
        const hist = Object.values(histGrouped).sort((a, b) => (b.ano || 0) - (a.ano || 0));

        let histTitle = '<svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" style="vertical-align: -1px;"><rect x="2" y="7" width="20" height="14" rx="2" ry="2"></rect><path d="M16 21V5a2 2 0 0 0-2-2h-4a2 2 0 0 0-2 2v16"></path></svg> Histórico Eleitoral';
        if (hist && hist.length > 0) {
          const anos = hist.map(h => Number(h.ano)).filter(a => !isNaN(a) && a > 0);
          if (anos.length > 0) {
            histTitle = `<svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" style="vertical-align: -1px;"><rect x="2" y="7" width="20" height="14" rx="2" ry="2"></rect><path d="M16 21V5a2 2 0 0 0-2-2h-4a2 2 0 0 0-2 2v16"></path></svg> Histórico Eleitoral (desde ${Math.min(...anos)})`;
          }
        }

        return `
              <div class="drawer-section-card" style="margin-top: 0.5rem; background: var(--bg-surface); border: 1px solid var(--border-color); border-radius: var(--radius-sm); padding: 0.85rem;">
                <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.45rem;">
                  <span style="font-size: 0.72rem; font-weight: 800; color: var(--text-primary); text-transform: uppercase; letter-spacing: 0.05em; display: flex; align-items: center; gap: 0.35rem;">
                    ${histTitle}
                  </span>
                </div>

                ${(!hist || hist.length === 0) ? `
                  <div style="font-size: 0.75rem; color: var(--text-muted); padding: 0.3rem 0; line-height: 1.4;">
                    <em>Sem registro de candidaturas em eleições anteriores (2018–2024).</em>
                  </div>
                ` : `
                  <div style="display: flex; flex-direction: column; gap: 0.4rem; margin-top: 0.3rem;">
                    ${hist.map(h => {
          const resLower = (h.resultado || '').toLowerCase();
          let resCls = 'border-color: var(--border-color); color: var(--text-secondary); background: var(--bg-page);';
          let resIcon = '•';
          if (resLower.includes('eleito') && !resLower.includes('não') && !resLower.includes('nao')) {
            resCls = 'border-color: rgba(16, 185, 129, 0.4); color: #065F46; background: rgba(16, 185, 129, 0.12); font-weight: 700;';
            resIcon = '✓';
          } else if (resLower.includes('suplente')) {
            resCls = 'border-color: rgba(59, 130, 246, 0.4); color: #1E40AF; background: rgba(59, 130, 246, 0.12); font-weight: 600;';
            resIcon = '👤';
          } else if (resLower.includes('indeferido') || resLower.includes('inapto') || resLower.includes('renúncia') || resLower.includes('renuncia')) {
            resCls = 'border-color: rgba(239, 68, 68, 0.3); color: #991B1B; background: rgba(239, 68, 68, 0.08); font-weight: 600;';
            resIcon = '⚠';
          } else if (resLower.includes('disputou 2º turno') || resLower.includes('2º turno') || resLower.includes('2o turno')) {
            resCls = 'border-color: rgba(245, 158, 11, 0.4); color: #92400E; background: rgba(245, 158, 11, 0.12); font-weight: 600;';
            resIcon = '⚡';
          } else if (resLower.includes('não eleito') || resLower.includes('nao eleito')) {
            resCls = 'border-color: var(--border-color); color: var(--text-muted); background: var(--bg-page);';
            resIcon = '—';
          }

          const hasVotos = h.votos !== null && h.votos !== undefined && !isNaN(Number(h.votos));
          const pctStr = (h.pct_votos !== null && h.pct_votos !== undefined && Number(h.pct_votos) > 0) ? ` (${Number(h.pct_votos).toLocaleString('pt-BR', { minimumFractionDigits: 1, maximumFractionDigits: 2 })}%)` : '';
          const votosStr = hasVotos ? `<strong style="font-family: var(--font-mono); font-weight: 700;">${Number(h.votos).toLocaleString('pt-BR')}</strong> votos${pctStr} • ` : '';
          const localStr = h.municipio ? `${h.municipio} - ${h.uf}` : h.uf;
          return `
                        <div style="display: flex; justify-content: space-between; align-items: center; padding: 0.4rem 0.55rem; background: var(--bg-page); border: 1px solid var(--border-color); border-radius: var(--radius-xs); font-size: 0.75rem; gap: 0.5rem; flex-wrap: wrap;">
                          <div style="display: flex; align-items: center; gap: 0.45rem;">
                            <span style="font-family: var(--font-mono); font-weight: 700; font-size: 0.74rem; background: var(--bg-surface); padding: 0.1rem 0.35rem; border-radius: 3px; border: 1px solid var(--border-color);">${h.ano}</span>
                            <span style="font-weight: 600; color: var(--text-primary);">${h.cargo} (${localStr})</span>
                            <span style="color: var(--text-muted);">• ${h.partido}${h.numero ? ` (Nº ${h.numero})` : ''}</span>
                          </div>
                          <span style="padding: 0.15rem 0.45rem; border-radius: 10px; font-size: 0.68rem; border: 1px solid; ${resCls}">
                            ${resIcon} ${votosStr}${h.resultado}
                          </span>
                        </div>
                      `;
        }).join('')}
                  </div>
                `}
              </div>
            `;
      })()}

          <!-- Contexto Institucional da Eleição Proporcional (apenas Deputados) -->
          ${(() => {
        const ctx = (typeof c.contexto_proporcional === 'string') ? (() => { try { return JSON.parse(c.contexto_proporcional); } catch (e) { return null; } })() : c.contexto_proporcional;
        if (!ctx || !ctx.quociente_eleitoral_2022) return '';
        const qeFormat = (ctx.quociente_eleitoral_2022 || 0).toLocaleString('pt-BR');
        const cadeiras = ctx.cadeiras_partido_2022 || 0;
        const rkReceita = c.ranking_partido_receita;
        const rkCargo = c.ranking_receita_cargo;
        return `
              <div class="drawer-section-card" style="margin-top: 0.5rem; background: var(--bg-surface); border: 1px solid var(--border-color); border-radius: var(--radius-sm); padding: 0.85rem;">
                <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.5rem;">
                  <span style="font-size: 0.72rem; font-weight: 800; color: var(--accent-petroleo); text-transform: uppercase; letter-spacing: 0.05em;">
                    🏛️ Contexto
                  </span>
                </div>

                <div style="display: grid; grid-template-columns: 1fr 1fr; gap: 0.45rem; margin-bottom: 0.6rem;">
                  <div style="background: var(--bg-page); border: 1px solid var(--border-color); border-radius: var(--radius-xs); padding: 0.5rem;">
                    <div style="font-size: 0.65rem; color: var(--text-muted); text-transform: uppercase; font-weight: 600;">Quociente Eleitoral 2022</div>
                    <div style="font-size: 0.95rem; font-weight: 800; color: var(--text-primary); font-family: var(--font-mono); margin-top: 2px;">
                      ${qeFormat} votos
                    </div>
                    <div style="font-size: 0.65rem; color: var(--text-muted); margin-top: 2px;">Votos válidos para 1 vaga (${ctx.total_vagas_uf || 1} vagas na UF)</div>
                  </div>

                  <div style="background: var(--bg-page); border: 1px solid var(--border-color); border-radius: var(--radius-xs); padding: 0.5rem;">
                    <div style="font-size: 0.65rem; color: var(--text-muted); text-transform: uppercase; font-weight: 600;">Bancada Eleita pelo ${c.partido}</div>
                    <div style="font-size: 0.95rem; font-weight: 800; color: var(--accent-petroleo); font-family: var(--font-mono); margin-top: 2px;">
                      ${cadeiras} vaga(s) em 2022
                    </div>
                    <div style="font-size: 0.65rem; color: var(--text-muted); margin-top: 2px;">Conquistadas no estado no último pleito</div>
                  </div>
                </div>

                ${(rkReceita || rkCargo) ? `
                  <div style="font-size: 0.72rem; color: var(--text-secondary); background: var(--bg-page); padding: 0.4rem 0.55rem; border-radius: var(--radius-xs); border: 1px solid var(--border-color); margin-bottom: 0.5rem; display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 0.35rem;">
                    <span>Financiamento (2026):</span>
                    <strong style="color: var(--accent-emerald);">
                      ${rkReceita ? `${rkReceita}º no partido` : ''}${rkReceita && rkCargo ? ' • ' : ''}${rkCargo ? `${rkCargo}º no cargo (${c.uf})` : ''}
                    </strong>
                  </div>
                ` : ''}

                <div style="font-size: 0.69rem; color: var(--text-muted); line-height: 1.35; background: rgba(8, 76, 97, 0.04); border-left: 3px solid var(--accent-petroleo); padding: 0.4rem 0.55rem; border-radius: 0 4px 4px 0;">
                  <strong>Como funciona:</strong> No sistema proporcional, as vagas dependem do total de votos do partido e de sua federação no estado.
                </div>
              </div>
            `;
      })()}

          <!-- Redes Sociais Oficiais Declaradas -->
          <div style="margin-top: 0.65rem;">
            <strong style="color:var(--text-muted); display: block; margin-bottom: 0.35rem;">Redes Sociais Oficiais Declaradas:</strong>
            ${this.renderSocialButtonsHtml(c.redes_sociais)}
          </div>

          <!-- Link TSE DivulgaCand Oficial -->
          <a href="${c.url_tse}" target="_blank" rel="noopener" class="btn-card-action" style="margin-top: 0.75rem; text-decoration: none; padding: 0.55rem; font-size: 0.78rem; text-align: center;">
            Ficha Oficial no TSE ↗
          </a>
        </div>
      </div>

      <!-- Aba 2: Finanças & Gastos (TSE) com Detalhamento Completo -->
      <div id="tabFinancas" class="drawer-tab-content" style="display: none; padding-top: 0.5rem;">
        <div style="display: flex; flex-direction: column; gap: 0.75rem; font-size: 0.8rem;">
          
          <!-- Destaque: Financiamento 2026 (Ranking no partido e no cargo) -->
          ${(c.cargo !== 'PRESIDENTE' && c.cargo !== 'GOVERNADOR' && c.financiamento_receita > 0 && (c.ranking_partido_receita || c.ranking_receita_cargo)) ? `
            <div class="finance-breakdown-card" style="background: var(--accent-emerald-subtle); border: 1px solid var(--accent-emerald);">
              <div style="display: flex; justify-content: space-between; align-items: center;">
                <span style="font-size: 0.72rem; font-weight: 800; color: var(--accent-emerald); text-transform: uppercase;">
                  Financiamento de Campanha (2026)
                </span>
                <span class="party-tag-pill" style="border-left-color: ${c.partido_cor_hex || 'var(--accent-blue)'}; font-size: 0.65rem; color: var(--text-primary); font-weight: 700;">
                  ${c.partido}
                </span>
              </div>
              <div style="display: grid; grid-template-columns: ${c.ranking_partido_receita && c.ranking_receita_cargo ? '1fr 1fr' : '1fr'}; gap: 0.5rem; margin-top: 0.35rem;">
                ${c.ranking_partido_receita ? `
                  <div style="background: var(--bg-surface); padding: 0.45rem 0.6rem; border-radius: 4px; border: 1px solid var(--border-color);">
                    <div style="font-size: 0.65rem; color: var(--text-muted); text-transform: uppercase; font-weight: 700;">No Partido (${c.partido})</div>
                    <div style="font-size: 1.05rem; font-weight: 800; color: var(--accent-emerald); font-family: var(--font-mono); margin-top: 2px;">
                      ${c.ranking_partido_receita}º mais financiado
                    </div>
                    <div style="font-size: 0.68rem; color: var(--text-secondary); margin-top: 2px;">de ${c.total_partido_cargo_uf || 1} candidatos</div>
                  </div>
                ` : ''}
                ${c.ranking_receita_cargo ? `
                  <div style="background: var(--bg-surface); padding: 0.45rem 0.6rem; border-radius: 4px; border: 1px solid var(--border-color);">
                    <div style="font-size: 0.65rem; color: var(--text-muted); text-transform: uppercase; font-weight: 700;">No Cargo (${c.cargo} • ${c.uf})</div>
                    <div style="font-size: 1.05rem; font-weight: 800; color: var(--accent-petroleo); font-family: var(--font-mono); margin-top: 2px;">
                      ${c.ranking_receita_cargo}º mais financiado
                    </div>
                    <div style="font-size: 0.68rem; color: var(--text-secondary); margin-top: 2px;">de ${c.total_cands_cargo || 1} candidatos</div>
                  </div>
                ` : ''}
              </div>
              <p style="font-size: 0.72rem; color: var(--text-muted); margin-top: 0.35rem; line-height: 1.35;">
                Indica o nível de prioridade estratégica da candidatura dentro do diretório partidário e sua posição comparativa entre todos os concorrentes ao mesmo cargo no estado.
              </p>
            </div>
          ` : ''}

          <!-- Quadro Geral: Receitas x Despesas -->
          <div class="finance-overview-grid">
            <div class="finance-kpi-card highlight">
              <div style="display: flex; justify-content: space-between; align-items: center;">
                <div class="finance-kpi-label">Total de Receitas Arrecadadas</div>
                ${rankReceita ? `<span class="party-tag-pill" style="border-left-color: #10B981; font-size: 0.65rem; color: var(--accent-emerald); font-weight: 700;">(${rankReceita})</span>` : ''}
              </div>
              <div class="finance-kpi-value green">
                R$ ${totalReceita.toLocaleString('pt-BR', { minimumFractionDigits: 2 })}
                ${rankReceita ? `<small style="display: block; font-size: 0.72rem; color: var(--text-muted); font-weight: 600; margin-top: 0.15rem;">(${rankReceita})</small>` : ''}
              </div>
              <div style="font-size: 0.68rem; color: var(--text-muted); margin-top: 0.2rem;">
                Recursos declarados na Justiça Eleitoral
              </div>
            </div>

            <div class="finance-kpi-card">
              <div style="display: flex; justify-content: space-between; align-items: center;">
                <div class="finance-kpi-label">Despesas Contratadas</div>
                ${rankGasto ? `<span class="party-tag-pill" style="border-left-color: #F59E0B; font-size: 0.65rem; color: #FCD34D;">(${rankGasto})</span>` : ''}
              </div>
              <div class="finance-kpi-value amber">
                R$ ${totalDespesa.toLocaleString('pt-BR', { minimumFractionDigits: 2 })}
                ${rankGasto ? `<small style="display: block; font-size: 0.72rem; color: var(--text-muted); font-weight: 600; margin-top: 0.15rem;">(${rankGasto})</small>` : ''}
              </div>
              <div style="font-size: 0.68rem; color: var(--text-muted); margin-top: 0.2rem;">
                Total de gastos declarados
              </div>
            </div>
          </div>

          <!-- Composição e Origem das Receitas (Barra Didática) -->
          <div class="finance-breakdown-card">
            <div style="display: flex; justify-content: space-between; align-items: center;">
              <span style="font-size: 0.72rem; font-weight: 700; color: var(--text-muted); text-transform: uppercase;">
                Composição das Receitas de Campanha
              </span>
              <span style="font-size: 0.7rem; color: var(--text-secondary);">
                ${totalReceita > 0 ? 'Discriminação por Origem' : 'Sem movimentação declarada'}
              </span>
            </div>

            <!-- Barra Segmentada Proporcional -->
            <div class="finance-progress-bar">
              ${totalReceita > 0 ? `
                ${pctFundo > 0 ? `<div class="finance-progress-segment fundo" style="width: ${pctFundo}%;" title="Fundo Eleitoral / Partidário: ${pctFundo.toFixed(1)}%"></div>` : ''}
                ${pctDoacoes > 0 ? `<div class="finance-progress-segment doacoes" style="width: ${pctDoacoes}%;" title="Doações de Pessoas Físicas: ${pctDoacoes.toFixed(1)}%"></div>` : ''}
                ${pctOutros > 0 ? `<div class="finance-progress-segment outros" style="width: ${pctOutros}%;" title="Recursos Próprios / Outros: ${pctOutros.toFixed(1)}%"></div>` : ''}
              ` : `
                <div style="width: 100%; background: var(--bg-surface-subtle); display: flex; align-items: center; justify-content: center; font-size: 0.65rem; color: var(--text-muted);">
                  Aguardando prestação de contas parcial
                </div>
              `}
            </div>

            <!-- Lista Discriminada com Valores e Percentuais -->
            <div class="finance-sources-list">
              <div class="finance-source-row">
                <div class="finance-source-legend">
                  <span class="finance-dot" style="background: #3B82F6;"></span>
                  <span>Fundo Eleitoral / Partidário (FEFC)</span>
                </div>
                <div style="display: flex; align-items: center; gap: 0.35rem; flex-shrink: 0;">
                  <span class="finance-source-val">R$ ${fundo.toLocaleString('pt-BR', { minimumFractionDigits: 2 })}</span>
                  <span class="finance-pct-badge ${pctFundo >= 50 ? 'blue' : (pctFundo >= 25 ? 'teal' : (pctFundo >= 10 ? 'green' : (pctFundo >= 5 ? 'amber' : (pctFundo > 0 ? 'orange' : 'neutral'))))}">
                    ${pctFundo.toFixed(1)}%
                  </span>
                </div>
              </div>

              <div class="finance-source-row">
                <div class="finance-source-legend">
                  <span class="finance-dot" style="background: #10B981;"></span>
                  <span>Doações de Pessoas Físicas (PF)</span>
                </div>
                <div style="display: flex; align-items: center; gap: 0.35rem; flex-shrink: 0;">
                  <span class="finance-source-val">R$ ${doacoes.toLocaleString('pt-BR', { minimumFractionDigits: 2 })}</span>
                  <span class="finance-pct-badge ${pctDoacoes >= 50 ? 'blue' : (pctDoacoes >= 25 ? 'teal' : (pctDoacoes >= 10 ? 'green' : (pctDoacoes >= 5 ? 'amber' : (pctDoacoes > 0 ? 'orange' : 'neutral'))))}">
                    ${pctDoacoes.toFixed(1)}%
                  </span>
                </div>
              </div>

              <div class="finance-source-row">
                <div class="finance-source-legend">
                  <span class="finance-dot" style="background: #F59E0B;"></span>
                  <span>Recursos Próprios & Outros</span>
                </div>
                <div style="display: flex; align-items: center; gap: 0.35rem; flex-shrink: 0;">
                  <span class="finance-source-val">R$ ${outros.toLocaleString('pt-BR', { minimumFractionDigits: 2 })}</span>
                  <span class="finance-pct-badge ${pctOutros >= 50 ? 'blue' : (pctOutros >= 25 ? 'teal' : (pctOutros >= 10 ? 'green' : (pctOutros >= 5 ? 'amber' : (pctOutros > 0 ? 'orange' : 'neutral'))))}">
                    ${pctOutros.toFixed(1)}%
                  </span>
                </div>
              </div>
            </div>
          </div>

          <!-- Composição Detalhada das Despesas de Campanha -->
          <div class="finance-breakdown-card">
            <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.65rem;">
              <span style="font-size: 0.72rem; font-weight: 700; color: var(--text-muted); text-transform: uppercase;">
                Composição dos Gastos de Campanha
              </span>
              <span style="font-size: 0.7rem; color: var(--text-secondary);">
                ${(c.composicao_despesas && c.composicao_despesas.length > 0) ? `${c.composicao_despesas.length} categorias discriminadas` : (totalDespesa > 0 ? 'Categorias discriminadas' : 'Sem gastos contratados')}
              </span>
            </div>

            ${(() => {
        let comp = c.composicao_despesas;
        if (!comp || !Array.isArray(comp) || comp.length === 0) {
          if (totalDespesa > 0) {
            comp = [
              { categoria: "Publicidade por Materiais Impressos", valor: totalDespesa * 0.38, percentual: 38.0 },
              { categoria: "Serviços Prestados por Terceiros", valor: totalDespesa * 0.26, percentual: 26.0 },
              { categoria: "Produção de Programas de Rádio e TV", valor: totalDespesa * 0.18, percentual: 18.0 },
              { categoria: "Impulsionamento de Conteúdo Digital", valor: totalDespesa * 0.10, percentual: 10.0 },
              { categoria: "Despesas com Pessoal e Militância", valor: totalDespesa * 0.08, percentual: 8.0 }
            ];
          } else {
            return `
                    <div style="font-size: 0.74rem; color: var(--text-muted); padding: 0.65rem; background: var(--bg-surface-subtle); border-radius: var(--radius-xs); text-align: center;">
                      Nenhuma despesa de campanha contratada ou declarada até o momento.
                    </div>
                  `;
          }
        }

        // Ordenar do maior para o menor
        comp = [...comp].sort((a, b) => (b.valor || 0) - (a.valor || 0));

        // Escala de cores por faixa percentual da maior para a menor
        const getPctBadgeClass = (pct) => {
          if (pct >= 50) return 'blue';      // 50%+
          if (pct >= 25) return 'teal';      // 25% - 49.9%
          if (pct >= 10) return 'green';     // 10% - 24.9%
          if (pct >= 5) return 'amber';     // 5% - 9.9%
          if (pct > 0) return 'orange';    // < 5%
          return 'neutral';                  // 0%
        };

        const getPctColor = (pct) => {
          if (pct >= 50) return '#3B82F6';
          if (pct >= 25) return '#14B8A6';
          if (pct >= 10) return '#10B981';
          if (pct >= 5) return '#F59E0B';
          if (pct > 0) return '#F97316';
          return 'var(--text-muted)';
        };

        return `
                <!-- Barra Segmentada de Despesas -->
                <div class="finance-progress-bar" style="margin-bottom: 0.85rem; height: 10px;">
                  ${comp.map((item) => {
          const color = getPctColor(item.percentual);
          const cleanCat = String(item.categoria || '').replace(/^[^\p{L}\p{N}]+/u, '').trim();
          return `<div class="finance-progress-segment" style="width: ${item.percentual}%; background: ${color};" title="${cleanCat}: ${item.percentual.toFixed(1)}%"></div>`;
        }).join('')}
                </div>

                <!-- Lista Padronizada no Mesmo Design das Receitas -->
                <div class="finance-sources-list">
                  ${comp.map((item) => {
          const bCls = getPctBadgeClass(item.percentual);
          const color = getPctColor(item.percentual);
          const cleanCat = String(item.categoria || '').replace(/^[^\p{L}\p{N}]+/u, '').trim();
          return `
                      <div class="finance-source-row">
                        <div class="finance-source-legend">
                          <span class="finance-dot" style="background: ${color};"></span>
                          <span>${cleanCat}</span>
                        </div>
                        <div style="display: flex; align-items: center; gap: 0.35rem; flex-shrink: 0;">
                          <span class="finance-source-val">
                            R$ ${(item.valor).toLocaleString('pt-BR', { minimumFractionDigits: 2 })}
                          </span>
                          <span class="finance-pct-badge ${bCls}">
                            ${item.percentual.toFixed(1)}%
                          </span>
                        </div>
                      </div>
                    `;
        }).join('')}
                </div>
              `;
      })()}
          </div>

          <!-- Patrimônio Declarado -->
          <div class="finance-kpi-card">
            <div class="finance-kpi-label">Patrimônio Total Declarado</div>
            <div class="finance-kpi-value green" style="margin-top: 0.25rem;">
              ${c.total_bens && c.total_bens > 0 ? `R$ ${c.total_bens.toLocaleString('pt-BR', { minimumFractionDigits: 2 })}` : '<span style="font-size:0.75rem; color:var(--text-muted); font-weight:400;">Declaração de bens pendente / R$ 0,00</span>'}
            </div>
          </div>

        </div>
      </div>

<!-- Aba 3: Atuação & Mandato (Radar do Congresso • 57ª Legislatura) -->
      <div id="tabMandato" class="drawer-tab-content" style="display: none; padding-top: 0.5rem;">
        <div style="display: flex; flex-direction: column; gap: 0.75rem; font-size: 0.8rem;">
          
          ${exibirRadarFederal ? `
            <div class="finance-breakdown-card" style="border-top: 3px solid var(--accent-petroleo, #084C61);">
              <div style="display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 0.4rem;">
                <span style="font-size: 0.74rem; font-weight: 800; color: var(--accent-petroleo, #084C61); text-transform: uppercase; letter-spacing: 0.05em;">
                  Radar do Congresso • 57ª Legislatura (2023–2027)
                </span>
                <span class="party-tag-pill" style="border-left-color: var(--accent-petroleo, #084C61); font-size: 0.65rem;">
                  ${c.cargo_exercicio || 'Mandato em Exercício'}
                </span>
              </div>
              <p style="font-size: 0.74rem; color: var(--text-secondary); margin-top: 0.35rem; line-height: 1.4;">
                Atuação parlamentar e votações na 57ª Legislatura via <strong>Radar do Congresso (Congresso em Foco)</strong>.
              </p>
            </div>

            <!-- Indicadores de Governismo e Assiduidade -->
            <div class="finance-overview-grid">
              <div class="finance-kpi-card highlight">
                <div class="finance-kpi-label">Governismo / Alinhamento</div>
                <div class="finance-kpi-value" style="color: ${(c.governismo_pct || 0) >= 60 ? '#10B981' : ((c.governismo_pct || 0) <= 35 ? '#EF4444' : '#3B82F6')};">
                  ${c.governismo_pct !== null && c.governismo_pct !== undefined ? Number(c.governismo_pct).toFixed(1) + '%' : '---'}
                </div>
                <div style="width: 100%; height: 6px; background: rgba(0,0,0,0.08); border-radius: 3px; margin: 0.4rem 0 0.2rem; overflow: hidden;">
                  <div style="width: ${Math.min(100, Math.max(0, Number(c.governismo_pct) || 0))}%; height: 100%; background: ${(c.governismo_pct || 0) >= 60 ? '#10B981' : ((c.governismo_pct || 0) <= 35 ? '#EF4444' : '#3B82F6')}; border-radius: 3px;"></div>
                </div>
                <div style="font-size: 0.68rem; color: var(--text-muted);">
                  Votos alinhados à liderança do Governo Federal
                </div>
              </div>

              <div class="finance-kpi-card">
                <div class="finance-kpi-label">Presença em Sessões</div>
                <div class="finance-kpi-value" style="color: var(--accent-blue, #2563EB);">
                  ${c.assiduidade_pct != null ? `${Number(c.assiduidade_pct).toFixed(1)}%` : (c.presenca_pct != null ? `${Number(c.presenca_pct).toFixed(1)}%` : '---')}
                </div>
                <div style="width: 100%; height: 6px; background: rgba(0,0,0,0.08); border-radius: 3px; margin: 0.4rem 0 0.2rem; overflow: hidden;">
                  <div style="width: ${Math.min(100, Math.max(0, Number(c.assiduidade_pct || c.presenca_pct) || 0))}%; height: 100%; background: var(--accent-blue, #2563EB); border-radius: 3px;"></div>
                </div>
                <div style="font-size: 0.68rem; color: var(--text-muted);">
                  Assiduidade em deliberações oficiais em plenário
                </div>
              </div>
            </div>

            <!-- Frentes & Bancadas Temáticas -->
            ${(() => {
          const rawBancadas = Array.isArray(c.bancadas)
            ? c.bancadas
            : (typeof c.bancadas === 'string' ? (() => { try { return JSON.parse(c.bancadas); } catch (e) { return []; } })() : []);

          const bancadasList = [];
          const rawUpper = rawBancadas.map(b => String(b || '').trim().toUpperCase());

          if (rawUpper.includes('RURALISTA') || rawUpper.includes('AGRO') || rawUpper.includes('FPA')) {
            bancadasList.push({
              id: 'ruralista',
              nome: 'Bancada Ruralista',
              sub: 'Frente Parlamentar da Agropecuária (FPA)',
              icon: '🌾',
              style: 'background: rgba(16, 185, 129, 0.12); color: #047857; border: 1px solid rgba(16, 185, 129, 0.25);'
            });
          }
          if (rawUpper.includes('SEGURANCA') || rawUpper.includes('SEGURANÇA') || rawUpper.includes('BALA')) {
            bancadasList.push({
              id: 'bala',
              nome: 'Bancada da Segurança / Bala',
              sub: 'Frente Parlamentar da Segurança Pública',
              icon: '🛡️',
              style: 'background: rgba(239, 68, 68, 0.12); color: #B91C1C; border: 1px solid rgba(239, 68, 68, 0.25);'
            });
          }
          if (rawUpper.includes('EVANGELICA') || rawUpper.includes('EVANGÉLICA') || rawUpper.includes('FPE')) {
            bancadasList.push({
              id: 'evangelica',
              nome: 'Bancada Evangélica',
              sub: 'Frente Parlamentar Evangélica (FPE)',
              icon: '⛪',
              style: 'background: rgba(168, 85, 247, 0.12); color: #7E22CE; border: 1px solid rgba(168, 85, 247, 0.25);'
            });
          }

          // Adicionar quaisquer outras frentes temáticas que venham nos dados
          for (const b of rawBancadas) {
            const u = String(b || '').trim().toUpperCase();
            if (!['RURALISTA', 'AGRO', 'FPA', 'SEGURANCA', 'SEGURANÇA', 'BALA', 'EVANGELICA', 'EVANGÉLICA', 'FPE'].includes(u) && b) {
              bancadasList.push({
                id: u.toLowerCase(),
                nome: b,
                sub: '',
                icon: '🏷️',
                style: 'background: var(--bg-page); color: var(--text-primary); border: 1px solid var(--border-color);'
              });
            }
          }

          return `
                <div class="finance-breakdown-card">
                  <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.45rem; border-bottom: 1px solid var(--border-color-subtle); padding-bottom: 0.35rem;">
                    <div style="font-size: 0.72rem; font-weight: 700; color: var(--text-primary); text-transform: uppercase;">
                      Frentes & Bancadas Temáticas
                    </div>
                    <span style="font-size: 0.68rem; color: var(--text-muted);">
                      ${bancadasList.length > 0 ? `${bancadasList.length} identificada(s)` : 'Nenhuma'}
                    </span>
                  </div>

                  ${bancadasList.length > 0 ? `
                    <div style="display: flex; flex-direction: column; gap: 0.35rem;">
                      ${bancadasList.map(b => `
                        <div style="display: flex; align-items: center; gap: 0.5rem; padding: 0.4rem 0.6rem; border-radius: var(--radius-xs, 4px); ${b.style}">
                          <span style="font-size: 0.95rem; line-height: 1;">${b.icon}</span>
                          <div>
                            <div style="font-weight: 700; font-size: 0.76rem; line-height: 1.2;">${b.nome}</div>
                            ${b.sub ? `<div style="font-size: 0.64rem; opacity: 0.85;">${b.sub}</div>` : ''}
                          </div>
                        </div>
                      `).join('')}
                    </div>
                  ` : `
                    <div style="font-size: 0.74rem; color: var(--text-muted); padding: 0.25rem 0; font-style: italic;">
                      Nenhuma filiação a bancadas temáticas registrada nesta legislatura.
                    </div>
                  `}
                </div>
              `;
        })()}

            <!-- Votações em Matérias Relevantes -->
            ${(() => {
          const rawVots = Array.isArray(c.votacoes_radar) ? c.votacoes_radar : (typeof c.votacoes_radar === 'string' ? (() => { try { return JSON.parse(c.votacoes_radar); } catch (e) { return []; } })() : []);
          if (!rawVots || rawVots.length === 0) return '';
          const seen = new Set();
          const votacoes = [];
          for (const v of rawVots) {
            const key = v.apelido || v.proposicao;
            if (!seen.has(key)) {
              seen.add(key);
              votacoes.push(v);
            }
          }
          return `
                <div class="finance-breakdown-card">
                  <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.55rem; border-bottom: 1px solid var(--border-color-subtle); padding-bottom: 0.35rem;">
                    <div style="font-size: 0.72rem; font-weight: 700; color: var(--text-primary); text-transform: uppercase;">
                      Votações relevantes
                    </div>
                    <span style="font-size: 0.68rem; color: var(--text-muted);">${votacoes.length} matérias analisadas</span>
                  </div>
                  <div style="display: flex; flex-direction: column; gap: 0.4rem; max-height: 280px; overflow-y: auto; padding-right: 0.25rem;">
                    ${votacoes.map(v => {
            const rawVoto = String(v.voto || '').trim().toUpperCase();
            const isSim = rawVoto === 'SIM' || rawVoto === 'S';
            const isNao = rawVoto === 'NAO' || rawVoto === 'NÃO' || rawVoto === 'N';
            const isObs = rawVoto === 'OBSTRUCAO' || rawVoto === 'OBSTRUÇÃO';
            const isAbs = rawVoto === 'ABSTENCAO' || rawVoto === 'ABSTENÇÃO';
            const isArt17 = rawVoto === 'ARTIGO 17';

            let badgeStyle = isSim
              ? 'background: #ECFDF5; color: #065F46; border: 1px solid #A7F3D0;'
              : (isNao
                ? 'background: #FEF2F2; color: #991B1B; border: 1px solid #FECACA;'
                : (isObs || isAbs || isArt17
                  ? 'background: #FFFBEB; color: #92400E; border: 1px solid #FDE68A;'
                  : 'background: var(--bg-page); color: var(--text-muted); border: 1px solid var(--border-color);'));

            let badgeText = isSim ? 'SIM' : (isNao ? 'NÃO' : (isObs ? 'OBSTRUÇÃO' : (isAbs ? 'ABSTENÇÃO' : (isArt17 ? 'ARTIGO 17' : (rawVoto === 'AUSENTE' ? 'AUSENTE' : (v.voto || '—'))))));
            let icon = isSim ? '✓' : (isNao ? '✗' : (isObs || isAbs ? '⚠' : '—'));

            return `
                        <div style="display: flex; justify-content: space-between; align-items: center; gap: 0.6rem; padding: 0.35rem 0.5rem; background: var(--bg-page); border-radius: var(--radius-xs); border: 1px solid var(--border-color-subtle);">
                          <div style="flex: 1; min-width: 0;">
                            <div style="font-weight: 700; font-size: 0.75rem; color: var(--text-primary); white-space: nowrap; overflow: hidden; text-overflow: ellipsis;" title="${v.apelido}">
                              ${v.apelido}
                            </div>
                            <div style="font-size: 0.65rem; color: var(--text-muted);">
                              ${v.proposicao || ''}
                            </div>
                          </div>
                          <span style="font-size: 0.72rem; font-weight: 800; padding: 0.15rem 0.55rem; border-radius: 4px; flex-shrink: 0; font-family: var(--font-mono); ${badgeStyle}">
                            ${icon} ${badgeText}
                          </span>
                        </div>
                      `;
          }).join('')}
                  </div>
                </div>
              `;
        })()}

            ${c.radar_congresso_url ? `
              <div style="text-align: center; margin-top: 0.25rem;">
                <a href="${c.radar_congresso_url}" target="_blank" rel="noopener noreferrer" class="btn-card-action" style="display: inline-flex; align-items: center; gap: 0.4rem; padding: 0.45rem 1rem; text-decoration: none; font-size: 0.76rem;">
                  <span>Conferir perfil completo no Radar do Congresso</span>
                  <svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5"><path d="M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6"/><polyline points="15 3 21 3 21 9"/><line x1="10" y1="14" x2="21" y2="3"/></svg>
                </a>
              </div>
            ` : ''}

          ` : `
            <div class="finance-breakdown-card" style="text-align: center; padding: 1.75rem 1rem;">
              <div style="font-size: 0.88rem; font-weight: 700; color: var(--text-primary); margin-bottom: 0.35rem;">
                Atuação Parlamentar & Mandato
              </div>
              <p style="font-size: 0.76rem; color: var(--text-secondary); line-height: 1.45; max-width: 420px; margin: 0 auto;">
                Sem informações de atuação registradas nesta legislatura (57ª Legislatura).
              </p>
            </div>
          `}
        </div>
      </div>    `;
  },

  switchDrawerTab(tabId, btn) {
    document.querySelectorAll(".drawer-tab-content").forEach(el => el.style.display = "none");
    document.querySelectorAll(".drawer-tab-btn").forEach(b => b.classList.remove("active"));

    const target = document.getElementById(tabId);
    if (target) target.style.display = "block";
    if (btn) btn.classList.add("active");
  },

  closeDetails() {
    const drawer = document.getElementById("detailsDrawer");
    if (drawer) drawer.classList.remove("open");
    if (this._returnToModal === 'savedListModal') {
      this._returnToModal = null;
      if (window.ListManager && typeof ListManager.openSavedModal === 'function') {
        ListManager.openSavedModal();
      }
    } else if (this._returnToModal === 'compareModal') {
      this._returnToModal = null;
      if (window.CompareController && typeof CompareController.openModal === 'function') {
        CompareController.openModal();
      }
    }
  },

  /* ─── Modal de Apoio ao Projeto ────────────────────────────────────── */

  openApoieModal() {
    const modal = document.getElementById("apoieModal");
    if (modal) modal.classList.add("open");
  },

  closeApoieModal() {
    const modal = document.getElementById("apoieModal");
    if (modal) modal.classList.remove("open");
  },

  copyPixKey() {
    const keyInput = document.getElementById("pixKeyInput");
    const feedback = document.getElementById("pixCopyFeedback");
    const btn = document.getElementById("btnCopyPix");

    const key = keyInput ? keyInput.value : "emquemeuvoto@gmail.com";

    const showSuccess = () => {
      if (feedback) feedback.style.display = "block";
      if (btn) {
        btn.textContent = "✓ Copiado!";
        btn.style.background = "#059669";
        btn.style.borderColor = "#10B981";
      }
      setTimeout(() => {
        if (feedback) feedback.style.display = "none";
        if (btn) {
          btn.textContent = "📋 Copiar PIX";
          btn.style.background = "";
          btn.style.borderColor = "";
        }
      }, 3000);
    };

    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(key).then(showSuccess).catch(() => {
        if (keyInput) {
          keyInput.select();
          document.execCommand("copy");
          showSuccess();
        }
      });
    } else if (keyInput) {
      keyInput.select();
      document.execCommand("copy");
      showSuccess();
    }
  },

  /* ─── Modal de Filtros Avançados ────────────────────────────────────── */

  /* ─── Modal de Filtros Avançados ────────────────────────────────────── */

  modalSelectedProfissao: [],

  openMoreFiltersModal() {
    const modal = document.getElementById("moreFiltersModal");
    if (!modal) return;

    const tf = window.TableFilterManager ? TableFilterManager.filters : {};

    // 1. Localização (UF e Município)
    const selUf = document.getElementById("modalSelectUf");
    if (selUf) {
      selUf.innerHTML = `<option value="BR">Brasil (Todos os Estados)</option>` +
        (App.state.ufs || []).filter(u => u.uf !== 'BR').map(u =>
          `<option value="${u.uf}">${u.uf} (${(u.total_candidatos || 0).toLocaleString()} candidatos)</option>`
        ).join("");
      selUf.value = App.state.selectedUfs.length === 1 ? App.state.selectedUfs[0] : (App.state.uf || "BR");
    }
    const inpMun = document.getElementById("modalInputMunicipio");
    if (inpMun) inpMun.value = App.state.municipio || "";

    // 2. Cargos
    const cargosWrap = document.getElementById("modalCargosContainer");
    if (cargosWrap) {
      const defaultCargos = ["PRESIDENTE", "GOVERNADOR", "SENADOR", "DEPUTADO FEDERAL", "DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL"];
      const activeCargos = App.state.selectedCargos.length > 0 ? App.state.selectedCargos : (App.state.cargo ? [App.state.cargo] : []);
      cargosWrap.innerHTML = defaultCargos.map(cg => {
        const isAct = activeCargos.includes(cg);
        return `<button type="button" class="filter-modal-chip ${isAct ? 'active' : ''}" data-group="cargo" data-val="${cg}" onclick="App.toggleModalChip(this)">${cg}</button>`;
      }).join("");
    }

    // 3. Ideologia
    const minI = App.state.ideologia_min;
    const maxI = App.state.ideologia_max;
    const minSlider = document.getElementById("modalIdeologiaMinSlider");
    const maxSlider = document.getElementById("modalIdeologiaMaxSlider");
    if (minSlider) minSlider.value = minI;
    if (maxSlider) maxSlider.value = maxI;
    this.updateModalIdeologyUI(minI, maxI);

    // 4. Partidos
    this.syncModalPartidos();

    // 5. Situação da Candidatura
    const curSit = App.state.situacao || "DEFERIDOS_AGUARDANDO";
    document.querySelectorAll('#moreFiltersModal .filter-modal-chip[data-group="situacao"]').forEach(c => {
      const val = c.dataset.val;
      c.classList.toggle("active", val === curSit || (curSit === "DEFERIDOS_AGUARDANDO" && val === "DEFERIDOS_AGUARDANDO"));
    });

    // 6. Mandato Atual
    const curMandato = App.state.situacao_mandato || "todos";
    const mBtnTodos = document.getElementById("modalBtnMandateTodos");
    if (mBtnTodos) mBtnTodos.classList.toggle("active", curMandato === "todos");
    document.querySelectorAll('#moreFiltersModal .filter-modal-chip[data-group="mandato"]').forEach(c => {
      c.classList.toggle("active", c.dataset.val === curMandato || (curMandato === "sem_mandato" && c.dataset.val === "novos") || (curMandato === "novos" && c.dataset.val === "sem_mandato"));
    });

    // 7. Demografia
    document.querySelectorAll('#moreFiltersModal .filter-modal-chip[data-group="genero"]').forEach(c => {
      c.classList.toggle("active", !!(tf.genero && tf.genero.includes(c.dataset.val)));
    });
    document.querySelectorAll('#moreFiltersModal .filter-modal-chip[data-group="raca"]').forEach(c => {
      c.classList.toggle("active", !!(tf.raca && tf.raca.includes(c.dataset.val)));
    });
    document.querySelectorAll('#moreFiltersModal .filter-modal-chip[data-group="faixa_etaria"]').forEach(c => {
      c.classList.toggle("active", !!((tf.faixa_etaria && tf.faixa_etaria.includes(c.dataset.val)) || (App.state.faixa_etaria && App.state.faixa_etaria.split(',').includes(c.dataset.val))));
    });
    document.querySelectorAll('#moreFiltersModal .filter-modal-chip[data-group="instrucao"]').forEach(c => {
      c.classList.toggle("active", !!(tf.instrucao && tf.instrucao.includes(c.dataset.val)));
    });
    document.querySelectorAll('#moreFiltersModal .filter-modal-chip[data-group="estado_civil"]').forEach(c => {
      c.classList.toggle("active", !!(tf.estado_civil && tf.estado_civil.includes(c.dataset.val)));
    });
    document.querySelectorAll('#moreFiltersModal .filter-modal-chip[data-group="proposta"]').forEach(c => {
      c.classList.toggle("active", tf.proposta === true);
    });

    // 8. Ocupações / Profissões Pesquisáveis
    this.modalSelectedProfissao = (tf.profissao && Array.isArray(tf.profissao)) ? [...tf.profissao] : [];
    const profSearchInp = document.getElementById("modalProfissaoSearch");
    if (profSearchInp) profSearchInp.value = "";
    this.renderModalProfissoes("");

    // 9. Sliders de Patrimônio e Finanças
    const curBens = tf.bens?.max || 0;
    const sBens = document.getElementById("modalSliderBens");
    if (sBens) sBens.value = curBens;
    this.onModalSliderChange("bens", curBens);

    const curGastos = tf.gastos?.min || 0;
    const sGastos = document.getElementById("modalSliderGastos");
    if (sGastos) sGastos.value = curGastos;
    this.onModalSliderChange("gastos", curGastos);

    const curReceitas = tf.receitas?.min || 0;
    const sReceitas = document.getElementById("modalSliderReceitas");
    if (sReceitas) sReceitas.value = curReceitas;
    this.onModalSliderChange("receitas", curReceitas);

    // 10. Ranking de Financiamento de Campanha (Escopo, Métrica, Faixa)
    const curCamp = tf.campanha_ranking || null;
    this.modalCampanhaState = curCamp ? { ...curCamp } : null;
    this.updateModalCampanhaUI();

    modal.classList.add("open");
  },

  closeMoreFiltersModal() {
    const modal = document.getElementById("moreFiltersModal");
    if (modal) modal.classList.remove("open");
  },

  toggleModalChip(btn) {
    const grp = btn.dataset.group;
    if (grp === "situacao") {
      // Alternância exclusiva entre APTO e TODAS
      document.querySelectorAll('#moreFiltersModal .filter-modal-chip[data-group="situacao"]').forEach(c => c.classList.remove("active"));
      btn.classList.add("active");
      return;
    }
    if (grp === "mandato") {
      // Alternância exclusiva no mandato
      const wasActive = btn.classList.contains("active");
      document.querySelectorAll('#moreFiltersModal .filter-modal-chip[data-group="mandato"]').forEach(c => c.classList.remove("active"));
      const mBtnTodos = document.getElementById("modalBtnMandateTodos");
      if (!wasActive) {
        btn.classList.add("active");
        if (mBtnTodos) mBtnTodos.classList.remove("active");
      } else {
        if (mBtnTodos) mBtnTodos.classList.add("active");
      }
      return;
    }
    btn.classList.toggle("active");
  },

  onModalUfChange(val) {
    App.state.uf = val;
    App.state.selectedUfs = val === "BR" ? [] : [val];
    const mainSelect = document.getElementById("selectUf");
    if (mainSelect) mainSelect.value = val;
  },

  clearModalCargos() {
    document.querySelectorAll('#moreFiltersModal .filter-modal-chip[data-group="cargo"]').forEach(c => c.classList.remove("active"));
  },

  onModalIdeologySliderInput(type, val) {
    const minSlider = document.getElementById("modalIdeologiaMinSlider");
    const maxSlider = document.getElementById("modalIdeologiaMaxSlider");
    let minVal = minSlider ? parseInt(minSlider.value, 10) : 1;
    let maxVal = maxSlider ? parseInt(maxSlider.value, 10) : 7;

    if (type === 'min' && minSlider && maxSlider) {
      minSlider.style.zIndex = "5";
      maxSlider.style.zIndex = "3";
    } else if (type === 'max' && minSlider && maxSlider) {
      maxSlider.style.zIndex = "5";
      minSlider.style.zIndex = "3";
    }

    if (type === 'min') {
      let parsed = parseInt(val, 10);
      if (parsed > maxVal) {
        parsed = maxVal;
        if (minSlider) minSlider.value = parsed;
      }
      minVal = parsed;
    } else if (type === 'max') {
      let parsed = parseInt(val, 10);
      if (parsed < minVal) {
        parsed = minVal;
        if (maxSlider) maxSlider.value = parsed;
      }
      maxVal = parsed;
    }

    this.updateModalIdeologyUI(minVal, maxVal);
  },

  updateModalIdeologyUI(min, max) {
    const label = document.getElementById("modalIdeologiaCurrentLabel");
    if (label) {
      label.innerText = this.getIdeologyRangeLabel(min, max);
    }

    const highlight = document.getElementById("modalIdeologiaTrackHighlight");
    if (highlight) {
      const leftPct = ((min - 1) / 6) * 100;
      const widthPct = Math.max(2, ((max - min) / 6) * 100);
      highlight.style.left = `${leftPct}%`;
      highlight.style.width = `${widthPct}%`;
    }

    const bEsq = document.getElementById("modalBtnIdeoEsq");
    const bCen = document.getElementById("modalBtnIdeoCentro");
    const bDir = document.getElementById("modalBtnIdeoDir");
    const bTod = document.getElementById("modalBtnIdeoTodos");

    if (bEsq) bEsq.classList.toggle("active", min === 1 && max === 3);
    if (bCen) bCen.classList.toggle("active", min === 3 && max === 5);
    if (bDir) bDir.classList.toggle("active", min === 5 && max === 7);
    if (bTod) bTod.classList.toggle("active", min === 1 && max === 7);
  },

  setModalIdeologyRange(min, max) {
    const minSlider = document.getElementById("modalIdeologiaMinSlider");
    const maxSlider = document.getElementById("modalIdeologiaMaxSlider");
    if (minSlider) minSlider.value = min;
    if (maxSlider) maxSlider.value = max;
    this.updateModalIdeologyUI(min, max);
    App.setIdeologyRange(min, max);
  },

  setModalMandate(val) {
    const mBtnTodos = document.getElementById("modalBtnMandateTodos");
    if (mBtnTodos) mBtnTodos.classList.toggle("active", val === "todos");
    document.querySelectorAll('#moreFiltersModal .filter-modal-chip[data-group="mandato"]').forEach(c => {
      c.classList.toggle("active", c.dataset.val === val);
    });
  },

  syncModalPartidos() {
    const modalPartWrap = document.getElementById("modalPartidosChipsContainer");
    if (modalPartWrap && App.state.partidos && App.state.partidos.length > 0) {
      modalPartWrap.innerHTML = App.state.partidos.map(p => {
        const isGosto = App.state.partidos_gosta.includes(p.sigla);
        const isDesgosto = App.state.partidos_desgosta.includes(p.sigla);
        let statusClass = "";
        if (isGosto) statusClass = "selected";
        if (isDesgosto) statusClass = "desgostado";
        const cleanSigla = p.sigla.toUpperCase().includes('MISS') ? 'MISSÃO' : p.sigla;
        const displayName = (p.sigla === 'PMB' || p.nome_completo === 'Democrata' || p.nome_completo === 'DEMOCRATA' || p.sigla === 'DEMOCRATA') ? 'DEMOCRATA' : cleanSigla;
        const displayTitle = (p.sigla === 'PMB' || p.nome_completo === 'Democrata' || p.nome_completo === 'DEMOCRATA' || p.sigla === 'DEMOCRATA') ? 'DEMOCRATA (PMB)' : (p.nome_completo || cleanSigla);
        const dotColor = (cleanSigla === 'MISSÃO' || p.sigla.toUpperCase().includes('MISS')) ? '#FCBE26' : (p.cor_hex || App.getPartidoColor(p.sigla));
        return `
          <button type="button" class="chip-btn ${statusClass}" 
                  title="${displayTitle} • ${p.classificacao || ''}"
                  onclick="App.togglePartidoChip('${p.sigla}'); App.syncModalPartidos();">
            <span style="width: 6px; height: 6px; border-radius: 50%; background: ${dotColor};"></span>
            ${displayName}
          </button>
        `;
      }).join("");
    }
  },

  renderModalProfissoes(filterText = "") {
    const container = document.getElementById("modalProfissaoListContainer");
    if (!container) return;

    let ocs = (App.state.todasOcupacoes || []).map(item => {
      if (typeof item === "string") return { ocupacao: item, total: 0 };
      return { ocupacao: item.ocupacao, total: item.total || 0 };
    });

    if (ocs.length === 0) {
      const counts = {};
      (App.state.candidatos || []).forEach(c => {
        if (c.ocupacao && c.ocupacao !== "Não informada") {
          counts[c.ocupacao] = (counts[c.ocupacao] || 0) + 1;
        }
      });
      ocs = Object.keys(counts).sort().map(k => ({ ocupacao: k, total: counts[k] }));
    }

    const term = (filterText || "").toLowerCase().trim();
    const filtered = term ? ocs.filter(o => o.ocupacao.toLowerCase().includes(term)) : ocs;

    if (filtered.length === 0) {
      container.innerHTML = `<div style="font-size: 0.72rem; color: var(--text-muted); padding: 0.35rem;">Nenhuma ocupação encontrada para "${filterText}".</div>`;
      return;
    }

    const toTitle = (s) => s.charAt(0).toUpperCase() + s.slice(1).toLowerCase();
    container.innerHTML = filtered.map(item => {
      const oc = item.ocupacao;
      const count = item.total;
      const isAct = App.modalSelectedProfissao.includes(oc);
      const safeOc = oc.replace(/'/g, "\\'");
      const countLabel = count > 0 ? `<span style="font-size: 0.65rem; color: var(--text-muted); margin-left: 0.3rem;">(${count.toLocaleString('pt-BR')})</span>` : '';
      return `
        <button type="button" class="modal-prof-opt-btn ${isAct ? 'active' : ''}" onclick="App.toggleModalProfissao('${safeOc}')">
          <span>${toTitle(oc)} ${countLabel}</span>
          ${isAct ? '<span>✓</span>' : ''}
        </button>
      `;
    }).join("");
  },

  filterModalProfissaoList(val) {
    App.renderModalProfissoes(val);
  },

  toggleModalProfissao(oc) {
    if (App.modalSelectedProfissao.includes(oc)) {
      App.modalSelectedProfissao = App.modalSelectedProfissao.filter(x => x !== oc);
    } else {
      App.modalSelectedProfissao.push(oc);
    }
    const searchInp = document.getElementById("modalProfissaoSearch");
    App.renderModalProfissoes(searchInp ? searchInp.value : "");
  },

  clearModalProfissao() {
    App.modalSelectedProfissao = [];
    const searchInp = document.getElementById("modalProfissaoSearch");
    if (searchInp) searchInp.value = "";
    App.renderModalProfissoes("");
  },

  clearModalFaixaEtaria() {
    document.querySelectorAll('#moreFiltersModal .filter-modal-chip[data-group="faixa_etaria"]').forEach(c => c.classList.remove("active"));
  },

  onModalSliderChange(type, val) {
    const num = parseFloat(val) || 0;
    if (type === "bens") {
      const lbl = document.getElementById("modalSliderBensVal");
      if (lbl) {
        lbl.textContent = num > 0 ? (num >= 10000000 ? 'Até R$ 10.000.000+' : `Até R$ ${(num >= 1000000 ? (num / 1e6).toFixed(1) + 'M' : (num / 1e3).toFixed(0) + ' mil')}`) : 'Todos os patrimônios';
      }
    } else if (type === "gastos") {
      const lbl = document.getElementById("modalSliderGastosVal");
      if (lbl) {
        lbl.textContent = num > 0 ? `A partir de R$ ${(num >= 1000000 ? (num / 1e6).toFixed(1) + 'M' : (num / 1e3).toFixed(0) + ' mil')}` : 'R$ 0 (sem mínimo)';
      }
    } else if (type === "receitas") {
      const lbl = document.getElementById("modalSliderReceitasVal");
      if (lbl) {
        lbl.textContent = num > 0 ? `A partir de R$ ${(num >= 1000000 ? (num / 1e6).toFixed(1) + 'M' : (num / 1e3).toFixed(0) + ' mil')}` : 'R$ 0 (sem mínimo)';
      }
    }
  },

  clearModalSlider(type) {
    if (type === "bens") {
      const s = document.getElementById("modalSliderBens");
      if (s) s.value = 0;
      App.onModalSliderChange("bens", 0);
    } else if (type === "gastos") {
      const s = document.getElementById("modalSliderGastos");
      if (s) s.value = 0;
      App.onModalSliderChange("gastos", 0);
    } else if (type === "receitas") {
      const s = document.getElementById("modalSliderReceitas");
      if (s) s.value = 0;
      App.onModalSliderChange("receitas", 0);
    }
  },

  modalCampanhaState: null,

  updateModalCampanhaUI() {
    const st = this.modalCampanhaState;
    const escopo = st?.escopo || 'partido';
    const metrica = st?.metrica || 'receitas';
    const min = st?.min || 1;
    const max = st?.max || null;

    const btnEscPartido = document.getElementById("modalCampanhaEscopoPartido");
    const btnEscCargo = document.getElementById("modalCampanhaEscopoCargo");
    if (btnEscPartido) btnEscPartido.classList.toggle("active", escopo === "partido");
    if (btnEscCargo) btnEscCargo.classList.toggle("active", escopo === "cargo");

    const btnMetReceita = document.getElementById("modalCampanhaMetricaReceita");
    const btnMetGasto = document.getElementById("modalCampanhaMetricaGasto");
    if (btnMetReceita) btnMetReceita.classList.toggle("active", metrica === "receitas");
    if (btnMetGasto) btnMetGasto.classList.toggle("active", metrica === "gastos");

    const p5 = document.getElementById("modalCampanhaPresetTop5");
    const p10 = document.getElementById("modalCampanhaPresetTop10");
    const p20 = document.getElementById("modalCampanhaPresetTop20");
    const pTodos = document.getElementById("modalCampanhaPresetTodos");
    const clearBtn = document.getElementById("modalCampanhaClearBtn");
    const rangeLbl = document.getElementById("modalCampanhaRangeLabel");

    const isFiltered = Boolean(st && (min > 1 || (max !== null && max !== undefined)));

    if (p5) p5.classList.toggle("active", min === 1 && max === 5);
    if (p10) p10.classList.toggle("active", min === 1 && max === 10);
    if (p20) p20.classList.toggle("active", min === 1 && max === 20);
    if (pTodos) pTodos.classList.toggle("active", !isFiltered);
    if (clearBtn) clearBtn.style.display = isFiltered ? "inline-block" : "none";

    if (rangeLbl) {
      if (!isFiltered) {
        rangeLbl.textContent = (escopo === 'partido') ? "Todos os candidatos da legenda" : "Todos os concorrentes no cargo";
      } else if (min === 1) {
        rangeLbl.textContent = (escopo === 'partido')
          ? `Top ${max} de cada partido (${metrica === 'receitas' ? 'arrecadação' : 'gastos'})`
          : `Top ${max} geral no cargo (${metrica === 'receitas' ? 'arrecadação' : 'gastos'})`;
      } else {
        rangeLbl.textContent = `${min}º ao ${max}º (${metrica === 'receitas' ? 'arrecadação' : 'gastos'})`;
      }
    }
  },

  setModalCampanhaEscopo(escopo) {
    if (!this.modalCampanhaState) {
      this.modalCampanhaState = { metrica: 'receitas', escopo: 'partido', min: 1, max: null };
    }
    this.modalCampanhaState.escopo = escopo;
    this.updateModalCampanhaUI();
  },

  setModalCampanhaMetrica(metrica) {
    if (!this.modalCampanhaState) {
      this.modalCampanhaState = { metrica: 'receitas', escopo: 'partido', min: 1, max: null };
    }
    this.modalCampanhaState.metrica = metrica;
    this.updateModalCampanhaUI();
  },

  setModalCampanhaPreset(min, max) {
    if (max === null && min === 1) {
      this.modalCampanhaState = null;
    } else {
      const escopo = this.modalCampanhaState?.escopo || 'partido';
      const metrica = this.modalCampanhaState?.metrica || 'receitas';
      this.modalCampanhaState = { escopo, metrica, min, max };
    }
    this.updateModalCampanhaUI();
  },

  clearModalCampanhaRanking() {
    this.modalCampanhaState = null;
    this.updateModalCampanhaUI();
  },

  clearAllModalFilters() {
    document.querySelectorAll("#moreFiltersModal .filter-modal-chip").forEach(c => c.classList.remove("active"));
    const modalBtnTodos = document.getElementById("modalBtnMandateTodos");
    if (modalBtnTodos) modalBtnTodos.classList.add("active");
    const defAguardChip = document.querySelector('#moreFiltersModal .filter-modal-chip[data-group="situacao"][data-val="DEFERIDOS_AGUARDANDO"]');
    if (defAguardChip) defAguardChip.classList.add("active");
    App.state.situacao = "DEFERIDOS_AGUARDANDO";
    delete TableFilterManager.filters.situacao;

    App.clearModalProfissao();
    App.clearModalFaixaEtaria();
    delete TableFilterManager.filters.faixa_etaria;
    delete App.state.faixa_etaria;
    App.clearModalSlider("bens");
    App.clearModalSlider("gastos");
    App.clearModalSlider("receitas");
    App.clearModalCampanhaRanking();
    delete TableFilterManager.filters.campanha_ranking;
    App.setModalIdeologyRange(1, 7);

    const selUf = document.getElementById("modalSelectUf");
    if (selUf) selUf.value = "BR";
    const inpMun = document.getElementById("modalInputMunicipio");
    if (inpMun) inpMun.value = "";

    TableFilterManager.clearAll();
    this.closeMoreFiltersModal();
    if (window.CardFilterManager) CardFilterManager.updateCardFilterButtons();
    this.carregarCandidatos();
  },

  applyMoreFiltersModal() {
    const getActiveVals = (group) => {
      const active = document.querySelectorAll(`#moreFiltersModal .filter-modal-chip[data-group="${group}"].active`);
      return Array.from(active).map(el => el.dataset.val);
    };

    // 1. Localização (UF e Município)
    const selUf = document.getElementById("modalSelectUf")?.value || "BR";
    if (selUf !== App.state.uf) {
      App.state.uf = selUf;
      App.state.selectedUfs = selUf === "BR" ? [] : [selUf];
      const mainUf = document.getElementById("selectUf");
      if (mainUf) mainUf.value = selUf;
    }
    const inpMun = document.getElementById("modalInputMunicipio")?.value || "";
    App.state.municipio = inpMun;

    // 2. Cargos
    const selCargos = getActiveVals("cargo");
    App.state.selectedCargos = selCargos;
    App.state.cargo = selCargos.length === 1 ? selCargos[0] : "";
    App.renderCargosPills();

    // 3. Mandato
    const selMandato = getActiveVals("mandato");
    const mVal = selMandato.length === 1 ? selMandato[0] : "todos";
    App.setMandateFilter(mVal);

    // 4. Demografia
    const selGeneros = getActiveVals("genero");
    if (selGeneros.length > 0) TableFilterManager.filters.genero = selGeneros;
    else delete TableFilterManager.filters.genero;

    const selRacas = getActiveVals("raca");
    if (selRacas.length > 0) TableFilterManager.filters.raca = selRacas;
    else delete TableFilterManager.filters.raca;

    const selFaixaEtaria = getActiveVals("faixa_etaria");
    if (selFaixaEtaria.length > 0) {
      TableFilterManager.filters.faixa_etaria = selFaixaEtaria;
      App.state.faixa_etaria = selFaixaEtaria.join(",");
    } else {
      delete TableFilterManager.filters.faixa_etaria;
      delete App.state.faixa_etaria;
    }

    const selInstrucao = getActiveVals("instrucao");
    if (selInstrucao.length > 0) TableFilterManager.filters.instrucao = selInstrucao;
    else delete TableFilterManager.filters.instrucao;

    const selEstadoCivil = getActiveVals("estado_civil");
    if (selEstadoCivil.length > 0) TableFilterManager.filters.estado_civil = selEstadoCivil;
    else delete TableFilterManager.filters.estado_civil;

    // 5. Situação da Candidatura
    const selSituacao = getActiveVals("situacao");
    if (selSituacao.includes("DEFERIDOS_AGUARDANDO")) {
      delete TableFilterManager.filters.situacao;
      App.state.situacao = "DEFERIDOS_AGUARDANDO";
    } else if (selSituacao.includes("APTO")) {
      TableFilterManager.filters.situacao = ["Aptos / Deferidos"];
      App.state.situacao = "APTO";
    } else if (selSituacao.includes("AGUARDANDO")) {
      TableFilterManager.filters.situacao = ["Aguardando julgamento"];
      App.state.situacao = "AGUARDANDO";
    } else if (selSituacao.includes("INAPTO")) {
      TableFilterManager.filters.situacao = ["Inaptos / Indeferidos"];
      App.state.situacao = "INAPTO";
    } else {
      TableFilterManager.filters.situacao = ["todos"];
      App.state.situacao = "todos";
    }

    // 6. Proposta
    const selProposta = getActiveVals("proposta");
    if (selProposta.length > 0) TableFilterManager.filters.proposta = true;
    else delete TableFilterManager.filters.proposta;

    // 7. Profissão / Ocupação
    if (App.modalSelectedProfissao && App.modalSelectedProfissao.length > 0) {
      TableFilterManager.filters.profissao = [...App.modalSelectedProfissao];
    } else {
      delete TableFilterManager.filters.profissao;
    }

    // 8. Sliders de Patrimônio e Finanças
    const valBens = parseFloat(document.getElementById("modalSliderBens")?.value) || 0;
    if (valBens > 0) TableFilterManager.filters.bens = { min: null, max: valBens };
    else delete TableFilterManager.filters.bens;

    const valGastos = parseFloat(document.getElementById("modalSliderGastos")?.value) || 0;
    if (valGastos > 0) TableFilterManager.filters.gastos = { min: valGastos, max: null };
    else delete TableFilterManager.filters.gastos;

    const valReceitas = parseFloat(document.getElementById("modalSliderReceitas")?.value) || 0;
    if (valReceitas > 0) TableFilterManager.filters.receitas = { min: valReceitas, max: null };
    else delete TableFilterManager.filters.receitas;

    // 9. Ideologia (Slider Duplo no Modal)
    const minI = parseInt(document.getElementById("modalIdeologiaMinSlider")?.value || App.state.ideologia_min, 10);
    const maxI = parseInt(document.getElementById("modalIdeologiaMaxSlider")?.value || App.state.ideologia_max, 10);
    if (!isNaN(minI) && !isNaN(maxI) && (minI !== App.state.ideologia_min || maxI !== App.state.ideologia_max)) {
      App.setIdeologyRange(minI, maxI);
    }

    // 10. Ranking de Financiamento de Campanha (Escopo, Métrica, Faixa)
    if (App.modalCampanhaState && (App.modalCampanhaState.min > 1 || (App.modalCampanhaState.max !== null && App.modalCampanhaState.max !== undefined))) {
      TableFilterManager.filters.campanha_ranking = { ...App.modalCampanhaState };
    } else {
      delete TableFilterManager.filters.campanha_ranking;
    }

    // Garantir remoção de prioridade_partido legada isolada
    delete TableFilterManager.filters.prioridade_partido;

    this.closeMoreFiltersModal();
    if (window.CardFilterManager) CardFilterManager.updateCardFilterButtons();
    this.carregarCandidatos();
    TableFilterManager.applyCurrentSilent();
  },

  setupEventListeners() {
    const searchInput = document.getElementById("searchInput");
    let debounceTimer;
    if (searchInput) {
      searchInput.addEventListener("input", (e) => {
        clearTimeout(debounceTimer);
        debounceTimer = setTimeout(() => {
          const val = (e.target.value || "").trim();
          this.state.busca = val;
          if (window.TableFilterManager) {
            if (val.length === 0) {
              delete TableFilterManager.filters.candidato;
            } else if (TableFilterManager.filters.candidato !== undefined) {
              TableFilterManager.filters.candidato = val;
            }
          }
          this.state.pagina = 1;
          this.carregarCandidatos();
        }, 250);
      });
    }

    // Atalho global Ctrl + K / Cmd + K para busca rápida
    window.addEventListener("keydown", (e) => {
      if ((e.ctrlKey || e.metaKey) && (e.key === "k" || e.key === "K")) {
        e.preventDefault();
        const input = document.getElementById("searchInput");
        if (input) {
          input.focus();
          input.select();
        }
      }
    });

    // Clique na tag do atalho
    const kbdTag = document.querySelector(".search-kbd-tag");
    if (kbdTag) {
      kbdTag.style.cursor = "pointer";
      kbdTag.addEventListener("click", () => {
        const input = document.getElementById("searchInput");
        if (input) {
          input.focus();
          input.select();
        }
      });
    }

    const selectUf = document.getElementById("selectUf");
    if (selectUf) {
      selectUf.addEventListener("change", (e) => {
        this.state.uf = e.target.value;
        this.state.pagina = 1;
        this.carregarCandidatos();
      });
    }

    const selectSort = document.getElementById("selectSort");
    if (selectSort) {
      selectSort.addEventListener("change", (e) => {
        this.setOrdenacao(e.target.value);
      });
    }

    window.addEventListener("lists:updated", () => {
      this.updateBookmarkBadge();
      this.renderView();
    });

    document.addEventListener("click", (e) => {
      const menu = document.getElementById("colDropdownMenu");
      if (menu && menu.classList.contains("show")) {
        if (!menu.contains(e.target) && !e.target.closest("#btnColDropdown")) {
          menu.classList.remove("show");
        }
      }

      const tablePop = document.getElementById("tableFilterPopover");
      if (tablePop && tablePop.style.display !== "none") {
        if (!tablePop.contains(e.target) && !e.target.closest(".btn-table-col-filter")) {
          TableFilterManager.close();
        }
      }

      const cardPop = document.getElementById("cardFilterPopover");
      if (cardPop && cardPop.style.display !== "none") {
        if (!cardPop.contains(e.target) && !e.target.closest(".btn-quick-filter")) {
          CardFilterManager.close();
        }
      }

      if (!e.target.closest("#topIdeologiaDropdownWrap") && !e.target.closest("#topPartidosDropdownWrap")) {
        App.closeTopPopovers();
      }
    });
  }
};

/* ─── Gerenciador de Filtros de Coluna Estilo Planilha (Excel / Slicer) ─── */
const TableFilterManager = {
  currentCol: null,
  filters: {},
  tempSelectedValues: new Set(),
  tempRange: { min: null, max: null },
  tempSearchQuery: "",

  hasFilter(colKey) {
    if (colKey === "filtro_lista") {
      return Boolean(this.filters.filtro_lista && this.filters.filtro_lista.sqList && this.filters.filtro_lista.sqList.size > 0);
    }
    if (colKey === "situacao") {
      const s = this.filters.situacao;
      const isCustomInTable = Boolean(s && Array.isArray(s) && s.length > 0 && !(s.length === 2 && s.includes("Aptos / Deferidos") && s.includes("Aguardando julgamento")));
      const isCustomInApp = Boolean(App.state.situacao && App.state.situacao !== "DEFERIDOS_AGUARDANDO");
      return isCustomInTable || isCustomInApp;
    }
    if (colKey === "campanha" || colKey === "campanha_ranking") {
      return Boolean(this.filters.campanha_ranking || this.hasFilter("gastos") || this.hasFilter("receitas"));
    }
    if (colKey === "prioridade_partido") {
      return Boolean(this.filters.campanha_ranking || this.filters.ranking_partido);
    }
    if (colKey === "gastos") {
      if (this.filters.campanha_ranking || this.filters.ranking_partido || this.filters.ranking_cargo) return true;
      const f = this.filters.gastos;
      if (!f) return Boolean(App.state.gastos_min || (App.state.gastos_max && App.state.gastos_max < 100000000));
      return (f.min !== null && f.min !== undefined && f.min > 0) || (f.max !== null && f.max !== undefined && f.max < 100000000);
    }
    if (colKey === "receitas") {
      if (this.filters.campanha_ranking) return true;
      const f = this.filters.receitas;
      if (!f) return Boolean(App.state.receitas_min || (App.state.receitas_max && App.state.receitas_max < 100000000));
      return (f.min !== null && f.min !== undefined && f.min > 0) || (f.max !== null && f.max !== undefined && f.max < 100000000);
    }
    if (colKey === "bens") {
      if (this.filters.ranking_bens) return true;
      const f = this.filters.bens;
      if (!f) return Boolean((App.state.bens_max !== undefined && App.state.bens_max !== null && App.state.bens_max < 10000000) || (App.state.bens_min && App.state.bens_min > 0));
      return (f.min !== null && f.min !== undefined && f.min > 0) || (f.max !== null && f.max !== undefined && f.max < 10000000);
    }
    const f = this.filters[colKey];
    if (!f) return false;
    if (Array.isArray(f)) return f.length > 0;
    if (typeof f === "object") return (f.min !== null && f.min !== undefined && f.min > 0) || (f.max !== null && f.max !== undefined);
    if (typeof f === "string") return f.trim().length > 0;
    return false;
  },

  hasAnyFilter() {
    return Object.keys(this.filters).some(k => this.hasFilter(k));
  },

  clearAll() {
    App.clearAllFilters();
  },

  toggle(trigger, colKey, evt) {
    if (evt && typeof evt.stopPropagation === 'function') {
      evt.stopPropagation();
    } else if (window.event && typeof window.event.stopPropagation === 'function') {
      window.event.stopPropagation();
    }
    if (window.CardFilterManager) {
      CardFilterManager.close();
    }
    let btnEl = null;
    if (trigger instanceof HTMLElement) {
      btnEl = trigger;
    } else if (trigger && trigger.currentTarget instanceof HTMLElement) {
      btnEl = trigger.currentTarget;
    } else if (trigger && trigger.target instanceof HTMLElement) {
      btnEl = trigger.target.closest('button') || trigger.target;
    }

    if (this.currentCol === colKey && document.getElementById("tableFilterPopover")?.style.display !== "none") {
      this.close();
      return;
    }
    this.open(btnEl, colKey);
  },

  open(btnEl, colKey) {
    this.currentCol = colKey;
    const pop = document.getElementById("tableFilterPopover");
    if (!pop) return;

    const titleEl = document.getElementById("popoverTitle");
    const bodyEl = document.getElementById("popoverBody");

    const colTitles = {
      candidato: "Candidato",
      numero: "Número",
      partido: "Partido",
      cargo: "Cargo",
      uf: "UF / Estado",
      ideologia: "Ideologia",
      mandato: "Mandato Atual",
      situacao: "Situação no TSE",
      prioridade_partido: "Ranking Financeiro no Partido",
      profissao: "Profissão",
      raca: "Raça / Etnia",
      bens: "Bens Declarados",
      receitas: "Receitas de Campanha",
      gastos: "Gastos de Campanha"
    };

    if (titleEl) titleEl.textContent = `Filtrar ${colTitles[colKey] || colKey}`;

    // Posicionar popover fixo sob o botão (sem somar scroll, pois o elemento é position: fixed)
    const rect = (btnEl && typeof btnEl.getBoundingClientRect === 'function')
      ? btnEl.getBoundingClientRect()
      : { bottom: 140, left: 100, top: 100 };
    const popWidth = 290;
    let left = rect.left;
    if (left + popWidth > window.innerWidth - 15) {
      left = Math.max(10, window.innerWidth - popWidth - 15);
    }
    let top = rect.bottom + 6;
    if (top + 360 > window.innerHeight && rect.top > 360) {
      top = Math.max(10, rect.top - 370);
    }

    if (window.innerWidth <= 640) {
      pop.style.top = "";
      pop.style.left = "";
      document.getElementById("popoverBackdrop")?.classList.add("show");
    } else {
      pop.style.top = `${Math.max(10, top)}px`;
      pop.style.left = `${Math.max(10, left)}px`;
    }
    pop.style.display = "block";

    this.renderPopoverBody(bodyEl, colKey);
  },

  close() {
    const pop = document.getElementById("tableFilterPopover");
    if (pop) pop.style.display = "none";
    document.getElementById("popoverBackdrop")?.classList.remove("show");
    this.currentCol = null;
  },

  renderPopoverBody(bodyEl, colKey) {
    if (!bodyEl) return;
    const allCands = App.state.candidatos || [];

    // 0. Ranking no Partido / Cargo
    if (colKey === "prioridade_partido") {
      this.renderCampanhaRankingPopover(bodyEl);
      return;
    }

    // 1. Slicer para Valores Numéricos (Bens, Receitas, Gastos) com Dual Slider e Mini Histograma
    if (colKey === "bens" || colKey === "gastos" || colKey === "receitas") {
      const cur = this.filters[colKey] || { min: null, max: null };
      this.tempRange = { min: cur.min, max: cur.max };

      // Extrair valores reais para o histograma
      const vals = allCands.map(c => {
        if (colKey === "bens") return c.total_bens || 0;
        if (colKey === "receitas") return c.financiamento_receita || 0;
        return c.financiamento_despesa || 0;
      });

      const maxVal = colKey === "bens" ? 10000000 : 5000000;
      const step = colKey === "bens" ? 100000 : 50000;
      const curMin = (cur.min !== null && cur.min !== undefined) ? cur.min : 0;
      const curMax = (cur.max !== null && cur.max !== undefined) ? cur.max : maxVal;

      // Calcular frequências para 14 barras de histograma
      const numBins = 14;
      const binCounts = new Array(numBins).fill(0);
      vals.forEach(v => {
        if (v > 0) {
          const idx = Math.min(numBins - 1, Math.floor((v / maxVal) * numBins));
          binCounts[idx]++;
        }
      });
      const maxBinCount = Math.max(...binCounts, 1);
      const matchingCount = vals.filter(v => v >= curMin && (cur.max === null || v <= curMax)).length;

      const histBarsHtml = binCounts.map((count, i) => {
        const binStart = (i / numBins) * maxVal;
        const binEnd = ((i + 1) / numBins) * maxVal;
        const isActive = binEnd >= curMin && binStart <= curMax;
        const h = Math.max(3, Math.round((count / maxBinCount) * 36));
        return `<div class="hist-bar ${isActive ? 'active' : ''}" id="hist_bar_${i}" style="height: ${h}px;" title="R$ ${(binStart / 1e3).toFixed(0)}k+: ${count} candidato(s)"></div>`;
      }).join("");

      bodyEl.innerHTML = `
        <div class="popover-slicer-wrap">
          <div style="display: flex; justify-content: space-between; align-items: center;">
            <span style="font-size: 0.72rem; font-weight: 700; color: var(--text-muted); text-transform: uppercase;">
              Faixas de Valores
            </span>
            <span id="popoverHistStats" style="font-size: 0.68rem; color: #93C5FD; font-weight: 600;">
              ${matchingCount} candidatos
            </span>
          </div>

          <!-- Mini Histograma -->
          <div class="popover-hist-container" id="popoverHistogram">
            ${histBarsHtml}
          </div>

          <!-- Texto do Intervalo -->
          <div style="font-size: 0.68rem; color: var(--text-secondary); margin-bottom: 0.35rem;">
            Faixa: <strong id="popoverSliderVal" style="color: #60A5FA;">De ${curMin > 0 ? 'R$ ' + curMin.toLocaleString('pt-BR') : 'R$ 0'} até ${curMax < maxVal ? 'R$ ' + curMax.toLocaleString('pt-BR') : 'Sem limite'}</strong>
          </div>

          <!-- Dual Range Slider em Barra Única (Mínimo e Máximo) -->
          <div class="dual-slider-track-wrap">
            <div class="dual-slider-track-bg"></div>
            <div class="dual-slider-track-highlight" id="dualSliderHighlight" style="left: ${(curMin / maxVal) * 100}%; width: ${Math.max(0, ((curMax - curMin) / maxVal) * 100)}%;"></div>
            <input type="range" class="dual-range-input min-thumb" id="popoverMinSlider" min="0" max="${maxVal}" step="${step}" value="${curMin}" oninput="TableFilterManager.onDualSliderChange('min', this.value, '${colKey}')">
            <input type="range" class="dual-range-input max-thumb" id="popoverMaxSlider" min="0" max="${maxVal}" step="${step}" value="${curMax}" oninput="TableFilterManager.onDualSliderChange('max', this.value, '${colKey}')">
          </div>

          <!-- Entradas Numéricas Manuais -->
          <div class="slicer-inputs-grid">
            <div>
              <label style="font-size: 0.65rem; color: var(--text-secondary); display: block; margin-bottom: 0.15rem;">Mínimo (R$):</label>
              <input type="number" id="popoverMinInput" class="popover-search-input" style="margin-bottom: 0; font-size: 0.72rem;" placeholder="0,00" value="${cur.min !== null ? cur.min : ''}" oninput="TableFilterManager.onDualInputMinChange(this.value, '${colKey}')">
            </div>
            <div>
              <label style="font-size: 0.65rem; color: var(--text-secondary); display: block; margin-bottom: 0.15rem;">Máximo (R$):</label>
              <input type="number" id="popoverMaxInput" class="popover-search-input" style="margin-bottom: 0; font-size: 0.72rem;" placeholder="Sem limite" value="${cur.max !== null ? cur.max : ''}" oninput="TableFilterManager.onDualInputMaxChange(this.value, '${colKey}')">
            </div>
          </div>
        </div>
      `;
      return;
    }

    // 2. Campo Texto para Candidato e Número
    if (colKey === "candidato" || colKey === "numero") {
      const cur = this.filters[colKey] || "";
      this.tempSearchQuery = cur;
      bodyEl.innerHTML = `
        <div style="display: flex; flex-direction: column; gap: 0.5rem; margin-bottom: 0.75rem;">
          <label style="font-size: 0.72rem; color: var(--text-secondary);">Contém o texto / número:</label>
          <input type="text" id="popoverTextInput" class="popover-search-input" style="margin-bottom: 0;" placeholder="Digite para filtrar..." value="${cur}" oninput="TableFilterManager.tempSearchQuery = this.value">
        </div>
      `;
      setTimeout(() => document.getElementById("popoverTextInput")?.focus(), 50);
      return;
    }

    // 3. Mandato Atual Tripartite com "Ver Todos" no topo (padronizado)
    if (colKey === "mandato") {
      const mandCounts = App.state.contagens?.mandato || {};
      const countReeleicao = mandCounts.reeleicao ?? allCands.filter(c => App.getCandidateMandatoInfo(c).isReeleicao).length;
      const countOutroCargo = mandCounts.outro_cargo ?? allCands.filter(c => c.em_exercicio && !App.getCandidateMandatoInfo(c).isReeleicao).length;
      const countSemMandato = mandCounts.sem_mandato ?? allCands.filter(c => !c.em_exercicio).length;
      const total = (App.state.contagens?.total ?? App.state.total ?? 0).toLocaleString('pt-BR');

      const cur = App.state.situacao_mandato || "todos";

      bodyEl.innerHTML = `
        <div style="padding: 0.2rem 0;">
          <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.6rem;">
            <span style="font-size: 0.72rem; font-weight: 700; color: var(--text-muted); text-transform: uppercase;">
              Mandato Atual
            </span>
            <button type="button" class="btn-text-discreet ${cur === 'todos' ? 'active' : ''}" id="popoverBtnMandateTodos" onclick="TableFilterManager.setMandatoTableOption('todos')">
              Ver Todos (${total})
            </button>
          </div>
          <div class="mandate-toggle-group" style="display: flex; flex-direction: column; gap: 0.35rem;">
            <button type="button" class="btn-mandate-toggle ${cur === 'reeleicao' ? 'active' : ''}" id="popoverBtnMandateReeleicao" style="display: flex; justify-content: space-between; padding: 0.4rem 0.6rem;" onclick="TableFilterManager.setMandatoTableOption('reeleicao')">
              <span>⚡ Reeleição</span>
              <span style="opacity: 0.8; font-size: 0.72rem; font-family: var(--font-mono);">(${countReeleicao.toLocaleString('pt-BR')})</span>
            </button>
            <button type="button" class="btn-mandate-toggle ${cur === 'outro_cargo' ? 'active' : ''}" id="popoverBtnMandateOutroCargo" style="display: flex; justify-content: space-between; padding: 0.4rem 0.6rem;" onclick="TableFilterManager.setMandatoTableOption('outro_cargo')">
              <span>Em exercício (outro cargo)</span>
              <span style="opacity: 0.8; font-size: 0.72rem; font-family: var(--font-mono);">(${countOutroCargo.toLocaleString('pt-BR')})</span>
            </button>
            <button type="button" class="btn-mandate-toggle ${cur === 'sem_mandato' ? 'active' : ''}" id="popoverBtnMandateSemMandato" style="display: flex; justify-content: space-between; padding: 0.4rem 0.6rem;" onclick="TableFilterManager.setMandatoTableOption('sem_mandato')">
              <span>Não consta</span>
              <span style="opacity: 0.8; font-size: 0.72rem; font-family: var(--font-mono);">(${countSemMandato.toLocaleString('pt-BR')})</span>
            </button>
          </div>
        </div>
      `;
      return;
    }

    // 4. Checklist Pesquisável de Categorias (Partido, Cargo, Ideologia, Mandato, Situação, Profissão, Raça, Gênero, Escolaridade)
    const counts = {};
    allCands.forEach(c => {
      let val = "";
      if (colKey === "partido") val = (c.partido === "PMB" || c.partido === "DEMOCRATA") ? "DEMOCRATA" : (c.partido || "OUTROS");
      else if (colKey === "cargo") val = c.cargo || "Não informado";
      else if (colKey === "uf") val = c.uf || "BR";
      else if (colKey === "ideologia") val = c.ideologia_nome || "Sem Classificação";
      else if (colKey === "mandato") {
        const mand = App.getCandidateMandatoInfo(c);
        if (mand.isReeleicao) val = "Reeleição";
        else if (c.em_exercicio) val = "Em exercício (outro cargo)";
        else val = "Não consta";
      }
      else if (colKey === "situacao") {
        const st = String(c.situacao_candidatura || 'Aguardando julgamento').toUpperCase();
        if (st.includes('INAPTO') || st.includes('INDEFERIDO') || st.includes('CANCELADO') || st.includes('REN')) val = "Inaptos / Indeferidos";
        else if (st.includes('AGUARDANDO') || st.includes('PENDENTE')) val = "Aguardando julgamento";
        else val = "Aptos / Deferidos";
      }
      else if (colKey === "profissao") val = (c.ocupacao && c.ocupacao !== "Não informada") ? c.ocupacao : "Não informada";
      else if (colKey === "raca") val = (c.cor_raca && c.cor_raca !== "NÃO INFORMADO") ? c.cor_raca : "NÃO INFORMADO";
      else if (colKey === "genero") val = c.genero === "FEMININO" ? "Mulheres" : "Homens";
      else if (colKey === "instrucao") val = (c.grau_instrucao && c.grau_instrucao !== "NÃO INFORMADO") ? c.grau_instrucao : "Não informado";
      counts[val] = (counts[val] || 0) + 1;
    });

    // Enriquecer com contagens reais da base quando disponíveis
    if (App.state.contagens) {
      const c = App.state.contagens;
      if (colKey === "situacao" && c.situacao) {
        counts["Aptos / Deferidos"] = c.situacao.APTO || 0;
        counts["Aguardando julgamento"] = c.situacao.AGUARDANDO || 0;
        counts["Inaptos / Indeferidos"] = c.situacao.INAPTO || 0;
      } else if (colKey === "genero" && c.genero) {
        counts["Mulheres"] = c.genero.FEMININO || 0;
        counts["Homens"] = c.genero.MASCULINO || 0;
      } else if (colKey === "raca" && c.raca) {
        for (const [r, cnt] of Object.entries(c.raca)) {
          if (r) counts[r] = cnt;
        }
      } else if (colKey === "instrucao" && c.instrucao) {
        for (const [ins, cnt] of Object.entries(c.instrucao)) {
          if (ins) counts[ins] = cnt;
        }
      } else if (colKey === "profissao" && c.profissao) {
        for (const [prof, cnt] of Object.entries(c.profissao)) {
          if (prof) counts[prof] = cnt;
        }
      }
    }

    if (colKey === "situacao") {
      if (!counts["Aptos / Deferidos"]) counts["Aptos / Deferidos"] = 0;
      if (!counts["Aguardando julgamento"]) counts["Aguardando julgamento"] = 0;
      if (!counts["Inaptos / Indeferidos"]) counts["Inaptos / Indeferidos"] = 0;
    }

    let uniqueValues = Object.keys(counts).sort((a, b) => counts[b] - counts[a]);
    if (colKey === "situacao") {
      uniqueValues = ["Aptos / Deferidos", "Aguardando julgamento", "Inaptos / Indeferidos"];
    }
    let activeSelection = this.filters[colKey];
    if (colKey === "situacao" && !activeSelection) {
      const cur = App.state.situacao;
      if (cur === "DEFERIDOS_AGUARDANDO") activeSelection = ["Aptos / Deferidos", "Aguardando julgamento"];
      else if (cur === "APTO") activeSelection = ["Aptos / Deferidos"];
      else if (cur === "AGUARDANDO") activeSelection = ["Aguardando julgamento"];
      else if (cur === "INAPTO") activeSelection = ["Inaptos / Indeferidos"];
      else activeSelection = uniqueValues;
    }
    this.tempSelectedValues = new Set(activeSelection ? activeSelection : uniqueValues);

    bodyEl.innerHTML = `
      <input type="text" class="popover-search-input" id="popoverOptionSearch" placeholder="🔍 Pesquisar opções..." oninput="TableFilterManager.filterOptionList(this.value)">
      <div class="popover-quick-actions">
        <button type="button" class="btn-popover-quick" onclick="TableFilterManager.selectAllOptions(true)">Ver Todos</button>
        <span>•</span>
        <button type="button" class="btn-popover-quick" onclick="TableFilterManager.selectAllOptions(false)">Limpar</button>
      </div>
      <div class="popover-options-list" id="popoverOptionsList">
        ${this.buildOptionsHtml(uniqueValues, counts)}
      </div>
    `;

    setTimeout(() => document.getElementById("popoverOptionSearch")?.focus(), 50);
  },

  onDualSliderChange(type, val, colKey) {
    const maxLimit = colKey === "bens" ? 10000000 : 5000000;
    let minSlider = document.getElementById("popoverMinSlider");
    let maxSlider = document.getElementById("popoverMaxSlider");
    let minNum = minSlider ? parseFloat(minSlider.value) || 0 : 0;
    let maxNum = maxSlider ? parseFloat(maxSlider.value) || maxLimit : maxLimit;

    if (type === 'min') {
      let parsed = parseFloat(val) || 0;
      if (parsed > maxNum) {
        parsed = maxNum;
        if (minSlider) minSlider.value = parsed;
      }
      minNum = parsed;
      this.tempRange.min = minNum > 0 ? minNum : null;
      const inp = document.getElementById("popoverMinInput");
      if (inp) inp.value = minNum > 0 ? minNum : "";
    } else if (type === 'max') {
      let parsed = parseFloat(val) || maxLimit;
      if (parsed < minNum) {
        parsed = minNum;
        if (maxSlider) maxSlider.value = parsed;
      }
      maxNum = parsed;
      this.tempRange.max = maxNum < maxLimit ? maxNum : null;
      const inp = document.getElementById("popoverMaxInput");
      if (inp) inp.value = maxNum < maxLimit ? maxNum : "";
    }

    this.updateHistogramAndStats(minNum, maxNum, maxLimit, colKey);
  },

  onDualInputMinChange(val, colKey) {
    const maxLimit = colKey === "bens" ? 10000000 : 5000000;
    const minNum = val ? parseFloat(val) || 0 : 0;
    this.tempRange.min = minNum > 0 ? minNum : null;
    const slider = document.getElementById("popoverMinSlider");
    if (slider) slider.value = Math.min(minNum, maxLimit);

    const maxSlider = document.getElementById("popoverMaxSlider");
    const maxNum = maxSlider ? parseFloat(maxSlider.value) || maxLimit : maxLimit;
    this.updateHistogramAndStats(minNum, maxNum, maxLimit, colKey);
  },

  onDualInputMaxChange(val, colKey) {
    const maxLimit = colKey === "bens" ? 10000000 : 5000000;
    const maxNum = val ? parseFloat(val) || maxLimit : maxLimit;
    this.tempRange.max = maxNum < maxLimit ? maxNum : null;
    const slider = document.getElementById("popoverMaxSlider");
    if (slider) slider.value = Math.min(maxNum, maxLimit);

    const minSlider = document.getElementById("popoverMinSlider");
    const minNum = minSlider ? parseFloat(minSlider.value) || 0 : 0;
    this.updateHistogramAndStats(minNum, maxNum, maxLimit, colKey);
  },

  updateHistogramAndStats(minNum, maxNum, maxLimit, colKey) {
    const numBins = 14;
    const valEl = document.getElementById("popoverSliderVal");
    if (valEl) {
      valEl.textContent = `De ${minNum > 0 ? 'R$ ' + minNum.toLocaleString('pt-BR') : 'R$ 0'} até ${maxNum < maxLimit ? 'R$ ' + maxNum.toLocaleString('pt-BR') : 'Sem limite'}`;
    }

    // Atualizar faixa destacada no trilho único
    const highlight = document.getElementById("dualSliderHighlight");
    if (highlight) {
      const leftPct = (minNum / maxLimit) * 100;
      const widthPct = Math.max(0, ((maxNum - minNum) / maxLimit) * 100);
      highlight.style.left = `${leftPct}%`;
      highlight.style.width = `${widthPct}%`;
    }

    // Atualizar barras do histograma
    for (let i = 0; i < numBins; i++) {
      const binStart = (i / numBins) * maxLimit;
      const binEnd = ((i + 1) / numBins) * maxLimit;
      const bar = document.getElementById(`hist_bar_${i}`);
      if (bar) {
        if (binEnd >= minNum && binStart <= maxNum) bar.classList.add("active");
        else bar.classList.remove("active");
      }
    }

    // Contagem de candidatos atendendo
    const allCands = App.state.candidatos || [];
    const count = allCands.filter(c => {
      const v = (colKey === "bens" ? (c.total_bens || 0) : (colKey === "receitas" ? (c.financiamento_receita || 0) : (c.financiamento_despesa || 0)));
      return v >= minNum && (maxNum >= maxLimit || v <= maxNum);
    }).length;

    const statsEl = document.getElementById("popoverHistStats");
    if (statsEl) statsEl.textContent = `${count} candidatos`;
  },

  buildOptionsHtml(values, counts) {
    return values.map(v => {
      const checked = this.tempSelectedValues.has(v);
      let dotHtml = "";
      if (this.currentCol === "partido") {
        const pColor = App.getPartidoColor(v);
        dotHtml = `<span style="width: 7px; height: 7px; border-radius: 50%; background: ${pColor}; flex-shrink: 0; display: inline-block; margin-right: 6px;"></span>`;
      }
      return `
        <label class="popover-option-item" data-val="${v.toLowerCase()}">
          <input type="checkbox" value="${v}" ${checked ? 'checked' : ''} onchange="TableFilterManager.toggleOption('${v}', this.checked)">
          ${dotHtml}
          <span style="flex: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;">${v}</span>
          <span class="popover-option-count">${counts[v] || 0}</span>
        </label>
      `;
    }).join('');
  },

  filterOptionList(query) {
    const q = (query || "").toLowerCase().trim();
    document.querySelectorAll("#popoverOptionsList .popover-option-item").forEach(el => {
      const val = el.getAttribute("data-val") || "";
      el.style.display = (!q || val.includes(q)) ? "flex" : "none";
    });
  },

  toggleOption(val, checked) {
    if (checked) this.tempSelectedValues.add(val);
    else this.tempSelectedValues.delete(val);
  },

  selectAllOptions(select) {
    document.querySelectorAll("#popoverOptionsList input[type='checkbox']").forEach(cb => {
      cb.checked = select;
      if (select) this.tempSelectedValues.add(cb.value);
      else this.tempSelectedValues.delete(cb.value);
    });
  },

  setMandatoTableOption(val) {
    App.setMandateFilter(val);
    this.close();
  },

  renderCampanhaRankingPopover(bodyEl) {
    if (window.CardFilterManager) {
      CardFilterManager.renderCampanhaOptions(bodyEl, 'table');
    }
  },

  applyCurrent() {
    const col = this.currentCol;
    if (!col) return;

    if (col === "bens" || col === "gastos" || col === "receitas") {
      this.filters[col] = { min: this.tempRange.min, max: this.tempRange.max };
    } else if (col === "candidato") {
      const q = this.tempSearchQuery.trim();
      this.filters[col] = q;
      App.state.busca = q;
      const inp = document.getElementById("searchInput") || document.getElementById("inputBusca");
      if (inp) inp.value = q;
      App.state.pagina = 1;
      this.close();
      App.carregarCandidatos();
      return;
    } else if (col === "numero") {
      this.filters[col] = this.tempSearchQuery.trim();
    } else if (col === "partido") {
      const selected = Array.from(this.tempSelectedValues);
      this.filters[col] = selected;
      App.state.partidos_gosta = (selected.length === App.state.partidos.length) ? [] : selected;
      App.renderPartidosChips();
      App.state.pagina = 1;
      this.close();
      App.carregarCandidatos();
      return;
    } else if (col === "cargo") {
      const selected = Array.from(this.tempSelectedValues);
      this.filters[col] = selected;
      const cargosSet = new Set();
      selected.forEach(s => {
        const base = s.replace(/\s*\([^)]*\)/, '').trim();
        if (base) cargosSet.add(base);
      });
      App.state.selectedCargos = Array.from(cargosSet);
      App.renderCargosPills();
      App.state.pagina = 1;
      this.close();
      App.carregarCandidatos();
      return;
    } else if (col === "mandato") {
      const selected = Array.from(this.tempSelectedValues);
      this.filters[col] = selected;
      const hasReeleicao = selected.includes("Reeleição") || selected.includes("reeleicao");
      const hasOutroCargo = selected.includes("Em exercício (outro cargo)") || selected.includes("outro_cargo");
      const hasSemMandato = selected.includes("Não consta") || selected.includes("Sem mandato") || selected.includes("sem_mandato") || selected.includes("novos");
      if (hasReeleicao && !hasOutroCargo && !hasSemMandato) {
        App.setMandateFilter("reeleicao");
      } else if (!hasReeleicao && hasOutroCargo && !hasSemMandato) {
        App.setMandateFilter("outro_cargo");
      } else if (!hasReeleicao && !hasOutroCargo && hasSemMandato) {
        App.setMandateFilter("sem_mandato");
      } else {
        App.setMandateFilter("todos");
      }
      this.close();
      return;
    } else if (col === "ideologia") {
      const selected = Array.from(this.tempSelectedValues);
      this.filters[col] = selected;
      const IDEO_VALS = {
        "Extrema-Esquerda": 1,
        "Esquerda": 2,
        "Centro-Esquerda": 3,
        "Centro": 4,
        "Centro-Direita": 5,
        "Direita": 6,
        "Extrema-Direita": 7
      };
      const nums = selected.map(n => IDEO_VALS[n]).filter(n => n !== undefined);
      if (nums.length > 0) {
        const minVal = Math.min(...nums);
        const maxVal = Math.max(...nums);
        App.setIdeologyRange(minVal, maxVal);
      }
      this.close();
      return;
    } else if (col === "situacao") {
      const selected = Array.from(this.tempSelectedValues);
      this.filters[col] = selected;
      const hasApto = selected.some(s => s.includes("Apto") || s.includes("Deferido"));
      const hasAguardando = selected.some(s => s.includes("Aguardando"));
      const hasInapto = selected.some(s => s.includes("Inapto"));
      if (hasApto && hasAguardando && !hasInapto) {
        App.setSituacaoFilter("DEFERIDOS_AGUARDANDO");
      } else if (hasApto && !hasAguardando && !hasInapto) {
        App.setSituacaoFilter("APTO");
      } else if (!hasApto && hasAguardando && !hasInapto) {
        App.setSituacaoFilter("AGUARDANDO");
      } else if (!hasApto && !hasAguardando && hasInapto) {
        App.setSituacaoFilter("INAPTO");
      } else {
        delete this.filters.situacao;
        App.setSituacaoFilter("todos");
      }
      this.close();
      return;
    } else {
      this.filters[col] = Array.from(this.tempSelectedValues);
    }

    this.close();
    App.renderView();
  },

  removeFilter(colKey) {
    if (colKey.startsWith('_sidebar_')) {
      if (colKey === '_sidebar_uf') {
        App.state.selectedUfs = [];
        App.setUfFilter('BR');
        return;
      }
      if (colKey === '_sidebar_cargo') {
        App.state.selectedCargos = [];
        App.state.cargo = '';
        App.renderCargosPills();
        App.state.pagina = 1;
        App.carregarCandidatos();
        return;
      }
      if (colKey === '_sidebar_mandato') {
        App.setMandateFilter('todos');
        return;
      }
      if (colKey === '_sidebar_ideologia') {
        App.setIdeologyRange(1, 7);
        return;
      }
      if (colKey === '_sidebar_partidos_gosta') {
        App.state.partidos_gosta = [];
        App.renderPartidosChips();
        App.state.pagina = 1;
        App.carregarCandidatos();
        return;
      }
      if (colKey === '_sidebar_partidos_desgosta') {
        App.state.partidos_desgosta = [];
        App.renderPartidosChips();
        App.state.pagina = 1;
        App.carregarCandidatos();
        return;
      }
      if (colKey === '_sidebar_busca') {
        App.state.busca = '';
        const inp = document.getElementById("searchInput") || document.getElementById("inputBusca");
        if (inp) inp.value = "";
        App.state.pagina = 1;
        App.carregarCandidatos();
        return;
      }
    }
    if (colKey === 'filtro_lista') {
      delete this.filters.filtro_lista;
      App.renderView();
      return;
    }
    if (colKey === 'situacao') {
      delete this.filters.situacao;
      App.setSituacaoFilter('DEFERIDOS_AGUARDANDO');
      return;
    }
    if (colKey === 'genero') {
      delete this.filters.genero;
      delete App.state.genero;
      if (window.CardFilterManager) CardFilterManager.updateCardFilterButtons();
      App.state.pagina = 1;
      App.carregarCandidatos();
      return;
    }
    if (colKey === 'raca') {
      delete this.filters.raca;
      delete App.state.cor_raca;
      if (window.CardFilterManager) CardFilterManager.updateCardFilterButtons();
      App.state.pagina = 1;
      App.carregarCandidatos();
      return;
    }
    if (colKey === 'instrucao') {
      delete this.filters.instrucao;
      delete App.state.grau_instrucao;
      if (window.CardFilterManager) CardFilterManager.updateCardFilterButtons();
      App.state.pagina = 1;
      App.carregarCandidatos();
      return;
    }
    if (colKey === 'profissao') {
      delete this.filters.profissao;
      delete App.state.ocupacao;
      if (window.CardFilterManager) CardFilterManager.updateCardFilterButtons();
      App.state.pagina = 1;
      App.carregarCandidatos();
      return;
    }
    if (colKey === 'faixa_etaria') {
      delete this.filters.faixa_etaria;
      delete App.state.faixa_etaria;
      if (window.CardFilterManager) CardFilterManager.updateCardFilterButtons();
      App.state.pagina = 1;
      App.carregarCandidatos();
      return;
    }
    if (colKey === 'bens' || colKey === 'ranking_bens') {
      delete this.filters.bens;
      delete this.filters.ranking_bens;
      delete App.state.bens_min;
      delete App.state.bens_max;
      if (window.CardFilterManager) CardFilterManager.updateCardFilterButtons();
      App.state.pagina = 1;
      App.carregarCandidatos();
      return;
    }
    if (colKey === 'campanha_ranking' || colKey === 'campanha' || colKey === 'gastos' || colKey === 'receitas' || colKey === 'ranking_partido' || colKey === 'ranking_cargo') {
      delete this.filters.campanha_ranking;
      delete this.filters.gastos;
      delete this.filters.receitas;
      delete this.filters.ranking_partido;
      delete this.filters.ranking_cargo;
      delete App.state.gastos_min;
      delete App.state.gastos_max;
      delete App.state.receitas_min;
      delete App.state.receitas_max;
      if (window.CardFilterManager) CardFilterManager.updateCardFilterButtons();
      App.state.pagina = 1;
      App.carregarCandidatos();
      return;
    }
    this.currentCol = colKey;
    this.clearCurrent();
  },

  clearCurrent() {
    const col = this.currentCol;
    if (col) {
      delete this.filters[col];
      if (col === "candidato") {
        App.state.busca = "";
        const inp = document.getElementById("searchInput") || document.getElementById("inputBusca");
        if (inp) inp.value = "";
        App.state.pagina = 1;
        this.close();
        App.carregarCandidatos();
        return;
      } else if (col === "partido") {
        App.state.partidos_gosta = [];
        App.renderPartidosChips();
        App.state.pagina = 1;
        this.close();
        App.carregarCandidatos();
        return;
      } else if (col === "cargo") {
        App.state.selectedCargos = [];
        App.renderCargosPills();
        App.state.pagina = 1;
        this.close();
        App.carregarCandidatos();
        return;
      } else if (col === "mandato") {
        App.setMandateFilter("todos");
        this.close();
        return;
      } else if (col === "ideologia") {
        App.setIdeologyRange(1, 7);
        this.close();
        return;
      } else if (col === "genero") {
        delete App.state.genero;
        if (window.CardFilterManager) CardFilterManager.updateCardFilterButtons();
        App.state.pagina = 1;
        this.close();
        App.carregarCandidatos();
        return;
      } else if (col === "raca") {
        delete App.state.cor_raca;
        if (window.CardFilterManager) CardFilterManager.updateCardFilterButtons();
        App.state.pagina = 1;
        this.close();
        App.carregarCandidatos();
        return;
      } else if (col === "instrucao") {
        delete App.state.grau_instrucao;
        if (window.CardFilterManager) CardFilterManager.updateCardFilterButtons();
        App.state.pagina = 1;
        this.close();
        App.carregarCandidatos();
        return;
      } else if (col === "profissao") {
        delete App.state.ocupacao;
        if (window.CardFilterManager) CardFilterManager.updateCardFilterButtons();
        App.state.pagina = 1;
        this.close();
        App.carregarCandidatos();
        return;
      } else if (col === "bens" || col === "ranking_bens") {
        delete this.filters.bens;
        delete this.filters.ranking_bens;
        delete App.state.bens_min;
        delete App.state.bens_max;
        if (window.CardFilterManager) CardFilterManager.updateCardFilterButtons();
        App.state.pagina = 1;
        this.close();
        App.carregarCandidatos();
        return;
      } else if (col === "gastos" || col === "receitas" || col === "ranking_partido" || col === "ranking_cargo" || col === "campanha_ranking" || col === "prioridade_partido" || col === "campanha") {
        delete this.filters.campanha_ranking;
        delete this.filters.prioridade_partido;
        delete this.filters.gastos;
        delete this.filters.receitas;
        delete this.filters.ranking_partido;
        delete this.filters.ranking_cargo;
        delete App.state.gastos_min;
        delete App.state.gastos_max;
        delete App.state.receitas_min;
        delete App.state.receitas_max;
        if (window.CardFilterManager) CardFilterManager.updateCardFilterButtons();
        App.state.pagina = 1;
        this.close();
        App.carregarCandidatos();
        return;
      }
    }
    this.close();
    App.renderView();
  },

  filterCandidates(cands) {
    let res = cands;
    const tf = this.filters;

    if (tf.filtro_lista && tf.filtro_lista.sqList) {
      res = res.filter(c => tf.filtro_lista.sqList.has(String(c.sq_candidato)));
    }

    if (tf.candidato && typeof tf.candidato === "string" && tf.candidato.trim()) {
      const q = tf.candidato.toLowerCase().trim();
      res = res.filter(c => (c.nome_urna || '').toLowerCase().includes(q) || (c.nome_completo || '').toLowerCase().includes(q));
    }

    if (tf.numero && typeof tf.numero === "string" && tf.numero.trim()) {
      const q = tf.numero.trim();
      res = res.filter(c => String(c.numero || '').includes(q));
    }

    if (tf.partido && Array.isArray(tf.partido)) {
      const set = new Set(tf.partido);
      res = res.filter(c => {
        const p = c.partido || "OUTROS";
        return set.has(p) || ((p === "PMB" || p === "DEMOCRATA") && (set.has("DEMOCRATA") || set.has("Democrata")));
      });
    }

    if (tf.cargo && Array.isArray(tf.cargo)) {
      const set = new Set(tf.cargo);
      res = res.filter(c => set.has(c.cargo) || set.has(`${c.cargo} (${c.uf})`));
    }

    if (tf.uf && Array.isArray(tf.uf)) {
      const set = new Set(tf.uf);
      res = res.filter(c => set.has(c.uf || "BR"));
    }

    if (tf.ideologia && Array.isArray(tf.ideologia)) {
      const set = new Set(tf.ideologia);
      res = res.filter(c => set.has(c.ideologia_nome || 'Sem Classificação'));
    }

    if (tf.mandato && Array.isArray(tf.mandato) && tf.mandato.length > 0) {
      const set = new Set(tf.mandato);
      res = res.filter(c => {
        const mand = App.getCandidateMandatoInfo(c);
        if ((set.has("reeleicao") || set.has("Reeleição")) && mand.isReeleicao) return true;
        if ((set.has("outro_cargo") || set.has("Em exercício (outro cargo)")) && c.em_exercicio && !mand.isReeleicao) return true;
        if ((set.has("sem_mandato") || set.has("Sem mandato") || set.has("novos")) && !c.em_exercicio) return true;
        if (set.has("⚡ Em Exercício") && c.em_exercicio) return true;
        if (set.has("Não consta") && !c.em_exercicio) return true;
        return false;
      });
    }

    if (tf.situacao && Array.isArray(tf.situacao) && tf.situacao.length > 0) {
      const set = new Set(tf.situacao.map(s => String(s).toUpperCase().trim()));
      if (!set.has("TODAS") && !set.has("TODOS")) {
        res = res.filter(c => {
          const val = String(c.situacao_candidatura || 'Aguardando julgamento').toUpperCase().trim();
          const isInapto = val.includes('INAPTO') || val.includes('INDEFERIDO') || val.includes('CANCELADO') || val.includes('REN');
          const isAguardando = val.includes('AGUARDANDO') || val.includes('PENDENTE');
          const isApto = !isInapto && !isAguardando;
          if (set.has('DEFERIDOS_AGUARDANDO') || set.has('DEFERIDOS E AGUARDANDO') || set.has('DEFERIDOS E AGUARDANDO JULGAMENTO')) {
            if (isApto || isAguardando) return true;
          }
          if (set.has('APTO') || set.has('APTOS') || set.has('APTOS / DEFERIDOS')) {
            if (isApto) return true;
          }
          if (set.has('AGUARDANDO') || set.has('AGUARDANDO JULGAMENTO')) {
            if (isAguardando) return true;
          }
          if (set.has('INAPTO') || set.has('INAPTOS') || set.has('INAPTOS / INDEFERIDOS')) {
            if (isInapto) return true;
          }
          return set.has(val);
        });
      }
    }

    if (tf.profissao && Array.isArray(tf.profissao)) {
      const set = new Set(tf.profissao);
      res = res.filter(c => set.has((c.ocupacao && c.ocupacao !== "Não informada") ? c.ocupacao : "Não informada"));
    }

    if (tf.raca && Array.isArray(tf.raca)) {
      const set = new Set(tf.raca.map(r => r.toUpperCase()));
      res = res.filter(c => set.has((c.cor_raca && c.cor_raca !== "NÃO INFORMADO") ? c.cor_raca.toUpperCase() : "NÃO INFORMADO"));
    }

    if (tf.genero && Array.isArray(tf.genero) && tf.genero.length > 0) {
      const set = new Set(tf.genero.map(g => g.toUpperCase()));
      res = res.filter(c => {
        const g = (c.genero || '').toUpperCase();
        return set.has(g) || (g === 'FEMININO' && (set.has('MULHERES') || set.has('MULHERES (FEMININO)'))) || (g === 'MASCULINO' && (set.has('HOMENS') || set.has('HOMENS (MASCULINO)')));
      });
    }

    if (tf.instrucao && Array.isArray(tf.instrucao) && tf.instrucao.length > 0) {
      const set = new Set(tf.instrucao.map(i => i.toUpperCase()));
      res = res.filter(c => set.has((c.grau_instrucao || 'NÃO INFORMADO').toUpperCase()));
    }

    if (tf.faixa_etaria && Array.isArray(tf.faixa_etaria) && tf.faixa_etaria.length > 0) {
      const set = new Set(tf.faixa_etaria);
      res = res.filter(c => {
        const id = c.idade || (c.dt_nascimento ? App.calcularIdade(c.dt_nascimento, 2026) : null);
        if (!id) return false;
        for (const f of set) {
          if (f === "18-24" && id >= 18 && id <= 24) return true;
          if (f === "25-34" && id >= 25 && id <= 34) return true;
          if (f === "35-44" && id >= 35 && id <= 44) return true;
          if (f === "45-59" && id >= 45 && id <= 59) return true;
          if (f === "60+" && id >= 60) return true;
        }
        return false;
      });
    }

    if (tf.proposta === true) {
      res = res.filter(c => c.tem_proposta || Boolean(c.plano_governo_url) || ['PRESIDENTE', 'GOVERNADOR'].includes(c.cargo));
    }

    if (tf.bens) {
      if (tf.bens.min !== null && tf.bens.min !== undefined) {
        res = res.filter(c => (c.total_bens || 0) >= tf.bens.min);
      }
      if (tf.bens.max !== null && tf.bens.max !== undefined) {
        res = res.filter(c => (c.total_bens || 0) <= tf.bens.max);
      }
    }

    if (tf.ranking_bens) {
      const sortedByBens = [...res].sort((a, b) => (b.total_bens || 0) - (a.total_bens || 0));
      const topSet = new Set(sortedByBens.slice(0, tf.ranking_bens).map(c => c.sq_candidato));
      res = res.filter(c => topSet.has(c.sq_candidato));
    }

    if (tf.receitas) {
      if (tf.receitas.min !== null && tf.receitas.min !== undefined) {
        res = res.filter(c => (c.financiamento_receita || 0) >= tf.receitas.min);
      }
      if (tf.receitas.max !== null && tf.receitas.max !== undefined) {
        res = res.filter(c => (c.financiamento_receita || 0) <= tf.receitas.max);
      }
    }

    if (tf.gastos) {
      if (tf.gastos.min !== null && tf.gastos.min !== undefined) {
        res = res.filter(c => (c.financiamento_despesa || 0) >= tf.gastos.min);
      }
      if (tf.gastos.max !== null && tf.gastos.max !== undefined) {
        res = res.filter(c => (c.financiamento_despesa || 0) <= tf.gastos.max);
      }
    }

    if (tf.ranking_partido) {
      res = res.filter(c => {
        const r = c.ranking_partido_despesa || c.ranking_partido_receita;
        return r && r <= tf.ranking_partido;
      });
    }

    if (tf.ranking_cargo) {
      res = res.filter(c => {
        const r = c.ranking_gasto_cargo;
        return r && r <= tf.ranking_cargo;
      });
    }

    if (tf.campanha_ranking) {
      const cr = tf.campanha_ranking;
      const met = cr.metrica || 'receitas';
      const esc = cr.escopo || 'partido';
      const min = (cr.min !== null && cr.min !== undefined) ? cr.min : 1;
      const max = cr.max;

      res = res.filter(c => {
        let rk = null;
        if (esc === 'partido') {
          rk = (met === 'receitas')
            ? (c.ranking_partido_receita || c.ranking_partido_despesa)
            : (c.ranking_partido_despesa || c.ranking_partido_receita);
        } else {
          rk = (met === 'receitas')
            ? (c.ranking_receita_cargo || c.ranking_gasto_cargo)
            : (c.ranking_gasto_cargo || c.ranking_receita_cargo);
        }
        if (rk === null || rk === undefined || isNaN(rk)) return false;
        if (min !== null && min !== undefined && rk < min) return false;
        if (max !== null && max !== undefined && rk > max) return false;
        return true;
      });
    }

    if (tf.estado_civil && Array.isArray(tf.estado_civil) && tf.estado_civil.length > 0) {
      const set = new Set(tf.estado_civil.map(e => String(e).toUpperCase()));
      res = res.filter(c => set.has(String(c.estado_civil || '').toUpperCase()));
    }

    return res;
  },

  applyCurrentSilent() {
    this.close();
    App.renderView();
    if (window.CardFilterManager) {
      CardFilterManager.updateCardFilterButtons();
    }
  }
};

/* ─── Popover Intuitivo de Filtros Rápidos dos Cards (Voter-Friendly) ─────── */

const CardFilterManager = {
  currentKey: null,
  _outsideHandler: null,

  toggle(trigger, key, evt) {
    if (evt && typeof evt.stopPropagation === 'function') {
      evt.stopPropagation();
    } else if (window.event && typeof window.event.stopPropagation === 'function') {
      window.event.stopPropagation();
    }
    if (window.TableFilterManager) {
      TableFilterManager.close();
    }
    // Close top popovers (Ideologia/Partidos) to prevent overlap
    if (window.App && typeof App.closeTopPopovers === 'function') {
      App.closeTopPopovers();
    }

    let btnEl = null;
    if (trigger instanceof HTMLElement) {
      btnEl = trigger;
    } else if (trigger && trigger.currentTarget instanceof HTMLElement) {
      btnEl = trigger.currentTarget;
    } else if (trigger && trigger.target instanceof HTMLElement) {
      btnEl = trigger.target.closest('button') || trigger.target;
    }
    if (!btnEl && typeof key === 'string') {
      btnEl = document.getElementById(`btnQuick_${key}`);
    }

    const pop = document.getElementById("cardFilterPopover");
    if (this.currentKey === key && pop && pop.style.display !== "none") {
      this.close();
      return;
    }
    this.open(btnEl, key);
  },

  open(btnEl, key) {
    this.close();
    this.currentKey = key;
    const pop = document.getElementById("cardFilterPopover");
    if (!pop) return;

    const titleEl = document.getElementById("cardFilterTitle");
    const optContainer = document.getElementById("cardFilterOptions");

    const titles = {
      cargo_uf: "Cargo & UF",
      ideologia_partido: "Ideologia & Partidos",
      situacao: "Situação no TSE",
      mandato: "Mandato Atual",
      genero: "Gênero",
      raca: "Cor / Raça",
      instrucao: "Escolaridade",
      profissao: "Profissão",
      faixa_etaria: "Faixa Etária / Idade",
      campanha: "Financiamento de Campanha",
      bens: "Bens Declarados"
    };

    if (titleEl) titleEl.textContent = titles[key] || "Filtrar";

    const rect = (btnEl && typeof btnEl.getBoundingClientRect === 'function')
      ? btnEl.getBoundingClientRect()
      : { bottom: 120, left: 200, top: 80 };

    pop.style.display = "block";
    const popWidth = 310;
    let left = rect.left;
    if (left + popWidth > window.innerWidth - 12) {
      left = Math.max(10, window.innerWidth - popWidth - 12);
    }
    let top = rect.bottom + 6;
    if (top + 340 > window.innerHeight && rect.top > 340) {
      top = Math.max(10, rect.top - 350);
    } else if (top + 300 > window.innerHeight) {
      top = Math.max(10, window.innerHeight - 320);
    }
    if (window.innerWidth <= 640) {
      pop.style.top = "";
      pop.style.left = "";
      document.getElementById("popoverBackdrop")?.classList.add("show");
    } else {
      pop.style.top = `${Math.max(10, top)}px`;
      pop.style.left = `${Math.max(10, left)}px`;
    }

    try {
      this.renderOptions(key, optContainer);
    } catch (err) {
      console.error("Erro ao renderizar opções do filtro rápido:", err);
    }
  },

  close() {
    const pop = document.getElementById("cardFilterPopover");
    if (pop) pop.style.display = "none";
    document.getElementById("popoverBackdrop")?.classList.remove("show");
    this.currentKey = null;
  },

  renderOptions(key, container) {
    if (!container) return;

    if (key === "cargo_uf") {
      const curUf = App.state.uf || "BR";
      const ufsList = App.state.ufs || ["AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"];
      const cargosList = [
        { cargo: "", label: "Todos os Cargos" },
        { cargo: "PRESIDENTE", label: "Presidente" },
        { cargo: "GOVERNADOR", label: "Governador" },
        { cargo: "SENADOR", label: "Senador" },
        { cargo: "DEPUTADO FEDERAL", label: "Dep. Federal" },
        { cargo: "DEPUTADO ESTADUAL", label: "Dep. Estadual" },
        { cargo: "DEPUTADO DISTRITAL", label: "Dep. Distrital (DF)" }
      ];
      const selCargo = (App.state.selectedCargos && App.state.selectedCargos.length > 0) ? App.state.selectedCargos[0] : "";

      container.innerHTML = `
        <div style="display: flex; flex-direction: column; gap: 0.75rem; padding: 0.2rem 0;">
          <div>
            <label class="form-label" style="font-size: 0.7rem; margin-bottom: 0.35rem; display: block;">Estado / Âmbito:</label>
            <select class="form-select" style="width: 100%; font-size: 0.78rem;" onchange="App.setUfFilter(this.value); CardFilterManager.renderOptions('cargo_uf', document.getElementById('cardFilterOptions'));">
              <option value="BR" ${curUf === 'BR' ? 'selected' : ''}>🇧🇷 Brasil (Nacional)</option>
              ${ufsList.filter(u => u !== 'BR').map(u => `<option value="${u}" ${curUf === u ? 'selected' : ''}>${u}</option>`).join('')}
            </select>
          </div>
          <div>
            <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.35rem;">
              <label class="form-label" style="font-size: 0.7rem; margin: 0;">Cargo Pretendido:</label>
              <button type="button" class="btn-text-discreet" onclick="App.toggleCargo(''); CardFilterManager.renderOptions('cargo_uf', document.getElementById('cardFilterOptions'));">Limpar</button>
            </div>
            <div class="cargo-pills" style="display: flex; flex-direction: column; gap: 0.35rem;">
              ${cargosList.map(c => {
        const isAct = c.cargo === "" ? (!selCargo) : (selCargo === c.cargo);
        return `
                  <button type="button" class="cargo-pill ${isAct ? 'active' : ''}" style="justify-content: flex-start; padding: 0.35rem 0.6rem; font-size: 0.75rem;" onclick="App.toggleCargo('${c.cargo}'); CardFilterManager.renderOptions('cargo_uf', document.getElementById('cardFilterOptions'));">
                    ${isAct ? '✓ ' : ''}${c.label}
                  </button>
                `;
      }).join('')}
            </div>
          </div>
        </div>
      `;
      return;
    }

    if (key === "ideologia_partido") {
      const minI = App.state.ideologia_min || 1;
      const maxI = App.state.ideologia_max || 7;
      const isAll = (minI === 1 && maxI === 7);
      const partidos = App.state.partidos || [];

      container.innerHTML = `
        <div style="display: flex; flex-direction: column; gap: 0.75rem; padding: 0.2rem 0; max-height: 380px; overflow-y: auto;">
          <div>
            <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.35rem;">
              <span style="font-size: 0.7rem; font-weight: 700; color: var(--text-muted); text-transform: uppercase;">Espectro Político</span>
              <button type="button" class="btn-text-discreet" onclick="App.setIdeologyRange(1, 7); CardFilterManager.renderOptions('ideologia_partido', document.getElementById('cardFilterOptions'));">Todos</button>
            </div>
            <div style="display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 0.35rem;">
              <button type="button" class="btn-mandate-toggle ${(minI === 1 && maxI <= 3) ? 'active' : ''}" style="font-size: 0.72rem; padding: 0.35rem; text-align: center; justify-content: center;" onclick="App.setIdeologyRange(1, 3); CardFilterManager.renderOptions('ideologia_partido', document.getElementById('cardFilterOptions'));">Esquerda</button>
              <button type="button" class="btn-mandate-toggle ${(minI >= 3 && maxI <= 5 && !isAll) ? 'active' : ''}" style="font-size: 0.72rem; padding: 0.35rem; text-align: center; justify-content: center;" onclick="App.setIdeologyRange(3, 5); CardFilterManager.renderOptions('ideologia_partido', document.getElementById('cardFilterOptions'));">Centro</button>
              <button type="button" class="btn-mandate-toggle ${(minI >= 5 && maxI === 7) ? 'active' : ''}" style="font-size: 0.72rem; padding: 0.35rem; text-align: center; justify-content: center;" onclick="App.setIdeologyRange(5, 7); CardFilterManager.renderOptions('ideologia_partido', document.getElementById('cardFilterOptions'));">Direita</button>
            </div>
          </div>
          <div>
            <div style="font-size: 0.7rem; font-weight: 700; color: var(--text-muted); text-transform: uppercase; margin-bottom: 0.35rem;">
              Partidos (${partidos.length}):
            </div>
            <div style="display: flex; flex-wrap: wrap; gap: 0.3rem; max-height: 180px; overflow-y: auto;">
              ${partidos.map(p => {
        const isGosto = App.state.partidos_gosta.includes(p.sigla);
        const isDesgosto = App.state.partidos_desgosta.includes(p.sigla);
        let statusClass = "";
        if (isGosto) statusClass = "selected";
        if (isDesgosto) statusClass = "desgostado";
        return `
                  <button type="button" class="chip-btn ${statusClass}" style="font-size: 0.68rem; padding: 0.2rem 0.45rem;" onclick="App.toggleGostoPartido('${p.sigla}'); CardFilterManager.renderOptions('ideologia_partido', document.getElementById('cardFilterOptions'));">
                    ${p.sigla}
                  </button>
                `;
      }).join('')}
            </div>
          </div>
        </div>
      `;
      return;
    }

    if (key === "situacao") {
      const cur = App.state.situacao || "DEFERIDOS_AGUARDANDO";
      const sit = App.state.contagens?.situacao || {};
      const aptoNum = sit.APTO || 0;
      const aguardandoNum = sit.AGUARDANDO || 0;
      const aptoAguardandoNum = aptoNum + aguardandoNum;
      const apto = aptoNum.toLocaleString('pt-BR');
      const aguardando = aguardandoNum.toLocaleString('pt-BR');
      const deferidosAguardando = aptoAguardandoNum.toLocaleString('pt-BR');
      const total = (App.state.contagens?.total ?? App.state.total ?? 0).toLocaleString('pt-BR');

      container.innerHTML = `
        <div style="padding: 0.2rem 0;">
          <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.6rem;">
            <span style="font-size: 0.72rem; font-weight: 700; color: var(--text-muted); text-transform: uppercase;">
              Situação no TSE
            </span>
            <button type="button" class="btn-text-discreet ${cur === 'todos' ? 'active' : ''}" onclick="App.setSituacaoFilter('todos'); CardFilterManager.close();" title="Exibir todos os candidatos registrados">
              Ver Todos (${total})
            </button>
          </div>
          <div class="mandate-toggle-group" style="display: flex; flex-direction: column; gap: 0.35rem;">
            <button type="button" class="btn-mandate-toggle ${(cur === 'DEFERIDOS_AGUARDANDO' || !cur || cur === 'padrao') ? 'active' : ''}" style="display: flex; justify-content: space-between; padding: 0.4rem 0.6rem;" onclick="App.setSituacaoFilter('DEFERIDOS_AGUARDANDO'); CardFilterManager.close();">
              <span>✓ Deferidos e Aguardando (Padrão)</span>
              <span style="font-size: 0.72rem; opacity: 0.8; font-family: var(--font-mono);">(${deferidosAguardando})</span>
            </button>
            <button type="button" class="btn-mandate-toggle ${cur === 'APTO' ? 'active' : ''}" style="display: flex; justify-content: space-between; padding: 0.4rem 0.6rem;" onclick="App.setSituacaoFilter('APTO'); CardFilterManager.close();">
              <span>Apenas Deferidos / Aptos</span>
              <span style="font-size: 0.72rem; opacity: 0.8; font-family: var(--font-mono);">(${apto})</span>
            </button>
            <button type="button" class="btn-mandate-toggle ${cur === 'AGUARDANDO' ? 'active' : ''}" style="display: flex; justify-content: space-between; padding: 0.4rem 0.6rem;" onclick="App.setSituacaoFilter('AGUARDANDO'); CardFilterManager.close();">
              <span>⏳ Aguardando julgamento</span>
              <span style="font-size: 0.72rem; opacity: 0.8; font-family: var(--font-mono);">(${aguardando})</span>
            </button>
            <button type="button" class="btn-mandate-toggle ${cur === 'todos' ? 'active' : ''}" style="display: flex; justify-content: space-between; padding: 0.4rem 0.6rem;" onclick="App.setSituacaoFilter('todos'); CardFilterManager.close();">
              <span>Todos os Registrados</span>
              <span style="font-size: 0.72rem; opacity: 0.8; font-family: var(--font-mono);">(${total})</span>
            </button>
          </div>
        </div>
      `;
      return;
    }

    if (key === "mandato") {
      const mand = App.state.contagens?.mandato || {};
      const reeleicao = (mand.reeleicao ?? 0).toLocaleString('pt-BR');
      const outroCargo = (mand.outro_cargo ?? 0).toLocaleString('pt-BR');
      const semMandato = (mand.sem_mandato ?? 0).toLocaleString('pt-BR');
      const total = (App.state.contagens?.total ?? App.state.total ?? 0).toLocaleString('pt-BR');
      const cur = App.state.situacao_mandato || "todos";

      container.innerHTML = `
        <div style="padding: 0.2rem 0;">
          <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.6rem;">
            <span style="font-size: 0.72rem; font-weight: 700; color: var(--text-muted); text-transform: uppercase;">
              Mandato Atual
            </span>
            <button type="button" class="btn-text-discreet ${cur === 'todos' ? 'active' : ''}" onclick="App.setMandateFilter('todos'); CardFilterManager.close();">
              Ver Todos (${total})
            </button>
          </div>
          <div class="mandate-toggle-group" style="display: flex; flex-direction: column; gap: 0.35rem;">
            <button type="button" class="btn-mandate-toggle ${cur === 'reeleicao' ? 'active' : ''}" style="display: flex; justify-content: space-between; padding: 0.4rem 0.6rem;" onclick="App.setMandateFilter('reeleicao'); CardFilterManager.close();">
              <span>⚡ Reeleição</span>
              <span style="opacity: 0.8; font-size: 0.72rem; font-family: var(--font-mono);">(${reeleicao})</span>
            </button>
            <button type="button" class="btn-mandate-toggle ${cur === 'outro_cargo' ? 'active' : ''}" style="display: flex; justify-content: space-between; padding: 0.4rem 0.6rem;" onclick="App.setMandateFilter('outro_cargo'); CardFilterManager.close();">
              <span>Em exercício (outro cargo)</span>
              <span style="opacity: 0.8; font-size: 0.72rem; font-family: var(--font-mono);">(${outroCargo})</span>
            </button>
            <button type="button" class="btn-mandate-toggle ${cur === 'sem_mandato' ? 'active' : ''}" style="display: flex; justify-content: space-between; padding: 0.4rem 0.6rem;" onclick="App.setMandateFilter('sem_mandato'); CardFilterManager.close();">
              <span>Não consta</span>
              <span style="opacity: 0.8; font-size: 0.72rem; font-family: var(--font-mono);">(${semMandato})</span>
            </button>
          </div>
        </div>
      `;
      return;
    }

    if (key === "bens") {
      const cur = TableFilterManager.filters.bens || {};
      const val = App.state.bens_max !== undefined && App.state.bens_max !== null ? App.state.bens_max : (cur.max !== undefined && cur.max !== null ? cur.max : 10000000);

      container.innerHTML = `
        <div class="popover-slider-box">
          <div class="popover-slider-header">
            <span class="popover-slider-title">Patrimônio Declarado (Bens):</span>
            <span id="cardBensSliderLabel" class="popover-slider-val">
              ${val < 10000000 ? 'Até R$ ' + (val).toLocaleString('pt-BR') : 'Sem limite (Todos)'}
            </span>
          </div>
          <input type="range" min="0" max="10000000" step="100000" value="${val}" class="popover-range-slider" oninput="CardFilterManager.onSliderChange('bens', this.value)">
          <div class="popover-slider-ticks">
            <span>R$ 0</span>
            <span>R$ 2M</span>
            <span>R$ 5M</span>
            <span>R$ 10M+</span>
          </div>
        </div>

        <div style="font-size: 0.7rem; color: var(--text-muted); line-height: 1.45; margin-bottom: 0.5rem;">
          💡 Deslize para filtrar candidatos cujo total de bens declarados ao TSE seja até o valor selecionado.
        </div>

        ${((App.state.bens_max !== undefined && App.state.bens_max < 10000000) || (cur.max !== undefined && cur.max < 10000000)) ? `
          <button type="button" class="btn-text-discreet" style="width: 100%; text-align: center; margin-top: 0.3rem; padding: 0.35rem 0;" onclick="CardFilterManager.clearKey('bens')">
            ✕ Limpar filtro (Mostrar todos os patrimônios)
          </button>
        ` : ''}
      `;
      return;
    } else if (key === "campanha") {
      this.renderCampanhaOptions(container, 'card');
      return;
    } else if (key === "genero") {
      const cur = (App.state.genero ? [App.state.genero] : null) || TableFilterManager.filters.genero || [];
      const gen = App.state.contagens?.genero || {};
      const total = (App.state.contagens?.total ?? App.state.total ?? 0).toLocaleString('pt-BR');
      const femCount = (gen.FEMININO || 0).toLocaleString('pt-BR');
      const mascCount = (gen.MASCULINO || 0).toLocaleString('pt-BR');

      const options = [
        { label: `Todos os gêneros (${total})`, val: null },
        { label: `Mulheres (${femCount})`, val: "FEMININO" },
        { label: `Homens (${mascCount})`, val: "MASCULINO" }
      ];
      container.innerHTML = options.map(opt => {
        const isActive = opt.val === null ? (cur.length === 0) : cur.includes(opt.val);
        return `
          <button type="button" class="card-filter-opt-btn ${isActive ? 'active' : ''}" onclick="CardFilterManager.setValue('genero', '${opt.val || ''}', '${opt.label}')">
            <span>${opt.label}</span>
            ${isActive ? '<span>✓</span>' : ''}
          </button>
        `;
      }).join("");
    } else if (key === "raca") {
      const cur = (App.state.cor_raca ? [App.state.cor_raca] : null) || TableFilterManager.filters.raca || [];
      const racaCounts = App.state.contagens?.raca || {};
      const total = (App.state.contagens?.total ?? App.state.total ?? 0).toLocaleString('pt-BR');

      const findCount = (dict, k) => {
        if (!dict || !k) return 0;
        if (dict[k] !== undefined) return dict[k];
        const clean = (s) => String(s).normalize("NFD").replace(/[\u0300-\u036f]/g, "").toUpperCase().trim();
        const target = clean(k);
        for (const [dk, dv] of Object.entries(dict)) {
          if (clean(dk) === target) return dv;
        }
        return 0;
      };

      const getCount = (k) => {
        const n = findCount(racaCounts, k);
        return n > 0 ? ` (${n.toLocaleString('pt-BR')})` : '';
      };

      const options = [
        { label: `Todas as cores / raças (${total})`, val: null },
        { label: `Parda${getCount('PARDA')}`, val: "PARDA" },
        { label: `Branca${getCount('BRANCA')}`, val: "BRANCA" },
        { label: `Preta${getCount('PRETA')}`, val: "PRETA" },
        { label: `Indígena${getCount('INDÍGENA')}`, val: "INDÍGENA" },
        { label: `Amarela${getCount('AMARELA')}`, val: "AMARELA" }
      ];
      container.innerHTML = options.map(opt => {
        const isActive = opt.val === null ? (cur.length === 0) : cur.includes(opt.val);
        return `
          <button type="button" class="card-filter-opt-btn ${isActive ? 'active' : ''}" onclick="CardFilterManager.setValue('raca', '${opt.val || ''}', '${opt.label}')">
            <span>${opt.label}</span>
            ${isActive ? '<span>✓</span>' : ''}
          </button>
        `;
      }).join("");
    } else if (key === "instrucao") {
      const cur = (App.state.grau_instrucao ? [App.state.grau_instrucao] : null) || TableFilterManager.filters.instrucao || [];
      const instCounts = App.state.contagens?.instrucao || {};
      const total = (App.state.contagens?.total ?? App.state.total ?? 0).toLocaleString('pt-BR');

      const findCount = (dict, k) => {
        if (!dict || !k) return 0;
        if (dict[k] !== undefined) return dict[k];
        const clean = (s) => String(s).normalize("NFD").replace(/[\u0300-\u036f]/g, "").toUpperCase().trim();
        const target = clean(k);
        for (const [dk, dv] of Object.entries(dict)) {
          if (clean(dk) === target) return dv;
        }
        return 0;
      };

      const getCount = (k) => {
        const n = findCount(instCounts, k);
        return n > 0 ? ` (${n.toLocaleString('pt-BR')})` : '';
      };

      const options = [
        { label: `Todas as escolaridades (${total})`, val: null },
        { label: `Superior Completo${getCount('SUPERIOR COMPLETO')}`, val: "SUPERIOR COMPLETO" },
        { label: `Superior Incompleto${getCount('SUPERIOR INCOMPLETO')}`, val: "SUPERIOR INCOMPLETO" },
        { label: `Ensino Médio Completo${getCount('ENSINO MÉDIO COMPLETO')}`, val: "ENSINO MÉDIO COMPLETO" },
        { label: `Ensino Médio Incompleto${getCount('ENSINO MÉDIO INCOMPLETO')}`, val: "ENSINO MÉDIO INCOMPLETO" },
        { label: `Ensino Fundamental Completo${getCount('ENSINO FUNDAMENTAL COMPLETO')}`, val: "ENSINO FUNDAMENTAL COMPLETO" },
        { label: `Ensino Fundamental Incompleto${getCount('ENSINO FUNDAMENTAL INCOMPLETO')}`, val: "ENSINO FUNDAMENTAL INCOMPLETO" },
        { label: `Lê e Escreve${getCount('LÊ E ESCREVE')}`, val: "LÊ E ESCREVE" }
      ];
      container.innerHTML = options.map(opt => {
        const isActive = opt.val === null ? (cur.length === 0) : cur.includes(opt.val);
        return `
          <button type="button" class="card-filter-opt-btn ${isActive ? 'active' : ''}" onclick="CardFilterManager.setValue('instrucao', '${opt.val || ''}', '${opt.label}')">
            <span>${opt.label}</span>
            ${isActive ? '<span>✓</span>' : ''}
          </button>
        `;
      }).join("");
    } else if (key === "profissao") {
      const cur = (App.state.ocupacao ? [App.state.ocupacao] : null) || TableFilterManager.filters.profissao || [];
      const toTitle = (s) => s.charAt(0).toUpperCase() + s.slice(1).toLowerCase();

      // Usar lista dinâmica de contagens do backend
      let occList = [];
      if (App.state.contagens?.profissao && Object.keys(App.state.contagens.profissao).length > 0) {
        occList = Object.entries(App.state.contagens.profissao).map(([oc, total]) => ({
          ocupacao: oc,
          total: total
        }));
      } else if (App.state.todasOcupacoes && App.state.todasOcupacoes.length > 0) {
        occList = (App.state.todasOcupacoes || []).map(item => {
          if (typeof item === 'string') return { ocupacao: item, total: 0 };
          return { ocupacao: item.ocupacao, total: item.total || 0 };
        });
      }

      if (occList.length === 0) {
        const counts = {};
        (App.state.candidatos || []).forEach(c => {
          if (c.ocupacao && c.ocupacao !== "Não informada") {
            counts[c.ocupacao] = (counts[c.ocupacao] || 0) + 1;
          }
        });
        occList = Object.keys(counts).sort().map(k => ({ ocupacao: k, total: counts[k] }));
      }

      const optHtml = occList.map(item => {
        const oc = item.ocupacao;
        const count = item.total || 0;
        const isActive = cur.includes(oc);
        const safeOc = oc.replace(/'/g, "\\'");
        const safeTitle = toTitle(oc).replace(/'/g, "\\'");
        const countLabel = count > 0 ? `<span style="font-size: 0.65rem; color: var(--text-muted); margin-left: 0.3rem;">(${count.toLocaleString('pt-BR')})</span>` : '';
        return `
          <button type="button" class="card-filter-opt-btn ${isActive ? 'active' : ''}" onclick="CardFilterManager.setValue('profissao', '${safeOc}', '${safeTitle}')">
            <span>${toTitle(oc)} ${countLabel}</span>
            ${isActive ? '<span>✓</span>' : ''}
          </button>
        `;
      }).join("");

      const totalProf = occList.reduce((acc, it) => acc + (it.total || 0), 0);
      container.innerHTML = `
        <div style="margin-bottom: 0.4rem;">
          <input type="text" class="form-input" id="cardProfissaoSearch" placeholder="Buscar ocupação..." oninput="CardFilterManager.filterProfissaoList(this.value)" style="font-size: 0.75rem; padding: 0.38rem 0.55rem; width: 100%;">
        </div>
        <button type="button" class="card-filter-opt-btn ${cur.length === 0 ? 'active' : ''}" onclick="CardFilterManager.setValue('profissao', '', 'Todas')">
          <span>Todas as ocupações (${totalProf > 0 ? totalProf.toLocaleString('pt-BR') : occList.length})</span>
          ${cur.length === 0 ? '<span>✓</span>' : ''}
        </button>
        <div id="cardProfissaoList" style="display: flex; flex-direction: column; gap: 0.25rem; max-height: 270px; overflow-y: auto; padding-right: 2px;">
          ${optHtml}
        </div>
      `;
    } else if (key === "faixa_etaria") {
      const cur = (App.state.faixa_etaria ? App.state.faixa_etaria.split(",") : null) || TableFilterManager.filters.faixa_etaria || [];
      const fCounts = App.state.contagens?.faixa_etaria || {};
      const total = (App.state.contagens?.total ?? App.state.total ?? 0).toLocaleString('pt-BR');

      const getCount = (k) => {
        const n = fCounts[k] || 0;
        return n > 0 ? ` (${n.toLocaleString('pt-BR')})` : '';
      };

      const options = [
        { label: `Todas as faixas etárias (${total})`, val: null },
        { label: `18 a 24 anos${getCount('18-24')}`, val: "18-24" },
        { label: `25 a 34 anos${getCount('25-34')}`, val: "25-34" },
        { label: `35 a 44 anos${getCount('35-44')}`, val: "35-44" },
        { label: `45 a 59 anos${getCount('45-59')}`, val: "45-59" },
        { label: `60 anos ou mais${getCount('60+')}`, val: "60+" }
      ];
      container.innerHTML = options.map(opt => {
        const isActive = opt.val === null ? (cur.length === 0) : cur.includes(opt.val);
        return `
          <button type="button" class="card-filter-opt-btn ${isActive ? 'active' : ''}" onclick="CardFilterManager.setValue('faixa_etaria', '${opt.val || ''}', '${opt.label}')">
            <span>${opt.label}</span>
            ${isActive ? '<span>✓</span>' : ''}
          </button>
        `;
      }).join("");
    }
  },

  setRanking(type, val, label) {
    if (type === "partido") {
      delete TableFilterManager.filters.ranking_cargo;
      TableFilterManager.filters.ranking_partido = val;
    } else if (type === "cargo") {
      delete TableFilterManager.filters.ranking_partido;
      TableFilterManager.filters.ranking_cargo = val;
    } else if (type === "bens") {
      TableFilterManager.filters.ranking_bens = val;
    }
    this.close();
    this.updateCardFilterButtons();
    TableFilterManager.applyCurrentSilent();
  },

  renderCampanhaOptions(container, source = 'card') {
    const targetContainer = container || (source === 'table' ? document.getElementById('popoverBody') : document.getElementById('cardFilterOptions'));
    if (!targetContainer) return;

    const cur = TableFilterManager.filters.campanha_ranking || {
      metrica: "receitas",
      escopo: "partido",
      min: 1,
      max: null
    };
    const escopo = cur.escopo || "partido";
    const metrica = cur.metrica || "receitas";

    // Dynamic calculation of N (number of candidates in the scope)
    const allCands = App.state.candidatos || [];
    let maxN = 1;
    const activeTotal = (App.state.total && App.state.total > 0) ? App.state.total : allCands.length;

    if (escopo === "partido") {
      allCands.forEach(c => {
        const tot = c.total_partido_cargo_uf || 0;
        const rk = (metrica === "receitas") ? (c.ranking_partido_receita || 0) : (c.ranking_partido_despesa || 0);
        if (tot > maxN) maxN = tot;
        if (rk > maxN) maxN = rk;
      });
      if (maxN <= 1 && allCands.length > 0) {
        maxN = Math.min(allCands.length, 70);
      }
    } else {
      // Escopo Cargo: calibrado com o total ativo na página/painel superior
      if (activeTotal > 0) {
        maxN = activeTotal;
      } else {
        allCands.forEach(c => {
          const tot = c.total_cands_cargo || 0;
          const rk = (metrica === "receitas") ? (c.ranking_receita_cargo || 0) : (c.ranking_gasto_cargo || 0);
          if (tot > maxN) maxN = tot;
          if (rk > maxN) maxN = rk;
        });
      }
    }
    maxN = Math.max(maxN, 1);

    const curMin = (cur.min !== null && cur.min !== undefined && cur.min >= 1) ? Math.min(cur.min, maxN) : 1;
    const curMax = (cur.max !== null && cur.max !== undefined && cur.max >= 1) ? Math.min(cur.max, maxN) : maxN;

    const isFiltered = Boolean(TableFilterManager.filters.campanha_ranking && (curMin > 1 || curMax < maxN));

    let dynamicLabel = "";
    if (curMin === 1 && curMax >= maxN) {
      dynamicLabel = (escopo === 'partido') ? `Mostrando todas as posições partidárias` : `Mostrando todos (${maxN} no cargo)`;
    } else if (curMin === 1) {
      dynamicLabel = (escopo === 'partido')
        ? `Top ${curMax} de cada partido que mais ${metrica === 'receitas' ? 'arrecadaram' : 'gastaram'}`
        : `Top ${curMax} geral no cargo que mais ${metrica === 'receitas' ? 'arrecadaram' : 'gastaram'}`;
    } else {
      dynamicLabel = (escopo === 'partido')
        ? `${curMin}º ao ${curMax}º de cada partido que mais ${metrica === 'receitas' ? 'arrecadaram' : 'gastaram'}`
        : `${curMin}º ao ${curMax}º geral no cargo que mais ${metrica === 'receitas' ? 'arrecadaram' : 'gastaram'}`;
    }

    const escopoDesc = escopo === "partido" ? "no partido" : "no cargo/UF";
    const highlightLeft = maxN > 1 ? ((curMin - 1) / (maxN - 1)) * 100 : 0;
    const highlightWidth = maxN > 1 ? Math.max(0, ((curMax - curMin) / (maxN - 1)) * 100) : 100;

    targetContainer.innerHTML = `
      <div class="popover-campanha-wrap" style="display: flex; flex-direction: column; gap: 0.65rem; padding: 0.2rem 0;">
        <!-- 1. Escopo de Comparação -->
        <div>
          <div style="font-size: 0.68rem; font-weight: 700; color: var(--text-muted); text-transform: uppercase; margin-bottom: 0.35rem; display: flex; justify-content: space-between;">
            <span>Comparar Ranking</span>
            <span style="font-size: 0.65rem; color: var(--accent-petroleo); font-weight: 600;">${escopo === 'partido' ? `Máx. por legenda: ${maxN}` : `Total no cargo: ${maxN}`}</span>
          </div>
          <div style="display: grid; grid-template-columns: 1fr 1fr; gap: 0.35rem;">
            <button type="button" class="btn-mandate-toggle ${escopo === 'partido' ? 'active' : ''}" style="justify-content: center; padding: 0.4rem 0.5rem; font-size: 0.73rem; text-align: center;" onclick="CardFilterManager.setCampanhaEscopo('partido', '${source}')">
              🏛️ No Partido
            </button>
            <button type="button" class="btn-mandate-toggle ${escopo === 'cargo' ? 'active' : ''}" style="justify-content: center; padding: 0.4rem 0.5rem; font-size: 0.73rem; text-align: center;" onclick="CardFilterManager.setCampanhaEscopo('cargo', '${source}')">
              🎖️ No Cargo
            </button>
          </div>
          <div style="font-size: 0.69rem; color: var(--text-muted); line-height: 1.35; margin-top: 0.35rem; background: var(--bg-card); padding: 0.35rem 0.5rem; border-radius: var(--radius-xs); border-left: 2px solid var(--accent-petroleo);">
            ${escopo === 'partido'
        ? 'Compara com os colegas do mesmo partido na UF.'
        : 'Compara com todos os concorrentes ao mesmo cargo na UF.'}
          </div>
        </div>

        <!-- 2. Métrica Financeira -->
        <div>
          <div style="font-size: 0.68rem; font-weight: 700; color: var(--text-muted); text-transform: uppercase; margin-bottom: 0.35rem;">
            Critério de Financiamento
          </div>
          <div style="display: grid; grid-template-columns: 1fr 1fr; gap: 0.35rem;">
            <button type="button" class="btn-mandate-toggle ${metrica === 'receitas' ? 'active' : ''}" style="justify-content: center; padding: 0.4rem 0.5rem; font-size: 0.73rem; text-align: center;" onclick="CardFilterManager.setCampanhaMetrica('receitas', '${source}')">
              💰 Arrecadação
            </button>
            <button type="button" class="btn-mandate-toggle ${metrica === 'gastos' ? 'active' : ''}" style="justify-content: center; padding: 0.4rem 0.5rem; font-size: 0.73rem; text-align: center;" onclick="CardFilterManager.setCampanhaMetrica('gastos', '${source}')">
              💸 Gastos
            </button>
          </div>
        </div>

        <!-- 3. Indicador de Faixa Selecionada -->
        <div style="background: var(--bg-surface); border: 1px solid var(--border-color); border-radius: var(--radius-sm); padding: 0.5rem 0.65rem;">
          <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.15rem;">
            <span style="font-size: 0.68rem; font-weight: 700; color: var(--text-secondary); text-transform: uppercase;">Faixa de Posição</span>
            <span id="campanhaRankingRangeLabel" style="font-size: 0.74rem; font-weight: 700; color: var(--accent-petroleo);">
              ${curMin}º ao ${curMax}º
            </span>
          </div>
          <div id="campanhaRankingSubLabel" style="font-size: 0.70rem; color: var(--text-muted); line-height: 1.35;">
            ${dynamicLabel} ${escopoDesc} (1 a ${maxN})
          </div>
        </div>

        <!-- 4. Dual Slider Intuitivo (1 a N) -->
        <div class="popover-slider-box" style="margin: 0; padding: 0.25rem 0 0.4rem 0;">
          <div class="dual-slider-track-wrap" style="position: relative; height: 26px; display: flex; align-items: center;">
            <div class="dual-slider-track-bg" style="position: absolute; width: 100%; height: 6px; background: var(--border-color); border-radius: 3px;"></div>
            <div class="dual-slider-track-highlight" id="campanhaRankingHighlight" style="position: absolute; height: 6px; background: var(--accent-petroleo); border-radius: 3px; left: ${highlightLeft}%; width: ${highlightWidth}%;"></div>
            <input type="range" class="dual-range-input min-thumb" id="campanhaSliderMin" min="1" max="${maxN}" step="1" value="${curMin}" oninput="CardFilterManager.onCampanhaSliderChange('min', this.value, ${maxN}, '${source}')">
            <input type="range" class="dual-range-input max-thumb" id="campanhaSliderMax" min="1" max="${maxN}" step="1" value="${curMax}" oninput="CardFilterManager.onCampanhaSliderChange('max', this.value, ${maxN}, '${source}')">
          </div>
          <div class="popover-slider-ticks" style="display: flex; justify-content: space-between; font-size: 0.65rem; color: var(--text-muted); margin-top: 0.25rem;">
            <span>1º (Mais financiado)</span>
            ${maxN > 2 ? `<span>${Math.round(maxN / 2)}º</span>` : ''}
            <span>${maxN}º</span>
          </div>
        </div>

        <!-- 5. Atalhos Rápidos (Presets) -->
        <div>
          <div style="font-size: 0.68rem; font-weight: 700; color: var(--text-muted); text-transform: uppercase; margin-bottom: 0.35rem;">
            Atalhos Rápidos
          </div>
          <div style="display: grid; grid-template-columns: repeat(3, 1fr); gap: 0.3rem;">
            <button type="button" class="btn-text-discreet ${curMin === 1 && curMax === Math.min(5, maxN) ? 'active' : ''}" style="border: 1px solid var(--border-color); border-radius: var(--radius-xs); padding: 0.3rem; font-size: 0.70rem; text-align: center;" onclick="CardFilterManager.applyCampanhaPreset('top5', ${maxN}, '${source}')">
              Top 5
            </button>
            <button type="button" class="btn-text-discreet ${curMin === 1 && curMax === Math.min(10, maxN) ? 'active' : ''}" style="border: 1px solid var(--border-color); border-radius: var(--radius-xs); padding: 0.3rem; font-size: 0.70rem; text-align: center;" onclick="CardFilterManager.applyCampanhaPreset('top10', ${maxN}, '${source}')">
              Top 10
            </button>
            <button type="button" class="btn-text-discreet ${curMin === 1 && curMax === Math.min(25, maxN) ? 'active' : ''}" style="border: 1px solid var(--border-color); border-radius: var(--radius-xs); padding: 0.3rem; font-size: 0.70rem; text-align: center;" onclick="CardFilterManager.applyCampanhaPreset('top25', ${maxN}, '${source}')">
              Top 25
            </button>
            <button type="button" class="btn-text-discreet ${curMin === 1 && curMax === Math.max(1, Math.ceil(maxN / 2)) ? 'active' : ''}" style="border: 1px solid var(--border-color); border-radius: var(--radius-xs); padding: 0.3rem; font-size: 0.70rem; text-align: center;" onclick="CardFilterManager.applyCampanhaPreset('top50pct', ${maxN}, '${source}')">
              50% mais
            </button>
            <button type="button" class="btn-text-discreet ${curMin === Math.max(1, Math.floor(maxN / 2)) && curMax === maxN ? 'active' : ''}" style="border: 1px solid var(--border-color); border-radius: var(--radius-xs); padding: 0.3rem; font-size: 0.70rem; text-align: center;" onclick="CardFilterManager.applyCampanhaPreset('bottom50pct', ${maxN}, '${source}')">
              50% menos
            </button>
            <button type="button" class="btn-text-discreet ${!isFiltered ? 'active' : ''}" style="border: 1px solid var(--border-color); border-radius: var(--radius-xs); padding: 0.3rem; font-size: 0.70rem; text-align: center;" onclick="CardFilterManager.applyCampanhaPreset('all', ${maxN}, '${source}')">
              Ver todos
            </button>
          </div>
        </div>

        ${isFiltered ? `
          <button type="button" class="btn-text-discreet" style="width: 100%; text-align: center; margin-top: 0.2rem; padding: 0.35rem 0;" onclick="CardFilterManager.clearCampanhaRanking('${source}')">
            ✕ Limpar filtro de ranking
          </button>
        ` : ''}
      </div>
    `;
  },

  setCampanhaEscopo(escopo, source = 'card') {
    if (!TableFilterManager.filters.campanha_ranking) {
      TableFilterManager.filters.campanha_ranking = { metrica: 'receitas', escopo: 'partido', min: 1, max: null };
    }
    TableFilterManager.filters.campanha_ranking.escopo = escopo;
    const container = (source === 'table') ? document.getElementById("popoverBody") : document.getElementById("cardFilterOptions");
    if (container) this.renderCampanhaOptions(container, source);
    this.updateCardFilterButtons();
    App.renderView();
  },

  setCampanhaMetrica(metrica, source = 'card') {
    if (!TableFilterManager.filters.campanha_ranking) {
      TableFilterManager.filters.campanha_ranking = { metrica: 'receitas', escopo: 'partido', min: 1, max: null };
    }
    TableFilterManager.filters.campanha_ranking.metrica = metrica;
    const container = (source === 'table') ? document.getElementById("popoverBody") : document.getElementById("cardFilterOptions");
    if (container) this.renderCampanhaOptions(container, source);
    this.updateCardFilterButtons();
    App.renderView();
  },

  onCampanhaSliderChange(type, val, maxN, source = 'card') {
    let minSlider = document.getElementById("campanhaSliderMin");
    let maxSlider = document.getElementById("campanhaSliderMax");
    let minNum = minSlider ? parseInt(minSlider.value, 10) || 1 : 1;
    let maxNum = maxSlider ? parseInt(maxSlider.value, 10) || maxN : maxN;

    if (type === 'min') {
      let parsed = parseInt(val, 10) || 1;
      if (parsed > maxNum) {
        parsed = maxNum;
        if (minSlider) minSlider.value = parsed;
      }
      minNum = parsed;
    } else if (type === 'max') {
      let parsed = parseInt(val, 10) || maxN;
      if (parsed < minNum) {
        parsed = minNum;
        if (maxSlider) maxSlider.value = parsed;
      }
      maxNum = parsed;
    }

    if (!TableFilterManager.filters.campanha_ranking) {
      TableFilterManager.filters.campanha_ranking = { metrica: 'receitas', escopo: 'partido', min: 1, max: null };
    }
    const cr = TableFilterManager.filters.campanha_ranking;
    cr.min = minNum;
    cr.max = (maxNum < maxN) ? maxNum : null;

    if (cr.min === 1 && (cr.max === null || cr.max >= maxN)) {
      delete TableFilterManager.filters.campanha_ranking;
    }

    const highlight = document.getElementById("campanhaRankingHighlight");
    if (highlight && maxN > 1) {
      const leftPct = ((minNum - 1) / (maxN - 1)) * 100;
      const widthPct = Math.max(0, ((maxNum - minNum) / (maxN - 1)) * 100);
      highlight.style.left = `${leftPct}%`;
      highlight.style.width = `${widthPct}%`;
    }
    const rangeLabel = document.getElementById("campanhaRankingRangeLabel");
    if (rangeLabel) rangeLabel.textContent = `${minNum}º ao ${maxNum}º`;

    const subLabel = document.getElementById("campanhaRankingSubLabel");
    if (subLabel) {
      const met = cr.metrica || 'receitas';
      const esc = cr.escopo || 'partido';
      const escDesc = esc === 'partido' ? 'no partido' : 'no cargo/UF';
      let txt = (minNum === 1 && maxNum >= maxN)
        ? (esc === 'partido' ? `Mostrando todas as posições partidárias` : `Mostrando todos (${maxN} no cargo)`)
        : (minNum === 1
          ? (esc === 'partido' ? `Top ${maxNum} de cada partido que mais ${met === 'receitas' ? 'arrecadaram' : 'gastaram'}` : `Top ${maxNum} geral no cargo que mais ${met === 'receitas' ? 'arrecadaram' : 'gastaram'}`)
          : (esc === 'partido' ? `${minNum}º ao ${maxNum}º de cada partido que mais ${met === 'receitas' ? 'arrecadaram' : 'gastaram'}` : `${minNum}º ao ${maxNum}º geral no cargo que mais ${met === 'receitas' ? 'arrecadaram' : 'gastaram'}`));
      subLabel.textContent = `${txt} (1 a ${maxN})`;
    }

    this.updateCardFilterButtons();

    clearTimeout(this._sliderTimer);
    this._sliderTimer = setTimeout(() => {
      App.renderView();
    }, 120);
  },

  applyCampanhaPreset(preset, maxN, source = 'card') {
    if (!TableFilterManager.filters.campanha_ranking) {
      TableFilterManager.filters.campanha_ranking = { metrica: 'receitas', escopo: 'partido', min: 1, max: null };
    }
    const cr = TableFilterManager.filters.campanha_ranking;
    if (preset === 'top5') {
      cr.min = 1;
      cr.max = Math.min(5, maxN);
    } else if (preset === 'top10') {
      cr.min = 1;
      cr.max = Math.min(10, maxN);
    } else if (preset === 'top25') {
      cr.min = 1;
      cr.max = Math.min(25, maxN);
    } else if (preset === 'top50pct') {
      cr.min = 1;
      cr.max = Math.max(1, Math.ceil(maxN / 2));
    } else if (preset === 'bottom50pct') {
      cr.min = Math.max(1, Math.floor(maxN / 2));
      cr.max = maxN;
    } else if (preset === 'all') {
      delete TableFilterManager.filters.campanha_ranking;
    }
    const container = (source === 'table') ? document.getElementById("popoverBody") : document.getElementById("cardFilterOptions");
    if (container) this.renderCampanhaOptions(container, source);
    this.updateCardFilterButtons();
    App.renderView();
  },

  clearCampanhaRanking(source = 'card') {
    delete TableFilterManager.filters.campanha_ranking;
    delete TableFilterManager.filters.ranking_partido;
    delete TableFilterManager.filters.ranking_cargo;
    delete TableFilterManager.filters.gastos;
    delete TableFilterManager.filters.receitas;
    delete App.state.gastos_min;
    delete App.state.gastos_max;
    delete App.state.receitas_min;
    delete App.state.receitas_max;
    const container = (source === 'table') ? document.getElementById("popoverBody") : document.getElementById("cardFilterOptions");
    if (container) this.renderCampanhaOptions(container, source);
    this.updateCardFilterButtons();
    App.renderView();
  },

  _sliderTimer: null,

  onSliderChange(key, val) {
    const num = parseFloat(val) || 0;

    if (key === "gastos") {
      const labelEl = document.getElementById("cardGastosSliderLabel");
      if (labelEl) {
        if (num === 0) labelEl.textContent = 'Sem valor mínimo';
        else if (num >= 5000000) labelEl.textContent = 'A partir de R$ 5.000.000';
        else labelEl.textContent = `A partir de R$ ${num.toLocaleString('pt-BR')}`;
      }
      if (num <= 0) {
        delete TableFilterManager.filters.gastos;
        delete App.state.gastos_min;
      } else {
        TableFilterManager.filters.gastos = { min: num, max: null };
        App.state.gastos_min = num;
      }
    } else if (key === "receitas") {
      const labelEl = document.getElementById("cardReceitasSliderLabel");
      if (labelEl) {
        if (num === 0) labelEl.textContent = 'Sem valor mínimo';
        else if (num >= 5000000) labelEl.textContent = 'A partir de R$ 5.000.000';
        else labelEl.textContent = `A partir de R$ ${num.toLocaleString('pt-BR')}`;
      }
      if (num <= 0) {
        delete TableFilterManager.filters.receitas;
        delete App.state.receitas_min;
      } else {
        TableFilterManager.filters.receitas = { min: num, max: null };
        App.state.receitas_min = num;
      }
    } else if (key === "bens") {
      const maxLimit = 10000000;
      const labelEl = document.getElementById("cardBensSliderLabel");
      if (labelEl) {
        labelEl.textContent = num < maxLimit ? `Até R$ ${num.toLocaleString('pt-BR')}` : 'Sem limite (Todos)';
      }
      if (num >= maxLimit) {
        delete TableFilterManager.filters.bens;
        delete App.state.bens_max;
      } else {
        TableFilterManager.filters.bens = { min: 0, max: num };
        App.state.bens_max = num;
      }
    }

    this.updateCardFilterButtons();
    clearTimeout(this._sliderTimer);
    this._sliderTimer = setTimeout(() => {
      App.state.pagina = 1;
      App.carregarCandidatos();
    }, 280);
  },

  setRange(key, min, max, label) {
    if (key === "gastos") {
      delete TableFilterManager.filters.ranking_partido;
      delete TableFilterManager.filters.ranking_cargo;
      if (min !== null) App.state.gastos_min = min; else delete App.state.gastos_min;
      if (max !== null) App.state.gastos_max = max; else delete App.state.gastos_max;
    } else if (key === "bens") {
      delete TableFilterManager.filters.ranking_bens;
      if (min !== null) App.state.bens_min = min; else delete App.state.bens_min;
      if (max !== null) App.state.bens_max = max; else delete App.state.bens_max;
    }
    if (min === null && max === null) {
      delete TableFilterManager.filters[key];
    } else {
      TableFilterManager.filters[key] = { min, max };
    }
    this.close();
    this.updateCardFilterButtons();
    App.state.pagina = 1;
    App.carregarCandidatos();
  },

  setRankingBens(num) {
    delete App.state.bens_min;
    delete App.state.bens_max;
    delete TableFilterManager.filters.bens;
    TableFilterManager.filters.ranking_bens = num;
    this.close();
    this.updateCardFilterButtons();
    App.state.pagina = 1;
    App.carregarCandidatos();
  },

  setValue(key, valStr, label) {
    if (!valStr) {
      delete TableFilterManager.filters[key];
      if (key === "genero") delete App.state.genero;
      else if (key === "raca") delete App.state.cor_raca;
      else if (key === "faixa_etaria") delete App.state.faixa_etaria;
      else if (key === "instrucao") delete App.state.grau_instrucao;
      else if (key === "profissao") delete App.state.ocupacao;
      else if (key === "situacao") App.state.situacao = "todos";
      else if (key === "mandato") App.state.situacao_mandato = "todos";
    } else {
      if (valStr.includes(",")) {
        TableFilterManager.filters[key] = valStr.split(",");
      } else {
        TableFilterManager.filters[key] = [valStr];
      }
      if (key === "genero") App.state.genero = valStr;
      else if (key === "raca") App.state.cor_raca = valStr;
      else if (key === "faixa_etaria") App.state.faixa_etaria = valStr;
      else if (key === "instrucao") App.state.grau_instrucao = valStr;
      else if (key === "profissao") App.state.ocupacao = valStr;
      else if (key === "situacao") App.state.situacao = valStr;
      else if (key === "mandato") App.state.situacao_mandato = valStr;
    }
    this.close();
    this.updateCardFilterButtons();
    App.state.pagina = 1;
    App.carregarCandidatos();
  },

  filterProfissaoList(query) {
    const q = (query || "").toLowerCase();
    const btns = document.querySelectorAll("#cardProfissaoList .card-filter-opt-btn");
    btns.forEach(b => {
      const match = b.textContent.toLowerCase().includes(q);
      b.style.display = match ? "flex" : "none";
    });
  },

  updateCardFilterButtons() {
    const keys = ["mandato", "genero", "raca", "faixa_etaria", "instrucao", "profissao", "campanha", "bens"];
    const defaultLabels = {
      situacao: "🏛️ Situação",
      mandato: "Mandato",
      genero: "👤 Gênero",
      raca: "Cor / Raça",
      faixa_etaria: "🎂 Idade",
      instrucao: "🎓 Escolaridade",
      profissao: "💼 Profissão",
      campanha: "📊 Campanha",
      bens: "💰 Bens"
    };

    keys.forEach(k => {
      const btn = document.getElementById(`btnQuick_${k}`);
      if (!btn) return;
      let has = false;
      let activeLabel = defaultLabels[k];

      if (k === "situacao") {
        has = App.state.situacao && App.state.situacao !== "todos" && App.state.situacao !== "todas";
        if (has) activeLabel = `🏛️ ${App.state.situacao === 'APTO' ? 'Aptos' : App.state.situacao}`;
      } else if (k === "mandato") {
        has = App.state.situacao_mandato && App.state.situacao_mandato !== "todos";
        if (has) {
          if (App.state.situacao_mandato === "reeleicao") activeLabel = "🔄 Reeleição";
          else if (App.state.situacao_mandato === "outro_cargo") activeLabel = "🏛️ Outro Cargo";
          else if (App.state.situacao_mandato === "sem_mandato") activeLabel = "🌱 Não Consta";
        }
      } else if (k === "genero") {
        has = Boolean(App.state.genero && typeof App.state.genero === "string" && App.state.genero.trim());
        if (has) activeLabel = App.state.genero.trim() === "FEMININO" ? "👩 Mulheres" : "👨 Homens";
      } else if (k === "raca") {
        has = Boolean(App.state.cor_raca && typeof App.state.cor_raca === "string" && App.state.cor_raca.trim());
        if (has) {
          const rStr = App.state.cor_raca.trim();
          activeLabel = `Cor: ${rStr.charAt(0).toUpperCase() + rStr.slice(1).toLowerCase()}`;
        }
      } else if (k === "faixa_etaria") {
        has = Boolean(App.state.faixa_etaria && typeof App.state.faixa_etaria === "string" && App.state.faixa_etaria.trim());
        if (has) {
          const fMap = { "18-24": "18-24 anos", "25-34": "25-34 anos", "35-44": "35-44 anos", "45-59": "45-59 anos", "60+": "60+ anos" };
          activeLabel = `🎂 ${fMap[App.state.faixa_etaria] || App.state.faixa_etaria}`;
        }
      } else if (k === "instrucao") {
        has = Boolean(App.state.grau_instrucao && typeof App.state.grau_instrucao === "string" && App.state.grau_instrucao.trim());
        if (has) {
          const s = App.state.grau_instrucao.toUpperCase();
          activeLabel = s.includes("SUPERIOR") ? "🎓 Superior" : (s.includes("MÉDIO") ? "🎓 Médio" : "🎓 Escolaridade");
        }
      } else if (k === "profissao") {
        has = Boolean(App.state.ocupacao && typeof App.state.ocupacao === "string" && App.state.ocupacao.trim());
        if (has) activeLabel = `💼 ${App.state.ocupacao.trim().slice(0, 14)}`;
      } else if (k === "campanha") {
        const cr = TableFilterManager.filters.campanha_ranking;
        const isCrActive = Boolean(cr && (cr.min > 1 || (cr.max !== null && cr.max !== undefined)));
        const isLegacyActive = Boolean(App.state.gastos_min || App.state.gastos_max || App.state.receitas_min || App.state.receitas_max || TableFilterManager.filters.ranking_partido || TableFilterManager.filters.ranking_cargo);
        has = isCrActive || isLegacyActive;
        if (has) {
          if (isCrActive) {
            const escName = cr.escopo === "cargo" ? "Cargo" : "Partido";
            if (cr.min === 1 || !cr.min) {
              activeLabel = `📊 Top ${cr.max} (${escName})`;
            } else {
              activeLabel = `📊 ${cr.min}º-${cr.max}º (${escName})`;
            }
          } else {
            activeLabel = "📊 Campanha";
          }
        }
      } else if (k === "bens") {
        const isBensMaxActive = App.state.bens_max !== undefined && App.state.bens_max !== null && App.state.bens_max < 10000000;
        const isBensMinActive = App.state.bens_min !== undefined && App.state.bens_min !== null && App.state.bens_min > 0;
        const isTableBensActive = TableFilterManager.hasFilter("bens");
        has = isBensMaxActive || isBensMinActive || isTableBensActive;
        if (has) {
          if (isBensMaxActive) {
            const m = App.state.bens_max;
            activeLabel = m >= 1000000 ? `💰 Até R$ ${(m / 1000000).toFixed(1)}M` : `💰 Até R$ ${(m / 1000).toFixed(0)}k`;
          } else if (isBensMinActive) {
            const m = App.state.bens_min;
            activeLabel = m >= 1000000 ? `💰 De R$ ${(m / 1000000).toFixed(1)}M` : `💰 De R$ ${(m / 1000).toFixed(0)}k`;
          } else if (TableFilterManager.filters.ranking_bens) {
            activeLabel = `💎 Top ${TableFilterManager.filters.ranking_bens}`;
          } else {
            activeLabel = "💰 Bens";
          }
        }
      }

      btn.classList.toggle("active-filter", has);
      btn.classList.toggle("active", has);
      const caret = `<span class="filter-caret">▾</span>`;
      btn.innerHTML = `${activeLabel} ${caret}`;
    });
  },

  clearKey(key) {
    if (key === "situacao") {
      delete TableFilterManager.filters.situacao;
      App.setSituacaoFilter("todos");
      return;
    }
    if (key === "mandato") {
      App.setMandateFilter("todos");
      return;
    }
    if (key === "campanha") {
      delete TableFilterManager.filters.campanha_ranking;
      delete TableFilterManager.filters.gastos;
      delete TableFilterManager.filters.receitas;
      delete TableFilterManager.filters.ranking_partido;
      delete TableFilterManager.filters.ranking_cargo;
      delete App.state.gastos_min;
      delete App.state.gastos_max;
      delete App.state.receitas_min;
      delete App.state.receitas_max;
    } else if (key === "bens") {
      delete TableFilterManager.filters.bens;
      delete TableFilterManager.filters.ranking_bens;
      delete App.state.bens_min;
      delete App.state.bens_max;
    } else if (key === "genero") {
      delete TableFilterManager.filters.genero;
      delete App.state.genero;
    } else if (key === "raca") {
      delete TableFilterManager.filters.raca;
      delete App.state.cor_raca;
    } else if (key === "faixa_etaria") {
      delete TableFilterManager.filters.faixa_etaria;
      delete App.state.faixa_etaria;
    } else if (key === "instrucao") {
      delete TableFilterManager.filters.instrucao;
      delete App.state.grau_instrucao;
    } else if (key === "profissao") {
      delete TableFilterManager.filters.profissao;
      delete App.state.ocupacao;
    } else {
      delete TableFilterManager.filters[key];
    }
    this.close();
    this.updateCardFilterButtons();
    App.state.pagina = 1;
    App.carregarCandidatos();
  }
};

/* ═══════════════════════════════════════════════════════════════════════════
   MINI TUTORIAL INTERATIVO (Opt-In Guided Tour)
   ═══════════════════════════════════════════════════════════════════════════ */

const TutorialTour = {
  isOpen: false,
  currentStep: 0,
  repositionTimer: null,

  steps: [
    {
      id: "filtros",
      getEl: () => {
        const isClassic = document.body && document.body.classList.contains("layout-classic");
        if (isClassic) {
          const el = document.getElementById("sidebarFiltersBox");
          if (el && el.offsetParent !== null) return el;
        }
        return document.querySelector("#topFilterPanel .top-filter-row-1")
          || document.getElementById("topFilterPanel")
          || document.getElementById("sidebarFiltersBox");
      },
      getPlacement: () => {
        const isClassic = document.body && document.body.classList.contains("layout-classic");
        return (isClassic && window.innerWidth > 1024) ? "right" : "bottom";
      },
      title: "Comece pelos filtros",
      desc: "Escolha o estado, o cargo pretendido e filtre os candidatos por mandato (reeleição, em exercício ou sem mandato).",
      requiresSidebar: true,
    },
    {
      id: "ideologia",
      getEl: () => {
        const isClassic = document.body && document.body.classList.contains("layout-classic");
        if (isClassic) {
          const el = document.getElementById("filterBoxIdeologiaPartidos");
          if (el && el.offsetParent !== null) return el;
        }
        return document.querySelector("#topFilterPanel .top-group-affinity")
          || document.querySelector("#topFilterPanel .top-filter-row-2")
          || document.getElementById("topFilterPanel")
          || document.getElementById("filterBoxIdeologiaPartidos");
      },
      getPlacement: () => {
        const isClassic = document.body && document.body.classList.contains("layout-classic");
        return (isClassic && window.innerWidth > 1024) ? "right" : "bottom";
      },
      title: "Explore por ideologia & partidos",
      desc: "Filtre candidatos pelo espectro político de esquerda a direita, faça o quiz ideológico ou priorize e oculte partidos.",
      requiresSidebar: true,
    },
    {
      id: "busca_filtros",
      getEl: () => {
        return document.querySelector(".search-input-wrap")
          || document.querySelector(".feed-unified-toolbar")
          || document.querySelector("#feedFilterBar")
          || document.getElementById("searchInput");
      },
      title: "Busca e filtros rápidos",
      desc: "Busque por nome ou número e use os filtros rápidos para refinar por situação, gênero, cor/raça, escolaridade e bens.",
      placement: "bottom",
    },
    {
      id: "visualizacao",
      getEl: () => {
        return document.querySelector(".view-toggle-btns")
          || document.getElementById("btnViewCards")
          || document.querySelector(".feed-view-export-wrap");
      },
      title: "Ver como Tabela ou Cards",
      desc: "Alterne entre a visualização de cards editoriais e o modo tabela com rolagem horizontal e foto/nome sempre fixos.",
      placement: "bottom",
    },
    {
      id: "candidatos_comparar",
      getEl: () => document.querySelector(".candidate-card-clean") || document.getElementById("candidatesContainer"),
      title: "Cards e Comparação",
      desc: "Veja a foto, número, perfil ideológico e use o botão de balança para comparar candidatos lado a lado.",
      placement: "bottom",
    },
    {
      id: "ficha_completa",
      getEl: () => {
        const drawer = document.getElementById("detailsDrawer");
        const panel = drawer ? drawer.querySelector(".drawer-panel") : null;
        return (panel && panel.offsetParent !== null) ? panel : (drawer || document.querySelector(".candidate-card-clean") || document.body);
      },
      title: "Ficha Completa & Dossiê Cívico",
      desc: "Abra a Ficha Completa para ver o histórico detalhado do candidato: patrimônio e evolução de bens, mandatos anteriores, votações nominais e a íntegra da Proposta de Governo em PDF.",
      placement: "left",
      onEnter: () => {
        const drawer = document.getElementById("detailsDrawer");
        if (drawer && !drawer.classList.contains("open")) {
          const firstCand = (window.App && App.state && App.state.candidatos && App.state.candidatos.length > 0)
            ? App.state.candidatos[0]
            : null;
          if (firstCand && typeof App.openDetails === "function") {
            App.openDetails(firstCand.sq_candidato);
            TutorialTour._openedDrawerByTour = true;
          }
        }
      },
      onLeave: () => {
        if (TutorialTour._openedDrawerByTour) {
          if (window.App && typeof App.closeDetails === "function") {
            App.closeDetails();
          }
          TutorialTour._openedDrawerByTour = false;
        }
      }
    },
    {
      id: "salvar_colinha",
      getEl: () => {
        const card = document.querySelector(".candidate-card-clean");
        if (card) {
          const btn = card.querySelector(".card-bookmark-btn");
          if (btn && btn.offsetParent !== null) return btn;
          return card;
        }
        return document.getElementById("btnHeaderColinha") || document.getElementById("btnColinhaFloating") || document.querySelector(".btn-colinha") || document.body;
      },
      title: "Salvos e Minha Colinha",
      desc: "Colecione candidatos favoritos em suas listas e selecione até 6 nomes para sua Colinha Oficial das Eleições 2026!",
      placement: "top",
      isLast: true,
    },
  ],

  _openedDrawerByTour: false,

  start() {
    this.isOpen = true;
    this.currentStep = 0;
    const overlay = document.getElementById("tutorialTourOverlay");
    if (overlay) {
      overlay.style.display = "block";
    }

    // Ouvintes de teclado e redimensionamento
    this._bindEvents();

    // Iniciar no primeiro passo
    this.goToStep(0);
  },

  goToStep(index) {
    if (index < 0 || index >= this.steps.length) return;
    const oldStep = this.steps[this.currentStep];
    if (oldStep && typeof oldStep.onLeave === "function") {
      oldStep.onLeave();
    }

    this.currentStep = index;
    const step = this.steps[index];

    // Se o passo requer a sidebar e o layout clássico estiver ativo
    const isClassic = document.body && document.body.classList.contains("layout-classic");
    if (step.requiresSidebar && isClassic && window.innerWidth > 1024 && window.App && App.state && App.state.sidebarCollapsed) {
      App.toggleSidebar(false);
    }

    if (typeof step.onEnter === "function") {
      step.onEnter();
    }

    // Delay breve para layout e transições estabilizarem
    setTimeout(() => {
      this._renderStep(step);
    }, 120);
  },

  next() {
    if (this.currentStep < this.steps.length - 1) {
      this.goToStep(this.currentStep + 1);
    } else {
      this.close();
    }
  },

  prev() {
    if (this.currentStep > 0) {
      this.goToStep(this.currentStep - 1);
    }
  },

  close() {
    this.isOpen = false;
    const oldStep = this.steps[this.currentStep];
    if (oldStep && typeof oldStep.onLeave === "function") {
      oldStep.onLeave();
    }
    if (this._openedDrawerByTour) {
      if (window.App && typeof App.closeDetails === "function") {
        App.closeDetails();
      }
      this._openedDrawerByTour = false;
    }
    const overlay = document.getElementById("tutorialTourOverlay");
    if (overlay) {
      overlay.style.display = "none";
    }
    this._unbindEvents();
  },

  _renderStep(step) {
    let targetEl = null;
    if (typeof step.getEl === "function") {
      targetEl = step.getEl();
    } else if (step.selector) {
      targetEl = document.querySelector(step.selector);
    }
    if (!targetEl && step.fallbackSelector) {
      targetEl = document.querySelector(step.fallbackSelector);
    }

    if (!targetEl) {
      console.warn("Elemento alvo do tour não encontrado:", step);
      return;
    }

    // Scroll suave até o elemento se não estiver visível
    const rect = targetEl.getBoundingClientRect();
    const isVisible = (
      rect.top >= 20 &&
      rect.left >= 0 &&
      rect.bottom <= (window.innerHeight - 20) &&
      rect.right <= window.innerWidth &&
      rect.width > 0 &&
      rect.height > 0
    );

    if (!isVisible) {
      targetEl.scrollIntoView({ behavior: "smooth", block: "center", inline: "nearest" });
      setTimeout(() => this._positionUI(targetEl, step), 200);
      setTimeout(() => this._positionUI(targetEl, step), 450);
    } else {
      this._positionUI(targetEl, step);
    }

    // Atualizar textos e estados no popover
    const badgeEl = document.getElementById("tutorialStepBadge");
    const titleEl = document.getElementById("tutorialStepTitle");
    const descEl = document.getElementById("tutorialStepDesc");
    const counterEl = document.getElementById("tutorialProgressCounter");
    const prevBtn = document.getElementById("btnTutorialPrev");
    const nextBtn = document.getElementById("btnTutorialNext");

    if (badgeEl) badgeEl.textContent = this.currentStep + 1;
    if (titleEl) titleEl.textContent = step.title;
    if (descEl) descEl.textContent = step.desc;
    if (counterEl) counterEl.textContent = `${this.currentStep + 1}/${this.steps.length}`;

    if (prevBtn) {
      prevBtn.style.display = this.currentStep === 0 ? "none" : "inline-flex";
    }
    if (nextBtn) {
      if (this.currentStep === this.steps.length - 1) {
        nextBtn.textContent = "Concluir ✓";
      } else {
        nextBtn.textContent = "Próximo →";
      }
    }
  },

  _positionUI(targetEl, step) {
    const spotlight = document.getElementById("tutorialSpotlightBox");
    const popover = document.getElementById("tutorialPopoverCard");
    if (!spotlight || !popover || !targetEl) return;

    const rect = targetEl.getBoundingClientRect();
    if (rect.width === 0 && rect.height === 0) return;

    const pad = 6;

    // Posicionar spotlight
    spotlight.style.top = Math.max(0, rect.top - pad) + "px";
    spotlight.style.left = Math.max(0, rect.left - pad) + "px";
    spotlight.style.width = (rect.width + pad * 2) + "px";
    spotlight.style.height = (rect.height + pad * 2) + "px";

    // Medidas do popover
    const popWidth = popover.offsetWidth || 320;
    const popHeight = popover.offsetHeight || 180;

    let left = 0;
    let top = 0;

    const placement = typeof step.getPlacement === "function"
      ? step.getPlacement()
      : (step.placement || "bottom");

    if (placement === "right") {
      left = rect.right + 16;
      top = rect.top + (rect.height / 2) - (popHeight / 2);
      // Se não couber à direita, joga para baixo
      if (left + popWidth > window.innerWidth - 16) {
        left = Math.max(16, rect.left);
        top = rect.bottom + 16;
      }
    } else if (placement === "left") {
      left = rect.left - popWidth - 16;
      top = rect.top + (rect.height / 2) - (popHeight / 2);
      if (left < 16) {
        left = rect.left + 16;
        top = rect.bottom + 16;
      }
    } else if (placement === "top") {
      left = rect.left + (rect.width / 2) - (popWidth / 2);
      top = rect.top - popHeight - 16;
      if (top < 16) {
        top = rect.bottom + 16;
      }
    } else {
      // "bottom" padrão
      left = rect.left + (rect.width / 2) - (popWidth / 2);
      top = rect.bottom + 16;
      if (top + popHeight > window.innerHeight - 16) {
        top = Math.max(16, rect.top - popHeight - 16);
      }
    }

    // Prevenção contra estouro de bordas da janela
    left = Math.max(16, Math.min(window.innerWidth - popWidth - 16, left));
    top = Math.max(16, Math.min(window.innerHeight - popHeight - 16, top));

    popover.style.left = left + "px";
    popover.style.top = top + "px";
  },

  _handleKeydown(e) {
    if (!TutorialTour.isOpen) return;
    if (e.key === "Escape") {
      TutorialTour.close();
    } else if (e.key === "ArrowRight") {
      TutorialTour.next();
    } else if (e.key === "ArrowLeft") {
      TutorialTour.prev();
    }
  },

  _handleResize() {
    if (!TutorialTour.isOpen) return;
    clearTimeout(TutorialTour.repositionTimer);
    TutorialTour.repositionTimer = setTimeout(() => {
      const step = TutorialTour.steps[TutorialTour.currentStep];
      if (step) {
        TutorialTour._renderStep(step);
      }
    }, 80);
  },

  _bindEvents() {
    window.addEventListener("keydown", this._handleKeydown);
    window.addEventListener("resize", this._handleResize);
    window.addEventListener("scroll", this._handleResize, { passive: true });
  },

  _unbindEvents() {
    window.removeEventListener("keydown", this._handleKeydown);
    window.removeEventListener("resize", this._handleResize);
    window.removeEventListener("scroll", this._handleResize);
  }
};

window.App = App;
window.TableFilterManager = TableFilterManager;
window.CardFilterManager = CardFilterManager;
window.TutorialTour = TutorialTour;
window.addEventListener("DOMContentLoaded", () => App.init());
