# The Broadsheet Design System — Diretrizes de Design Rigoroso & Editorial
> **Projeto:** Em Quem Eu Voto (Eleições 2026)  
> **Filosofia:** Editorial de Imprensa Clássica, Densidade Informativa e Arquitetura Flat (*Anti-Vibecoding*)  
> **Referências Analisadas:** *Financial Times*, *The New York Times*, *The Washington Post*, *The Wall Street Journal*, *The Athletic* e *Politon Data*.

---

## 1. Manifesto: The Broadsheet Digital (Filosofia Anti-Vibecoding)

O design de plataformas contemporâneas geradas por IA costuma cair em armadilhas previsíveis de **"vibecoding"**:
* Gradientes roxos/azuis cósmicos e neons artificiais.
* Sombras difusas gigantescas (`box-shadow: 0 25px 50px rgba(...)`) tentando simular falsa tridimensionalidade flutuante.
* Superfícies translúcidas embaçadas desnecessárias (*glassmorphism / backdrop-blur*).
* Cantos hiperarredondados (*pills* de 24px a 9999px) em todos os botões e cards, ignorando hierarquia.
* Baixa densidade de dados e excesso de "espaço vazio inútil" que oculta informações essenciais.

O **Broadsheet Design System** adota o caminho oposto: **a tradição editorial de imprensa centenária**. Ele transmite **credibilidade institucional, peso investigativo, serenidade visual e altíssima legibilidade analítica**.

### Princípios Inegociáveis
1. **O Primado da Tinta e do Papel**: A tela deve evocar o jornal físico impresso no Modo Claro (tons de papel marfim, areia ou salmão fosco com tinta tipográfica carvão profunda) e uma placa de ardósia/chumbo sólido no Modo Escuro.
2. **Geometria Plana e Fiel (Flat Architecture)**: Quase **zero sombras**. A separação entre elementos se dá por **bordas nítidas de 1px (hairlines)** e contrastes sutis de fundo.
3. **Réguas Tipográficas e Estruturais**: Uso de réguas duplas (*thick/thin rule*), divisores verticais de coluna e fios de corte clássicos de diagramação jornalística.
4. **Bifurcação Tipográfica Rigorosa**:
   * **Fontes Serifadas Clássicas**: exclusivas para Manchetes, Títulos de Seção, Nomes de Candidatos e Frases de Destaque.
   * **Fontes Sans-serif Neutras**: exclusivas para Dados Numéricos, Tabelas, Rótulos, Filtros, Metadados e Menus.
   * **Numerais Tabulares (`tabular-nums`)**: todo valor financeiro, contagem de votos, bens e porcentagens usam largura fixa para alinhamento vertical matemático perfeito.

---

## 2. Decomposição Técnica dos Veículos de Referência

Análise direta extraída dos prints inspecionados:

| Veículo | Característica Marcante | Cores Chave Extraídas | Tipografia Típica | Tratamento Estrutural |
| :--- | :--- | :--- | :--- | :--- |
| **Financial Times (FT)** | Fundo salmão icônico, claret/borgonha nobre, seriedade de mercado | Papel Salmão `#FFF1E5`, Superfície `#F2DFCE`, Claret `#990F3D`, Carvão `#1E242B` | Serif Financier Display + Sans Metric | Linhas pretas de topo de 3px a 4px, metadados em caixa alta (kickers), zero cantos arredondados |
| **The New York Times (NYT)** | Pureza de tinta preta sobre papel marfim, máxima autoridade | Papel Marfim `#FBF9F5`, Branco `#FFFFFF`, Tinta `#121212`, Hairline `#E2E2E2` | Serif Cheltenham/Georgia + Sans Franklin | Divisórias verticais de 1px entre matérias, régua dupla sob o cabeçalho, grid de 4 a 6 colunas |
| **The Washington Post (WaPo)** | Alto contraste elegante no Dark Mode, azul cobalto focado | Chumbo Sólido `#121417`, Superfície `#1A1E24`, Borda `#2D343F`, Azul `#0060F6` | Serif Postoni Display + Sans Franklin | Cards retangulares flat, caixas delimitadas por fios de 1px sem sombra, destaque azul cobalto |
| **The Athletic** | Densidade esportiva/analítica moderna, acento carmim | Fundo Preto `#1A1A1A`, Branco `#FFFFFF`, Cinza Neutro `#757575`, Carmim `#C92C2C` | Serif Condensed Headline + Sans Grotesque | Imagens com proporção fixa (16:9), divisores horizontais discretos, tags em caps |
| **Politon Data (Fronis)** | Plataforma eleitoral com autoridade de consultoria política | Navy Profundo `#0B1320`, Superfície `#131E30`, Borda `#1E2C42`, Dourado `#D8B365` | Serif Clássica Imponente + Sans Inter | Caixas métricas bem definidas, números grandes com rótulos miúdos, acento em ouro velho |

---

## 3. Paleta de Cores Rigorosa

### 3.1 Modo Claro — *The Broadsheet Paper*

Duas opções de papel editorial com contraste testado para acessibilidade WCAG AAA:

#### Opção A: Salmão Editorial (*Financial Times Style*) — Recomendada para máxima distinção
* **Fundo de Página (`--bg-page`)**: `#FFF1E5` (O tom salmão característico do FT).
* **Fundo de Superfície / Card (`--bg-surface`)**: `#F2DFCE` (Tom de papel prensado quente).
* **Fundo de Destaque / Modal (`--bg-surface-elevated`)**: `#FFFFFF` (Branco puro para contraste de leitura).
* **Tinta Primária (`--text-primary`)**: `#121212` (Off-black impresso, nunca preto artificial).
* **Tinta Secundária (`--text-secondary`)**: `#4D4845` (Grafite quente editorial).
* **Tinta Silenciada (`--text-muted`)**: `#78726D` (Cinza sépia para datas e metadados).
* **Borda Hairline (`--border-color`)**: `#DCD3C6` (Linha fina tom papel prensado).
* **Borda Intensa / Régua (`--border-strong`)**: `#121212` (Tinta preta pura para réguas).

#### Opção B: Papel Marfim Nobre (*NYT / WSJ Style*) — Recomendada para sobriedade clássica
* **Fundo de Página (`--bg-page`)**: `#FBF9F5` (Off-white marfim de jornal recém-impresso).
* **Fundo de Superfície / Card (`--bg-surface`)**: `#FFFFFF` (Branco clássico delimitado por bordas).
* **Fundo Sutil (`--bg-surface-subtle`)**: `#F3EFEA` (Areia pálida para fundos de filtros/tabelas).
* **Tinta Primária (`--text-primary`)**: `#111111`.
* **Tinta Secundária (`--text-secondary`)**: `#4B4947`.
* **Tinta Silenciada (`--text-muted`)**: `#76736F`.
* **Borda Hairline (`--border-color`)**: `#E2DDD5`.
* **Borda Intensa / Régua (`--border-strong`)**: `#111111`.

---

### 3.2 Modo Escuro — *Solid Slate & Carbon*

Fundo fosco, sólido, sem gradientes de neon ou roxos cibernéticos:
* **Fundo de Página (`--bg-page`)**: `#121417` (Cinza-chumbo profundo sólido, estilo Washington Post).
* **Fundo de Superfície / Card (`--bg-surface`)**: `#1A1E24` (Superfície mineral plana).
* **Fundo Sutil / Tabela (`--bg-surface-subtle`)**: `#16191F` (Cinza carvão fosco).
* **Fundo Elevado / Popovers (`--bg-surface-elevated`)**: `#222831` (Ardósia sólida).
* **Tinta Primária (`--text-primary`)**: `#F3F4F6` (Branco giz de alta legibilidade).
* **Tinta Secundária (`--text-secondary`)**: `#9CA3AF` (Cinza prateado legível).
* **Tinta Silenciada (`--text-muted`)**: `#6B7280` (Grafite neutro).
* **Borda Hairline (`--border-color`)**: `#28303C` (1px cinza mineral sólido).
* **Borda Intensa / Régua (`--border-strong`)**: `#4B5563` (Linha de destaque).

---

### 3.3 Acentos Cromáticos Editoriais (Ambos os Modos)

Cores sóbrias, pigmentadas, inspiradas em tintas de tipografia:

| Papel Semântico | Nome | Hex Modo Claro | Hex Modo Escuro | Uso Principal |
| :--- | :--- | :--- | :--- | :--- |
| **Kicker / Categoria** | *Claret FT / Vinho* | `#990F3D` | `#E05273` | Tags de categoria, seções em destaque, alertas editoriais |
| **Institucional / Ação** | *Azul Cobalto WaPo* | `#004F9F` | `#3B82F6` | Botões ativos, links, filtros selecionados |
| **Econômico / Bens** | *Verde Petróleo FT* | `#0D7680` | `#10B981` | Valores arrecadados, receitas, deferimentos |
| **Destaque Nobre** | *Areia / Ouro Politon* | `#9E7B30` | `#D8B365` | Selos especiais, estrelas de favoritos, cargos majoritários |
| **Atenção / Advertência** | *Âmbar Queimado* | `#B45309` | `#F59E0B` | Aguardando julgamento, dados parciais |
| **Alerta / Inaptidão** | *Vermelho Carmim* | `#B91C1C` | `#EF4444` | Inaptos, impugnados, cancelamentos |

---

## 4. Sistema Tipográfico

### 4.1 Escolha das Fontes (Google Fonts Gratuitas e de Alta Performance)

```html
<!-- Importação no cabeçalho HTML -->
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Lora:ital,wght@0,400;0,500;0,600;0,700;1,400;1,600&family=Merriweather:ital,wght@0,300;0,400;0,700;0,900;1,300;1,400&family=Plus+Jakarta+Sans:wght@400;500;600;700&family=JetBrains+Mono:wght@400;500;600&display=swap" rel="stylesheet">
```

| Categoria | Família Recomendada | Papel no Projeto | Justificativa Técnica |
| :--- | :--- | :--- | :--- |
| **Serif Primária (Manchetes & Nomes)** | **`Merriweather`** | Títulos de seções, nomes dos candidatos, masthead | Desenhada especificamente para legibilidade em telas digitais, serifas robustas que não somem em telas de baixa resolução. |
| **Serif Alternativa (Elegância Literária)** | **`Lora`** | Subtítulos, introduções analíticas, citações | Proporções clássicas inspiradas na tipografia renascentista, curvas elegantes e curvas itálicas fluidas. |
| **Serif de Grande Impacto** | **`Playfair Display`** | Grandes números de votação, percentuais de vitória, hero headlines | Alto contraste entre traços grossos e finos, evoca o *Vogue* e a primeira página de jornais de domingo. |
| **Sans-serif (UI & Dados)** | **`Plus Jakarta Sans`** ou **`Inter`** | Menus, botões, filtros, dados tabulares, metadados | Neutra, desenho limpo, proporções geométricas precisas sem atrair atenção excessiva. |
| **Monoespacada (Valores R$)** | **`JetBrains Mono`** | Valores financeiros (R$), CNPJ, datas, horas de atualização | Altura-de-x confortável, clareza absoluta na distinção entre 0 e O, 1 e l. |

---

### 4.2 Escala Tipográfica Modular

```css
/* Escala de tamanhos e pesos */
--font-size-kicker:   0.6875rem; /* 11px - Caixa alta, rastreamento largo */
--font-size-caption:  0.75rem;   /* 12px - Metadados, notas de rodapé */
--font-size-body-sm:  0.8125rem; /* 13px - Tabelas, rótulos de campos */
--font-size-body:     0.875rem;  /* 14px - Texto corrido, filtros */
--font-size-lead:     1.00rem;   /* 16px - Subtítulos editoriais */
--font-size-h3:       1.125rem;  /* 18px - Nome do candidato no card */
--font-size-h2:       1.375rem;  /* 22px - Títulos de módulos e seções */
--font-size-h1:       1.75rem;   /* 28px - Manchetes principais */
--font-size-display:  2.50rem;   /* 40px - Masthead do jornal / métricas */
```

### 4.3 O Padrão "Kicker" de Imprensa
O *kicker* é a linha curta em caixa alta que antecede a manchete em grandes jornais:
```css
.editorial-kicker {
  font-family: var(--font-sans);
  font-size: 0.6875rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  color: var(--accent-claret);
  margin-bottom: 0.25rem;
  display: block;
}
```

---

## 5. Geometria, Réguas e Espaçamento (Anti-Vibecoding Rules)

### 5.1 Cantos e Geometria (Border Radius)
* **Regra de Ouro**: Nada de botões estilo pílula ou cartões bolha.
* `border-radius: 0px` para o padrão jornalístico puro (FT/NYT).
* `border-radius: 2px` a `3px` para caixas interativas, inputs e selects (micro-fillet funcional).
* **Proibido**: `border-radius: 12px`, `16px` ou `9999px`.

### 5.2 Réguas Editoriais de Imprensa
As linhas de separação são a assinatura visual dos periódicos:

```css
/* 1. Régua Simples Hairline (1px) */
.rule-hairline {
  border: 0;
  height: 1px;
  background-color: var(--border-color);
  margin: 1rem 0;
}

/* 2. Régua Dupla Clássica (Thick/Thin Rule - Padrão NYT/WSJ) */
.rule-double {
  border: 0;
  border-top: 3px solid var(--border-strong);
  border-bottom: 1px solid var(--border-strong);
  height: 5px;
  margin: 1.25rem 0;
}

/* 3. Régua de Topo de Bloco (Padrão Financial Times) */
.ft-section-header {
  border-top: 3px solid var(--border-strong);
  padding-top: 0.5rem;
  margin-bottom: 1rem;
}
```

### 5.3 Sombras (Shadows)
* **Padrão em Cards, Modais e Botões**: `box-shadow: none !important;`.
* A profundidade visual é dada pela **troca de fundo (`--bg-surface-elevated`)** e pela **borda de 1px sólida (`--border-color`)**.
* Se um popover flutuante sobrepor conteúdo denso:
  `box-shadow: 0 1px 3px rgba(0, 0, 0, 0.12);` (estritamente sutil, sem raio de dispersão exagerado).

---

## 6. Variáveis CSS Prontas para Uso

Cole o bloco abaixo no arquivo principal de estilos (`frontend/css/style.css`):

```css
/* ==========================================================================
   THE BROADSHEET DESIGN SYSTEM — CSS TOKENS
   ========================================================================== */

:root {
  /* Fontes */
  --font-serif: 'Merriweather', Georgia, 'Times New Roman', serif;
  --font-serif-display: 'Lora', 'Playfair Display', serif;
  --font-sans: 'Plus Jakarta Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
  --font-mono: 'JetBrains Mono', Consolas, monospace;

  /* Modo Claro Editorial (The Broadsheet Paper — FT/NYT) */
  --bg-page: #FFF1E5;               /* Papel Salmão FT (ou #FBF9F5 para Marfim NYT) */
  --bg-surface: #F5E5D5;            /* Superfície de card quente */
  --bg-surface-subtle: #EFE0D0;     /* Fundo sutil para cabeçalhos de tabela */
  --bg-surface-elevated: #FFFFFF;   /* Branco puro para modais e leitura */
  --bg-hover: #E8D5C2;              /* Hover nítido e discreto */

  --text-primary: #121212;          /* Tinta preta editorial */
  --text-secondary: #4A4643;        /* Grafite de apoio */
  --text-muted: #7A746E;            /* Metadados e datas */
  
  --border-color: #D8CCC0;          /* Hairline de 1px */
  --border-subtle: #E8DFD5;         /* Linha muito discreta */
  --border-strong: #121212;         /* Linha de corte de imprensa */
  --border-focus: #004F9F;          /* Foco institucional */

  /* Acentos Cromáticos */
  --accent-claret: #990F3D;         /* Vinho Borgonha FT */
  --accent-cobalt: #004F9F;         /* Azul Político */
  --accent-teal: #0D7680;           /* Verde Editorial */
  --accent-gold: #9E7B30;           /* Ouro / Areia */
  --accent-amber: #B45309;          /* Âmbar de Alerta */
  --accent-rose: #B91C1C;           /* Carmim de Inaptidão */

  /* Geometria Flat */
  --radius-none: 0px;
  --radius-sm: 2px;
  --radius-md: 3px;
  --radius-lg: 4px;

  /* Sombras */
  --shadow-flat: none;
  --shadow-floating: 0 1px 3px rgba(0, 0, 0, 0.08);
}

/* Modo Escuro (Solid Slate & Charcoal — WaPo/Politon) */
[data-theme="dark"], body.dark-mode {
  --bg-page: #121417;               /* Cinza-chumbo sólido fosco */
  --bg-surface: #1A1E24;            /* Ardósia pura */
  --bg-surface-subtle: #15181E;     /* Carvão plano */
  --bg-surface-elevated: #222831;   /* Modal / Popover */
  --bg-hover: #29303A;              /* Hover fosco */

  --text-primary: #F3F4F6;          /* Branco giz suave */
  --text-secondary: #9CA3AF;        /* Cinza prata legível */
  --text-muted: #6B7280;            /* Grafite neutro */

  --border-color: #28303C;          /* 1px mineral sólido */
  --border-subtle: #1E242E;         /* Linha sutil */
  --border-strong: #4B5563;         /* Régua de seção */
  --border-focus: #3B82F6;          /* Azul cobalto */

  /* Acentos Cromáticos (Dark) */
  --accent-claret: #E05273;
  --accent-cobalt: #3B82F6;
  --accent-teal: #10B981;
  --accent-gold: #D8B365;
  --accent-amber: #F59E0B;
  --accent-rose: #EF4444;

  --shadow-floating: 0 2px 4px rgba(0, 0, 0, 0.35);
}
```

---

## 7. Equivalência Tailwind CSS (Configuração e Utilitários)

Para projetos utilizando Tailwind CSS (ou para mapear mentalmente as classes nos elementos HTML):

### 7.1 Configuração (`tailwind.config.js`)

```javascript
/** @type {import('tailwindcss').Config} */
module.exports = {
  darkMode: ['class', '[data-theme="dark"]'],
  theme: {
    extend: {
      colors: {
        paper: {
          DEFAULT: '#FFF1E5',       // Salmão FT
          light: '#FBF9F5',         // Marfim NYT
          surface: '#F5E5D5',
          elevated: '#FFFFFF',
          hover: '#E8D5C2',
          border: '#D8CCC0',
        },
        slate: {
          page: '#121417',          // Chumbo WaPo
          surface: '#1A1E24',
          subtle: '#15181E',
          elevated: '#222831',
          hover: '#29303A',
          border: '#28303C',
        },
        ink: {
          primary: '#121212',
          secondary: '#4A4643',
          muted: '#7A746E',
          chalk: '#F3F4F6',
          silver: '#9CA3AF',
        },
        editorial: {
          claret: '#990F3D',
          cobalt: '#004F9F',
          teal: '#0D7680',
          gold: '#D8B365',
          amber: '#B45309',
          crimson: '#B91C1C',
        }
      },
      fontFamily: {
        serif: ['Merriweather', 'Georgia', 'serif'],
        'serif-display': ['Lora', 'Playfair Display', 'serif'],
        sans: ['Plus Jakarta Sans', 'Inter', 'sans-serif'],
        mono: ['JetBrains Mono', 'monospace'],
      },
      borderRadius: {
        none: '0px',
        xs: '2px',
        sm: '3px',
      },
      boxShadow: {
        none: 'none',
        editorial: '0 1px 3px rgba(0, 0, 0, 0.08)',
      }
    },
  },
  plugins: [],
}
```

### 7.2 Classes de Componentes Essenciais

#### 1. Card de Candidato / Artigo
```html
<article class="bg-paper-surface dark:bg-slate-surface border border-paper-border dark:border-slate-border p-4 rounded-none transition-colors hover:bg-paper-hover dark:hover:bg-slate-hover">
  <!-- Kicker de Seção -->
  <span class="block text-[11px] font-bold uppercase tracking-wider text-editorial-claret dark:text-red-400 mb-1">
    Governador • Rio de Janeiro
  </span>
  
  <!-- Manchete / Nome com Serifada Clássica -->
  <h3 class="font-serif text-lg font-bold text-ink-primary dark:text-ink-chalk leading-snug mb-2">
    Nome do Candidato
  </h3>
  
  <!-- Metadados em Sans-serif com Tabular Nums -->
  <div class="flex items-center justify-between text-xs text-ink-secondary dark:text-ink-silver border-t border-paper-border dark:border-slate-border pt-2 mt-3 font-mono tabular-nums">
    <span>Nº 10.123</span>
    <span class="font-semibold text-editorial-teal dark:text-emerald-400">R$ 1.450.000,00</span>
  </div>
</article>
```

#### 2. Barra de Filtros Sóbria
```html
<div class="flex flex-wrap items-center gap-2 py-3 border-b border-paper-border dark:border-slate-border">
  <span class="text-xs font-bold uppercase text-ink-muted tracking-wider mr-2 font-sans">
    Filtrar por:
  </span>
  <button class="px-3 py-1 text-xs font-medium border border-ink-primary dark:border-ink-chalk bg-ink-primary dark:bg-ink-chalk text-paper-elevated dark:text-slate-page rounded-xs">
    Aptos & Julgamento
  </button>
  <button class="px-3 py-1 text-xs font-medium border border-paper-border dark:border-slate-border bg-transparent text-ink-secondary dark:text-ink-silver hover:border-ink-primary dark:hover:border-ink-chalk rounded-xs">
    + Filtros
  </button>
</div>
```

#### 3. Tabela Eleitoral com Alinhamento Numérico
```html
<table class="w-full text-left border-collapse border-t-2 border-b-2 border-ink-primary dark:border-slate-border font-sans">
  <thead>
    <tr class="border-b border-paper-border dark:border-slate-border text-[11px] uppercase tracking-wider text-ink-muted">
      <th class="py-2.5 px-3">Candidato</th>
      <th class="py-2.5 px-3">Partido</th>
      <th class="py-2.5 px-3">Situação</th>
      <th class="py-2.5 px-3 text-right">Receitas</th>
    </tr>
  </thead>
  <tbody class="divide-y divide-paper-border dark:divide-slate-border text-sm">
    <tr class="hover:bg-paper-hover/50 dark:hover:bg-slate-hover/50">
      <td class="py-2.5 px-3 font-serif font-bold text-ink-primary dark:text-ink-chalk">Eduardo Paes</td>
      <td class="py-2.5 px-3 font-sans text-ink-secondary dark:text-ink-silver">PSD</td>
      <td class="py-2.5 px-3 font-sans text-xs">
        <span class="inline-block px-1.5 py-0.5 border border-emerald-700 dark:border-emerald-500 text-emerald-800 dark:text-emerald-400 text-[10px] font-semibold uppercase">Deferido</span>
      </td>
      <td class="py-2.5 px-3 text-right font-mono tabular-nums font-semibold text-ink-primary dark:text-ink-chalk">R$ 5.200.000</td>
    </tr>
  </tbody>
</table>
```

---

## 8. Matriz de Decisão: O Que Fazer vs. O Que Nunca Fazer

| Elemento | ✅ Prática Recomendada (Editorial & Sóbrio) | ❌ Prática Proibida (Vibecoding / AI Cliché) |
| :--- | :--- | :--- |
| **Sombras** | `box-shadow: none;` no layout padrão. Delimitação estrita por bordas de 1px. | Sombras difusas e coloridas (`0 20px 40px rgba(99,102,241,0.2)`). |
| **Bordas** | `1px solid var(--border-color)` nítida e contínua. Fios de corte de 3px para manchetes. | Bordas com degradê neon, brilhos coloridos tipo *glow*. |
| **Cantos** | `0px` ou no máximo `2px` a `3px` (cantos quase retos, aparência de papel cortado). | Cantos de `12px`, `16px` ou formato pílula (`9999px`) em cards. |
| **Fundo Light** | Papel Salmão (`#FFF1E5`) ou Marfim Nobre (`#FBF9F5`). Sensação táctil e analítica. | Branco hospitalar puro(`#FFFFFF`) com cinza claro lavado sem contraste de papel. |
| **Fundo Dark** | Cinza-chumbo sólido e fosco (`#121417` / `#1A1E24`). Alta legibilidade e calma visual. | Fundo OLED preto absoluto (`#000000`) com neon roxo, ciano ou rosa. |
| **Tipografia** | Bifurcação: Serifada clássica nos títulos, Sans-serif nos controles e Mono nos valores. | Usar apenas uma fonte genérica sem serifas (ex.: Arial, Poppins genérico) em tudo. |
| **Números** | `font-variant-numeric: tabular-nums;` com `font-family: var(--font-mono);`. | Números com largura proporcional desalinhando vírgulas e casas decimais. |
| **Hierarquia** | Uso de *Kickers* em caixa alta, sublinhados editoriais e réguas duplas. | Cards cheios de ícones 3D gigantes, ilustrações abstratas ou "emojis" em headers. |
| **Efeitos** | Transições rápidas e secas (`transition: 0.12s ease-in-out`). | Animações saltitantes (*bouncy/elastic*), glassmorphism pesado (*blur*). |

---

## 9. Roteiro de Aplicação no Código Existente

1. **Atualizar `frontend/css/style.css`**:
   - Importar `Merriweather` e `Lora` no topo do arquivo.
   - Definir as novas variáveis de `:root` (Light Mode com fundo papel salmão/marfim) e `[data-theme="dark"]` (Dark Mode cinza-chumbo fosco).
   - Atualizar os seletores `.card-candidato`, `.candidate-name`, `.candidate-cargo` para usar `var(--font-serif)`.
   - Garantir que `.receipts-value`, `.bens-value` e campos de tabela usem `var(--font-mono)` com `tabular-nums`.
2. **Atualizar o Header/Masthead em `frontend/index.html`**:
   - Adicionar uma régua dupla sutil ou linha de corte sob o masthead.
   - Usar títulos serifados de prestígio para a marca "Em Quem Eu Voto 2026".
3. **Validar no Navegador**:
   - Conferir que não há sombras flutuantes nem bordas arredondadas destoantes.
