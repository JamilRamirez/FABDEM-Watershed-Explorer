(function () {
  'use strict';

  function injectStyles() {
    if (document.getElementById('morph-hypso-layout-style')) return;

    var style = document.createElement('style');
    style.id = 'morph-hypso-layout-style';
    style.textContent = [
      '.morph-hypso-combined{grid-column:1/-1;border:1px solid #ddd;border-radius:7px;background:#fff;padding:12px;}',
      '.morph-hypso-combined-head{display:flex;justify-content:space-between;align-items:flex-end;gap:12px;margin:0 0 10px 0;padding:0 2px 10px 2px;border-bottom:1px solid #eceff1;}',
      '.morph-hypso-combined-head h4{margin:0;font-size:16px;font-weight:600;}',
      '.morph-hypso-combined-head span{font-size:12px;color:#68757d;text-align:right;}',
      '.morph-hypso-panels{display:grid;grid-template-columns:minmax(0,1.42fr) minmax(360px,.88fr);gap:18px;align-items:stretch;}',
      '.morph-hypso-panels>.morph-card{border:0;border-radius:0;padding:2px 4px 4px 4px;box-shadow:none;min-width:0;}',
      '.morph-hypso-panels>.morph-card:first-child{padding-right:18px;border-right:1px solid #eceff1;}',
      '.morph-hypso-panels .morph-plot-head{margin-bottom:6px;}',
      '#morfometria-hipsometrica,#morfometria-hist_elevacion{height:450px!important;min-height:450px!important;}',
      '.morph-grid>.morph-card,.morph-grid>.shiny-panel-conditional>.morph-card{min-width:0;}',
      '@media(max-width:900px){.morph-hypso-panels{grid-template-columns:1fr;}.morph-hypso-panels>.morph-card:first-child{padding-right:4px;border-right:0;border-bottom:1px solid #eceff1;padding-bottom:14px;}.morph-hypso-combined-head{align-items:flex-start;flex-direction:column;}.morph-hypso-combined-head span{text-align:left;}#morfometria-hipsometrica,#morfometria-hist_elevacion{height:420px!important;min-height:420px!important;}}'
    ].join('');
    document.head.appendChild(style);
  }

  function closestCard(node) {
    if (!node) return null;
    return node.closest('.morph-card');
  }

  function normalizePanelTitles(hypsoCard, elevationCard) {
    var h1 = hypsoCard ? hypsoCard.querySelector('.morph-plot-head h4') : null;
    var h2 = elevationCard ? elevationCard.querySelector('.morph-plot-head h4') : null;
    if (h1) h1.textContent = 'Curva hipsométrica';
    if (h2) h2.textContent = 'Distribución altitudinal';
  }

  function arrangeMorphometry() {
    injectStyles();

    var hypsoPlot = document.getElementById('morfometria-hipsometrica');
    var elevationPlot = document.getElementById('morfometria-hist_elevacion');
    if (!hypsoPlot || !elevationPlot) return;

    var hypsoCard = closestCard(hypsoPlot);
    var elevationCard = closestCard(elevationPlot);
    if (!hypsoCard || !elevationCard) return;

    normalizePanelTitles(hypsoCard, elevationCard);

    var grid = hypsoCard.parentElement;
    if (!grid || !grid.classList.contains('morph-grid')) return;

    var existing = grid.querySelector(':scope > .morph-hypso-combined');
    if (existing) return;

    var combined = document.createElement('div');
    combined.className = 'morph-hypso-combined';

    var head = document.createElement('div');
    head.className = 'morph-hypso-combined-head';

    var title = document.createElement('h4');
    title.textContent = 'Relieve hipsométrico de la cuenca';

    var subtitle = document.createElement('span');
    subtitle.textContent = 'Curva hipsométrica, referencias geomorfológicas y distribución altitudinal por área';

    head.appendChild(title);
    head.appendChild(subtitle);

    var panels = document.createElement('div');
    panels.className = 'morph-hypso-panels';

    grid.insertBefore(combined, hypsoCard);
    combined.appendChild(head);
    combined.appendChild(panels);
    panels.appendChild(hypsoCard);
    panels.appendChild(elevationCard);
  }

  function scheduleArrange() {
    window.setTimeout(arrangeMorphometry, 30);
    window.setTimeout(arrangeMorphometry, 250);
  }

  document.addEventListener('DOMContentLoaded', scheduleArrange);
  document.addEventListener('shiny:connected', scheduleArrange);
  document.addEventListener('shiny:value', scheduleArrange);
  document.addEventListener('shown.bs.tab', scheduleArrange);

  if (window.MutationObserver) {
    var observer = new MutationObserver(function () {
      scheduleArrange();
    });
    observer.observe(document.documentElement, {
      childList: true,
      subtree: true
    });
  }
})();
