(() => {
  "use strict";
  const labels = JSON.parse(document.getElementById("em-catalog").textContent);
  const key = "exmort.ui.language:" + location.pathname;
  const root = document.documentElement;
  function preferredLanguage() {
    try { return localStorage.getItem(key) === "zh-CN" ? "zh-CN" : "en"; }
    catch (_) { return "en"; }
  }
  root.lang = preferredLanguage();

  function translate(english) {
    return root.lang === "zh-CN" && Object.hasOwn(labels, english) ? labels[english] : english;
  }

  // Shiny/selectize own the option objects. Only update display labels on
  // marked selects, never the values sent to the model or an uploaded sheet.
  function refreshSelect(el) {
    // On a remembered Chinese preference, let Selectize read the original
    // English options first. Shiny emits shiny:bound after it initializes.
    if (!el.selectize && el.parentElement.querySelector("script[data-for]")) return;
    const dynamicFilter = el.closest("[data-em-select]");
    const allowed = dynamicFilter?.dataset.emSelect.split("|");
    const labelFor = text => allowed && !allowed.includes(text) ? text : translate(text);
    const control = el.selectize;
    if (control) {
      for (const [value, data] of Object.entries(control.options)) {
        const field = control.settings.labelField;
        const english = data.emEnglish ?? data[field];
        const label = labelFor(english);
        if (label !== data[field]) {
          control.updateOption(value, { ...data, emEnglish: english, [field]: label });
        }
      }
      const placeholder = control.settings.placeholder;
      if (placeholder) {
        el.dataset.emPlaceholder ??= placeholder;
        control.settings.placeholder = translate(el.dataset.emPlaceholder);
        control.updatePlaceholder();
      }
    }
    for (const opt of el.options) {
      opt.dataset.emEnglish ??= opt.text;
      const label = labelFor(opt.dataset.emEnglish);
      if (opt.text !== label) opt.text = label;
    }
  }

  function refresh(scope) {
    const find = selector => [
      ...(scope.matches?.(selector) ? [scope] : []), ...scope.querySelectorAll(selector)
    ];
    for (const el of find("select[data-em-choices], [data-em-select] select")) refreshSelect(el);
    for (const attr of ["title", "placeholder", "aria-label"]) {
      for (const el of find("[data-em-" + attr + "]")) {
        el.setAttribute(attr, translate(el.getAttribute("data-em-" + attr)));
      }
    }
  }

  const englishTitle = () => document.querySelector("#navbar a[data-value='tab1'] [lang='en']")?.textContent;
  function applyLanguage(lang) {
    root.lang = lang === "zh-CN" ? lang : "en";
    const picker = document.getElementById("ui_language");
    if (picker) picker.value = root.lang;
    if (englishTitle()) document.title = translate(englishTitle());
    refresh(document);
  }
  document.addEventListener("change", event => {
    if (event.target.id !== "ui_language") return;
    applyLanguage(event.target.value);
    try { localStorage.setItem(key, root.lang); } catch (_) { /* storage may be disabled */ }
  });
  document.addEventListener("DOMContentLoaded", () => applyLanguage(root.lang));
  $(document).on("shiny:bound", event => {
    if (event.target.id === "ui_language") event.target.value = root.lang;
    if (event.target.tagName === "SELECT" && event.target.closest("[data-em-select], [data-em-choices]")) {
      refreshSelect(event.target);
    }
  });
  $(document).on("shiny:connected", () => {
    applyLanguage(root.lang);
    Shiny.setInputValue("ui_language", root.lang);
  });
})();
