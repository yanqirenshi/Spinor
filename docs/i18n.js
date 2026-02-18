const translations = {
  en: {
    "nav.roadmap": "Roadmap",
    "hero.tagline": "The Statically-Typed Lisp.",
    "hero.sub": "Lisp syntax with Haskell semantics.<br>Built for safety, clarity, and performance.",
    "hero.cta1": "Get Started",
    "hero.cta2": "View on GitHub",
    "features.title": "Features",
    "features.typing.title": "Static Typing",
    "features.typing.desc": "Catch errors at compile time, not at runtime. Powered by Hindley-Milner type inference for maximum safety with minimal annotations.",
    "features.semantics.title": "Haskell Semantics",
    "features.semantics.desc": "Pure functions, algebraic data types, and pattern matching \u2014 the power of Haskell, expressed in the elegance of Lisp syntax.",
    "features.bootstrap.title": "Self-Hosting",
    "features.bootstrap.desc": "A Haskell kernel implements the core; the standard library is written in Spinor itself. The language extends itself.",
    "features.concurrency.title": "Lightweight Concurrency",
    "features.concurrency.desc": "Built on Haskell\u2019s green threads and MVar primitives for simple, safe concurrent programming.",
    "code.title": "See It in Action",
    "start.title": "Get Started",
    "start.step1": "Clone the Repository",
    "start.step2": "Build from Source",
    "start.step3": "Run the REPL",
  },
  ja: {
    "nav.roadmap": "\u30ED\u30FC\u30C9\u30DE\u30C3\u30D7",
    "hero.tagline": "\u9759\u7684\u578B\u4ED8\u3051 Lisp\u3002",
    "hero.sub": "Lisp \u306E\u69CB\u6587\u3068 Haskell \u306E\u610F\u5473\u8AD6\u3002<br>\u5B89\u5168\u6027\u3001\u660E\u5FEB\u3055\u3001\u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u306E\u305F\u3081\u306B\u3002",
    "hero.cta1": "\u59CB\u3081\u308B",
    "hero.cta2": "GitHub \u3067\u898B\u308B",
    "features.title": "\u7279\u5FB4",
    "features.typing.title": "\u9759\u7684\u578B\u4ED8\u3051",
    "features.typing.desc": "\u30B3\u30F3\u30D1\u30A4\u30EB\u6642\u306B\u30A8\u30E9\u30FC\u3092\u691C\u51FA\u3057\u3001\u5B9F\u884C\u6642\u30A8\u30E9\u30FC\u3092\u672A\u7136\u306B\u9632\u304E\u307E\u3059\u3002Hindley-Milner \u578B\u63A8\u8AD6\u306B\u3088\u308A\u3001\u6700\u5C0F\u9650\u306E\u6CE8\u91C8\u3067\u6700\u5927\u306E\u5B89\u5168\u6027\u3092\u5B9F\u73FE\u3002",
    "features.semantics.title": "Haskell \u306E\u610F\u5473\u8AD6",
    "features.semantics.desc": "\u7D14\u7C8B\u95A2\u6570\u3001\u4EE3\u6570\u7684\u30C7\u30FC\u30BF\u578B\u3001\u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\u30F3\u30B0 \u2014 Haskell \u306E\u5F37\u529B\u306A\u6A5F\u80FD\u3092 Lisp \u306E\u7F8E\u3057\u3044\u69CB\u6587\u3067\u3002",
    "features.bootstrap.title": "\u30BB\u30EB\u30D5\u30DB\u30B9\u30C6\u30A3\u30F3\u30B0",
    "features.bootstrap.desc": "Haskell \u88FD\u30AB\u30FC\u30CD\u30EB\u304C\u30B3\u30A2\u3092\u5B9F\u88C5\u3057\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306F Spinor \u81EA\u8EAB\u3067\u8A18\u8FF0\u3002\u8A00\u8A9E\u304C\u81EA\u3089\u3092\u62E1\u5F35\u3057\u307E\u3059\u3002",
    "features.concurrency.title": "\u8EFD\u91CF\u4E26\u884C\u51E6\u7406",
    "features.concurrency.desc": "Haskell \u306E\u8EFD\u91CF\u30B9\u30EC\u30C3\u30C9\u3068 MVar \u3092\u6D3B\u7528\u3057\u3001\u30B7\u30F3\u30D7\u30EB\u3067\u5B89\u5168\u306A\u4E26\u884C\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u3092\u30B5\u30DD\u30FC\u30C8\u3002",
    "code.title": "\u30B3\u30FC\u30C9\u4F8B",
    "start.title": "\u59CB\u3081\u65B9",
    "start.step1": "\u30EA\u30DD\u30B8\u30C8\u30EA\u3092\u30AF\u30ED\u30FC\u30F3",
    "start.step2": "\u30BD\u30FC\u30B9\u304B\u3089\u30D3\u30EB\u30C9",
    "start.step3": "REPL \u3092\u8D77\u52D5",
  },
};

function setLang(lang) {
  document.documentElement.lang = lang;
  document.querySelectorAll("[data-i18n]").forEach(function (el) {
    var key = el.getAttribute("data-i18n");
    var text = translations[lang][key];
    if (!text) return;
    if (el.hasAttribute("data-i18n-html")) {
      el.innerHTML = text;
    } else {
      el.textContent = text;
    }
  });

  var sel = document.getElementById("lang-select");
  sel.value = lang;

  localStorage.setItem("spinor-lang", lang);
}

document.getElementById("lang-select").addEventListener("change", function () {
  setLang(this.value);
});

// Initialize: use saved preference, or browser language
(function () {
  var saved = localStorage.getItem("spinor-lang");
  if (saved) {
    setLang(saved);
  } else if (navigator.language && navigator.language.startsWith("ja")) {
    setLang("ja");
  }
})();
