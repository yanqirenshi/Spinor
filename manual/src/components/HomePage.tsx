import './HomePage.css'

export default function HomePage() {
  return (
    <div className="landing">
      {/* Hero */}
      <section className="hero">
        <img src={`${import.meta.env.BASE_URL}assets/spinor-logo.png`} alt="" className="hero-logo" />
        <h1 className="hero-name">Spinor</h1>
        <p className="hero-tagline">The Statically-Typed Lisp.</p>
        <p className="hero-sub">
          Lisp syntax with Haskell semantics.<br />
          Built for safety, clarity, and performance.
        </p>
        <div className="hero-cta">
          <a href="#get-started" className="btn btn-primary">Get Started</a>
          <a href="https://github.com/yanqirenshi/Spinor" className="btn btn-secondary">View on GitHub</a>
        </div>
      </section>

      {/* Features */}
      <section className="features">
        <h2>Features</h2>
        <div className="features-grid">
          <div className="feature-card">
            <div className="feature-icon">&lambda;</div>
            <h3>Static Typing</h3>
            <p>Catch errors at compile time, not at runtime. Powered by Hindley-Milner type inference for maximum safety with minimal annotations.</p>
          </div>
          <div className="feature-card">
            <div className="feature-icon">&rarr;</div>
            <h3>Haskell Semantics</h3>
            <p>Pure functions, algebraic data types, and pattern matching &mdash; the power of Haskell, expressed in the elegance of Lisp syntax.</p>
          </div>
          <div className="feature-card">
            <div className="feature-icon">&infin;</div>
            <h3>Self-Hosting</h3>
            <p>A Haskell kernel implements the core; the standard library is written in Spinor itself. The language extends itself.</p>
          </div>
          <div className="feature-card">
            <div className="feature-icon">&#x21C6;</div>
            <h3>Lightweight Concurrency</h3>
            <p>Built on Haskell's green threads and MVar primitives for simple, safe concurrent programming.</p>
          </div>
        </div>
      </section>

      {/* Code Example */}
      <section className="code-example">
        <h2>See It in Action</h2>
        <div className="code-block">
          <div className="code-header">
            <span className="code-lang">spinor</span>
          </div>
          <pre><code>{`;; Define a type-safe length function for lists
(defun length ((xs (List a))) : Int
  (match xs
    ('() 0)
    ((cons _ rest) (+ 1 (length rest)))))

;; Calculate the length of a list of integers
(length '(1 2 3 4 5))
;; => 5`}</code></pre>
        </div>
      </section>

      {/* Get Started */}
      <section id="get-started" className="get-started">
        <h2>Get Started</h2>
        <div className="steps">
          <div className="step">
            <div className="step-number">1</div>
            <div className="step-content">
              <h3>Clone the Repository</h3>
              <div className="code-block code-block-small">
                <pre><code>git clone https://github.com/yanqirenshi/Spinor.git{'\n'}cd Spinor</code></pre>
              </div>
            </div>
          </div>
          <div className="step">
            <div className="step-number">2</div>
            <div className="step-content">
              <h3>Build from Source</h3>
              <div className="code-block code-block-small">
                <pre><code>cabal build</code></pre>
              </div>
            </div>
          </div>
          <div className="step">
            <div className="step-number">3</div>
            <div className="step-content">
              <h3>Run the REPL</h3>
              <div className="code-block code-block-small">
                <pre><code>cabal run spinor</code></pre>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* Footer */}
      <footer className="landing-footer">
        <div className="footer-content">
          <p>&copy; 2026 Spinor Project</p>
          <ul className="footer-links">
            <li><a href="https://github.com/yanqirenshi/Spinor">GitHub</a></li>
            <li><a href="https://github.com/yanqirenshi/Spinor/blob/master/TODO.md">Roadmap</a></li>
          </ul>
        </div>
      </footer>
    </div>
  )
}
