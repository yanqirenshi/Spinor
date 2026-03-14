import './HomePage.css'
import { Link } from 'react-router-dom'

export default function HomePage() {
  return (
    <div className="landing">
      {/* Hero */}
      <section className="hero">
        <img src={`${import.meta.env.BASE_URL}assets/spinor-logo.png`} alt="" className="hero-logo" />
        <h1 className="hero-name">Spinor</h1>

        <p className="hero-tagline">Haskell × LLVM で駆動する Lisp コンパイラ</p>
        <p className="hero-sub">
          洗練された Lisp の記述力で<br/>
          OS 直結の爆速ネイティブバイナリを錬成する<br />
          インタプリタの壁を越えた、新しい Lisp 体験
        </p>

        <div className="hero-cta">
          <Link to="/docs/installation" className="btn btn-primary">はじめる</Link>
          <a href="https://github.com/yanqirenshi/Spinor" className="btn btn-secondary">GitHub</a>
        </div>
      </section>

      {/* Features */}
      <section className="features">
        <h2>Features</h2>
        <div className="features-grid">
          <div className="feature-card">
            <div className="feature-icon">⚡</div>
            <h3>Native AOT Compilation</h3>
            <p>Lisp スクリプトを美しい LLVM IR へと変換し、Clang と連携。Windows (.exe) や Linux で直接動作するスタンドアロンな機械語バイナリを生成します。</p>
          </div>
          <div className="feature-card">
            <div className="feature-icon">💎</div>
            <h3>Pure Haskell Core</h3>
            <p>重厚な C++ バインディング (llvm-hs) を排除。Windows の PowerShell でも `cabal build` 一発で構築できる、圧倒的なポータビリティを実現。</p>
          </div>
          <div className="feature-card">
            <div className="feature-icon">🛠️</div>
            <h3>Best-in-class REPL</h3>
            <p>ネイティブビルドされた高速な `spinor.exe` をバックエンドに据え、Emacs と SLY を通じたモダンで快適な REPL 駆動開発をサポートします。</p>
          </div>
          <div className="feature-card">
            <div className="feature-icon">🌐</div>
            <h3>WASM & GPU Ready</h3>
            <p>Emscripten を用いた WASM ビルドによるブラウザ実行や、OpenCL/OpenGL へのバインディングを備え、ハードウェアの限界まで性能を引き出します。</p>
          </div>
        </div>
      </section>

      {/* Code Example */}
      <section className="code-example">
        <h2>See It in Action: LLVM Compilation</h2>
        <div className="code-block">
          <div className="code-header">
            <span className="code-lang">spinor</span>
          </div>
          <pre><code>{`;; 再帰関数も LLVM IR 経由で爆速の機械語に翻訳
(def fib (fn (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2))))))

(print (fib 30))
;; Compile: spinor build-llvm app.spin
;; Execute: ./app.exe => 832040`}</code></pre>
        </div>
      </section>

      {/* Get Started */}
      <section id="get-started" className="get-started">
        <h2>Quick Start</h2>
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
              <h3>Compile to Native (.exe)</h3>
              <div className="code-block code-block-small">
                <pre><code>cabal run spinor -- build-llvm your-file.spin</code></pre>
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
