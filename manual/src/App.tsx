import { useState } from 'react'
import { Routes, Route, useLocation } from 'react-router-dom'
import Sidebar from './components/Sidebar'
import HomePage from './components/HomePage'
import MarkdownViewer from './components/MarkdownViewer'
import './App.css'

function App() {
  const location = useLocation()
  const isLandingPage = location.pathname === '/'
  const [isMobileMenuOpen, setIsMobileMenuOpen] = useState(false)

  const handleMenuClose = () => {
    setIsMobileMenuOpen(false)
  }

  return (
    <div className={`app-layout ${isLandingPage ? 'no-sidebar' : ''}`}>
      {!isLandingPage && (
        <>
          <button
            className="mobile-menu-button"
            onClick={() => setIsMobileMenuOpen(!isMobileMenuOpen)}
            aria-label={isMobileMenuOpen ? 'Close menu' : 'Open menu'}
          >
            {isMobileMenuOpen ? '\u2715' : '\u2630'}
          </button>
          {isMobileMenuOpen && (
            <div className="mobile-overlay" onClick={handleMenuClose} />
          )}
          <Sidebar isOpen={isMobileMenuOpen} onClose={handleMenuClose} />
        </>
      )}
      <main className="main-content">
        <Routes>
          <Route path="/" element={<HomePage />} />
          <Route path="/docs/*" element={<MarkdownViewer />} />
        </Routes>
      </main>
    </div>
  )
}

export default App
