import { Routes, Route, useLocation } from 'react-router-dom'
import Sidebar from './components/Sidebar'
import HomePage from './components/HomePage'
import MarkdownViewer from './components/MarkdownViewer'
import './App.css'

function App() {
  const location = useLocation()
  const isLandingPage = location.pathname === '/'

  return (
    <div className={`app-layout ${isLandingPage ? 'no-sidebar' : ''}`}>
      {!isLandingPage && <Sidebar />}
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
