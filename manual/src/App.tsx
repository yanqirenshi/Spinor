import { Routes, Route } from 'react-router-dom'
import Sidebar from './components/Sidebar'
import HomePage from './components/HomePage'
import MarkdownViewer from './components/MarkdownViewer'
import './App.css'

function App() {
  return (
    <div className="app-layout">
      <Sidebar />
      <main className="main-content">
        <Routes>
          <Route path="/" element={<HomePage />} />
          <Route path="/docs/:slug" element={<MarkdownViewer />} />
        </Routes>
      </main>
    </div>
  )
}

export default App
