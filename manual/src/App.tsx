import { Routes, Route } from 'react-router-dom'
import Sidebar from './components/Sidebar'
import HomePage from './components/HomePage'
import MarkdownViewer from './components/MarkdownViewer'
import './App.css'

function DocsLayout() {
  return (
    <div className="app-layout">
      <Sidebar />
      <main className="main-content">
        <MarkdownViewer />
      </main>
    </div>
  )
}

function App() {
  return (
    <Routes>
      <Route path="/" element={<HomePage />} />
      <Route path="/docs/*" element={<DocsLayout />} />
    </Routes>
  )
}

export default App
