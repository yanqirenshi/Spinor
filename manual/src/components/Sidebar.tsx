import { Link } from 'react-router-dom'

const sections = [
  { label: 'Home', to: '/' },
  { label: 'Introduction', to: '/docs/introduction' },
  { label: 'Syntax', to: '/docs/syntax' },
  { label: 'API Reference', to: '/docs/api-index' },
]

export default function Sidebar() {
  return (
    <nav className="sidebar">
      <h2 className="sidebar-title">Spinor Manual</h2>
      <ul className="sidebar-nav">
        {sections.map((s) => (
          <li key={s.to}>
            <Link to={s.to}>{s.label}</Link>
          </li>
        ))}
      </ul>
    </nav>
  )
}
