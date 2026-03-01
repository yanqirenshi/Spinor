import { useState } from 'react'
import { Link, useLocation } from 'react-router-dom'

interface NavItem {
  label: string
  to?: string
  items?: NavItem[]
}

const sections: NavItem[] = [
  { label: 'Home', to: '/' },
  { label: 'Introduction', to: '/docs/introduction' },
  { label: 'Cookbook (逆引きレシピ)', to: '/docs/cookbook' },
  {
    label: 'Syntax',
    to: '/docs/syntax',
    items: [
      { label: 'Atoms', to: '/docs/syntax/atoms' },
      { label: 'Type System', to: '/docs/syntax/type-system' },
      { label: 'Evaluation', to: '/docs/syntax/evaluation' },
      { label: 'Definitions', to: '/docs/syntax/definitions' },
      { label: 'Control Flow', to: '/docs/syntax/control-flow' },
      { label: 'Conditions & Errors', to: '/docs/syntax/conditions' },
      { label: 'Iteration', to: '/docs/syntax/iteration' },
      { label: 'Algebraic Types', to: '/docs/syntax/data-types' },
    ],
  },
  {
    label: 'Data Types',
    items: [
      { label: 'Symbols', to: '/docs/syntax/symbols' },
      { label: 'Numbers', to: '/docs/syntax/numbers' },
      { label: 'Characters', to: '/docs/syntax/characters' },
      { label: 'Strings', to: '/docs/syntax/strings' },
      { label: 'Conses', to: '/docs/syntax/conses' },
      { label: 'Arrays & Matrices', to: '/docs/syntax/arrays' },
    ],
  },
  {
    label: 'System',
    items: [
      { label: 'Packages & Modules', to: '/docs/syntax/packages' },
      { label: 'Files', to: '/docs/syntax/files' },
      { label: 'Environment', to: '/docs/syntax/environment' },
    ],
  },
  { label: 'Build Guide', to: '/docs/build' },
  { label: 'Editor Setup', to: '/docs/emacs_setup' },
  { label: 'API Reference', to: '/docs/api-index' },
  { label: 'Architecture & Internals', to: '/docs/architecture' },
  {
    label: 'AI Integration',
    items: [
      { label: 'AI-Native Workflow', to: '/docs/ai_workflow' },
    ],
  },
  {
    label: 'Experimental',
    items: [
      { label: 'Linear Types & Ownership', to: '/docs/syntax/ownership' },
      { label: 'Region-based Memory', to: '/docs/syntax/regions' },
    ],
  },
]

interface NavItemProps {
  item: NavItem
  depth?: number
}

function NavItemComponent({ item, depth = 0 }: NavItemProps) {
  const location = useLocation()
  const [isExpanded, setIsExpanded] = useState(() => {
    // Auto-expand if current path matches any child or the item itself
    if (item.items) {
      const matchesChild = item.items.some((child) => child.to && location.pathname === child.to)
      const matchesSelf = item.to && location.pathname === item.to
      return matchesChild || matchesSelf
    }
    return false
  })

  const hasChildren = item.items && item.items.length > 0
  const isActive = item.to && location.pathname === item.to
  const paddingLeft = depth * 16

  if (hasChildren) {
    return (
      <li>
        <div className="sidebar-category-wrapper">
          <button
            className="sidebar-category-toggle"
            onClick={() => setIsExpanded(!isExpanded)}
            aria-label={isExpanded ? 'Collapse' : 'Expand'}
          >
            <span className="sidebar-category-icon">{isExpanded ? '▼' : '▶'}</span>
          </button>
          {item.to ? (
            <Link
              to={item.to}
              className={`sidebar-category-link ${isActive ? 'active' : ''}`}
              style={{ paddingLeft: `${paddingLeft}px` }}
            >
              {item.label}
            </Link>
          ) : (
            <button
              className={`sidebar-category ${isExpanded ? 'expanded' : ''}`}
              onClick={() => setIsExpanded(!isExpanded)}
              style={{ paddingLeft: `${paddingLeft}px` }}
            >
              {item.label}
            </button>
          )}
        </div>
        {isExpanded && (
          <ul className="sidebar-subnav">
            {item.items!.map((child, index) => (
              <NavItemComponent key={child.to || index} item={child} depth={depth + 1} />
            ))}
          </ul>
        )}
      </li>
    )
  }

  return (
    <li>
      <Link
        to={item.to!}
        className={`sidebar-link ${isActive ? 'active' : ''}`}
        style={{ paddingLeft: `${paddingLeft + 12}px` }}
      >
        {item.label}
      </Link>
    </li>
  )
}

export default function Sidebar() {
  return (
    <nav className="sidebar">
      <h2 className="sidebar-title">Spinor Manual</h2>
      <ul className="sidebar-nav">
        {sections.map((item, index) => (
          <NavItemComponent key={item.to || index} item={item} />
        ))}
      </ul>
    </nav>
  )
}
