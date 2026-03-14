import { useState } from 'react'
import { Link, useLocation } from 'react-router-dom'

interface NavItem {
  label: string
  to?: string
  items?: NavItem[]
}

const sections: NavItem[] = [
  { label: 'Introduction', to: '/docs/introduction' },
  { label: 'Installation', to: '/docs/installation' },
  {
    label: 'Usage',
    to: '/docs/usage',
    items: [
      { label: 'Editor Setup', to: '/docs/usage/editor' },
      { label: 'Cookbook (逆引きレシピ)', to: '/docs/usage/cookbook' },
      { label: 'AI-Native Workflow', to: '/docs/usage/ai_workflow' },
    ],
  },
  {
    label: 'Reference',
    to: '/docs/reference',
    items: [
      { label: 'Syntax Overview', to: '/docs/reference/syntax-overview' },
      { label: 'API Reference', to: '/docs/reference' },
    ],
  },
  {
    label: 'Syntax',
    to: '/docs/reference/syntax-overview',
    items: [
      { label: 'Atoms', to: '/docs/reference/syntax/atoms' },
      { label: 'Type System', to: '/docs/reference/syntax/type-system' },
      { label: 'Evaluation', to: '/docs/reference/syntax/evaluation' },
      { label: 'Definitions', to: '/docs/reference/syntax/definitions' },
      { label: 'Control Flow', to: '/docs/reference/syntax/control-flow' },
      { label: 'Conditions & Errors', to: '/docs/reference/syntax/conditions' },
      { label: 'Iteration', to: '/docs/reference/syntax/iteration' },
      { label: 'Algebraic Types', to: '/docs/reference/syntax/data-types' },
    ],
  },
  {
    label: 'Data Types',
    items: [
      { label: 'Symbols', to: '/docs/reference/syntax/symbols' },
      { label: 'Numbers', to: '/docs/reference/syntax/numbers' },
      { label: 'Characters', to: '/docs/reference/syntax/characters' },
      { label: 'Strings', to: '/docs/reference/syntax/strings' },
      { label: 'Conses', to: '/docs/reference/syntax/conses' },
      { label: 'Arrays & Matrices', to: '/docs/reference/syntax/arrays' },
    ],
  },
  {
    label: 'System',
    items: [
      { label: 'Packages & Modules', to: '/docs/reference/syntax/packages' },
      { label: 'Files', to: '/docs/reference/syntax/files' },
      { label: 'Environment', to: '/docs/reference/syntax/environment' },
    ],
  },
  {
    label: 'Development',
    to: '/docs/development',
    items: [
      { label: 'Architecture & Internals', to: '/docs/development/architecture' },
    ],
  },
  {
    label: 'Experimental',
    items: [
      { label: 'Linear Types & Ownership', to: '/docs/reference/syntax/ownership' },
      { label: 'Region-based Memory', to: '/docs/reference/syntax/regions' },
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
      <h2 className="sidebar-title">
        <Link to="/" className="sidebar-title-link">Spinor</Link>
      </h2>
      <ul className="sidebar-nav">
        {sections.map((item, index) => (
          <NavItemComponent key={item.to || index} item={item} />
        ))}
      </ul>
    </nav>
  )
}
