import { useState } from 'react'
import { Link, useLocation } from 'react-router-dom'

interface NavItem {
  label: string
  to?: string
  items?: NavItem[]
}

const sections: NavItem[] = [
  { label: 'Introduction', to: '/docs/introduction' },
  {
    label: 'Installation',
    to: '/docs/installation',
    items: [
      { label: '共通の前提条件', to: '/docs/installation/prerequisites' },
      { label: 'Linux / WSL2', to: '/docs/installation/linux' },
      { label: 'Windows 11', to: '/docs/installation/windows-msys2' },
      { label: 'トラブルシューティング', to: '/docs/installation/troubleshooting' },
    ],
  },
  {
    label: 'Native Compilation (AOT)',
    to: '/docs/aot',
    items: [
      { label: 'build (C 経由)', to: '/docs/aot/build-c' },
      { label: 'build-llvm (LLVM IR)', to: '/docs/aot/build-llvm' },
      { label: 'WASM ビルド (Emscripten)', to: '/docs/aot/build-wasm' },
    ],
  },
  {
    label: 'Usage',
    to: '/docs/usage',
    items: [
      { label: 'Editor Setup', to: '/docs/usage/editor' },
      { label: 'Cookbook (逆引きレシピ)', to: '/docs/usage/cookbook' },
      { label: 'AI-Native Workflow', to: '/docs/usage/ai_workflow' },
    ],
  },
  // CLHS-style Syntax Categories
  {
    label: 'Syntax and Evaluation',
    items: [
      { label: 'Atoms', to: '/docs/syntax/atoms' },
      { label: 'Evaluation', to: '/docs/syntax/evaluation' },
    ],
  },
  {
    label: 'Types and Classes',
    items: [
      { label: 'Type System', to: '/docs/syntax/type-system' },
      { label: 'Algebraic Types', to: '/docs/syntax/data-types' },
    ],
  },
  {
    label: 'Data and Control Flow',
    items: [
      { label: 'Definitions', to: '/docs/syntax/definitions' },
      { label: 'Control Flow', to: '/docs/syntax/control-flow' },
      { label: 'Iteration', to: '/docs/syntax/iteration' },
    ],
  },
  {
    label: 'Conditions',
    items: [
      { label: 'Conditions & Errors', to: '/docs/syntax/conditions' },
    ],
  },
  {
    label: 'Built-in Data Types',
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
    label: 'System and Environment',
    items: [
      { label: 'Packages & Modules', to: '/docs/syntax/packages' },
      { label: 'Files', to: '/docs/syntax/files' },
      { label: 'Environment', to: '/docs/syntax/environment' },
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
      { label: 'Linear Types & Ownership', to: '/docs/syntax/ownership' },
      { label: 'Region-based Memory', to: '/docs/syntax/regions' },
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
]

interface NavItemProps {
  item: NavItem
  depth?: number
  onLinkClick?: () => void
}

function NavItemComponent({ item, depth = 0, onLinkClick }: NavItemProps) {
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
              onClick={onLinkClick}
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
              <NavItemComponent key={child.to || index} item={child} depth={depth + 1} onLinkClick={onLinkClick} />
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
        onClick={onLinkClick}
      >
        {item.label}
      </Link>
    </li>
  )
}

interface SidebarProps {
  isOpen?: boolean
  onClose?: () => void
}

export default function Sidebar({ isOpen, onClose }: SidebarProps) {
  return (
    <nav className={`sidebar ${isOpen ? 'sidebar-open' : ''}`}>
      <h2 className="sidebar-title">
        <Link to="/" className="sidebar-title-link" onClick={onClose}>Spinor</Link>
      </h2>
      <ul className="sidebar-nav">
        {sections.map((item, index) => (
          <NavItemComponent key={item.to || index} item={item} onLinkClick={onClose} />
        ))}
      </ul>
    </nav>
  )
}
