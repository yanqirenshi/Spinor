import { useEffect, useState, useCallback } from 'react'
import { useParams, useNavigate } from 'react-router-dom'
import ReactMarkdown from 'react-markdown'
import remarkGfm from 'remark-gfm'
import hljs from 'highlight.js'
import 'highlight.js/styles/github.css'

export default function MarkdownViewer() {
  const { '*': path } = useParams<{ '*': string }>()
  const navigate = useNavigate()
  const [content, setContent] = useState<string>('')
  const [error, setError] = useState<string | null>(null)
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    if (!path) return

    setLoading(true)
    setError(null)

    fetch(`${import.meta.env.BASE_URL}docs/${path}.md`)
      .then((res) => {
        if (!res.ok) throw new Error(`Document not found: ${path}`)
        return res.text()
      })
      .then((text) => {
        setContent(text)
        setLoading(false)
      })
      .catch((err) => {
        setError(err.message)
        setLoading(false)
      })
  }, [path])

  // Highlight code blocks after content renders
  useEffect(() => {
    if (!loading && content) {
      document.querySelectorAll('pre code').forEach((block) => {
        hljs.highlightElement(block as HTMLElement)
      })
    }
  }, [loading, content])

  const renderCode = useCallback(
    (props: React.HTMLAttributes<HTMLElement> & { className?: string; children?: React.ReactNode }) => {
      const { className, children, ...rest } = props
      const match = /language-(\w+)/.exec(className || '')
      if (match) {
        return (
          <code className={className} {...rest}>
            {children}
          </code>
        )
      }
      return (
        <code className={className} {...rest}>
          {children}
        </code>
      )
    },
    []
  )

  // Intercept internal links and use React Router navigation
  const renderLink = useCallback(
    (props: React.AnchorHTMLAttributes<HTMLAnchorElement> & { children?: React.ReactNode }) => {
      const { href, children, ...rest } = props
      const isExternal = href && (href.startsWith('http://') || href.startsWith('https://'))
      if (href && !isExternal) {
        const target = href.startsWith('/') ? href : `/docs/${href}`
        return (
          <a
            href={href}
            {...rest}
            onClick={(e) => {
              e.preventDefault()
              navigate(target)
            }}
          >
            {children}
          </a>
        )
      }
      return <a href={href} {...rest}>{children}</a>
    },
    [navigate]
  )

  if (loading) return <p>Loading...</p>
  if (error) return <p className="error">{error}</p>

  return (
    <article className="markdown-body">
      <ReactMarkdown
        remarkPlugins={[remarkGfm]}
        components={{ code: renderCode, a: renderLink }}
      >
        {content}
      </ReactMarkdown>
    </article>
  )
}
