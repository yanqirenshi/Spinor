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

    const baseUrl = import.meta.env.BASE_URL
    const primaryUrl = `${baseUrl}docs/${path}.md`
    const indexUrl = `${baseUrl}docs/${path}/index.md`

    // Helper to check if response is valid markdown (not HTML fallback)
    const isValidMarkdown = (text: string): boolean => {
      const trimmed = text.trim()
      return !trimmed.startsWith('<!') && !trimmed.startsWith('<html')
    }

    const fetchMarkdown = async (url: string): Promise<string> => {
      const res = await fetch(url)
      if (!res.ok) throw new Error('Not found')
      const text = await res.text()
      if (!isValidMarkdown(text)) throw new Error('Not markdown')
      return text
    }

    fetchMarkdown(primaryUrl)
      .catch(() => fetchMarkdown(indexUrl))
      .then((text) => {
        setContent(text)
        setLoading(false)
      })
      .catch(() => {
        setError(`Document not found: ${path}`)
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
