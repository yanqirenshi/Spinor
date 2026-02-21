import { useEffect, useState, useCallback } from 'react'
import { useParams } from 'react-router-dom'
import ReactMarkdown from 'react-markdown'
import remarkGfm from 'remark-gfm'
import hljs from 'highlight.js'
import 'highlight.js/styles/github.css'

export default function MarkdownViewer() {
  const { '*': path } = useParams<{ '*': string }>()
  const [content, setContent] = useState<string>('')
  const [error, setError] = useState<string | null>(null)
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    if (!path) return

    setLoading(true)
    setError(null)

    fetch(`/${path}.md`)
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

  if (loading) return <p>Loading...</p>
  if (error) return <p className="error">{error}</p>

  return (
    <article className="markdown-body">
      <ReactMarkdown
        remarkPlugins={[remarkGfm]}
        components={{ code: renderCode }}
      >
        {content}
      </ReactMarkdown>
    </article>
  )
}
