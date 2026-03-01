# AI-Native Development with Spinor

Spinor is designed from the ground up to work seamlessly with AI coding assistants like Claude Code, Gemini CLI, and other LLM-powered tools. This guide explains how to leverage Spinor's AI-native features for a revolutionary development experience.

## Why AI-Native?

### The Perfect Match: Lisp Syntax + Static Types

Spinor combines two properties that make it uniquely suited for AI-assisted development:

1. **S-expression syntax**: The uniform, parenthesized structure of Lisp is trivially parseable and unambiguous. AI models can generate and manipulate code without getting confused by complex syntax rules.

2. **Static type system**: Hindley-Milner type inference catches errors at compile time. When an AI writes incorrect code, the type checker immediately identifies the problem—no need to run the code and discover bugs at runtime.

This combination enables a powerful workflow:

> **Humans express intent through types. AI implements the code. The compiler verifies correctness.**

### The Traditional Problem

With dynamically typed languages, AI-generated code might look correct but fail at runtime with cryptic errors. With complex syntax languages, AI might generate syntactically invalid code that requires manual fixing.

Spinor eliminates both problems: the simple syntax prevents parsing errors, and the type system catches logical errors before execution.

---

## Getting Started with `spinor init`

### Creating an AI-Ready Project

```bash
spinor init my-project
cd my-project
```

This creates:

```
my-project/
  src/
    main.spin          # Entry point
  test/
    test.spin          # Test suite
  CLAUDE.md            # AI context file
  .agents/             # Agent Teams directory
    TEAMS.md           # Multi-agent protocol
    tasks/
      todo/
      in-progress/
      review/
      done/
    mailboxes/
  .gitignore
```

### The `CLAUDE.md` File

The `CLAUDE.md` file is crucial for AI assistants. When an AI opens your project, it reads this file to understand:

- How to build and run the project
- The language syntax and type system
- Coding conventions and best practices
- Available commands for verification

**Key insight**: The AI doesn't need to learn Spinor from scratch—`CLAUDE.md` provides all the context it needs.

---

## The Self-Healing Loop

### Machine-Readable Error Output

The `spinor check` command performs type checking without execution:

```bash
spinor check src/main.spin
```

For AI consumption, use the `--json` flag:

```bash
spinor check --json src/main.spin
```

### Success Response

```json
{
  "status": "success",
  "command": "check",
  "message": "Type check passed. 5 expressions analyzed."
}
```

### Error Response

```json
{
  "status": "error",
  "errors": [
    {
      "file": "src/main.spin",
      "line": 12,
      "col": 5,
      "message": "Type mismatch: expected Int, got Str",
      "code": "TYPE_ERROR"
    }
  ]
}
```

### The Loop in Action

When an AI assistant modifies your code, it should:

1. **Write** the code changes
2. **Run** `spinor check --json <file>`
3. **Parse** the JSON response
4. **Fix** any errors at the reported line/column
5. **Repeat** until `status: "success"`

This creates an autonomous correction cycle where the AI iteratively improves its output until the type system is satisfied.

### Error Codes

| Code | Description |
|------|-------------|
| `PARSE_ERROR` | Syntax error (unbalanced parens, etc.) |
| `UNDEFINED_SYMBOL` | Reference to undefined variable |
| `TYPE_ERROR` | Type mismatch |
| `ARITY_ERROR` | Wrong number of arguments |
| `ERROR` | Other errors |

---

## MCP Server Integration

### What is MCP?

MCP (Model Context Protocol) allows AI assistants to interact with external tools in real-time. Spinor's MCP server lets AI query the language runtime directly, preventing hallucinations about types and behavior.

### Starting the MCP Server

```bash
spinor mcp
```

The server communicates via JSON-RPC 2.0 over stdio.

### Configuring Claude Code

Add this to your Claude Code MCP settings (`~/.config/claude-code/settings.json` or project-local):

```json
{
  "mcpServers": {
    "spinor": {
      "command": "spinor",
      "args": ["mcp"]
    }
  }
}
```

### Available Tools

#### `eval` - Execute Spinor Code

```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "eval",
    "arguments": { "code": "(+ 1 2)" }
  },
  "id": 1
}
```

Response:
```json
{
  "jsonrpc": "2.0",
  "result": {
    "content": [{ "type": "text", "text": "3" }]
  },
  "id": 1
}
```

#### `typecheck` - Infer Expression Type

```json
{
  "params": {
    "name": "typecheck",
    "arguments": { "code": "(fn (x) (+ x 1))" }
  }
}
```

Response:
```json
{
  "result": {
    "content": [{ "type": "text", "text": "Int -> Int" }]
  }
}
```

#### `macroexpand` - Expand Macros

```json
{
  "params": {
    "name": "macroexpand",
    "arguments": { "code": "(defun foo (x) (+ x 1))" }
  }
}
```

Returns the fully expanded S-expression.

#### `list-symbols` - List Package Symbols

```json
{
  "params": {
    "name": "list-symbols",
    "arguments": { "package": "spinor-user" }
  }
}
```

Lists all symbols in the specified package.

### Benefits

- **No hallucination**: AI can verify types and behavior in real-time
- **Interactive exploration**: AI can experiment with code before writing
- **Accurate documentation**: AI can query actual function signatures

---

## Multi-Agent Development (Agent Teams)

### Overview

For complex projects, multiple AI agents can work in parallel using the Agent Teams workflow. This file-based system coordinates work between a **Team Lead** and multiple **Teammates**.

### Directory Structure

```
.agents/
  TEAMS.md              # Protocol documentation
  tasks/
    todo/               # Unassigned tasks
    in-progress/        # Currently being worked on
    review/             # Completed, awaiting review
    done/               # Finished tasks
  mailboxes/            # Inter-agent messaging
```

### Roles

#### Team Lead

- **Breaks down** project goals into task files in `todo/`
- **Reviews** completed work in `review/`
- Runs `spinor check --json` to verify submissions
- Moves approved tasks to `done/` or rejects with comments

#### Teammate (Developer)

- **Claims** a task by moving it from `todo/` to `in-progress/`
- **Implements** the required functionality
- **Self-verifies** with `spinor check --json`
- **Submits** by moving the task to `review/`

### Task File Format

```markdown
# Task: Implement map function

## Description
Create a map function that applies f to each element of a list.

## Acceptance Criteria
- [ ] Works with empty lists
- [ ] Works with non-empty lists
- [ ] Type checks pass

## Assigned To
teammate-1

## Status
in-progress

## Notes
Using recursion with pattern matching.
```

### Lock Protocol

The file system provides atomic locking:

1. Task files in `todo/` are available for claiming
2. Moving a file to `in-progress/` is atomic (only one agent succeeds)
3. This prevents duplicate work

### Starting a Team Session

1. Launch the Team Lead agent:
   ```bash
   claude --agent-mode
   # "You are the Team Lead. Read .agents/TEAMS.md and manage tasks."
   ```

2. Launch Teammate agents:
   ```bash
   claude --agent-mode
   # "You are a Teammate. Check .agents/tasks/todo/ for available work."
   ```

Each agent operates independently, communicating through the file system.

---

## Best Practices

### 1. Trust the Type System

Don't add runtime checks for things the type system already guarantees. Let the AI write code freely and rely on `spinor check` to catch errors.

### 2. Write Types First

Define your data types and function signatures before implementation:

```lisp
;; Define the shape of data
(data Tree
  (Leaf a)
  (Node (Tree a) a (Tree a)))

;; Define function signature via type annotation (optional but helpful)
(defun tree-sum (t)  ; AI will infer: Tree Int -> Int
  (match t
    ((Leaf x) x)
    ((Node l v r) (+ (tree-sum l) (+ v (tree-sum r))))))
```

### 3. Use MCP for Complex Queries

When unsure about behavior, have the AI use MCP to test hypotheses:

```
AI: "Let me check what type this expression has..."
    -> MCP typecheck: (map (fn (x) (+ x 1)) '(1 2 3))
    -> Result: List Int
```

### 4. Iterate with the Self-Healing Loop

Encourage AI to run `spinor check --json` after every change. This creates a tight feedback loop:

```
Write -> Check -> Fix -> Check -> Success
```

### 5. Leverage Agent Teams for Large Projects

For multi-file changes or feature development:
- Break work into small, independent tasks
- Let agents work in parallel
- Use the review step for quality control

---

## Troubleshooting

### AI generates code that doesn't parse

Check for:
- Unbalanced parentheses
- Missing quotes around strings
- Invalid escape sequences

The `PARSE_ERROR` code in JSON output will indicate the problem location.

### AI can't find functions

Ensure:
- The function is defined before use
- The package is properly imported with `(:use :package-name)`
- The function is exported from its defining package

### MCP server doesn't respond

Check:
- The server is running (`spinor mcp`)
- The JSON-RPC request format is correct
- The tool name is spelled correctly (`eval`, `typecheck`, etc.)

### Agent Teams tasks getting stuck

Verify:
- Task files are valid Markdown
- Agents are reading `TEAMS.md` for the protocol
- No agent is holding a task without working on it

---

## Summary

Spinor's AI-native design enables a new paradigm of software development:

| Feature | Human Role | AI Role |
|---------|------------|---------|
| Type System | Define intent | Generate implementation |
| Self-Healing Loop | Review results | Iterate on errors |
| MCP Server | Configure | Query in real-time |
| Agent Teams | Set goals | Parallel execution |

The future of programming is collaborative. Spinor makes that collaboration seamless.
