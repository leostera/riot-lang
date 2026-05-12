# Red Green Syntax Trees - an Overview - Plingdollar

[

# Plingdollar

](/)

# Red Green Syntax Trees - an Overview

24 Nov 2021

It’s no secret that I’m a big fan of a good parser. One of the key parsing techniques that I have tried to focus on in the past is “infallible parsing”. The idea that for any input the parser should _always_ generate some output. This is important to try and give as accurate a set of errors as possible when malformed or partial code is being analysed. An idea that builds on this is _lossless syntax trees_. In this post I’ll discuss a high level overview of a kind of lossless tree, discuss where it came from, and how it can help when parsing and compiling.

The idea of _red / green syntax trees_ initially came from the Roslyn C# compiler. It was adopted by Swift in their `libsyntax` implementation. The Rust Analyzer project built further upon this with the Rowan crate. Rowan extends the model of red-green syntax trees with the idea of _dynamically typed nodes_.

The core of the idea is to represent parsed syntax as two distinct trees. The base tree is the _green_ tree. This represents pieces of concrete syntax, such as the character `1`, the operator `+`, or the expression `1 + 1`. The green tree contains no position information. Each node in the tree is immutable meaning that portions of the tree with the same shape can share nodes.

For example given the `1 + 1` expression we _could_ represent it as the following green tree:

```
           Node BIN_EXPR
                 |
     +-----------+-----------+
     |           |           |
 Token '1'   Token '+'   Token '1'
```

But we could equally share the token’s for `1` and use the following tree:

```
           Node BIN_EXPR
                 |
     +-----------+-----------+
     |           |           |
     |       Token '+'       |
     |                       |
     +------ Token '1' ------+
```

To the outside observer there should be no difference. This sharing of sub-trees is more useful where larger parts of the tree can be shared, such as repeated uses of the same path or calls to the same method.

An simplified definition of a tree might look like the following in F#:

```
type GreenToken =
	{ Kind: SyntaxKind
	  Text: string }
and GreenNode =
	{ Kind: SyntaxKind
	  Width: int
	  Children: Choice<GreenNode, GreenToken> list }
```

In the green tree each node has no specific location, only a width. The width of a token is the width of the text it contains. The with of nodes is cached on the node. It is the sum of the widths of its children.

On its own the green tree represents abstract syntactic elements. To provide a richer view over the top the red tree is the a layer over the top similar to the following:

```
type SyntaxToken =
	{ Offset: int
	  Parent: SyntaxNode option
	  Green: GreenToken }
and SyntaxNode =
	{ Offset: int
	  Parent: SyntaxNode option
	  Green: GreenNode }
```

Each node is extended with two things: a reference to the parent node, and an absolute offset of the start of this node form the beginning of the source text. The offset paired with the widths from the green nodes provides a source range for each element.

Nodes in the red tree all have a reference to a parent node which can be used to traverse the tree and find siblings. A red node fabricates it’s children lazily when a traversal descends into the node. This means we only pay the cost for the portion of the red tree we actually use. Once we no longer reference a portion of the red tree it is free to be garbage collected.

I’m currently experimenting with these ideas for a re-write of the [Feersum scheme Compiler’s parser](https://github.com/iwillspeak/feersum/blob/main/src/Feersum.CompilerServices/Syntax.fs). See the [Firethorn](https://github.com/iwillspeak/firethorn) project for the red / green tree implementation.

For more details on this approach see:

-   [The Rowan crate from Rust Analyzer](https://github.com/rust-analyzer/rowan)
-   [Swift’s `libsyntax`](https://github.com/apple/swift/tree/main/lib/Syntax)
-   [The Roslyn parser](https://github.com/KirillOsenkov/Bliki/wiki/Roslyn-Immutable-Trees)

Another notable mention, although logically distinct, is the [Oil shell’s lossless syntax tree](https://www.oilshell.org/blog/2017/02/11.html).

<iframe style="display: none;"></iframe>

## Embedded Content
