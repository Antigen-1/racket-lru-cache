#lang scribble/manual
@require[@for-label[racket-lru-cache
                    racket/base]]

@title{racket-lru-cache}
@author{zhanghao}

@defmodule[racket-lru-cache]

@defform[(lambda/lru-cache kw-formals body ...+)
         #:grammar [(kw-formals (arg ...) (arg ...+ . rest-id) rest-id)
                    (arg id (code-line) [id default-expr] (code-line) keyword id (code-line) keyword [id default-expr])]]
