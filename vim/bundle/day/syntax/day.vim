if exists("b:current_syntax")
    finish
endif

" syntax region NONE matchgroup=dayTodo start=/\v^TODO:/ skip=/\v\\./ end=/\v(\n\n)|(\nTODO:)|(\nDONE:)|(GOAL:)|(NOTE:)/
" syntax region NONE matchgroup=dayTodo start=/\v^DONE:/ skip=/\v\\./ end=/\v(\n\n)|(\nTODO:)|(\nDONE:)|(GOAL:)|(NOTE:)/
" syntax region NONE matchgroup=dayGoal start=/\v^GOAL:/ skip=/\v\\./ end=/\v(\n\n)|(\nTODO:)|(\nDONE:)|(GOAL:)|(NOTE:)/
" syntax region NONE matchgroup=dayNote start=/\v^NOTE:/ skip=/\v\\./ end=/\v(\n\n)|(\nTODO:)|(\nDONE:)|(GOAL:)|(NOTE:)/
syntax match dayTodo /\v^TODO:/
syntax match dayTodo /\v^DONE:/
syntax match dayGoal /\v^GOAL:/
syntax match dayNote /\v^NOTE:/
syntax match dayIdea /\v^IDEA:/
syntax match dayUpdate /\v^UPDATE:/
highlight link dayGoal Keyword
highlight link dayTodo PreProc
highlight link dayNote Identifier
highlight link dayIdea Type
highlight link dayUpdate Constant

let b:current_syntax = "day"
