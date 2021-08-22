# Redwood

A simple imperative programming language built to make small games.

![image](https://user-images.githubusercontent.com/33556084/130372226-45740e5f-b41c-4d28-a041-630672101ca8.png)

## Grammar

```
program ::= stmt*

args ::= ident? (',' ident)*

block ::= '{' stmt* '}'

stmt ::= expr
       | ident '=' expr              Assignment, variable definition
       | 'while' expr block          While loop
       | 'for' ident (',' ident)?    For loop (value only, and key-value version)
         'in' expr block
       | 'return' expr               Return statement
       | 'func' ident '(' args ')'   Function definition
            block

expr ::= number                Float literal
       | string                String lieral
       | array
       | dictionary
       | 'if' expr block       If statement
         ('else if' expr block)?
         ('else' block)?
       | true | false          Boolean literal
       | expr binop expr       Arithmetic
       | ident                 Variable access
       | ident '(' args ')'    Function call
       | expr '.' ident        Dictionary access
       | expr '[' expr ']'     Array/dictionary indexing
       | '!' expr              Boolean not
       | '-' expr
       | 'func' '(' args ')' block  Lambda
       | 'null'

array ::= '[' expr? (',' expr)* ']'              Array literals
       
dictionary ::= '{' keyvalue (',' keyvalue)* '}'  Dictionary literals

keyvalue ::= ident ':' expr

binop ::= '+' | '-' | '*' | '/' | '<' | '>' | '==' | '&' | '|' 
```

## Example code

```
player = null
obstacle = null

func initial() {
    player = sprite('dino.png', width() / 2, height() / 2)
    player.y_vel = 0
    obstacle = sprite('cactus.png', width(), height() / 2)
}


func update() {
    obstacle.x = obstacle.x - 10
    if colliding(player, obstacle) {
        show_text("gameover")
        stop()
    }
    
    if obstacle.x == 0 {
        obstacle.x = width()
    }
        
    if player.y == height() / 2 & key_pressed("space") {
        player.y_vel = 10
    } else if player.y != height() / 2 {
        player.y_vel = player.y_vel - 1
        player.y = player.y + player.y_vel
    }
    if layer.y == height() / 2 {
        player.yvel = 0
    }
}
```
