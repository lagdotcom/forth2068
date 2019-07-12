# Forth2068

It's forth, but running on a TS2068! Except it only supports the Spectrum right now, haha.

## Word implementation

Y? means it's a Spectrum-specific implementation, so it needs replacing.
Y! means it's implemented but not to specification.
N\* means it's not implemented but is trivial to add in FORTH alone.

| Word        | ?   | Set      | Stack Effect                     | Comment                                       |
| ----------- | --- | -------- | -------------------------------- | --------------------------------------------- |
| `!`         | Y   | CORE     | x a-addr --                      | store value                                   |
| `#>`        | N   | CORE     | xd -- c-addr u                   | finalise pictured numeric output              |
| `#`         | N   | CORE     | ud1 -- ud2                       | pictured numeric output digit                 |
| `#S`        | N   | CORE     | ud1 -- ud2                       | pictured numeric output continuous conversion |
| `'`         | Y   | CORE     | "<spaces\>name" -- xt            | look up xt of word                            |
| `(`         | Y   | CORE     | "ccc<paren\>" --                 | comment                                       |
| `*`         | Y   | CORE     | n1 n2 -- n1×n2                   |                                               |
| `*/`        | N   | CORE     | n1 n2 n3 -- n4                   | push `(n1×n2÷n3)`                             |
| `*/MOD`     | N   | CORE     | n1 n2 n3 -- rem quo              | push `(n1×n2) divmod n3`                      |
| `+`         | Y   | CORE     | n1 n2 -- n1+n2                   |                                               |
| `+!`        | Y   | CORE     | x a-addr --                      | add and store value                           |
| `+LOOP`     | N   | CORE     | n --                             | LOOP except the increment is `n`              |
| `,`         | Y   | CORE     | x --                             | compile                                       |
| `-`         | Y   | CORE     | n1 n2 -- n1-n2                   |                                               |
| `-!`        | Y   | CORE     | x a-addr --                      | subtract and store value                      |
| `-ROT`      | Y   |          | x1 x2 x3 -- x3 x1 x2             |                                               |
| `.`         | Y?  | CORE     | n --                             | show `n`                                      |
| `."`        | Y   | CORE     | --                               | compile LITSTRING                             |
| `.(`        | Y   | CORE-EXT | "ccc<paren\>" --                 | parse and display string                      |
| `.R`        | N   | CORE-EXT | n size --                        | display `n` right-aligned in `size` chars     |
| `.S`        | N   | TOOLS    | --                               | display stack contents                        |
| `/`         | N\* | CORE     | n1 n2 -- n1÷n2                   | `/MOD SWAP DROP`                              |
| `/MOD`      | Y   | CORE     | n1 n2 -- rem quo                 |                                               |
| `0<`        | N\* | CORE     | n -- flag                        | is negative?                                  |
| `0<>`       | Y   | CORE-EXT | n -- flag                        | is not 0?                                     |
| `0<=`       | N\* |          | n -- flag                        | is negative or 0?                             |
| `0=`        | Y   | CORE     | n -- flag                        | is 0?                                         |
| `0>`        | N\* | CORE-EXT | n -- flag                        | is positive?                                  |
| `0>=`       | N\* |          | n -- flag                        | is positive or 0?                             |
| `0BRANCH`   | Y   |          | ...                              | branch if 0                                   |
| `1+`        | Y   | CORE     | x -- x+1                         |                                               |
| `1-`        | Y   | CORE     | x -- x-1                         |                                               |
| `2!`        | N\* | CORE     | x1 x2 a-addr --                  | store double-width value                      |
| `2*`        | N   | CORE     | n -- n<<1                        | logical shift left                            |
| `2/`        | N   | CORE     | n -- n>>1                        | logical shift right                           |
| `2>R`       | N\* | CORE-EXT | x1 x2 -- R: -- x1 x2             | put double-width value on rstack              |
| `2@`        | N\* | CORE     | a-addr -- x1 x2                  | fetch double-width value                      |
| `2DROP`     | Y   | CORE     | x1 x2 --                         |                                               |
| `2DUP`      | N\* | CORE     | x1 x2 -- x1 x2 x1 x2             | duplicate double-width value                  |
| `2OVER`     | N   | CORE     | x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 |                                               |
| `2R>`       | N\* | CORE-EXT | -- x1 x2 R: x1 x2 --             | take double-width value from rstack           |
| `2R@`       | N\* | CORE-EXT | -- x1 x2 R: x1 x2 -- x1 x2       | copy double-width value from rstack           |
| `2SWAP`     | Y   | CORE     | x1 x2 x3 x4 -- x3 x4 x1 x2       |                                               |
| `:`         | Y   | CORE     | C: "<spaces\>name" -- colon-sys  | define and begin compilation of new word      |
| `:NONAME`   | N   | CORE-EXT | C: "<spaces\>name" -- colon-sys  | define a nameless word, push xt               |
| `;`         | Y   | CORE     | C: colon-sys --                  | end word definition                           |
| `<`         | Y   | CORE     | x1 x2 -- flag                    |                                               |
| `<#`        | N   | CORE     | --                               | begin pictured numeric output                 |
| `<=`        | Y   |          | x1 x2 -- flag                    |                                               |
| `<>`        | Y   | CORE-EXT | x1 x2 -- flag                    |                                               |
| `=`         | Y   | CORE     | x1 x2 -- flag                    |                                               |
| `>=`        | Y   |          | x1 x2 -- flag                    |                                               |
| `>`         | Y   | CORE     | x1 x2 -- flag                    |                                               |
| `>BODY`     | Y   | CORE     | xt -- a-addr                     | to data-field address                         |
| `>CFA`      | Y   |          | xt -- a-addr                     | to code-field address                         |
| `>IN`       | N   | CORE     | -- a-addr                        | get offset from input buffer to parse area    |
| `>NUMBER`   | N   | CORE     | ud1 c-addr1 u1 -- ud2 c-addr2 u2 | ???                                           |
| `>R`        | Y   | CORE     | x -- R: -- x                     |                                               |
| `?`         | N\* | TOOLS    | a-addr --                        | `@ .`                                         |
| `?DO`       | N   | CORE-EXT | x1 x2 --                         | like DO, but if `x1=x2` then skip             |
| `?DUP`      | Y   | CORE     | x -- 0 \| x x                    |                                               |
| `@`         | Y   | CORE     | a-addr -- x                      |                                               |
| `ABORT`     | N   | CORE     | * -- R: * --                     | empty both stacks and `QUIT`                  |
| `ABORT"`    | N   | CORE     | "ccc<quote\>" flag --            | if flag, show error and `ABORT`               |
| `ABS`       | N   | CORE     | x -- \|x\|                       |                                               |
| `ACCEPT`    | N   | CORE     | addr maxlen -- len               | get string from input                         |
| `ACTION-OF` | N   | CORE-EXT | "<spaces\>name" -- xt            | get xt of `DEFER`red word                     |
| `AGAIN`     | N   | CORE-EXT | --                               | jump back to previous `BEGIN`                 |
| `ALIGN`     | N\* | CORE     | --                               | align the data pointer                        |
| `ALIGNED`   | N\* | CORE     | addr1 -- addr2                   | align an address                              |
| `ALLOT`     | N\* | CORE     | n --                             | add to the data pointer                       |
| `AND`       | Y   | CORE     | x1 x2 -- x1&x2                   |                                               |
| `BASE`      | Y   | CORE     | -- a-addr                        |                                               |
| `BEGIN`     | N   | CORE     | --                               | set jump point for `UNTIL`/`REPEAT`           |
| `BL`        | Y   | CORE     | -- x                             | get space character code                      |
| `BORDER`    | Y?  |          | n --                             | set border colour                             |
| `BRANCH`    | Y   |          | ...                              |                                               |
| `BUFFER:`   | N   | CORE-EXT | size "<spaces\>name" --          | define word that returns continuguous buffer  |
| `C!`        | Y   | CORE     | char c-addr --                   |                                               |
| `C"`        | N   | CORE-EXT | "ccc<quote\>" --                 | compile counted string                        |
| `C,`        | Y   | CORE     | char --                          | compile a character                           |
| `C@`        | Y   | CORE     | c-addr -- char                   |                                               |
| `C@C!`      | N   |          | src dst -- src+1 dst+1           |                                               |
| `CASE`      | N   | CORE-EXT | --                               | begin `CASE..OF..ENDOF..ENDCASE`              |
| `CELL+`     | Y   | CORE     | a-addr -- a-addr+2               | next cell address                             |
| `CELL-`     | Y   |          | a-addr -- a-addr-2               | previous cell address                         |
| `CELLS`     | N\* | CORE     | x -- x*2                         | calculate size of this many cells             |
| `CHAR`      | N\* | CORE     | "<spaces\>name" -- char          | `WORD DROP C@`                                |
| `CHAR+`     | N\* | CORE     | c-addr -- c-addr+1               |                                               |
| `CHARS`     | N\* | CORE     | x -- x                           | calculate size of this many chars (no-op)     |
| `CLS`       | Y?  |          | --                               | clears the screen                             |
| `CMOVE`     | Y   | STRING   | src dst len --                   |                                               |
| `COLOUR`    | Y?  |          | n --                             | set text/bg colour                            |
| `COMPILE,`  | N\* | CORE-EXT | xt --                            | compile xt                                    |
| `CONSTANT`  | N\* | CORE     | x "<spaces\>name" --             | declare named constant                        |
| `COUNT`     | N   | CORE     | c-addr -- c-addr+1 count         | expand counted string                         |
| `CR`        | N\* | CORE     | --                               | output carriage return                        |
| `CREATE`    | Y   | CORE     | "<spaces\>name" --               | create a new word that returns its dfa        |
| `DECIMAL`   | N\* | CORE     | --                               | set current base to decimal                   |
| `DEFER`     | N\* | CORE-EXT | "<spaces\>name" --               | create word with reassignable definition      |
| `DEFER!`    | N   | CORE-EXT | xt defer --                      | set `DEFER`red word to execute xt             |
| `DEFER@`    | N   | CORE-EXT | defer -- xt                      | get xt of `DEFER`red word                     |
| `DEPTH`     | Y   | CORE     | -- n                             | get current stack depth                       |
| `DO`        | N   | CORE     | limit index --                   | set bounded loop point beginning              |
| `DOCOL`     | Y   |          | -- a-addr                        | address of `:` runtime routine                |
| `DOES>`     | Y   | CORE     | --                               | assigns runtime behaviour to latest word      |
| `DP`        | Y   |          | -- adr                           | get data pointer address                      |
| `DROP`      | Y   | CORE     | x --                             |                                               |
| `DSP!`      | Y   |          | a-addr --                        | set data stack address                        |
| `DSP@`      | Y   |          | -- a-addr                        | get data stack address                        |
| `DUP`       | Y   | CORE     | x -- x x                         |                                               |
| `ECHO`      | Y   |          | -- a-addr                        | set keyboard echo                             |
| `ELSE`      | N   | CORE     | --                               | midpoint of `IF` .. `ELSE` .. `THEN`          |
| `EMIT`      | Y   | CORE     | x --                             | print char                                    |
| `ENDCASE`   | N   | CORE-EXT | --                               | resolve `CASE`                                |
| `ENDOF`     | N   | CORE-EXT | --                               | resolve `OF`                                  |
|`ENVIRONMENT?`|N   | CORE     | c-addr u -- false \| * true      | get environment value                         |
| `ERASE`     | N   | CORE-EXT | addr u --                        | zero specified memory range                   |
| `EVALUATE`  | N   | CORE     | * c-addr u -- *                  | evaluate string as FORTH code                 |
| `EXECUTE`   | Y   | CORE     | xt --                            |                                               |
| `EXIT`      | Y   | CORE     | R: nest-sys --                   |                                               |
| `F_HIDDEN`  | Y   |          | -- x                             | hidden flag                                   |
| `F_IMMED`   | Y   |          | -- x                             | immediate flag                                |
| `F_LENMASK` | Y   |          | -- x                             | bitmask for word length byte                  |
| `FALSE`     | N\* | CORE-EXT | -- false                         | get false flag                                |
| `FILL`      | N   | CORE     | c-addr u char --                 | fill memory with char                         |
| `FIND`      | Y!  | CORE     | adr len -- xt \| 0               |                                               |
| `FM/MOD`    | N   | CORE     | d n -- rem quo                   | floored double-single division                |
| `HERE`      | Y   | CORE     | -- adr                           | get data pointer                              |
| `HEX`       | N   | CORE-EXT | --                               | set current base to hexadecimal               |
| `HIDDEN`    | Y   |          | xt --                            | hides word                                    |
| `HIDE`      | Y   |          | "<spaces\>name" --               | hides word                                    |
| `HOLD`      | N   | CORE     | char --                          | add char to pictured numeric output           |
| `HOLDS`     | N   | CORE-EXT | c-addr u --                      | add string to pictured numeric output         |
| `I`         | N   | CORE     | -- x                             | push innermost loop index                     |
| `IF`        | N   | CORE     | flag --                          | skip until `ELSE` or `THEN` if false          |
| `IMMEDIATE` | Y   | CORE     | --                               | latest word becomes immediate                 |
| `INTERPRET` | Y?  |          | --                               | starts interpreter loop                       |
| `INVERT`    | Y   | CORE     | x -- ~x                          |                                               |
| `IS`        | N\* | CORE-EXT | xt "<spaces\>name" --            | set xt of `DEFER`red word                     |
| `J`         | N   | CORE     | -- x                             | push second-most loop index                   |
| `KEY`       | Y?  | CORE     | -- x                             |                                               |
| `LATEST`    | Y   |          | -- adr                           |                                               |
| `LEAVE`     | N   | CORE     | --                               | exit current `DO`..`LOOP`                     |
| `LIT`       | Y   |          | ...                              |                                               |
| `LITERAL`   | N\* | CORE     | x --                             | compile `LIT x`                               |
| `LITSTRING` | Y   |          | ...                              |                                               |
| `LOOP`      | N   | CORE     | --                               | increment index, jump if limit not reached    |
| `LSHIFT`    | N   | CORE     | x u -- x << u                    | shift x u places left                         |
| `M*`        | N   | CORE     | x1 x2 -- d                       | multiply to double-length value               |
| `MARKER`    | N\* | CORE-EXT | "<spaces\>name" --               | define word that resets data/search pointers  |
| `MAX`       | N\* | CORE     | x1 x2 -- x                       | choose max value                              |
| `MIN`       | N\* | CORE     | x1 x2 -- x                       | choose min value                              |
| `MOD`       | N\* | CORE     | x1 x2 -- x1%x2                   | `/MOD DROP`                                   |
| `MOVE`      | N\* | CORE     | src dst u --                     | copy cells from dst to src                    |
| `NEGATE`    | N   | CORE     | x -- -x                          | artihmetic inverse                            |
| `NIP`       | N\* | CORE-EXT | x1 x2 -- x2                      |                                               |
| `NUMBER`    | Y   |          | --                               |                                               |
| `OF`        | N   | CORE-EXT | --                               | split execution during `CASE`                 |
| `OR`        | Y   | CORE     | x1 x2 -- x1\|x2                  |                                               |
| `OVER`      | Y   | CORE     | x1 x2 -- x1 x2 x1                |                                               |
| `PAD`       | N   | CORE-EXT | -- c-addr                        | get address of transient buffer region        |
| `PARSE`     | Y   | CORE-EXT | char "ccc<char\>" -- c-addr u    | grab input until char reached                 |
| `PARSE-NAME`| N   | CORE-EXT | "<spaces\>name<space\>" -- c-addr u | grab next word from input                  |
| `PICK`      | N   | CORE-EXT | * u -- * xu                      | copy xu to top of stack                       |
| `POSTPONE`  | N\* | CORE     | "<spaces\>name" --               | compile xt for name                           |
| `QUIT`      | Y   | CORE     | R: i\*x --                       | set up for `INTERPRET` then call it           |
| `R0`        | Y   |          | -- a-addr                        | get return stack base                         |
| `R>`        | Y   | CORE     | -- x R: x --                     |                                               |
| `R@`        | Y   | CORE     | -- x R: x -- x                   | copy top of return stack to stack             |
| `RAND`      | Y   |          | -- x                             | random number generator                       |
| `RDROP`     | Y   |          | R: x --                          | `DROP` for the return stack                   |
| `RECURSE`   | N\* | CORE     | --                               | compile call to current word                  |
| `REFILL`    | N   | CORE-EXT | -- flag                          | attempt to refill input buffer                |
| `REPEAT`    | N   | CORE     | --                               | jump back to previous `BEGIN`                 |
|`RESTORE-INPUT`|N  | CORE-EXT | input-spec -- flag               | restore saved input-spec                      |
| `ROLL`      | N   | CORE-EXT | * u -- *                         | rotate top u cells of stack                   |
| `ROT`       | Y   | CORE     | x1 x2 x3 -- x2 x3 x1             |                                               |
| `RSHIFT`    | N   | CORE     | x u -- x >> u                    | logical shift x u places right                |
| `RSP!`      | Y   |          | a-addr --                        | set return stack address                      |
| `RSP@`      | Y   |          | -- a-addr                        | get return stack address                      |
| `S"`        | N   | CORE     | "ccc<quote\>" --                 | compile string                                |
| `S>D`       | N   | CORE     | n -- d                           | convert single-width to double-width value    |
| `S0`        | Y   |          | -- a-addr                        | get data stack base                           |
| `S\"`       | N   | CORE-EXT | "ccc<quote\>" --                 | compile string with escape codes              |
| `SAVE-INPUT`| N   | CORE-EXT | -- input-spec                    | preserve current input-spec                   |
| `SIGN`      | N   | CORE     | n --                             | if negative, add - to pictured numeric output |
| `SM/REM`    | N   | CORE     | d n -- rem quo                   | symmetric double-single division              |
| `SOURCE`    | N   | CORE     | -- c-addr u                      | return current input buffer                   |
| `SOURCE-ID` | N   | CORE-EXT | -- n                             | 0 = user input, -1 = string                   |
| `SPACE`     | Y   | CORE     | --                               | `BL EMIT`                                     |
| `SPACES`    | N   | CORE     | n --                             | print n SPACES                                |
| `STATE`     | Y   | CORE     | -- a-addr                        |                                               |
| `SWAP`      | Y   | CORE     | x1 x2 -- x2 x1                   |                                               |
| `THEN`      | N   | CORE     | --                               | resolve `IF`/`ELSE` reference                 |
| `TO`        | N   | CORE-EXT | * "<spaces\>name" --             | store into `VALUE` name                       |
| `TRUE`      | N\* | CORE-EXT | -- true                          | get true flag                                 |
| `TUCK`      | N\* | CORE-EXT | x1 x2 -- x2 x1 x2                |                                               |
| `TYPE`      | Y?  | CORE     | adr len --                       | output string                                 |
| `U.`        | N   | CORE     | u --                             | display unsigned number                       |
| `U.R`       | N   | CORE-EXT | u size --                        | display right-aligned unsigned number         |
| `U<`        | N   | CORE     | u1 u2 -- flag                    | unsigned less than                            |
| `U>`        | N   | CORE-EXT | u1 u2 -- flag                    | unsigned greater than                         |
| `UM*`       | N   | CORE     | u1 u2 -- d                       | unsigned multiply to double-width value       |
| `UM/MOD`    | N   | CORE     | ud u -- rem quo                  | unsigned double-single division               |
| `UNLOOP`    | N   | CORE     | --                               | discard current `DO` context                  |
| `UNTIL`     | N   | CORE     | flag --                          | if false, jump back to previous `BEGIN`       |
| `UNUSED`    | N   | CORE-EXT | -- u                             | get remaining space in data area              |
| `VALUE`     | N   | CORE-EXT | x "<spaces\>name" --             | create word that returns x but can be changed |
| `VARIABLE`  | N   | CORE     | "<spaces\>name" --               | declare named memory address                  |
| `VERSION`   | Y   |          | -- x                             | get Forth2068 version                         |
| `WHILE`     | N   | CORE     | x --                             | if false, jump past next `REPEAT`             |
| `WITHIN`    | N   | CORE-EXT | x lo hi -- flag                  | check that x is between lo and hi             |
| `WORD`      | Y!  | CORE     | "<spaces\>name" -- adr len       |                                               |
| `WORDS`     | Y?  | TOOLS    | --                               | show implemented words                        |
| `XOR`       | Y   | CORE     | x1 x2 -- x1^x2                   |                                               |
| `[`         | Y   | CORE     | --                               | enter interpretation mode                     |
| `[']`       | N   | CORE     | "<spaces\>name" --               | immediate version of `'`                      |
| `[CHAR]`    | N   | CORE     | "<spaces\>name" --               | immediate version of `CHAR`                   |
| `[COMPILE]` | N   | CORE-EXT | "<spaces\>name" --               | compile compilation semantics of name         | 
| `\`         | Y   | CORE-EXT | "ccc<eol\>" --                   | ignore rest of line                           |
| `]`         | Y   | CORE     | --                               | enter compilation mode                        |

## Other things

-   INTERPRET parsing jumps the gun; even a spacebar will make Forth2068 parse the previously-entered word. It should probably wait for a CR.
-   KEY will return any character code it is given, including BASIC keywords and non-printables. WORD should handle all these gracefully. Maybe.
