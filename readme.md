# Forth2068

It's forth, but running on a TS2068! Except it only supports the Spectrum right now, haha.

## Word implementation

Y? means it's a Spectrum-specific implementation, so it needs replacing.
Y! means it's implemented but not to specification.
N\* means it's not implemented but is trivial to add in FORTH alone.

| Word        | ?   | Set      | Stack Effect                     | Comment                                       |
| ----------- | --- | -------- | -------------------------------- | --------------------------------------------- |
| `!`         | Y   | CORE     | x a-addr --                      |                                               |
| `#>`        | N   | CORE     | xd -- c-addr u                   | finalise pictured numeric output              |
| `#`         | N   | CORE     | ud1 -- ud2                       | pictured numeric output digit                 |
| `#S`        | N   | CORE     | ud1 -- ud2                       | pictured numeric output continuous conversion |
| `'`         | Y   | CORE     | "<spaces\>name" -- xt            |                                               |
| `(`         | Y   | CORE     | "ccc<paren\>" --                 | comment                                       |
| `*`         | Y   | CORE     | n1 n2 -- n1×n2                   |                                               |
| `*/`        | N   | CORE     | n1 n2 n3 -- n4                   | push `(n1×n2÷n3)`                             |
| `*/MOD`     | N   | CORE     | n1 n2 n3 -- rem quo              | push `(n1×n2) divmod n3`                      |
| `+`         | Y   | CORE     | n1 n2 -- n1+n2                   |                                               |
| `+!`        | Y   | CORE     | x a-addr --                      |                                               |
| `+LOOP`     | N   | CORE     | n --                             | LOOP except the increment is `n`              |
| `,`         | Y   | CORE     | x --                             |                                               |
| `-`         | Y   | CORE     | n1 n2 -- n1-n2                   |                                               |
| `-!`        | Y   | CORE     | x a-addr --                      |                                               |
| `-ROT`      | Y   |          | x1 x2 x3 -- x3 x1 x2             |                                               |
| `.`         | Y?  | CORE     | n --                             | show `n`                                      |
| `."`        | N   | CORE     | --                               | compile LITSTRING                             |
| `.(`        | N   | CORE-EXT | "ccc<paren\>" --                 | parse and display string                      |
| `.R`        | N   | CORE-EXT | n size --                        | display `n` right-aligned in `size` chars     |
| `.S`        | N   | TOOLS    | --                               | display stack contents                        |
| `/`         | N\* | CORE     | n1 n2 -- n1÷n2                   | `/MOD SWAP DROP`                              |
| `/MOD`      | Y   | CORE     | n1 n2 -- rem quo                 |                                               |
| `0<`        | N\* | CORE     | n -- flag                        | `0 <`                                         |
| `0<>`       | Y   | CORE-EXT | n -- flag                        |                                               |
| `0<=`       | N\* |          | n -- flag                        | `0 <=`                                        |
| `0=`        | Y   | CORE     | n -- flag                        |                                               |
| `0>`        | N\* | CORE-EXT | n -- flag                        | `0 >`                                         |
| `0>=`       | N\* |          | n -- flag                        | `0 >=`                                        |
| `0BRANCH`   | Y   |          | ...                              |                                               |
| `1+`        | Y   | CORE     | x -- x+1                         |                                               |
| `1-`        | Y   | CORE     | x -- x-1                         |                                               |
| `2!`        | N\* | CORE     | x1 x2 a-addr --                  | `SWAP OVER ! CELL+ !`                         |
| `2*`        | N\* | CORE     | n -- n<<1                        | `2 *`                                         |
| `2/`        | N\* | CORE     | n -- n>>1                        | `2 /`                                         |
| `2>R`       | N\* | CORE-EXT | x1 x2 -- R: -- x1 x2             | `SWAP >R >R`                                  |
| `2@`        | N\* | CORE     | a-addr -- x1 x2                  | `DUP CELL+ @ SWAP @`                          |
| `2DROP`     | Y   | CORE     | x1 x2 --                         |                                               |
| `2DUP`      | N\* | CORE     | x1 x2 -- x1 x2 x1 x2             | `OVER OVER`                                   |
| `2OVER`     | N   | CORE     | x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 |                                               |
| `2R>`       | N\* | CORE-EXT | -- x1 x2 R: x1 x2 --             | `R> R> SWAP`                                  |
| `2R@`       | N\* | CORE-EXT | -- x1 x2 R: x1 x2 -- x1 x2       | `R> R> 2DUP >R >R SWAP`                       |
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
| `AND`       | Y   | CORE     | x1 x2 -- x1&x2                   |                                               |
| `BASE`      | Y   | CORE     | -- a-addr                        |                                               |
| `BL`        | Y   | CORE     | -- x                             | get space character code                      |
| `BORDER`    | Y?  |          | n --                             | set border colour                             |
| `BRANCH`    | Y   |          | ...                              |                                               |
| `C!`        | Y   | CORE     | char c-addr --                   |                                               |
| `C@`        | Y   | CORE     | c-addr -- char                   |                                               |
| `C@C!`      | N   |          | src dst -- src+1 dst+1           |                                               |
| `CELL+`     | Y   | CORE     | a-addr -- a-addr+2               | `2 +`                                         |
| `CELL-`     | Y   |          | a-addr -- a-addr-2               | `2 -`                                         |
| `CHAR`      | N\* | CORE     | "<spaces\>name" -- char          | `WORD DROP C@`                                |
| `CLS`       | Y?  |          | --                               | clears the screen                             |
| `CMOVE`     | N   | STRING   | dst src len --                   |                                               |
| `COLOUR`    | Y?  |          | n --                             | set text/bg colour                            |
| `CONSTANT`  | N   | CORE     | x "<spaces\>name" --             | declare named constant                        |
| `CREATE`    | Y   | CORE     | "<spaces\>name" --               |                                               |
| `DOCOL`     | Y   |          | -- a-addr                        | address of `:` runtime routine                |
| `DROP`      | Y   | CORE     | x --                             |                                               |
| `DSP!`      | Y   |          | a-addr --                        | set data stack address                        |
| `DSP@`      | Y   |          | -- a-addr                        | get data stack address                        |
| `DUP`       | Y   | CORE     | x -- x x                         |                                               |
| `ECHO`      | Y   |          | -- a-addr                        | set keyboard echo                             |
| `EMIT`      | Y   | CORE     | x --                             | print char                                    |
| `EXECUTE`   | Y   | CORE     | xt --                            |                                               |
| `EXIT`      | Y   | CORE     | R: nest-sys --                   |                                               |
| `F_HIDDEN`  | Y   |          | -- x                             | hidden flag                                   |
| `F_IMMED`   | Y   |          | -- x                             | immediate flag                                |
| `F_LENMASK` | Y   |          | -- x                             | bitmask for word length byte                  |
| `FIND`      | Y!  | CORE     | adr len -- xt \| 0               |                                               |
| `HERE`      | Y   | CORE     | -- adr                           |                                               |
| `HIDDEN`    | Y   |          | adr --                           | hides word                                    |
| `HIDE`      | Y   |          | "<spaces\>name" --               | hides word                                    |
| `IMMEDIATE` | Y   | CORE     | --                               | latest word becomes immediate                 |
| `INTERPRET` | Y?  |          | --                               | starts interpreter loop                       |
| `INVERT`    | Y   | CORE     | x -- ~x                          |                                               |
| `KEY`       | Y?  | CORE     | -- x                             |                                               |
| `LATEST`    | Y   |          | -- adr                           |                                               |
| `LIT`       | Y   |          | ...                              |                                               |
| `LITSTRING` | N   |          | ...                              |                                               |
| `MOD`       | N\* | CORE     | x1 x2 -- x1%x2                   | `/MOD DROP`                                   |
| `NUMBER`    | Y   |          | --                               |                                               |
| `OR`        | Y   | CORE     | x1 x2 -- x1\|x2                  |                                               |
| `OVER`      | Y   | CORE     | x1 x2 -- x1 x2 x1                |                                               |
| `QUIT`      | Y   | CORE     | R: i\*x --                       | set up for `INTERPRET` then call it           |
| `R0`        | Y   |          | -- a-addr                        | get return stack base                         |
| `R>`        | Y   | CORE     | -- x R: x --                     |                                               |
| `RAND`      | Y   |          | -- x                             | random number generator                       |
| `RDROP`     | Y   |          | R: x --                          | `DROP` for the return stack                   |
| `ROT`       | Y   | CORE     | x1 x2 x3 -- x2 x3 x1             |                                               |
| `RSP!`      | Y   |          | a-addr --                        | set return stack address                      |
| `RSP@`      | Y   |          | -- a-addr                        | get return stack address                      |
| `S0`        | Y   |          | -- a-addr                        | get data stack base                           |
| `SPACE`     | Y   | CORE     | --                               | `BL EMIT`                                     |
| `STATE`     | Y   | CORE     | -- addr                          |                                               |
| `SWAP`      | Y   | CORE     | x1 x2 -- x2 x1                   |                                               |
| `TYPE`      | Y?  | CORE     | adr len --                       |                                               |
| `VARIABLE`  | N   | CORE     | "<spaces\>name" --               | declare named memory address                  |
| `VERSION`   | Y   |          | -- x                             | get Forth2068 version                         |
| `WORD`      | Y!  | CORE     | "<spaces\>name" -- adr len       |                                               |
| `WORDS`     | Y?  | TOOLS    | --                               | show implemented words                        |
| `XOR`       | Y   | CORE     | x1 x2 -- x1^x2                   |                                               |
| `[`         | Y   | CORE     | --                               | `0 STATE !`                                   |
| `]`         | Y   | CORE     | --                               | `1 STATE !`                                   |

## Other things

-   INTERPRET parsing jumps the gun; even a spacebar will make Forth2068 parse the previously-entered word. It should probably wait for a CR.
-   KEY will return any character code it is given, including BASIC keywords and non-printables. WORD should handle all these gracefully. Maybe.
