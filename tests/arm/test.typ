#import "/src/lib.typ" as kleene
#import kleene.prelude: *

#let rules = kleene.grammar(
  spaces: {
    pat(iter(" "))
    rw(none)
    yy(
      ` `,
      `        `,
    )
  },
  xspaces: {
    pat(maybe(<spaces>))
    rw(none)
  },
  comment: {
    pat("//", $$, `[^\n]*`, fork("\n", eof()))
    rw(none)
    yy(
      ```// comment```,
      ```
      // comment
      
      ```,
      `//`,
      ```
      //
      
      ```,
    )
  },
  comma: {
    pat(<xspaces>, ",", <xspaces>)
    rw(none)
    yy(
      `,`,
      `    , `,
    )
  },
  lb: {
    pat(iter(<xspaces>, fork(eof(), "\n")))
    rw(none)
    yy(```
               
          
           
    ```)
  },
  linebreak: {
    pat(iter(<xspaces>, fork(<comment>, <lb>)))
    rw(none)
    yy(
      ```
           

      
      ```,
      ```
      // comment

      // eh
      ```
    )
    nn(
      ```

             x
      ```,
      ```
      // comment
         x
      ```,
      ```
      // comment

         x
      ```,
      ```
      // comment
         x
      ```
    )
  },
  ident-startchar: {
    pat(`[a-z]`)
    pat(`[A-Z]`)
    yy(`f`, `A`)
    nn(`_`)
  },
  ident-anychar: {
    pat(fork(<ident-startchar>, `[0-9]`, "_"))
    yy(`A`, `0`, `x`, `_`)
  },
  ident: {
    pat(<ident-startchar>, $$, maybe(iter(<ident-anychar>)))
    rw(cs => (lab: cs.flatten().join()))
    yy(`LD_foo`, `main`, `loop0`)
    nn(`0var`)
  },
  size: {
    pat(drop("."), $$, fork("byte", "hword", "word"))
    rw(sz => {
      let size = (byte: 1, hword: 2, word: 4).at(sz)
      (size: size)
    })
    yy(
      `.byte`,
      `.word`,
      `.hword`,
    )
    nn(
      `.bte`,
    )
  },
  hexvalue: {
    pat(drop("0x"), $$, iter(`[0-9a-fA-F]`))
    rw(cs => (hex: cs.flatten().join()))
    yy(
      `0x45`,
      `0xdead`,
      `0xffffffff`,
    )
    nn(`0xz`)
  },
  decvalue: {
    pat(`[0-9]`, $$, star(`[0-9]`))
    rw(cs => (int: cs.flatten().join()))
    yy(`42`, `1000`, `0`)
    nn(`45x`)
  },
  value: {
    pat(fork(<hexvalue>, <decvalue>, <ident>))
    yy(`0x44`, `420`, `mask`)
    nn(`0var`)
  },
  concrete-value: {
    pat(<size>, <spaces>, <value>)
    rw(((sz, val),) => (: ..sz, ..val))
    yy(
      `.byte 0x42`,
      `.word mask`,
    )
    nn(
      `.bte 0x42`,
      `.byte 45x`,
    )
  },
  abstract-value: {
    pat(drop(".skip"), $$, <spaces>, <decvalue>)
    rw(v => (size: int(v.int)))
    yy(`.skip 5`)
    nn(
      `skip 3`,
      `.skip 0x42`,
    )
  },
  data-value: {
    pat(fork(<abstract-value>, <concrete-value>))
    yy(
      `.byte 0x42`,
      `.skip 8`,
    )
  },
  data-label: {
    pat(<ident>, $$, drop(":"))
    yy(`a:`, `B:`)
  },
  data-contents: {
    pat(maybe(iter(
      <xspaces>,
      fork(<data-label>, <data-value>),
      fork(<linebreak>, <spaces>),
    )))
    rw(elems => elems.flatten())
    yy(
      ```
      A:
        B: 
        .word 0x64
             
        .byte 0x42

        
      foo: .hword 42
      bar: baz: .byte 1 .byte 2 .byte 3
      ```,
    )
    nn(
      `.byte 1 c`,
    )
  },
  data-section: {
    pat(<xspaces>, drop(".data"), <linebreak>, <data-contents>)
    rw(data => (data: data.flatten()))
    yy(
      ```
        .data
      foo: .word 0x42
      bar: .word 0x43
      ```,
    )
  },
  instr-ldr: {
    pat(fork("ldrb", "ldrh", "ldr"))
    rw(instr => (instr: "ldr", size: (ldr: 4, ldrh: 2, ldrb: 1).at(instr)))
  },
  instr-any: {
    pat(fork("mov", "mvn", "add", "sub", "lsl", "lsr", "eor", "orr", "and", "b"))
    rw(instr => (instr: instr))
  },
  instr-code: {
    pat(fork(<instr-ldr>, <instr-any>))
    yy(
      `ldr`,
      `add`,
      `lsl`,
      `ldrh`,
      `b`,
    )
  },
  register-number: {
    pat(drop("r"), iter(`[0-9]`))
    rw(id => (reg: int(id.flatten().join(""))))
    yy(
      `r0`,
      `r9`,
      `r12`,
    )
  },
  register-alias: {
    pat(fork("lr", "sp", "pc"))
    rw(id => (reg: (sp: 13, lr: 14, pc: 15).at(id)))
    yy(
      `sp`,
      `lr`,
      `pc`,
    )
  },
  register: {
    pat(fork(<register-number>, <register-alias>))
    yy(
      `r8`,
      `sp`,
      `lr`,
      `r0`,
    )
  },
  constant: {
    pat(drop("#"), <value>)
    yy(
      `#4`,
      `#0x1`,
    )
  },
  deref-offset: {
    pat(<comma>, fork(<register>, <constant>))
    yy(
      `, r1`,
      `, #3`,
    )
  },
  deref-reg: {
    pat(drop("["), <xspaces>, <register>, maybe(iter(<deref-offset>)), <xspaces>, drop("]"))
    rw(((base, extras),) => (deref: (base, ..extras.flatten())))
    yy(
      `[r1]`,
      `[ r2  ]`,
      `[r1, r2]`,
      `[r1 , #1]`,
      `[r1, r2, r3]`,
      `[r1, r2, #2]`,
    )
  },
  local-label: {
    pat(drop("."), <ident>)
    rw(id => (lab: "." + id.lab))
    yy(
      `.LD_foo`,
    )
  },
  eqlabel: {
    pat(drop("="), <ident>)
    rw(id => (eq: id.lab))
  },
  operand: {
    pat(fork(<register>, <deref-reg>, <local-label>, <constant>, <eqlabel>, <ident>))
    yy(
      `lr`,
      `#1`,
      `[ r1 , #2 ]`,
      `.LD_xx`,
      `=mask`,
      `loop0`,
    )
  },
  comma-operand: {
    pat(<comma>, <operand>)
  },
  operands: {
    pat(<operand>, maybe(iter(<comma-operand>)))
    rw(((base, extras),) => { (base, ..extras.flatten()) })
    yy(
      `lr, #1`,
      `r0 , r0 , [r1, #2]`,
    )
  },
  instruction: {
    pat(<xspaces>, <instr-code>, <spaces>, <operands>)
    rw(((instr, ops),) => (: ..instr, ops: ops))
    yy(
      `ldr r0, [r1]`,
      `add r0, r1, r2`,
      `sub r1, #1`,
      `lsl r1, #8`
    )
  },
  inline-data: {
    pat(<xspaces>, <local-label>, drop(":"), fork(<linebreak>, <spaces>), <xspaces>, <data-value>)
    rw(((lab, val),) => (: ..lab, ..val))
    yy(
      `.LD_xx: .word x`,
      ```
      .LD_xx: 
        .byte 0x42
      ```
    )
  },
  inline-label: {
    pat(<ident>, drop(":"))
    rw(id => (tag: id.lab))
    yy(
      `main:`,
    )
  },
  print-width: {
    pat(drop("/"), <decvalue>)
    rw(w => int(w.int))
    yy(
      `/8`,
      `/16`,
    )
  },
  register-slice: {
    pat(drop("["), maybe(<decvalue>), drop(":"), maybe(<decvalue>), drop("]"))
    rw(((start,len),) => (start: start.at(0, default: auto), len: len.at(0, default: auto)))
    yy(
      `[:]`,
      `[1:]`,
      `[16:8]`,
    )
  },
  print-register: {
    pat(<register>, maybe(<register-slice>))
    rw(((r,sl),) => (: ..r, ..sl.at(0, default: (:))))
    yy(
      `r0`,
      `r0[:8]`,
      `r0[16:32]`,
    )
  },
  print-list: {
    pat(<print-register>, maybe(iter(<comma>, <print-register>)))
    rw(((r,rs),) => (r, ..rs.flatten().filter(x => x != none)))
    yy(
      `r0, r1, r2`,
    )
  },
  print-directive: {
    pat(drop("print"), maybe(<print-width>), <xspaces>, drop(":"), <xspaces>, <print-list>)
    rw(((w,rs),) => (print: (width: w.at(0, default: auto), regs: rs)))
    yy(
      `print/8 : r0`,
      `print : r1, r2`,
    )
  },
  directive: {
    pat(drop("@"), <xspaces>, fork(<print-directive>, /* maybe more in the future */))
    yy(
      `@ print/8: r0`,
    )
  },
  text-contents: {
    pat(iter(
      <xspaces>,
      fork(<instruction>, <inline-label>, <inline-data>, <directive>),
      <linebreak>,
    ))
    yy(
      ```
      main:
        ldr r0, [r1]
        add r0, #1 // test
      loop0:
        eor r0, r1, r2
      .LD_data: .word data
      ```,
    )
  },
  text-section: {
    pat(<xspaces>, drop(".text"), <linebreak>, <text-contents>)
    rw(body => (text: body.flatten()))
    yy(
      ```
        .text

      main:
        mov r0, #1 // test
        add r0, r0, r0
      ```,
    )
  },
  arm: {
    pat(<data-section>, <text-section>, <xspaces>)
    rw(((data, text),) => (: ..data, ..text))
    yy(
      ```
  .data
A: .byte 0x64
   .word 0x95
   .hword 45

  
  .text
main: // main function
  ldr r0, .LD_A
  ldr r0, [r6]
  ldrb r0, [r6]
  ldrh r0, [r5, #1] // yay
  mov r5, r6
  mvn r5, r6
  mov r5, #0x1
  add r5, r6, #1
   
  lsl r5, #1
  lsr r5, #1 // do some stuff
  // testing comments
  eor r3, r4, r5
  orr r3, r4, r5
  and r3, r4, r5 
  
  
.LD_A: .word A
  
    ```,
    )
  },
)

#kleene.test(rules)

#let _ = kleene.freshen(rules, prefix: "_", exclude: (<arm>,))
#let _ = kleene.trim(rules, roots: (<text-section>, <data-section>))
