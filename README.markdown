ZOWIE
=====

Introduction
------------

ZOWIE is a programming language where all flow-control is both
*memory-mapped* and *structured*. It is memory-mapped in the sense that
changes in flow are triggered by changes made to memory locations, and
it is structured in the sense of structured programming – the programmer
never deals with `goto`s, offsets, or labels of any kind.

History
-------

The primary design goal of ZOWIE was to have memory-mapped structured
flow control. This goal was inspired by Jeffry Johnston's unsuccessful
attempt to reduce the number of instructions in
[BitChanger](http://esolangs.org/wiki/BitChanger) (while retaining
Turing-completeness) by memory-mapping the loop operation.

I initially thought that the difficulty lay in BitChanger's minimalism.
To do memory-mapped flow control in general sounded easy – just start a
loop when one memory location is written, and end it when some other
location is written, right? But no. It's not that simple, as I attempt
to explain below. ZOWIE is the last of several, sometimes painful,
attempts over the summer of 2009 to realize this design goal (and it is
not clear that the techniques used in ZOWIE could be usefully imported
back into BitChanger.) The final, workable idea crystallized at about
the time September turned into October.

The thing with loop structures is that there is usually some way to jump
to a known point in the loop (say, the start, or the end) – really,
that's what makes them structured. And to jump to a point in a loop, you
have to know where that point is. And if you can analyze the loop
structure statically, for example if your semantics are directed by
syntax as in almost all high-level languages, then it's not difficult to
know where that point is.

But if that point is defined by when some *memory* location is changed,
it is not in general possible to detect it statically (by Rice's
Theorem, which is just a generalization of the Halting Problem.) So it
is not, in general, possible to know ahead of time where the loop begins
or ends.

There are a few things to note about this.

One is that by "statically" I do not necessarily mean "at compile-time".
Many Brainfuck and Mouse interpreters seek out the end of the loop only
when they know they must exit it. However, because they are looking
through the program text, it is still a kind of static analysis.

Another thing is that it would of course be possible to detect some kind
of (fixed) command to change the memory location associated with ending
a loop – but that would be cheating! (Also, if memory locations can be
computed, it is still not fully general, because we cannot look for all
possible computations that would result in that memory location.)

Lastly, note that we really don't have a problem detecting the start of
a loop. As soon as we execute the start of a loop, we know it's a loop,
and we know where it is, so we can record that location. The problem is
any other point in the loop, like the end. A little reflection will
reveal that this means it will be more difficult to do a "WHILE" loop or
a structured conditional ("IF-THEN-ENDIF") than a "REPEAT" loop (where
the condition is at the end of the loop.) However, it is widely known
that "REPEAT" loops alone are not sufficient for a Turing-complete
language. We'll see below that ZOWIE manages to create generalized loops
through the use of transactions.

The secondary design goal of ZOWIE was to strike the perfect balance
between It's a Mad Mad Mad Mad World and The Party. It is generally
considered a morbid failure in that regard, what with not being a madcap
60's movie and all.

Syntax and Semantics
--------------------

To mitigate retooling costs, ZOWIE borrows much of its archiecture and
instruction repertoire from [SMITH](http://catseye.tc/projects/smith/).
There are an unlimited number of registers, numbered from 0 upward; each
register contains a non-negative integer value of unbounded extent. The
contents of a register before it has ever been written is guaranteed to
be 0.

There are five instruction forms. The first register is the destination
register, and is written to; the second register (or immediate value) is
read from. As in SMITH, square brackets indicate indirect register
access.

      MOV register, immediate       e.g.  MOV R8, 141
      MOV register, register              MOV R8, R9
      MOV [register], register            MOV R[R8], R9
      MOV register, [register]            MOV R8, R[R9]
      MOV [register], [register]          MOV R[R8], R[R9]

Not only flow control, but in fact all operations in ZOWIE are
memory-mapped. The lowest-numbered nine registers have special behaviour
when written to or read from:

`R0`

When a value is written into R0, the Unicode symbol represented by the
value is sent to the standard output channel.

Reading from R0 waits until a Unicode symbol is available on the
standard input, then offers its value as the value of this register.

This is similar to the `TTY` pseudo-register of SMITH.

*Note: although implementations should make a best effort, the external
encoding and representation of Unicode characters is ultimately
implementation-defined, especially on systems which are only capable of
accurately displaying a subset of the Unicode character set. For
example, on a strict ASCII teletype or other device incapable of
displaying the DOWNWARDS ARROW (↓) symbol, it would be reasonable to
output `&#8595;` or some similar "escape sequence" when executing
`MOV R0, 8595`.*

`R1`

When a value is written into R1, a **BEGIN TRANSACTION** occurs;
conceptually, a copy of the program state, including all registers and
the location of the currently executing instruction, is made, and pushed
onto a stack.

Reading from R1 always offers the value 1.

`R2`

When a value is written into R2, what happens depends on the value.

If the value is greater than zero, the current transaction is
**COMMIT**ted; conceptually, the topmost program state is popped from
the stack and discarded.

If the value is equal to zero, the current transaction is
**ROLLBACK**ed. Conceptually, the topmost program state is popped; the
contents of all registers are reset to what they were in the popped
program state; but the location of the currently executing instruction
is unchanged.

Reading from R2 always offers the value 2.

`R3`

When a value is written into R3, what happens depends on the value.

If the value is greater than zero, the current transaction is **COMMIT
AND REPEAT**ed; conceptually, the topmost program state is popped from
the stack; the location of the currently executing instruction is reset
to what it was in the program state; and a copy of this new program
state is pushed once more onto the stack.

If the value is equal to zero, the current transaction is **COMMIT**ed
(described previously in R2).

Reading from R3 always offers the value 3.

`R4`

When a value is written into R4, that value is added to the value in R8,
and the result is written into R8. Reading from R4 always offers the
value 4.

`R5`

When a value is written into R5, that value is subtracted from the value
in R8, and the result is written into R8. If the result would be
negative, the result will be zero. Reading from R5 always offers the
value 5.

`R6`

When a value is written into R6, the product of that value and the value
in R8 is written into R8. Reading from R6 always offers the value 6.

`R7`

When a value is written into R7, the boolean negation of that value is
written into R7: 1 if the value was 0, and 0 otherwise. Reading from R7
always offers the value 7.

`R8`

Not really memory-mapped, but used as an "accumulator" by the registers
R4 through R7.

Because the reading and writing of registers can have side-effects, the
order of reads and writes during the execution of a single instruction
is strictly defined as follows:

-   The indirect source register, if any, is read (to discover the
    direct source register.)
-   The direct source register is read.
-   The indirect destination register, if any, is read (to discover the
    direct destination register.)
-   The direct destination register is written.

Computational Class
-------------------

I believe ZOWIE is Turing-complete because the transactions can simulate
both "IF" and "REPEAT" control structures, which, taken together, can
simulate a "WHILE", which is widely known to be sufficient, along with
the usual arithmetical operations on an unbounded number of unbounded
integer registers, to have a system that is Turing-complete.

For example, a crude translation of Brainfuck into ZOWIE might go like:

    preamble MOV R10, 100        ; the Brainfuck tape index
             MOV R11, 101        ; the saved-test-value stack pointer

    >        MOV R8, R10         ; inc tape index by two
             MOV R4, R2
             MOV R10, R8

    <        MOV R8, R10         ; dec tape index by two
             MOV R5, R2
             MOV R10, R8

    +        MOV R8, R[R10]      ; inc value on tape
             MOV R4, R1
             MOV R[R10], R8
             
    -        MOV R8, R[R10]      ; dec value on tape
             MOV R5, R1
             MOV R[R10], R8

    .        MOV R0, R[R10]      ; output

    ,        MOV R[R10], R0      ; input

    [        MOV R1, R1          ; BEGIN TRANSACTION for "REPEAT"
             MOV R8, R11         ; bump up the saved-value stack pointer
             MOV R4, R2
             MOV R11, R8
             MOV R[R11], R[R10]  ; save the value we are testing
             MOV R1, R1          ; BEGIN TRANSACTION for "IF"

    ]        MOV R2, R[R11]      ; COMMIT if non-zero or ROLLBACK otherwise
             MOV R12, R11        ; retain a copy of the saved-stack pointer
             MOV R8, R11         ; bump down the saved-stack pointer
             MOV R5, R2
             MOV R11, R8
             MOV R3, R[R12]      ; COMMIT AND REPEAT if non-zero

Three things to note:

-   In this translation, the simulated Brainfuck tape and the
    saved-value stack are interleaved.
-   It is important to save the value being tested *before* the "IF"
    transaction is begun – otherwise, the value will be rolled back
    before it can be tested for the **COMMIT AND REPEAT**.
-   The input-output behaviour of ZOWIE programs produced by this
    translation does differ from Brainfuck. If the value on the tape is
    initially zero, a Brainfuck "while" loop will never be executed at
    all, whereas a ZOWIE transaction *will* be executed, but afterwards
    undone – everything, that is, except input and output, because being
    interactions with the outside world, those can't be undone. This
    limitation does not affect whether ZOWIE is Turing-complete or not
    (you could just refrain from outputting anything until the very end
    of the computation), but it does imply that ZOWIE has limitations on
    how it can communicate.

That's all.

Happy *«deleted by black helicopters»*! \
Chris Pressey \
December 29^th^, 2009 CE \
Evanston, IL
