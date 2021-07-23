Tests for ZOWIE
===============

This non-exhaustive test suite is written in format of Falderal 0.9.

    -> Tests for functionality "Interpret ZOWIE Program"

Display the Roman alphabet in reverse.

    | MOV R10, 90   ; initially it's "Z"
    | MOV R1, R1    ; BEGIN TRANSACTION for "REPEAT"
    | MOV R0, R10   ; output character
    | MOV R8, R10   ; decrement character
    | MOV R5, 1
    | MOV R10, R8
    | MOV R8, R10   ; test if character is above "@"
    | MOV R5, 64
    | MOV R3, R8    ; COMMIT AND REPEAT if non-zero
    = ZYXWVUTSRQPONMLKJIHGFEDCBA

Compute a factorial.

    | MOV R11, 5    ; let's find 5!
    | MOV R10, 1    ; accumulator
    | 
    | MOV R8, R11   ; increase multiplicand
    | MOV R4, 1
    | MOV R11, R8
    | 
    | MOV R1, R1    ; BEGIN TRANSACTION for "REPEAT"
    | 
    | MOV R8, R11   ; decrease multiplicand
    | MOV R5, 1
    | MOV R11, R8
    | 
    | MOV R1, R1    ; BEGIN TRANSACTION for "IF"
    | 
    | MOV R8, R10   ; accumulator
    | MOV R6, R11   ; multiplied
    | MOV R10, R8
    | 
    | MOV R2, R11   ; COMMIT if multiplicand above zero, or ROLLBACK otherwise
    | MOV R3, R11   ; COMMIT AND REPEAT if multiplicand above zero
    | 
    | MOV R0, R10   ; output accumulator (as single Unicode character)
    = x

The only command in the language is `MOV` and it must be in uppercase.

    | cmp R11, 5
    ? 

    | mov R11, 5
    ? 

Register names must be uppercase, too.

    | MOV r11, 5
    ? 

The destination cannot be an immediate.

    | MOV 5, R11
    ? 

Retrieve indirect reference.

    | MOV R11, 65
    | MOV R12, 11
    | MOV R0, R[R12]
    = A

Store indirect reference.

    | MOV R12, 11
    | MOV R[R12], 65
    | MOV R0, R11
    = A

Commit a transaction.

    | MOV R10, 65
    | MOV R1, 1
    | MOV R10, 66
    | MOV R2, 1
    | MOV R0, R10
    = B

Rollback a transaction.

    | MOV R10, 65
    | MOV R1, 1
    | MOV R10, 66
    | MOV R2, 0
    | MOV R0, R10
    = A

Commit (but do not repeat) a transaction.

    | MOV R10, 65
    | MOV R1, 1
    | MOV R10, 66
    | MOV R3, 0
    | MOV R0, R10
    = B

Commit and repeat a transaction.

    | MOV R10, 65
    | MOV R11, 1
    | MOV R12, 0
    | MOV R1, 1         ; begin the transaction
    | MOV R17, R11
    | MOV R11, R12
    | MOV R3, R17       ; the first time, R17 will have 1; the second, 0
    | MOV R0, R10
    = A
