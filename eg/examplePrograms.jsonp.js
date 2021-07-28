examplePrograms = [
    {
        "contents": "; Display the Roman alphabet in reverse, in ZOWIE\n; This example source is in the public domain.\n\nMOV R10, 90   ; initially it's \"Z\"\nMOV R1, R1    ; BEGIN TRANSACTION for \"REPEAT\"\nMOV R0, R10   ; output character\nMOV R8, R10   ; decrement character\nMOV R5, 1\nMOV R10, R8\nMOV R8, R10   ; test if character is above \"@\"\nMOV R5, 64\nMOV R3, R8    ; COMMIT AND REPEAT if non-zero\n",
        "filename": "chars.zow"
    },
    {
        "contents": "; Compute a factorial, in ZOWIE\n; This example source is in the public domain.\n\n; Expected output is LATIN SMALL LETTER X (Unicode character 120).\n; NOTE: this code is also able to properly compute 0! = 1.\n\nMOV R11, 5    ; let's find 5!\nMOV R10, 1    ; accumulator\n\nMOV R8, R11   ; increase multiplicand\nMOV R4, 1\nMOV R11, R8\n\nMOV R1, R1    ; BEGIN TRANSACTION for \"REPEAT\"\n\nMOV R8, R11   ; decrease multiplicand\nMOV R5, 1\nMOV R11, R8\n\nMOV R1, R1    ; BEGIN TRANSACTION for \"IF\"\n\nMOV R8, R10   ; accumulator\nMOV R6, R11   ; multiplied\nMOV R10, R8\n\nMOV R2, R11   ; COMMIT if multiplicand above zero, or ROLLBACK otherwise\nMOV R3, R11   ; COMMIT AND REPEAT if multiplicand above zero\n\nMOV R0, R10   ; output accumulator (as single Unicode character)\n",
        "filename": "fact.zow"
    },
    {
        "contents": "; Hello, world! in ZOWIE\n; This example source is in the public domain.\n\nMOV R0, 72\nMOV R0, 101\nMOV R0, 108\nMOV R0, 108\nMOV R0, 111\nMOV R0, 44\nMOV R0, 32\nMOV R0, 119\nMOV R0, 111\nMOV R0, 114\nMOV R0, 108\nMOV R0, 100\nMOV R0, 33\nMOV R0, 10\n",
        "filename": "hello.zow"
    }
];
