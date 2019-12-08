macro PushRegs
    PHA
    TXA
    PHA
    TYA
    PHA
endm

macro PullRegs
    PLA
    TAY
    PLA
    TAX
    PLA
endm
