! vi:ft=brainfuck
! comments: (all start with !)
!   ~ means minus
!   ? is a context recap
!   ← is move (rhs cell is zeroed and its content is added to lhs)
!   ←~ is a negative move (rhs cell is zeroed and its content is removed from lhs)
!   = is a set
!   // description/n defines a block; n is the number of cells it uses (assumes zeroed)
!   cn is cell n; usually relative to the current block

!// parse command
>>>>>>>>>>>>>>>>>>>>+++++++++++++++++++++++++++++++++++ ! c20 ← 35 (hash)
>++++++++++++++++++++++++++++++++++++++++++++++ ! c21 ← 46 (dot)
<<<<<<<<<<<<<<<<<<<++++++++++++++++++++ ! c2 ← 20
>+ ! c3 ← 1
>
,
[
  >>>>>>[-]>[-]<<<<<<< ! c10 c11 = 0
  >+< ! c5 ← 1
  -------------------------------------------------------
  ------------------------------------------------------- ! c4 ← input ~ 110 (input ~ 'n')
  [ ! if line is a "addx"
    ,,,,[-] ! skip over "ddx " to get to the number
    >[-] ! c5 = 0
    
    <<[>>>>>>>+>+<<<<<<<<-] ! c10 c11 ← c3
    >>>>>>>>[<<<<<<<<+>>>>>>>>-] ! c3 ← c11
    ++ ! c11 ← 2
    
    <<<<<<<<<<[>>>>+>+<<<<<-] ! c5 c6 ← c1
    >>>>>[<<<<<+>>>>>-] ! c1 ← c6
    <<<<[>>>+>+>+>+<<<<<<-] ! c5 c6 c7 c8 ← c2
    >>>>>[<<<<<+>>>>>-] ! c2 ← c7
    +>>+ ! c7 ← 1; c9 ← 1
    
    !? cur: c9; c0: sum; c1: cycle; c2: rem; c3: x; c4: char;
    !? c5: cycle plus rem; c6: rem; c7: 1; c8: rem; c9: 1
    <--[>[-]<[-]] ! c9 = (rem == 2)
    <<-[>[-]<[-]] ! c7 = (rem == 1)
    >>>[<<<+>>>-] ! c6 ← c9
    <<[<+>-]<     ! c6 ← c7
    !? cur: c6; c5: rem plus cycle; c6: rem == 1 || rem == 2; c7 c8 c9 = 0
    [ ! if rem == 1 or rem == 2
      [-] ! c6 = 0
      <<<<++++++++++++++++++++++++++++++++++++++++ ! c2 ← 40
      >>> !? cur: c5
      [ ! c6 ← c3 * c5: x * (cycle plus rem)
        <<[>>>+>+<<<<-] ! c6 c7 ← c3
        >>>>[<<<<+>>>>-] ! c3 ← c7
        <<- ! c5 ←~ 1
      ]
      >[<<<<<<+>>>>>>-] ! c0 ← c6
    ]
    <[-]
    !// parse new line terminated number/5 | num → c0; sign ← c1
    >>++++++++++++++++++++++++++++++++++++++ ! c2 ← 38
    [ !? cur: c2
      -------------------------------------- ! c2 ← c2 ~ 38
      <<[>++++++++++<-] ! c1 ← c0 * 10 
      >[<+>-] ! c0 ← c1
      >[<<+>>-] ! c0 ← c2
      ,---------- ! c1 ← input ~ 10
      [<+>>+<-] ! c1 c3 ← c2
      <----------------------------------- ! c1 ← c1 ~ 35
      [+++++++++++++++++++++++++++++++++++[>+<-]] ! if c1 != 35: c2 ← c1
      !? cur: c1; c1: 0; c2: (input ~ 10) or 0; c3: (input ~ 10)
      >>[<-<+>>-] ! c2 ←~ c3; c1 ← c3
      <[ ! if c2 != 0 (if input was a minus sign)
        !? cur: c2
        <[-],---------- ! c1 = input ~ 10
        >>>+ ! c4 ← 1
        <<[-] ! c2 = 0
      ]
      <[>+<-] ! c2 ← c1
      >
    ]
    >>[<<<+>>>-]
    <<<<
    !// end

    >>+<< ! c7 ← 1
    !? cur: c5; c2: cycle; c3: x reg; c4: char; c5: number; c6: negative (bool); c7: 1
    >[ ! if number is negative
      <[<<->>-] ! c3 ←~ c5
      >>[-] ! c7 = 0 (unset c7)
      <[-] ! c6 = 0
    ]
    >[ ! if number is positive
      <<[<<+>>-] ! c3 ← c5
      >>[-] ! c7 = 0
    ]
    <<<<<-- ! c2 ←~ 2
    <++ ! c1 ← 2
    >>>[-] ! c4 = 0
  ]
  >[ ! if the instruction is "noop"
    ,,,,[-] ! skip "oop\n"

    <<[>>>>>>>+>+<<<<<<<<-] ! c10 c11 ← c3
    >>>>>>>>[<<<<<<<<+>>>>>>>>-] ! c3 ← c11
    + ! c11 ← 1

    <<<<<<<<<[>>>>+<<<<-] ! c6 ← c2
    >>>>[<+<<<+>>>>-] ! c2 c5 ← c6
    +<- ! c6 ← 1; c5 ←~ 1
    [>[-]<[-]] ! invert bool in c6
    !? c6: 1 if c2 == 1 else 0
    >[[-]
      <<<<++++++++++++++++++++++++++++++++++++++++ ! c2 ← 40
      <[>>>>>+<<<<<-] ! c6 ← c1
      >>>>>[<+<<<<+>>>>>-] ! c1 c5 ← c6
      <+ ! c5 ← 1 (this is c1 plus c2 because we know c2 is 1)
      <<[>>>>+<<<<-] ! c7 ← c3
      >>>>[<+<<<+>>>>-] ! c3 c6 ← c7
      ! multiplies c1 and c3 into c7
      <<[ !? cur: c5
        >[>>+<<-] ! c8 ← c6
        >>[<+<+>>-] ! c6 c7 ← c8
        <<<- ! c5 ←~ 1
      ]
      >>[<<<<<<<+>>>>>>>-] ! c0 ← c7
      <[-] ! c6 = 0
    ]
    <<<<- ! c2 ←~ 1
    <+ ! c1 ← 1
    >>>>[-] ! c5 = 0
  ] !? cur: c5

  >>>>>

  !// draw
  >[
    <[>>>+>>+>>+>>+<<<<<<<<<-] ! c3 c5 c7 c9 ← c0
    >>[<<+>>-] ! c0 ← c2
    <<[>>+>>+>>+>>+<<<<<<<<-] ! c2 c4 c6 c8 ← c0
    >>>>>>>>>[<<<<<<<<<+>>>>>>>>>-] ! c0 ← c9
    <<<<<<
    
    >+[<->-]+> ! c3 ←~ c4 plus 1; c4 = 1
    > [<->-]+> ! c5 ←~ c6; c6 = 1
    >-[<->-]+  ! c7 ←~ c8 ~ 1; c8 = 1

    <[>[-]<[-]]<
    <[>[-]<[-]]<
    <[>[-]<[-]]<
    !? cur: c2; c0: 0; c2: column; c3: 0; c4: col == x ~ 1; c5: 0; c6: col == x; c7: 0; c8: x == col plus 1
    >>>>>>
    [<<+>>-]<<
    [<<+>>-]<<
    [<+>-]<
    !? cur: c3; c3 = col == x ~ 1 || col == x || col == x plus 1
    >+<
    [ ! print hash
      >[-] ! c4 = 0
      >>>>>>. ! print c9 (c19 or hash)
      <<<<<<<[-] ! c3 = 0
    ]
    >[ ! print dot
      >>>>>>>. ! print c10 (c20 or dot)
      <<<<<<<[-] ! c4 = 0
    ]
    
    <<+ ! c2 ← 1
    [>+>+<<-] ! c3 c4 ← c2
    >>[<<+>>-] ! c2 ← c4
    <<---------------------------------------- ! c2 ←~ 40
    [ ! if c2 wasn't 40
      [+] ! c2 = 0 (faster since we know c2 has overflowed)
      >[<+>-] ! c2 ← c3
    ]
    !?    cur: c2; c2: 0 (was 40); c3: 40
    !? or cur: c3; c2: c2 (before ~ 40); c3: 0 
    >[
      >++++++++++.[-] ! print new line
    ]<[-] !? cur: c3; c2: new c2 value; c3 c4: 0
    <<- ! c1 ←~ 1
  ]<
  !// end

  !? cur: c10
  <<<<<<, ! c4 ← input
]<<<<
!//end

! clear the memory (only leave c0 / result of part 1)
>[-]>[-]>[-]>>>>>>>[-]>>>>>>>>>>[-]>[-]
<<<<<<<<<<<<<<<<<<<<<

>++++++++++.[-]< ! print new line

[
  [
    - ! c0 ←~ 1
    >+ ! c1 ← 1
    [>+>+<<-] ! c2 c3 ← c1
    >>[<<+>>-] ! c1 ← c3
    +<---------- ! c2 ←~ 10; c3 ← 1
    [>-<[+]]>
    [ ! if c1 == 10
      <<[-]
      >>>+
      <[-]
    ]<<<
  ]
  >>>>[<<<<+>>>>-]<<<< ! c0 ← c4
  >++++++++++++++++++++++++++++++++++++++++++++++++ ! c1 ← '0'
  [>>>>>+<<<<<-]>>>>> ! c6 ← c1
  !? cur: c6
  [[>]>+<<[<]>-]
  >[>]+[<]
  <<<<<<
]
>>>>>>> !? cur: c7
[>]<[.<<]
! print new line
[-]++++++++++.
