> // keep one zero cell to come back to
>>> >>> >>> > // [Get 10 cells (0..=9)]
,[>,] // [Get all input in cells 10..]
<[<]  // [go back to cell 9]
>[
  <<<<<< <[>+<-] <[>+<-] <[>+<-] <[-]> >>> >>> >>>
  [<+<+< <<< <<< <+> >>> >>> >>>-] // [move element to cell 7, 8 and -1]
  + << --- --- --- - // subtract 10 from cell 7 and set cell 9 to 1
  [
    // element was not a new line
    > ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- // remove 48 ('0') from cell 8
    > - < // set cell 9 to 0
    <[-]>[<+<<<<<<<+>>>>>>>>-]<[>+<-] [move cell 8 to cell 8 and 0]
    [-] +++ +++ +++ + // set cell 8 to 10
    < + // set cell 6 to 1
    >>[<-[>]<-]<[<] // [10 > cell 8 -> cell 6: cell 6 is one if the element was a digit]
    >>[-]>[-]<< // clear compare cells keep only boolean and set cell 9 to 1
    [
      // element was a digit save first and last
      <<<[-] // clear cell 3 (last digit)
      <<<[>>>+>+<<<<-]
      +>>[<<-]>>[>]<<<<<
      [>>>>[<<+>>-]<<<<[-]]
      >>>>[-]>>[-]
    ]
  ]
  >[-]
  >[
    // element was a new line
    <<<<<<[<<+>>-]<[<+++ +++ +++ +>-]>>>>>>>[-]<
  ]
  >>
]
<<< <<< <<
[-]+++ +++ +++ +
[> +++ +++ +++ ++ > +++ +++ +++ + > +++ +++ +++ ++ > +++ +++ +++ +++ > +++ > +++ ++ > +++ +++ > +++ <<<<<<<< -]
> ++ > --- > ++++ > ---- > ++ > - > -- > ++
<<<<<<<.[-]>.[-]>.[-]>.[-]>.[-]>.[-]>.[-]>.[-] <<< <<< <<<
<[-]>[>>+<<-]>>

[<[-]+>>+++++++++<[>[<-[>]<-]<[<]>[>->>>+<<]+++++++++<]<[>>+<<<<]>>[-]++++++++++
>>[<<->>-]<<++++++++++++++++++++++++++++++++++++++++++++++++[>>>>>+<<<<<-]>>>>>[
>>[>]>+<<[<]<-]>>[>]+[<]<<[<<<+>>>-]<<<]>>>>>>[>]<[.[-]<-<]<<<<<


<<<<

// reserve an additional 100 cells at the beginning

[[>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>+<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
<<<<-]<]

// setup data for string comparisons
>+++ +++ +++ +
[
  >>>> +++ +++ +++ ++ > +++ +++ +++ ++ > +++ +++ +++ + >
  >>>> +++ +++ +++ ++ > +++ +++ +++ ++ > +++ +++ +++ ++ >
  >>>> +++ +++ +++ ++ > +++ +++ +++ + > +++ +++ +++ ++ > +++ +++ +++ + > +++ +++ +++ + >
  >>>> +++ +++ +++ + > +++ +++ +++ ++ > +++ +++ +++ ++ > +++ +++ +++ ++ > 
  >>>> +++ +++ +++ + > +++ +++ +++ + > +++ +++ +++ ++ > +++ +++ +++ + >
  >>>> +++ +++ +++ ++ > +++ +++ +++ + > +++ +++ +++ +++ >
  >>>> +++ +++ +++ ++ > +++ +++ +++ + > +++ +++ +++ ++ > +++ +++ +++ + > +++ +++ +++ ++ >
  >>>> +++ +++ +++ + > +++ +++ +++ + > +++ +++ +++ + > +++ +++ +++ + > +++ +++ +++ ++ >
  >>>> +++ +++ +++ ++ > +++ +++ +++ + > +++ +++ +++ ++ > +++ +++ +++ + >
  <<<< <<<< <<<< <<<< <<<< <<<< <<<< <<<< <<<< <<<<<<<<< <<<<<<<<< <<<<<<<<< <<<<<< <<<
  -
]
> + > +         > +++   > + > > + >
> + > ++        > +++   > +++ +++ > +++ +++ +++ > + >
> + > +++       > +++++ > +++ +++ > +++ + > +++ + > + > + >
> + > ++++      > ++++  > ++ > + > +++ +++ + > +++ + >
> + > +++++     > ++++  > ++ > +++ ++ > +++ +++ ++ > + >
> + > ++++++    > +++   > +++ ++ > +++ ++ > >
> + > +++++++   > +++++ > +++ ++ > + > +++ +++ ++ > + > >
> + > ++++++++  > +++++ > + > +++ ++ > +++ > +++ + > +++ +++ >
> + > +++++++++ > ++++  > > +++ ++ > > + >

>>

// At this point we have 25 (0..=24) free cells (from the current cursor)
>>>>>> >>>>>> >>>>>> >>>>>> // go to cell 24

 // cell 25 / chariot
>[>]>
[
[<<[<]<+>>[>]>-]<<[<]>[>]+[<]<
[<+<+>>-] + < --- --- --- -
<<+>>[<<-<+>>>[-]]<<
[ // handle new line
  -<<<< <<<< [<<+>>-]<[<++++++++++>-] >>>>> >>>>
]
<
[ // not new line
  ->>>>-<
  [-]<[>+<-]>[<+<<+<<<<<<+>>>>>> >> > -]<<<
  ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
  < +++ +++ +++ + < + >>
  [<-[>]<-]<[<]> // check if digit
  >[-]+<
  // handle digit
  [
    <<<< ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
    <[-]>[>>+<<<+>-]
    >+<<<[>>>-<<[<]]>[>]>
    [>[<<<<+>>>>-]<-]>[-]
    >>>-<-
  ]
  >>[-]<[<+>-]<
  [ // else (no digit) check for spelled number
    >>>>[<<<+>>>-]<<
    // Fetch the next 4 characters
    >>>>>>[>]>[<<[<]<<<<+>>>>>[>]>-]<<[<]<<<<[<+>>>>>>[>]>+<<[<]<<<<-]
    >>>>>[>]>>[<<<[<]<<<+>>>>[>]>>-]<<<[<]<<<[<+>>>>>[>]>>+<<<[<]<<<-]
    >>>>[>]>>>[<<<<[<]<<+>>>[>]>>>-]<<<<[<]<<[<+>>>>[>]>>>+<<<<[<]<<-]
    >>>[>]>>>>[<<<<<[<]<+>>[>]>>>>-]<<<<<[<]<[<+>>>[>]>>>>+<<<<<[<]<-]
    
    <<<<<<[-]
    <<<< <<<< <<<< <<<< <<+++++++++
    [
      >
        <[<]<[<]>->
      [
        [[>]>[>]>+<<[<]<[<]>-]>[>]>[>]>[[>]>+<<[<]<[<]<[<]<+>>[>]>[>]>-]>[>]>[<+>-]<[<]<<[<]<[<]>
      ]
      <<[[>+<-]<]>+[>]>[>]

      >>> >>> >>> >>> >>> >>> // go the the first char of the string we have
      [<+>-]<[>+< <<<<<<<<<<<<<->>>>>>>>>>>>>-]>>
      [<<+>>-]<<[>>+<< <<<<<<<<<<<<->>>>>>>>>>>>-]>>>
      [<<<+>>>-]<<<[>>>+<<< <<<<<<<<<<<->>>>>>>>>>>-]>>>>
      [<<<<+>>>>-]<<<<[>>>>+<<<< <<<<<<<<<<->>>>>>>>>>-]>>>>>
      [<<<<<+>>>>>-]<<<<<[>>>>>+<<<<< <<<<<<<<<->>>>>>>>>-]>>>>>>
      <<< <<< <<< <<< <<< <<< <
      [<<<+>>>[-]]>
      [<<<<+>>>>[-]]>
      [<<<<<+>>>>>[-]]>
      [<<<<<<+>>>>>>[-]]>
      [<<<<<<<+>>>>>>>[-]]>
      <<<< <<
      [<<+>>-]<<-----
      >>+<<[>>-<<[-]]>>
      [
        >>>> >>>> > [-] < <<<< <<<<
        <[>>>> >> + >>>> + <<<< <<<< << -]
        >>>>>>> + >> [<<-]<<[<]>>
        [
          <[>>>+<<<-]>-
        ] <[-]<
        <<<<-
      ]
      [-]<[-]<<

      >> [-]>[-]>[-]>[-]>[-]>[-]>[-]> <<<<<<<<<<
      <[<]+[>]
      <-
    ]
    < - <<<< <<<< - <<<< <<<< < - <<<< <<<< < - <<<< <<< - <<<< <<<< - <<<< <<<< - <<<< <<<< < - <<<< <<< -
    >>>>>>>> >>>>>>>> >>>>>>>> >>>>>>>> >>>>>>>> >>>>>>>> >>>>>>>> >>>>>>>>
    >>>>>>>> >>>>>>>> >>>>
  ] >> [-] <[-]<[-]<[-]<[-]<[-]<[-]>>>>>>
]
>[-]>[-]>[-]>[-]>[-]
>[>]> #
]

[-]+++ +++ +++ +
[> + > +++ +++ +++ ++ > +++ +++ +++ + > +++ +++ +++ ++ > +++ +++ +++ +++ > +++ > +++ ++ > +++ +++ > +++ <<<<<<<<< -]
> > ++ > --- > ++++ > ---- > ++ >  > -- > ++
<<<<<<<<.[-]>.[-]>.[-]>.[-]>.[-]>.[-]>.[-]>.[-]>.[-] <<< <<< <<<

<<[-<]<<<< <<<< <<<< <<

[<[-]+>>+++++++++<[>[<-[>]<-]<[<]>[>->>>+<<]+++++++++<]<[>>+<<<<]>>[-]++++++++++
>>[<<->>-]<<++++++++++++++++++++++++++++++++++++++++++++++++[>>>>>+<<<<<-]>>>>>[
>>[>]>+<<[<]<-]>>[>]+[<]<<[<<<+>>>-]<<<]>>>>>>[>]<[.[-]<-<]<<<<<

++++++++++.
