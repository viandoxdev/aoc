module fingerprint_mod
        implicit none
        interface operator (==)
                module procedure fingerprint_eq
        end interface
        type fingerprint
                logical, dimension(7,30) :: rows
                integer :: jet_index
                integer :: rock_index
        end type
contains
        function fingerprint_eq(a, b)
                implicit none
                logical :: fingerprint_eq
                type(fingerprint), intent(in) :: a
                type(fingerprint), intent(in) :: b
                fingerprint_eq = a%jet_index == b%jet_index .and. a%rock_index == b%rock_index .and. all(a%rows .eqv. b%rows)
        end function fingerprint_eq
end module fingerprint_mod

program main
        use fingerprint_mod
        implicit none

        integer, allocatable :: jet(:) ! jet pattern (heap)
        integer              :: jet_len ! jet pattern length
        integer              :: jet_index ! jet pattern index

        complex, dimension(6,5) :: rocks ! rocks matrix
        integer(kind=8)         :: rock ! rock count

        logical, allocatable :: grid(:,:)
        integer              :: grid_height
        integer              :: roof

        integer :: part1
        integer(kind=8) :: part2

        type(fingerprint) :: fa
        type(fingerprint) :: fb
        integer(kind=8) :: cycle_length
        integer(kind=8) :: cycle_height
        integer(kind=8) :: cycle_count

        grid_height = 2 ** 20

        allocate(jet(16384))
        allocate(grid(0:8,0:grid_height))

        !       (x, y), (x, y), (x, y), (x, y), (x, y)? (i, h)
        rocks = reshape([                                       &
                (2, 0), (3, 0), (4, 0), (5, 0), (5, 0), (0, 0), & 
                (3, 0), (2, 1), (3, 1), (4, 1), (3, 2), (1, 2), &
                (2, 0), (3, 0), (4, 0), (4, 1), (4, 2), (2, 2), &
                (2, 0), (2, 1), (2, 2), (2, 3), (2, 3), (3, 3), &
                (2, 0), (3, 0), (2, 1), (3, 1), (3, 1), (4, 1)  &
        ], [6,5])

        call parse_input(jet_len, jet)
        jet_index = 1
        ! clear screen
        print *, achar(27) // "[2J"

        ! part 1

        call reset()
        do rock = 1,2022
                call drop()
        end do
        part1 = roof

        ! part 2
        ! no reset because we want some rocks already (cycle doesn't begin immediately)

        fa = fp() ! fingerprint current state
        do while(.not. fa == fb)
                call drop()
                call draw()
                fb = fp()
                rock = rock + 1
        end do
        rock = rock - 1
        cycle_length = rock - 2022 ! - 2022 because that's where the cycle started
        cycle_height = roof - part1

        cycle_count = (1000000000000 - rock) / cycle_length
        rock = rock + cycle_count * cycle_length

        print *, ""
        print *, "Skipped", cycle_count * cycle_length

        do while(rock < 1000000000000)
                call drop()
                rock = rock + 1
        end do
        
        part2 = roof + cycle_count * cycle_height

        print *, "Part1", part1
        print *, "Part2", part2
contains
        subroutine parse_input(len, out)
                integer, intent(out) :: len
                integer, intent(out) :: out(:)
                integer :: i
                integer :: j
                integer :: input
                character :: c
                character(256) :: buffer

                open(newunit=input, file="input", status="old", action="read")
                j = 1
                i = 1
                c = '<'
                do while(c == '<' .or. c == '>')
                        read(input,'(A$)') buffer
                        do j = 1,256
                                c = buffer(j:j)
                                if(c == '<') then
                                        out(i) = -1
                                else if (c == '>') then
                                        out(i) = 1
                                else
                                        exit
                                end if
                                i = i + 1
                        end do
                end do
                close(input)

                len = i - 1
        end subroutine parse_input
        subroutine reset()
                grid(:,:) = .false.
                grid(:, 0) = .true.
                grid(0, :) = .true.
                grid(8, :) = .true.
                roof = 0
                jet_index = 1
                do while(.not. realpart(rocks(6, 1)) == 0)
                        rocks = cshift(rocks, shift=1, dim=2)
                end do
        end subroutine reset
        subroutine draw()
                implicit none

                integer :: top
                integer :: y
                integer :: x
                integer :: index
                character(len=18) line
                ! makes sure that the window from top to top - 30 is fully in the grid
                top = min(max(roof - 25, 0) + 30, grid_height)
                line(1:2) = '||'
                line(17:18) = '||'
                print *, achar(27) // "[;H"
                print *, "=================="
                do y = 0, 29
                        do x = 1, 7
                                index = x * 2 + 1
                                if (grid(x,top - y)) then
                                        line(index:index+1) = '[]'
                                else
                                        line(index:index+1) = '. '
                                end if
                        end do

                        print *, line
                end do
                print *, "=================="
                print *, roof
        end subroutine draw
        subroutine drop()
                implicit none
                complex :: offset
                complex :: jet_offset
                integer :: top

                complex, dimension(5) :: blocks
                integer, dimension(5) :: x
                integer, dimension(5) :: y

                offset = cmplx(1, roof + 4)

                do while (.not. collides(offset))
                        jet_offset = cmplx(jet(jet_index), 0)

                        if (.not. collides(offset + jet_offset)) then
                                offset = offset + jet_offset
                        end if

                        jet_index = jet_index + 1
                        if (jet_index > jet_len) then
                                jet_index = 1
                        endif

                        offset = offset - (0, 1)
                end do
                offset = offset + (0, 1)

                blocks = rocks(1:5, 1) + offset
                x = realpart(blocks)
                y = imagpart(blocks)

                grid(x(1), y(1)) = .true.
                grid(x(2), y(2)) = .true.
                grid(x(3), y(3)) = .true.
                grid(x(4), y(4)) = .true.
                grid(x(5), y(5)) = .true.

                top = imagpart(offset + rocks(6, 1))
                roof = max(roof, top)

                rocks = cshift(rocks, shift=1, dim=2)
        end subroutine drop
        function fp()
                type(fingerprint) :: fp
                fp%rows = grid(1:7,roof-29:roof)
                ! this is technically off by one, but since its always off by one it doesn't matter
                fp%rock_index = realpart(rocks(6, 1))
                fp%jet_index = jet_index
        end function fp
        function collides(offset)
                implicit none
                logical :: collides
                complex :: offset

                complex, dimension(5) :: blocks
                integer, dimension(5) :: x
                integer, dimension(5) :: y

                blocks = rocks(1:5, 1) + offset
                x = realpart(blocks)
                y = imagpart(blocks)

                collides = &
                        grid(x(1), y(1)) .or. &
                        grid(x(2), y(2)) .or. &
                        grid(x(3), y(3)) .or. &
                        grid(x(4), y(4)) .or. &
                        grid(x(5), y(5))
        end function collides
end program main
