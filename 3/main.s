# no single single quote in comments bc it fucks with discord code highlighting
.set SYS_READ, 0
.set SYS_WRITE, 1
.set SYS_OPEN, 2
.set SYS_CLOSE, 3
.set SYS_LSEEK, 8
.set SYS_EXIT, 60

.set STDOUT, 1

.bss 
.lcomm input_fd, 8
.lcomm read_buf, 256
.lcomm line_buf, 128
.lcomm total, 8
.lcomm total2, 8

.lcomm lines_buf, 128 * 3
.lcomm lines_len, 8

.lcomm line_inter, 128
.lcomm line_inter2, 128

# ascii table
.lcomm count, 127

.text

input_file: .asciz "input"
panic_aborting: .ascii "Aborting...\n"

panic_nofile: .asciz "Failed to open input file"

.globl _start

# print null terminated string in %rax and exits with -1
# string must be at least one character long
# This doesnt return
# cobblers: everything
panic:
	movq %rax, %rdi # rdi is char*
	xorq %rdx, %rdx # rdx is length
	_panic_loop:
		incq %rdi # next character
		incq %rdx # inc length

		movb (%rdi), %cl # get character
		cmpb $0, %cl # test if '\0'
		jnz _panic_loop

	movq %rax, %rsi
	movq $SYS_WRITE, %rax
	movq $STDOUT, %rdi
	syscall

	# print newline
	subq $1, %rsp
	movb $'\n', (%rsp)
	movq $SYS_WRITE, %rax
	movq $STDOUT, %rdi
	movq %rsp, %rsi
	movq $1, %rdx
	syscall
	addq $1, %rsp

	# print "Aborting..."
	movq $SYS_WRITE, %rax
	movq $STDOUT, %rdi
	leaq panic_aborting(%rip), %rsi
	movq $12, %rdx
	syscall

	# exit
	movq $SYS_EXIT, %rax
	movq $-1, %rdi
	syscall

# open input file, puts file descriptor in input_fd
# cobblers: rax, rdi, rsi, rcx, r11
open_input:
	
	movq $SYS_OPEN, %rax
	leaq input_file(%rip), %rdi
	# O_RDONLY
	movq $0, %rsi
	syscall

	cmpq $0, %rax
	jns _oi_success

	leaq panic_nofile(%rip), %rax
	call panic

	_oi_success:
	movq %rax, input_fd(%rip)

	ret

# compute intersection between two strings
# takes char * in rax and rdx
# intersection in rcx
# cobblers: rax rbx r8
string_intersection:
	pushq %rdx
	pushq %rcx
	xorq %rbx, %rbx

	_si_loop1:
		movb (%rax), %bl
		movq 8(%rsp), %rdx
		_si_loop2:
			movb (%rdx), %r8b
			cmpb %bl, %r8b
			jne _sil2_not_equal

			movb %r8b, (%rcx)
			incq %rcx

			_sil2_not_equal:
			incq %rdx
			cmpb $0, (%rdx)
			jnz _si_loop2
		incq %rax
		movb (%rax), %bl
		cmpq $0, %rbx
		jnz _si_loop1
	
	movb $0, (%rcx)

	popq %rcx
	popq %rdx
	ret

# process a line
# cobblers: nothing
process_line:
	pushq %rdi
	pushq %rdx
	pushq %rcx
	pushq %rsi
	pushq %r11
	pushq %rax
	pushq %r14
	pushq %r15
	
	leaq line_buf(%rip), %rdi # rdi is char*
	xorq %rdx, %rdx # rdx is length
	_pl_loop:
		incq %rdi # next character
		incq %rdx # inc length

		movb (%rdi), %cl # get character
		cmpb $0, %cl # test if '\0'
		jnz _pl_loop

	# length is in rdx
	shrq $1, %rdx # rdx is length of a pocket

	leaq line_buf(%rip), %rax # rax is a char* to the first pocket
	movq %rax, %rcx
	addq %rdx, %rcx # rcx is a char* to the second pocket

	xorq %rdi, %rdi # rdi is an index into the first pocket
	_pl_search_loop:
		xorq %rsi, %rsi # rsi is an index into the second pocket
		movb (%rax, %rdi, 1), %r11b # r11b is the current character (in pocket 1)
		_pl_search_loop_2:
			cmpb %r11b, (%rcx, %rsi, 1)
			jne _plsl2_not_equal

			# if char is a-z we need to subtract ('a' - 1) to get values [1-26]
			movq $'a' - 1, %r14
			# if char is A-Z we need to subtract ('A' - 27) to get values [27-52]
			movq $'A' - 27, %r15

			cmpb $'a', %r11b 
			# Choose the correct offset
			cmovb %r15, %r14

			# compute priority in r11b
			subb %r14b, %r11b
			# zero extend to quadword
			movzxb %r11b, %r11
			addq %r11, total(%rip)

			jmp _pl_search_end

			_plsl2_not_equal:
			incq %rsi
			cmpq %rdx, %rsi
			jb _pl_search_loop_2
		incq %rdi
		cmpq %rdx, %rdi
		jb _pl_search_loop

	_pl_search_end:

	movq lines_len(%rip), %rax
	leaq lines_buf(%rip), %rdx
	movq %rax, %rcx
	shlq $7, %rcx
	addq %rcx, %rdx
	leaq line_buf(%rip), %rcx
	_pl_copy_loop:
		movb (%rcx), %r8b
		movb %r8b, (%rdx)
		incq %rcx
		incq %rdx

		movb (%rcx), %r8b
		cmpb $0, %r8b
		jnz _pl_copy_loop
	movb $0, (%rdx)

	incq %rax
	movq %rax, lines_len(%rip)
	cmpq $3, %rax
	jne _pl_cont

	leaq lines_buf(%rip), %rax # load lines[0] in rax
	movq %rax, %rdx
	addq $128, %rdx # load lines[1] in rdx

	leaq line_inter2(%rip), %rcx # use line_inter2 as buffer
	call string_intersection

	leaq lines_buf(%rip), %rax
	addq $256, %rax # load lines[2] in rax
	leaq line_inter2(%rip), %rdx # load previous intersection in rdx
	leaq line_inter(%rip), %rcx # use line_inter as buffer
	call string_intersection

	xorq %rax, %rax
	movb (%rcx), %al

	# if char is a-z we need to subtract ('a' - 1) to get values [1-26]
	movq $'a' - 1, %r14
	# if char is A-Z we need to subtract ('A' - 27) to get values [27-52]
	movq $'A' - 27, %r15

	cmpb $'a', %al 
	# Choose the correct offset
	cmovb %r15, %r14

	# compute priority in al
	subb %r14b, %al
	addq %rax, total2(%rip)

	movq $0, lines_len(%rip)

	_pl_cont:

	popq %r15
	popq %r14
	popq %rax
	popq %r11
	popq %rsi
	popq %rcx
	popq %rdx
	popq %rdi
	ret

# print number in rax
# cobblers: everything
print_number:
	movq $10, %rcx
	leaq line_buf(%rip), %rdi
	_string_loop:
		xorq %rdx, %rdx
		# rax /= 10
		# rdx = rax % 10
		divq %rcx
		addb $'0', %dl # dl is (unsigned char)rdx or (uint8_t)rdx
		movb %dl, (%rdi)
		incq %rdi

		cmpq $0, %rax
		jnz _string_loop
	leaq line_buf(%rip), %rsi # left pointer in rsi
	movq %rdi, %rax
	subq %rsi, %rax # length in rax
	decq %rdi # right pointer in rdi
	_string_rev_loop:
		# swap both pointer value
		movb (%rsi), %r11b
		movb (%rdi), %r12b
		movb %r11b, (%rdi)
		movb %r12b, (%rsi)

		incq %rsi
		decq %rdi
		cmpq %rsi, %rdi
		ja _string_rev_loop
	
	leaq line_buf(%rip), %rsi
	movb $'\n', (%rsi, %rax, 1)
	incq %rax

	movq %rax, %rdx
	movq $SYS_WRITE, %rax
	movq $STDOUT, %rdi
	leaq line_buf(%rip), %rsi
	syscall

	ret

_start:
	call open_input

	_read_loop:
		movq $SYS_READ, %rax
		movq input_fd(%rip), %rdi
		leaq read_buf(%rip), %rsi
		movq $256, %rdx
		syscall

		cmpq $0, %rax
		jz _read_loop_end

		# &read_buf is in rsi
		movq %rsi, %rcx
		# r12 will hold a pointer to the last new line in read_buf
		movq %rsi, %r12 
		# rcx holds a pointer to the last character of read_buf
		addq %rax, %rcx
		decq %rcx

		leaq line_buf(%rip), %rdx

		_line_split_loop:
			movb (%rsi), %bl
			movb %bl, (%rdx)

			incq %rsi
			incq %rdx

			movb (%rsi), %bl
			cmpb $'\n', %bl
			jne _lsl_not_nl

			# weve got a line
			movq %rsi, %r12
			movb $0, (%rdx) # null terminate
			call process_line
			incq %rsi
			leaq line_buf(%rip), %rdx

			_lsl_not_nl:
			cmpq %rcx, %rsi
			jae _end_of_buf

			jmp _line_split_loop

		_end_of_buf:
		# sub pointer to last new line from pointer to end of buffer
		subq %r12, %rcx
		# rcx now holds the number of extra bytes that were read

		# seek back into the file
		movq $SYS_LSEEK, %rax
		movq input_fd(%rip), %rdi
		movq %rcx, %rsi
		negq %rsi
		# SEEK_CUR
		movq $1, %rdx
		syscall

		jmp _read_loop

	_read_loop_end:

	movq total(%rip), %rax
	call print_number
	movq total2(%rip), %rax
	call print_number

	movq $SYS_EXIT, %rax
	movq $0, %rdi
	syscall
