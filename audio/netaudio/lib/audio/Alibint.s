	.file	"Alibint.c"
gcc2_compiled.:
___gnu_compiled_c:
.text
LC0:
	.ascii "@(#)Network Audio System Release ?.? ALPHA\0"
.data
	.align 2
_release:
	.long LC0
	.align 2
_padlength:
	.long 0
	.long 3
	.long 2
	.long 1
	.align 1
__dummy_request:
	.byte 0
	.byte 0
	.word 0
.text
	.align 4
__AuWaitForWritable:
	pushl %ebp
	movl %esp,%ebp
	subl $2068,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%edi
	movl $0,-8(%ebp)
	movl $0,-4(%ebp)
	movl $0,-16(%ebp)
	movl $0,-12(%ebp)
	.align 2,0x90
L6:
	movl 8(%edi),%edx
	sarl $5,%edx
	movl 8(%edi),%ecx
	andl $31,%ecx
	movl $1,%eax
	sall %cl,%eax
	orl %eax,-8(%ebp,%edx,4)
	movl 8(%edi),%edx
	sarl $5,%edx
	movl 8(%edi),%ecx
	andl $31,%ecx
	movl $1,%eax
	sall %cl,%eax
	orl %eax,-16(%ebp,%edx,4)
	.align 2,0x90
L8:
	pushl $0
	pushl $0
	leal -16(%ebp),%eax
	pushl %eax
	leal -8(%ebp),%eax
	pushl %eax
	movl 8(%edi),%eax
	incl %eax
	pushl %eax
	call _select
	movl %eax,%ebx
	addl $20,%esp
	testl %ebx,%ebx
	jge L10
	cmpl $4,_errno
	je L10
	pushl %edi
	call __AuIOError
	addl $4,%esp
L10:
	testl %ebx,%ebx
	jle L8
	cmpl $0,-8(%ebp)
	jne L13
	cmpl $0,-4(%ebp)
	je L12
L13:
	leal -2068(%ebp),%eax
	pushl %eax
	pushl $1074030207
	movl 8(%edi),%ecx
	pushl %ecx
	call _ioctl
	addl $12,%esp
	testl %eax,%eax
	jge L14
	pushl %edi
	call __AuIOError
	addl $4,%esp
L14:
	movl -2068(%ebp),%esi
	cmpl $31,%esi
	jg L15
	movl $32,%esi
L15:
	cmpl $2048,%esi
	jle L16
	movl $2048,%esi
L16:
	movl %esi,%eax
	testl %esi,%esi
	jge L17
	leal 31(%esi),%eax
L17:
	movl %eax,%esi
	andl $-32,%esi
	pushl %esi
	leal -2064(%ebp),%ebx
	pushl %ebx
	pushl %edi
	call __AuRead
	addl $12,%esp
L28:
	testl %esi,%esi
	jle L12
	cmpb $1,(%ebx)
	jne L21
	movl %esi,-2068(%ebp)
	pushl $1
	leal -2068(%ebp),%eax
	pushl %eax
	pushl %ebx
	pushl %ebx
	pushl %edi
	call __AuAsyncReply
	movl %eax,%ebx
	movl -2068(%ebp),%esi
	addl $20,%esp
	jmp L28
	.align 4,0x90
L21:
	cmpb $0,(%ebx)
	jne L23
	pushl %ebx
	pushl %edi
	call __AuError
	addl $8,%esp
	jmp L24
	.align 4,0x90
L23:
	pushl $1
	pushl %ebx
	pushl %edi
	call __AuEnq
	addl $12,%esp
L24:
	addl $32,%ebx
	addl $-32,%esi
	jmp L28
	.align 4,0x90
L12:
	cmpl $0,-16(%ebp)
	jne L5
	cmpl $0,-12(%ebp)
	je L6
L5:
	leal -2080(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
__AuWaitForReadable:
	pushl %ebp
	movl %esp,%ebp
	subl $8,%esp
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%esi
	movl $0,-8(%ebp)
	movl $0,-4(%ebp)
	.align 2,0x90
L30:
	movl 8(%esi),%edx
	sarl $5,%edx
	movl 8(%esi),%ecx
	andl $31,%ecx
	movl $1,%eax
	sall %cl,%eax
	orl %eax,-8(%ebp,%edx,4)
	pushl $0
	pushl $0
	pushl $0
	leal -8(%ebp),%eax
	pushl %eax
	movl 8(%esi),%eax
	incl %eax
	pushl %eax
	call _select
	movl %eax,%ebx
	addl $20,%esp
	cmpl $-1,%ebx
	jne L32
	cmpl $4,_errno
	je L32
	pushl %esi
	call __AuIOError
	addl $4,%esp
L32:
	testl %ebx,%ebx
	jle L30
	leal -16(%ebp),%esp
	popl %ebx
	popl %esi
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl __AuFlush
__AuFlush:
	pushl %ebp
	movl %esp,%ebp
	subl $4,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%edi
	testb $1,1152(%edi)
	jne L34
	movl 88(%edi),%esi
	subl 84(%edi),%esi
	movl %esi,%ebx
	movl 84(%edi),%eax
	movl %eax,88(%edi)
	movl %eax,-4(%ebp)
L50:
	testl %esi,%esi
	je L37
	movl $0,_errno
	pushl %ebx
	movl -4(%ebp),%edx
	pushl %edx
	movl 8(%edi),%edx
	pushl %edx
	call _write
	addl $12,%esp
	testl %eax,%eax
	jl L38
	subl %eax,%esi
	movl %esi,%ebx
	addl %eax,-4(%ebp)
	jmp L50
	.align 4,0x90
L38:
	cmpl $35,_errno
	je L41
	jne L40
L41:
	pushl %edi
	call __AuWaitForWritable
	jmp L49
	.align 4,0x90
L40:
	cmpl $40,_errno
	jne L43
	cmpl $1,%ebx
	jle L44
	sarl $1,%ebx
	jmp L50
	.align 4,0x90
L44:
	pushl %edi
	call __AuWaitForWritable
	jmp L49
	.align 4,0x90
L43:
	cmpl $4,_errno
	je L50
	pushl %edi
	call __AuIOError
L49:
	addl $4,%esp
	jmp L50
	.align 4,0x90
L37:
	movl $__dummy_request,80(%edi)
L34:
	leal -16(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	ret
.lcomm _zero_time.10,8
	.align 4
.globl __AuEventsQueued
__AuEventsQueued:
	pushl %ebp
	movl %esp,%ebp
	subl $2060,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%edi
	cmpl $2,12(%ebp)
	jne L52
	pushl %edi
	call __AuFlush
	addl $4,%esp
	cmpl $0,68(%edi)
	jne L69
L52:
	testb $1,1152(%edi)
	jne L69
	leal -2052(%ebp),%ebx
	pushl %ebx
	pushl $1074030207
	movl 8(%edi),%ecx
	pushl %ecx
	call _ioctl
	addl $12,%esp
	testl %eax,%eax
	jge L55
	pushl %edi
	call __AuIOError
	addl $4,%esp
L55:
	cmpl $0,-2052(%ebp)
	jne L56
	cmpl $0,68(%edi)
	jne L56
	movl 1160(%edi),%eax
	incl %eax
	movl %eax,1160(%edi)
	cmpl $255,%eax
	jle L56
	movl $0,1160(%edi)
	movl $0,-2060(%ebp)
	movl $0,-2056(%ebp)
	movl 8(%edi),%edx
	sarl $5,%edx
	movl 8(%edi),%ecx
	andl $31,%ecx
	movl $1,%eax
	sall %cl,%eax
	orl %eax,-2060(%ebp,%edx,4)
	pushl $_zero_time.10
	pushl $0
	pushl $0
	leal -2060(%ebp),%eax
	pushl %eax
	movl 8(%edi),%eax
	incl %eax
	pushl %eax
	call _select
	movl %eax,-2052(%ebp)
	addl $20,%esp
	testl %eax,%eax
	je L56
	jle L58
	pushl %ebx
	pushl $1074030207
	movl 8(%edi),%ecx
	pushl %ecx
	call _ioctl
	addl $12,%esp
	testl %eax,%eax
	jge L59
	pushl %edi
	call __AuIOError
	addl $4,%esp
L59:
	cmpl $0,-2052(%ebp)
	jne L56
	movl $32,-2052(%ebp)
	jmp L56
	.align 4,0x90
L58:
	testl %eax,%eax
	jge L56
	cmpl $4,_errno
	je L56
	pushl %edi
	call __AuIOError
	addl $4,%esp
L56:
	movl -2052(%ebp),%esi
	testl %esi,%esi
	je L69
	cmpl $31,%esi
	jg L64
	movl $32,%esi
	jmp L65
	.align 4,0x90
L64:
	cmpl $2048,%esi
	jle L65
	movl $2048,%esi
L65:
	movl %esi,%eax
	testl %esi,%esi
	jge L67
	leal 31(%esi),%eax
L67:
	movl %eax,%esi
	andl $-32,%esi
	movl $0,1160(%edi)
	pushl %esi
	leal -2048(%ebp),%ebx
	pushl %ebx
	pushl %edi
	call __AuRead
	addl $12,%esp
L77:
	testl %esi,%esi
	jle L69
	cmpb $1,(%ebx)
	jne L71
	movl %esi,-2052(%ebp)
	pushl $1
	leal -2052(%ebp),%eax
	pushl %eax
	pushl %ebx
	pushl %ebx
	pushl %edi
	call __AuAsyncReply
	movl %eax,%ebx
	movl -2052(%ebp),%esi
	addl $20,%esp
	jmp L77
	.align 4,0x90
L71:
	cmpb $0,(%ebx)
	jne L73
	pushl %ebx
	pushl %edi
	call __AuError
	addl $8,%esp
	jmp L74
	.align 4,0x90
L73:
	pushl $1
	pushl %ebx
	pushl %edi
	call __AuEnq
	addl $12,%esp
L74:
	addl $32,%ebx
	addl $-32,%esi
	jmp L77
	.align 4,0x90
L69:
	movl 68(%edi),%eax
	leal -2072(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl __AuReadEvents
__AuReadEvents:
	pushl %ebp
	movl %esp,%ebp
	subl $2056,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%edi
	movl $1,-2056(%ebp)
	.align 2,0x90
L79:
	leal -2052(%ebp),%eax
	pushl %eax
	pushl $1074030207
	movl 8(%edi),%edx
	pushl %edx
	call _ioctl
	addl $12,%esp
	testl %eax,%eax
	jge L82
	pushl %edi
	call __AuIOError
	addl $4,%esp
L82:
	movl -2052(%ebp),%esi
	cmpl $31,%esi
	jg L83
	movl $32,%esi
	cmpl $0,-2056(%ebp)
	je L83
	movl 68(%edi),%ebx
	pushl %edi
	call __AuFlush
	addl $4,%esp
	cmpl %ebx,68(%edi)
	jne L78
	movl $0,-2056(%ebp)
L83:
	cmpl $2048,%esi
	jle L86
	movl $2048,%esi
L86:
	movl %esi,%eax
	testl %esi,%esi
	jge L87
	leal 31(%esi),%eax
L87:
	movl %eax,%esi
	andl $-32,%esi
	pushl %esi
	leal -2048(%ebp),%ebx
	pushl %ebx
	pushl %edi
	call __AuRead
	addl $12,%esp
L96:
	testl %esi,%esi
	jle L81
	cmpb $1,(%ebx)
	jne L91
	movl %esi,-2052(%ebp)
	pushl $1
	leal -2052(%ebp),%eax
	pushl %eax
	pushl %ebx
	pushl %ebx
	pushl %edi
	call __AuAsyncReply
	movl %eax,%ebx
	movl -2052(%ebp),%esi
	addl $20,%esp
	jmp L96
	.align 4,0x90
L91:
	cmpb $0,(%ebx)
	jne L93
	pushl %ebx
	pushl %edi
	call __AuError
	addl $8,%esp
	jmp L94
	.align 4,0x90
L93:
	pushl $1
	pushl %ebx
	pushl %edi
	call __AuEnq
	addl $12,%esp
L94:
	addl $32,%ebx
	addl $-32,%esi
	jmp L96
	.align 4,0x90
L81:
	cmpl $0,56(%edi)
	je L79
L78:
	leal -2068(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl __AuRead
__AuRead:
	pushl %ebp
	movl %esp,%ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%esi
	movl 12(%ebp),%edi
	movl 16(%ebp),%ebx
	testb $1,1152(%esi)
	jne L97
	testl %ebx,%ebx
	je L97
	movl $0,_errno
	.align 2,0x90
L100:
	pushl %ebx
	pushl %edi
	movl 8(%esi),%edx
	pushl %edx
	call _read
	addl $12,%esp
	cmpl %ebx,%eax
	je L97
	testl %eax,%eax
	jle L102
	subl %eax,%ebx
	addl %eax,%edi
	jmp L100
	.align 4,0x90
L102:
	cmpl $35,_errno
	je L105
	jne L104
L105:
	pushl %esi
	call __AuWaitForReadable
	movl $0,_errno
	addl $4,%esp
	jmp L100
	.align 4,0x90
L104:
	testl %eax,%eax
	jne L107
	movl $32,_errno
	jmp L110
	.align 4,0x90
L107:
	cmpl $4,_errno
	je L100
L110:
	pushl %esi
	call __AuIOError
	addl $4,%esp
	jmp L100
	.align 4,0x90
L97:
	leal -12(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl __AuReadPad
__AuReadPad:
	pushl %ebp
	movl %esp,%ebp
	subl $20,%esp
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%esi
	movl 16(%ebp),%ebx
	testb $1,1152(%esi)
	jne L111
	testl %ebx,%ebx
	je L111
	movl %ebx,-12(%ebp)
	movl 12(%ebp),%ecx
	movl %ecx,-16(%ebp)
	movl %ebx,%eax
	andl $3,%eax
	movl _padlength(,%eax,4),%eax
	movl %eax,-4(%ebp)
	movl %ebp,%ecx
	addl $-20,%ecx
	movl %ecx,-8(%ebp)
	addl -4(%ebp),%ebx
	movl $0,_errno
	.align 2,0x90
L114:
	pushl $2
	leal -16(%ebp),%eax
	pushl %eax
	movl 8(%esi),%ecx
	pushl %ecx
	call _readv
	movl %eax,%edx
	addl $12,%esp
	cmpl %ebx,%edx
	je L111
	testl %edx,%edx
	jle L116
	subl %edx,%ebx
	movl -12(%ebp),%eax
	subl %edx,%eax
	movl %eax,-12(%ebp)
	jns L117
	addl %eax,-4(%ebp)
	movl -12(%ebp),%ecx
	subl %ecx,-8(%ebp)
	movl $0,-12(%ebp)
	jmp L114
	.align 4,0x90
L117:
	addl %edx,-16(%ebp)
	jmp L114
	.align 4,0x90
L116:
	cmpl $35,_errno
	je L121
	jne L120
L121:
	pushl %esi
	call __AuWaitForReadable
	movl $0,_errno
	addl $4,%esp
	jmp L114
	.align 4,0x90
L120:
	testl %edx,%edx
	jne L123
	movl $32,_errno
	jmp L126
	.align 4,0x90
L123:
	cmpl $4,_errno
	je L114
L126:
	pushl %esi
	call __AuIOError
	addl $4,%esp
	jmp L114
	.align 4,0x90
L111:
	leal -28(%ebp),%esp
	popl %ebx
	popl %esi
	movl %ebp,%esp
	popl %ebp
	ret
.data
_pad.19:
	.byte 0
	.byte 0
	.byte 0
.text
	.align 4
.globl __AuSend
__AuSend:
	pushl %ebp
	movl %esp,%ebp
	subl $52,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl $0,-28(%ebp)
	movl 8(%ebp),%ebx
	movl 88(%ebx),%ecx
	subl 84(%ebx),%ecx
	movl %ecx,-32(%ebp)
	movl 16(%ebp),%eax
	andl $3,%eax
	movl _padlength(,%eax,4),%eax
	movl %eax,-36(%ebp)
	movl %ecx,%eax
	addl 16(%ebp),%eax
	addl -36(%ebp),%eax
	movl %eax,-40(%ebp)
	movl -40(%ebp),%edi
	testb $1,1152(%ebx)
	jne L127
	testl %edi,%edi
	je L130
	leal -24(%ebp),%ebx
	movl %ebx,-44(%ebp)
	.align 2,0x90
L150:
	movl %edi,-52(%ebp)
	xorl %esi,%esi
	movl -32(%ebp),%edx
	subl -28(%ebp),%edx
	cmpl %edi,%edx
	jle L131
	movl %edi,%edx
L131:
	testl %edx,%edx
	jg L132
	negl %edx
	movl %edx,-48(%ebp)
	jmp L133
	.align 4,0x90
L132:
	movl %edx,-20(%ebp)
	movl -28(%ebp),%ebx
	movl 8(%ebp),%ecx
	addl 84(%ecx),%ebx
	movl %ebx,-24(%ebp)
	movl $1,%esi
	movl %edi,%ecx
	subl %edx,%ecx
	movl %ecx,-52(%ebp)
	movl $0,-48(%ebp)
L133:
	movl 16(%ebp),%edx
	subl -48(%ebp),%edx
	cmpl %edx,-52(%ebp)
	jge L134
	movl -52(%ebp),%edx
L134:
	testl %edx,%edx
	jg L135
	negl %edx
	movl %edx,-48(%ebp)
	jmp L136
	.align 4,0x90
L135:
	leal 0(,%esi,8),%eax
	movl -44(%ebp),%ebx
	movl %edx,4(%eax,%ebx)
	movl -48(%ebp),%ecx
	addl 12(%ebp),%ecx
	movl %ecx,(%eax,%ebx)
	incl %esi
	subl %edx,-52(%ebp)
	movl $0,-48(%ebp)
L136:
	movl -36(%ebp),%edx
	subl -48(%ebp),%edx
	cmpl %edx,-52(%ebp)
	jge L137
	movl -52(%ebp),%edx
L137:
	testl %edx,%edx
	jle L139
	leal 0(,%esi,8),%eax
	movl -44(%ebp),%ebx
	movl %edx,4(%eax,%ebx)
	movl -48(%ebp),%ecx
	addl $_pad.19,%ecx
	movl %ecx,(%eax,%ebx)
	incl %esi
L139:
	movl $0,_errno
	pushl %esi
	movl -44(%ebp),%ebx
	pushl %ebx
	movl 8(%ebp),%ecx
	movl 8(%ecx),%ecx
	pushl %ecx
	call _writev
	movl %eax,%edx
	addl $12,%esp
	testl %edx,%edx
	jl L140
	addl %edx,-28(%ebp)
	subl %edx,-40(%ebp)
	movl -40(%ebp),%edi
	jmp L129
	.align 4,0x90
L140:
	cmpl $35,_errno
	je L143
	jne L142
L143:
	movl 8(%ebp),%ebx
	pushl %ebx
	call __AuWaitForWritable
	jmp L151
	.align 4,0x90
L142:
	cmpl $40,_errno
	jne L145
	cmpl $1,%edi
	jle L146
	sarl $1,%edi
	jmp L129
	.align 4,0x90
L146:
	movl 8(%ebp),%ecx
	pushl %ecx
	call __AuWaitForWritable
	jmp L151
	.align 4,0x90
L145:
	cmpl $4,_errno
	je L129
	movl 8(%ebp),%ebx
	pushl %ebx
	call __AuIOError
L151:
	addl $4,%esp
L129:
	cmpl $0,-40(%ebp)
	jne L150
L130:
	movl 8(%ebp),%ecx
	movl 84(%ecx),%ebx
	movl %ebx,88(%ecx)
	movl $__dummy_request,80(%ecx)
L127:
	leal -64(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	ret
LC1:
	.ascii "audiolib: resource ID allocation space exhausted!\12\0"
	.align 4
.globl __AuAllocID
__AuAllocID:
	pushl %ebp
	movl %esp,%ebp
	pushl %ebx
	movl 8(%ebp),%ebx
	movl 40(%ebx),%ecx
	movl 36(%ebx),%eax
	sall %cl,%eax
	cmpl %eax,32(%ebx)
	jb L153
	incl 36(%ebx)
	addl 28(%ebx),%eax
	jmp L155
	.align 4,0x90
L153:
	cmpl $268435456,%eax
	je L154
	pushl $LC1
	pushl $___sF+168
	call _fprintf
	movl $268435456,%eax
	movl 40(%ebx),%ecx
	movl $268435456,%edx
	shrl %cl,%edx
	movl %edx,36(%ebx)
L154:
L155:
	movl -4(%ebp),%ebx
	movl %ebp,%esp
	popl %ebp
	ret
LC2:
	.ascii "audiolib: sequence lost (0x%lx > 0x%lx) in reply type 0x%x!\12\0"
	.align 4
.globl __AuSetLastRequestRead
__AuSetLastRequestRead:
	pushl %ebp
	movl %esp,%ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%esi
	movl 12(%ebp),%ecx
	movl 72(%esi),%edx
	andl $-65536,%edx
	movzwl 2(%ecx),%eax
	movl %edx,%ebx
	orl %eax,%ebx
	movl 72(%esi),%eax
L161:
	cmpl %eax,%ebx
	jae L158
	addl $65536,%ebx
	cmpl %ebx,76(%esi)
	jae L161
	movzbl (%ecx),%eax
	pushl %eax
	movl 76(%esi),%edi
	pushl %edi
	pushl %ebx
	pushl $LC2
	pushl $___sF+168
	call _fprintf
	addl $-65536,%ebx
L158:
	movl %ebx,72(%esi)
	movl %ebx,%eax
	leal -12(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl __AuReply
__AuReply:
	pushl %ebp
	movl %esp,%ebp
	subl $24,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 12(%ebp),%edi
	movl 8(%ebp),%edx
	movl 76(%edx),%edx
	movl %edx,-16(%ebp)
	cmpl $0,24(%ebp)
	jne L163
	leal -4(%ebp),%ecx
	movl %ecx,24(%ebp)
L163:
	movl 24(%ebp),%edx
	movl $0,(%edx)
	movl 8(%ebp),%ecx
	testb $1,1152(%ecx)
	jne L190
	movl 8(%ebp),%edx
	pushl %edx
	call __AuFlush
	addl $4,%esp
	.align 2,0x90
L165:
	pushl $32
	pushl %edi
	movl 8(%ebp),%ecx
	pushl %ecx
	call __AuRead
	addl $12,%esp
	movzbl (%edi),%eax
	testl %eax,%eax
	je L177
	cmpl $1,%eax
	jne L187
	movzwl 2(%edi),%edx
	movzwl -16(%ebp),%eax
	cmpl %eax,%edx
	jne L169
	movl -16(%ebp),%ecx
	movl 8(%ebp),%edx
	movl %ecx,72(%edx)
	jmp L170
	.align 4,0x90
L169:
	movl $32,-8(%ebp)
	pushl $0
	leal -8(%ebp),%eax
	pushl %eax
	pushl %edi
	pushl %edi
	movl 8(%ebp),%edx
	pushl %edx
	call __AuAsyncReply
	addl $20,%esp
	cmpl %edi,%eax
	jne L165
L170:
	cmpl $0,16(%ebp)
	jne L172
	cmpl $0,20(%ebp)
	je L191
	cmpl $0,4(%edi)
	je L191
	movl 4(%edi),%edi
	leal 0(,%edi,4),%eax
	pushl %eax
	movl 8(%ebp),%ecx
	pushl %ecx
	call __AuEatData
	jmp L191
	.align 4,0x90
L172:
	movl 16(%ebp),%edx
	cmpl %edx,4(%edi)
	jne L174
	leal 0(,%edx,4),%eax
	pushl %eax
	leal 32(%edi),%eax
	pushl %eax
	movl 8(%ebp),%ecx
	pushl %ecx
	call __AuRead
L191:
	movl $1,%eax
	jmp L189
	.align 4,0x90
L174:
	movl 16(%ebp),%edx
	cmpl %edx,4(%edi)
	jbe L175
	leal 0(,%edx,4),%eax
	pushl %eax
	leal 32(%edi),%eax
	pushl %eax
	movl 8(%ebp),%ecx
	pushl %ecx
	call __AuRead
	addl $12,%esp
	cmpl $0,20(%ebp)
	je L191
	movl 4(%edi),%eax
	subl 16(%ebp),%eax
	sall $2,%eax
	pushl %eax
	movl 8(%ebp),%edx
	pushl %edx
	call __AuEatData
	jmp L191
	.align 4,0x90
L175:
	movl 4(%edi),%ecx
	leal 0(,%ecx,4),%eax
	pushl %eax
	leal 32(%edi),%eax
	pushl %eax
	movl 8(%ebp),%edx
	pushl %edx
	call __AuRead
	movl 8(%ebp),%ecx
	pushl %ecx
	call __AuIOError
	movl 24(%ebp),%edx
L190:
	movl $13,(%edx)
	xorl %eax,%eax
	jmp L189
	.align 4,0x90
L177:
	xorl %esi,%esi
	movl $0,-12(%ebp)
	movl %edi,-20(%ebp)
	movzbl 1(%edi),%edx
	movl 24(%ebp),%ecx
	movl %edx,(%ecx)
	pushl %edi
	movl 8(%ebp),%ecx
	pushl %ecx
	call __AuSetLastRequestRead
	movl %eax,-24(%ebp)
	addl $8,%esp
	movl -16(%ebp),%edx
	cmpl %edx,%eax
	jne L178
	leal -4(%ebp),%eax
	cmpl %eax,24(%ebp)
	je L178
	xorl %eax,%eax
	jmp L189
	.align 4,0x90
L178:
	movl 8(%ebp),%ecx
	movl 120(%ecx),%ebx
	jmp L192
	.align 4,0x90
	.align 2,0x90
L184:
	cmpl $0,24(%ebx)
	je L182
	leal -12(%ebp),%eax
	pushl %eax
	leal 4(%ebx),%eax
	pushl %eax
	movl -20(%ebp),%edx
	pushl %edx
	movl 8(%ebp),%ecx
	pushl %ecx
	movl 24(%ebx),%eax
	call *%eax
	movl %eax,%esi
	addl $16,%esp
L182:
	movl (%ebx),%ebx
L192:
	testl %esi,%esi
	jne L185
	testl %ebx,%ebx
	jne L184
	testl %esi,%esi
	jne L185
	movl -20(%ebp),%edx
	pushl %edx
	movl 8(%ebp),%ecx
	pushl %ecx
	call __AuError
	movl $0,-12(%ebp)
	addl $8,%esp
L185:
	movl -16(%ebp),%edx
	cmpl %edx,-24(%ebp)
	jne L165
	movl -12(%ebp),%eax
	jmp L189
	.align 4,0x90
L187:
	pushl $2
	pushl %edi
	movl 8(%ebp),%ecx
	pushl %ecx
	call __AuEnq
	addl $12,%esp
	jmp L165
	.align 4,0x90
L189:
	leal -36(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	ret
LC3:
	.ascii "audiolib: unexpected async reply (sequence 0x%lx)!\12\0"
	.align 4
__AuAsyncReply:
	pushl %ebp
	movl %esp,%ebp
	subl $4,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 16(%ebp),%esi
	xorl %edi,%edi
	movl 12(%ebp),%ecx
	pushl %ecx
	movl 8(%ebp),%edx
	pushl %edx
	call __AuSetLastRequestRead
	movl 12(%ebp),%ecx
	movl 4(%ecx),%ecx
	leal 0(,%ecx,4),%eax
	leal 32(%eax),%ebx
	movl 8(%ebp),%edx
	movl 1148(%edx),%eax
	addl $8,%esp
	testl %eax,%eax
	je L195
	.align 2,0x90
L198:
	movl (%eax),%ecx
	movl %ecx,-4(%ebp)
	movl 8(%eax),%edx
	pushl %edx
	movl 20(%ebp),%ecx
	movl (%ecx),%ecx
	pushl %ecx
	pushl %esi
	movl 12(%ebp),%edx
	pushl %edx
	movl 8(%ebp),%ecx
	pushl %ecx
	movl 4(%eax),%eax
	call *%eax
	movl %eax,%edi
	addl $20,%esp
	testl %edi,%edi
	jne L199
	movl -4(%ebp),%eax
	testl %eax,%eax
	jne L198
L195:
	testl %edi,%edi
	jne L199
	cmpl $0,24(%ebp)
	jne L200
	movl %esi,%eax
	jmp L208
	.align 4,0x90
L200:
	movl 8(%ebp),%edx
	movl 72(%edx),%edx
	pushl %edx
	pushl $LC3
	pushl $___sF+168
	call _fprintf
	addl $12,%esp
	movl 20(%ebp),%ecx
	cmpl %ebx,(%ecx)
	jge L199
	movl %ebx,%eax
	subl (%ecx),%eax
	pushl %eax
	movl 8(%ebp),%edx
	pushl %edx
	call __AuEatData
	addl $8,%esp
L199:
	movl 20(%ebp),%ecx
	cmpl %ebx,(%ecx)
	jg L202
	addl (%ecx),%esi
	movl $0,(%ecx)
	movl %esi,%eax
	jmp L208
	.align 4,0x90
L202:
	movl 20(%ebp),%edx
	movl (%edx),%eax
	subl %ebx,%eax
	movl %eax,(%edx)
	addl %ebx,%esi
	movl %eax,%ebx
	movl %esi,%edi
	cmpl $32,%ebx
	jle L204
	.align 2,0x90
L206:
	cmpb $1,(%esi)
	je L207
	addl $32,%esi
	addl $-32,%ebx
	cmpl $32,%ebx
	jg L206
L204:
	leal -1(%ebx),%eax
	cmpl $30,%eax
	ja L207
	movl %edi,%esi
	movl $32,%ecx
	subl %ebx,%ecx
	movl %ecx,%ebx
	subl %ebx,%edi
	movl 20(%ebp),%edx
	movl (%edx),%edx
	pushl %edx
	pushl %esi
	pushl %edi
	call _memmove
	pushl %ebx
	movl %edi,%eax
	movl 20(%ebp),%ecx
	addl (%ecx),%eax
	pushl %eax
	movl 8(%ebp),%edx
	pushl %edx
	call __AuRead
	movl 20(%ebp),%ecx
	addl %ebx,(%ecx)
L207:
	movl %edi,%eax
L208:
	leal -16(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl __AuForceRoundTrip
__AuForceRoundTrip:
	pushl %ebp
	movl %esp,%ebp
	subl $64,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%esi
	movl 16(%ebp),%ecx
	movl 20(%ebp),%ebx
	movl 24(%ebp),%edi
	movl 76(%esi),%eax
	movl %eax,-64(%ebp)
	movl %eax,-60(%ebp)
	movb 12(%ebp),%dl
	movb %dl,-56(%ebp)
	movb %cl,-55(%ebp)
	movw %bx,-54(%ebp)
	movl $0,-48(%ebp)
	movl 1148(%esi),%edx
	movl %edx,-44(%ebp)
	movl $__AuAsyncErrorHandler,-40(%ebp)
	movl %ebp,%edx
	addl $-64,%edx
	movl %edx,-36(%ebp)
	leal -44(%ebp),%ebx
	movl %ebx,1148(%esi)
	movl 88(%esi),%eax
	addl $4,%eax
	cmpl %eax,92(%esi)
	jae L211
	pushl %esi
	call __AuFlush
	addl $4,%esp
L211:
	movl 88(%esi),%eax
	movl %eax,80(%esi)
	movb $33,(%eax)
	movw $1,2(%eax)
	addl $4,88(%esi)
	incl 76(%esi)
	pushl $0
	pushl $1
	pushl $0
	leal -32(%ebp),%eax
	pushl %eax
	pushl %esi
	call __AuReply
	addl $20,%esp
	cmpl %ebx,1148(%esi)
	jne L212
	movl -44(%ebp),%edx
	movl %edx,1148(%esi)
	jmp L213
	.align 4,0x90
L212:
	pushl %ebx
	pushl %esi
	call __AuDoDeqAsyncHandler
L213:
	testl %edi,%edi
	je L214
	cmpl $0,-48(%ebp)
	jle L215
	movzbl -52(%ebp),%edx
	movl %edx,(%edi)
	jmp L214
	.align 4,0x90
L215:
	movl $0,(%edi)
L214:
	cmpl $0,-48(%ebp)
	sete %al
	andl $255,%eax
	leal -76(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl __AuEatData
__AuEatData:
	pushl %ebp
	movl %esp,%ebp
	subl $2048,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 12(%ebp),%esi
	testl %esi,%esi
	je L219
	leal -2048(%ebp),%edi
	.align 2,0x90
L221:
	movl %esi,%ebx
	cmpl $2048,%esi
	jbe L220
	movl $2048,%ebx
L220:
	pushl %ebx
	pushl %edi
	movl 8(%ebp),%edx
	pushl %edx
	call __AuRead
	subl %ebx,%esi
	addl $12,%esp
	testl %esi,%esi
	jne L221
L219:
	leal -2060(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl _AuRegisterEventEnqHandler
_AuRegisterEventEnqHandler:
	pushl %ebp
	movl %esp,%ebp
	pushl %ebx
	movl 8(%ebp),%ebx
	pushl $20
	call _malloc
	movl %eax,%edx
	testl %edx,%edx
	jne L223
	xorl %eax,%eax
	jmp L225
	.align 4,0x90
L223:
	movl 12(%ebp),%ecx
	movl %ecx,4(%edx)
	movl 16(%ebp),%ecx
	movl %ecx,(%edx)
	movl 20(%ebp),%ecx
	movl %ecx,8(%edx)
	movl $0,16(%edx)
	movl 1168(%ebx),%ecx
	movl %ecx,12(%edx)
	cmpl $0,1168(%ebx)
	je L224
	movl 1168(%ebx),%eax
	movl %edx,16(%eax)
L224:
	movl %edx,1168(%ebx)
	movl %edx,%eax
L225:
	movl -4(%ebp),%ebx
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl _AuUnregisterEventEnqHandler
_AuUnregisterEventEnqHandler:
	pushl %ebp
	movl %esp,%ebp
	pushl %ebx
	movl 8(%ebp),%ecx
	movl 12(%ebp),%edx
	cmpl $0,12(%edx)
	je L227
	movl 12(%edx),%eax
	movl 16(%edx),%ebx
	movl %ebx,16(%eax)
L227:
	cmpl $0,16(%edx)
	je L228
	movl 16(%edx),%eax
	movl 12(%edx),%edx
	movl %edx,12(%eax)
	jmp L229
	.align 4,0x90
L228:
	movl 12(%edx),%edx
	movl %edx,1168(%ecx)
L229:
	movl -4(%ebp),%ebx
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
__AuEventEnqueued:
	pushl %ebp
	movl %esp,%ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%esi
	movl 16(%ebp),%edi
	movl 1168(%esi),%eax
	testl %eax,%eax
	je L232
	.align 2,0x90
L235:
	movl 12(%eax),%ebx
	cmpl $0,4(%eax)
	je L234
	movl 12(%ebp),%edx
	cmpl %edx,4(%eax)
	jne L233
L234:
	movl 8(%eax),%edx
	pushl %edx
	pushl %edi
	pushl %eax
	pushl %esi
	movl (%eax),%eax
	call *%eax
	addl $16,%esp
L233:
	movl %ebx,%eax
	testl %eax,%eax
	jne L235
L232:
	leal -12(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl __AuEnq
__AuEnq:
	pushl %ebp
	movl %esp,%ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%ebx
	movl 12(%ebp),%edi
	movl 64(%ebx),%esi
	testl %esi,%esi
	je L237
	movl (%esi),%ecx
	movl %ecx,64(%ebx)
	jmp L238
	.align 4,0x90
L237:
	pushl $48
	call _malloc
	movl %eax,%esi
	addl $4,%esp
	testl %esi,%esi
	jne L238
	movl $12,_errno
	pushl %ebx
	call __AuIOError
	addl $4,%esp
L238:
	movl $0,(%esi)
	movb (%edi),%dl
	andl $127,%edx
	pushl %edi
	leal 4(%esi),%eax
	pushl %eax
	pushl %ebx
	movl 124(%ebx,%edx,4),%eax
	call *%eax
	addl $12,%esp
	testl %eax,%eax
	je L240
	cmpl $0,60(%ebx)
	je L241
	movl 60(%ebx),%eax
	movl %esi,(%eax)
	jmp L242
	.align 4,0x90
L241:
	movl %esi,56(%ebx)
L242:
	movl %esi,60(%ebx)
	incl 68(%ebx)
	addl $-44,%esp
	movl %esp,%edi
	addl $4,%esi
	cld
	movl $11,%ecx
	rep
	movsl
	movl 16(%ebp),%ecx
	pushl %ecx
	pushl %ebx
	call __AuEventEnqueued
	jmp L243
	.align 4,0x90
L240:
	movl 64(%ebx),%ecx
	movl %ecx,(%esi)
	movl %esi,64(%ebx)
L243:
	leal -12(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl __AuUnknownWireEvent
__AuUnknownWireEvent:
	pushl %ebp
	movl %esp,%ebp
	xorl %eax,%eax
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl __AuUnknownNativeEvent
__AuUnknownNativeEvent:
	pushl %ebp
	movl %esp,%ebp
	xorl %eax,%eax
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl __AuWireToEvent
__AuWireToEvent:
	pushl %ebp
	movl %esp,%ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%edi
	movl 12(%ebp),%esi
	movl 16(%ebp),%ebx
	movb (%ebx),%dl
	andl $127,%edx
	movl %edx,(%esi)
	pushl %ebx
	pushl %edi
	call __AuSetLastRequestRead
	movl %eax,4(%esi)
	movb (%ebx),%al
	shrb $7,%al
	movl %eax,%edx
	andl $1,%edx
	movl %edx,8(%esi)
	movl %edi,12(%esi)
	movl 4(%ebx),%edx
	movl %edx,16(%esi)
	addl $8,%esp
	movb (%ebx),%al
	andl $127,%eax
	cmpl $2,%eax
	je L248
	cmpl $4,%eax
	je L249
	jmp L250
	.align 4,0x90
L248:
	movl 8(%ebx),%edx
	movl %edx,20(%esi)
	movb 12(%ebx),%dl
	movb %dl,24(%esi)
	movb 14(%ebx),%dl
	movb %dl,25(%esi)
	movb 16(%ebx),%dl
	movb %dl,26(%esi)
	movb 18(%ebx),%dl
	movb %dl,27(%esi)
	movb 20(%ebx),%dl
	movb %dl,28(%esi)
	movl 24(%ebx),%ebx
	movl %ebx,32(%esi)
	jmp L247
	.align 4,0x90
L249:
	movl 8(%ebx),%edx
	movl %edx,20(%esi)
	movb 12(%ebx),%dl
	movb %dl,24(%esi)
	movb 14(%ebx),%dl
	movb %dl,25(%esi)
	movb 15(%ebx),%dl
	movb %dl,26(%esi)
	movw 16(%ebx),%dx
	movw %dx,28(%esi)
	movw 18(%ebx),%dx
	movw %dx,30(%esi)
	movl 20(%ebx),%edx
	movl %edx,32(%esi)
	movl 24(%ebx),%edx
	movl %edx,36(%esi)
	movl 28(%ebx),%ebx
	movl %ebx,40(%esi)
	jmp L247
	.align 4,0x90
L250:
	pushl %ebx
	pushl %esi
	pushl %edi
	call __AuUnknownWireEvent
	jmp L252
	.align 4,0x90
L247:
	movl $1,%eax
L252:
	leal -12(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	ret
LC4:
	.ascii "unknown error\0"
LC5:
	.ascii "no such error\0"
	.align 4
__SysErrorMsg:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%ebp),%eax
	testl %eax,%eax
	jl L254
	cmpl %eax,_sys_nerr
	jle L254
	movl _sys_errlist(,%eax,4),%eax
	jmp L255
	.align 4,0x90
L254:
	movl $LC4,%eax
L255:
	testl %eax,%eax
	jne L256
	movl $LC5,%eax
L256:
	movl %ebp,%esp
	popl %ebp
	ret
LC6:
	.ascii "NCD-AUDIO connection to %s broken (explicit kill or server shutdown).\15\12\0"
LC7:
	.ascii "AuIO:  fatal IO error %d (%s) on audio server \"%s\"\15\12\0"
LC8:
	.ascii "      after %lu requests (%lu known processed) with %d events remaining.\15\12\0"
	.align 4
.globl __AuDefaultIOError
__AuDefaultIOError:
	pushl %ebp
	movl %esp,%ebp
	pushl %ebx
	movl 8(%ebp),%ebx
	cmpl $32,_errno
	jne L258
	movl 104(%ebx),%ebx
	pushl %ebx
	pushl $LC6
	pushl $___sF+168
	call _fprintf
	addl $12,%esp
	jmp L259
	.align 4,0x90
L258:
	movl 104(%ebx),%edx
	pushl %edx
	movl _errno,%edx
	pushl %edx
	call __SysErrorMsg
	addl $4,%esp
	pushl %eax
	movl _errno,%edx
	pushl %edx
	pushl $LC7
	pushl $___sF+168
	call _fprintf
	movl 68(%ebx),%edx
	pushl %edx
	movl 72(%ebx),%edx
	pushl %edx
	movl 76(%ebx),%ebx
	pushl %ebx
	pushl $LC8
	pushl $___sF+168
	call _fprintf
	addl $40,%esp
L259:
	pushl $1
	call _exit
	.align 4,0x90
LC9:
	.ascii "audiolib\0"
LC10:
	.ascii "Audio Error\0"
LC11:
	.ascii "AuError\0"
LC12:
	.ascii "%s:  %s\12  \0"
LC13:
	.ascii "Request Major code %d\0"
LC14:
	.ascii "MajorCode\0"
LC15:
	.ascii "%d\0"
LC16:
	.ascii "\0"
LC17:
	.ascii "AuRequest\0"
LC18:
	.ascii " (%s)\12\0"
LC19:
	.ascii "Request Minor code %d\0"
LC20:
	.ascii "MinorCode\0"
LC21:
	.ascii "  \0"
LC22:
	.ascii "%s.%d\0"
LC23:
	.ascii " (%s)\0"
LC24:
	.ascii "\12\0"
LC25:
	.ascii "Value\0"
LC26:
	.ascii "Error Serial #%d\0"
LC27:
	.ascii "ErrorSerial\0"
LC28:
	.ascii "Current Serial #%d\0"
LC29:
	.ascii "CurrentSerial\0"
LC30:
	.ascii "\12  \0"
	.align 4
.globl __AuPrintDefaultError
__AuPrintDefaultError:
	pushl %ebp
	movl %esp,%ebp
	subl $2088,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	xorl %esi,%esi
	xorl %edi,%edi
	pushl $1024
	leal -1024(%ebp),%ecx
	movl %ecx,-2084(%ebp)
	pushl %ecx
	movl 12(%ebp),%edx
	movzbl 24(%edx),%eax
	pushl %eax
	movl 8(%ebp),%ecx
	pushl %ecx
	call _AuGetErrorText
	pushl $1024
	leal -2048(%ebp),%ebx
	pushl %ebx
	pushl $LC10
	pushl $LC11
	pushl $LC9
	movl 8(%ebp),%edx
	pushl %edx
	call _AuGetErrorDatabaseText
	addl $40,%esp
	movl -2084(%ebp),%ecx
	pushl %ecx
	pushl %ebx
	pushl $LC12
	movl 16(%ebp),%edx
	pushl %edx
	call _fprintf
	pushl $1024
	pushl %ebx
	pushl $LC13
	pushl $LC14
	pushl $LC9
	movl 8(%ebp),%ecx
	pushl %ecx
	call _AuGetErrorDatabaseText
	addl $40,%esp
	movl 12(%ebp),%edx
	movzbl 25(%edx),%eax
	pushl %eax
	pushl %ebx
	movl 16(%ebp),%ecx
	pushl %ecx
	call _fprintf
	addl $12,%esp
	movl 12(%ebp),%edx
	cmpb $127,25(%edx)
	ja L261
	movzbl 25(%edx),%eax
	pushl %eax
	pushl $LC15
	leal -2080(%ebp),%ebx
	pushl %ebx
	call _sprintf
	pushl $1024
	movl -2084(%ebp),%ecx
	pushl %ecx
	pushl $LC16
	pushl %ebx
	pushl $LC17
	movl 8(%ebp),%edx
	pushl %edx
	call _AuGetErrorDatabaseText
	addl $36,%esp
	jmp L262
	.align 4,0x90
L261:
	movl 8(%ebp),%ecx
	movl 120(%ecx),%esi
	testl %esi,%esi
	je L267
	movl 12(%ebp),%edx
	movzbl 25(%edx),%eax
	cmpl %eax,8(%esi)
	je L264
	.align 2,0x90
L265:
	movl (%esi),%esi
	testl %esi,%esi
	je L267
	movl 12(%ebp),%ecx
	movzbl 25(%ecx),%eax
	cmpl %eax,8(%esi)
	jne L265
L264:
	testl %esi,%esi
	je L267
	movl 32(%esi),%edx
	pushl %edx
	leal -1024(%ebp),%eax
	pushl %eax
	call _strcpy
	addl $8,%esp
	jmp L262
	.align 4,0x90
L290:
	movl %esi,%edi
	jmp L273
	.align 4,0x90
L267:
	movb $0,-1024(%ebp)
L262:
	leal -1024(%ebp),%ecx
	movl %ecx,-2088(%ebp)
	pushl %ecx
	pushl $LC18
	movl 16(%ebp),%edx
	pushl %edx
	call _fprintf
	addl $12,%esp
	movl 12(%ebp),%ecx
	cmpb $127,25(%ecx)
	jbe L269
	pushl $1024
	leal -2048(%ebp),%ebx
	pushl %ebx
	pushl $LC19
	pushl $LC20
	pushl $LC9
	movl 8(%ebp),%edx
	pushl %edx
	call _AuGetErrorDatabaseText
	movl 16(%ebp),%ecx
	pushl %ecx
	pushl $LC21
	call _fputs
	addl $32,%esp
	movl 12(%ebp),%edx
	movzbl 26(%edx),%eax
	pushl %eax
	pushl %ebx
	movl 16(%ebp),%ecx
	pushl %ecx
	call _fprintf
	addl $12,%esp
	testl %esi,%esi
	je L270
	movl 12(%ebp),%edx
	movzbl 26(%edx),%eax
	pushl %eax
	movl 32(%esi),%esi
	pushl %esi
	pushl $LC22
	pushl %ebx
	call _sprintf
	pushl $1024
	movl -2088(%ebp),%ecx
	pushl %ecx
	pushl $LC16
	pushl %ebx
	pushl $LC17
	movl 8(%ebp),%edx
	pushl %edx
	call _AuGetErrorDatabaseText
	addl $40,%esp
	movl -2088(%ebp),%ecx
	pushl %ecx
	pushl $LC23
	movl 16(%ebp),%edx
	pushl %edx
	call _fprintf
	addl $12,%esp
L270:
	movl 16(%ebp),%ecx
	pushl %ecx
	pushl $LC24
	call _fputs
	addl $8,%esp
L269:
	movl 12(%ebp),%edx
	cmpb $127,24(%edx)
	jbe L271
	movb $0,-1024(%ebp)
	movl 8(%ebp),%ecx
	movl 120(%ecx),%esi
	testl %esi,%esi
	je L273
	.align 2,0x90
L279:
	cmpl $0,28(%esi)
	je L275
	pushl $1024
	leal -1024(%ebp),%eax
	pushl %eax
	leal 4(%esi),%eax
	pushl %eax
	movl 12(%ebp),%edx
	movzbl 24(%edx),%eax
	pushl %eax
	movl 8(%ebp),%ecx
	pushl %ecx
	movl 28(%esi),%eax
	call *%eax
	addl $20,%esp
L275:
	cmpb $0,-1024(%ebp)
	jne L290
	cmpl $0,16(%esi)
	je L274
	movl 12(%ebp),%edx
	movzbl 24(%edx),%eax
	cmpl %eax,16(%esi)
	jge L274
	testl %edi,%edi
	je L278
	movl 16(%esi),%eax
	cmpl %eax,16(%edi)
	jge L274
L278:
	movl %esi,%edi
L274:
	movl (%esi),%esi
	testl %esi,%esi
	jne L279
L273:
	testl %edi,%edi
	je L280
	movl 12(%ebp),%ecx
	movzbl 24(%ecx),%eax
	subl 16(%edi),%eax
	pushl %eax
	movl 32(%edi),%edi
	pushl %edi
	pushl $LC22
	leal -1024(%ebp),%eax
	pushl %eax
	call _sprintf
	addl $16,%esp
	jmp L281
	.align 4,0x90
L280:
	movl LC25,%edx
	movl %edx,-1024(%ebp)
	movw LC25+4,%cx
	movw %cx,-1020(%ebp)
L281:
	pushl $1024
	leal -2048(%ebp),%ebx
	pushl %ebx
	pushl $LC16
	leal -1024(%ebp),%eax
	pushl %eax
	pushl $LC9
	movl 8(%ebp),%edx
	pushl %edx
	call _AuGetErrorDatabaseText
	addl $24,%esp
	cmpb $0,-2048(%ebp)
	je L282
	movl 16(%ebp),%ecx
	pushl %ecx
	pushl $LC21
	call _fputs
	movl 12(%ebp),%edx
	movl 20(%edx),%edx
	pushl %edx
	pushl %ebx
	movl 16(%ebp),%ecx
	pushl %ecx
	call _fprintf
	movl 16(%ebp),%edx
	pushl %edx
	pushl $LC24
	call _fputs
	addl $28,%esp
L282:
	movl 8(%ebp),%ecx
	movl 120(%ecx),%esi
	testl %esi,%esi
	je L271
	.align 2,0x90
L287:
	cmpl $0,36(%esi)
	je L285
	movl 16(%ebp),%edx
	pushl %edx
	movl 12(%ebp),%ecx
	pushl %ecx
	movl 8(%ebp),%edx
	pushl %edx
	movl 36(%esi),%eax
	call *%eax
	addl $12,%esp
L285:
	movl (%esi),%esi
	testl %esi,%esi
	jne L287
L271:
	pushl $1024
	leal -2048(%ebp),%ebx
	pushl %ebx
	pushl $LC26
	pushl $LC27
	pushl $LC9
	movl 8(%ebp),%ecx
	pushl %ecx
	call _AuGetErrorDatabaseText
	movl 16(%ebp),%edx
	pushl %edx
	pushl $LC21
	call _fputs
	addl $32,%esp
	movl 12(%ebp),%ecx
	movl 4(%ecx),%ecx
	pushl %ecx
	pushl %ebx
	movl 16(%ebp),%edx
	pushl %edx
	call _fprintf
	pushl $1024
	pushl %ebx
	pushl $LC28
	pushl $LC29
	pushl $LC9
	movl 8(%ebp),%ecx
	pushl %ecx
	call _AuGetErrorDatabaseText
	addl $36,%esp
	movl 16(%ebp),%edx
	pushl %edx
	pushl $LC30
	call _fputs
	movl 8(%ebp),%ecx
	movl 76(%ecx),%ecx
	pushl %ecx
	pushl %ebx
	movl 16(%ebp),%edx
	pushl %edx
	call _fprintf
	movl 16(%ebp),%ecx
	pushl %ecx
	pushl $LC24
	call _fputs
	movl 12(%ebp),%edx
	cmpb $17,24(%edx)
	je L288
	movl $1,%eax
	jmp L289
	.align 4,0x90
L288:
	xorl %eax,%eax
L289:
	leal -2100(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl __AuDefaultError
__AuDefaultError:
	pushl %ebp
	movl %esp,%ebp
	pushl $___sF+168
	movl 12(%ebp),%edx
	pushl %edx
	movl 8(%ebp),%edx
	pushl %edx
	call __AuPrintDefaultError
	addl $12,%esp
	testl %eax,%eax
	jne L292
	xorl %eax,%eax
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4,0x90
L292:
	pushl $1
	call _exit
	.align 4,0x90
	.align 4
.globl __AuDefaultWireError
__AuDefaultWireError:
	pushl %ebp
	movl %esp,%ebp
	movl $1,%eax
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl __AuError
__AuError:
	pushl %ebp
	movl %esp,%ebp
	subl $44,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%edi
	movl 12(%ebp),%ebx
	pushl %ebx
	pushl %edi
	call __AuSetLastRequestRead
	movl %eax,-40(%ebp)
	movl 1148(%edi),%eax
	addl $8,%esp
	testl %eax,%eax
	je L297
	.align 2,0x90
L300:
	movl (%eax),%esi
	movl 8(%eax),%edx
	pushl %edx
	pushl $32
	pushl %ebx
	pushl %ebx
	pushl %edi
	movl 4(%eax),%eax
	call *%eax
	addl $20,%esp
	testl %eax,%eax
	jne L309
	movl %esi,%eax
	testl %eax,%eax
	jne L300
L297:
	movl %edi,-32(%ebp)
	movl $0,-44(%ebp)
	movl 4(%ebx),%edx
	movl %edx,-28(%ebp)
	movl 8(%ebx),%edx
	movl %edx,-24(%ebp)
	movb 1(%ebx),%dl
	movb %dl,-20(%ebp)
	movb 14(%ebx),%dl
	movb %dl,-19(%ebp)
	movb 12(%ebx),%dl
	movb %dl,-18(%ebp)
	movl 16(%ebx),%edx
	movl %edx,-16(%ebp)
	movl 20(%ebx),%edx
	movl %edx,-12(%ebp)
	movl 24(%ebx),%edx
	movl %edx,-8(%ebp)
	movl 28(%ebx),%edx
	movl %edx,-4(%ebp)
	cmpl $0,1156(%edi)
	je L304
	movzbl 1(%ebx),%ecx
	movl 1156(%edi),%edx
	pushl %ebx
	leal -44(%ebp),%eax
	pushl %eax
	pushl %edi
	movl (%edx,%ecx,4),%eax
	call *%eax
	addl $12,%esp
	testl %eax,%eax
	jne L304
L309:
	xorl %eax,%eax
	jmp L307
	.align 4,0x90
L304:
	cmpl $0,1172(%edi)
	jne L305
	leal -44(%ebp),%eax
	pushl %eax
	pushl %edi
	call __AuDefaultError
	jmp L307
	.align 4,0x90
L305:
	leal -44(%ebp),%eax
	pushl %eax
	pushl %edi
	movl 1172(%edi),%eax
	call *%eax
L307:
	leal -56(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl __AuIOError
__AuIOError:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%ebp),%eax
	orb $1,1152(%eax)
	cmpl $0,1176(%eax)
	je L311
	pushl %eax
	movl 1176(%eax),%eax
	call *%eax
	jmp L313
	.align 4,0x90
L311:
	pushl %eax
	call __AuDefaultIOError
L313:
	addl $4,%esp
	pushl $1
	call _exit
	.align 4,0x90
	.align 4
.globl __AuAllocScratch
__AuAllocScratch:
	pushl %ebp
	movl %esp,%ebp
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%ebx
	movl 12(%ebp),%esi
	cmpl %esi,112(%ebx)
	jae L315
	cmpl $0,108(%ebx)
	je L316
	movl 108(%ebx),%edx
	pushl %edx
	call _free
	addl $4,%esp
L316:
	pushl %esi
	call _malloc
	movl %eax,108(%ebx)
	testl %eax,%eax
	je L317
	movl %esi,112(%ebx)
	jmp L315
	.align 4,0x90
L317:
	movl $0,112(%ebx)
L315:
	movl 108(%ebx),%eax
	leal -8(%ebp),%esp
	popl %ebx
	popl %esi
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl _AuFree
_AuFree:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%ebp),%eax
	pushl %eax
	call _free
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl __AuFreeQ
__AuFreeQ:
	pushl %ebp
	movl %esp,%ebp
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%esi
	movl 64(%esi),%eax
	testl %eax,%eax
	je L322
	.align 2,0x90
L323:
	movl (%eax),%ebx
	pushl %eax
	call _free
	movl %ebx,%eax
	addl $4,%esp
	testl %eax,%eax
	jne L323
L322:
	movl $0,64(%esi)
	leal -8(%ebp),%esp
	popl %ebx
	popl %esi
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl __AuGetHostname
__AuGetHostname:
	pushl %ebp
	movl %esp,%ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%ebx
	movl 12(%ebp),%esi
	movb $0,(%ebx)
	pushl %esi
	pushl %ebx
	call _gethostname
	movb $0,-1(%ebx,%esi)
	xorb %al,%al
	movl %ebx,%edi
	cld
	movl $-1,%ecx
	repne
	scasb
	movl %ecx,%eax
	notl %eax
	decl %eax
	leal -12(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl _AuRegisterSyncHandler
_AuRegisterSyncHandler:
	pushl %ebp
	movl %esp,%ebp
	pushl %ebx
	movl 8(%ebp),%ebx
	pushl $16
	call _malloc
	movl %eax,%edx
	testl %edx,%edx
	jne L326
	xorl %eax,%eax
	jmp L328
	.align 4,0x90
L326:
	movl 12(%ebp),%ecx
	movl %ecx,(%edx)
	movl 16(%ebp),%ecx
	movl %ecx,4(%edx)
	movl $0,12(%edx)
	movl 100(%ebx),%ecx
	movl %ecx,8(%edx)
	cmpl $0,100(%ebx)
	je L327
	movl 100(%ebx),%eax
	movl %edx,12(%eax)
L327:
	movl %edx,100(%ebx)
	movl %edx,%eax
L328:
	movl -4(%ebp),%ebx
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl _AuUnregisterSyncHandler
_AuUnregisterSyncHandler:
	pushl %ebp
	movl %esp,%ebp
	pushl %ebx
	movl 8(%ebp),%ecx
	movl 12(%ebp),%edx
	cmpl $0,8(%edx)
	je L330
	movl 8(%edx),%eax
	movl 12(%edx),%ebx
	movl %ebx,12(%eax)
L330:
	cmpl $0,12(%edx)
	je L331
	movl 12(%edx),%eax
	movl 8(%edx),%edx
	movl %edx,8(%eax)
	jmp L332
	.align 4,0x90
L331:
	movl 8(%edx),%edx
	movl %edx,100(%ecx)
L332:
	movl -4(%ebp),%ebx
	movl %ebp,%esp
	popl %ebp
	ret
	.align 4
.globl __AuDoSyncHandle
__AuDoSyncHandle:
	pushl %ebp
	movl %esp,%ebp
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%esi
	movl 100(%esi),%eax
	testl %eax,%eax
	je L335
	.align 2,0x90
L336:
	movl 8(%eax),%ebx
	movl 4(%eax),%edx
	pushl %edx
	pushl %eax
	pushl %esi
	movl (%eax),%eax
	call *%eax
	movl %ebx,%eax
	addl $12,%esp
	testl %eax,%eax
	jne L336
L335:
	leal -8(%ebp),%esp
	popl %ebx
	popl %esi
	movl %ebp,%esp
	popl %ebp
	ret
