.intel_syntax

.global runReturnHere
.global switchStacks
.global finalRestore

runReturnHere:
// when the async function finally returns, instead of yielding, it will return here
// the stack pointer will be 16 bytes below the top of the stack, pointing to a copy of
// the context pointer that was saved by run(). we restore that context pointer so that
// we know what the original stack to switch back into is, and then execute the latter half
// of switch_stacks() normally (switch to the new stack pointer, pop registers, return).
pop rdi
jmp finalRestore

switchStacks:
// push registers to old stack
push rbx
push rbp
push r12
push r13
push r14
push r15
// swap stacks
finalRestore:
xchg rsp, qword ptr [rdi]
// pop registers from new stack
pop r15
pop r14
pop r13
pop r12
pop rbp
pop rbx
// return into new code
ret
