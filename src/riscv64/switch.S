.global runReturnHere
.global switchStacks
.global finalRestore

runReturnHere:
// preserve return value of child function
mv a3, a0
// retrieve context pointer from stack
ld a0, 0(sp)

// clean up (dupe of final half of switchStacks)
// switch stacks
// copy new sp to temporary
ld a1, 0(a0)
// store old sp
sd sp, 0(a0)

// switch
mv sp, a1

// restore registers from old stack
ld gp, 0(sp)
ld tp, 8(sp)

ld s0, 16(sp)
ld s1, 24(sp)
ld s2, 32(sp)
ld s3, 40(sp)
ld s4, 48(sp)
ld s5, 56(sp)
ld s6, 64(sp)
ld s7, 72(sp)
ld s8, 80(sp)
ld s9, 88(sp)
ld s10, 96(sp)
ld s11, 104(sp)

fld fs0, 112(sp)
fld fs1, 120(sp)
fld fs2, 128(sp)
fld fs3, 136(sp)
fld fs4, 144(sp)
fld fs5, 152(sp)
fld fs6, 160(sp)
fld fs7, 168(sp)
fld fs8, 176(sp)
fld fs9, 184(sp)
fld fs10, 192(sp)
fld fs11, 200(sp)

ld ra, 208(sp)
ld a2, 216(sp)

// return
addi sp, sp, 224
// difference from switchStacks: we grab the child function's return value instead of passing
// through a parameter
mv a0, a3
jr a2


switchStacks:
// store registers on old stack

addi sp, sp, -224

sd gp, 0(sp)
sd tp, 8(sp)

sd s0, 16(sp)
sd s1, 24(sp)
sd s2, 32(sp)
sd s3, 40(sp)
sd s4, 48(sp)
sd s5, 56(sp)
sd s6, 64(sp)
sd s7, 72(sp)
sd s8, 80(sp)
sd s9, 88(sp)
sd s10, 96(sp)
sd s11, 104(sp)

fsd fs0, 112(sp)
fsd fs1, 120(sp)
fsd fs2, 128(sp)
fsd fs3, 136(sp)
fsd fs4, 144(sp)
fsd fs5, 152(sp)
fsd fs6, 160(sp)
fsd fs7, 168(sp)
fsd fs8, 176(sp)
fsd fs9, 184(sp)
fsd fs10, 192(sp)
fsd fs11, 200(sp)

sd ra, 208(sp)
sd ra, 216(sp)

finalRestore:
// switch stacks
// copy new sp to temporary
ld a1, 0(a0)
// store old sp
sd sp, 0(a0)

// switch
mv sp, a1

// restore registers from old stack
ld gp, 0(sp)
ld tp, 8(sp)

ld s0, 16(sp)
ld s1, 24(sp)
ld s2, 32(sp)
ld s3, 40(sp)
ld s4, 48(sp)
ld s5, 56(sp)
ld s6, 64(sp)
ld s7, 72(sp)
ld s8, 80(sp)
ld s9, 88(sp)
ld s10, 96(sp)
ld s11, 104(sp)

fld fs0, 112(sp)
fld fs1, 120(sp)
fld fs2, 128(sp)
fld fs3, 136(sp)
fld fs4, 144(sp)
fld fs5, 152(sp)
fld fs6, 160(sp)
fld fs7, 168(sp)
fld fs8, 176(sp)
fld fs9, 184(sp)
fld fs10, 192(sp)
fld fs11, 200(sp)

ld ra, 208(sp)
ld a2, 216(sp)

// return
addi sp, sp, 224
jr a2
