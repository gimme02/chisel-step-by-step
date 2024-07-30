package MemorySystem

import chisel3._
import chisel3.util._
import chisel3.experimental._
import AXI._
import AXI.AXIConstants._

object MemoryState extends ChiselEnum{
    val sIdle, sWrite, sWrAck, sRead = Value
}

class MemIO(params: AXIBundleParameters) extends Bundle {
  val aw  = Decoupled(new AXIAddressBundle(params))
  val w   = Decoupled(new AXIWriteDataBundle(params))
  val b   = Flipped(Decoupled(new AXIWriteResponseBundle(params)))
  val ar  = Decoupled(new AXIAddressBundle(params))
  val r   = Flipped(Decoupled(new AXIReadDataBundle(params)))
  val state = Flipped(UInt(2.W))
}

class Memory(MemParam: MemoryConfig, BusParam: AXIBundleParameters) extends Module {
    /*io = AW/W/B, AR/R*/
    val io  = IO(Flipped(new MemIO(BusParam)))
    val mem = Mem(MemParam.capacity, UInt(BusParam.dataBits.W))
    io.state := 0.U
    val cnterr  = RegInit(false.B)
    val done    = RegInit(false.B)
    val cycle   = RegInit(0.U(log2Ceil(65536).W))

    cycle       := cycle + 1.U
    cnterr      := cycle === 65536.U

    import MemoryState._
    val state   = RegInit(sIdle)
    val addr    = Reg(UInt(BusParam.addrBits.W))
    val id      = Reg(UInt(BusParam.idBits.W))
    val len     = Reg(UInt(LenBits.W))
    val size    = Reg(UInt(SizeBits.W))
    val offset  = RegInit(0.U(LenBits.W))
    /*Write data with strobe*/
    val write = (0 until (BusParam.dataBits / 8)).foldLeft(0.U(BusParam.dataBits.W)) { (write, i) =>
        write |
        (Mux(io.w.bits.strb(i), io.w.bits.data, mem(addr))(
            8 * (i + 1) - 1,
            8 * i
        ) << (8 * i).U).asUInt
    }
    val rpipe   = Wire(new AXIReadDataBundle(BusParam))
    val bpipe   = Wire(new AXIWriteResponseBundle(BusParam))
    rpipe       := AXIReadDataBundle(BusParam)(id, mem(addr+offset), offset===len)
    bpipe       := AXIWriteResponseBundle(BusParam)(id)

    /*Wire-up IO*/
    io.ar.ready := state===sIdle
    io.aw.ready := state===sIdle
    io.w.ready  := state===sWrite
    io.r.valid  := state===sRead
    io.b.valid  := state===sWrAck
    io.b.bits   <> bpipe
    io.r.bits   <> rpipe

    /*State Transition*/
    switch(state){
        /*1. Accept Request - Valid iff handshake is made*/
        is(sIdle){
            io.state    := 0.U
            when(io.aw.valid){
                done    := false.B
                addr    := io.aw.bits.addr
                id      := io.aw.bits.id
                len     := io.aw.bits.len
                size    := io.aw.bits.size
                offset  := 0.U
                state   := sWrite
            }.elsewhen(io.ar.valid){
                done    := false.B
                addr    := io.ar.bits.addr
                id      := io.ar.bits.id
                len     := io.ar.bits.len
                size    := io.ar.bits.size
                offset  := 0.U
                state   := sRead
            }
        }
        /*2. Update State */
        is(sWrite){
            io.state    := 1.U
            when(io.w.valid){
                mem(addr+offset)    := io.w.bits.data
                when(offset===len){
                    state   := sWrAck
                }.otherwise{
                    offset  := offset + 1.U
                }
            }
        }
        is(sWrAck){
            io.state    := 2.U
            when(io.b.ready){
                done    := true.B
                state   := sIdle
            }
        }
        is(sRead){
            io.state    := 3.U
            when(io.r.valid){
                when(offset===len){
                    done    := true.B
                    state   := sIdle
                }.otherwise{
                    offset  := offset + 1.U
                }
            }
        }
    }


}