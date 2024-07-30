package MemorySystem

import AXI._ 
import chisel3._
import chisel3.util._
import chisel3.experimental._
import chiseltest._
import org.scalatest.flatspec._

class MemoryTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "MemoryModule"
    val MemConfig = MemoryConfig(capacity = 8192)
    val AXIParam  = AXIBundleParameters(addrBits = 32, dataBits = 32, idBits = 5)

    def InitMemoryIO (c: Memory): Unit = {
        c.io.ar.valid.poke(false.B)
        c.io.aw.valid.poke(false.B)
        c.io.w.valid.poke(false.B)
        c.io.b.ready.poke(false.B)
        c.io.r.ready.poke(false.B)
    }

    def WriteAddressRequest (c: Memory, id: UInt, addr: UInt, len: UInt, size: UInt) : Unit = {
        //val aw = AXIAddressBundle(AXIParam)(id, addr, size, len)
        c.io.aw.bits.id.poke(id) 
        c.io.aw.bits.addr.poke(addr) 
        c.io.aw.bits.len.poke(len) 
        c.io.aw.bits.size.poke(size) 
        c.io.aw.bits.burst.poke(AXIConstants.BurstIncr) 
        c.io.aw.bits.lock.poke(false.B) 
        c.io.aw.bits.cache.poke(0.U) 
        c.io.aw.bits.prot.poke(0.U) 
        c.io.aw.bits.qos.poke(0.U) 
        c.io.aw.valid.poke(true.B)
        c.io.ar.valid.poke(false.B)
        c.io.w.valid.poke(false.B)
        c.io.r.ready.poke(false.B)
        c.io.b.ready.poke(false.B)
    }
    def WriteDataRequest (c: Memory, data: UInt, last : Bool = true.B, strb : Option[UInt] = None) : Unit = {
        //val w  = AXIWriteDataBundle(AXIParam)(data)
        c.io.w.bits.strb.poke(strb.getOrElse((1<<(AXIParam.dataBits/8)-1).U))
        c.io.w.bits.data.poke(data)
        c.io.w.bits.last.poke(last)
        c.io.aw.valid.poke(false.B)
        c.io.ar.valid.poke(false.B)
        c.io.w.valid.poke(true.B)
        c.io.b.ready.poke(false.B)
        c.io.r.ready.poke(false.B)
        step(1)
    }
    def ReadAddressRequest (c: Memory, id: UInt, addr: UInt, len: UInt, size: UInt) : Unit = {
        //val ar = AXIAddressBundle(AXIParam)(id, addr, size, len)
        c.io.ar.bits.id.poke(id) 
        c.io.ar.bits.addr.poke(addr) 
        c.io.ar.bits.len.poke(len) 
        c.io.ar.bits.size.poke(size) 
        c.io.ar.bits.burst.poke(AXIConstants.BurstIncr) 
        c.io.ar.bits.lock.poke(false.B) 
        c.io.ar.bits.cache.poke(0.U) 
        c.io.ar.bits.prot.poke(0.U) 
        c.io.ar.bits.qos.poke(0.U) 
        c.io.aw.valid.poke(false.B)
        c.io.ar.valid.poke(true.B)
        c.io.w.valid.poke(false.B)
        //c.io.r.ready.poke(false.B)
        //c.io.b.ready.poke(false.B)
    }

    def write(c: Memory, id: Int, addr: Int, len: Int, size: Int, data: List[Int]) : Unit = {
        /*1. First establish address channel handshake*/
        println("==========Memory Write Request==========")
        println(f"[Address]:$addr%d\t[len]:$len%d\n")
        WriteAddressRequest(c, id.U, addr.U, len.U, size.U)
        while(!c.io.aw.ready.peek().litToBoolean){
            println("Memory is busy")
            step(1)
        }
        step(1)
        /*2. Once write channel is established, send data sequentailly*/
        for(i<-0 until len+1){
            val wdata = data(i)
            println(f"[data]:$wdata%d")
            WriteDataRequest(c, data(i).U)
        }
        c.io.b.ready.poke(true.B)
        if(c.io.b.valid.peek().litToBoolean && c.io.b.bits.resp.peek().litValue == 0){
            println("==========Memory Write Success==========\n")
        }
        else{
            println("=======!!!Memory Write Failed!!!=======\n")
        }
        step(1)
    }

    def read(c: Memory, id: Int, addr: Int, len: Int, size: Int, data: List[Int]) : Unit = {
        /*1. First establish address channel handshake*/
        println("==========Memory Read Request==========")
        println(f"[Address]: $addr%d\t[len]: $len%d\n")
        c.io.r.ready.poke(true.B)
        ReadAddressRequest(c, id.U, addr.U, len.U, size.U)
        while(!c.io.ar.ready.peek().litToBoolean){
            println("Memory is busy")
            step(1)
        }
        step(1)
        /*2. Once write channel is established, send data sequentailly*/
        for(i<-0 until len+1){
            if(c.io.r.valid.peek().litToBoolean){
                val rdata   = c.io.r.bits.data.peek().litValue
                println(s"[data]: $rdata")
                c.io.r.bits.data.expect(data(i).U)
                step(1)
            }
        }
        println("==========Memory Read Success==========\n")
    }

    it should "Write and Read Correctly" in {
        test(new Memory(MemConfig, AXIParam)) {c => 
            InitMemoryIO(c)
            val testcase1 = List(0, 1, 2, 3)
            val testcase2 = List(5, 6, 7)
            val testcase3 = List(108)

            write(c, 0, 0, 3, 4, testcase1)
            write(c, 0, 100, 3, 4, testcase1)
            read(c, 0, 0, 3, 4, testcase1)
            read(c, 0, 100, 3, 4, testcase1)
            read(c, 0, 102, 1, 4, List(2, 3))
        }
    }
}