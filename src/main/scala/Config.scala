package MemorySystem

import chisel3._
import AXI._

case class MemoryConfig(capacity: Int)
case class CacheConfig(nSets: Int, blockBytes: Int)