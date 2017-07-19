package fommil.stalagmite.stats

import fommil.stalagmite.DataInfo
import fommil.stalagmite.DataStats

import scala.collection.immutable.Seq
import scala.meta._

object HeapOptimizationStats {
  object UnpackGettersStats extends DataStats {
    override def classStats(dataInfo: DataInfo): Seq[Stat] =
      dataInfo.classParamsWithTypes.zip(dataInfo.bitPositions).map {
        case ((param, tpe), bitPos) => {
          val fieldGetter = bitPos.booleanBit match {
            case Some(bBit) => q"(_bitmask & (1 << ${Lit.Int(bBit)})) != 0"
            case None =>
              (bitPos.fieldByte, bitPos.fieldSize) match {
                case (Some(fByte), Some(fSize)) =>
              }
          }

          bitPos.optionBit match {
            case Some(oBit) =>
            case None       =>
          }
        }
        q"def $param: $tpe = this.${Term.Name("_" + param.value)}"
      }

    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {}
  }
}
