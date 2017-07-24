package fommil.stalagmite.stats

import fommil.stalagmite.DataInfo
import fommil.stalagmite.DataStats

import scala.collection.immutable.Seq
import scala.meta._

object HeapOptimizationStats {

  /**
   * @param args Constructor arguments before packing
   * @return Packing stats, new packed arguments
   */
  def specialApplyPart(dataInfo: DataInfo,
                       args: Seq[Term]): (Seq[Stat], Seq[Term]) =
    (Seq(q"val packed = pack(..$args)"),
     dataInfo.optimizedParams.indices
       .map(ind => q"packed.${Term.Name("_" + ind)}"))

  /**
   * Special logic for `readObject` method for packing case
   * @return Packing stats
   */
  def specialReadObjectPart(dataInfo: DataInfo): Seq[Stat] = {
    val pack =
      q"""
          val packed = ${dataInfo.termName}
            .pack(..${dataInfo.classParamNames})
        """
    val fieldsAssignments = dataInfo.optimizedParams.unzip._1 match {
      case Seq(p1) => Seq(q"${Term.Name("_" + p1.value)} = packed")
      case ps =>
        ps.zipWithIndex.map {
          case (param, ind) =>
            q"""${Term.Name("_" + param.value)} =
                   packed.${Term.Name("_" + ind)}"""
        }
    }
    pack +: fieldsAssignments
  }

  object UnpackGettersStats extends DataStats {
    override def classStats(dataInfo: DataInfo): Seq[Stat] =
      dataInfo.classParamsWithTypes.zip(dataInfo.bitPositions).map {
        case ((param, tpe), bitPos) =>
          val fieldName = Term.Name("_" + param.value)

          val typeWithoutOption = tpe match {
            case t"Option[$t]" if dataInfo.getMod("optimiseHeapOptions") => t
            case t                                                       => t
          }

          val fieldGetter = bitPos.booleanBit match {
            case Some(bBit) => q"(_bitmask & (1 << ${Lit.Int(bBit)})) != 0"
            case None =>
              typeWithoutOption match {
                case t"String" if dataInfo.getMod("optimiseHeapStrings") =>
                  q"new String(this.$fieldName)"
                case _ =>
                  q"this.$fieldName"
              }
          }

          val fullGetter = bitPos.optionBit match {
            case Some(oBit) =>
              q"""
                 if ((_bitmask & (1 << ${Lit.Int(oBit)})) != 0) {
                   None
                 } else {
                   $fieldGetter
                 }
               """
            case None =>
              tpe match {
                case t"Option[$_]" if dataInfo.getMod("optimiseHeapOptions") =>
                  q"""
                   if (this.$fieldName == null) {
                     None
                   } else {
                     Some($fieldGetter)
                   }
                 """
                case _ => fieldGetter
              }
          }
          q"def $param: $tpe = $fullGetter"
      }
  }

  object PackStats extends DataStats {
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val params = dataInfo.classParamsWithTypes.map {
        case (param, tpe) => param"$param: $tpe"
      }
      val packedType = dataInfo.optimizedParams.unzip._2 match {
        case Seq(tpe1) => tpe1
        case tpes      => t"(..$tpes)"
      }

      val packs = dataInfo.classParamsWithTypes.zip(dataInfo.bitPositions).map {
        case ((param, tpe), bitPos) =>
          val fieldName = Term.Name("_" + param.value)

          val (typeWithoutOption, paramAccess: Term) = tpe match {
            case t"Option[$t]" if dataInfo.getMod("optimiseHeapOptions") =>
              (t, q"$param.get")
            case t =>
              (t, q"$param")
          }

          val (fieldPack: Term, needVal) = bitPos.booleanBit match {
            case Some(bBit) =>
              (q"""if ($paramAccess) {
                     _bitmask |= (1 << ${Lit.Int(bBit)})
                   }""", false)
            case None =>
              typeWithoutOption match {
                case t"String" if dataInfo.getMod("optimiseHeapStrings") =>
                  (q"$paramAccess.getBytes", true)
                case _ =>
                  (q"$paramAccess", true)
              }
          }

          val fullPack: Term = bitPos.optionBit match {
            case Some(oBit) =>
              q"""
                  if ($param == None) {
                    _bitmask |= (1 << ${Lit.Int(oBit)})
                  } else {
                    $fieldPack
                  }
               """
            case None =>
              tpe match {
                case t"Option[$_]" if dataInfo.getMod("optimiseHeapOptions") =>
                  q"""
                   if ($param == None) {
                     null
                   } else {
                     $fieldPack
                   }
                 """
                case _ => fieldPack
              }
          }

          if (needVal) {
            q"val ${Pat.Var.Term(fieldName)} = $fullPack"
          } else {
            fullPack
          }
      }

      val optionalBitmask = if (dataInfo.hasBitmask) {
        Seq(q"var _bitmask: Long = 0L")
      } else {
        Seq()
      }

      val output = dataInfo.optimizedParams.unzip._1
        .map(param => Term.Name("_" + param.value)) match {
        case Seq(p1) => p1
        case ps      => q"(..$ps)"
      }

      Seq(
        q"""private def pack(..$params): $packedType = {
          ..$optionalBitmask
          ..$packs
          $output
         }
       """
      )
    }
  }
}
